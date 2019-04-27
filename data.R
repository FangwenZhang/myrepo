library(tidyverse)
library(rvest)
library(shiny)
library(httr)
library(Quandl)

#setting the key for the API
Quandl.api_key("akHYXf6xiWgjt6itcz4X")

planodat <- as_tibble(Quandl("ZILLOW/C72_ZHVIAH")) %>% select(Date, "Plano"=Value)
bostondat <- as_tibble(Quandl("ZILLOW/C22_ZHVIAH")) %>% select(Date, "Boston"=Value)
chicagodat <- as_tibble(Quandl("ZILLOW/C3_ZHVIAH")) %>% select(Date, "Chicago"=Value)
esterodat <- as_tibble(Quandl("ZILLOW/C2246_ZHVIAH")) %>% select(Date, "Estero"=Value)
usdat <- as_tibble(Quandl("ZILLOW/M1_ZHVIAH")) %>% select(Date, "US"=Value)
arlingtondat <- as_tibble(Quandl("ZILLOW/CO3401_ZHVIAH")) %>% select(Date, "Arlington" =Value)


dat1 <- full_join(planodat, bostondat, by=c("Date"))
dat2 <- full_join(chicagodat, esterodat, by=c("Date"))
dat3 <- full_join(usdat, arlingtondat, by=c("Date"))
dat4<- full_join(dat1, dat2, by=c("Date"))
dat <- full_join(dat4, dat3, by=c("Date")) %>% arrange(Date)
dat

dat <- cbind(dat[,1], (dat[,-1]/1000))

#Using a loop to build separate tibbles for each indexed year

for(i in 1996:2018){
  x <- ifelse(i==1996, 1, ((i-1996)*12)-2)
  
  dat %>%
    mutate(Plano = Plano/Plano[x],
           Boston = Boston/Boston[x],
           Chicago = Chicago/Chicago[x],
           Estero = Estero/Estero[x],
           US = US/US[x],
           Arlington = Arlington/Arlington[x]) %>%
    gather(key="City", value="Value", Boston, Chicago, Arlington, Estero, Plano, US) %>%
    arrange(., Date) %>%
    assign(paste0("dat", i), ., envir = .GlobalEnv)
}

#also using the gather function for the non-indexed data
dattrue <- dat %>%
  gather(key="City", value="Value", Boston, Chicago, Arlington, Estero, Plano, US) %>%
  arrange(Date)

#Creating the U.S.-adjusted data
datadjust <- dat %>% mutate(Plano = Plano/US,
                            Boston = Boston/US,
                            Chicago = Chicago/US,
                            Estero = Estero/US,
                            Arlington = Arlington/US,
                            US=US/US) %>%
  select(Date, Arlington, Boston, Chicago, Estero, Plano, US) 

datadjusttrue <-datadjust %>% gather(key="City", value="Value", Boston, Chicago, Arlington, Estero, Plano, US) %>%
  arrange(., Date)

#Creating the tibbles for each year for hte U.S.-adjusted data
for(i in 1996:2018){
  x <- ifelse(i==1996, 1, ((i-1996)*12)-2)
  
  datadjust %>%
    mutate(Plano = Plano/Plano[x],
           Boston = Boston/Boston[x],
           Chicago = Chicago/Chicago[x],
           Estero = Estero/Estero[x],
           Arlington = Arlington/Arlington[x],
           US=US/US[x]) %>%
    gather(key="City", value="Value", Boston, Chicago, Arlington, Estero, Plano, US) %>%
    arrange(., Date) %>%
    assign(paste0("datadjust", i), ., envir = .GlobalEnv)
}

#creating a vector to manually assign colors later in the visualization 
cols <- c("Boston" = "orangered2", 
          "Chicago"= "gold2", 
          "Plano"= "dodgerblue3", 
          "Arlington"="magenta3", 
          "Estero" = "steelblue1",
          "US"="black")
names <- c("Boston"="Boston, MA", "Chicago"= "Chicago, IL", 
           "Plano"= "Plano, TX" , "Arlington" = "Arlington, VA", 
           "Estero"= "Estero, FL", "US"= "U.S. Average")



### EVERYTHING BELOW IS THE ACTUAL SHINY APP

# Defining the User Interface:

ui <- fluidPage( 
  titlePanel("Corporate Relocation and Housing Prices"), #The title shown at the top of the app
  
  fluidRow(
    column(3,
           column(7, 
                  fluidRow(
                    checkboxGroupInput("cityinput", "Housing Markets:", 
                                       c("Boston"="Boston", "Plano"="Plano", 
                                         "Estero"="Estero", "Chicago"="Chicago",
                                         "Arlington"="Arlington", "U.S. Average"="US" ),
                                       selected = c("Boston"="Boston", "Plano"="Plano", 
                                                    "Estero"="Estero", "Chicago"="Chicago",
                                                    "Arlington"="Arlington", "U.S. Average"="US" )
                    )
                  ),
                  
                  #This part is the slider to change the years shown (altering the x-axis):
                  fluidRow(
                    sliderInput("yearrange", "Year Range:", min=1996, max=2018, value=c(1996,2018), ticks=FALSE, sep="")
                  ),
                  
                  #This selector changes the index year
                  fluidRow(
                    selectInput("yearindex", "Index Base Year:", c("No Index (True Values)"="true", 1996:2018), selected="true")
                  ),
                  
                  #Selecting whether to control for U.S. housing prices
                  fluidRow(
                    checkboxInput("controlUS", "Control for Overall U.S. Housing Trends", value=F)
                  ),
                  
                  #Selecting whether or not to show the announcement date on the graph
                  fluidRow(
                    checkboxInput("corpdata1", "Display Announcement Date", value=T)
                  ),
                  
                  #Selecting whether or not to show the announcement date on the graph
                  fluidRow(
                    checkboxInput("corpdata2", "Display Date of Opening", value=T))
           )
    ),
    
    #Designating the space where our plot will go:
    column(6, 
           plotOutput("plot")),
    
    #Creating the space for the extra info on the companies
    column(3, 
           fluidRow(column(10, p(textOutput("Arlingtontext")))),
           fluidRow(column(10, p(textOutput("Bostontext")))),
           fluidRow(column(10, p(textOutput("Chicagotext")))),
           fluidRow(column(10, p(textOutput("Esterotext")))),
           fluidRow(column(10, p(textOutput("Planotext"))))
    )
  ))


# Defining server logic:
server <- function(input, output) { 
  
  output$plot <- renderPlot({ 
    
    get(paste0("dat", if(input$controlUS){"adjust"}, input$yearindex))[ifelse(min(input$yearrange)==1996, 1 , (((min(input$yearrange)-1996)*72)-17)) :
                                                                         ifelse(max(input$yearrange)==1996, 54 , (((max(input$yearrange)-1996)*72)+54)), ] %>%
      #the line above selects the data set based on what year the user wants to use as the index
      #the ifelse statements within the brackets then only includes entries that fall into the selected date range
      
      filter(City %in% input$cityinput) %>% #using filter to only show the cities selected cities
      
      ggplot(aes(x=Date)) + #building the graphic
      
      #Adjusting the y-axis label based on the index year
      labs(y= ifelse(input$yearindex == "true" & !input$controlUS, expression(atop("Zillow Home Value Index", "(thousands of dollars)")), 
                     ifelse(input$yearindex == "true" & input$controlUS, expression(atop("Zillow Home Value Index","(divided by average U.S. value)")),
                            ifelse(input$controlUS, expression(atop("Zillow Home Value Index", "(U.S.-adjusted & indexed to selected year)")),
                                   expression(atop("Zillow Home Value Index", "(indexed to selected year)"))
                            )
                     )
      ),
      caption = "Note: This visualization alone does not necessarily imply any causal \nrelationships between housing prices and corporate relocations. More \nrigourous econometric analysis is needed to draw such conclusions."
      ) + 
      
      
      #Making the lines, grouping by City:
      geom_line(mapping=aes(y=Value, color=City), size = 1) +
      
      #Using the previously-made cols vector to manually assign colors
      scale_color_manual(values = cols, labels = names) +
      
      theme(legend.text = element_text(size=13),
            legend.title = element_text(size=15, face="bold"),
            axis.title = element_text(size=13),
            plot.caption = element_text(size=10, hjust=0, vjust=1)) +
      
      #Building the Boston/GE line and label in the plot
            {if(("Boston" %in% input$cityinput) & (input$corpdata1) & (2016 <= max(input$yearrange)) & (2016 >= min(input$yearrange)))
              geom_vline(xintercept=as.numeric(as.Date("2016-01-13")), colour="orangered3", linetype="dotdash")} +
      #{if(("Boston" %in% input$cityinput) & (input$corpdata) & (2016 <= max(input$yearrange)) & (2016 >= min(input$yearrange)))
      #annotate(geom="text", x=as.Date("2016-01-13"), 
      #y= -Inf,
      #label="GE Announces Move to Boston",
      #angle= 90,
      #hjust=0,
      #vjust=1,
      #color="orangered3",
      # size=3.5)} +
      
      #Making the Chicago/Conagra lines and labels in the plot
              {if(("Chicago" %in% input$cityinput) & (input$corpdata1) & (2015 <= max(input$yearrange)) & (2015 >= min(input$yearrange)))
                geom_vline(xintercept=as.numeric(as.Date("2015-10-01")), colour="gold2", linetype="dotdash")} +
      #{if(("Chicago" %in% input$cityinput) & (input$corpdata) & (2015 <= max(input$yearrange)) & (2015 >= min(input$yearrange)))
      #annotate(geom="text", x=as.Date("2015-10-01"), 
      #y= -Inf,
      #label="Conagra Announces Move to Chicago",
      #angle=90,
      #hjust=0,
      #vjust=1,
      #color="gold4",
      #size=3.5)} +
                {if(("Chicago" %in% input$cityinput) & (input$corpdata2) & (2016 <= max(input$yearrange)) & (2016 >= min(input$yearrange)))
                  geom_vline(xintercept=as.numeric(as.Date("2016-06-01")), colour="gold2", linetype="longdash")} +
      #{if(("Chicago" %in% input$cityinput) & (input$corpdata) & (2016 <= max(input$yearrange)) & (2016 >= min(input$yearrange)))
      #annotate(geom="text", x=as.Date("2016-06-01"), 
      #y= -Inf,
      #label="New Conagra HQ Opens",
      #angle=90,
      #hjust=0,
      #vjust=1,
      #color="gold4",
      #size=3.5)} +
      
      
    #Making the Arlington/Nestle lines and labels in the plot
                  {if(("Arlington" %in% input$cityinput) & (input$corpdata1) & (2017 <= max(input$yearrange)) & (2017 >= min(input$yearrange)))
                    geom_vline(xintercept=as.numeric(as.Date("2017-01-01")), colour="magenta4", linetype="dotdash")} +
      #{if(("Arlington" %in% input$cityinput) & (input$corpdata) & (2017 <= max(input$yearrange)) & (2017 >= min(input$yearrange)))
      #annotate(geom="text", x=as.Date("2017-01-01"), 
      #y= -Inf,
      #label="Nestle Announces Move to Arlington",
      #angle=90,
      #hjust=0,
      #vjust=1,
      #color="magenta4",
      #size=3.5)}  +
      
      #Making the Estero/Hertz lines and labels in the plot
                    {if(("Estero" %in% input$cityinput) & (input$corpdata1) & (2013 <= max(input$yearrange)) & (2013 >= min(input$yearrange)))
                      geom_vline(xintercept=as.numeric(as.Date("2013-05-01")), colour="steelblue1", linetype="dotdash")} +
      #{if(("Estero" %in% input$cityinput) & (input$corpdata) & (2013 <= max(input$yearrange)) & (2013 >= min(input$yearrange)))
      #annotate(geom="text", x=as.Date("2013-05-01"), 
      #y= -Inf,
      #label="Hertz Announces Move to Estero",
      #angle=90,
      # hjust=0,
      #vjust=1,
      #color="steelblue4",
      #size=3.5)} +
                      {if(("Estero" %in% input$cityinput) & (input$corpdata2) & (2016 <= max(input$yearrange)) & (2015 >= min(input$yearrange)))
                        geom_vline(xintercept=as.numeric(as.Date("2016-01-01")), colour="steelblue1", linetype="longdash")} +
      #{if(("Estero" %in% input$cityinput) & (input$corpdata) & (2016 <= max(input$yearrange)) & (2015 >= min(input$yearrange)))
      #annotate(geom="text", x=as.Date("2016-01-01"), 
      #y= -Inf,
      #label="New Hertz HQ Opens",
      #angle=90,
      #hjust=0,
      #vjust=1,
      #color="steelblue4",
      #size=3.5)} +
      
      #Making the Plano/Toyota lines and labels in the plot
                        {if(("Plano" %in% input$cityinput) & (input$corpdata1) & (2014 <= max(input$yearrange)) & (2014 >= min(input$yearrange)))
                          geom_vline(xintercept=as.numeric(as.Date("2014-04-01")), colour="dodgerblue4", linetype="dotdash")} +
      #{if(("Plano" %in% input$cityinput) & (input$corpdata) & (2014 <= max(input$yearrange)) & (2014 >= min(input$yearrange)))
      #annotate(geom="text", x=as.Date("2014-04-01"), 
      #y= -Inf,
      #label="Toyota Announces Move to Plano",
      #angle=90,
      #hjust=0,
      #vjust=1,
      #color="dodgerblue4",
      #size=3.5)} +
                          {if(("Plano" %in% input$cityinput) & (input$corpdata2) & (2017 <= max(input$yearrange)) & (2017 >= min(input$yearrange)))
                            geom_vline(xintercept=as.numeric(as.Date("2017-07-01")), colour="dodgerblue4", linetype="longdash")} 
    #{if(("Plano" %in% input$cityinput) & (input$corpdata) & (2017 <= max(input$yearrange)) & (2017 >= min(input$yearrange)))
    #annotate(geom="text", x=as.Date("2017-07-01"), 
    #    y= -Inf,
    #    label="New Toyota HQ Opens",
    #    angle=90,
    #   hjust=0,
    #   vjust=1,
    #   color="dodgerblue4",
    #   size=3.5)}
  })
  
  #Adding the paragraph output for info on the GE relocation to Boston
  output$Bostontext <- renderText({
    {if(("Boston" %in% input$cityinput))"Information about Boston and GE goes here. This is a test to see what the formatting looks like for the paragraph. This space will have actually useful information in the final version."}
  })
  
  #Adding the paragraph output for info on the Conagra relocation to Chicago
  output$Chicagotext <- renderText({
    {if(("Chicago" %in% input$cityinput))"Information about Chicago and Conagra goes here. This is a test to see what the formatting looks like for the paragraph. This space will have actually useful information in the final version."}
  })
  
  #Adding the paragraph output for info on the Nestle relocation to Arlington
  output$Arlingtontext <- renderText({
    {if(("Arlington" %in% input$cityinput))"Information about Arlington and Nestle goes here. This is a test to see what the formatting looks like for the paragraph. This space will have actually useful information in the final version."}
  })
  
  #Adding the paragraph output for info on the Toyota relocation to Plano
  output$Planotext <- renderText({
    {if(("Plano" %in% input$cityinput))"Information about Plano and Toyota goes here. This is a test to see what the formatting looks like for the paragraph. This space will have actually useful information in the final version."}
  })
  
  #Adding the paragraph output for info on the Hertz relocation to Estero
  output$Esterotext <- renderText({{if(("Estero" %in% input$cityinput))"Information about Estero and Hertz goes here. This is a test to see what the formatting looks like for the paragraph. This space will have actually useful information in the final version."}
  })
}

# Running the app using ui and server (the input and output that we defined)
shinyApp(ui = ui, server = server) 