library(ggplot2)
library(lattice)
library(MASS)
reading_colors<-c()
for(i in 1:length(sum_state_2008_2017$STATE)){
  if (sum_state_2008_2017$Creative_class_p[i]>16.85885){
    col<-"#B8E186"
  } else{
    col<-"#6ad0ee"
  }
  reading_colors<-c(reading_colors,col)
}

parallelplot(sum_state_2008_2017[,c(15,5,8,11,18)],horizontal.axis=FALSE,col=reading_colors)

