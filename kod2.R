library("readr")
library("readxl")
library("tidyverse")
library("gapminder")
library("dplyr")
library("ggthemes")
library("ggplot2")
library("graphics")
library("lattice")
library("latticeExtra")
library("plotly")


#pobieranie danych 
{
  zachorowania <- read.delim("Stepniewski_dane_zmienione_zachorowania.txt", 
                             col.names = c("country","country_code","continent","population","indicator",
                                           "weekly_count","week","rate_14_day","cumulative_count","source"),
                             sep = ",")
  zgony <- read.delim("Stepniewski_dane_zmienione_zgony.txt", 
                      col.names = c("country","country_code","continent","population","indicator",
                                    "weekly_count","week","rate_14_day","cumulative_count","source"),
                      sep = ",")
 
  kraje <- c("Poland", "Germany", "Spain", "Finland", "Greece", "Hungary", "Iceland", "Italy", "Norway", "Malta")
   
  zgony_europa <- filter(zgony, (is.element(zgony$country, kraje)))
  
  zgony_europa_53 <- filter(zgony, zgony$week=="53" & (is.element(zgony$country, kraje)))
   
  zachorowania_europa <- filter(zachorowania, (is.element(zachorowania$country, kraje)))
}


kolory<-c("Poland"="red", "Germany" ="green", "Spain"="blueviolet", "Finland"="orange", 
          "Greece"="red2", "Hungary"="black", "Iceland"="brown", "Italy"= "blue",
          "Norway"="yellow", "Malta"="red4")


#ggplot
{
  zachorowania_plot = ggplot() +     
    ggtitle("Iloœæ zachorowañ w poszczególnych pañstwach") +
    xlab('Tydzieñ') +
    ylab('Iloœæ zachorowañ')+scale_colour_manual("",values=kolory)
    
  for( i in 1:10){
    zachorowania_i <- filter(zachorowania, zachorowania$country == kraje[i])
    zachorowania_plot<-zachorowania_plot + geom_line(data = zachorowania_i, aes(x = week, y = weekly_count, color = kolory))
  }
  
  #zachorowania_plot<-zachorowania_plot+scale_colour_manual("",values=kolory)
  
  print(zachorowania_plot)
}

