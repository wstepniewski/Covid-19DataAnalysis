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


#loading data 
{
  infections <- read.delim("Stepniewski_dane_zmienione_zachorowania.txt", 
                             col.names = c("country","country_code","continent","population","indicator",
                                           "weekly_count","week","rate_14_day","cumulative_count","source"),
                             sep = ",")
  deaths <- read.delim("Stepniewski_dane_zmienione_zgony.txt", 
                      col.names = c("country","country_code","continent","population","indicator",
                                    "weekly_count","week","rate_14_day","cumulative_count","source"),
                      sep = ",")
 
  #my_countries <- c("Poland", "Germany", "Spain", "Finland", "Greece", "Hungary", "Iceland", "Italy", "Norway", "Malta")
   
  my_countries<-c('Finland', 'Germany','Greece', 'Hungary', 
                'Iceland', 'Italy', 'Malta', 'Norway', 'Poland', 'Spain' )
  deaths_europe <- filter(deaths, (is.element(deaths$country, my_countries)))
  
  deaths_europe_53 <- filter(deaths, deaths$week=="53" & (is.element(deaths$country, my_countries)))
   
  infections_europe <- filter(infections, (is.element(infections$country, my_countries)))
}


country_color<-c("Poland"="red", "Germany" ="green", "Spain"="blueviolet", "Finland"="orange", 
          "Greece"="red2", "Hungary"="black", "Iceland"="brown", "Italy"= "blue",
          "Norway"="yellow", "Malta"="red4")


#ggplot
{
  infections_plot = ggplot() +     
    ggtitle("Number of infections in each country") +
    xlab('Week') +
    ylab('Number of infections')+scale_colour_manual("",values=country_color)
   
  for( i in 1:10){
    infections_i <- filter(infections, infections$country == my_countries[i])
    infections_plot <- infections_plot + geom_line(aes(x = week, y = weekly_count, color = Country), 
                                                       data=cbind(Country=my_countries[i], infections_i))
  }
  
  print(infections_plot)
}


#barplot
{
  #par(mfrow=c(1,1))
  my_colors <-c('orange', 'green', 'red2', 'black', 'brown', 'blue', 'red4', 'yellow', 'red', 'blueviolet')

  barplot(deaths_europe_53$cumulative_count, col=my_colors, ylim=c(0,90000), 
          main="Summary number of deaths \nin each country",ylab="Number of deaths")
  legend("topleft", legend=my_countries, fill=my_colors, cex=0.68)
}


#xyplot
{
  xyplot(weekly_count ~ week, deaths_europe, groups = country,
         xlab="Week",
         ylab="Number of deaths",
         main="Number of deaths in each country",
         pch=19,
         cex=0.5,
         fill=my_colors,
         auto.key = list(space = "right",
                         title = "",
                         text = my_countries))
}


#plotly 
{
  poland_deaths <-filter(deaths, deaths$country == "Poland")
  poland_infections <-filter(infections, infections$country == "Poland")
    
  plot1<-ggplotly(ggplot()+
                    geom_line(data = poland_deaths, aes(x = week, y = weekly_count, color = "deaths")) +
                    geom_line(data = poland_infections, aes(x = week, y = weekly_count, color = "infections")) +
                    xlab('Tydzieñ') +
                    ylab(' ') +
                    scale_y_log10() +
                    scale_colour_manual("",values=c("deaths"="red","infections" ="orange")))
  
  
  plot2<-ggplotly(ggplot()+
                    geom_line(data = poland_deaths, aes(x = week, y = weekly_count, color = "deaths")) +
                    geom_line(data = poland_infections, aes(x = week, y = weekly_count, color = "infections")) +
                    ylab(' ') +
                    ggtitle('Infections and deaths in Poland') +
                    scale_colour_manual("",values=c("deaths"="red","infections" ="orange")))
  
  subplot(plot2, plot1, nrows=2)
}


#histogram
{
  infections_europe2<-filter(infections, infections$country == "Europe (total)")
  hist(infections_europe2$weekly_count, breaks=10, col=rainbow(5), xlab="Number of infections", ylab="",
       main="Number of weeks with a particular \nnumber of infections in Europe")
}
