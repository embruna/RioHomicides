####
####


library(ggplot2)
library(dplyr)
library(tidyr)

#Clear out everything from the environment
rm(list=ls())

######################################################
######################################################
### DATA ENTRY AND CLEANUP
######################################################
######################################################

#Step 1: load the individual CSV files and save them as dataframes

homicides2013<-read.csv("./Data/Homicides_2013.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )
homicides2014<-read.csv("./Data/Homicides_2014.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )
homicides2015<-read.csv("./Data/Homicides_2015.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )
homicides2016<-read.csv("./Data/Homicides_2016.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )

# Convert from Wide format to Long format

H2013<-gather(homicides2013, "month", "homicides", 3:14)
H2013["year"] <- 2013 

H2014<-gather(homicides2014, "month", "homicides", 3:14)
H2014["year"] <- 2014 

H2015<-gather(homicides2015, "month", "homicides", 3:14)
H2015["year"] <- 2015 

H2016<-gather(homicides2016, "month", "homicides", 3:14)
H2016["year"] <- 2016 

#

homicides<-rbind(H2013,H2014,H2015, H2016)

# Convert years and months into ordered factors
homicides$year<-as.factor(homicides$year)
homicides$year<- ordered(homicides$year, levels = c("2013", "2014", "2015", "2016"))

homicides$month<-as.factor(homicides$month)
homicides$month<- ordered(homicides$month, levels = c("jan", "feb", "march", "april","may", "june", 
                                                      "july", "aug","sept", "oct", "nov", "dez"))

# Convert to a dataframe and delete missing months from 2016
homicides<-as.data.frame(homicides)
homicides[complete.cases(homicides),]



######################################################
######################################################
### DATA SUMMARIES
######################################################
######################################################


# Homicides per year (2016 not included)
YearlyHom<-homicides %>% group_by(year) %>% summarize(total.homicides=sum(homicides))

# Homicides per year, month
MonthlyHom<-homicides %>% group_by(year, month) %>% summarize(total.homicides=sum(homicides))

# Total Homicides per month per year (same as above, but easier to compare each month for different years)
TotalMonthlyHom<-homicides %>% group_by(month, year) %>% summarize(total.homicides=sum(homicides))

# Cumulative Homicides over the course of the year, by year
CumulativeHom<-homicides %>% group_by(year, month) %>% summarize(total.homicides=sum(homicides))
CumulativeHom<-CumulativeHom %>% group_by(year) %>% mutate(cumulative.homicides=cumsum(total.homicides))




######################################################
######################################################
### DATA VISUALIZATION
######################################################
######################################################


CumHomPlot<-ggplot(CumulativeHom, aes(x = month, y = cumulative.homicides, col=year, group=year)) + 
  #geom_line()+
  geom_point(shape=16, size = 3) +
  theme_classic() +
  ylab("Cumulative Homicides") +
  xlab("month")+
  ggtitle("Cumulative Homicides, Rio de Janeiro, 2013-2016") +
  geom_smooth(method=lm,se=FALSE) +    # Add linear regression lines
  scale_colour_manual(values=c("darkred", "darkblue", "black", "orange"))  #I chose my own colors for the lines

CumHomPlot

# Mojnthly with regression
MonthlyHomPlot<-ggplot(MonthlyHom, aes(x = month, y = total.homicides, col=year, group=year)) + 
  #geom_line()+
  geom_point(shape=16, size = 3) +
  theme_classic() +
  ylab("Homicides") +
  xlab("month")+
  ggtitle("Homicides per Month, Rio de Janeiro (2013-2016)") +
  geom_smooth(method=lm,se=FALSE) +    # Add linear regression lines
  scale_colour_manual(values=c("darkred", "darkblue", "black", "orange"))  #I chose my own colors for the lines

MonthlyHomPlot


#Monthly without regression
MonthlyHomPlot2<-ggplot(MonthlyHom, aes(x = month, y = total.homicides, col=year, group=year)) + 
  geom_line()+
  geom_point(shape=16, size = 3) +
  theme_classic() +
  ylab("Homicides") +
  xlab("month")+
  ggtitle("Homicides per Month, Rio de Janeiro (2013-2016)") +
  scale_colour_manual(values=c("darkred", "darkblue", "black", "orange"))  #I chose my own colors for the lines

MonthlyHomPlot2
