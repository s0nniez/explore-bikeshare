
#import libraries
library(readr)
library(lubridate)
library(ggplot2)
library(tidyverse)

#import datasets
ny = read.csv('new_york_city.csv')
wash = read.csv('washington.csv')
chi = read.csv('chicago.csv')

#check ny dataset
head(ny)

#check summary of ny dataset
summary(ny)

#check datatypes of ny datasets
str(ny)

#check wash dataset
head(wash)

#check datatypes of wash dataset
str(wash)

#check summary of wash dataset
summary(wash)

#check chi dataset
head(chi)

#check datatypes of chi dataset
str(chi)

#check summary of chi dataset
summary(chi)

#remove rows with NA
ny <- na.omit(ny) 
wash <- na.omit(wash) 
chi <- na.omit(chi) 

#extract Hour from Start.Time
ny$`Start.Time` <- format(as.POSIXct(ny$`Start.Time`), format = "%H")

#check dataset
head(ny)

#extract Hour from Start.Time
wash$`Start.Time` <- format(as.POSIXct(wash$`Start.Time`), format = "%H")

#check dataset
head(wash)

#extract Hour from Start.Time
chi$`Start.Time` <- format(as.POSIXct(chi$`Start.Time`), format = "%H")

#check dataset
head(chi)

#import external crosstab function
source("http://pcwww.liv.ac.uk/~william/R/crosstab.r")

#display crosstab of hour and count for new york
crosstab(ny, row.vars = "Start.Time")

#display crosstab of hour and count for washington
crosstab(wash, row.vars = "Start.Time")

#display crosstab of hour and count for chicago
crosstab(chi, row.vars = "Start.Time")

#create barplot
ggplot(ny, aes(x=ny$Start.Time)) + 
geom_bar(stat = "count", fill="red", color="black") +
ggtitle('Most Popular Bike Time for New York') + 
labs(y = 'Count of Start Hour', x = 'Hour')

#create barplot
ggplot(wash, aes(x=wash$Start.Time)) + 
geom_bar(stat = "count", fill="blue", color="black") +
ggtitle('Most Popular Bike Time for Washington') + 
labs(y = 'Count of Start Hour', x = 'Hour')

#create barplot
ggplot(chi, aes(x=chi$Start.Time)) + 
geom_bar(stat = "count", fill="green", color="black") +
ggtitle('Most Popular Bike Time for Chicago') + 
labs(y = 'Count of Start Hour', x = 'Hour')

#getting age from Birth.Year using 2021 as the target year
ny$age <- 2021-ny$Birth.Year

#check dataset
head(ny)

#create crosstab of age and count
crosstab(ny, row.vars = "age")

#create histogram of age distribution in new york
hist(ny$age,
    xlim=c(20,90),
    xlab="Age",
    ylab="Count of Bikers",
    main="Age of Bikers in New York",
    col="red")

#getting age from Birth.Year using 2021 as the target year
chi$age <- 2021-chi$Birth.Year

#check dataset
head(chi)

#create crosstab of age and count
crosstab(chi, row.vars = "age")

#create histogram of age distribution in chicago
hist(chi$age,
    xlim=c(19,90),
    xlab="Age",
    ylab="Count of Bikers",
    main="Age of Bikers in Chicago",
    col="green")

#find the mean of trip duration by gender in new york. trip duration is current in seconds, will convert to minutes
aggregate(ny$Trip.Duration/30, list(ny$Gender), FUN=mean) 

#since there are nulls in some rows, we will omit those
ny <- ny[!(is.na(ny$Gender) | ny$Gender==""), ]

#check if nulls still exist
aggregate(ny$Trip.Duration/30, list(ny$Gender), FUN=mean) 

#find the mean of trip duration by gender in chicago. trip duration is current in seconds, will convert to minutes
aggregate(chi$Trip.Duration/30, list(chi$Gender), FUN=mean) 

#since there are nulls in some rows, we will omit those
chi <- chi[!(is.na(chi$Gender) | chi$Gender==""), ]

#check if nulls still exist
aggregate(chi$Trip.Duration/30, list(chi$Gender), FUN=mean) 

#create barplot for new york
ggplot(ny, aes(x=ny$Gender, y=ny$Trip.Duration/30)) + 
geom_bar(stat = "summary", fill="red", color="black", fun.y="mean") +
ggtitle('Riding Time Average by Gender in New York') + 
labs(y = 'Average Minutes', x = 'Gender') +

#create barplot for chicago
ggplot(chi, aes(x=chi$Gender, y=chi$Trip.Duration/30)) + 
geom_bar(stat = "summary", fill="green", color="black", fun.y="mean") +
ggtitle('Riding Time Average by Gender in New York') + 
labs(y = 'Average Minutes', x = 'Gender')

system('python -m nbconvert Explore_bikeshare_data.ipynb')
