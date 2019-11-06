library(tidyverse)

#People that suffered from Hypertension
sum(df$Hypertension == TRUE)

#People that did not suffer from Stroke but were aged less than or equal to 60.
filter(df, Stroke==FALSE, Age%in%seq(1:60)) %>% nrow()

#Mean. Median. Range. Interquartile Range. Variance. Standard Deviation. Coefficient of Variation. Skewness. Kurtosis
df %>% summarize( average = mean(BMI), Median = median(BMI), Interquartile = IQR(BMI), standard_deviation = sd(BMI))
summary(df$BMI)
32.90-23.90 
#Range
62.90 - 13.20

#Variance
var(df$BMI)

#Standard Deviation    
sd(df$BMI) / mean(df$BMI)

#Sknewness and Kurtosis
library(moments)
skewness(n.sample)
kurtosis(n.sample)

#GGplot2 tasks
library(tidyverse)

# A histogram of the age of people that did not suffer from hypertension
data1 <- filter(df, Hypertension==FALSE)
ggplot(data= data1) + geom_histogram(mapping = aes(x=Age), binwidth = 5, color= 'black', fill ='white')

# A scatterplot between age and BMI and color coded by Hypertension.
ggplot(data = df) + 
  +         geom_point(mapping = aes(x = Age, y = BMI, color = Hypertension))

# A boxplot between hypertension and age.
boxplot(df$Hypertension, df$Age, main="Boxplot between Hypertension and Age")