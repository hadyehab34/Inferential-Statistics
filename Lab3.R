install.packages("detzrcr", dependencies = TRUE)

library(dplyr)
library(ggplot2)
library(detzrcr)
library(gcookbook)

data(heightweight)
heightweight
population = heightweight$heightIn
mu = mean(population); mu
sigma = sd(population); sigma


population = data.frame(heightweight)
sampl=sample_n(population,size = 50)
mean_sampl=sum(sampl$heightIn)/length(sampl$heightIn)
mean_sampl
mean(sampl$heightIn)
s = sd(sampl$heightIn)
length(sampl$heightIn)#50

#1
t = qt(p = 0.975, df = 49) #t-value 
lower = mean_sampl - t * (s/sqrt(50))
upper = mean_sampl + t * (s/sqrt(50))



#2
population = data.frame(heightweight)
sampl=sample_n(population,size = 200)
mean_est=sum(sampl$heightIn)/length(sampl$heightIn)
sum((sampl$heightIn-mean_est)^2)/ length (sampl$heightIn) #Estimated plug-in variance
var(sampl$heightIn)


