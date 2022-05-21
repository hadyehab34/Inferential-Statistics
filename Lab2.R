install.packages("dplyr", dependencies = TRUE)
install.packages("ggplot2", dependencies = TRUE)
install.packages("ggpubr", dependencies = TRUE)
install.packages("gcookbook", dependencies = TRUE)
#STEP 2: the step’s objective is to load the required packages
library(dplyr)
library(ggplot2)
library(ggpubr)
library(gcookbook)


#Lab2 Exercises

data(heightweight)
heightweight$BMI = (heightweight$weightLb /((heightweight$heightIn)^2)) * 703
#Rounding to first decimal
heightweight = heightweight %>% mutate_if(is.numeric, round, 1)

ggplot(heightweight, aes(x = BMI)) +
  stat_ecdf()

#3
fun.ecdf <- ecdf(heightweight$BMI)
my.ecdf <- fun.ecdf(sort(heightweight$BMI))

# summary(ecdf(heightweight$BMI))
# Empirical CDF:	  80 unique values with summary
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 13.60   16.98   18.95   19.30   21.23   26.30 
# > a = sort(heightweight$BMI)
# > which(a==18.5)
# [1] 119 120 121
# > my.ecdf[120]
# [1] 0.5127119
# > my.ecdf[119]
# [1] 0.5127119
# > my.ecdf[121]
# [1] 0.5127119
# > which(a==29.9)
# integer(0)
# > which(a==25)
# integer(0)
# > which(a==25.2)
# [1] 233 234
# > my.ecdf[233]
# [1] 0.9915254
# > 1 - my.ecdf[233]
# [1] 0.008474576
# > Ecdf.default