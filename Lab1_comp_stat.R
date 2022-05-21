#Lab 1 Excercises

#Population
ames %>%
  summarise(mu = mean(price), pop_med = median(price), 
            sigma = sd(price), pop_iqr = IQR(price),
            pop_min = min(price), pop_max = max(price),
            pop_q1 = quantile(price, 0.25), # first quartile, 25th percentile
            pop_q3 = quantile(price, 0.75)) # third quartile, 75th percentile


#1
ames %>%
  sample_n(size = 50) %>%
  summarise(x_bar = mean(price))

#2
sample_means50 <- ames %>%
  rep_sample_n(size = 50, reps = 5000, replace = TRUE) %>%
  summarise(x_bar = mean(price))
ggplot(data = sample_means50, aes(x = x_bar)) + geom_histogram(binwidth = 20)
mean(sample_means50$x_bar)

#3 LAB1
sample_means150 <- ames %>%
  rep_sample_n(size = 150, reps = 5000, replace = TRUE) %>%
  summarise(x_bar = mean(price))
ggplot(data = sample_means150, aes(x = x_bar)) + geom_histogram(binwidth = 20)
mean(sample_means150$x_bar)

#4
ames %>%
  sample_n(size = 15) %>%
  summarise(x_bar = mean(price))

#5
sample_means15 <- ames %>%
  rep_sample_n(size = 15, reps = 2000, replace = TRUE) %>%
  summarise(x_bar = mean(price))
ggplot(data = sample_means15, aes(x = x_bar)) + geom_histogram(binwidth = 20)
mean(sample_means15$x_bar)
#Population
ames %>%
  summarise(mu = mean(price))

#6
sample_means150 <- ames %>%
  rep_sample_n(size = 150, reps = 2000, replace = TRUE) %>%
  summarise(x_bar = mean(price))
ggplot(data = sample_means150, aes(x = x_bar)) + geom_histogram(binwidth = 20)
mean(sample_means150$x_bar)

