#lab 6
s = c(0.55,-0.41, -0.11, -1.15, 0.22, 0.57, -0.23, -1.07, 1.06, 1.38, 1.08, 0.18, -0.79, -1.35, -1.37, 0.67, 0.17, 1.48, -0.89, 0.60)
#1 bootstrap resampling
#install.packages("boot",dependencies = TRUE)
library(boot)

boot_data_calc_mean=function(data,idx){
  mean(data[idx])
}
bootstrap_data <- boot(s, boot_data_calc_mean, R = 200)
bootstrap_data # bias , SE
mean(bootstrap_data[["t"]]) # sample mean of replicates

# Confidence Interval
boot.ci(boot.out = bootstrap_data, type = "perc")


#2 jackknife
#install.packages("bootstrap",dependencies = TRUE)
library(bootstrap)
theta = function(x){mean(x)}
jackmean = jackknife(s,theta)
jack_es = mean(jackmean$jack.values) # Sample mean
meanjack = mean(s) - jackmean$jack.bias # Bias-corrected jackknife estimate
jackmean$jack.bias # bias
meanjack # Bias-corrected jackknife estimator for mean
jackmean$jack.se # SE

# Calculate 95% confidence interval for the sample mean
LowerCI = meanjack-1.96* jackmean$jack.se
upperCI = meanjack+1.96* jackmean$jack.se
c(LowerCI,upperCI)


#3 parametric bootstrap (Normal SD=1)
for (i in 1:2000){
  boot_sampl = rnorm(100, mean = mean(s), sd = 1)
  thetahat_es[i] = mean(boot_sampl)
}
boot_thet_par_es = mean(thetahat_es)
boot_thet_par_es # Sampling dist mean

# empirical CI
deltastar = thetahat_es - mean(s)
d = quantile(deltastar, c(.025,.975))
ci = mean(s) - c(d[2], d[1])
ci

