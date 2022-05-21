#Lab7
#1 random sample from a gamma distribution n = 20 Method of moments
rs  = c(1.3403,1.8915,4.1428,1.8450,3.0095,2.1611,4.8945,3.4758,2.8798,2.1612,1.8716,1.3810,1.4057,3.5031,1.5061,3.2319,1.4956,1.7329,3.1107,1.3254) 
mean(rs)
beta_est = (sum(rs*rs)/(20*mean(rs))) - mean(rs)
alpha_est = mean(rs)/beta_est
alpha_est
beta_est

#2 random sample from an exponential distribution n = 20 MLE
rs2 = c(0.5708,0.3957,0.1829,0.9300,0.8252,1.4966,0.2008,2.7691,0.1393,0.3103,0.0559,0.0413,0.4433,0.2934,0.4825,0.6840,0.3299,0.5206,1.0475,0.5522)
NLL = function(pars, data) {
  # Extract parameters from the vector
  rate = pars[1]
  # Calculate Negative Log-LIkelihood
  NLL= -sum(dexp(x = data, rate = rate, log = TRUE))
}
mle = optim(par = c(rate = .5), fn = NLL, data = rs2,control = list(parscale = c(rate = .5)))
mle$par # mle estimate for lambda


#3 Likelihood-based confidence intervals
p = seq(0, 3, by = 0.01) # range of chi-squared is [0,inf] and mle estimator is 1.62
Llikhood = numeric(length( p ))
likva = function(p, data) {
  sum(dexp(x = data, rate = p, log = TRUE))
}
for (i in 1:length(p))
{
  Llikhood[i] = likva(p[i],rs2)
}

library(ggplot2)
likeResults = data.frame(Proportion = p, Llikhood = Llikhood)
ggplot(data = likeResults, aes(x = Proportion,y = Llikhood)) +
  geom_line()+geom_hline(yintercept = max(Llikhood), linetype = "dashed")
phat = p[Llikhood == max(Llikhood)]
phat

Chi_value= qchisq(0.05, df=1, lower.tail=FALSE)/2
lower = max(p[p <= phat & Llikhood <= max(Llikhood) - 1.92] )
upper = min( p[p >= phat & Llikhood <= max(Llikhood) - 1.92] ) 
lower
upper


