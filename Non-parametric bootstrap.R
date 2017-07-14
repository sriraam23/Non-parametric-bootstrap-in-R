#Reading the CSV file
data = read.csv("CPU_Times.csv")
#Calculating mean of the sample provided
avg = mean(data$CPU_Time)
#Calculating theta = log(mean) of the sample provided
theta = log(avg)
#function to create a new bootstrap sample
result = function(){
  x = sample(data$CPU_Time, 30, replace = TRUE)
  theta_hat = log(mean(x))
  return(theta_hat)
}
#Sampling
x = replicate(1000, result())
#Calculating bias and SE of theta_hat
bias_theta_hat = mean(x) - theta
SE_theta_hat = sqrt(var(x))
#Calculating 2.5th and 97.5th percentile of theta hat
theta_hat_2.5 = quantile(x, 0.025)
theta_hat_97.5 = quantile(x, 0.975)
#Calculating 2.5th and 97.5th percentile of theta hat - theta
theta_hat_minus_theta_2.5 = theta_hat_2.5 - theta
theta_hat_minus_theta_97.5 = theta_hat_97.5 - theta
#Calculating 95%CI using normal approximation
L1 = theta - bias_theta_hat - qnorm(0.975)*SE_theta_hat
U1 = theta - bias_theta_hat - qnorm(0.025)*SE_theta_hat
#Calculating 95%CI using basic bootstrap
L2 = 2*theta - quantile(x, 0.975)
U2 = 2*theta - quantile(x, 0.025)
#Calculating 95%CI using percentile bootstrap
L3 = quantile(x, 0.025)
U3 = quantile(x, 0.975)