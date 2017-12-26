##### a) ##################################################################################################

CRLB = function(sigma, m, c=""){
  if (c==""){
    CRLB1 = sigma^2/m
    CRLB2 = 1/(sigma^2*m)
    CRLB = c(CRLB1,CRLB2)
  }
  if (c!=""){
    CRLB1 = sigma^2/m
    CRLB2 = 1/(sigma^2*m)
    CRLB3 = c^2*exp(-2*c/sigma)/(m*sigma^2)
    CRLB = c(CRLB1,CRLB2,CRLB3)
  }
  return(CRLB)
}

# Compute the Cramer-Rao-Lower-Bound for both Cases: missing or given value of parameter c
# according to results from the pen & paper-tutorial

##### b) ##################################################################################################

estimator = function(x, c=""){
  if (c==""){
    t_1 = mean(x)
    t_2 = (length(x)-1)/sum(x)
    t = c(t_1, t_2)
  }
  if (c!=""){
    t_1 = mean(x)
    t_2 = (length(x)-1)/sum(x)
    t_3 = mean(as.numeric((x>c)))
    t = c(t_1, t_2, t_3)
  }
  return(t)
}

# Evaluate the Estimators for both Cases: missing or given value of parameter c
# according to results from the pen & paper-tutorial

##### c) ##################################################################################################

# Set variables
set.seed(666)
MC=1000 
n=1:100
theta=c(0.1, 1, 2, 5)
c=c(1, 10)

# Loop over the first two estimators
for (i in 1:2){

  # PDF settings  
  path = file.path(getwd(), paste("HA3_stu127762_stu107307_stu107135_stu128161_c", i, ".pdf", sep = ""))
  pdf(file = path)
  par(mfrow = c(2,2))
  
  # Loop over all values of Theta
  for (j in theta){
      y = c()
      
      # Evaluate variance over all n
      for (l in n){
        y[l] = var(replicate(MC, estimator(x = rexp(l, rate = 1/j))[i]))
      }
      
      # Plot Results and CRLB
      plot(n, y, type = "l", col = "blue", xlab = "n", ylab = "variance",
           main = substitute(paste(theta, "=", u), list(u=j)))
      lines(n, matrix(CRLB(sigma = j, m = n),length(n),2)[,i], col = "green")
      legend("topright", legend = c("estimator", "CRLB"), fill = c("blue", "green"))
      box()
  }
  dev.off()
}

# Different PDF settings for the third estimator
path = file.path(getwd(), paste("HA3_stu127762_stu107307_stu107135_stu128161_c", "3", ".pdf", sep = ""))
pdf(file = path)
par(mfrow = c(3,3))

  # Loop over all values of theta
  for (j in theta){
    
    # Loop over all values of c
    for(k in c){
    y = c()
      
      # Compute variance over all n
      for (l in n){
        y[l] = var(replicate(MC, estimator(x = rexp(l, rate = 1/j), c = k)[3]))
      }
 
    # Plot Results and CRLB
    plot(n,y, type = "l", col = "blue", xlab = "n", ylab = "variance",
       main = substitute(paste(theta, "=", u, ", c=", v), list(u=j, v=k)))
  lines(n, matrix(CRLB(sigma = j, m = n, k),length(n),3)[,3], col = "green")
  legend("topright", legend = c("estimator", "CRLB"), fill = c("blue", "green"))
  box()
  }
}
dev.off()

##### d) ##################################################################################################

# According to the plots we can see that the Cramer-Rao inequality holds.
# Estimator 1 and 2 both reach the CRLB for all values of Theta, where for Estimator 1
# A lower value of Theta leads to a faster convergence and for Estimator 2 a bigger Value of Theta 
# leads to a faster convergence.
# For Estimator 3 and c=1 we see that the CRLB is reached and a higher value of Theta leads to a slower
# convergence rate. for c=10 with the seed we set and Theta=0.1 we don't see results for var(p(X > c)).
# For bigger Values of Theta we again see that a higher value of Theta leads to a slower convergence.
# In comparison a higher value of c leads to a faster convergence to the CRLB.