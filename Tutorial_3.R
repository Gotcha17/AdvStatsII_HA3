
# Problem1 / Home Assignment 2 --------------------------------------------

# define variables
set.seed(666)
MC = 100
n = 1:100
theta = c(0.1, 1, 5, 25)

# Part a)
T_n = function(x){
  t = mean(x)
  return(t)
}

Tbar_n = function(x){
  t = sqrt(mean(x^2)/2)
  return(t)
}

# Part b)
MSE = function(x, m){
  bias = mean(x)-m
  var = var(x)
  MSE = bias^2+var
  return(c(bias, var, MSE))
}

# Part c)
path = file.path(getwd(), paste("Problem_1_c.pdf")) #simplified syntax
pdf(file=path)
par(mfrow = c(2,2))
for (i in theta){
  Y1 = c()
  Y2 = c()
  for (j in n){
    #since in R exp dist is defined differently as in formulary for adv stats I therefore
    #we must define parameter as 1/theta and not just simly rate=theta
    X1 = replicate(MC, T_n(x = rexp(j, rate = 1/i)))
    X2 = replicate(MC, Tbar_n(x = rexp(j, rate = 1/i)))
    Y1[j]=MSE(x = X1, m = i)[3] #only third element of mse output function is of interest
    Y2[j]=MSE(x = X2, m = i)[3]
  }
  #bquote is used to display the greek letter for theta//theta is equal to values of i
  plot(n, Y1, col = "green", type = "l", xlab = "n", ylab = "MSE", main = bquote(theta == .(i)), lwd = 2)
  lines(n, Y2, col = "blue", lwd = 2)
  legend("topright", legend = c("T_n", "T'_n"), fill = c("green", "blue"))
  box()
}
dev.off()

# Part d)
path = file.path(getwd(), paste("Problem_1_d.pdf")) #simplified syntax
pdf(file=path)
par(mfrow = c(2,2))
for (i in theta){
  Y1 = c()
  Y2 = c()
  for (j in n){
    #since in R exp dist is defined differently as in formulary for adv stats I therefore
    #we must define parameter as 1/theta and not just simly rate=theta
    X1 = replicate(MC, T_n(x = rexp(j, rate = 1/i)))
    X2 = replicate(MC, Tbar_n(x = rexp(j, rate = 1/i)))
    Y1[j]=MSE(x = X1, m = i)[2] #only third element of mse output function is of interest
    Y2[j]=MSE(x = X2, m = i)[2]
  }
  yratio = Y1/Y2 # To see which estimator has a larger variance for different n
  #bquote is used to display the greek letter for theta//theta is equal to values of i
  plot(n, yratio, col = "green", type = "l", xlab = "n", ylab = "Variance ratio", main = bquote(theta == .(i)), lwd = 2)
  #lines(n, Y2, col = "blue", lwd = 2)
  #legend("topright", legend = c("T_n", "T'_n"), fill = c("green", "blue"))
  box()
}
dev.off()

# Problem 2 ---------------------------------------------------------------

#CRLB is simply sigma^2/n if estimator is unbiased (see P&P Tutorial 3)

# Part a)
estim = function(x){
  mu1 = sum(x)/(length(x)-1)
  mu2 = sum(x[1:3])/3
  mu3 = mean(x)
  return(c(mu1, mu2, mu3))
}

# Part b)
CRLB = function(sigma, m){
  t = sigma^2/m
  return(t)
}

# Part c)
MC = 1000
n = 3:100
mu = c(0, 1, 5)
sigma = c(1, 2, 5)

for (i in 1:3){
  path = file.path(getwd(), paste("Problem_2_", i, ".pdf", sep = ""))
  pdf(file = path)
  par(mfrow = c(3,3))
  for (j in mu){
    for (k in sigma){
      y = c()
      for (l in n){
        y[l-2] = var(replicate(MC, estim(x = rnorm(l, mean = j, sd = k))[i]))
      }
      plot(n,y, type = "l", col = "blue", xlab = "n", ylab = "variance", ylim = c(0, max(y)+0.2),
           main = substitute(paste(mu, "=", u, ",", sigma, "=", v), list(u=j, v=k)))
      lines(n, CRLB(sigma = k, m = n), col = "green")
      legend("topright", legend = c("estimator", "CRLB"), fill = c("blue", "green"))
      box()
    }
  }
  dev.off()
}


