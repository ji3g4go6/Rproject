# 1.
f <- function(x){
  1/sqrt(2*pi) * exp(-x^2/2)
}

integrate(f,-Inf,Inf)
range <- seq(-3,3,0.01)

g <- function(x){
  exp(-x)
}

range2 <- seq(0,3,0.01)

plot(range,f(range),type = "l", ylim = c(0,1))
lines(range2, g(range2))
lines(-range2, g(range2))

#min M = max f/g

max(f(range)/g(range)) # m

#f/g = sqrt(2/pi) * exp(-x^2/2 + x)

# d/dx = 0
# 1/sqrt(2*pi) * exp(-x^2/2 + x) * (1-x) = 0
# x = 1

M <- f(1)/g(1)
plot(range,f(range),type = "l", ylim = c(0,1))
lines(range2, M * g(range2))
lines(-range2, M * g(range2))


f_x <- rep(0,10000)

for(i in 1:length(f_x)){
  repeat{
    x <- -log(runif(1)) # g(x), use rexp(1)
    u <- runif(1)
    if(u < f(x)/(M*g(x))) {break}
  }
  
  if(runif(1) < 0.5){
    Z = x
  }else{
    Z = -x
  }
  
  f_x[i] <- Z
}

hist(f_x, prob = T)
curve(dnorm(x,0,1),add = T) # 模擬的結果接近分配實際的pdf

# 2.
x <- rt(10000, df = 3)
f <- function(x){
  1/sqrt(2*pi) * exp(-x^2/2)
}
w <- f(x) / dt(x, df = 3)

mean(w*x) # N(0,1)的期望值為0，模擬結果與真實值接近

# 3.
x <- rnorm(10000)
f <- function(x){
  2/(sqrt(3)*pi) * (1 + x^2/3)^-2
}
w <- f(x) / dnorm(x)

mean(w*x) # t(3)的期望值為0，模擬結果與真實值接近
