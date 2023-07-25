# Exercise 1: Discrete Random Variable

x = 1:5
min_x = 1
max_x = 5

pdf = dunif(x ,  min = min_x , max = max_x)
pdf = pdf/sum(pdf)
cdf = punif(x, min = min_x-1 , max = max_x)


plot(x, pdf, type = "h", col = "blue", lwd = 10, ylim = c(0, max(pdf, cdf)),
     ylab = "Probability", xlab = "Values", main = "Uniform Discrete Distribution")
lines(x, cdf, type = "s", col = "red", lwd = 2)
legend("topright", c("PDF", "CDF"), lty = c(1, 1), col = c("blue", "red"),
       lwd = c(10, 2), bty = "n", cex = 1.2)

samples = sample(x, size = 1000000, replace = TRUE, prob = pdf)

mean = mean(samples)
variance = var(samples)

func = function(k){k * (6 - k)}

expected_value = mean(func(samples))


plot(x, pdf, type = "h", col = "blue", lwd = 10, xlim = c(min_x - 1, max_x + 1),
     ylab = "Probability", xlab = "Values", main = "Sampled Data vs Uniform Discrete Distribution")
hist(samples, breaks = seq(min_x - 0.5, max_x + 0.5, by = 1), col = "red", border = "white" , freq = FALSE ,add = TRUE)


#Exercise 2: Continuous Random variable

a = 5
b = 15 
c = 7
f = function(x,a,b,c){ifelse(x<a|x>b, 0, ifelse(x<c, 2*(x-a)/((b-a)*(c-a)) , 2 * (b-x)/((b-a)*(b-c))))}


curve(f(x,a,b,c) , from = 0 , to = 20 , n = 100 , ylab = 'f(X)' , main = 'a = 5 , c = 10 , b = 15' ,col = "blue" , lwd = 2 )

X = function(u, a , b , c){ifelse(u < (c - a)/(b - a) , sqrt(u * (b - a) * (c - a)) + a , -sqrt((b - a) * (b -c) * (1 - u)) + b)}

u = runif(10000)
Xs = X(u , a,b,c)
hist(Xs, breaks = seq(a - 0.5, b + 0.5, by = 0.2), col = "red", border = "white" , freq = FALSE ,add = TRUE)

# Exercise 3: 
