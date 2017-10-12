# Logistic function defining the probability decay for general prediction
logistic.prob <- function(a,b,c,t) {
  c / (1 + a*exp(-b*t))
}

# Set 5% rate of probability decay (must be examined)
b <- -.05 

# Set a carrying rate of 1 (the highest possible probability)
c <- 1

# Set the initial predition value
init.pred <- .10

# Find the a such that P(0) = the initial prediction value
a <- c/init.pred - 1 

# Set the base prediction time frame
start.date <- '2017/9/12'

end.date <- '2017/10/25'

base.t <- 0:(as.numeric(as.Date(end.date) - as.Date(start.date)))


# Plot the decay over the base time frame
plot(logistic.prob(a,b,c,base.t) ~ base.t)

# Calculate current probability
t <- as.numeric(Sys.Date()) - as.numeric(as.Date(start.date)) + 1
logistic.prob(a,b,c,t)

