# Clearing the screen and work environment
rm(list = ls())
cat("\014")

# Set the working directory
setwd("/home/jacob/Documents")

library(ggplot2)

prob.of.success <- seq(0, 1, by = 0.01)
trials <- 10

df <- data.frame(prob.of.success,
                 zero.success = dbinom(0, trials, prob.of.success),
                 six.success = dbinom(6, trials, prob.of.success))


ggplot(data = df) + 
  geom_line(mapping = aes(x = prob.of.success, y = zero.success)) + 
  geom_line(mapping = aes(x = prob.of.success, y = six.success))

quadratic.formula <- function(a, b, c) {
  discriminant <- sqrt(b^2 - 4*a*c)
  c((-b - discriminant)/(2*a), (-b + discriminant)/(2*a))
}

find.null.hypothesis.parameter <- function(trials, proportion, 
                                           test.statisitc) {
  a <- (1 + test.statisitc^2/trials)
  b <- (-2*proportion - test.statisitc^2/trials)
  c <- (proportion^2)
  quadratic.formula(a,b,c)
}

trials <- 10
proportion <- 0.9 
test.statisitc <- 1.96

find.null.hypothesis.parameter(trials, proportion, test.statisitc)
