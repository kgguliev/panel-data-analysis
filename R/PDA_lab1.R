### Panel data: Analysis and Applications for the Social Sciences, 2022 ###
### Lab 1 ###

## Confidence interval for EX ## 
a <- c(1,3,10,8,3)
t.test(a)

## The coin problem ##

# define the distribution of the number of tails under the null hypothesis

# dbinom syntax
# dbinom(x,           # X-axis values (x = 0, 1, 2, ..., n)
#        size,        # Number of trials (n > = 0)
#        prob,        # The probability of success on each trial
#        log = FALSE) # If TRUE, probabilities are given as log

dbinom(4, 10, 0.5) # the probability of getting 4 successes out of 10 is 0.20

data <- data.frame(tails = 0:10, prob = dbinom(0:10, 10, 0.5))
data

# let us plot the distribution of the number of tails under the null hypothesis 

library(ggplot2)

ggplot(data, aes(tails, prob)) + 
  geom_bar(stat = "identity", colour = "red", fill = "yellow") +
  geom_text(aes(label=round(prob, 2))) +
  scale_x_continuous(breaks=seq(0,10,1)) +
  ggtitle("Binomial Distribution") + 
  ggthemes::theme_fivethirtyeight()

# let us highlight a p-value part of the plot 
data$region = ifelse(data$tails > 7, "pvalue", "confidence")
data

ggplot(data, aes(tails, prob, fill = region)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(label=round(prob, 2))) +
  scale_x_continuous(breaks=seq(0,10,1)) + 
  scale_fill_manual(values = c("pvalue" = "red", "confidence" = "green")) +
  scale_x_continuous(breaks=seq(0,10,1)) +
  ggtitle("Binomial Distribution", subtitle = "With rejection region") + 
  ggthemes::theme_fivethirtyeight()

# calculate p-value

# Let us toss a coin 10 times. As a result, you get 8 tails and 2
# heads. Test the null hypothesis that our coin is fair against the
# alternative P(tails) > P(heads) by using p-value.

pvalue <- dbinom(8, 10, 0.5) + dbinom(9, 10, 0.5) + dbinom(10, 10, 0.5)
pvalue

# We cannot reject our null hypothesis => our coin is fair.