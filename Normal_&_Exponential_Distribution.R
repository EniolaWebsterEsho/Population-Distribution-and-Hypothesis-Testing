##############Normal Distribution###########
###############Probability Density############

result <- dnorm(100,mean=0,sd = 1)
result

#what is the probability of someone having an IQ of exactly 140
#given that the average is 90 and the standard deviation is 15?
result <- dnorm(140, mean=90, sd=15)
result

#what is the probability of someone having an IQ of exactly 100
#given that the average is 90 and the standard deviation is 15?
result <- dnorm(100, mean=90, sd=15)
result

#what is the probability of someone having an IQ of exactly 80
#given that the average is 90 and the standard deviation is 15?
result <- dnorm(80, mean=90, sd=15)
result

#the same value because they fall in the same standard deviation

#what is the probability of a student receiving a 77 on a test
#given that the average is 75 and the standard deviation is 5.
result <- dnorm(77, mean=75, sd=5)
result

#################central density##########
#what is the probability of someone having an IQ of 80 or
#higher given that the average IQ is 90 and the standard
#deviation deviation is 5
result <- pnorm(80, mean=90, sd=15, lower.tail=F)
result

#what is the probability of a student receiving a 77 or
#lower on a test given that the average is 75 and the standard
#deviation is 5
result <- pnorm(80, mean=75, sd=15, lower.tail=T)
result

#what is the probability of a student receiving a grade between
#50 and 65 on a test given that the average is 75 and the standard
#deviation is 5
prob1 <- pnorm(65, mean=75, sd=5, lower.tail=T)
prob2 <- pnorm(50, mean=75, sd=5, lower.tail=T)
result <- prob1 - prob2
result

#another solution
prob1 <- pnorm(50, mean=75, sd=5, lower.tail = F)
prob2 <- pnorm(65, mean=75, sd=5, lower.tail = F)
result <- prob1 - prob2
result

#what is the probability of a student receiving a grade less than
#60 or greater than 90 on a test given that the average is 75 
#and the standard deviation is 5
prob1 <- pnorm(60, mean=75, sd=5, lower.tail = T)
prob2 <- pnorm(90, mean=75, sd=5, lower.tail = F)
result <- prob1 + prob2
result 

#Another solution
prob1 <- pnorm(90, mean=75, sd=5, lower.tail = T)
prob2 <- pnorm(60, mean=75, sd=5, lower.tail = T)
result <- 1-(prob1 - prob2)
result

#Another solution
prob1 <- pnorm(60, mean=75, sd=5, lower.tail = F)
prob2 <- pnorm(90, mean=75, sd=5, lower.tail = F)
result <- 1-(prob1 - prob2)
result

#what is the probability of having 17 or more students in
#a class of 20 given that the average attendance is 15 and 
#the standard deviation is 3

result <- pnorm(17, mean=15, sd=3, lower.tail = F)
result

#####################qnorm#################
#suppose IQs are normally distributed with a mean of 100
#and a standard deviation of 15. what is the IQ that separates
#the lower 33 percent from the others?

qnorm(.33, mean = 100, sd= 15, lower.tail = T)

#suppose IQs are normally distributed with a mean of 100
#and a standard deviation of 15. what is the IQ that separates
#the lower 80 percent from the others?

qnorm(.8, mean = 100, sd= 15, lower.tail = T)

#suppose IQs are normally distributed with a mean of 100
#and a standard deviation of 15. what is the IQ that separates
#the upper 10 percent from the others?

qnorm(.1, mean = 100, sd= 15, lower.tail = F)

####################rnorm#######################
result <- rnorm(1000, mean=0, sd=1)
summary(result)

#generate 100  random numbers with a mean of 65 and a
#standard  deviation of 5
rnorm(100,mean=65,sd=5)

#####################Exponential Distribution###########

##################dexp###########
?dexp()
#the average time between customer arrivals at a coffeshop
#is 5 minutes(m=5). If a customer has just walked into the
#coffeeshop, what is the probability of the next customer 
#arriving in 10 minutes?

result <- dexp(10, 1/5)
result 

#the average time it takes a fisherman to catch a fish is
#20 minutes. if the fisherman just caught a fish, what is the 
#probability of him catching the next fish in 12 minutes?

result <- dexp(12, rate=1/20)
result

#the average number of years it takes for the stock market
#to crash is 10. if the stock market just crashed , what is
#the probability of it crashing in 9 years?

result <- dexp(9, rate=1/10)
result

#the average number of years it takes for the stock market
#to crash is 10. if the stock market just crashed , what is
#the probability of it crashing in 15 years?

result <- dexp(15, rate=1/10)
result

#############central density function##########

#the average time between customer arrivals at a coffeshop
#is 5 minutes(m=5). If a customer has just walked into the
#coffeeshop, what is the probability of the next customer 
#arriving in less than 10 minutes?

result <- pexp(10, 1/5,lower.tail = T)
result 

#the average time between customer arrivals at a coffeshop
#is 5 minutes(m=5). If a customer has just walked into the
#coffeeshop, what is the probability of the next customer 
#arriving in more than 6 minutes?

result <- pexp(6, 1/5,lower.tail = F)
result 

#the average distance a biker travels before encountering
#a hill is 1 mile. if the biker just encountered a hill,
#what is the probability of the biker encountering the next
#hill in 1/4 of a mile or more

result <-pexp(1/4, rate = 1, lower.tail=F)
result

#the average time between customer arrivals at a coffeshop
#is 5 minutes(m=5). If a customer has just walked into the
#coffeeshop, what is the probability of the next customer 
#arriving between 3 and 6 minutes?

result <- pexp(6, 1/5,lower.tail = T) - 
  pexp(3, 1/5,lower.tail = T)

result 

result <- pexp(3, 1/5,lower.tail = F) - 
  pexp(6, 1/5,lower.tail = F)

result 

result <- 1- (pexp(3, 1/5,lower.tail = T) +
                pexp(6, 1/5,lower.tail = F))
result

#the average time between customer arrivals at a coffeshop
#is 5 minutes(m=5). If a customer has just walked into the
#coffeeshop, what is the probability of the next customer 
#arriving less than 3 minutes or more than 6 minutes?

result <- pexp(3, 1/5,lower.tail = T) + 
  pexp(6, 1/5,lower.tail = F)

result

#############Quantile Function############

#the average time between customer arrivals at a coffeshop
#is 5 minutes(m=5). What is the number of minutes at which
#85% or less of the customers arrive at?

result <- qexp(.85, rate =1/5, lower.tail = T)
result

#############Random Function##########
#random samples from an exponential distribution
#generate 100 random numbers that represent times that elapse
#before a next customer arrives at a coffeeshop, given that
#the average amount of time between arrivals is 4 minutes
result <- rexp(100, 1/4)
result
