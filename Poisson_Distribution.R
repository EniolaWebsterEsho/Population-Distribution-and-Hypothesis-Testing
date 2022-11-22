###############Poisson Distribution###########
###########Density Function############

#what is the probability of making 2 sales in a week if the
#average sales is 3 per week?

result <- dpois(x=2, lambda =3)
result

#what is the probability of making 3 sales in 2 days if the
#average sales is 7 per week?

#71 week = 7 days, 7/week = 7/7days
#7/7 days = ?/1 day = 1 sale/day
result <- dpois(x=3, lambda = 1)
result

###########central density function##################
#what is the probability of making 2 to 4 sales in a week 
#if the average is 3 per week?

result <- (ppois(q=4, lambda = 3, lower.tail = T)-ppois(
  q=1, lambda = 3, lower.tail = T
))
result

#what is the probability of making less than 5 sales in a 
#week. If the average sales rate is 3 per week?
result <- ppois(q=4,lambda = 3, lower.tail = T)
result

#what is the probability of making more than 5 sales in a 
#week if the average sales rate is 3 per week?
result <- ppois(q=6, lambda = 3, lower.tail = F)
result

result <- 1- ppois(q=6, lambda = 3, lower.tail = T)
result

#what is the probability of making 5 or more sales in a
#week if the average sales rate is 3 per week?
result <- ppois(q=5, lambda = 3, lower.tail = F)
result

#what is the probability of making 2 or fewer sales in
#a week if the average sales rate is 3 per week?
result <- ppois(q=2, lambda = 3, lower.tail = T)
result

#if cars are entering the gate at an average of two cars
#per minute. what is the probability of 4 cars entering 
#the gate in one minute.
result <- dpois(x=4, lambda = 2)
result
?dpois()

#if cars are entering the gate at a rate of two cars
#per minute if a car just entered the gate, what is the 
#probability of the next car entering in 3 minutes?
#2 cars per 1 minute, 
result <- dexp(x=3, rate = 2)
result

#if cars are entering the gate at rate of two cars
#per minute if a car just entered the gate, what is the 
#probability of the next car entering within in 3 minutes?
result <- pexp(q=3, rate =2, lower.tail = T)
result


