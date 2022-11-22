#Binomial Distribution

#Probability Density Function

#Compute the probability of having exactly 23 successes when
#flipping a coin 100 times where probability of success for
#each flip is 0.5

result <- dbinom(x=23, size = 100, prob = 0.5)
result

#compute the probability of having exactly 15 tails when 
#flipping a coin 50 times where the probability of success
# for each flip is 0.5

#n (size) = 50, successes (x)=15, prob =0.5

result <- dbinom(x=15, size=50, prob=1/2)
result

#I wish to write an exam with 20 T/F questions,
#compute the probability of me writing 15 false questions
#given that the probability of me writing a true question
#is 0.6

#size = 20, successes (x) = 15, prob = 1-.6=.4

result <- dbinom(x=15,size=20,prob=.4)
result

#A fast food has 2 options on their menu pizza or burger
#Assume 30 customers make orders at the restaurant. What
#is the probability of 10 people ordering pizza, given that
#the probability of ordering a burger is .55?

#size = 30, x = 10 (pizza), prob = 1-.55 =.45

result <- dbinom(size = 30, x =10, prob = .45)
result

#if i have a class of 50  students, what is the probability of 
#25 student failing the class given that the failure rate in the
#is .3
#size = 50, x= (success, failing the class)=25, n-x=25, prob=.3)

result <- dbinom(size = 50, x= 25, prob = .3)
result

#compute the probability of having 46 to 54 (inclusive) successes
#when flipping a coin 100 times where the probability of success
#for each flip is 0.5

result <- dbinom(46:54, 100, 0.5)
dbinom()
sum(result)

result <- pbinom(46:54, 100, 0.5)
result

#The probabbility of having 3 or more heads when flipping a coin 10 times

result <- pbinom(q=3, 10, prob=.5, lower.tail = F)

#The probability of having 3 or fewer heads when flipping a coin 20 minutes

result <- pbinom(q=3, 10, prob = .5, lower.tail = T)
result

#The probability of me writing a T/F test with 20 questions with 10 or fewer
#true questions given that the probability of me writing a False question is .7?
result <- pbinom(q=10, size=20, prob = 1-.7, lower.tail = T)
result

#The probability of me writing a T/F test with 20 questions with 10 or fewer
#true questions given that the probability of me writing a True question is .7?
result <- pbinom(q=10, size=20, prob = .7, lower.tail = F)
result

#######################qbinom##########################
result <- qbinom(.5,100,.25)
result

######################rbinom########################
#generate 100 random numbers with 10 trials each with a
#probability of success of .5
#rbinom(n,size,prob)

result <- rbinom(100,10,.5)
result

#generate 50 random numbers that represent writing a T/F
#test with 20 questions, with the probability of 
#writing a true question being .4

result <- rbinom(50, 20, .6)
result

#generate 50 random numbers that represent writing a T/F
#test with 20 questions, with the probability of 
#writing a true question being .9

result <- rbinom(50, 20, .1)#prob of false
result
#results in fewer false questions

###################duniform###############
#probability of getting exactly 25 in the range 0 to 100
result <- dunif(25, min=0, max=100)
result

#probability of having 4 classes when the minimum is 1 and
#maximum is 6
result <- dunif(4, min=1, max=6)
result

#probability of having 2853 students at TU when the minimum 
#number is 2000 and maximum is 3500
result <- dunif(2853, min=2000,max=3500)
result

#####################punif()##############
result <- punif(25, min=0,max=100)
result

#equivalent
result <- punif(25, min=0,max=100, lower.tail = T)
result

#probability of 25 or higher
result <- punif(25, min=0,max=100, lower.tail = F)
result

#probability of having 4 or fewer classes when the minimum 
#is 1 and maximum is 6
result <- punif(4, min=1, max=6, lower.tail = T)
result

#probability of having 5 or more classes when the minimum 
#is 1 and maximum is 6
result <- punif(5, min=1, max=6, lower.tail = F)
result

#probabaility of having exactly 6 classes when the min is
#1 and max is 6?
result <- dunif(6, min=1, max=6)
result

#############qunif####################
#25 quantile 
result <- qunif(.25, min=0, max =100)
result
##############runif###################
#generate 100 random numbers between 0 and 100
result <- runif(100, min=0, max=100)
result

#generate 50 random between 1 and 6
result <-runif(50, min = 1, max = 6)
result
