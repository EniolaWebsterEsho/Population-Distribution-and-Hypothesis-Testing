
getwd()

lungcap_df <- read.csv("LungCapData.csv")

?t.test

#1

#we want to test whether or not the average lung capacity 
#is less than 8 with 95% confidence level?

#null hypothesis: mu=8
#alternative hypothesis: mu < 8

t.test(lungcap_df$LungCap, alternative = "less", mu=8,
       conf.level = 0.95)
#p-value = 0.08336
#level of significance = 1 -.95 =0.05
#if p-value is less than level of significance I reject 
#the null hypothesis otherwise I fail to reject the null
#hypothesis
#0.08336 > 0.05 so I fail to reject the null hypothesis

#2

#we want to test whether or not the average lung capacity
#is greater than 8 with a 98% confidence level

#null hypothesis: mu=8
#alternative hypothesis: mu > 8

t.test(lungcap_df$LungCap, alternative = "greater", mu=8,
       conf.level = 0.98)
#p-value = 0.9166
#level of significance = 1 -.98 =0.02
#0.9166 > 0.02 
#fail to reject the null hypothesis

#3

#we want to test whether or not the average lung capacity
#is different from 8 with a 98% confidence level

#null hypothesis: mu=8
#alternative hypothesis: mu != 8

t.test(lungcap_df$LungCap, alternative = "two.sided", mu=8,
       conf.level = 0.90)

#p-value = 0.1667
#level of significance = 1 -.90 =0.1
#0.1667 > 0.1
#fail to reject the null hypothesis

#calculate the average age in the lungcap_df dataset:
mean(lungcap_df$Age)

#4

#we want to test whether the average age in the lungcap_df
#dataset is less than 13 with a 95% confidence level?

#Default confidence level is 95%

#null hypothesis: mu=13
#alternative hypothesis: mu < 13

t.test(lungcap_df$Age, alt="less", mu=13, conf.level = .95)
#p-value = 3.519e-06
#level of significance = 1 -.95 =0.05
#3.519e-06 =.000003519 < 0.5
#we reject the null hypothesis

#5
#we want to test whether the mean of students grades before
#attending a review session is less than mean of students 
#after attending a review session

grades_df <- read.csv("beforeafter.csv")

#null hypothesis: there is no difference in averages (mu=0)
#alternative hypothesis: mu(before) < mu(after)

t.test(grades_df$before, grades_df$after, alt="less", mu=0,
       paired=T)
#p-value = 0.5454
#level of significance = 1 -.95 =0.05
#0.5454 > .05
#fail to reject null hypothesis

t.test(grades_df$after, grades_df$before,alt="greater",mu=0,
       paired=T)
#This is the same as the previous t-test

####################11_3_Inclass#####################
#we want to test whether the average alertness of a group
#of participants increases after drinking coffee

#null hypothesis: there is no difference in average level
#of alertness before and after (mu=0)
#alternative hypothesis: mu(before) < mu(after)
#one tailed(less)
#paired (testing the same variable under two different
#circumstances for the same group of participants)

halloween_df <- read.csv("halloween_22.csv")

#we want to test if the average rating of almond joy is
#different from 5 with a 98% confidence level

#if less or greater = one tailed
#if we dont care = two tailed

t.test(halloween_df$Almond.Joy,alternative = "two.sided",
       mu=5, conf.level = .98)
#p-value = 0.468
#level of significance = 1 -.98 =0.02
#0.468 > .02
#fail to reject null hypothesis

mean(halloween_df$Almond.Joy)

#we want to test if the average rating of Hawaiian host is
#less than 5 with a 95% conf level
#one tailed test(less)

#null hypothesis: mu=5
#alternative hypothesis: mu < 5

t.test(halloween_df$Hawaiian.Host, alternative = "less",
       mu=5, conf = .95)
#also
t.test(halloween_df$Hawaiian.Host, alternative = "less",
       mu=5)
#p-value = 6.38e-06 = 0.00000638
#level of significance = 1 -.95 =0.05
#0.00000638 < .05
#we reject null hypothesis

#we wish to test whether the average rating of KIT KAT is
#greater than 5 
#null hypothesis: mu=5
#alternative hypothesis: mu > 5
t.test(halloween_df$Kit.Kat, alternative = "greater",
       mu=5, conf = .95)


#we wish to test whether the average rating of Twix is 
#different from 6
t.test(halloween_df$Twix ,alternative = "two.sided",
       mu=6)

#we want to test whether average the rating of skittkles is 
#less than 7
t.test(halloween_df$Skittles, alternative = "less", mu=7)

#we want to test whether the average rating of jolly 
#ranchers is greater than 5
t.test(halloween_df$Jolly.Ranchers.Candy,
       alternative = "greater",
       mu=5)
#p-value = 0.5728
#level of significance = 1 -.95 =0.05
#0.5728 > .05
#fail to reject null hypothesis

#we wish to test whether the average rating of m&m is not
#4

t.test(halloween_df$M.M.s, mu=4, alternative = "two.sided")
#p-value = 0.08631
#level of significance = 1 -.95 =0.05
#0.08631 > .05
#fail to reject null hypothesis


#####################two sample t-test###################
review_df <-read.csv("review.csv")

#is there a significant difference in average test scores
#between students that attended a review and those that did
#not attend the review session before the exam

t.test(review_df$grade~review_df$review, alt = "two.sided", 
       mu=0)
#mu=0 because no difference in averages(null hypothesis)
#p-value = 0.4663
#level of significance = 1 -.95 =0.05
#0.4663 > .05
#fail to reject null hypothesis

#is there a significant difference in the average rating of
#starburst between grad and undergrad student

t.test(halloween_df$Starburst~halloween_df$Classification,
       alt="two.sided", mu=0)
#p-value = 0.9529
#level of significance = 1 -.95 =0.05
#0.9529 > .05
#fail to reject null hypothesis

###NB Numeric first then non numeric/categorical second

#we want to test if the average Ugrad rating of salt water 
#Taffy is less than the average Grad rating

###NB
#Based on the alphabetical order of values in 
#classification column Grad will be listed before Ugrad 
#so our value of the alternative argument will be 
#worded based on:G____U
#since we are interested in testing if Ugrad < grad 
#average,then we will write this as Grad > Ugrad 
#(G __>___U)

t.test(halloween_df$Salt.Water.Taffy~
         halloween_df$Classification,
       alt="greater",mu=0)
#p-value = 0.3965
#level of significance = 1 -.95 =0.05
#0.3965 > 0.05
#fail to reject null hypothesis

#we want to test whether students that do not prefer 
#chocolate candy have a lower average rating for Hershey's 
#chocolate bars than those that do prefer chocolate
#(NO < YES)
t.test(halloween_df$Hershey.s.Mini.Chocolate.Candy.Bars
       ~halloween_df$Pref.Chocolate,alt="less",mu=0)
#p-value = 0.1922
#level of significance = 1 -.95 =0.05
#0.1922 > 0.05
#fail to reject null hypothesis

#we want to test whether students that prefer coconut
#have a higher average rating for Almond Joy
#than those that do not prefer chocolate(YES > NO i.e 
#NO < YES)

t.test(halloween_df$Almond.Joy~halloween_df$Pref.Coconut,
       alt="less",mu=0)
#p-value = 0.05469
#level of significance = 1 -.95 =0.05
#0.05469 > 0.05
#fail to reject null hypothesis

#we want to test whether students that prefer fruit 
#flavored candy have a different average rating for 
#skittles than those that do not with 90% confidence level

t.test(halloween_df$Skittles~
         halloween_df$Pref.Fruit.Flavored,
       alt="two.sided", mu=0, conf=0.9)
#p-value = 0.3371

#we want to test graduate students have higher
#average rating for snickers than Ugrad with a 97% 
#confidence level(G > U)

t.test(halloween_df$Snickers~halloween_df$Classification,
       alt="greater", mu=0, conf=0.97)
#p-value = 0.4594
#level of significance = 1 -.97 =0.03
#0.4594 > 0.03
#fail to reject null hypothesis

#we want to test whether students that prefer nuts in 
#their candy have higher average(YES > NO i.e NO < YES)

t.test(halloween_df$Reese.s.Peanut.Butter.Cups~
         halloween_df$Pref.Nuts, alt="less", mu=0, 
       conf=0.9)
#p-value = 0.1956
#level of significance = 1 -.9 =0.1
#0.1956 > 0.1
#fail to reject the null hypothesis

#we want to test whether students that prefer crisped 
#rice/wafers in their candy have a lower average rating for 
#kit kat than those that do not prefer crisped rice/wafer

t.test(halloween_df$Kit.Kat~
         halloween_df$Pref.Crisped.rice.wafer,
       alt="greater", mu=0)
#p-value = 0.8344
#level of significance = 1 -.95 =0.05
#0.8344 > 0.05
#fail to reject null hypothesis

#we want to test if those whose mother smoked during 
#pregnancy have a lower average lung capacity than those 
#whose mother did not smoke(YES > NO i.e NO > YES)

t.test(lungcap_df$LungCap~lungcap_df$Smoke, alt="greater", 
       mu=0)
#p-value = 0.9998 > 0.5
#fail to reject null hypothesis

#we want to test if there is a significant difference in 
#average lung capacity between male and female participants

t.test(lungcap_df$LungCap~lungcap_df$Gender, alt="two.sided",
       mu=0)
#p-value = 4.21e-06 < 0.05
#we reject the null hypothesis

#we want to test if the average height of male participants
#is less than the average height of female participants with
#a 93% confidence levels

t.test(lungcap_df$Height~lungcap_df$Gender, alt="less",mu=0,
       conf=.93)
#p-value = 6.608e-05 < 0.07
#we reject the null hypothesis
