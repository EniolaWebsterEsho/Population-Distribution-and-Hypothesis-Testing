############Chi-Squared Test#################
#Used on categorical variable
#H0 Variable A and B are independent
#H1 Variable A and B are not independent

#Test of independent, Homogeneity and Goodness of Fit Test
#Test of independent to determine whether there is a signifi
#cant association between two variables

#####Test of independent#######
#Is being a grad/ undergrad related to chocolate preference

halloween_df <- read.csv("halloween_22.csv")

setwd("C:/Users/admin/OneDrive - Tuskegee University/Documents/Fall 2022/Statistics with R/R")
getwd()

chocolate_tbl <- table(halloween_df$Classification,
                       halloween_df$Pref.Chocolate)
#we pass in the row first which is classification
#column is Pref.Chocolate

chocolate_tbl
chisq.test(chocolate_tbl)
#p-value = 1 > 0.05
#we fail to reject the null hypothesis
#there is no relationship between classification and choco-
#late preference

#is there a relationship between preferences of nuts and 
#preference of crisped rice/wafers
preference_tbl <- table(halloween_df$Pref.Nuts,
                        halloween_df$Pref.Crisped.rice.wafer)
preference_tbl
rownames(preference_tbl) <- c("Nuts No", "Nuts Yes")
colnames(preference_tbl) <- c("Wafer No", "Wafer Yes")
chisq.test(preference_tbl)
#p-value = 0.546 > 0.05
#we fail to reject the null hypothesis
#there is no relationship between preference of nuts and 
#crisped rice/wafers in candy

#In lungcap dataset, is there a relationship between gender
#and smoking

lungcap_df <- read.csv("LungCapData.csv")
affect_tbl <- table(lungcap_df$Gender,lungcap_df$Smoke)
affect_tbl
chisq.test(affect_tbl)
#p-value = 0.1866 > 0.05
#we fail to reject the null hypothesis
#there is no relationship


#########Test of Homogeneity###############
#Test of Homogeneity determine whether frequency counts are
#distributed identically across different populations.

#is there a difference in the admission to steroid yes across
#NCAA divisions 
div1 <- c(103,8440)
div2 <- c(52, 4289)
div3 <- c(65, 6428)

steriod_mtx <- rbind(div1,div2,div3)
steriod_mtx
colnames(steriod_mtx) <- c("Yes","No")
steriod_mtx
chisq.test(steriod_mtx)
#p-value = 0.456 > 0.05
#we fail to reject the null hypothesis
#there is no significant difference

div1 <- c(1,8440)
div2 <- c(52, 4289)
div3 <- c(65, 6428)

steriod_mtx <- rbind(div1,div2,div3)
steriod_mtx
colnames(steriod_mtx) <- c("Yes","No")
steriod_mtx
chisq.test(steriod_mtx)
#p-value = 2.2e-16 < 0.05
#we reject the null hypothesis
#there is significant difference

pizzaparties_df <- read.csv("pizzaparties.csv")
#is there a relationship between being an honor roll student
#and liking pizza parties?
pizzaparties_tbl <- table(pizzaparties_df$HonorRoll,
                          pizzaparties_df$PizzaParty)
pizzaparties_tbl
chisq.test(pizzaparties_tbl)
#p-value = 1 > 0.05
#we fail to reject the null hypothesis
#there is no significant relationship

####################Test of Good Fit################ 
#Five students working on a group project are expected to put
#in equal amounts of effort and contribute 20 hours a week
#to the project each

#null hypothesis: there is no difference between expected and
#observed distribution
#Alternative hypothesis: the distribution of the observed
#values does not match the expected distribution

observed <- c(20,18,13,30,19)
expected <- c(.20,.20,.20,.20,.20)
chisq.test(observed,p=expected)

#p-value = 0.1032 > 0.05
##we fail to reject the null hypothesis

#According to the manufacturer of M&M candy, the color 
#distribution for plain chocolate M&M is 13% Brown, 13%
#red,14% yellow, 24% blue, 20% orange and 16% green

observed <- c(150, 200, 350, 500, 410, 730)
expected <- c(.13,.13,.14,.24,.20,.16)
chisq.test(observed, p=expected)

#p-value = 2.2e-16 < 0.05
#we reject the null hypothesis

#According to a record producing company the distribution of
#sales based on record media is 10% CDs, 15% cassettes, 15%
#vinyl, 60% streaming services.

observed <- c(204, 330, 450, 625)
expected <- c(.10,.15,.15,.60)
chisq.test(observed, p=expected)
#p-value = 2.2e-16 < 0.05
#we reject the null hypothesis

#################ANOVA#################
fb_player_weights <- read.csv("FBPlayerWeights.csv")
fb_player_weights

teams <- c(rep("Dallas",17), rep("GreenBay",17),
           rep("Denver",17), rep("Miami",17), 
           rep("SanFrancisco", 17))
#rep = repeat
teams
player_weights <- c(fb_player_weights$Dallas,
                    fb_player_weights$GreenBay,
                    fb_player_weights$Denver,
                    fb_player_weights$Miami,
                    fb_player_weights$SanFrancisco)
fb_players_df <- data.frame(teams, player_weights)
View(fb_players_df)

fb_anova <- aov(fb_players_df$player_weights~fb_players_df$teams)
summary(fb_anova)
TukeyHSD(fb_anova)

#Anova is used to compare more than two tests/pairing/test
#while t-test is used for comparison of two teams
ads_df <- read.csv("ads.csv")
ads_twoway_anova <- aov(ads_df$NumOfAds ~ ads_df$Day + ads_df$Section)
#equivalent to:
ads_twoway_anova <- aov(NumOfAds ~ Day + Section, data=ads_df)

summary(ads_twoway_anova)
