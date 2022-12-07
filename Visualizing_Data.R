###############Visualizing Data###########
##################Pie Chart###############

getwd()
setwd("C:/Users/admin/OneDrive - Tuskegee University/Documents/Fall 2022/Statistics with R/R"
)
halloween_df <- read.csv("halloween_22.csv")
classification_tbl <- table(halloween_df$Classification)
classification_tbl

pie(classification_tbl)
?pie
pie(classification_tbl,
    main = "Classification Distribution",
    init.angle = 90,
    col = c("orchid4","deepskyblue2","yellow1"))

caramel_tbl <-table(halloween_df$Pref.Caramel)
caramel_tbl

pie(caramel_tbl,
    main = "Caramel Preference\nCSCI360/ISCS510",
    col = c("orchid1","magenta4"),
    labels = c("Do not prefer","Prefer")
    )
install.packages("vcd")
library(vcd)
Arthritis

improved <-table(Arthritis$Improved)
improved

pie(improved[order(improved, decreasing = TRUE)],
    init.angle = 90, #will start at 12 o clock instead of 3 o clock
    clockwise = TRUE, #Default is FALSE
    col = c("plum1","cadetblue2","lightpink"),
    main = "Pie Chart of Improved from Arthritis")
str(Arthritis)

################Bar Chart###########

nuts_tbl <-table(halloween_df$Pref.Nuts)
nuts_tbl

barplot(nuts_tbl)
?barplot

barplot(nuts_tbl,
        main = "Nut Preferences in Candy\nFor CSCI360/CSCI510",
        xlab = "Preferences",
        ylab = "Number of Students",
        col = c("chocolate1"),
        border = 1,
        names.arg = c("Do not Prefer","Prefer"))

barplot(improved[order(improved)], #create chart
        horiz = TRUE,
        las = 1, #orientation of axis labels
        col = c("beige","blanchedalmond","bisque1"),
        border = NA, # No borders on bars
        main  = "Frequencies of Treatment Levels\nin Arthritis Dataset",
        xlab  = "Number of patients")

###################Histogram#################
lynx
hist(lynx)

hist(lynx,
     breaks = 11,
     freq = FALSE,
     col = "thistle1", # Or use: col = colors() [626]
     main = "Histogram of Annual Canadian Lynx Trappings\n1821-1934",
     xlab = "Number of Lynx Trapped")

hist(lynx,
     breaks = seq(0, 7000, by = 200),
     freq = FALSE,
     col = "thistle1", # Or use: col = colors() [626]
     main = "Histogram of Annual Canadian Lynx Trappings\n1821-1934",
     xlab = "Number of Lynx Trapped")

hist(lynx,
     breaks = c(0, 100, 300, 500, 3000, 3500, 7000),
     freq = FALSE,
     col = "thistle1", # Or use: col = colors() [626]
     main = "Histogram of Annual Canadian Lynx Trappings\n1821-1934",
     xlab = "Number of Lynx Trapped")

faithful

duration<-faithful$eruptions
faithful_breaks<-seq(1.5,5.5,by=0.5)
faithful_breaks
duration_cut<- cut(duration, faithful_breaks, right = FALSE)
duration_cut
duration_freq_tbl<-table(duration_cut)
duration_freq_tbl

cumulative_freq<-c(0,cumsum(duration_freq_tbl))
plot(faithful_breaks, cumulative_freq,
     main = "Old Faithful Eruption",
     xlab = "Duration minutes",
     ylab = "Cumulative eruptions")
lines(faithful_breaks,cumulative_freq) #joins the points

#############Dotcharts#########
mtcars
dotchart(mtcars$mpg, labels = row.names(mtcars),
         cex = 0.6, xlab = "mpg")

x <- seq(-pi,pi,0.1)
plot(x, sin(x))

plot(x, sin(x),
     main="The Sine Function",
     ylab="sin(x)",
     type="l",
     col="blue")

plot

############Box plot###########
boxplot(USJudgeRatings$RTEN)

boxplot(USJudgeRatings$RTEN,horizontal = T)

boxplot(USJudgeRatings$RTEN,horizontal = T,
        main = "Lawyers' Ratings of State Judges in the\nUS Superior Court (c. 1977)",
        xlab = "Lawyers' Ratings")

boxplot(USJudgeRatings$RTEN,
        horizontal = TRUE,
        las = 1, # Make all labels horizontal
        notch = TRUE, # a notch is the thick black line in the middle of the box
        ylim = c(0, 10), # Specify range on Y axis, will view the entire range from 1 to 10
        col = "slategray3",  # R's named colors (n = 657)
        boxwex = 0.5, # Width of box as proportion of original
        whisklty = 1, # Whisker line type; 1 = solid line, range of the non outlying variables staplelty = 
        0, # Staple (line at end) type; 0 = none
        outpch = 16, # Symbols for outliers; 16 = filled circle
        outcol = "slategray3", # Color for outliers
        main = "Lawyers' Ratings of State Judges in the\nUS Superior Court (c. 1977)",
        xlab = "Lawyers' Ratings")

?boxplot
########ggplot####

install.packages("ggplot2")
library(ggplot2)
iris

iris_plot <- ggplot(iris,aes(Sepal.Length,Petal.Length,
                             colour=Species)) + geom_point()
print(iris_plot)

print(iris_plot + labs(y="Petal Length(cm)", x= "Sepal Length(cm)"
                       + ggtitle("Petal and Sepal length of Iris"))
      )
iris_plot <- ggplot(iris,aes(Sepal.Length,Petal.Length,colour=Species))+
  geom_point(shape=1) + 
  labs(y="Petal Length(cm)", x= "Sepal Length(cm)")+ 
  ggtitle("Petal and Sepal length of Iris")+
  #annotate("text", x=4:6,y=5:7, label="text")
  #annotate("rect", xmin = 5,xmax = 7,ymin = 4,ymax = 6,alpha=.4)
  #annotate("segment",x=5,xend=7,y=4,yend=6,colour="black)
  theme(legend.position = "bottom")+
  geom_smooth(method = lm)
iris_plot
