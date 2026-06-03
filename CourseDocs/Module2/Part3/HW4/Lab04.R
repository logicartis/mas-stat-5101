## Module 2, Parts 2 and 3
## ---------------------------------------------------------------------------------

#Segment 7
# read in the data
library(readxl) # make sure to install if it isn't already installed
hb <- read_excel('hamburger.xlsx')
head(hb)



## ---------------------------------------------------------------------------------
wilcox.test(cfu~treatment, data=hb, correct=F)
wilcox.test(cfu~treatment, data=hb, exact=T)


## ---------------------------------------------------------------------------------
hb$Ranks<- rank(hb$cfu, ties.method = "average")

sum(hb$Ranks[hb$treatment=="control"])
sum(hb$Ranks[hb$treatment=="active"])


## ---------------------------------------------------------------------------------

##Segment 8
schiz <- read.table('case0202.txt', header=T, as.is=T)

wilcox.test(schiz$unaff, schiz$aff, paired = T)


## ---------------------------------------------------------------------------------
library(DescTools)

SignTest(schiz$unaff, schiz$aff)


## ---------------------------------------------------------------------------------

##Segment 9

eh.colors<- read.csv("m2_s9.hairandeyecolor.csv", header = T)
eh.colors$EyeColor<- factor(eh.colors$EyeColor, 
                      levels = c("Blue",  "Hazel/Green", "Brown", "Other"))

eh.colors$HairColor<- factor(eh.colors$HairColor,
                      levels<- c("Blonde", "Red", "Brown", "Black", "Other"))


## ---------------------------------------------------------------------------------
table(eh.colors$EyeColor, eh.colors$HairColor)


## ---------------------------------------------------------------------------------
library(ggplot2)
ggplot(eh.colors, aes(x=`HairColor`))+
 geom_bar(fill = "steelblue") +
  labs(
    title = "Bar Chart of Hair color",
    x = "Hair Color",
    y = "Count"
  ) +
  theme_minimal()


## ---------------------------------------------------------------------------------

library(ggmosaic)

ggplot(data = eh.colors)+
  geom_mosaic(aes(x = product(EyeColor),
                  fill = HairColor), na.rm = TRUE)+
  theme_bw()+
  theme(plot.title = element_text(hjust=0.5,
                                  size = rel(1.2)),
        axis.title.y = element_text(size = rel(1.1)),
        axis.title.x = element_text(size = rel(1.1)),
        strip.text.y = element_text(size = rel(1.1)))+
  labs(x = "Eye Color",
       y = "Hair Color",
       fill = "Hair Color",
       title = "Mosaic Plot of Hair Color Given Eye Color")



## ---------------------------------------------------------------------------------
ggplot(data = eh.colors)+
  geom_mosaic(aes(x = product(HairColor),
                  fill = EyeColor), na.rm = TRUE)+
  theme_bw()+
  theme(plot.title = element_text(hjust=0.5,
                                  size = rel(1.2)),
        axis.title.y = element_text(size = rel(1.1)),
        axis.title.x = element_text(size = rel(1.1)),
        strip.text.y = element_text(size = rel(1.1)))+
  labs(x = "Hair Color",
       y = "Eye Color",
       fill = "Eye Color",
       title = "Mosaic Plot of Eye Color Given Hair Color")


## ---------------------------------------------------------------------------------
#Segment 10

survey.data<- read.csv("m2_s10.doctorsurvey.csv", header = T)
survey.data$Receive.Letter<- factor(survey.data$Receive.Letter, 
                                    levels = c("Yes", "No"))
survey.data$Return.Survey<- factor(survey.data$Return.Survey, 
                                    levels = c("Yes", "No"))


## ---------------------------------------------------------------------------------
survey.table<- table(survey.data$Receive.Letter, survey.data$Return.Survey)
survey.table

ggplot(data = survey.data)+
  geom_mosaic(aes(x = product(Receive.Letter),
                  fill = Return.Survey), na.rm = TRUE)+
  theme_bw()+
  theme(plot.title = element_text(hjust=0.5,
                                  size = rel(1.2)),
        axis.title.y = element_text(size = rel(1.1)),
        axis.title.x = element_text(size = rel(1.1)),
        strip.text.y = element_text(size = rel(1.1)))+
  labs(x = "Receive Letter?",
       y = "Return Survey?",
       fill = "Return Survey?",
       title = "Mosaic Plot of Doctor Survey Data")


## ---------------------------------------------------------------------------------
groupsuccess<- survey.table[,1]
groupsize<- margin.table(survey.table, 1)


## ---------------------------------------------------------------------------------

prop.test(groupsuccess, groupsize, alternative = "two.sided", correct = F)


## ---------------------------------------------------------------------------------
#Segment 12

surgery.data<- read.csv("m2_s11.cancersurgery.csv", header = T)
surgery.data$Surgery<- factor(surgery.data$Surgery, levels = c("Yes", "No"))
surgery.data$Died<- factor(surgery.data$Died, levels = c("Yes", "No"))


## ---------------------------------------------------------------------------------
surgery.table<- table(surgery.data$Surgery, surgery.data$Died)
surgery.table

ggplot(data = surgery.data)+
  geom_mosaic(aes(x = product(Surgery),
                  fill = Died), na.rm = TRUE)+
  theme_bw()+
  theme(plot.title = element_text(hjust=0.5,
                                  size = rel(1.2)),
        axis.title.y = element_text(size = rel(1.1)),
        axis.title.x = element_text(size = rel(1.1)),
        strip.text.y = element_text(size = rel(1.1)))+
  labs(x = "Surgery?",
       y = "Died from Prostate Cancer?",
       fill = "Died from Prostate Cancer?",
       title = "Mosaic Plot of Prostate Cancer Surgery Data")

OddsRatio(surgery.table, conf.level = 0.95)


## ---------------------------------------------------------------------------------
#Segment 13

smoking.data<- read.csv("m2_s13.smoking.csv", header = T)


## ---------------------------------------------------------------------------------
smoking.data$Parent<- factor(smoking.data$Parent, 
                             levels = c("Neither", "One", "Both"))
smoking.data$Student<- factor(smoking.data$Student,
                              levels = c("Non-Smoker", "Smoker"))


## ---------------------------------------------------------------------------------
smoking.table<- table(smoking.data$Parent, smoking.data$Student)
smoking.table


## ---------------------------------------------------------------------------------
ggplot(data = smoking.data)+
  geom_mosaic(aes(x = product(Parent),
                  fill = Student), na.rm = TRUE)+
  theme_bw()+
  theme(plot.title = element_text(hjust=0.5,
                                  size = rel(1.2)),
        axis.title.y = element_text(size = rel(1.1)),
        axis.title.x = element_text(size = rel(1.1)),
        strip.text.y = element_text(size = rel(1.1)))+
  labs(x = "Parent Smoking Status",
       y = "Student Smoking Status",
       fill = "Student Smoking Status",
       title = "Mosaic Plot of Smoking Data")


## ---------------------------------------------------------------------------------
smoking.test<- chisq.test(smoking.table, correct = F)
smoking.test


## ---------------------------------------------------------------------------------
smoking.test$expected
(smoking.test$residuals)^2


## ---------------------------------------------------------------------------------
#Segment 14

homesdata<- read.csv("m2_s14.homesdata.csv", header = T)
homesdata$Home.Style<- factor(homesdata$Home.Style, 
                              levels = c("Colonial", "Cabin", "A-frame"))
homesdata$Repurchase<- factor(homesdata$Repurchase,
                              levels = c("Yes", "No", "Maybe"))

homes<- table(homesdata)
homes

ggplot(data = homesdata)+
  geom_mosaic(aes(x = product(Home.Style),
                  fill = Repurchase), na.rm = TRUE)+
  theme_bw()+
  theme(plot.title = element_text(hjust=0.5,
                                  size = rel(1.2)),
        axis.title.y = element_text(size = rel(1.1)),
        axis.title.x = element_text(size = rel(1.1)),
        strip.text.y = element_text(size = rel(1.1)))+
  labs(x = "Home Style",
       y = "Would Repurchase Same Style",
       fill = "Would Repurchase Same Style",
       title = "Mosaic Plot of Home Satisfaction Data")

homes.test<- chisq.test(homes)
homes.test
homes.test$expected


## ---------------------------------------------------------------------------------

#Segment 15

hist.data<- read.csv("m2_s15.histogramq.csv", header = T)
hist.data$Question1<- factor(hist.data$Question1, 
                      levels = c("Correct", "Incorrect"))
hist.data$Question2<- factor(hist.data$Question2, 
                      levels = c("Correct", "Incorrect"))


## ---------------------------------------------------------------------------------
hist.table<- table(hist.data$Question1, hist.data$Question2)


## ---------------------------------------------------------------------------------
mcnemar.test(hist.table, correct = F)


## ---------------------------------------------------------------------------------
mcnemar.ci<- function(table, conf.level = 0.95){
  alpha<- 1 - conf.level
  z<- qnorm(1 - alpha/2)
  
  n<- sum(table)
  y1.<- margin.table(table, 1)[1]
  y.1<- margin.table(table, 2)[1]
  diff12<- table[1,2]-table[2,1]
  add12<- table[1,2]+table[2,1]
  se<- sqrt(add12 - diff12^2/n)/n
  lowerci<- y1./n - y.1/n - z*se
  upperci<- y1./n - y.1/n + z*se
  cat("Confidence Interval = ", lowerci, upperci, "\n")
}

mcnemar.ci(hist.table, conf.level = 0.95)



