# creativityV2.r: numerical and graphical summaries of data

# version 2 using tidyverse and ggplot2 commands

# install.packages('dplyr')    # only needs to be done once
# install.packages('ggplot2')  # only needs to be done once.

library(dplyr)      # data manipulation library
library(ggplot2)    # GGplot graphics library

getwd() #get a reminder of the path to the folder you are using to run this code
setwd('XXXX') 
# NOTE: you will change XXX the folder name according to your path

creativity <- read.csv('creativity.csv')
names(creativity)

head(creativity)
View(creativity)

summary(creativity$score)


creativity %>%
  head()

creativity %>%
  select(score)  %>%
  head()

creativity %>%
  select(score)  %>%
  summary()
  
creativity %>%
  select(score) %>%
  mean()

creativity %>%
  summarize(
    meanScore = mean(score) 
    )

creativity %>%
  summarize(
    meanScore = mean(score),
    medianScore = median(score) 
    )
  
creativity %>%
  group_by(treatment) %>%
   summarize(
    meanScore = mean(score),
    medianScore = median(score) 
    )

# dotplot of one group of data
ggplot(
    creativity,
    aes(y = score, x=1) ) +
  geom_dotplot( binaxis='y', stackdir='center') 

# side-by-side dotplots of two (or more) groups
ggplot(
    creativity,
    aes(y = score, x = treatment) ) +
  geom_dotplot( binaxis='y', stackdir='center') 

# separating the setup from the plot
creativity.setup1 <- ggplot(
  creativity,
  aes(x = score) )

creativity.setup1 + geom_histogram()
creativity.setup1 + geom_boxplot()
creativity.setup1 + coord_flip() + geom_boxplot()

# plots for multiple groups
creativity.setup2 <- ggplot(
  creativity,
  aes(x = treatment, y = score) )

creativity.setup2 + geom_boxplot()
creativity.setup2 + theme_classic() + geom_boxplot()


# adding color to separate groups
creativity.setup3 <- ggplot(
  creativity,
  aes(x = score, fill=treatment, color=treatment) )

creativity.setup3 + geom_histogram(position='dodge')

creativity.setup3 + scale_color_grey() + scale_fill_grey() + 
  geom_histogram(position='dodge')


# saving a plot to import to word

ggsave('histogram.emf')
ggsave('histogram.emf', height=5, width=6)

getwd()#another reminder where you will find the saved images


