#set the working directory
setwd("C:/Users/Yogya Hridey Sareen/Downloads/BC2406 Analytics 1 Course Materials for Students/BC2406 Analytics 1 Course Materials for Students/Special Session 1 Gender Discrimination Lawsuit")

#start using data table library
library(data.table)

#import data from csv file
lawsuit.dt <- fread("Lawsuit.csv")
lawsuit.dt$Gender <- factor(lawsuit.dt$Gender)

library(ggplot2)
library(scales)

levels(lawsuit.dt$Gender) <- c("Female","Male")

mtry <- lm(Rank ~ Gender + Cert + Exper, data = lawsuit.dt)
summary(mtry)

ggplot(data = lawsuit.dt, aes(Gender, Sal95, color = Gender)) +geom_boxplot() + labs(title = "Salary in 1994", x = "Gender", y = "Salary") + facet_grid(.~Dept) 
options(scipen = 10)

ggplot(data = lawsuit.dt, aes(Gender, Sal94, color = Gender)) +geom_boxplot() + labs(title = "Salary in 1995", x = "Gender", y = "Salary") + facet_grid(.~Dept) 

