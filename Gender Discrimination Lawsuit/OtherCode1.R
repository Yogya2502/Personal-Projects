# set a working directory
setwd("C:/Users/Yogya Hridey Sareen/Downloads/BC2406 Analytics 1 Course Materials for Students/BC2406 Analytics 1 Course Materials for Students/Special Session 1 Gender Discrimination Lawsuit")

# import csv, packages: data.table

library(data.table)
lawsuit.dt <- fread("Lawsuit.csv", stringsAsFactors = TRUE)

# change from integer to factor
lawsuit.dt$Dept <- factor(lawsuit.dt$Dept)
lawsuit.dt$Gender <- factor(lawsuit.dt$Gender)
lawsuit.dt$Clin <- factor(lawsuit.dt$Clin)
lawsuit.dt$Cert <- factor(lawsuit.dt$Cert)
lawsuit.dt$Rank <- factor(lawsuit.dt$Rank, ordered = T, levels = c("1","2","3"))


# Renaming and reformatting
# New facet label names for variable

levels(lawsuit.dt$Gender)<-c("F","M")
levels(lawsuit.dt$Cert) <-c("NC","BC")
levels(lawsuit.dt$Clin) <-c("R","C")
levels(lawsuit.dt$Rank) <-c("Ast","Assoc","Prof")
# packages: ggplot2,scales
# create a stacked bar graph faceted by Cert,Clin, and Gender
library(ggplot2)
library(scales)

ggplot(data=lawsuit.dt)+ geom_bar(aes(x=Dept,y=after_stat(count/sum(count)), fill=Rank))+ scale_fill_manual("Rank", values = c("Ast"= "tan2", "Assoc"="lightpink4", "Prof"="salmon"))+ facet_grid(.~Cert+Clin+Gender)+ labs(y="Proportion")