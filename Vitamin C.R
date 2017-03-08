library(dplyr)
library(ggplot2)
library(RColorBrewer)
data("ToothGrowth")
mdata <- ToothGrowth
sum(is.na(mdata))

head(mdata)
str(mdata)
summary(mdata)
table(mdata$dose, mdata$supp)

# Global
splitbySupp <- mdata %>% group_by(supp,dose) %>% summarise(len = mean(len))
splitbySupp

#Analyse on the dose levels of Vitamin C
Global_Dose <- mdata %>% group_by(dose) %>% summarise(len = mean(len))

ggplot(mdata, aes(factor(dose), len)) + 
        geom_boxplot(aes(fill = factor(dose))) +
        scale_fill_brewer(palette="Pastel1") + 
        xlab("Dose levels of Vitamin (mg/day)") + 
        ylab("Length of Tooth") + 
        ggtitle("Effect of dose levels of Vitamic C on the Tooth Growth") + 
        guides(fill=guide_legend(title="Dose Levels"))+
        theme(plot.title = element_text(hjust = 0.5))

Global_Dose

ggplot(mdata, aes(factor(dose), len)) + 
        geom_boxplot(aes(fill = factor(supp))) + 
        facet_grid( ~ supp) +  
        scale_fill_brewer(palette="Pastel1") + 
        xlab("Dose levels of Vitamin C (mg/day)") + 
        ylab("Length of Tooth")  + 
        ggtitle("Comparaison effect of dose levels of Vitamin C on the Tooth Growth by Delivery Methods") +
        guides(fill=guide_legend(title="Delivery Methods")) + 
        theme(plot.title = element_text(size = 11, face = "bold",hjust = 0.5))

# Analysis on delivery methods
splitbySupp <- mdata %>% group_by(supp,dose) %>% summarise(len = mean(len))
Global_Supp_Mean <- splitbySupp %>% group_by(supp) %>% summarise(len = mean(len))
Global_OJ <- splitbySupp %>% filter(supp=="OJ")
Global_VC <- splitbySupp %>% filter(supp=="VC")

Global_Supp_Mean
Global_OJ
Global_VC

# Hypothesis tests :

t.test(len~supp, paired = FALSE, var.equal = FALSE, mdata)
t.test(len~supp, paired = FALSE, var.equal = FALSE, subset(mdata, dose == 0.5))
t.test(len~supp, paired = FALSE, var.equal = FALSE, subset(mdata, dose == 1))
t.test(len~supp, paired = FALSE, var.equal = FALSE, subset(mdata, dose == 2))
t.test(len~supp, paired = FALSE, var.equal = FALSE, subset(mdata, dose != 2))

cor(mdata$dose,mdata$len)

dose1 <- subset(mdata, dose %in% c(0.5,1))
t.test(len~dose, paired = FALSE, var.equal = FALSE, dose1)

dose2 <- subset(mdata, dose %in% c(1,2))
t.test(len~dose, paired = FALSE, var.equal = FALSE, dose2)