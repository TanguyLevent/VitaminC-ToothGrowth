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

# Analysis on Orange Jus
Global_OJ <- splitbySupp %>% filter(supp=="OJ")
Global_OJ
#Analysis on Absorbic Acid
Global_VC <- splitbySupp %>% filter(supp=="VC")
Global_VC
# Analyse Global delivery Method

Global_Supp_Mean <- splitbySupp %>% group_by(supp) %>% summarise(len = mean(len))
Global_Supp_Mean

d0.5<- mdata %>% filter(dose==.5)
hist(d0.5$len, breaks = 20)

OJ <- mdata %>% filter(supp=="OJ")
VC <- mdata %>% filter(supp=="VC")


t.test(OJ$len,VC$len,alternative = "greater",paired = FALSE, var.equal = FALSE, conf.level = 0.95)
fit <- cor (VC$len ,OJ$len)
fit
