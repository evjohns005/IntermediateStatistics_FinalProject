# Lesson 10 

## Scenario III: What is the average savings practices of different demographic groups?

library(dplyr)
library(rcompanion)
library(car)
library(tidyr)

savings <- read.csv("C:/Users/evjoh/Downloads/savings.csv")

savings.reformat <- gather(savings, key="Group", value="Price")

head(savings.reformat)

savings1 <- na.omit(savings.reformat)
head(savings1)

savings1$Price <- as.numeric(savings1$Price)

# Testing Assumptions:
## Normality already tested in Python; assumption met. For practice purposes:
plotNormalHistogram(savings1$Price)

## Using Bartlett Test since data is normally distributed:
bartlett.test(Price ~ Group, data=savings1)

## p-value is 0.04867 which is less than 0.05 so the assumption has been met.  
## R gives a better view of the p-value, since in Python it showed p-value of 0.0

## Sample Size assumption has been met: minimum of 80 (total size= 211 cases)

savingsANOVA <- aov(savings1$Price ~ savings1$Group)
summary(savingsANOVA)

## Shows that there is a significant difference (p-value= <2e-16) between the savings account balance and demographics

pairwise.t.test(savings1$Price, savings1$Group, p.adjust="bonferroni")

savingsMeans <- savings1 %>% group_by(Group) %>% summarize(Mean = mean(Price))
savingsMeans

## Conclusion: The difference can be seen looking at the mean of each group.  
## Group A shows the highest average savings account balance.
## Group C shows the lowest average savings account balance.
## Without more information on the different groups, that is the only information we can derive from this dataset.