---
title: "PARCS Survey Weighting"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
Grab data from MCCSC, because we only need MCCSC for this study.
```{r}
setwd("~/Desktop/QualData")
mccsc = read.csv("MCCSCStaffSurvey.csv", header = TRUE)
```
Next we grab the variables of interest which are the SEL variables from each of them
Only have 1st through 12th for years worked, because we forgot to add K for MCCSC

Only grab MCCSC for this study, because we have knowledge of their programs.
```{r}
mccsc1 = mccsc[c("Q1_1", "Q1_2", "Q1_3", "Q1_4", "Q1_5", "Q1_6", "Q36", "Q38", "Q44","Q30",	"Q15_2_1",	"Q15_3_1",	"Q15_4_1",	"Q15_5_1",	"Q15_6_1",	"Q15_7_1",	"Q15_8_1",	"Q15_9_1",	"Q15_10_1",	"Q15_11_1",	"Q15_12_1",	"Q15_13_1")]
both = mccsc1[-c(1:2), ]
```
Need to get the blanks for the years variable changed from blanks to zero's.  And then we need to bring the variables both into the same dataset with both
```{r}
both1 = both[c("Q15_2_1",	"Q15_3_1",	"Q15_4_1",	"Q15_5_1",	"Q15_6_1",	"Q15_7_1",	"Q15_8_1",	"Q15_9_1",	"Q15_10_1",	"Q15_11_1",	"Q15_12_1",	"Q15_13_1")]
write.csv(both1, "both1.csv")
both1 = read.csv("both1.csv", header = TRUE, , na.strings = c(""))
both1[is.na(both1)] = 0; both1
# Create a seperate dataset with the other non years variables so we don't duplicate them when combining them
both2 = both[c("Q1_1", "Q1_2", "Q1_3", "Q1_4", "Q1_5", "Q1_6", "Q36", "Q38", "Q44", "Q30")]
# Want both two first to perserve the order from the original data
both = cbind(both2, both1)
```


Then we need to get rid of the missing values
```{r}
write.csv(both, "both.csv")
both1 = read.csv("both.csv", header = TRUE, , na.strings = c(""))
both2 = na.omit(both1)
```

Need to combine the ethnicity variables that are multiracial into the multiracial category
```{r}
Q30Factor = as.numeric(factor(Q30Factor$Q30))
Q30Factor = as.data.frame(Q30Factor)

Q30Factor = apply(Q30Factor, 2, function(x){ifelse(x == "7", 5, x)})
Q30Factor = as.data.frame(Q30Factor); head(Q30Factor)

Q30Factor = apply(Q30Factor, 2, function(x){ifelse(x == "8", 5, x)})
Q30Factor = as.data.frame(Q30Factor); head(Q30Factor)

Q30Factor = apply(Q30Factor, 2, function(x){ifelse(x == "9", 5, x)})
Q30Factor = as.data.frame(Q30Factor); head(Q30Factor)

Q30Factor = apply(Q30Factor, 2, function(x){ifelse(x == "10", 5, x)})
Q30Factor = as.data.frame(Q30Factor); head(Q30Factor)

```


Now we need to transform all of the categorical variables for the resposnes to SEL into numerical ones
```{r}
both2 = apply(both2, 2, function(x){ifelse(x == "Strongly agree", 7, x)})
both2 = as.data.frame(both2); head(both2)
both2 = apply(both2, 2, function(x){ifelse(x == "Agree", 6, x)})
both2 = as.data.frame(both2)
both2 = apply(both2, 2, function(x){ifelse(x == "Somewhat agree", 5, x)})
both2 = as.data.frame(both2)
both2 = apply(both2, 2, function(x){ifelse(x == "Neither agree nor disagree", 4, x)})
both2 = as.data.frame(both2)
both2 = apply(both2, 2, function(x){ifelse(x == "Somewhat disagree", 3, x)})
both2 = as.data.frame(both2)
both2 = apply(both2, 2, function(x){ifelse(x == "Disagree", 2, x)})
both2 = as.data.frame(both2)
both2 = apply(both2, 2, function(x){ifelse(x == "Strongly disagree", 1, x)})
both2 = as.data.frame(both2)
setwd("~/Desktop/QualData")
write.csv(both2, "both2.csv")
```


CFA: Need to get the factor scores for SEL for using in regression. 
```{r}
library(lavaan)
setwd("~/Desktop/QualData")
both2 = read.csv("both2.csv", header = TRUE)
cfaSEL = 'Satisfaction = ~ Q1_1 + Q1_2 + Q1_3 + Q1_4'
cfaSEL2 = cfa(cfaSEL, estimator = "MLR", data = both2)
SELSatis = lavPredict(cfaSEL2, type = "lv")
```
Now we need to combine the dataset with the altered variables into a new dataset ready for regression.  There is something werid with the names, but the data is the same.
```{r}
selSurveyWeight = cbind( Q30Factor, SELSatis)
names(selSurveyWeight) = c("Ethnicity", "SELSVScore")
head(selSurveyWeight)
setwd("~/Desktop/QualData")
write.csv(selSurveyWeight, "selSurveyWeight.csv")
```
Create a unweighted dataset within the survey survey package
```{r}
setwd("~/Desktop/QualData")
selSurveyWeight = read.csv("selSurveyWeight.csv", header = TRUE)
library(survey)
data.svy.unweighted <- svydesign(ids=~1, data=selSurveyWeight)
data.svy.unweighted$variables

```
Next we need to create a list of population values taken from the Indiana Department of Education webiste for ethnicity.  Here we are getting the expected frequencies.   
```{r}
ethnicity.dist <- data.frame( Ethnicity= c("1", "2","3", "4", "5","6"),
                       Freq = nrow(selSurveyWeight) * c(0.0091, 0.004, .043,.013,.005,.934))
```
Now are are weighted the data, by the expected frequencies (need to figure out what this is actually doing)
```{r}
data.svy.rake <- rake(design = data.svy.unweighted,
                   sample.margins = list(~Ethnicity),
                   population.margins = list(ethnicity.dist))
```
Now we trim the data (double check this as well)
```{r}
data.svy.rake.trim <- trimWeights(data.svy.rake, lower=0.3, upper=3,
                                  strict=TRUE)
round(svymean(selSurveyWeight, data.svy.rake.trim),2)

round(mean(selSurveyWeight$SELSVScore),2)
```

