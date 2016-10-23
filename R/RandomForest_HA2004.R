library(data.table)
library(plyr)
library(dplyr)
library(ggplot2)
theme_set(theme_bw())
library(ggthemes)
library(scales)
library(lme4)
library(optimx)
#random intercept for family
library(randomForest)
library(smbinning)
library(stringr)

wls <- fread("C:/Users/Brad/Desktop/STAT 998/Project 2/WLS2.csv",
             data.table = FALSE)

#create variables

#convert ha2004, doc2011, hac2004, hac2011 to binary

wls$ha2004_bin <- ifelse(wls$HA2004 == 'Yes', 1, 0)
#wls$doc2011_bin <- ifelse(wls$doc2011 == 'Yes', 1, 0)

# wls$hac2004_bin <- ifelse(wls$HAC2004 == 'Yes', 1, 0)
# wls$hac2011_bin <- ifelse(wls$HAC2011 == 'Yes', 1, 0)

#look at age a bit more closely
#fix age
wls$age_2004 <- 2004 - ifelse(is.na(wls$birthyr) & wls$Rtype == 'g', 1938.5, 
                              wls$birthyr)

# wls$age_2011 <- 2011 - ifelse(is.na(wls$birthyr) & wls$Rtype == 'g', 1938.5, 
#                               wls$birthyr)

#create relative heart attack variable
wls$HArel2004 <- ifelse(wls$HArelless552004 == 'Yes' | wls$HArelmore552004 == 'Yes', 1, 0)
#create relative stroke variable
wls$strokefam2004 <- ifelse(wls$strokefamless652004 == 'Yes' |
                                wls$strokefammore652004 == 'Yes',
                            1, 0)
#select non-NA values
wls_ha2004 <- wls[!is.na(wls$ha2004_bin), ]

#select a subset of the variables, since we can't use any values
#collected in 2011

names_to_remove <- c(
    names(wls_ha2004)[str_detect(names(wls_ha2004), '2011')],
    'HA2004','HAC2004','ha2004','birthyr',
    'doc2004','doc2011',
    'hac2004_bin')

names_ha04 <- names(wls_ha2004)[! names(wls_ha2004) %in% names_to_remove]

wls_ha2004 <- wls_ha2004[names_ha04]

miss_values <- stack(apply(wls_ha2004, 2, function(x) mean(is.na(x))),
                     stringsAsFactors = FALSE) 

final_names <- as.character(miss_values[miss_values$values <= 0.175, 2])

wls_ha2004 <- wls_ha2004[final_names]

wls_ha2004_complete <- wls_ha2004[complete.cases(wls_ha2004), ]

#use randomForest

y = as.factor(wls_ha2004_complete$ha2004_bin)

X = select(wls_ha2004_complete, - ha2004_bin, -idpub)

X_modelmat <- model.matrix(~ -1 + ., data = X)

tune.rf = tuneRF(x = X_modelmat, y = y, ntree=1500, mtryStart = 8, 
                 stepFactor = 1, nodesize = 5)

## Fit the model
fit.rf  = randomForest(x = X_modelmat, y = y, 
                       ntree = 1500, 
                       mtry= 8, 
                       nodesize = 5, 
                       importance=T)

## Get the variable importance score
varimp = varImpPlot(fit.rf)


#build up a prospective logistic regression
fit1 <- glm(ha2004_bin ~ sex + age_2004 + Rtype + alcoholdays2004 + highchol2004 + 
            highbp2004 + stressmakepos2004, 
            data = wls_ha2004_complete, family = 'binomial')

gfit1 <- glmer(ha2004_bin ~ sex + age_2004 + Rtype + alcoholdays2004 + highchol2004 + 
                 highbp2004 + stressmakepos2004 + jailed2004 + (1|idpub), 
             data = wls_ha2004_complete, family = 'binomial',
             control = glmerControl(
                 optimizer = "optimx", calc.derivs = FALSE,
                 optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))


miss_class <- function(preds, actual){
    
    t_p_a <- table(preds, actual)
    
    list(
        'Total' = (t_p_a[1,2] + t_p_a[2,1]) / length(actual),
        'PredNoWasYes' = t_p_a[1,2] / (sum(t_p_a[1,])),
        'PredYesWasNo' = t_p_a[2,1] / (sum(t_p_a[2,]))
    )
    
}
