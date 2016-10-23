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

#wls$ha2011_bin <- ifelse(wls$HA2011 == 'Yes', 1, 0)
wls$doc2011_bin <- ifelse(wls$doc2011 == 'Yes', 1, 0)

# wls$hac2004_bin <- ifelse(wls$HAC2004 == 'Yes', 1, 0)
# wls$hac2011_bin <- ifelse(wls$HAC2011 == 'Yes', 1, 0)

#look at age a bit more closely
#fix age
wls$age_2004 <- 2004 - ifelse(is.na(wls$birthyr) & wls$Rtype == 'g', 1938.5, 
                              wls$birthyr)

wls$age_2011 <- 2011 - ifelse(is.na(wls$birthyr) & wls$Rtype == 'g', 1938.5, 
                              wls$birthyr)

#create relative heart attack variable
wls$HArel2004 <- ifelse(wls$HArelless552004 == 'Yes' | wls$HArelmore552004 == 'Yes', 1, 0)
#create relative stroke variable
wls$strokefam2004 <- ifelse(wls$strokefamless652004 == 'Yes' |
                                wls$strokefammore652004 == 'Yes',
                            1, 0)
#select non-NA values
wls_doc2011 <- wls[!is.na(wls$doc2011_bin), ]

#select a subset of the variables, since we can't use any values
#collected in 2011

names_to_remove <- c('HA2004','HAC2004','ha2004','birthyr',
                     'doc2004','doc2011', 'HAC2011','HA2011')

names_ha2011 <- names(wls_doc2011)[!names(wls_doc2011) %in% names_to_remove]

wls_doc2011 <- wls_doc2011[names_ha2011]

miss_values <- stack(apply(wls_doc2011, 2, function(x) mean(is.na(x))),
                     stringsAsFactors = FALSE) 

final_names <- as.character(miss_values[miss_values$values <= .175, 2])

wls_doc2011 <- wls_doc2011[final_names]

wls_doc2011_complete <- wls_doc2011[complete.cases(wls_doc2011), ]

#use randomForest

y = as.factor(wls_doc2011_complete$doc2011_bin)

X = select(wls_doc2011_complete, -doc2011_bin, -idpub)

X_modelmat <- model.matrix(~ -1 + ., data = X)

tune.rf = tuneRF(x = X_modelmat, y = y, ntree=600, mtryStart = 8, 
                 stepFactor = 1, nodesize = 5)

## Fit the model
fit.rf  = randomForest(x = X_modelmat, y = y, 
                       ntree = 1500, 
                       mtry= 8, 
                       nodesize = 5, 
                       importance = T)

## Get the variable importance score
varimp = varImpPlot(fit.rf)


#build up a prospective logistic regression

#create a sleeprestlessly categorical variable
wls_doc2011_complete$twodays_sleeprestlessly2004 <- ifelse(
    wls_doc2011_complete$sleeprestlessly2004 >= 2, 'Yes', 'No'
)

fit1 <- glm(doc2011_bin ~ sex + age_2011 + Rtype + THI2011 +
                highbp2011 + smokpkyrs2004 + highchol2004 + diabetes2011 +
                sumdepressionindex2004 + stroke2004 +
                alcoholdays2011,
            data = wls_doc2011_complete, family = 'binomial'
)

fit2 <- glm(doc2011_bin ~ sex + age_2011 + Rtype + THI2011 +
                highbp2011 + smokpkyrs2004 + highchol2004 + diabetes2011 +
                twodays_sleeprestlessly2004 + stroke2004 +
                alcoholdays2011,
            data = wls_doc2011_complete, family = 'binomial'
)

gfit1 <- glmer(doc2011_bin ~ sex + age_2011 + Rtype + THI2011 +
                   highbp2011 + smokpkyrs2004 + highchol2004 + diabetes2011 +
                   sumdepressionindex2004 + stroke2004 + alcoholdays2011 +
                   (1|idpub), 
               data = wls_doc2011_complete, 
               family = 'binomial',
               control = glmerControl(
                   optimizer = "optimx", calc.derivs = FALSE,
                   optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))

# summary(fit2 <- update(fit1.s, .~. + Rtype + smokever2004))

miss_class <- function(preds, actual){
    
    t_p_a <- table(preds, actual)
    
    list(
        'Total' = (t_p_a[1,2] + t_p_a[2,1]) / length(actual),
        'PredNoWasYes' = t_p_a[1,2] / (sum(t_p_a[1,])),
        'PredYesWasNo' = t_p_a[2,1] / (sum(t_p_a[2,]))
    )
    
}

#need to somehow adjust for weight too 
#either BMI or mosteverweigh
fit1.full <- glm(doc2011_bin ~ sex + age_2011 + Rtype + THI2011 + BMI2011 +
                     highbp2011 + smokpkyrs2004 + highchol2004 + 
                     diabetes2011 + sumdepressionindex2004 + stroke2004 + alcoholdays2011,
                 data = wls_doc2011, family = 'binomial')

fit2.full <- glm(formula = doc2011_bin ~ sex + age_2011 + Rtype + THI2011 + 
                     highbp2011 + smokpkyrs2004 + highchol2004 + diabetes2011 + 
                     sumdepressionindex2004 + stroke2004 + alcoholdays2011 + mosteverweigh2011, 
                 family = "binomial", data = wls_doc2011)

gfit1.full <- glmer(doc2011_bin ~ sex + age_2011 + Rtype + THI2011 + BMI2011 +
                    highbp2011 + smokpkyrs2004 + highchol2004 + 
                    diabetes2011 + sumdepressionindex2004 + stroke2004 + 
                    alcoholdays2011 + (1|idpub),
                  data = wls_doc2011, family = 'binomial',
                  control = glmerControl(
                      optimizer = "optimx", calc.derivs = FALSE,
                      optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)),
                  na.action = na.exclude)


#has a better misclassification rate than gfit1.full
gfit2.full <- glmer(formula = doc2011_bin ~ sex + age_2011 + Rtype + THI2011 + 
                    highbp2011 + smokpkyrs2004 + highchol2004 + diabetes2011 + 
                    sumdepressionindex2004 + stroke2004 + alcoholdays2011 + 
                    mosteverweigh2011 + (1|idpub), 
                  family = "binomial", data = wls_doc2011,
                  control = glmerControl(
                      optimizer = "optimx", calc.derivs = FALSE,
                      optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)),
                  na.action = na.exclude)