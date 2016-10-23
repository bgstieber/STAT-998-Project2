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

wls$ha2011_bin <- ifelse(wls$HA2011 == 'Yes', 1, 0)
#wls$doc2011_bin <- ifelse(wls$doc2011 == 'Yes', 1, 0)

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
wls_ha2011 <- wls[!is.na(wls$ha2011_bin), ]

#select a subset of the variables, since we can't use any values
#collected in 2011

names_to_remove <- c('HA2004','HAC2004','ha2004','birthyr',
    'doc2004','doc2011', 'HAC2011','HA2011')

names_ha2011 <- names(wls_ha2011)[!names(wls_ha2011) %in% names_to_remove]

wls_ha2011 <- wls_ha2011[names_ha2011]

miss_values <- stack(apply(wls_ha2011, 2, function(x) mean(is.na(x))),
                     stringsAsFactors = FALSE) 

final_names <- as.character(miss_values[miss_values$values <= .175, 2])

wls_ha2011 <- wls_ha2011[final_names]

wls_ha2011_complete <- wls_ha2011[complete.cases(wls_ha2011), ]

#use randomForest

y = as.factor(wls_ha2011_complete$ha2011_bin)

X = select(wls_ha2011_complete, -ha2011_bin, -idpub)

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

fit1 <- glm(ha2011_bin ~ sex + age_2011 + Rtype + alcoholdays2004 + highchol2004 + 
                highbp2004 + diabetes2004 + stressgiveup2004 + smokpkdayX2004,
            data = wls_ha2011_complete, family = 'binomial'
)



gfit1 <- glmer(ha2011_bin ~ sex + age_2011 + Rtype + alcoholdays2004 + highchol2004 + 
                   highbp2004 + diabetes2004 + stressgiveup2004 + smokpkdayX2004 +
                   (1|idpub), 
               data = wls_ha2011_complete, 
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

miss_class(preds >= .5, y)

wls_ha2011.full <- wls[!is.na(wls$ha2011_bin), ]

#uses recursive partitioning to determine splits

#split at 27
smbinning(df = wls_ha2011.full, y = 'ha2011_bin', x = 'BMI2004')
#no split found
smbinning(df = wls_ha2011.full, y = 'ha2011_bin', x = 'BMI2011')
#split at 168
smbinning(df = wls_ha2011.full, y = 'ha2011_bin', x = 'mosteverweigh2004')
#split at 205
smbinning(df = wls_ha2011.full, y = 'ha2011_bin', x = 'mosteverweigh2011')

wls_ha2011.full$obese2004 <- ifelse(wls_ha2011.full$BMI2004 > 27, 'Obese','NotObese')

wls_ha2011.full$weigh2004_split <- ifelse(wls_ha2011.full$mosteverweigh2004 > 168,
                                          'Over','Under')

wls_ha2011.full$weigh2011_split <- ifelse(wls_ha2011.full$mosteverweigh2011 > 205,
                                          'Over','Under')

gfit1.full <- glmer(ha2011_bin ~ sex + age_2011 + Rtype + THI2011 + 
                    alcoholdays2004 + highchol2004 + mosteverweigh2011 +
                   highbp2004 + diabetes2004  + smokpkyrs2004 +
                   (1|idpub), 
               data = wls_ha2011.full, 
               family = 'binomial',
               control = glmerControl(
                   optimizer = "optimx", calc.derivs = FALSE,
                   optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)),
               na.action = na.exclude)


gfit2.full <- glmer(ha2011_bin ~ sex + age_2011 + Rtype + THI2011 + 
                        alcoholdays2004 + highchol2004 +
                        obese2004 + diabetes2004  + smokpkyrs2004 +
                        (1|idpub), 
                    data = wls_ha2011.full, 
                    family = 'binomial',
                    control = glmerControl(
                        optimizer = "optimx", calc.derivs = FALSE,
                        optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)),
                    na.action = na.exclude)


#final model?
gfit3.full <- glmer(ha2011_bin ~ sex + age_2011 + Rtype + THI2011 + 
                        alcoholdays2004 + highchol2004 +
                        weigh2004_split + diabetes2004  + smokpkyrs2004 +
                        (1|idpub), 
                    data = wls_ha2011.full, 
                    family = 'binomial',
                    control = glmerControl(
                        optimizer = "optimx", calc.derivs = FALSE,
                        optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)),
                    na.action = na.exclude)


gfit4.full <- glmer(ha2011_bin ~ sex + age_2011 + Rtype + THI2011 + 
                        alcoholdays2004 + highchol2004 +
                        weigh2011_split + diabetes2004  + smokpkyrs2004 +
                        (1|idpub), 
                    data = wls_ha2011.full, 
                    family = 'binomial',
                    control = glmerControl(
                        optimizer = "optimx", calc.derivs = FALSE,
                        optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)),
                    na.action = na.exclude)

#try a more complex weight variable
rpart(ha2011_bin ~ mosteverweigh2004, data = wls_ha2011.full,
      control = rpart.control(minsplit = 20, cp = .0025))

rpart(ha2011_bin ~ mosteverweigh2011, data = wls_ha2011.full,
      control = rpart.control(minsplit = 20, cp = .005))

wls_ha2011.full$mosteverweigh2004_category <- with(wls_ha2011.full,
    ifelse(mosteverweigh2004 < 168, 'low',
           ifelse(mosteverweigh2004 < 199, 'medlow',
                  ifelse(mosteverweigh2004 < 327, 'medhigh','high'))))

wls_ha2011.full$mosteverweigh2011_category <- with(wls_ha2011.full,
ifelse(mosteverweigh2011 < 205, 'low',
ifelse(mosteverweigh2011 < 248, 'med','high')))

#not any better gfit3.full is still good 
gfit6.full <- glmer(ha2011_bin ~ sex + age_2011 + Rtype + THI2011 + 
                        alcoholdays2004 + highchol2004 +
                        mosteverweigh2004_category + diabetes2004  + 
                        smokpkyrs2004 + (1|idpub), 
                    data = wls_ha2011.full, 
                    family = 'binomial',
                    control = glmerControl(
                        optimizer = "optimx", calc.derivs = FALSE,
                        optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)),
                    na.action = na.exclude)

#better than gfit3.full?
gfit7.full <- glmer(ha2011_bin ~ sex + age_2011 + Rtype + THI2011 + 
                        alcoholdays2004 + highchol2004 +
                        mosteverweigh2011_category + diabetes2004  + 
                        smokpkyrs2004 + (1|idpub), 
                    data = wls_ha2011.full, 
                    family = 'binomial',
                    control = glmerControl(
                        optimizer = "optimx", calc.derivs = FALSE,
                        optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)),
                    na.action = na.exclude)