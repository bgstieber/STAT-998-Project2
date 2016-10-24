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

run.rf <- FALSE

wls <- fread("C:/Users/Brad/Desktop/STAT 998/Project 2/WLS2.csv",
             data.table = FALSE)

#create variables

#convert doc2004, doc2011, hac2004, hac2011 to binary

wls$doc2004_bin <- ifelse(wls$doc2004 == 'Yes', 1, 0)
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
wls_doc04 <- wls[!is.na(wls$doc2004_bin), ]

#select a subset of the variables, since we can't use any values
#collected in 2011

names_to_remove <- c(
    names(wls_doc04)[str_detect(names(wls_doc04), '2011')],
    'idpub','HA2004','HAC2004','doc2004','birthyr',
    'hac2004_bin', 'ha2004_bin')

names_doc04 <- names(wls_doc04)[! names(wls_doc04) %in% names_to_remove]

wls_doc04 <- wls_doc04[names_doc04]

miss_values <- stack(apply(wls_doc04, 2, function(x) mean(is.na(x))),
                     stringsAsFactors = FALSE) 

final_names <- as.character(miss_values[miss_values$values <= 0.175, 2])

wls_doc04 <- wls_doc04[final_names]

wls_doc04_complete <- wls_doc04[complete.cases(wls_doc04), ]

#use randomForest

y = as.factor(wls_doc04_complete$doc2004_bin)

X = select(wls_doc04_complete, - doc2004_bin)

X_modelmat <- model.matrix(~ -1 + ., data = X)

if(run.rf){
tune.rf = tuneRF(x = X_modelmat, y = y, ntree = 600, mtryStart=10, stepFactor=1, 
                 nodesize = 10)

## Fit the model

fit.rf  = randomForest(x = X_modelmat, y = y, 
                       ntree = 1500, mtry = 10, nodesize=10, importance=T)

## Get the variable importance score
varimp = varImpPlot(fit.rf)
}

fit1 <- glm(doc2004_bin ~ sex +age_2004 + Rtype +THI2004 +
                highbp2004 + diabetes2004 + 
                smokpkdayX2004 + highchol2004 +
                sumdepressionindex2004 + stroke2004 + alcoholdays2004 +
                troublesleepamt2004, data = wls_doc04_complete, family = 'binomial')

#create new trouble sleep variable

wls_doc04_complete$troublesleepamt2004_binned <- 
    ifelse(wls_doc04_complete$troublesleepamt2004 %in% c('No','Monthlyorless'),
           'MonthlyOrLess', wls_doc04_complete$troublesleepamt2004)

fit2 <- glm(doc2004_bin ~ sex +age_2004 + Rtype +THI2004 +
                highbp2004 + diabetes2004 + 
                smokpkdayX2004 + highchol2004 +
                sumdepressionindex2004 + stroke2004 + alcoholdays2004 +
                troublesleepamt2004_binned, data = wls_doc04_complete, family = 'binomial')

fit3 <- glm(doc2004_bin ~ sex +age_2004 + Rtype +THI2004 +
                highbp2004 + diabetes2004 + 
                smokpkdayX2004 + highchol2004 +
                sumdepressionindex2004 + stroke2004 + alcoholdays2004 +
                sleeprestlessly2004, data = wls_doc04_complete, family = 'binomial')

wls_doc2004.full <- wls[!is.na(wls$doc2004),]

wls_doc2004.full$troublesleepamt2004_binned <- ifelse(
    wls_doc2004.full$troublesleepamt2004 == 'No',
    'Monthlyorless', wls_doc2004.full$troublesleepamt2004
)

miss_class <- function(preds, actual){
    
    t_p_a <- table(preds, actual)
    
    list(
        'Total' = (t_p_a[1,2] + t_p_a[2,1]) / length(actual),
        'PredNoWasYes' = t_p_a[1,2] / (sum(t_p_a[1,])),
        'PredYesWasNo' = t_p_a[2,1] / (sum(t_p_a[2,]))
    )
    
}

#has a better misclassification rate than gfit3

gfit2 <- glmer(
    doc2004_bin ~ sex +age_2004 + Rtype +THI2004 + BMI2004 +
        highbp2004 + diabetes2004 + 
        smokpkdayX2004 + highchol2004 +
        sumdepressionindex2004 + stroke2004 + alcoholdays2004 +
        troublesleepamt2004_binned + (1|idpub),
    data = wls_doc2004.full, family = 'binomial',
    control = glmerControl(
        optimizer = "optimx", calc.derivs = FALSE,
        optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)),
    na.action = na.exclude
)

gfit3 <- glmer(
    doc2004_bin ~ sex +age_2004 + Rtype +THI2004 + BMI2004 +
        highbp2004 + diabetes2004 + 
        smokpkdayX2004 + highchol2004 +
        sumdepressionindex2004 + stroke2004 + alcoholdays2004 +
        sleeprestlessly2004 + (1|idpub),
    data = wls_doc2004.full, family = 'binomial',
    control = glmerControl(
        optimizer = "optimx", calc.derivs = FALSE,
        optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)),
    na.action = na.exclude
)

#add some stressful life events
#none are worth keeping
gfit2.update <- update(gfit2, .~. + jailed2004 + deepdebt2004 + servedwar2004 + 
                           parentsdrugs2004 + stressconcefforts2004)

#I think gfit2 is final model for doc2004
