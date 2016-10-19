library(data.table)
library(plyr)
library(dplyr)
library(ggplot2)
theme_set(theme_bw())
library(ggthemes)
library(scales)
#random intercept for family
library(randomForest)
library(smbinning)
library(stringr)

wls <- fread("C:/Users/Brad/Desktop/STAT 998/Project 2/WLS2.csv",
             data.table = FALSE)

#create variables

#convert doc2004, doc2011, hac2004, hac2011 to binary

wls$doc2004_bin <- ifelse(wls$doc2004 == 'Yes', 1, 0)
wls$doc2011_bin <- ifelse(wls$doc2011 == 'Yes', 1, 0)

wls$hac2004_bin <- ifelse(wls$HAC2004 == 'Yes', 1, 0)
wls$hac2011_bin <- ifelse(wls$HAC2011 == 'Yes', 1, 0)

#look at age a bit more closely

wls$age_2004 <- 2004 - wls$birthyr
wls$age_2011 <- 2011 - wls$birthyr

#create relative heart attack variable
wls$HArel2004 <- ifelse(wls$HArelless552004 == 'Yes' | wls$HArelmore552004 == 'Yes', 1, 0)
#create relative stroke variable
wls$strokefam2004 <- ifelse(wls$strokefamless652004 == 'Yes' |
                                wls$strokefammore652004 == 'Yes',
                            1, 0)
#select non-NA values
wls_hac04 <- wls[!is.na(wls$hac2004_bin), ]

#select a subset of the variables, since we can't use any values
#collected in 2011

names_to_remove <- c(
    names(wls_hac04)[str_detect(names(wls_hac04), '2011')],
    'idpub','Rtype','HA2004','HAC2004','doc2004','birthyr',
    'doc2004_bin')

names_hac04 <- names(wls_hac04)[! names(wls_hac04) %in% names_to_remove]

wls_hac04 <- wls_hac04[names_hac04]

miss_values <- stack(apply(wls_hac04, 2, function(x) mean(is.na(x))),
                     stringsAsFactors = FALSE) 

final_names <- as.character(miss_values[miss_values$values <= 0.20,2])

wls_hac04 <- wls_hac04[final_names]

wls_hac04_complete <- wls_hac04[complete.cases(wls_hac04), ]

#use randomForest

y = as.factor(wls_hac04_complete$hac2004_bin)

X = select(wls_hac04_complete, - hac2004_bin)

X_modelmat <- model.matrix(~ -1 + ., data = X)

tune.rf = tuneRF(x = X_modelmat, y = y, ntree=1000, mtryStart=10, stepFactor=1, nodesize=10)

## Fit the model
fit.rf  = randomForest(x = X_modelmat, y = y, 
                       ntree=1000, mtry=10, nodesize=10, importance=T)


## Get the variable importance score
varimp = varImpPlot(fit.rf)
