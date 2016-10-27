
## control = glmerControl(
##         optimizer = "optimx", calc.derivs = FALSE,
##         optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE))
## 
library(data.table)
library(lme4)
library(optimx)


wls <- fread("C:/Users/Brad/Desktop/STAT 998/Project 2/WLS2.csv",
             data.table = FALSE)

#create variables

#convert doc2004, doc2011, ha2004, ha2011 to binary

wls$doc2004_bin <- ifelse(wls$doc2004 == 'Yes', 1, 0)
wls$doc2011_bin <- ifelse(wls$doc2011 == 'Yes', 1, 0)

wls$ha2004_bin <- ifelse(wls$HA2004 == 'Yes', 1, 0)
wls$ha2011_bin <- ifelse(wls$HA2011 == 'Yes', 1, 0)

#look at age a bit more closely
#fix age
wls$age_2004 <- 2004 - ifelse(is.na(wls$birthyr) & wls$Rtype == 'g', 1938.5, 
                              wls$birthyr)

wls$age_2011 <- 2011 - ifelse(is.na(wls$birthyr) & wls$Rtype == 'g', 1938.5, 
                              wls$birthyr)

#create relative heart attack variable
wls$HArel2004 <- ifelse(
    wls$HArelless552004 == 'Yes' | wls$HArelmore552004 == 'Yes', 1, 0
    )
#create relative stroke variable
wls$strokefam2004 <- ifelse(
    wls$strokefamless652004 == 'Yes' | wls$strokefammore652004 == 'Yes', 1, 0
    )

#bin troublesleepamt2004
wls$troublesleep04_bin <- 
    ifelse(wls$troublesleepamt2004 == 'No',
           'Monthlyorless', wls$troublesleepamt2004)

table(wls$troublesleepamt2004, wls$troublesleep04_bin)

#create overweight variable
wls$overweight2004 <- ifelse(wls$rightweight2004 == 'Overweight','Yes','No')
table(wls$overweight2004, wls$rightweight2004)

#create a weight split variable
rpart::rpart(ha2011_bin ~ mosteverweigh2004, data = wls)
wls$weigh2004_split <- ifelse(wls$mosteverweigh2004 < 168.5, 'Under168','Over168')

#create subsets for each response
wls_doc2004.full <- wls[!is.na(wls$doc2004_bin), ]
wls_doc2011.full <- wls[!is.na(wls$doc2011_bin), ]
wls_ha2004.full <- wls[!is.na(wls$ha2004_bin), ]
wls_ha2011.full <- wls[!is.na(wls$ha2011_bin), ]


miss_class <- function(preds, actual){
    
    t_p_a <- table(preds, actual)
    
    list(
        'Total' = (t_p_a[1,2] + t_p_a[2,1]) / length(actual),
        'PredNoWasYes' = t_p_a[1,2] / (sum(t_p_a[1,])),
        'PredYesWasNo' = t_p_a[2,1] / (sum(t_p_a[2,]))
    )
    
}

doc2004_glm <- glmer(
    doc2004_bin ~ sex  + age_2004 + Rtype + THI2004 + 
        BMI2004 + highbp2004 + diabetes2004 + stroke2004 +
        smokpkyrs2004 + highchol2004 + highcholfam2004 +
        HArelless552004 + 
        sumdepressionindex2004 + alcoholdays2004 +
        troublesleep04_bin + (1|idpub),
    data = wls_doc2004.full, family = 'binomial',
    control = glmerControl(
        optimizer = "optimx", calc.derivs = FALSE,
        optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)),
    na.action = na.exclude
)

summary(doc2004_glm)
miss_class(p <- predict(doc2004_glm, type = 'response') >= .5, 
           wls_doc2004.full$doc2004_bin)
doc2011_glm <- glmer(formula = doc2011_bin ~ sex + age_2011 + Rtype + THI2011 +
                               BMI2011 + 
                               highbp2011 + smokpkyrs2004 + diabetes2011 + 
                               alcoholdays2011 + mosteverweigh2011 + HArelless552004 +                                           highcholfam2004 + highchol2011 + stroke2011 +                                                     sumdepressionindex2011 + (1 | idpub), 
                  family = "binomial", data = wls_doc2011.full,
                  control = glmerControl(
                      optimizer = "optimx", calc.derivs = FALSE,
                      optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)),
                  na.action = na.exclude)

summary(doc2011_glm)
miss_class(p <- predict(doc2011_glm, type = 'response') >= .5, 
           wls_doc2011.full$doc2011_bin)
ha2004_glm <- 
    glmer(ha2004_bin ~ sex + age_2004 + Rtype + THI2004  + 
              BMI2004 + 
              alcoholdays2004 + 
              highchol2004 + stressmakepos2004 + 
              diabetes2004 + overweight2004 + smokpkyrs2004 + (1|idpub), 
          data = wls_ha2004.full, family = 'binomial',
          control = glmerControl(
              optimizer = "optimx", calc.derivs = FALSE,
              optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)),
          na.action = na.exclude)

summary(ha2004_glm)
miss_class(p <- predict(ha2004_glm, type = 'response') >= .5, 
           wls_ha2004.full$ha2004_bin)


ha2011_glm <- 
    glmer(ha2011_bin ~ sex + age_2011 + Rtype + THI2011 + BMI2011 + 
            diabetes2004 + compareAmerica2004 + smokpkyrs2011 + 
             alcoholdays2011 + highchol2011 + smokpkyrs2011 +
              HArelless552004 + (1 | idpub), 
                    data = wls_ha2011.full, 
                    family = 'binomial',
                    control = glmerControl(
                        optimizer = "optimx", calc.derivs = FALSE,
                        optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)),
                    na.action = na.exclude)

summary(ha2011_glm)
miss_class(p <- predict(ha2011_glm, type = 'response') >= .5, 
           wls_ha2011.full$ha2011_bin)
library(memisc)

signifs <- noquote("Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1")

foo <- mtable('DOC 2004' = doc2004_glm, 'HA 2004' = ha2004_glm,
              'DOC 2011' = doc2011_glm, 'HA 2011' = ha2011_glm,
              sdigits = 2, digits = 3)
foo$calls <- signifs

foo

library(ROCR)
doc04_preds <- predict(doc2004_glm, type = 'response')
doc04_actuals <- wls_doc2004.full$doc2004_bin
doc11_preds <- predict(doc2011_glm, type = 'response')
doc11_actuals <- wls_doc2011.full$doc2011_bin
ha04_preds <- predict(ha2004_glm, type = 'response')
ha04_actuals <- wls_ha2004.full$ha2004_bin
ha11_preds <- predict(ha2011_glm, type = 'response')
ha11_actuals <- wls_ha2011.full$ha2011_bin

doc04_roc <- performance(prediction(doc04_preds,
                                 doc04_actuals),
                         'tpr','fpr')

doc04_auc <- performance(prediction(doc04_preds,
                                 doc04_actuals),
                         'auc')
doc04_auc <- doc04_auc@y.values[[1]]

doc11_roc <- performance(prediction(doc11_preds,
                                 doc11_actuals),
                         'tpr','fpr')

doc11_auc <- performance(prediction(doc11_preds,
                                 doc11_actuals),
                         'auc')
doc11_auc <- doc11_auc@y.values[[1]]

ha04_roc <- performance(prediction(ha04_preds,
                                 ha04_actuals),
                         'tpr','fpr')

ha04_auc <- performance(prediction(ha04_preds,
                                 ha04_actuals),
                         'auc')

ha04_auc <- ha04_auc@y.values[[1]]


ha11_roc <- performance(prediction(ha11_preds,
                                 ha11_actuals),
                         'tpr','fpr')

ha11_auc <- performance(prediction(ha11_preds,
                                 ha11_actuals),
                         'auc')

ha11_auc <- ha11_auc@y.values[[1]]

cols_to_use <- c("#A1D99B", "#31A354",
                 "#BCBDDC", "#756BB1"
                 )

lgd_text <- paste(paste(c("DOC '04","DOC '11","HA '04","HA '11"), '- AUC:'),
                  round(c(doc04_auc, doc11_auc, ha04_auc, ha11_auc),2)
)


plot(doc04_roc, col = cols_to_use[1], main = 'ROC Curves for the Four Response Variables')
plot(doc11_roc, col = cols_to_use[2], add = T)
plot(ha04_roc, col = cols_to_use[3], add = T)
plot(ha11_roc, col = cols_to_use[4], add = T)
lines(seq(0,1,.1), seq(0,1,.1), lty = 2)
legend('bottomright', bty = 'n',
       col = cols_to_use,
       legend = lgd_text,
       lty = 1)




