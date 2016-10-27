
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

resp_vars <- c('doc2004_bin','doc2011_bin','ha2004_bin',
               'ha2011_bin')


form_summary <- character(1)
right_side_summary <- ' ~ '
form_anova <- character(1)
right_side_anova <- ' ~ age_2004 + sex + Rtype + '

names_to_analyze_04 <- c("mosteverweigh2004", 
                         "BMI2004", 
                         "smokpkyrs2004",
                         "sumdepressionindex2004",
                         "watchtvhrs2004", 
                         'sleeprestlessly2004',
                         "sumanxietyindex2004", 
                         'sumangerindex2004',
                         "ltactaloneothers2004", 
                         "extraversion2004", 
                         "alcoholdays2004")

names_to_analyze_11 <- c("mosteverweigh2011", 
                         "BMI2011", 
                         "smokpkyrs2011",
                         "sumdepressionindex2011",
                         "watchtvhrs2011", 
                         'sleeprestlessly2011',
                         "sumanxietyindex2011", 
                         'sumangerindex2011',
                         "ltactaloneothers2004", 
                         "extraversion2011", 
                         "alcoholdays2011")

agg_value <- NULL
anova_value <- NULL

summary_df <- data.frame('Variable' = character(0),
                         'Mean(sd)0' = character(0),
                         'Mean(sd)1' = character(0),
                         'Fstat' = numeric(0),
                         'pvalue' = numeric(0),
                         'N' = numeric(0),
                         stringsAsFactors = FALSE
)

for(j in resp_vars){
    
year_2004 <- stringr::str_detect(j, '2004')
    
for(i in 1:length(names_to_analyze_04)){
    
    if(year_2004){
    form_summary <- paste0(names_to_analyze_04[i],
                           ' ~ ', j)
    
    form_anova <- paste0(names_to_analyze_04[i],
                         right_side_anova, j)    
    }else{
        form_summary <- paste0(names_to_analyze_11[i],
                               ' ~ ', j)
        
        form_anova <- paste0(names_to_analyze_11[i],
                             right_side_anova, j)
    }
    
    
    aggregate(as.formula(form_summary),
              data = wls,
              function(x) 
                  c('mean' = mean(x, na.rm = T),
                    'sd' = sd(x, na.rm = T))) -> agg_value
    
    anova(lm(as.formula(form_anova), data = wls)) -> anova_value
    
    final_vec <-
        c(
            'Variable' = ifelse(year_2004, names_to_analyze_04[i], names_to_analyze_11[i]),
            'Mean(sd)0' = paste0(round(agg_value[1,2][1], 1)
                                 ,' (',
                                 round(agg_value[1,2][2], 1), ')'),
            'Mean(sd)1' = paste0(round(agg_value[2,2][1], 1)
                                 ,' (',
                                 round(agg_value[2,2][2], 1), ')'),
            
            'Fstat' = round(anova_value[4,4], 1),
            'pvalue' = round(anova_value[4,5], 3),
            'N' = sum(anova_value[,1])
            
        )
    summary_df[i,] = final_vec
    
    }
    assign(paste0('summary_df',j),
           summary_df)
    summary_df <- data.frame('Variable' = character(0),
                             'Mean(sd)0' = character(0),
                             'Mean(sd)1' = character(0),
                             'Fstat' = numeric(0),
                             'pvalue' = numeric(0),
                             'N' = numeric(0),
                             stringsAsFactors = FALSE
    )
    
}


summary_dfdoc2004_bin
summary_dfdoc2011_bin
summary_dfha2004_bin
summary_dfha2011_bin




convert_yes <- function(x) ifelse(x == 'Yes', 1, 0)

wls$sex_binary <- ifelse(wls$sex == 'Male', 0, 1)
wls$diabetes2004_binary <- convert_yes(wls$diabetes2004)
wls$stroke2004_binary <- convert_yes(wls$stroke2004)
wls$highchol2004_binary <- convert_yes(wls$highchol2004)
wls$smokever2004_binary <- convert_yes(wls$smokever2004)
wls$HArelless552004_binary <- convert_yes(wls$HArelless552004)
wls$strokefamless652004_binary <- convert_yes(wls$strokefamless652004)
wls$highbp2004_binary <- convert_yes(wls$highbp2004)
wls$servedwar2004 <- convert_yes(wls$servedwar2004)

wls$diabetes2011_binary <- convert_yes(wls$diabetes2011)
wls$stroke2011_binary <- convert_yes(wls$stroke2011)
wls$highchol2011_binary <- convert_yes(wls$highchol2011)
wls$smokever2011_binary <- convert_yes(wls$smokever2011)
#wls$HArelless552011_binary <- convert_yes(wls$HArelless552011)
#wls$strokefamless652011_binary <- convert_yes(wls$strokefamless652011)
wls$highbp2011_binary <- convert_yes(wls$highbp2011)
#wls$servedwar2011 <- convert_yes(wls$servedwar2011)

binary_names_04 <- c("sex_binary", "diabetes2004_binary", "stroke2004_binary",
                  "highchol2004_binary","smokever2004_binary", "HArelless552004_binary", 
                  "strokefamless652004_binary", "highbp2004_binary")

binary_names_11 <- c("sex_binary", "diabetes2011_binary", "stroke2011_binary",
                     "highchol2011_binary","smokever2011_binary", "HArelless552004_binary", 
                     "strokefamless652004_binary", "highbp2011_binary")

agg_value <- NULL
chisq_test <- NULL


summary_df_cs <- data.frame('Variable' = character(0),
                         'EventRate0' = numeric(0),
                         'EventRate1' = numeric(0),
                         'Chisq' = numeric(0),
                         'pvalue' = numeric(0),
                         'N' = numeric(0),
                         stringsAsFactors = FALSE)
final_vec <- NULL

for(j in resp_vars){

agg_form <- paste(j, '~')

year_2004 <- stringr::str_detect(j, '2004')
    
for(i in 1:length(binary_names_04)){

    if(year_2004){
    agg_value <- aggregate(as.formula(paste0(agg_form, binary_names_04[i])),
                           data = wls, mean)
    
    chisq_test <- chisq.test(
        table(wls[c(binary_names_04[i], j)])
    )
    }else{
        agg_value <- aggregate(as.formula(paste0(agg_form, binary_names_11[i])),
                               data = wls, mean)
        
        chisq_test <- chisq.test(
            table(wls[c(binary_names_11[i], j)])
        )   
    }
    
    final_vec <- c(
        'Variable' = ifelse(year_2004, binary_names_04[i], binary_names_11[i]),
        'EventRate0' = scales::percent(agg_value[1,2]),
        'EventRate1' = scales::percent(agg_value[2,2]),
        'Chisq' = round(chisq_test$statistic,1),
        'pvalue' = round(chisq_test$p.value, 3),
        'N' = sum(chisq_test$observed)
    )
    
    summary_df_cs[i,] = final_vec
}

assign(paste0('summary_df_cs',j), summary_df_cs)

summary_df_cs <- data.frame('Variable' = character(0),
                            'EventRate0' = numeric(0),
                            'EventRate1' = numeric(0),
                            'Chisq' = numeric(0),
                            'pvalue' = numeric(0),
                            'N' = numeric(0),
                            stringsAsFactors = FALSE)
}

summary_df_csdoc2004_bin
summary_df_csdoc2011_bin
summary_df_csha2004_bin
summary_df_csha2011_bin


#summary tables for all of the models

#write a function which takes a model,
#prints its coefficients along with odds ratios
#and 95% CIs for the odds ratios

print_summary <- function(model){
    
    model_coefs <- summary(model)$coefficients
    model_coefs[,1:3] <- round(model_coefs[,1:3], 3)
    model_coefs[,4] <- round(model_coefs[,4], 4)
    
    odds_ratios <- round(exp(model_coefs[,1]), 3)
    
    ci_90 <- confint.merMod(model, method = 'Wald', level = .95)
    
    ci_90 <- ci_90[-1,]
    ci_90[1,] <- NA
    
    or_ci_90 <- exp(ci_90)
    
    or_ci_final <- apply(or_ci_90, 1, function(x)
        paste0('(', round(x[1], 2), ' - ', round(x[2], 2),
               ')'))
    
    or_ci_final[1] = ''
    
    final_table <- cbind.data.frame(
        model_coefs[,1],
        odds_ratios,
        or_ci_final,
        model_coefs[,-1]
    )
    
    names(final_table) <- c('Coefficient',
                            'Odds Ratio',
                            'OR CI (2.5% - 97.5%)',
                            'SE',
                            'z',
                            'P(>|z|)')
    
    final_table
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


print_summary(doc2004_glm)


doc2011_glm <- glmer(formula = doc2011_bin ~ sex + age_2011 + Rtype + THI2011 +
                         BMI2011 + 
                         highbp2011 + smokpkyrs2004 + diabetes2011 + 
                         alcoholdays2011 + mosteverweigh2011 + HArelless552004 +                                           highcholfam2004 + highchol2011 + stroke2011 +                                                     sumdepressionindex2011 + (1 | idpub), 
                     family = "binomial", data = wls_doc2011.full,
                     control = glmerControl(
                         optimizer = "optimx", calc.derivs = FALSE,
                         optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)),
                     na.action = na.exclude)

print_summary(doc2011_glm)

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

print_summary(ha2004_glm)

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

print_summary(ha2011_glm)
