library(data.table)
library(plyr)
library(dplyr)
library(ggplot2)
theme_set(theme_bw())
library(ggthemes)
library(scales)
#random intercept for family
#library(lme4) #may need to fit mixed models
#library(glmnet) #may need to use model selection
library(smbinning)
library(stringr)

wls <- fread("C:/Users/Brad/Desktop/STAT 998/Project 2/WLS2.csv",
             data.table = FALSE)

#convert doc2004, doc2011, hac2004, hac2011 to binary

wls$doc2004_bin <- ifelse(wls$doc2004 == 'Yes', 1, 0)
wls$doc2011_bin <- ifelse(wls$doc2011 == 'Yes', 1, 0)

wls$hac2004_bin <- ifelse(wls$HAC2004 == 'Yes', 1, 0)
wls$hac2011_bin <- ifelse(wls$HAC2011 == 'Yes', 1, 0)

wls$ha2004_bin <- ifelse(wls$HA2004 == 'Yes', 1, 0)
wls$ha2011_bin <- ifelse(wls$HA2011 == 'Yes', 1, 0)

#look at age a bit more closely

wls$age_2004 <- 2004 - ifelse(is.na(wls$birthyr) & wls$Rtype == 'g', 
                              1938.5, wls$birthyr)
wls$age_2011 <- 2011 - ifelse(is.na(wls$birthyr) & wls$Rtype == 'g', 
                              1938.5, wls$birthyr)

#create relative heart attack variable
wls$HArel2004 <- ifelse(wls$HArelless552004 == 'Yes' | wls$HArelmore552004 == 'Yes', 1, 0)
#create relative stroke variable
wls$strokefam2004 <- ifelse(wls$strokefamless652004 == 'Yes' |
                                wls$strokefammore652004 == 'Yes',
                            1, 0)



create_four_plots <- function(variable, group = TRUE, size = 2, pch = 1){
    
    
    xvar <- wls[variable]
    groups <- wls$Rtype
    doc2004 <- wls$doc2004_bin
    doc2011 <- wls$doc2011_bin
    ha2004 <- wls$ha2004_bin
    ha2011 <- wls$ha2011_bin
    
    df_doc04 <- data.frame(x = xvar,
                           groups = groups,
                           y = doc2004,
                           Measure = 'DOC 2004', 
                           stringsAsFactors = FALSE)
    
    df_doc11 <- data.frame(x = xvar,
                           groups = groups,
                           y = doc2011,
                           Measure = 'DOC 2011', 
                           stringsAsFactors = FALSE)
    
    df_ha04 <- data.frame(x = xvar,
                           groups = groups,
                           y = ha2004,
                           Measure = 'HA 2004', 
                           stringsAsFactors = FALSE)
    
    df_ha11 <- data.frame(x = xvar,
                           groups = groups,
                           y = ha2011,
                           Measure = 'HA 2011', 
                           stringsAsFactors = FALSE)
    
    
    df_full <- bind_rows(df_doc04,
                         df_ha04,
                         df_doc11,
                         df_ha11)
    names(df_full)[1] = 'x'
    
    if(group){
        ggplot(df_full, aes(x = x, y = y, colour = groups, shape = groups))+
            stat_summary(fun.y = 'mean', geom = 'point',
                         size = size)+
            facet_wrap(~Measure, ncol = 2)
    }else{
        ggplot(df_full, aes(x = x, y = y))+
            stat_summary(fun.y = 'mean', geom = 'point', pch = pch,
                         size = size)+
            facet_wrap(~Measure, ncol = 2)
    }
}

##use in presentation

p1 <- create_four_plots(variable = 'smokyrsX2004')
p1$layers <- c(stat_smooth(se = F, method = 'lm', aes(linetype = groups)),
               p1$layers)

p1 + xlab('Number of Years Respondent has Smoked (2004)')+
    ylab('Proportion of Respondents with Event') +
    scale_colour_manual(name = 'Group Type',
                        labels = c('Graduate','Sibling'),
                        values = c('#2d112b', '#ff7e49'))+
    scale_linetype_stata(name = 'Group Type',
                         labels = c('Graduate', 'Sibling'))+
    scale_shape_cleveland(name = 'Group Type',
                          labels = c('Graduate', 'Sibling'))+
    coord_cartesian(ylim = c(0, .6)) + #adjust axis
    theme(legend.position = 'top',
          legend.margin = unit(.1, 'lines'),
          panel.margin = unit(0, 'lines'),
          strip.text = element_text(face = 'bold', size = 16),
          strip.background = element_rect(fill = 'white'),
          axis.title = element_text(size = 16),
          legend.title = element_text(size = 16),
          axis.text = element_text(size = 14),
          legend.text = element_text(size = 14),
          panel.grid.minor = element_blank()
    ) -> p1_final

##use in presentation

p2 <- create_four_plots(variable = 'BMI2004')

p2$layers <- c(stat_smooth(se = F, method = 'lm', aes(linetype = groups)),
               p2$layers)

p2 + xlab('BMI (as calculated in 2004)')+
    ylab('Proportion of Respondents with Event') +
    scale_colour_manual(name = 'Group Type',
                        labels = c('Graduate','Sibling'),
                        values = c('#2d112b', '#ff7e49'))+
    scale_linetype_stata(name = 'Group Type',
                         labels = c('Graduate', 'Sibling'))+
    scale_shape_cleveland(name = 'Group Type',
                          labels = c('Graduate', 'Sibling'))+
    # coord_cartesian(ylim = c(0, .6)) + #adjust axis
    theme(legend.position = 'top',
          legend.margin = unit(.1, 'lines'),
          panel.margin = unit(0, 'lines'),
          strip.text = element_text(face = 'bold', size = 16),
          strip.background = element_rect(fill = 'white'),
          axis.title = element_text(size = 16),
          legend.title = element_text(size = 16),
          axis.text = element_text(size = 14),
          legend.text = element_text(size = 14),
          panel.grid.minor = element_blank()
    ) -> p2_final



##use in presentation

p_d <- create_four_plots(variable = 'sumdepressionindex2004')

p_d$layers <- c(stat_smooth(se = F, method = 'lm', aes(linetype = groups)),
                p_d$layers)

p_d + xlab('Summary Score for Psychological Distress/Depression (2004)')+
    ylab('Proportion of Respondents with Event') +
    scale_colour_manual(name = 'Group Type',
                        labels = c('Graduate','Sibling'),
                        values = c('#2d112b', '#ff7e49'))+
    scale_linetype_stata(name = 'Group Type',
                         labels = c('Graduate', 'Sibling'))+
    scale_shape_cleveland(name = 'Group Type',
                          labels = c('Graduate', 'Sibling'))+
    # coord_cartesian(ylim = c(0, .6)) + #adjust axis
    theme(legend.position = 'top',
          legend.margin = unit(.1, 'lines'),
          panel.margin = unit(0, 'lines'),
          strip.text = element_text(face = 'bold', size = 16),
          strip.background = element_rect(fill = 'white'),
          axis.title = element_text(size = 16),
          legend.title = element_text(size = 16),
          axis.text = element_text(size = 14),
          legend.text = element_text(size = 14),
          panel.grid.minor = element_blank()
    ) -> p_d_final

#use in presentation to note that the signal isn't actually as strong as the others

p_a <- create_four_plots(variable = 'alcoholdays2004')

p_a$layers <- c(stat_smooth(se = F, method = 'lm', aes(linetype = groups)),
                p_a$layers)

p_a + xlab('# of Days Participant Drank Alcohol Last Month (2004)')+
    ylab('Proportion of Respondents with Event') +
    scale_colour_manual(name = 'Group Type',
                        labels = c('Graduate','Sibling'),
                        values = c('#2d112b', '#ff7e49'))+
    scale_linetype_stata(name = 'Group Type',
                         labels = c('Graduate', 'Sibling'))+
    scale_shape_cleveland(name = 'Group Type',
                          labels = c('Graduate', 'Sibling'))+
    # coord_cartesian(ylim = c(0, .6)) + #adjust axis
    theme(legend.position = 'top',
          legend.margin = unit(.1, 'lines'),
          panel.margin = unit(0, 'lines'),
          strip.text = element_text(face = 'bold', size = 16),
          strip.background = element_rect(fill = 'white'),
          axis.title = element_text(size = 16),
          legend.title = element_text(size = 16),
          axis.text = element_text(size = 14),
          legend.text = element_text(size = 14),
          panel.grid.minor = element_blank()
    ) -> p_a_final



# Summary statistics:
#     for a few basic variables do some summary tests, like t-tests comparing 
# doc2004_bin = 0 to doc2004_bin = 1
# 
# for continuous:
#     
#     for about 10 (ish) variables have a table that has
# variable name | mean (sd) group 0 | mean (sd) group 1 | t-stat | p-value |
#     
#     I would propose the following variables:
#     
# mosteverweigh2004, BMI2004, smokyrsX2004,
# ltactaloneothers2004, sumdepressionindex2004,
# extraversion2004, watchtvhrs2004, sumanxietyindex2004,
# alcoholdays2004 (curious result for alcoholdays)
# 
# A few four panel plots which detail the important variables
# sumdepressionindex2004, BMI2004, smokyrsX2004, alcoholdays2004 (signal not as strong)
# 


aggregate(mosteverweigh2004 ~ ha2004_bin, data = wls,
          function(x) c('mean' = mean(x, na.rm = T), 
                        'sd' = sd(x, na.rm = T)))

# we should actually extract the F statistic coming from a model adjusted
# for age, rtype, and sex

anova(lm(mosteverweigh2004 ~ age_2004 + sex + Rtype + ha2004_bin, data = wls))


form_summary <- character(1)
right_side_summary <- ' ~ doc2004_bin'
form_anova <- character(1)
right_side_anova <- ' ~ age_2004 + sex + Rtype + doc2004_bin'

names_to_analyze <- c('mosteverweigh2004','BMI2004','smokyrsX2004',
                      'ltactaloneothers2004','sumdepressionindex2004',
                      'extraversion2004','watchtvhrs2004','sumanxietyindex2004',
                      'alcoholdays2004')

agg_value <- NULL
anova_value <- NULL

summary_df <- data.frame('Variable' = character(0),
                         'Mean(sd)0' = character(0),
                         'Mean(sd)1' = character(0),
                         'Fstat' = numeric(0),
                         'pvalue' = numeric(0),
                         stringsAsFactors = FALSE
                         )

for(i in 1:length(names_to_analyze)){
    
    form_summary <- paste0(names_to_analyze[i],
                           right_side_summary)
    
    form_anova <- paste0(names_to_analyze[i],
                         right_side_anova)
    
    aggregate(as.formula(form_summary),
              data = wls,
              function(x) 
                  c('mean' = mean(x, na.rm = T),
                    'sd' = sd(x, na.rm = T))) -> agg_value
    
    anova(lm(as.formula(form_anova), data = wls)) -> anova_value
    
    final_vec <-
        c(
            'Variable' = names_to_analyze[i],
            'Mean(sd)0' = paste0(round(agg_value[1,2][1], 1)
                                 ,' (',
                                 round(agg_value[1,2][2], 1), ')'),
            'Mean(sd)1' = paste0(round(agg_value[2,2][1], 1)
                                 ,' (',
                                 round(agg_value[2,2][2], 1), ')'),
            
            'Fstat' = round(anova_value[4,4], 1),
            'pvalue' = round(anova_value[4,5], 3)
            
        )
    summary_df[i,] = final_vec
    
    
    
}


# for binary:
#     variable name | proportion of events for group 0 | proportion of events for group 1 | chisquared | p-value |
#     
#     I propose the following variables:
#     sex, diabetes2004, stroke2004, 
# highchol2004, smokeever2004, HArelless552004,
# strokefamless652004, highbp2004, servedwar2004

#convert sex to binary

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

binary_names <- c("sex_binary", "diabetes2004_binary", "stroke2004_binary",
                  "highchol2004_binary","smokever2004_binary", "HArelless552004_binary", 
                  "strokefamless652004_binary", "highbp2004_binary")

agg_value <- NULL
chisq_test <- NULL

agg_form <- 'doc2004_bin ~ '

summary_df <- data.frame('Variable' = character(0),
                         'EventRate0' = numeric(0),
                         'EventRate1' = numeric(0),
                         'Chisq' = numeric(0),
                         'pvalue' = numeric(0),
                         stringsAsFactors = FALSE)
final_vec <- NULL

for(i in 1:length(binary_names)){
    
    
    agg_value <- aggregate(as.formula(paste0(agg_form, binary_names[i])),
                           data = wls, mean)
    
    chisq_test <- chisq.test(
        table(wls[c(binary_names[i], 'doc2004_bin')])
    )
    
    final_vec <- c(
        'Variable' = binary_names[i],
        'EventRate0' = scales::percent(agg_value[1,2]),
        'EventRate1' = scales::percent(agg_value[2,2]),
        'Chisq' = round(chisq_test$statistic,1),
        'pvalue' = round(chisq_test$p.value, 3)
    )
    
    summary_df[i,] = final_vec
}



