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

#170 with doc2004 = 1 but doc2011 = 0??
table(wls$doc2004_bin, wls$doc2011_bin, useNA = 'always',
      dnn = list('doc2004','doc2011'))

#63 with hac2004 = 'yes' but hac2011 = 'no'??
table(wls$HAC2004, wls$HAC2011, useNA = 'always',
      dnn = c('hac2004','hac2011'))

#what is the appropriate response variable?
apply(wls[,3:8], 2, function(x) sum(is.na(x)))
apply(wls[,3:8], 2, function(x) mean(is.na(x)))

#HA2011 and HA2004 exhibit more missingness than
#HAC2011 and HAC2004
#should we even use these measures?

#I would propose just investigating HAC2004, HAC2011, doc2004, and doc2011

#of course, we are interested in historical data
#how did status in 1993 & 2004 affect 2011 status?
#is it also okay to use 2004 data to investigate 2004 status?

#check out general missingness

miss <- stack(apply(wls, 2, function(x) mean(is.na(x))),
              stringsAsFactors = FALSE)

miss$ind <- as.character(miss$ind)

miss$year <- substr(miss$ind, start = nchar(miss$ind) - 3, 
                    stop = nchar(miss$ind))

miss$year <- ifelse(! miss$year %in% c('1993','2004','2011'), 
                    'other', miss$year)

miss <- miss %>% arrange(-values)

miss$ind <- factor(miss$ind, levels = miss$ind)

ggplot(miss, aes(x = ind, y = values, colour = year))+
    geom_point()+
    coord_flip()+
    theme(axis.text.y = element_blank()) -> p1 

mean(miss$values <= 0.50) #44% of values exhibit less than 50% missingness

#let's break it down by response variable
#i.e. given you have a non-missing response, what is the probability
#you have missinginess in an independent variable?
#does this vary by responder (g or s) 

create_missing_table <- function(x){
    x1 <- stack(apply(x, 2, function(y) mean(is.na(y))))
    x1$ind <- as.character(x1$ind)
    x1$year <- substr(x1$ind, start = nchar(x1$ind) - 3, 
                        stop = nchar(x1$ind))
    
    x1$year <- ifelse(! x1$year %in% c('1993','2004','2011'), 
                        'other', x1$year)
    
    x1
}

miss_hac2004 <- create_missing_table(wls[!is.na(wls$HAC2004),])

sapply(c(.1, .2, .5), function(x) mean(miss_hac2004$values <= x))

#[1] 0.06730769 0.32692308 0.73717949

#look at missingness by year
do.call('rbind',
by(miss_hac2004, miss_hac2004$year, 
   function(x)
       sapply(c(.1,.2,.5), function(y) mean(x$values <= y)))
)

#           [,1]       [,2]      [,3]
# 1993      0.07843137 0.1372549 0.6078431
# 2004      0.07692308 0.6223776 0.7552448
# 2011      0.00000000 0.0000000 0.7614679
# other     0.66666667 0.6666667 0.8888889

miss_hac2011 <- create_missing_table(wls[!is.na(wls$HAC2011),])

sapply(c(.1, .2, .5), function(x) mean(miss_hac2011$values <= x))

#[1] 0.1057692 0.5641026 0.7564103

do.call('rbind',
        by(miss_hac2011, miss_hac2011$year, 
           function(x)
               sapply(c(.1,.2,.5), function(y) mean(x$values <= y)))
)

#       [,1]       [,2]      [,3]
# 1993  0.13725490 0.2941176 0.6666667
# 2004  0.06293706 0.6433566 0.7552448
# 2011  0.09174312 0.5596330 0.7889908
# other 0.77777778 0.7777778 0.8888889


miss_doc2004 <- create_missing_table(wls[!is.na(wls$doc2004),])

sapply(c(.1, .2, .5), function(x) mean(miss_doc2004$values <= x))

#[1] 0.06730769 0.32692308 0.73717949

do.call('rbind',
        by(miss_doc2004, miss_doc2004$year, 
           function(x)
               sapply(c(.1,.2,.5), function(y) mean(x$values <= y)))
)

#       [,1]       [,2]      [,3]
# 1993  0.07843137 0.1372549 0.6078431
# 2004  0.07692308 0.6223776 0.7552448
# 2011  0.00000000 0.0000000 0.7614679
# other 0.66666667 0.6666667 0.8888889

miss_doc2011 <- create_missing_table(wls[!is.na(wls$doc2011),])

sapply(c(.1, .2, .5), function(x) mean(miss_doc2011$values <= x))

#[1] 0.1057692 0.5641026 0.7564103

do.call('rbind',
        by(miss_doc2011, miss_doc2011$year, 
           function(x)
               sapply(c(.1,.2,.5), function(y) mean(x$values <= y)))
)

#       [,1]       [,2]      [,3]
# 1993  0.13725490 0.2941176 0.6666667
# 2004  0.06293706 0.6433566 0.7552448
# 2011  0.09174312 0.5688073 0.7889908
# other 0.77777778 0.7777778 0.8888889



wls_g <- subset(wls, Rtype == 'g')
wls_s <- subset(wls, Rtype == 's')

miss_g <- stack(apply(wls_g, 2, function(x) mean(is.na(x))),
              stringsAsFactors = FALSE)

miss_g$ind <- as.character(miss_g$ind)

miss_g$year <- substr(miss_g$ind, start = nchar(miss_g$ind) - 3, 
                    stop = nchar(miss_g$ind))

miss_g$year <- ifelse(! miss_g$year %in% c('1993','2004','2011'), 
                    'other', miss_g$year)

miss_g <- miss_g %>% arrange(-values)
miss_g$Rtype = 'g'

miss_s <- stack(apply(wls_s, 2, function(x) mean(is.na(x))),
                stringsAsFactors = FALSE)

miss_s$ind <- as.character(miss_s$ind)

miss_s$year <- substr(miss_s$ind, start = nchar(miss_s$ind) - 3, 
                      stop = nchar(miss_s$ind))

miss_s$year <- ifelse(! miss_s$year %in% c('1993','2004','2011'), 
                      'other', miss_s$year)

miss_s <- miss_s %>% arrange(-values)

miss_s$Rtype = 's'

miss_gs <- rbind(miss_g, miss_s)

miss_gs %>% 
    group_by(ind) %>% 
    summarise(meds = median(values)) %>%
    arrange(desc(meds)) %>%
    select(ind) -> levels_ind

levels_ind <- c(levels_ind)$ind

miss_gs$ind_f <- factor(miss_gs$ind, levels = c(levels_ind))

#less missingness for 2004 versus 2011
#less missingness for g versus s

ggplot(miss_gs, aes(x = ind_f, y = values, colour = year))+
    geom_point()+
    coord_flip()+
    facet_wrap(~Rtype)+
    xlab('')+ylab('Percentage of Missing Values')+
    theme(axis.text.y = element_blank(),
          panel.grid = element_blank(),
          axis.ticks = element_blank())+
    scale_y_continuous(labels = percent)

#a lot of the variables are related to survey questions
#let's start doing some cross tabulations
quick_table <- function(x, y){
    names_x = match.call()[2]
    names_y = match.call()[c(3)]
    t_xy <- table(x, y, dnn = c(names_x, names_y))
    missing_table <- table(is.na(x), is.na(y),
                           dnn = c(names_x, names_y))
    list(
        'table' = t_xy,
        'proptable' = prop.table(t_xy, margin = 1),
        'chisq' = chisq.test(t_xy, correct = FALSE),
        'missing' = missing_table
    )
}


#look at wls summary variables

wls_summary <- c("sumangerindex2004", "sumangerindex2011", "sumhostilityindex2004", 
                 "sumhostilityindex2011", "sumanxietyindex2004", "sumanxietyindex2011", 
                 "sumwellbeing1993", "sumdepressionindex1993", "sumdepressionindex2004", 
                 "sumdepressionindex2011", "sumfamilystress1993", "extraversion1993", 
                 "extraversion2004", "extraversion2011", "openness1993", "openness2004", 
                 "openness2011", "neuroticism1993", "neuroticism2004", "neuroticism2011", 
                 "conscientiousness1993", "conscientiousness2004", "conscientiousness2011", 
                 "agreeableness1993", "agreeableness2004", "agreeableness2011"
)

t(apply(wls[wls_summary], 2, summary))

#start creating plots
ggplot(wls, aes(x = BMI2004, y = doc2004_bin, colour = Rtype))+
    stat_summary(fun.y = 'mean', geom = 'point')+
    stat_smooth(se = F, method = 'lm')

mean(is.na(wls[!is.na(wls$doc2004_bin), ]$BMI2004))

smbinning(df = wls, y = 'doc2004_bin', x = 'BMI2004')

ggplot(wls, aes(x = BMI1993, y = doc2004_bin))+
    stat_summary(fun.y = 'mean', geom = 'point')+
    stat_smooth(se = F, method = 'lm')

mean(is.na(wls[!is.na(wls$doc2004_bin),]$BMI1993))

smbinning(df = wls, y = 'doc2004_bin', x = 'BMI1993')

names_wls_93_04 <- names(wls)[str_detect(names(wls), "1993|2004")]
names_wls_93_04 <- names_wls_93_04[-c(1:3, 192)]

names_wls_93_04_11 <- names(wls)[str_detect(names(wls), "1993|2004|2011")]
names_wls_93_04_11 <- names_wls_93_04_11[-c(1:6)]


table_chisq_doc04 <- data.frame(variable = names_wls_93_04,
                          chisq = 0, stringsAsFactors = FALSE)

table_chisq_doc11 <- data.frame(variable = names_wls_93_04_11,
                                chisq = 0, stringsAsFactors = FALSE)


table_chisq_hac04 <- data.frame(variable = names_wls_93_04,
                                chisq = 0, stringsAsFactors = FALSE)

table_chisq_hac11 <- data.frame(variable = names_wls_93_04_11,
                                chisq = 0, stringsAsFactors = FALSE)

#extract chi squared for doc2004 and doc2011

#for loop for doc2004
for(i in 1:nrow(table_chisq_doc04)){
    
    table_chisq_doc04[i,2] =
      chisq.test(
        table(
            data.frame(wls['doc2004_bin'],
              wls[table_chisq_doc04[i,1]]
              )
            )
        )$statistic
    
}
#filter data frame
#select top 30
table_chisq_doc04 %>%
    filter(! variable %in% c('doc2004_bin', 'hac2004_bin')) %>%
    arrange(desc(chisq)) %>%
    head(., 30) -> doc04_top30

#for loop for doc2011

for(i in 1:nrow(table_chisq_doc11)){
    
    table_chisq_doc11[i,2] =
        chisq.test(
            table(
                data.frame(wls['doc2011_bin'],
                           wls[table_chisq_doc11[i,1]]
                )
            )
        )$statistic
    
}

#filter data frame
#select top 30
table_chisq_doc11 %>%
    filter(! variable %in% c('doc2004_bin', 'hac2004_bin',
                             'doc2011_bin', 'hac2011_bin')) %>%
    arrange(desc(chisq)) %>%
    head(., 30) -> doc11_top30

#frequently occuring top variables
top_variables_doc <- sort(table(c(doc04_top20$variable, doc11_top20$variable)),
     decreasing = TRUE) %>% 
    stack %>%
    .[,c(2,1)]

#for loop for hac2004

for(i in 1:nrow(table_chisq_hac04)){
    
    table_chisq_hac04[i,2] =
        chisq.test(
            table(
                data.frame(wls['hac2004_bin'],
                           wls[table_chisq_hac04[i,1]]
                )
            )
        )$statistic
    
}
#filter data frame
#select top 30
table_chisq_hac04 %>%
    filter(! variable %in% c('doc2004_bin', 'hac2004_bin')) %>%
    arrange(desc(chisq)) %>%
    head(., 30) -> hac04_top30

#for loop for hac2011_bin

for(i in 1:nrow(table_chisq_hac11)){
    
    table_chisq_hac11[i,2] =
        chisq.test(
            table(
                data.frame(wls['hac2011_bin'],
                           wls[table_chisq_hac11[i,1]]
                )
            )
        )$statistic
    
}

table_chisq_hac11 %>%
    filter(! variable %in% c('doc2004_bin', 'hac2004_bin',
                             'doc2011_bin', 'hac2011_bin')) %>%
    arrange(desc(chisq)) %>%
    head(., 30) -> hac11_top30


top_variables_hac <- sort(table(c(hac04_top30$variable, hac11_top30$variable)),
                          decreasing = TRUE) %>% 
    stack %>%
    .[,c(2,1)]

top_variables_doc_hac <- 
    sort(
        table(
            c(hac04_top30$variable,
              hac11_top30$variable,
              doc04_top30$variable,
              doc11_top30$variable)
        ), decreasing = TRUE
    ) %>% stack %>% .[,c(2,1)]


#create a table of the top 10 variables for each variable

df1 <- cbind(
    doc04_top30[1:10, 1],
    doc11_top30[1:10, 1],
    hac04_top30[1:10, 1],
    hac11_top30[1:10, 1]
    
)

colnames(df1) <-  c('DOC_04','DOC_11','HAC_04','HAC_11')

#ten variables in top 30 for each response
top_variables_doc_hac[1:10,]

#age, weight, smoking, health conditions


glm1 <- glm(doc2004_bin ~ highchol2004 + highbp2004 +
        age_2004 + BMI1993  + smokpkdayX2004 + 
        diabetes1993, data = wls,
    family = 'binomial')

#there are some variables we can recode
#smokyrsX2004 - code everything over 40 (35?) to 40

ggplot(wls, aes(x = BMI2004, y = doc2004_bin, colour = factor(highchol2004)))+
    stat_summary(fun.y = 'mean', geom = 'point')+
    stat_smooth(method = 'lm',se = F)



create_four_plots <- function(variable, group = TRUE, size = 2, pch = 1){
    
    
    xvar <- c(wls[variable])
    groups <- wls$Rtype
    doc2004 <- wls$doc2004_bin
    doc2011 <- wls$doc2011_bin
    hac2004 <- wls$hac2004_bin
    hac2011 <- wls$hac2011_bin
    
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
    
    df_hac04 <- data.frame(x = xvar,
                           groups = groups,
                           y = hac2004,
                           Measure = 'HAC 2004', 
                           stringsAsFactors = FALSE)
    
    df_hac11 <- data.frame(x = xvar,
                           groups = groups,
                           y = hac2011,
                           Measure = 'HAC 2011', 
                           stringsAsFactors = FALSE)
    
    
    df_full <- bind_rows(df_doc04,
                         df_hac04,
                         df_doc11,
                         df_hac11)
    names(df_full)[1] = 'x'
    
    if(group){
        ggplot(df_full, aes(x = x, y = y, colour = groups))+
            stat_summary(fun.y = 'mean', geom = 'point', pch = pch,
                         size = size)+
            facet_wrap(~Measure, ncol = 2)
    }else{
    ggplot(df_full, aes(x = x, y = y))+
        stat_summary(fun.y = 'mean', geom = 'point', pch = pch,
                     size = size)+
        facet_wrap(~Measure, ncol = 2)
    }
}

p1 <- create_four_plots(variable = 'smokyrsX2004')
p1$layers <- c(stat_smooth(se = F, method = 'lm', aes(linetype = groups)),
               p1$layers)

p1 + xlab('Number of Years Respondent has Smoked (2004)')+
    ylab('Proportion of Respondents with Event') +
    scale_colour_colorblind(name = 'Group Type',
                            labels = c('Graduate','Sibling'))+
    scale_linetype_stata(name = 'Group Type',
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

p2 <- create_four_plots(variable = 'BMI1993')

p2$layers <- c(stat_smooth(se = F, method = 'lm', aes(linetype = groups)),
               p2$layers)

p2 + xlab('BMI (as calculated in 1993)')+
    ylab('Proportion of Respondents with Event') +
    scale_colour_colorblind(name = 'Group Type',
                            labels = c('Graduate','Sibling'))+
    scale_linetype_stata(name = 'Group Type',
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


p3 <- create_four_plots(variable = 'age_2004')

p3$layers <- c(stat_smooth(se = F, method = 'lm', aes(linetype = groups)),
               p3$layers)

p3 +xlab('Age (as calculated in 2004)')+
    ylab('Proportion of Respondents with Event') +
    scale_colour_colorblind(name = 'Group Type',
                            labels = c('Graduate','Sibling'))+
    scale_linetype_stata(name = 'Group Type',
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
    ) -> p3_final

#signal not as strong for this one

p4 <- create_four_plots(variable = 'smokpkdayX2004')
p4$layers <- c(stat_smooth(se = F, method = 'lm', aes(linetype = groups)),
               p4$layers)

p4 + xlab('Number of Packs per Day (2004)')+
    ylab('Proportion of Respondents with Event') +
    scale_colour_colorblind(name = 'Group Type',
                            labels = c('Graduate','Sibling'))+
    scale_linetype_stata(name = 'Group Type',
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
    ) -> p4_final
