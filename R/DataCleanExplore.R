library(data.table)
library(plyr)
library(dplyr)
library(ggplot2)
theme_set(theme_bw())
library(lmer) #may need to fit mixed models
library(glmnet) #may need to use model selection

wls <- fread("C:/Users/Brad/Desktop/STAT 998/Project 2/WLS2.csv")

#convert doc2004 and doc2011 to binary

wls$doc2004_bin <- ifelse(wls$doc2004 == 'Yes', 1, 0)
wls$doc2011_bin <- ifelse(wls$doc2011 == 'Yes', 1, 0)


table(wls$doc2004_bin, wls$doc2011_bin, useNA = 'always',
      dnn = list('doc2004','doc2011'))

#what is the appropriate response variable?
apply(wls[,3:8, with = F], 2, function(x) sum(is.na(x)))
apply(wls[,3:8, with = F], 2, function(x) mean(is.na(x)))

#HA2011 and HA2004 exhibit more missingness than
#HAC2011 and HAC2004
#should we even use these measures?

#I would propose just investigating HAC2004, HAC2011, doc2004, and doc2011

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