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
