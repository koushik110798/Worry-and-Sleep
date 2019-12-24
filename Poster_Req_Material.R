## ------------------------------------------------------------------------
# I set some GLOBAL R chunk options here.
#   (to hide this message add "echo=FALSE" to the code chunk options)
# In particular, see the fig.height and fig.width (in inches)
#   and notes about the cache option.

fn_this1 <- "ADA1_HW_02_LiteratureReview.Rmd"
setwd("/Users/vkoushikmuthyapu/desktop/ADA1")
library(knitr)
knitr::purl(fn_this1)
rmarkdown::render(fn_this1)
  
knitr::opts_chunk$set(comment = NA, message = FALSE, warning = FALSE, width = 100)
knitr::opts_chunk$set(fig.align = "center", fig.height = 4, fig.width = 6)

# Note: The "cache=TRUE" option will save the computations of code chunks
#   so R it doesn't recompute everything every time you recompile.
#   This can save _tons of time_ if you're working on a small section of code
#   at the bottom of the document.
#   Code chunks will be recompiled only if they are edited.
#   The autodep=TRUE will also update dependent code chunks.
#   A folder is created with the cache in it -- if you delete the folder, then
#   all the code chunks will recompute again.
#   ** If things are working as expected, or I want to freshly compute everything,
#      I delete the *_cache folder.
knitr::opts_chunk$set(cache = FALSE) #, autodep=TRUE)  #$


## ------------------------------------------------------------------------
setwd("/Users/vkoushikmuthyapu/Desktop/ADA1")
# data analysis packages
library(tidyverse)  # Data manipulation and visualization suite
library(forcats)    # Factor variables
library(lubridate)  # Dates

  ## 1. Download the ".RData" file for your dataset into your ADA Folder.
  ## 2. Use the load() statement for the dataset you want to use.
  ##
  ## load("AddHealth.RData")
  ## load("addhealth_public4.RData")
  ## load("NESARC.RData")

# read data
load("addhealth_public4.RData")

dim(addhealth_public4)


## ------------------------------------------------------------------------
# variables to include in our data subset
addhealth_public4_sub <-
  addhealth_public4 %>%
  select(
    aid
  , h4sp1h
  , h4sp1m
  , h4sp1t
  , h4sp2h
  , h4sp2m
  , h4sp2t
  , h4pe6
  , h4id5j
  , h4hr5a
  )


## ------------------------------------------------------------------------
# size of subset
dim(addhealth_public4_sub)
# structure of subset
str(addhealth_public4_sub)


## ------------------------------------------------------------------------
addhealth_public4_sub <-
  addhealth_public4_sub %>%
  dplyr::rename(
    aid = aid
  , workDayUp = h4sp1h 
  , workDayUpMin = h4sp1m
  , workDayUpAP = h4sp1t
  , workDayDown = h4sp2h
  , workDayDownMin = h4sp2m 
  , workDayDownAP = h4sp2t
  , worry = h4pe6
  , anxity = h4id5j
  , gender = h4hr5a
  )

str(addhealth_public4_sub)


## ------------------------------------------------------------------------
summary(addhealth_public4_sub)


## ------------------------------------------------------------------------


addhealth_public4_sub <-
  addhealth_public4_sub %>%
  mutate(
  # replace missing values
    anxity   = replace(anxity  , anxity   %in% c(6), NA)
  , workDayUp     = replace(workDayUp    , workDayUp     %in% c(96), NA)
  , workDayUp     = replace(workDayUp    , workDayUp     %in% c(98), NA)
  , workDayUpMin     = replace(workDayUpMin    , workDayUpMin     %in% c(96), NA)
  , workDayUpMin     = replace(workDayUpMin    , workDayUpMin     %in% c(98), NA)
  , workDayUpAP       = replace(workDayUpAP      , workDayUpAP       %in% c(6), NA)
  , workDayUpAP       = replace(workDayUpAP      , workDayUpAP       %in% c(8), NA)
  , workDayDown     = replace(workDayDown    , workDayDown     %in% c(96), NA)
  , workDayDown     = replace(workDayDown    , workDayDown     %in% c(98), NA)
  , workDayDownMin     = replace(workDayDownMin    , workDayDownMin     %in% c(96), NA)
  , workDayDownMin     = replace(workDayDownMin    , workDayDownMin    %in% c(98), NA)
  , workDayDownAP     = replace(workDayDownAP    , workDayDownAP     %in% c(6), NA)
  , workDayDownAP     = replace(workDayDownAP    , workDayDownAP     %in% c(8), NA)
  , worry = replace(worry    , worry     %in% c(6), NA)
  , worry = replace(worry    , worry     %in% c(8), NA)
  , gender = replace(gender    , gender     %in% c(6), NA)
  , gender = replace(gender    , gender     %in% c(7), NA)
  , gender = replace(gender    , gender     %in% c(8), NA)
  , gender = replace(gender    , gender     %in% c(.), NA)
  )



## ------------------------------------------------------------------------
table(addhealth_public4_sub$workDayUp)
table(addhealth_public4_sub$workDayUpAP)
table(addhealth_public4_sub$workDayDown)
table(addhealth_public4_sub$workDayDownAP)
table(addhealth_public4_sub$worry)
table(addhealth_public4_sub$anxity)
table(addhealth_public4_sub$workDayUpMin)
table(addhealth_public4_sub$workDayDownMin)
table(addhealth_public4_sub$gender)



## ------------------------------------------------------------------------
str(addhealth_public4_sub)
dim(addhealth_public4_sub)


## ------------------------------------------------------------------------
addhealth_public4_sub$anxity <-
  factor(addhealth_public4_sub$anxity
  , levels = c( 0
              , 1
              )
  , labels = c( "No"
              , "Yes"
              )
  )
# check ordering with a frequency table
table(addhealth_public4_sub$anxity)

addhealth_public4_sub$gender <-
  factor(addhealth_public4_sub$gender
  , levels = c( 1
              , 2
              )
  , labels = c( "male"
              , "female"
              )
  )
# check ordering with a frequency table
table(addhealth_public4_sub$gender)

addhealth_public4_sub$worry <-
  factor(addhealth_public4_sub$worry
  , levels = c( 1
              , 2
              , 3
              , 4
              , 5
              )
  , labels = c( "SA"
              , "A"
              , "NA"
              , "D"
              , "SD"
              )
  )
# check ordering with a frequency table
table(addhealth_public4_sub$worry)

addhealth_public4_sub$workDayUpAP <-
  factor(addhealth_public4_sub$workDayUpAP
  , levels = c( 1
              , 2
              )
  , labels = c( "AM"
              , "PM"
              )
  )
# check ordering with a frequency table
table(addhealth_public4_sub$workDayUpAP)

addhealth_public4_sub$workDayDownAP <-
  factor(addhealth_public4_sub$workDayDownAP
  , levels = c( 1
              , 2
              )
  , labels = c( "AM"
              , "PM"
              )
  )
# check ordering with a frequency table
table(addhealth_public4_sub$workDayDownAP)


## ------------------------------------------------------------------------
library(dplyr)

addhealth_public4_sub <-
  addhealth_public4_sub %>%
  mutate(
    # name the hour, minute, and am_pm variables
   workDayUp = as.numeric(workDayUp)
  , workDayUpMin = as.numeric(workDayUpMin)
  #, workDayUpAP = as.numeric(workDayUpAP)
  )


## ------------------------------------------------------------------------
addhealth_public4_sub <-
  addhealth_public4_sub %>%
  mutate(
    # convert hour to 24-hour time
   wake_hour24_1 = case_when(
                    # hour is 1-11 and AM, then hour is 1-11
                    ((workDayUp %in% 1:11) & (workDayUpAP == "AM")) ~ workDayUp 
                    # hour is 1-11 and PM, then hour adds 12 for 13-23
                  , ((workDayUp %in% 1:11) & (workDayUpAP == "PM")) ~ workDayUp + 12 
                    # hour is 12 and AM, then hour is 0 (midnight)
                  , ((workDayUp == 12) & (workDayUpAP == "AM")) ~ 0
                    # hour is 12 and PM, then hour is 12 (noon)
                  , ((workDayUp == 12) & (workDayUpAP == "PM")) ~ 12
                )
  )



## ------------------------------------------------------------------------
addhealth_public4_sub <-
  addhealth_public4_sub %>%
  mutate(
    # convert minutes from 0-59 to fractions of an hour
    wake_min_frac_1 = workDayUpMin / 60
    # add the 24-hour hour and the fraction minute to get a value from 0 to 23.99
  , wake_time_Workdayup = wake_hour24_1 + wake_min_frac_1
  )

# Here's an example of the new variables
# This looks correct :)
addhealth_public4_sub %>%
  select(workDayUp, workDayUpMin, workDayUpAP, wake_time_Workdayup) %>%
  na.omit() %>%
  arrange(wake_time_Workdayup) %>%
  slice(seq(1, n(), length = 10)  )


## ------------------------------------------------------------------------
library(dplyr)

addhealth_public4_sub <-
  addhealth_public4_sub %>%
  mutate(
    # name the hour, minute, and am_pm variables
   workDayDown = as.numeric(workDayDown)
  , workDayDownMin = as.numeric(workDayDownMin)
  #, workDayUpAP = as.numeric(workDayUpAP)
  )


## ------------------------------------------------------------------------
addhealth_public4_sub <-
  addhealth_public4_sub %>%
  mutate(
    # convert hour to 24-hour time
   wake_hour24_2 = case_when(
                    # hour is 1-11 and AM, then hour is 1-11
                    ((workDayDown %in% 1:11) & (workDayDownAP == "AM")) ~ workDayDown
                    # hour is 1-11 and PM, then hour adds 12 for 13-23
                  , ((workDayDown %in% 1:11) & (workDayDownAP == "PM")) ~ workDayDown + 12 
                    # hour is 12 and AM, then hour is 0 (midnight)
                  , ((workDayDown == 12) & (workDayDownAP == "AM")) ~ 0
                    # hour is 12 and PM, then hour is 12 (noon)
                  , ((workDayDown == 12) & (workDayDownAP == "PM")) ~ 12
                )
  )



## ------------------------------------------------------------------------
addhealth_public4_sub <-
  addhealth_public4_sub %>%
  mutate(
    # convert minutes from 0-59 to fractions of an hour
    wake_min_frac_2 = workDayUpMin / 60
    # add the 24-hour hour and the fraction minute to get a value from 0 to 23.99
  , wake_time_Workdaydown = wake_hour24_2 + wake_min_frac_2
  )
# Here's an example of the new variables
# This looks correct :)
#addhealth_public4_sub$wake_time_Workdaydown[addhealth_public4_sub$wake_time_Workdaydown >= 0 & addhealth_public4_sub$wake_time_Workdaydown <= 1 ] <- 24
table(addhealth_public4_sub$wake_time_Workdaydown)
addhealth_public4_sub %>%
  select(workDayDown, workDayDownMin, workDayDownAP, wake_time_Workdaydown) %>%
  na.omit() %>%
  arrange(wake_time_Workdaydown) %>%
  slice(seq(1, n(), length = 10)  )



## ------------------------------------------------------------------------
addhealth_public4_sub$rr <- (24 - (addhealth_public4_sub$wake_time_Workdaydown)) 
addhealth_public4_sub$rr2 <- (addhealth_public4_sub$rr + addhealth_public4_sub$wake_time_Workdayup)
addhealth_public4_sub$rr3 <- (addhealth_public4_sub$wake_time_Workdaydown - addhealth_public4_sub$wake_time_Workdayup)
addhealth_public4_sub$rr4 <- (addhealth_public4_sub$wake_time_Workdayup - addhealth_public4_sub$wake_time_Workdaydown)
addhealth_public4_sub <-
  addhealth_public4_sub %>%
  mutate(
    # convert hour to 24-hour time
   wake_sleepL = case_when(
                    # hour is 1-11 and AM, then hour is 1-11
                    ((wake_time_Workdaydown >= 12)) ~ addhealth_public4_sub$rr2
                    # hour is 1-11 and PM, then hour adds 12 for 13-23
                  , ((wake_time_Workdaydown < 12 )) ~ abs(addhealth_public4_sub$rr3)
                    # hour is 12 and AM, then hour is 0 (midnight)
                  , ((workDayDown == 0)) ~ addhealth_public4_sub$rr4
                    
                )
  )



## ------------------------------------------------------------------------
addhealth_public4_sub %>%
  select(wake_time_Workdayup, wake_time_Workdaydown, wake_sleepL) %>%
  na.omit() %>%
  arrange(wake_time_Workdaydown) %>%
  slice(seq(1, n(), length = 10)  )

## ------------------------------------------------------------------------
library(car)
aov_summary <- aov(wake_sleepL ~ worry, data = addhealth_public4_sub)


## ------------------------------------------------------------------------
# Plot the data using ggplot
df_res <- data.frame(res = aov_summary$residuals)
library(ggplot2)
p <- ggplot(df_res, aes(x = res))
p <- p + geom_histogram(aes(y = ..density..), binwidth = 4)
p <- p + geom_density(colour = "blue")
p <- p + geom_rug()
p <- p + stat_function(fun = dnorm, colour = "red", args = list(mean = mean(df_res$res), sd = sd(df_res$res)))
p <- p + labs(title = "ANOVA Residuals")
print(p)


## ------------------------------------------------------------------------
# QQ plot
par(mfrow=c(1,1))
library(car)
qqPlot(aov_summary$residuals, las = 1, id = list(n = 0, cex = 1), lwd = 1, main="QQ Plot")



## ------------------------------------------------------------------------
library(nortest)

#shapiro.test(aov_summary$residuals)
ad.test(aov_summary$residuals)



## ------------------------------------------------------------------------
fit.bt <- kruskal.test(wake_sleepL ~ worry, data =addhealth_public4_sub)
fit.bt
str(addhealth_public4_sub)

## ------------------------------------------------------------------------
library(ggplot2)
p <- ggplot(addhealth_public4_sub %>% drop_na(wake_sleepL, worry), aes(x = worry, y = wake_sleepL))
# plot a reference line for the global mean (assuming no groups)
p <- p + geom_hline(yintercept = mean(addhealth_public4_sub$wake_sleepL),
                    colour = "black", linetype = "dashed", size = 0.3, alpha = 0.5)
# boxplot, size=.75 to stand out behind CI
p <- p + geom_violin(width = 0.5, alpha = 0.25)
p <- p + geom_boxplot(width = 0.25, alpha = 0.25)
# points for observed data
p <- p + geom_point(position = position_jitter(w = 0.05, h = 0), alpha = 0.2)
# diamond at mean for each group
p <- p + stat_summary(fun.y = mean, geom = "point", shape = 18, size = 4,
                      colour = "red", alpha = 0.8)
# confidence limits based on normal distribution
p <- p + stat_summary(fun.data = "mean_cl_normal", geom = "errorbar",
                      width = .2, colour = "red", alpha = 0.8)
p <- p + labs(title = "Total cigarettes smoked by Ethnicity")
p <- p + ylab("Square-root of Total Cigs Smoked")
print(p)

## ------------------------------------------------------------------------
by(addhealth_public4_sub$wake_sleepL, addhealth_public4_sub$worry, summary)


## ------------------------------------------------------------------------
wilcox.test(wake_sleepL ~ worry, data = addhealth_public4_sub, subset = (worry %in% c("SA", "A")))
wilcox.test(wake_sleepL ~ worry, data = addhealth_public4_sub, subset = (worry %in% c("SA", "NA")))
wilcox.test(wake_sleepL ~ worry, data = addhealth_public4_sub, subset = (worry %in% c("SA", "D")))
wilcox.test(wake_sleepL ~ worry, data = addhealth_public4_sub, subset = (worry %in% c("SA", "SD")))
wilcox.test(wake_sleepL ~ worry, data = addhealth_public4_sub, subset = (worry %in% c("A", "NA")))
wilcox.test(wake_sleepL ~ worry, data = addhealth_public4_sub, subset = (worry %in% c("A", "D")))
wilcox.test(wake_sleepL ~ worry, data = addhealth_public4_sub, subset = (worry %in% c("A", "SD")))
wilcox.test(wake_sleepL ~ worry, data = addhealth_public4_sub, subset = (worry %in% c("NA", "D")))
wilcox.test(wake_sleepL ~ worry, data = addhealth_public4_sub, subset = (worry %in% c("NA", "SD")))
wilcox.test(wake_sleepL ~ worry, data = addhealth_public4_sub, subset = (worry %in% c("D", "SD")))


## ------------------------------------------------------------------------
library(Hmisc)
library(datasets)
library(tidyverse)
# Tabulate by two categorical variables:
tab_Anxityworry <- xtabs(~ worry + anxity, data = addhealth_public4_sub)
tab_Anxityworry
# column proportions
prop.table(tab_Anxityworry, margin = 2)


## ------------------------------------------------------------------------
chisq_gg <- chisq.test(tab_Anxityworry, correct=FALSE)
chisq_gg
  # names(chisq_gg) for the objects to report


## ------------------------------------------------------------------------
# The Pearson residuals
chisq_gg$residuals
# The sum of the squared residuals is the chi-squared statistic:
chisq_gg$residuals^2
sum(chisq_gg$residuals^2)


## ------------------------------------------------------------------------
# mosaic plot
library(vcd)
#mosaic(tab_GoalsGrade, shade=TRUE, legend=TRUE)
# this layout gives us the interpretation we want:
mosaic(~ anxity + worry, data = addhealth_public4_sub, shade=TRUE, legend=TRUE, direction = "v")

