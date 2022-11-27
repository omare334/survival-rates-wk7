#importing csv 
s_rates<-read.csv("example_surv.csv")
#packages
library(survival)
library(dplyr)
library(purrr)
library(Hmisc)
library(survminer)
library(lubridate)
library(gtsummary)
library(tidycmprsk)
library(condSURV)
#adding age thorugh out follow up 
s_rates_age<-s_rates %>%
  mutate(totaltime = s_rates$age_endfup - s_rates$age_enter)
s_3<-s_rates %>%
  mutate(time = s_rates$age_endfup - s_rates$age_enter)
#calculating survival time 
sur<-Surv(as.numeric(s_rates_age$totaltime, s_rates_age$status))[1:10]
s1 <- survfit(sur ~ 1, data = s_rates_age)
s1_status <- survfit(sur ~ status, data = s_rates_age)
str(s1)
#plotting keplan meir plot
survfit2(sur ~ 1, data = s_rates_age) %>% 
  ggsurvfit() +
  labs(
    x = "Years",
    y = "Overall survival probability"
  )
#also add confidence interval and risk table 
survfit2(sur ~ 1, data = s_rates_age) %>% 
  ggsurvfit() +
  labs(
    x = "Days",
    y = "Overall survival probability"
  ) + 
  add_confidence_interval() +
  add_risktable()
#estimating survival per year 
summary(survfit(Surv(s_rates_age$totaltime, s_rates_age$status) ~ 1, data = s_rates_age), times = 4)


# calculating cumilative hazard function  ---------------------------------
ggsurvplot(s1,
           conf.int = TRUE,
           risk.table.col = "status", # Change risk table color by groups
           ggtheme = theme_bw(), # Change ggplot2 theme
           palette = c("#E7B800", "#2E9FDF"),
           fun = "cumhaz")#plots cumilative hazard for s1
ggsurvplot(s1_status,
           conf.int = TRUE,
           risk.table.col = "status", # Change risk table color by groups
           ggtheme = theme_bw(), # Change ggplot2 theme
           palette = c("#E7B800", "#2E9FDF"),
           fun = "cumhaz")#plots cumilative hazard for s1

# repeating calculation using age as time scale ---------------------------
#pulling age from dates
date_ex <-
  s_rates %>% 
  mutate(
    d_enter = ymd(d_enter), 
    d_endfup = ymd(d_endfup)
  )
date_ex <-
  date_ex %>% 
  mutate(
    os_yrs = as.duration(d_enter %--% d_endfup) / dyears(1)
  )

date_ex
#calculating survival factor 
s2<-Surv(as.numeric(date_ex$os_yrs, date_ex$status))
s4 <- survfit(s2 ~ 1, data = s_rates_age)

survfit2(s2 ~ 1, data = lung) %>% 
  ggsurvfit() +
  labs(
    x = "Days",
    y = "Overall survival probability"
  )
#table of survivial factor 
summary(s4)

