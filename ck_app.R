## ---------------------------
##
## Script name: ck_app
##
## Purpose of script: Reproduction of Section 2 of the Term Paper
##
## Author: Norman Metzinger 
##
## Date Created: 10.02.2023
##
##
## ---------------------------
##
## Notes:
## With all libraries installed the code should run smoothly.   
## Every tables content and every figure of Section 2 can be
## reproduced with this code.
## ---------------------------


rm(list = ls())

library(ggplot2)
library(caret)
library(dplyr) 
library(ggpubr)
library(tidyr)
library(readr)
library(haven)
library(texreg)
library(xtable)
library(here)


#importing data set
here("Section_2", "CK1994.dta")
CK1994 <- read_dta("CK1994.dta")


theme_set(theme_bw()) #white background of pics



#Appendix A.2--------------
#descriptive statistics -----------
store_stat <- CK1994 %>%
  select(chain, state) %>%
  table() %>%
  prop.table(margin = 2)  %>%
  apply(MARGIN = 2,
        FUN = scales::percent_format(accuracy = 0.1)) 
xtable(store_stat, caption ="Distribution of store types")



#Histogram----------------------
#Figure 1
CKgraph <-  CK1994

CKgraph$state[CKgraph$state == 0] <- "Pennsylvania (control)"
CKgraph$state[CKgraph$state == 1] <- "New Jersey (treatment)"


hist.1 <- CKgraph %>%
  filter(time == "0") %>%
  ggplot(aes(x = wage_st, fill = state, group = state)) +
  geom_histogram(aes(y=c(..count..[..group..==1]/sum(..count..[..group..==1]),
                         ..count..[..group..==2]/sum(..count..[..group..==2]))*100),position = "dodge", alpha = 0.6, bins =25) +  #aes(y = after_stat(count / sum(count))),
  labs(title = "February 1992", x = "Starting Wage range", y = "Percent of stores", fill = "")
hist.2 <- CKgraph %>%
  filter(time == "1") %>%
  ggplot(aes(x = wage_st, fill = state, group = state)) +
  geom_histogram(aes(y=c(..count..[..group..==1]/sum(..count..[..group..==1]),
                         ..count..[..group..==2]/sum(..count..[..group..==2]))*100),position = "dodge", alpha = 0.6, bins =25) +  #aes(y = after_stat(count / sum(count))),
  labs(title = "November 1992", x = "Starting Wage range", y = "Percent of stores", fill = "")


ggarrange(hist.1, hist.2, ncol = 2, 
          common.legend = TRUE, legend = "bottom")



#Tabe 1---------
#produce a 2x2 table of treatment and control and pre and post treatment
CK1994_table2 <- CK1994
CK1994_table2<- CK1994_table2 %>% mutate(fte_before_and_after =empft+nmgrs+(0.5*emppt) ) #FTE is inidpendent of time here, must changhe probably

Table2_repr <- CK1994_table2  %>% #Table 2 done
  group_by(time, state)  %>%
  summarize(fte_mean = mean(fte_before_and_after,na.rm=T))
before_treatment_NJ <- Table2_repr  %>% 
  filter(time ==0, state ==1)   %>% 
  pull(fte_mean)
after_treatment_NJ <- Table2_repr  %>% 
  filter(time ==1, state ==1)   %>% 
  pull(fte_mean)

before_control_PY <- Table2_repr  %>% 
  filter(time ==0, state ==0)   %>% 
  pull(fte_mean)
after_control_PY <- Table2_repr  %>% 
  filter(time ==1, state ==0)   %>% 
  pull(fte_mean)
treatment_dif <- after_treatment_NJ - before_treatment_NJ
control_dif <- after_control_PY - before_control_PY
time_0_dif <-  before_control_PY -before_treatment_NJ 
time_1_dif <-  after_control_PY - after_treatment_NJ
overall_difference <- treatment_dif - control_dif

#Appendix A.1-----------
ggplot(data = Table2_repr, aes(x= time ,y= fte_mean,
                               color = state)) +
  geom_point(show.legend = FALSE) +
  geom_line(aes(group = state),show.legend = FALSE, lwd =1.5) +
  annotate(geom = "segment", x=0, xend = 1,
           y = before_treatment_NJ, yend =after_treatment_NJ- overall_difference,
           linetype = 5, lwd =1.5)  +                #annotate produce the nonexisting counterfactual
  annotate(geom = "segment", x=1, xend = 1,
           y = after_treatment_NJ - overall_difference,
           yend =after_treatment_NJ, col = "red", linetype = 4, lwd =1.5) + #green line is causal effect
  labs(
       x = "Time periods", y = "FTE mean", fill = "") 





#Regression Section 2.3 Application---------------
#Table 2
library(reshape2)
library(estimatr)

keep <- c("empft", "nmgrs", "emppt", "wage_st", "state",
          "time", "ncalls","store", "co_owned", "southj", 
          "centralj", "northj", "pa1","pa2",
          "shore", "store","chain","co_owned")
ck_wide <- CK1994[keep]
ck_wide <- dcast(melt(ck_wide,id.vars=c("store","time")), store~variable+time) 


ck_wide <- ck_wide[ , !names(ck_wide) %in% 
                      c("co_owned_1","stat_1","southj_1",
                        "centralj_1","northj_1","pa1_1",
                        "pa2_1","shore_1","chain_1","state_1")]


ck_wide<- ck_wide %>% 
  rename(
    state = state_0,
    chain = chain_0
  )

#create region variable
region_input <- subset(ck_wide, select = c("southj_0","centralj_0","northj_0","pa1_0","pa2_0","store"))
region_input <- cbind(region_input, regions=NA)
region_input <- region_input %>% mutate(regions = case_when(
  southj_0 == "1"  ~ "1",
  centralj_0 == "1"  ~ "2",
  northj_0== "1"  ~ "3",
  pa1_0== "1"  ~ "4",
  pa2_0== "1"  ~ "5",
  TRUE ~ "NO"
))
ck_wide<- merge(region_input,ck_wide)






ck_wide <- ck_wide %>% group_by(state) %>% mutate(fte=empft_0+nmgrs_0+(0.5*emppt_0),fte_after=empft_1+nmgrs_1+(0.5*emppt_1))

est.ck_wide <- ck_wide %>% ungroup() %>% filter(is.na(fte)==F & is.na(fte_after)==F & is.na(wage_st_0)==F & is.na(wage_st_1)==F) %>%
  mutate(delta_emp=fte_after-fte,
         gap=ifelse(state==1 & wage_st_0<= 5.05,((5.05-wage_st_0)/wage_st_0),0)) %>%
  mutate(chain1=ifelse(chain==1,1,0),
         chain2=ifelse(chain==2,1,0),
         chain3=ifelse(chain==3,1,0),
         chain4=ifelse(chain==4,1,0))

est.ck_wide <- ck_wide %>% ungroup() %>% filter(is.na(fte)==F & is.na(fte_after)==F & is.na(wage_st_0)==F & is.na(wage_st_1)==F) %>%
  mutate(delta_emp=fte_after-fte,
         gap=ifelse(state==1 & wage_st_0<= 5.05,((5.05-wage_st_0)/wage_st_0),0)) %>%
  mutate(chain1=ifelse(chain==1,1,0),
         chain2=ifelse(chain==2,1,0),
         chain3=ifelse(chain==3,1,0),
         chain4=ifelse(chain==4,1,0))

model1 <- lm(delta_emp~state, data=est.ck_wide)
model2 <- lm(delta_emp~state+co_owned_0+chain2+chain3+chain4,data=est.ck_wide)

texreg(list(model1,model2), include.ci = FALSE, stars = c(0.001, 0.01, 0.05,0.1),)


#homogenous treatment effects Section 2.4-----------
#table 3



fte_mean_before<- aggregate(est.ck_wide$fte, list(est.ck_wide$regions), mean)
fte_mean_after<- aggregate(est.ck_wide$fte_after, list(est.ck_wide$regions), mean)

fte_mean_before_r <- round(fte_mean_before$x, 2)
fte_mean_after_r <- round(fte_mean_after$x, 2)
differenc <-  fte_mean_after_r - fte_mean_before_r


#clustering 2.5-----------
#Table 4
library(lmtest)



lmrobust_store <- lm_robust(delta_emp ~ state,
                                      se_type = "stata",
                                      clusters = store,
                                      data=est.ck_wide)#clarify whether homo or heterosekedastic errors


lmrobust_state <- lm_robust(delta_emp ~ state,
                            se_type = "stata",
                            clusters = state,
                            data=est.ck_wide)#clarify whether homo or heterosekedastic errors

lmrobust_regions <- lm_robust(delta_emp ~ state,
                       se_type = "stata",
                       clusters = regions,
                       data=est.ck_wide)#clarify whether homo or heterosekedastic errors




texreg(list(lmrobust_store,lmrobust_regions,lmrobust_state), include.ci = FALSE, stars = c(0.001, 0.01, 0.05,0.1),)
