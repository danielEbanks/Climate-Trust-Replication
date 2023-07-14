
## Libraries

library(haven)
library(survey)
library(tidyverse)
library(stargazer)
library(jtools)
library(ggplot2)
library(margins)


## Load Data
national.design.climate.may2022 <- readRDS("national.design.climate.may2022.Rds")




# Run Models  ##########################

# Trust Logit
logit1a <- svyglm(formula = clim_impt ~ univ_trust + race_r + gender + educ_r + age_r + region_r + relig_d + relig_a + environment_info + pol_info + pid  + ideo_r + tv_news + radio_news + print_news + online_news, family=quasibinomial,design=national.design.climate.may2022)
logit1a_m <- margins(logit1a,design=national.design.climate.may2022)

# Trust Logit 2
logit2 <- svyglm(formula = clim_humans ~ univ_trust + race_r + gender + educ_r + age_r + region_r + relig_d + relig_a + environment_info + pol_info + pid  + ideo_r + tv_news + radio_news + print_news + online_news, family=quasibinomial, national.design.climate.may2022)
logit2_m <-margins(logit2,design=national.design.climate.may2022)


# Trust OLS regression
reg5 <- svyglm(formula = Q41_university ~ race_r + gender + educ_r + age_r + region_r + relig_d + relig_a + environment_info + pol_info + pid  + ideo_r + tv_news + radio_news + print_news + online_news, national.design.climate.may2022)


# Ordered Logit
orderedlogit1a <- svyolr(formula = clim_impt_4 ~ univ_trust + race_r + gender + educ_r + age_r + region_r + relig_d + relig_a + environment_info + pol_info + pid  + ideo_r + tv_news + radio_news + print_news + online_news,design=national.design.climate.may2022)

#LPM
LinProb <- svyglm(formula = clim_humans ~ univ_trust + race_r + gender + educ_r + age_r + region_r + relig_d + relig_a + environment_info + pol_info + pid  + ideo_r + tv_news + radio_news + print_news + online_news, family=gaussian,design=national.design.climate.may2022)



## Main Paper Figures ####################

#Fig 2

print("Create Fig 2")
pdf("trust-logit1a-ame.pdf")
plot_summs(logit1a_m, coefs=c("Trust: Low" = "univ_trustLow Trust", "Trust: Moderate" = "univ_trustMod Trust", "Race: Black" = "race_rBlack", "Race: Hisp/Latino" = "race_rHispanic", "Race: Other" = "race_rOther", "Gender: Woman" = "genderWoman", "Ed: College" = "educ_rCollGrad", "Ed: PostGrad" = "educ_rPostGrad", "Ed: SomeCall" = "educ_rSomeColl", "Age: 30-44" = "age_rb_30t44", "Age: 45-64" = "age_rc_45t64", "Age: 65+" = "age_rd_65Older", "Midwest" = "region_rMidwest", "Northeast" = "region_rNortheast", "West" = "region_rWest", "Rel: Catholic" = "relig_dCatholic", "Rel: Jewish" = "relig_dJewish", "Rel: None" = "relig_dNone", "Rel: Other" = "relig_dOther", "Attend: Frequent" = "relig_aFrequently", "Attend: Often" = "relig_aOften", "Attend: Sometimes" = "relig_aSometimes", "Environment Info." = "environment_info", "Political Info." = "pol_info", "Democrat" = "pidb_Democrat", "Independ." = "pidc_Independent", "Moderate" = "ideo_rb_Moderate", "Liberal" = "ideo_rc_Liberal", "News: TV" = "tv_news", "News: Radio" = "radio_news", "News: Print" = "print_news", "News: Online" = "online_news"), scale = TRUE) + xlim(-.5, .5)
dev.off()

# Fig 3


print("Create Fig 3")
pdf("trust-logit2-ame.pdf")
plot_summs(logit2_m, coefs=c("Trust: Low" = "univ_trustLow Trust", "Trust: Moderate" = "univ_trustMod Trust", "Race: Black" = "race_rBlack", "Race: Hisp/Latino" = "race_rHispanic", "Race: Other" = "race_rOther", "Gender: Woman" = "genderWoman", "Ed: College" = "educ_rCollGrad", "Ed: PostGrad" = "educ_rPostGrad", "Ed: SomeCall" = "educ_rSomeColl", "Age: 30-44" = "age_rb_30t44", "Age: 45-64" = "age_rc_45t64", "Age: 65+" = "age_rd_65Older", "Midwest" = "region_rMidwest", "Northeast" = "region_rNortheast", "West" = "region_rWest", "Rel: Catholic" = "relig_dCatholic", "Rel: Jewish" = "relig_dJewish", "Rel: None" = "relig_dNone", "Rel: Other" = "relig_dOther", "Attend: Frequent" = "relig_aFrequently", "Attend: Often" = "relig_aOften", "Attend: Sometimes" = "relig_aSometimes", "Environment Info." = "environment_info", "Political Info." = "pol_info", "Democrat" = "pidb_Democrat", "Independ." = "pidc_Independent", "Moderate" = "ideo_rb_Moderate", "Liberal" = "ideo_rc_Liberal", "News: TV" = "tv_news", "News: Radio" = "radio_news", "News: Print" = "print_news", "News: Online" = "online_news"), scale = TRUE) + xlim(-.5, .5)
dev.off()


#Fig 4

print("Create Fig 4")
pdf("trust-regression.pdf")
plot_summs(reg5, coefs=c("Race: Black" = "race_rBlack", "Race: Hisp/Latino" = "race_rHispanic", "Race: Other" = "race_rOther", "Gender: Woman" = "genderWoman", "Ed: College" = "educ_rCollGrad", "Ed: PostGrad" = "educ_rPostGrad", "Ed: SomeCall" = "educ_rSomeColl", "Age: 30-44" = "age_rb_30t44", "Age: 45-64" = "age_rc_45t64", "Age: 65+" = "age_rd_65Older", "Midwest" = "region_rMidwest", "Northeast" = "region_rNortheast", "West" = "region_rWest", "Rel: Catholic" = "relig_dCatholic", "Rel: Jewish" = "relig_dJewish", "Rel: None" = "relig_dNone", "Rel: Other" = "relig_dOther", "Attend: Frequent" = "relig_aFrequently", "Attend: Often" = "relig_aOften", "Attend: Sometimes" = "relig_aSometimes", "Environment Info." = "environment_info", "Political Info." = "pol_info", "Democrat" = "pidb_Democrat", "Independ." = "pidc_Independent", "Moderate" = "ideo_rb_Moderate", "Liberal" = "ideo_rc_Liberal", "News: TV" = "tv_news", "News: Radio" = "radio_news", "News: Print" = "print_news", "News: Online" = "online_news"), scale = TRUE) + xlim(-2.5, 2.5)
dev.off()




# Supporting Information Figures  ###########################################


# Section D
print("Fig SI 1")
logit1 <- svyglm(formula = clim_impt ~ univ_trust + race_r + gender + educ_r + age_r + region_r + relig_d + relig_a + environment_info + pol_info + pid  + ideo_r + tv_news + radio_news + print_news + online_news, family=quasibinomial, national.design.climate.may2022)
pdf("trust-logit1-coef.pdf")
plot_summs(logit1, coefs=c("Trust: Low" = "univ_trustLow Trust", "Trust: Moderate" = "univ_trustMod Trust", "Race: Black" = "race_rBlack", "Race: Hisp/Latino" = "race_rHispanic", "Race: Other" = "race_rOther", "Gender: Woman" = "genderWoman", "Ed: College" = "educ_rCollGrad", "Ed: PostGrad" = "educ_rPostGrad", "Ed: SomeCall" = "educ_rSomeColl", "Age: 30-44" = "age_rb_30t44", "Age: 45-64" = "age_rc_45t64", "Age: 65+" = "age_rd_65Older", "Midwest" = "region_rMidwest", "Northeast" = "region_rNortheast", "West" = "region_rWest", "Rel: Catholic" = "relig_dCatholic", "Rel: Jewish" = "relig_dJewish", "Rel: None" = "relig_dNone", "Rel: Other" = "relig_dOther", "Attend: Frequent" = "relig_aFrequently", "Attend: Often" = "relig_aOften", "Attend: Sometimes" = "relig_aSometimes", "Environment Info." = "environment_info", "Political Info." = "pol_info", "Democrat" = "pidb_Democrat", "Independ." = "pidc_Independent", "Moderate" = "ideo_rb_Moderate", "Liberal" = "ideo_rc_Liberal", "News: TV" = "tv_news", "News: Radio" = "radio_news", "News: Print" = "print_news", "News: Online" = "online_news"), scale = TRUE) + xlim(-4, 4)
dev.off()


print("Fig SI 2")
pdf("trust-logit2-coef.pdf")
plot_summs(logit2, coefs=c("Trust: Low" = "univ_trustLow Trust", "Trust: Moderate" = "univ_trustMod Trust", "Race: Black" = "race_rBlack", "Race: Hisp/Latino" = "race_rHispanic", "Race: Other" = "race_rOther", "Gender: Woman" = "genderWoman", "Ed: College" = "educ_rCollGrad", "Ed: PostGrad" = "educ_rPostGrad", "Ed: SomeCall" = "educ_rSomeColl", "Age: 30-44" = "age_rb_30t44", "Age: 45-64" = "age_rc_45t64", "Age: 65+" = "age_rd_65Older", "Midwest" = "region_rMidwest", "Northeast" = "region_rNortheast", "West" = "region_rWest", "Rel: Catholic" = "relig_dCatholic", "Rel: Jewish" = "relig_dJewish", "Rel: None" = "relig_dNone", "Rel: Other" = "relig_dOther", "Attend: Frequent" = "relig_aFrequently", "Attend: Often" = "relig_aOften", "Attend: Sometimes" = "relig_aSometimes", "Environment Info." = "environment_info", "Political Info." = "pol_info", "Democrat" = "pidb_Democrat", "Independ." = "pidc_Independent", "Moderate" = "ideo_rb_Moderate", "Liberal" = "ideo_rc_Liberal", "News: TV" = "tv_news", "News: Radio" = "radio_news", "News: Print" = "print_news", "News: Online" = "online_news"), scale = TRUE) + xlim(-4, 4)
dev.off()

# Section F

print("Fig SI 3")
LinProb1a <- svyglm(formula = clim_impt ~ univ_trust + race_r + gender + educ_r + age_r + region_r + relig_d + relig_a + environment_info + pol_info + pid  + ideo_r + tv_news + radio_news + print_news + online_news, family=gaussian,design=national.design.climate.may2022)
pdf("trust-linprob1a-ame.pdf")
plot_summs(LinProb1a, coefs=c("Trust: Low" = "univ_trustLow Trust", "Trust: Moderate" = "univ_trustMod Trust", "Race: Black" = "race_rBlack", "Race: Hisp/Latino" = "race_rHispanic", "Race: Other" = "race_rOther", "Gender: Woman" = "genderWoman", "Ed: College" = "educ_rCollGrad", "Ed: PostGrad" = "educ_rPostGrad", "Ed: SomeCall" = "educ_rSomeColl", "Age: 30-44" = "age_rb_30t44", "Age: 45-64" = "age_rc_45t64", "Age: 65+" = "age_rd_65Older", "Midwest" = "region_rMidwest", "Northeast" = "region_rNortheast", "West" = "region_rWest", "Rel: Catholic" = "relig_dCatholic", "Rel: Jewish" = "relig_dJewish", "Rel: None" = "relig_dNone", "Rel: Other" = "relig_dOther", "Attend: Frequent" = "relig_aFrequently", "Attend: Often" = "relig_aOften", "Attend: Sometimes" = "relig_aSometimes", "Environment Info." = "environment_info", "Political Info." = "pol_info", "Democrat" = "pidb_Democrat", "Independ." = "pidc_Independent", "Moderate" = "ideo_rb_Moderate", "Liberal" = "ideo_rc_Liberal", "News: TV" = "tv_news", "News: Radio" = "radio_news", "News: Print" = "print_news", "News: Online" = "online_news"), scale = TRUE) + xlim(-.5, .5)
dev.off()

print("Fig SI 4")
pdf("trust-ordededlogit1a-coef.pdf")
plot_summs(orderedlogit1a, coefs=c("Trust: Low" = "univ_trustLow Trust", "Trust: Moderate" = "univ_trustMod Trust", "Race: Black" = "race_rBlack", 
                                   "Race: Hisp/Latino" = "race_rHispanic", "Race: Other" = "race_rOther", "Gender: Woman" = "genderWoman", "Ed: College" = "educ_rCollGrad", 
                                   "Ed: PostGrad" = "educ_rPostGrad", "Ed: SomeCall" = "educ_rSomeColl", "Age: 30-44" = "age_rb_30t44", "Age: 45-64" = "age_rc_45t64", 
                                   "Age: 65+" = "age_rd_65Older", "Midwest" = "region_rMidwest", "Northeast" = "region_rNortheast", "West" = "region_rWest", "Rel: Catholic" = "relig_dCatholic", 
                                   "Rel: Jewish" = "relig_dJewish", "Rel: None" = "relig_dNone", "Rel: Other" = "relig_dOther", "Attend: Frequent" = "relig_aFrequently", "Attend: Often" = "relig_aOften", 
                                   "Attend: Sometimes" = "relig_aSometimes", "Environment Info." = "environment_info", "Political Info." = "pol_info", "Democrat" = "pidb_Democrat", "Independ." = "pidc_Independent", 
                                   "Moderate" = "ideo_rb_Moderate", "Liberal" = "ideo_rc_Liberal", "News: TV" = "tv_news", "News: Radio" = "radio_news", "News: Print" = "print_news", "News: Online" = "online_news"), scale = TRUE)# + 
dev.off()


print("Fig SI 5")
pdf("trust-linprob2-ame.pdf")
plot_summs(LinProb2a, coefs=c("Trust: Low" = "univ_trustLow Trust", "Trust: Moderate" = "univ_trustMod Trust", "Race: Black" = "race_rBlack", "Race: Hisp/Latino" = "race_rHispanic", "Race: Other" = "race_rOther", "Gender: Woman" = "genderWoman", "Ed: College" = "educ_rCollGrad", "Ed: PostGrad" = "educ_rPostGrad", "Ed: SomeCall" = "educ_rSomeColl", "Age: 30-44" = "age_rb_30t44", "Age: 45-64" = "age_rc_45t64", "Age: 65+" = "age_rd_65Older", "Midwest" = "region_rMidwest", "Northeast" = "region_rNortheast", "West" = "region_rWest", "Rel: Catholic" = "relig_dCatholic", "Rel: Jewish" = "relig_dJewish", "Rel: None" = "relig_dNone", "Rel: Other" = "relig_dOther", "Attend: Frequent" = "relig_aFrequently", "Attend: Often" = "relig_aOften", "Attend: Sometimes" = "relig_aSometimes", "Environment Info." = "environment_info", "Political Info." = "pol_info", "Democrat" = "pidb_Democrat", "Independ." = "pidc_Independent", "Moderate" = "ideo_rb_Moderate", "Liberal" = "ideo_rc_Liberal", "News: TV" = "tv_news", "News: Radio" = "radio_news", "News: Print" = "print_news", "News: Online" = "online_news"), scale = TRUE) + xlim(-.5, .5)
dev.off()


## SI Table descriptive statistics ###########################################

t1 <- round(prop.table(svytable(~Q41_university, national.design.climate.may2022)),3)
t2 <- round(prop.table(svytable(~clim_impt, national.design.climate.may2022)),3)
t3 <- round(prop.table(svytable(~clim_humans, national.design.climate.may2022)),3)
t4 <- round(prop.table(svytable(~race_r, national.design.climate.may2022)),3)
t5 <- round(prop.table(svytable(~gender, national.design.climate.may2022)),3)
t6 <- round(prop.table(svytable(~educ_r, national.design.climate.may2022)),3)
t7 <- round(prop.table(svytable(~age_r, national.design.climate.may2022)),3)
t8 <- round(prop.table(svytable(~region_r, national.design.climate.may2022)),3)
t9 <- round(prop.table(svytable(~relig_d, national.design.climate.may2022)),3)
t10 <- round(prop.table(svytable(~relig_a, national.design.climate.may2022)),3)
t11 <- round(prop.table(svytable(~environment_info, national.design.climate.may2022)),3)
t12 <- round(prop.table(svytable(~pol_info, national.design.climate.may2022)),3)
t13 <- round(prop.table(svytable(~pid, national.design.climate.may2022)),3)
t14 <- round(prop.table(svytable(~ideo_r, national.design.climate.may2022)),3)
t15 <- round(prop.table(svytable(~tv_news, national.design.climate.may2022)),3)
t16 <- round(prop.table(svytable(~radio_news, national.design.climate.may2022)),3)
t17 <- round(prop.table(svytable(~print_news, national.design.climate.may2022)),3)
t18 <- round(prop.table(svytable(~online_news, national.design.climate.may2022)),3)


