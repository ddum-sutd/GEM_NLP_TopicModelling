library(tidyverse)
library(broom)

cat("\014") 
rm(list = ls())
graphics.off()

df <- read.csv("C:\\Users\\franc\\Dropbox\\Francisco\\Papers_SUTD\\Circular_Economy\\Survey_Paper\\2-Data\\raw_df_topic_modelling_valid_indicator.csv", header  = T) %>% 
  select(weighted_sum, is_valid_for_Topic_modelling, country, region, yrsurv, age, gender, GEMEDUC, GEMHHINC, GEMWORK,
         opport, fearfail, suskill, nbgoodc, nbstatus, nbmedia, knowent) %>% 
  na.omit()%>% 
  filter(gender>0,GEMEDUC>0, GEMHHINC>0, GEMWORK>0, opport>0, fearfail>0, suskill>0,
         nbgoodc>0, nbstatus>0, nbmedia>0, knowent>0, GEMWORK!='Other')

df <- df %>% mutate(age_group = case_when( 
  age< 18 ~ '<18',
  age>17 & age<25 ~ '18-24',
  age>24 & age<35 ~ '25-34',
  age>34 & age<45 ~ '35-44',
  age>44 & age<55 ~ '45-54',
  age>54 & age<65 ~ '55-64',
  age> 64 ~ '>64'
)
) %>%
  filter(age_group!='<18', age_group!='>64') %>%
  mutate(age_group=factor(case_when(
    age_group=='25-34' ~ 1,
    age_group=='18-24' ~ 2,
    age_group=='35-44' ~ 3,
    age_group=='45-54' ~ 4,
    age_group=='55-64' ~ 5,
  ), levels = c(1:5), labels = c('25-34','18-24','35-44',
                                 '45-54','55-64') )) %>%
  mutate(yrsurv =factor(yrsurv)) %>%
  mutate(country = factor(country)) %>%
  mutate(GEMEDUC=factor(case_when(
    GEMEDUC=='NONE' ~ 1,
    GEMEDUC=='SOME SECONDARY' ~ 2,
    GEMEDUC=='SECONDARY DEGREE' ~ 3,
    GEMEDUC=='POST SECONDARY' ~ 4,
    GEMEDUC=='GRAD EXP' ~ 5,
  ), levels = c(1:5), labels = c('NONE','SOME SECONDARY','SECONDARY DEGREE',
                                 'POST SECONDARY','GRAD EXP') )) %>%
  mutate(GEMHHINC=factor(case_when(
    GEMHHINC=="Middle 33%tile" ~1,
    GEMHHINC=="Lowest 33%tile" ~2,
    GEMHHINC=="Upper  33%tile" ~3,
  ), levels = c(1:3), labels = c('Middle','Low','High') ))

df$GEMWORK <- ifelse(df$GEMWORK=='Part time only', 
                     'Full: full or part time (includes self-employment)',
                     df$GEMWORK)

table(df$age_group)
str(df$age_group)


df <- df %>% mutate(GEMWORK=factor(case_when(
  GEMWORK=='Full: full or part time (includes self-employment)'~1,
  GEMWORK=='Retired, disabled'~2,
  GEMWORK=='Homemaker'~3,
  GEMWORK=='Student'~4,
  GEMWORK=='Not working'~5,
), levels = c(1:5), labels = c('Full or part time','Retired','Homemaker',
                               'Student','Not working') ))  %>%
  mutate(gender =factor(case_when(
    gender=='Male'~1,
    gender=='Female'~2,
  ), levels = c(1:2), labels = c('Male','Female') ))  %>%
  mutate(opport =factor(case_when(
    opport=='No'~1,
    opport=='Yes'~2,
  ), levels = c(1:2), labels = c('No','Yes') ))  %>%
  mutate(fearfail =factor(case_when(
    fearfail=='No'~1,
    fearfail=='Yes'~2,
  ), levels = c(1:2), labels = c('No','Yes') ))   %>%
  mutate(suskill =factor(case_when(
    suskill=='No'~1,
    suskill=='Yes'~2,
  ), levels = c(1:2), labels = c('No','Yes') ))    %>%
  mutate(nbgoodc =factor(case_when(
    nbgoodc=='No'~1,
    nbgoodc=='Yes'~2,
  ), levels = c(1:2), labels = c('No','Yes') ))     %>%
  mutate(nbstatus =factor(case_when(
    nbstatus=='No'~1,
    nbstatus=='Yes'~2,
  ), levels = c(1:2), labels = c('No','Yes') ))      %>%
  mutate(nbmedia =factor(case_when(
    nbmedia=='No'~1,
    nbmedia=='Yes'~2,
  ), levels = c(1:2), labels = c('No','Yes') ))       %>%
  mutate(knowent =factor(case_when(
    knowent=='No'~1,
    knowent=='Yes'~2,
  ), levels = c(1:2), labels = c('No','Yes') )) 


lm_formula <- weighted_sum~opport+fearfail+suskill+
  nbgoodc+nbstatus+nbmedia+knowent+
  gender+age_group+GEMEDUC+GEMWORK+GEMHHINC+yrsurv+country

log_formula <- is_valid_for_Topic_modelling~opport+fearfail+suskill+
  nbgoodc+nbstatus+nbmedia+knowent+
  gender+age_group+GEMEDUC+GEMWORK+GEMHHINC+yrsurv+country

regions_list = split(df, df$region)

results = lapply(regions_list, function(dat) lm(lm_formula, data = dat))
results_logit = lapply(regions_list, function(dat) glm(log_formula, data = dat,
                                                       family = "binomial"))

setwd("C:\\Users\\franc\\Dropbox\\Francisco\\Papers_SUTD\\Circular_Economy\\Survey_Paper\\4-Results\\Entrepenurs_and_non_entrepeneurs")
write.csv( tidy(results$Africa) , "Africa_coefs.csv" )
write.csv( glance(results$Africa) , "Africa_an.csv" )

write.csv( tidy(results$`Asia & Oceania`) , "Asia_coefs.csv" )
write.csv( glance(results$`Asia & Oceania`) , "Asia_an.csv" )

write.csv( tidy(results$Europe) , "Europe_coefs.csv" )
write.csv( glance(results$Europe) , "Europe_an.csv" )

write.csv( tidy(results$`Latin America & Caribbean` ) , "LA_coefs.csv" )
write.csv( glance(results$`Latin America & Caribbean`) , "LA_an.csv" )

write.csv( tidy(results$`North America` ) , "NA_coefs.csv" )
write.csv( glance(results$`North America`) , "NA_an.csv" )


################ Create table

coef_africa <-  read.csv("Africa_coefs.csv", header  = T) %>% 
  filter(row_number()<24)  %>%
  mutate(p_value=ifelse(p.value<=0.01,"***","")) %>%
  mutate(b_p_val= paste(round(estimate,3),  p_value) ) %>%
  mutate(region="Africa") %>%
  select(region, term, b_p_val) 

coef_asia <-  read.csv("Asia_coefs.csv", header  = T) %>% 
  filter(row_number()<24)  %>%
  mutate(p_value=ifelse(p.value<0.01,"***","")) %>%
  mutate(b_p_val= paste(round(estimate,3),  p_value) ) %>%
  mutate(region="Asia") %>%
  select(region, term, b_p_val) 

coef_europe <-  read.csv("Europe_coefs.csv", header  = T) %>% 
  filter(row_number()<24)  %>%
  mutate(p_value=ifelse(p.value<0.01,"***","")) %>%
  mutate(b_p_val= paste(round(estimate,3),  p_value) ) %>%
  mutate(region="Europe") %>%
  select(region, term, b_p_val) 

coef_na <-  read.csv("NA_coefs.csv", header  = T) %>% 
  filter(row_number()<24)  %>%
  mutate(p_value=ifelse(p.value<0.01,"***","")) %>%
  mutate(b_p_val= paste(round(estimate,3), p_value) ) %>%
  mutate(region="North America") %>%
  select(region, term, b_p_val) 

coef_la<-  read.csv("LA_coefs.csv", header  = T) %>% 
  filter(row_number()<24)  %>%
  mutate(p_value=ifelse(p.value<0.01,"***","")) %>%
  mutate(b_p_val= paste(round(estimate,3),  p_value) ) %>%
  mutate(region="Latin America") %>%
  select(region, term, b_p_val) 


write.csv(coef_africa, "Africa_coefs.csv")
write.csv(coef_asia, "Asia_coefs.csv")
write.csv(coef_europe, "Europe_coefs.csv")
write.csv(coef_na, "NA_coefs.csv")
write.csv(coef_la, "LA_coefs.csv")


########################################
########Logit results
########################################
write.csv( tidy(results_logit$Africa) , "Africa_coefs_logit.csv" )
write.csv( glance(results_logit$Africa) , "Africa_an_logit.csv" )

write.csv( tidy(results_logit$`Asia & Oceania`) , "Asia_coefs_logit.csv" )
write.csv( glance(results_logit$`Asia & Oceania`) , "Asia_an_logit.csv" )

write.csv( tidy(results_logit$Europe) , "Europe_coefs_logit.csv" )
write.csv( glance(results_logit$Europe) , "Europe_an_logit.csv" )

write.csv( tidy(results_logit$`Latin America & Caribbean` ) , "LA_coefs_logit.csv" )
write.csv( glance(results_logit$`Latin America & Caribbean`) , "LA_an_logit.csv" )

write.csv( tidy(results_logit$`North America` ) , "NA_coefs_logit.csv" )
write.csv( glance(results_logit$`North America`) , "NA_an_logit.csv" )


################ Create table

coef_africa <-  read.csv("Africa_coefs_logit.csv", header  = T) %>% 
  filter(row_number()<24)  %>%
  mutate(p_value=ifelse(p.value<=0.01,"***","")) %>%
  mutate(b_p_val= paste(round(estimate,3),  p_value) ) %>%
  mutate(region="Africa") %>%
  select(region, term, b_p_val) 

coef_asia <-  read.csv("Asia_coefs_logit.csv", header  = T) %>% 
  filter(row_number()<24)  %>%
  mutate(p_value=ifelse(p.value<0.01,"***","")) %>%
  mutate(b_p_val= paste(round(estimate,3),  p_value) ) %>%
  mutate(region="Asia") %>%
  select(region, term, b_p_val) 

coef_europe <-  read.csv("Europe_coefs_logit.csv", header  = T) %>% 
  filter(row_number()<24)  %>%
  mutate(p_value=ifelse(p.value<0.01,"***","")) %>%
  mutate(b_p_val= paste(round(estimate,3),  p_value) ) %>%
  mutate(region="Europe") %>%
  select(region, term, b_p_val) 

coef_na <-  read.csv("NA_coefs_logit.csv", header  = T) %>% 
  filter(row_number()<24)  %>%
  mutate(p_value=ifelse(p.value<0.01,"***","")) %>%
  mutate(b_p_val= paste(round(estimate,3), p_value) ) %>%
  mutate(region="North America") %>%
  select(region, term, b_p_val) 

coef_la<-  read.csv("LA_coefs_logit.csv", header  = T) %>% 
  filter(row_number()<24)  %>%
  mutate(p_value=ifelse(p.value<0.01,"***","")) %>%
  mutate(b_p_val= paste(round(estimate,3),  p_value) ) %>%
  mutate(region="Latin America") %>%
  select(region, term, b_p_val) 


write.csv(coef_africa, "Africa_coefs_logit.csv")
write.csv(coef_asia, "Asia_coefs_logit.csv")
write.csv(coef_europe, "Europe_coefs_logit.csv")
write.csv(coef_na, "NA_coefs_logit.csv")
write.csv(coef_la, "LA_coefs_logit.csv")
