
library(tidyverse)
library(tidyr)
library(janitor)
library(dplyr)
library(readr)
library(plotly)
library(ggplot2)
library(scales)

load("/Users/andie/Documents/MeasureofAmerica/NSDUH_2020.Rdata")
names(NSDUH_2020) <- tolower(names(NSDUH_2020)) # make all var names lowercase
nsduh_df <- as_tibble(NSDUH_2020) # for better printing of huge df
main_nsduh_df <- select(nsduh_df, 
  questid2, #ID
  age2, #age ;
  irsex, #gender ;
  sexident, #sexuality
  speakengl, #englishspeaking
  ireduhighst2, #education
  irfamin3, #total family income
  newrace2, #race/hispanic ;
  
  dstnrv30, #how often felt nervous past 30 days ;
  dstngd30, #how often feel down on yourself, no good or worthless
  addprev, #several days of sad/empty/depressed 
  impweeks, #num of weeks have difficulties because of mental health in past 12 months ;
  adwrdlot, #when problems were worst, think a lot about death
  adwrsthk, #suicide ideation ;
  ) #less to more severe

#filter 50~64, 65+
adult_nsduh <- filter(main_nsduh_df, age2 == 17)

#Demographics ------------------------------------------

adult_nsduh <- adult_nsduh %>%
  mutate(
    #age
    age2 = recode_factor(
      age2, `16` = "Between 50 and 64 years", `17` = "Responding is 65 years old or older"),
   
     #gender
    irsex = recode_factor(
      irsex, `1` = "Male", `2` = "Female"),
    
    #sexuality
    sexident = recode_factor(
      sexident, `1` = "Heterosexual/Straight", `2` = "Lesbian/Gay", `3` = "Bisexual", `85` = "Bad Data", `94` = "Don't Know",
      `97` = "Refused", `98` = "N/A", `99` = "Legitimate Skip"),
   
    #education
    ireduhighst2 = factor(
      case_when(
      ireduhighst2 == 1 %in% 7 ~ "No diploma",
      ireduhighst2 == 8 ~ "High school diploma/GED",
      ireduhighst2 == 9 ~ "Some college credit, no degree",
      ireduhighst2 == 10 ~ "Associate's degree",
      ireduhighst2 == 11 ~ "College graduate or higher"
      )),
   
    #total family income
    irfamin3 = recode_factor(
      irfamin3, `1` = "Less than $10,000", `2` = "$10,000-$19,999", `3` = "$20,000-$29,999", `4` = "$30,000-$39,999",
      `5` = "$40,000-$49,999", `6` = "$50,000-$74,999", `7` = "$75,000 or more"),
    
    #race/latino; assumed all listed race is non-latino
    newrace2 = recode_factor(
      newrace2, `1` = "White", `2` = "Black", `3` = "Native American or Alaska Natives", `4` = "Native Hawaiian or Other Pacific Islander",
      `5` = "Asian", `6` = "More than one race", `7` = "Latino/a/x"),
    
    #englishspeaking
    speakengl = recode_factor(
      speakengl, `1` = "Very well", `2` = "Well", `3` = "Not well", `4` = "Not at all",
      `85` = "Bad Data", `94` = "Don't Know", `97` = "Refused", `98` = "N/A")
      ) %>%
  droplevels() %>%
  rename(Age = age2, 
         Gender = irsex, 
         Sexuality = sexident, 
         Education = ireduhighst2, 
         English = speakengl,
         Total_Fam_Income = irfamin3, 
         Race_Ethnicity = newrace2, 
         ID = questid2)

#Mental Health -----------------------------------------------
adult_mh <- adult_nsduh %>%
  mutate(
    #nervous past 30 days
    dstnrv30 = recode_factor(
      dstnrv30, `1` = "All of the time", `2` = "Most of the time", `3` = "Some of the time", `4` = "A little or none of the time",
      `5` = "A little or none of the time", `85` = "Bad Data", `94` = "Don't Know", `97` = "Refused", `98` = "N/A",
      `99` = "Legitimate Skip"),
    
    #how often feel down on yourself, no good or worthless in the past 30 days
    dstngd30 = recode_factor(
      dstngd30, `1` = "All of the time", `2` = "Most of the time", `3` = "Some of the time", `4` = "A little of the time",
      `5` = "None of the time", `85` = "Bad Data", `94` = "Don't Know", `97` = "Refused", `98` = "N/A",
      `99` = "Legitimate Skip"),
    
    #several days of sad/empty/depressed
    addprev = recode_factor(
      addprev, `1` = "Yes", `2` = "No", `85` = "Bad Data", `94` = "Don't Know", `97` = "Refused", `98` = "N/A",
      `99` = "Legitimate Skip"), #filter the Yes on a different line
    
    #num of weeks have difficulties because of mental health in past 12 months
    impweeks = factor(
      case_when(
        impweeks%in%1:10 ~ "1 to 10",
        impweeks%in%11:20 ~ "11 to 20",
        impweeks%in%21:30 ~ "21 to 30",
        impweeks%in%31:40 ~ "31 to 40",
        impweeks%in%41:52 ~ "41 to 52",
        impweeks == 85 ~ "Bad Data",
        impweeks == 94 ~ "Don't Know",
        impweeks == 97 ~ "Refused",
        impweeks == 98 ~ "N/A",
        impweeks == 99 ~ "Legitimate skip")),
   
    #when problems were worst, think a lot about death
    adwrdlot = recode_factor(
      adwrdlot, `1` = "Yes", `2` = "No", `94` = "Don't Know", `97` = "Refused", `98` = "N/A",
      `99` = "Legitimate Skip"),
    
    #suicide ideation
    adwrsthk = recode_factor(
      adwrsthk, `1` = "Yes", `2` = "No", `94` = "Don't Know", `97` = "Refused", `98` = "N/A",
      `99` = "Legitimate Skip"),
    ) %>%
  
  droplevels() %>%
  #all in the past 30 days
  rename(Nervous = dstnrv30, 
         Down_self = dstngd30, 
         Days_of_Depressed = addprev, 
         Difficult_Weeks = impweeks, 
         Think_Death = adwrdlot,
         Suicide_Ideation = adwrsthk)

write.csv(adult_mh, "adult_mental_health.csv")

#create new variable for race x gender (ifelse function) &

#sum don't know, refused, n/a sum function

################# Visualize

##Race and Mental Health------------------------------------

adult_mh_2 <- adult_mh %>%

  #follow & check numbers as i go
  #don't need to isolate, use colors that stand out more
#Race and Nervous
race_and_nervous <- adult_mh %>%
  count(Race_Ethnicity, Nervous)%>%
  group_by(Race_Ethnicity)%>%
  mutate(percent = (n/sum(n)))

new_denom <- race_and_nervous %>%
  mutate(new = n/percent)

subset_A <- subset (new_denom, Nervous=="All of the time" | Nervous == "Most of the time" | Nervous == "Some of the time" |
                      Nervous == "A little or none of the time")
subset_B <- subset (new_denom, Nervous=="Bad Data" | Nervous == "Refused" | Nervous == "N/A" |
                      Nervous == "Legitimate Skip")

subset_B_new <- subset_B %>%
  mutate(subset_B$n (summarize(newsum = sum(n))))

subset_B$n <- summarize(newsum = sum(n))

  race_and_nervous %>%
  ggplot(aes(x=Race_Ethnicity, y=percent, fill=Nervous)) + 
  geom_bar (stat='identity', position = "dodge") + scale_y_continuous(labels=scales::percent) +
          #not sure how to remove Don't Know, Refused, NA, Bad Data" or filter out some of the categories
          #not sure how to not affect the percentages when I remove categories
          #race_and_nervous <- subset (adult_mh, Nervous!= "Don't Know" & Nervous!= "Bad Data" & Nervous != "Legitimate Skip" & Nervous != "Refused" & Nervous != "N/A")
    labs(title = "Nervous in the Past 30 Days",
         x = "Race and Ethnicity", y = "Percentage") + 
    theme (plot.title = element_text (face = "bold"),
    axis.title.x = element_text (vjust = -2, margin=margin(50,0,0,0)),
    axis.title.y = element_text (vjust = +2, margin=margin(50,0,0,0))) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) + theme_light() +
    scale_fill_manual(values=c("#025043",
                            "#036c5f",
                            "#05998c",
                            "#4fb9af",
                            "#b3e0dc",
                            "#b3e0dc",
                            "#b3e0dc"))
  
  write.csv(race_and_nervous, file="race_and_nervous.csv")
  
  #datawrapper for keeping bar sizes
  #pivot table to have mental health on the top and race on the left

#Race and Down_self
race_and_downself <- adult_mh %>%
  count(Race_Ethnicity, Down_self)%>%
  group_by(Race_Ethnicity)%>%
  mutate(percent = (n/sum(n))*100)

race_and_downself %>%
  ggplot(aes(x=Race_Ethnicity, y=percent, fill=Down_self)) + 
  geom_bar (stat='identity', position = "dodge")

#Race and Depressed Days
race_and_days <- adult_mh %>%
  count(Race_Ethnicity, Days_of_Depressed)%>%
  group_by(Race_Ethnicity)%>%
  mutate(percent = (n/sum(n))*100)

race_and_days %>%
  ggplot(aes(x=Race_Ethnicity, y=percent, fill=Days_of_Depressed)) + 
  geom_bar (stat='identity', position = "dodge")

#Race and Weeks
race_and_weeks <- adult_mh %>%
  count(Race_Ethnicity, Difficult_Weeks)%>%
  group_by(Race_Ethnicity)%>%
  mutate(percent = (n/sum(n))*100)

write.csv(race_and_weeks, file="race_and_weeks.csv")

race_and_weeks %>%
  ggplot(aes(x=Race_Ethnicity, y=percent, fill=Difficult_Weeks)) + 
  geom_bar (stat='identity', position = "dodge")

#Race and Think Death
race_and_think <- adult_mh %>%
  count(Race_Ethnicity, Think_Death)%>%
  group_by(Race_Ethnicity)%>%
  mutate(percent = (n/sum(n))*100)

race_and_think %>%
  ggplot(aes(x=Race_Ethnicity, y=percent, fill=Think_Death)) + 
  geom_bar (stat='identity', position = "dodge")

#Race and Suicide Ideation
race_and_suicide <- adult_mh %>%
  count(Race_Ethnicity, Suicide_Ideation)%>%
  group_by(Race_Ethnicity)%>%
  mutate(percent = (n/sum(n))*100)

write.csv(race_and_suicide, file="race_and_suicide.csv")

race_and_suicide %>%
  ggplot(aes(x=Race_Ethnicity, y=percent, fill=Suicide_Ideation)) + 
  geom_bar (stat='identity', position = "dodge")


##Gender and Mental Health-------------------------------
#Gender and Nervous
gender_and_nervous <- adult_mh %>%
  count(Gender, Nervous)%>%
  group_by(Gender)%>%
  mutate(percent = (n/sum(n))*100)

write.csv(gender_and_nervous, file="gender_and_nervous.csv")

gender_and_nervous %>%
  ggplot(aes(x=Gender, y=percent, fill=Nervous)) + 
  geom_bar (stat='identity', position = "dodge")

#Gender and Down_self
gender_and_downself <- adult_mh %>%
  count(Gender, Down_self)%>%
  group_by(Gender)%>%
  mutate(percent = (n/sum(n))*100)

gender_and_downself %>%
  ggplot(aes(x=Gender, y=percent, fill=Down_self)) + 
  geom_bar (stat='identity', position = "dodge")

#Gender and Depressed Days
gender_and_days <- adult_mh %>%
  count(Gender, Days_of_Depressed)%>%
  group_by(Gender)%>%
  mutate(percent = (n/sum(n))*100)

gender_and_days %>%
  ggplot(aes(x=Gender, y=percent, fill=Days_of_Depressed)) + 
  geom_bar (stat='identity', position = "dodge")

#Gender and Weeks
gender_and_weeks <- adult_mh %>%
  count(Gender, Difficult_Weeks)%>%
  group_by(Gender)%>%
  mutate(percent = (n/sum(n))*100)

write.csv(gender_and_weeks, file="gender_and_weeks.csv")

gender_and_weeks %>%
  ggplot(aes(x=Gender, y=percent, fill=Difficult_Weeks)) + 
  geom_bar (stat='identity', position = "dodge")

#Gender and Think Death
gender_and_think <- adult_mh %>%
  count(Gender, Think_Death)%>%
  group_by(Gender)%>%
  mutate(percent = (n/sum(n))*100)

gender_and_think %>%
  ggplot(aes(x=Gender, y=percent, fill=Think_Death)) + 
  geom_bar (stat='identity', position = "dodge")

#Gender and Suicide Ideation
gender_and_suicide <- adult_mh %>%
  count(Gender, Suicide_Ideation)%>%
  group_by(Gender)%>%
  mutate(percent = (n/sum(n))*100)

write.csv(gender_and_suicide, file="gender_and_suicide.csv")
gender_and_suicide %>%
  ggplot(aes(x=Gender, y=percent, fill=Suicide_Ideation)) + 
  geom_bar (stat='identity', position = "dodge")

##Education and Mental Health-----------------------------------------------------
educ_and_nervous <- adult_mh %>%
  count(Education, Nervous)%>%
  group_by(Education)%>%
  mutate(percent = (n/sum(n))*100)

educ_and_nervous %>%
  ggplot(aes(x=Education, y=percent, fill=Nervous)) + 
  geom_bar (stat='identity', position = "dodge")

#Education and Down_self
educ_and_downself <- adult_mh %>%
  count(Education, Down_self)%>%
  group_by(Education)%>%
  mutate(percent = (n/sum(n))*100)

educ_and_downself %>%
  ggplot(aes(x=Education, y=percent, fill=Down_self)) + 
  geom_bar (stat='identity', position = "dodge")

#Education and Depressed Days
educ_and_days <- adult_mh %>%
  count(Education, Days_of_Depressed)%>%
  group_by(Education)%>%
  mutate(percent = (n/sum(n))*100)

educ_and_days %>%
  ggplot(aes(x=Education, y=percent, fill=Days_of_Depressed)) + 
  geom_bar (stat='identity', position = "dodge")

#Education and Weeks
educ_and_weeks <- adult_mh %>%
  count(Education, Difficult_Weeks)%>%
  group_by(Education)%>%
  mutate(percent = (n/sum(n))*100)

educ_and_weeks %>%
  ggplot(aes(x=Education, y=percent, fill=Difficult_Weeks)) + 
  geom_bar (stat='identity', position = "dodge")

#Education and Think Death
educ_and_think <- adult_mh %>%
  count(Education, Think_Death)%>%
  group_by(Education)%>%
  mutate(percent = (n/sum(n))*100)

educ_and_think %>%
  ggplot(aes(x=Education, y=percent, fill=Think_Death)) + 
  geom_bar (stat='identity', position = "dodge")

#Education and Suicide Ideation
educ_and_suicide <- adult_mh %>%
  count(Education, Suicide_Ideation)%>%
  group_by(Education)%>%
  mutate(percent = (n/sum(n))*100)

educ_and_suicide %>%
  ggplot(aes(x=Education, y=percent, fill=Suicide_Ideation)) + 
  geom_bar (stat='identity', position = "dodge")

##Total Family Income and Mental Health----------------------------------------
#Income and Nervous
income_and_nervous <- adult_mh %>%
  count(Total_Fam_Income, Nervous)%>%
  group_by(Total_Fam_Income)%>%
  mutate(percent = (n/sum(n))*100)

income_and_nervous %>%
  ggplot(aes(x=Total_Fam_Income, y=percent, fill=Nervous)) + 
  geom_bar (stat='identity', position = "dodge")

write.csv(income_and_nervous, file="income_and_nervous.csv")

#Income and Down_self
income_and_downself <- adult_mh %>%
  count(Total_Fam_Income, Down_self)%>%
  group_by(Total_Fam_Income)%>%
  mutate(percent = (n/sum(n))*100)

income_and_downself %>%
  ggplot(aes(x=Total_Fam_Income, y=percent, fill=Down_self)) + 
  geom_bar (stat='identity', position = "dodge")

#Income and Depressed Days
income_and_days <- adult_mh %>%
  count(Total_Fam_Income, Days_of_Depressed)%>%
  group_by(Total_Fam_Income)%>%
  mutate(percent = (n/sum(n))*100)

income_and_days %>%
  ggplot(aes(x=Total_Fam_Income, y=percent, fill=Days_of_Depressed)) + 
  geom_bar (stat='identity', position = "dodge")

#Income and Weeks
income_and_weeks <- adult_mh %>%
  count(Total_Fam_Income, Difficult_Weeks)%>%
  group_by(Total_Fam_Income)%>%
  mutate(percent = (n/sum(n))*100)

write.csv(income_and_weeks, file="income_and_weeks.csv")

income_and_weeks %>%
  ggplot(aes(x=Total_Fam_Income, y=percent, fill=Difficult_Weeks)) + 
  geom_bar (stat='identity', position = "dodge")

#Income and Think Death
income_and_think <- adult_mh %>%
  count(Total_Fam_Income, Think_Death)%>%
  group_by(Total_Fam_Income)%>%
  mutate(percent = (n/sum(n))*100)

income_and_think %>%
  ggplot(aes(x=Total_Fam_Income, y=percent, fill=Think_Death)) + 
  geom_bar (stat='identity', position = "dodge")


#Income and Suicide Ideation
income_and_suicide <- adult_mh %>%
  count(Total_Fam_Income, Suicide_Ideation)%>%
  group_by(Total_Fam_Income)%>%
  mutate(percent = (n/sum(n))*100)

write.csv(income_and_suicide, file="income_and_suicide.csv")

income_and_suicide %>%
  ggplot(aes(x=Total_Fam_Income, y=percent, fill=Suicide_Ideation)) + 
  geom_bar (stat='identity', position = "dodge")