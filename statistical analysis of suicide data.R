library(dplyr)
library(readxl)

suicide = read_excel("D:/term 1/statistics/Assignment-3 dataset - Suicide data (Batch 4B).xls")
View(suicide)
###--------------------------------------------------##_____________-------------##
#A. How many children (<= 18 years) have died of which how many by 
#(1) hanging, 
#(2) Poisson,
#(3) Burns 
#(4) drowning

a = c("Hanging","Poisson","Burns","Drowning")

suicide_a = suicide%>%filter(`Age (yrs)`<= 18,`Method of attempt to suicide` %in% a,`Outcome of suicide` == "Died")%>%
  group_by(`Age (yrs)`,`Method of attempt to suicide`)%>%
  summarise(total = n())
View(suicide_a)
####---------------------------------#####--------------------------####------------------###
#B. How many students died due to 
#(a) depression 
#(b) love failure 
#(c) Pain during menstruation (Dysmenorrhea)
#(d) failure in studies 
#(e) threatned by some one to marry

reasons = suicide%>%filter(`Occupation`=='Student',`Outcome of suicide`=='Died', `Cause of suicide` %in% c("Depression", "Love failure","Abdominal pain (Dysmenorrhea)",
  "Chronic abdomen pain (Dysmenorrhea)", "Dysmmenorhea", "Failure in studies", "Threatned by some one to marry"))%>%
  group_by(`Cause of suicide`) %>% summarise(Total =  n())
View(reasons)
###------------------------------3##----------------
# C. What is the ratio of male and female died to love failure?

love_failure_male = suicide%>%filter(`Cause of suicide` == "Love failure" & suicide$Sex == "Male")%>%group_by(`Cause of suicide`)%>%
  summarise(total_male = n())
View(love_failure_male)

love_failure_female = suicide%>%filter(`Cause of suicide` == "Love failure" & suicide$Sex == "Female")%>%group_by(`Cause of suicide`)%>%
  summarise(total_female = n())
View(love_failure_female)

ratio_love_failure = love_failure_male$total_male/love_failure_female$total_female
View(ratio_love_failure)
####-----------------------------------------####-------------------###-----------------
#D. How many students died due to Poisson?

suicide_poison = suicide%>%filter(`Method of attempt to suicide` == "Poisson")%>%
  summarise(total_poisson = n())
View(suicide_poison)
###---------------------------####---------------------------------------------###------------------------------
#E)How many died due to love failure among 
#(1) upper middle class Hindu, 
#(2) lower class christian, 
#(3) Middle class Muslim

suicide_hindu = suicide%>%filter(SES == "Upper Middle" , Religion == "Hindu" , `Cause of suicide`== "Love failure" , `Outcome of suicide` == "Died")%>%
  group_by(SES,Religion,`Cause of suicide`,`Outcome of suicide`)%>%
  summarize(total_hindu = n())
View(suicide_hindu)


suicide_christian = suicide%>%filter(SES == "Lower" & Religion == "Christian",`Cause of suicide`== "Love failure" , `Outcome of suicide` == "Died")%>%
  group_by(SES,Religion)%>%summarize(total_christian = n())
View(suicide_christian)

suicide_Muslim= suicide%>%filter(SES == "Middle" & Religion == "Muslim",`Cause of suicide`== "Love failure" , `Outcome of suicide` == "Died")%>%
  group_by(SES,Religion)%>%
  summarize(total_Muslim = n())
View(suicide_Muslim)
######-------------------------------------------------------------------------####---------------------------##
#6. How many between the age of 19 - 20 years have died due to 
#(a) love failure, 
#(b) dowry death, 
#(c) quarreling with husband, 
#(d) failure in studies


suicide_a = suicide%>%filter(`Cause of suicide` == "Love failure",`Age (yrs)` == 19 | `Age (yrs)` == 20,`Outcome of suicide`=="Died")%>%
  group_by(`Cause of suicide`,`Outcome of suicide`)%>%
  summarise(total_a = n())
View(suicide_a)

suicide_b = suicide%>%filter(`Cause of suicide` == "Dowry death",`Age (yrs)` == 19 | `Age (yrs)` == 20,`Outcome of suicide`=="Died")%>%
  group_by(`Cause of suicide`,`Outcome of suicide`)%>%
  summarise(total_b = n())
View(suicide_b)

suicide_c = suicide%>%filter(`Cause of suicide` == "Querreling with husband",`Age (yrs)` == 19 | `Age (yrs)` == 20,`Outcome of suicide`=="Died")%>%
  group_by(`Cause of suicide`,`Outcome of suicide`)%>%
  summarise(total_c = n())
View(suicide_c)

suicide_d = suicide%>%filter(`Cause of suicide` == "Failure in studies",`Age (yrs)` == 19 | `Age (yrs)` == 20,`Outcome of suicide`=="Died")%>%
  group_by(`Cause of suicide`,`Outcome of suicide`)%>%
  summarise(total_d = n())
View(suicide_d)
############################-----------------------------------------#####------------------------####
#7. How many between at the age of 21 years have died by taking sleeping pills due to love failure

s_lovepills = suicide%>%filter(`Cause of suicide` == "Love failure",`Age (yrs)` < 21,`Outcome of suicide`=="Died")%>%
  group_by(`Cause of suicide`,`Outcome of suicide`)%>%
  summarise(total_d = n())
View(s_lovepills)
####----------------------------------####----------------------------------------####--------------------
#8.How many have died death of husband? What is the age of the victim and belonging to which religion and SES?

suicide_husband = suicide%>%filter(Sex == "Male" , `Marital status` == "Married", `Outcome of suicide` == "Died")%>%
  group_by(Religion,SES)%>%
  summarise(total_hu = n())
View(suicide_husband)
###--------------------------------####--------------------------------------####
#9. What is the ratio of married to unmarried persons who have died? How many married women died due to dowry deaths? 
#How many of these are Hindus and Muslims?

suicide_married = suicide%>%filter(`Marital status` == "Married", `Outcome of suicide` == "Died")%>%group_by(`Marital status`,`Outcome of suicide`)%>%
  summarise(total_married = n())
View(suicide_married)

suicide_unmarried = suicide%>%filter(`Marital status` == "Unmarried",`Outcome of suicide` == "Died")%>%group_by(`Marital status`,`Outcome of suicide`)%>%
  summarise(total_unmarried = n())
View(suicide_unmarried)

suicide_ratio = suicide_married$total_married/suicide_unmarried$total_unmarried
View(suicide_ratio)

 suicide_women = suicide%>%filter(Sex == "Female", `Cause of suicide` == "Dowry death",`Outcome of suicide` == "Died",Religion == "Hindu" | Religion =="Muslim")%>%
   group_by(Sex,`Cause of suicide`,Religion)%>%
   summarise(total_women = n())
 View(suicide_women)

road_accident = read_excel("C:/Users/Administrator/Desktop/Road Accident.xlsx")
 ####-----------------------------------####------------------------------------------####----------------###
# 10.What is the ratio of suicide and accidental deaths? What is the distribution of time of deaths? 
#How many persons belonging to Jains died due to depression?
 
suicide_death = suicide%>%filter(`Outcome of suicide`== "Died")%>%group_by(`Outcome of suicide`)%>%
  summarise(total_accident = n())
View(suicide_death)

road_death = road_accident%>%summarise(total_death = n())
View(road_death)

ratio = (suicide_death$total_accident/road_death$total_death)
View(ratio)
 ####----------------------------#####------------------------------####
#11.State some of the hypothesis which may be formulated based on this data? 																

###hypothesis 1
#null hypo-death due to poisson does not depends on occupation
#alternate-death due to poisson depends on occupation

###hypothesis 2
#null-null hypo-lovefailure  does not depends on religion
#alternate hypo-lovefailure depends on religion 


#12.	Is there sufficient data to test these hypothesis? 
#If so, use the available and test those hypothesis. 																

###the data is sufficient to test the hypothesis
##1
M=aov(love_failure~religion,suicide)
summary(M)


##2
M=aov(poisson~occupationalStatus,suicide)
summary(M)




###-------------------------------------------####--------------------------------------------####
#13.	What proportion have survived even after attempt to suicide?																

suicide_survived = suicide%>%filter(`Outcome of suicide` == "Survived")%>%group_by(`Outcome of suicide`)%>%
  summarise(total_survied = n())
View(suicide_survived)
####--------------------------------------####-----------------------------####

# 14.	How many of the survivors brought within 60 minutes to hospital	


suicide_hospital = suicide%>%filter(`Time interval between attempt to suicide and bring to hospital`<= "60 minutes",
                                    `Time interval between attempt to suicide and bring to hospital`!= "1 hour"&
                                      `Time interval between attempt to suicide and bring to hospital` != "2 hours"&
                                      `Time interval between attempt to suicide and bring to hospital` != "2 days"&
                                      `Time interval between attempt to suicide and bring to hospital` != "3 hours"&
                                      `Time interval between attempt to suicide and bring to hospital` != "4 hours"&
                                      `Time interval between attempt to suicide and bring to hospital` != "5 hours")%>%
  group_by(`Time interval between attempt to suicide and bring to hospital`)%>%
  summarise(total_hospital = n())
View(suicide_hospital)



#15.	Considering outcome of suicide as dependent fit a logistic regression model taking all 
#other variables as independent variables.																

T=lm(suicide$`Outcome of suicide`~suicide$`Cause of suicide`,suicide)



  