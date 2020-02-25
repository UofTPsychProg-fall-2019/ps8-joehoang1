### In this problem set, you will tidy up an IAT dataset 
### The original data is available at https://osf.io/szwuf/, but it comes as an SPSS .sav file
### I've included trimmed down .csv version of 2019's data in this repository for you to work with

# loading libraries  ---------------------------------------------
library(tidyverse)

# reading in IAT data  ---------------------------------------------
#Setting Working Directory
setwd("~/Documents/GitHub/ps8-joehoang1")
# use a tidyverse function to read in the included IAT_2019.csv file 
tbl <- read.csv("IAT.csv")

# Removing unnecessary rows and columns  ---------------------------------------------
# This data frame only contains 21 of the 454 available variables, but it's still too much

# use tidyverse functions so that only the following variables are included: 'session_id',"genderidentity","raceomb_002","D_biep.White_Good_all","Mn_RT_all_3467",
#       "edu_14","politicalid_7","STATE","att_7","tblacks_0to10","twhites_0to10","labels"

#GenderIdentity is not a variable so I pulled out gender instead
tbl_clean <- select(tbl, session_id, gender, raceomb_002,D_biep.White_Good_all, Mn_RT_all_3467,
                          edu_14, politicalid_7, STATE, att_7, tblacks_0to10, twhites_0to10, labels)

# next, clean up the rows 
# our primary dependent variable is D_biep.White_Good_all, but some subjects
# don't have any data. Remove the rows with missing D_biep.White_Good_all entries 
tbl_clean <- drop_na(tbl_clean, D_biep.White_Good_all)

# Renaming varialbles  ---------------------------------------------

# next rename variables with more intuitive, short labels 
# here are some suggestions (along with variable info)
# id : session_id (subject number)
# gender : genderidentity (gender 1 "Male" 2 "Female" 3 "Trans male/Trans man" 4 "Trans female/Trans woman" 5 "Genderqueer/Gender nonconforming" 6 "A different identity") 
# race : raceomb_002 (race: 1 "American Indian" 2 "East Asian" 3 "South Asian" 4 "Hawaiian Pacifica Islander" 5 "black Africian American" 6 "white" 7 "other" 8 "multiracial")
# bias :D_biep.White_Good_all (overall IAT score)
# rt : Mn_RT_all_3467 (overall reaction time)
# edu : edu_14 (education: 1 "elementary" 2 "junior high" 3 "some high school" 4 "HS grad" 5 "some college" 6 "associate's" 7 "bachelor's" 8 "some grad" 9 "MA" 10 "JD" 11 "MD" 12 "PHD" 13 "other advanced" 14 "MBA")
# pol : politicalid_7 (political identification: 1 "strongly conservative 7 "strongly liberal)
# state : STATE
# att : att_7 (race attitude 1 "strongly prefer AA" 7 "strongly prefer white")
# temp_b : tblacks_0to10 (temperature feelings black 1 "extremely cold" 10 "extremly warm")
# temp_w : twhites_0to10 (temperature feelings black 1 "extremely cold" 10 "extremly warm")

tbl_clean <- rename(tbl_clean,
                    id = session_id, 
                    gender = gender,
                    race = raceomb_002,
                    bias =  D_biep.White_Good_all,
                    rt = Mn_RT_all_3467,
                    edu = edu_14,
                    pol = politicalid_7,
                    state = STATE,
                    att= att_7,
                    temp_b = tblacks_0to10,
                    temp_w = twhites_0to10)

#  missing values  ---------------------------------------------  

summary(tbl_clean)
# some of our variables have missing values that aren't properly coded as missing  
# recode missing values in gender and state

tbl_clean$gender <- na_if(tbl_clean$gender, "")
tbl_clean$state <- na_if(tbl_clean$state, "")

# changing variable types  ---------------------------------------------  
# next, convert id and all variables that are character types to factors
# try to convert all variables at once using tidyverse functions
# Have to covert: id and state

factor_var <- c('id','state')

tbl_clean <- mutate_at(tbl_clean, factor_var, ~factor(.))
#Check to see if id is now a factor
is.factor(tbl_clean$id)

# recoding variables  ---------------------------------------------  
# participants were instructed to select all the gender idenities that apply to them
# this results in a lot of combinations!
# this pipeline tabulates the number of participants who endorse different gender identities. 

#Because an error occured when running the bottom line of code, an additonal code was added
tbl_clean$gender <- fct_explicit_na(tbl_clean$gender)
gender_count <- tbl_clean %>% group_by(gender) %>% tally()  

# sort the output and then use indexing to print the 3 most common response (not inlcuding missing values)
gender_count <- arrange(gender_count, desc(n)) %>% slice(c(1,2,4))
print(gender_count)
#Most common gender: 2, 1, 3

# create a new variable that recodes gender to have 4 levels: the 3 most common responses and the others collapsed together
# you can use the key provided on line 31 to understand the levels
# check out recode documentation to see if there's a trick for setting defaults values for unspecified rows
# *note that this excercise in data recoding doesn't reflect the instructors' views on gender identities...
tbl_clean$gender4 <- ifelse(tbl_clean$gender=='[2]', 2,
                            ifelse(tbl_clean$gender=='[1]', 1,
                                   ifelse(tbl_clean$gender=='[3]', 3,
                                          ifelse(tbl_clean$gender=="NA", NA, 4))))
#check to see if done correctly
table(tbl_clean$gender4) #Correct!
is.factor(tbl_clean$gender4)
tbl_clean$gender4 <- as.factor(tbl_clean$gender4) #Turn new varible into a factor

# Now take a look at how highest obtained education is coded (key on line 35)
edu_count <- tbl_clean %>% group_by(edu) %>% tally()  

#create a new variable that recodes education into: no highscool, some highschool, highschool graduate, some college, postsecondary degree, masters (MA & MBA), advanced degree
#remember that the recode function isn't always the best solution for numeric variables

#since edu has 14 levels but only 7 were given, I recoded the remaining possible levels as 8
'no highscool'
'some highschool'
'highschoolgraduate'
'some college'
'postsecondary degree'
'masters (MA & MBA)'
'advanced degree'
tbl_clean$edu7 <- ifelse(tbl_clean$edu=='1', 'no highschool',
                         ifelse(tbl_clean$edu=='2', 'some highschool',
                                ifelse(tbl_clean$edu=='3', 'highschool graduate',
                                       ifelse(tbl_clean$edu=='4', 'some college',
                                              ifelse(tbl_clean$edu=='5', 'postsecondary degree',
                                                     ifelse(tbl_clean$edu=='6', 'masters (MA & MBA)',
                                                            ifelse(tbl_clean$edu=='7', 'advanced degree',
                                                                   ifelse(tbl_clean$edu=='8', 'other',NA))))))))


# mutating variables ---------------------------------------------  
# rewrite the above recoding steps so that they both occur within a single call of the mutate function
tbl_clean <- mutate(tbl_clean, edu8=ifelse(tbl_clean$edu=='1', 'no highschool',
                                 ifelse(tbl_clean$edu=='2', 'some highschool',
                                        ifelse(tbl_clean$edu=='3', 'highschool graduate',
                                               ifelse(tbl_clean$edu=='4', 'some college',
                                                      ifelse(tbl_clean$edu=='5', 'postsecondary degree',
                                                             ifelse(tbl_clean$edu=='6', 'masters (MA & MBA)',
                                                                    ifelse(tbl_clean$edu=='7', 'advanced degree',
                                                                           ifelse(tbl_clean$edu=='8', 'other',NA)))))))))
                     
  
# filtering and math ---------------------------------------------  

# using filtering, calculate and print the mean bias score for:

# white men
#race 6 = white
# gender 1 = men

tbl_clean %>%
  select(race, gender, bias) %>%
  filter(race == 6) %>%
  filter(gender == '[1]') %>%
  summarise(mean = mean(bias))
#M=.379

# white women
#race 6 = white
# gender 2 = women

tbl_clean %>%
  select(race, gender, bias) %>%
  filter(race == 6) %>%
  filter(gender == '[2]') %>%
  summarise(mean = mean(bias))
#M=.327

# advanced degree holders who are men
# advanced degree holders = edu7 7
# men = gender 1

tbl_clean %>%
  select(edu7, gender, bias) %>%
  filter(edu7 == 'advanced degree') %>%
  filter(gender == '[1]') %>%
  summarise(mean = mean(bias))
#M=.320

# high school graduates who are men
#high school graduates = edu7 3
#men = gender 1

tbl_clean %>%
  select(edu7, gender, bias) %>%
  filter(edu7 == 'highschool graduate') %>%
  filter(gender == '[1]') %>%
  summarise(mean = mean(bias))
#M=.31
