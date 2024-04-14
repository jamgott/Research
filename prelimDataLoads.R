library("ipumsr")
library(tidyverse)
library(stringr)
setwd("C:/Users/jmca9/Downloads/atus_00010.dat")
ddi <- read_ipums_ddi("atus_00010.xml")
#hierarchical
data1 <- read_ipums_micro(ddi)

setwd("C:/Users/jmca9/Downloads/atus_00011.dat")
ddi <- read_ipums_ddi("atus_00011.xml")
#rectangular activity
data2 <- read_ipums_micro(ddi)


setwd("C:/Users/jmca9/Downloads/atus_00015.dat")
ddi <- read_ipums_ddi("atus_00015.xml")
#rectangular person
data3 <- read_ipums_micro(ddi)



#Create dummy that is 1 when the activity happens at home and 0 o.w.
data2$HOME <- ifelse(data2$WHERE==101,1,0)
#Create dummy that is 1 when the activity happens at the office and 0 o.w.
data2$OFFICE <- ifelse(data2$WHERE==102,1,0)
#convert WHERE variable to a character variable for text analysis
data2$WHERE <- as.character(data2$WHERE)
#Create dummy that is 1 when the activity happens on transportation and 0 o.w.
data2$TRANSPORT <- ifelse(str_detect(data2$WHERE, '^2'),1,0)
#Create dummy that is 1 when the activity happens outside the previous 3 locations
data2$OTHER_LOC <- ifelse((data2$OFFICE == 1 | data2$HOME==1)|data2$TRANSPORT==1,
                          0,1)



#See distributions of where for work activities
data2$ACTIVITY<-as.character(data2$ACTIVITY)
data2$WORK = ifelse(str_detect(data2$ACTIVITY, '^501'),1,0)
#Show share of work done at home for each year
dat1 = data2%>%group_by(YEAR)%>%filter(WORK ==1)%>%summarize(HOME_SHARE = 
                                        sum(HOME)/(sum(HOME) +
                                        sum(OFFICE) + sum(TRANSPORT) + 
                                       sum(OTHER_LOC)))
#Show share of work done in the office for each year
dat2 = data2%>%group_by(YEAR)%>%filter(WORK == 1)%>%summarize(OFFICE_SHARE = 
                                       sum(OFFICE)/(sum(HOME) +
                                       sum(OFFICE) + sum(TRANSPORT) + 
                                       sum(OTHER_LOC)))
#Show share of work done on transportation for each year
dat3 = data2%>%group_by(YEAR)%>%filter(WORK ==1)%>%summarize(TRANSPORT_SHARE = 
                                       sum(TRANSPORT)/(sum(HOME) +
                                       sum(OFFICE) + sum(TRANSPORT) + 
                                       sum(OTHER_LOC)))
#Show share of work done in other locations or unrecorded locations for each year
dat4 = data2%>%group_by(YEAR)%>%filter(WORK ==1)%>%summarize(OTHER_SHARE = 
                                       sum(OTHER_LOC)/(sum(HOME) +
                                       sum(OFFICE) + sum(TRANSPORT) + 
                                       sum(OTHER_LOC)))
ggplot() +
  geom_point(data = dat1, aes(x = YEAR, y = HOME_SHARE), color = "blue") + 
  geom_point(data = dat2, aes(x = YEAR, y = OFFICE_SHARE), color = "red") + 
  geom_point(data = dat3, aes(x = YEAR, y = TRANSPORT_SHARE), color = "green") + 
  geom_point(data = dat4, aes(x = YEAR, y = OTHER_SHARE), color = "black") 
  

#See distributions of gender for work activities
#Show share of work done by men at home for each year
dat1 = data2%>%filter(SEX==1)%>%group_by(YEAR)%>%filter(WORK == 1)%>%summarize(HOME_SHARE = 
                                       sum(HOME)/(sum(HOME) +
                                       sum(OFFICE) + sum(TRANSPORT) + 
                                       sum(OTHER_LOC)))
#Show share of work done by men in the office for each year
dat2 = data2%>%filter(SEX==1)%>%group_by(YEAR)%>%filter(WORK == 1)%>%summarize(OFFICE_SHARE = 
                                        sum(OFFICE)/(sum(HOME) +
                                        sum(OFFICE) + sum(TRANSPORT) + 
                                        sum(OTHER_LOC)))
#Show share of work done on transportation for each year
# data2%>%filter(SEX==1)%>%group_by(YEAR)%>%filter(WORK ==1)%>%summarize(TRANSPORT_SHARE = 
#                                         sum(TRANSPORT)/(sum(HOME) +
#                                         sum(OFFICE) + sum(TRANSPORT) + 
#                                         sum(OTHER_LOC)))%>%
#                                         ggplot(aes(x=YEAR, y=TRANSPORT_SHARE)) + geom_point()
# #Show share of work done in other locations or unrecorded locations for each year
# data2%>%filter(SEX==1)%>%group_by(YEAR)%>%filter(WORK ==1)%>%summarize(OTHER_SHARE = 
#                                         sum(OTHER_LOC)/(sum(HOME) +
#                                         sum(OFFICE) + sum(TRANSPORT) + 
#                                         sum(OTHER_LOC)))%>%
#                                         ggplot(aes(x=YEAR, y=OTHER_SHARE)) + geom_point()




#share of work done by women in the home each year
dat3 = data2%>%filter(SEX==2)%>%group_by(YEAR)%>%filter(WORK == 1)%>%summarize(HOME_SHARE = 
                                       sum(HOME)/(sum(HOME) +
                                       sum(OFFICE) + sum(TRANSPORT) + 
                                       sum(OTHER_LOC)))
#Show share of work done by women in the office for each year
dat4 = data2%>%filter(SEX==2)%>%group_by(YEAR)%>%filter(WORK == 1)%>%summarize(OFFICE_SHARE = 
                                       sum(OFFICE)/(sum(HOME) +
                                       sum(OFFICE) + sum(TRANSPORT) + 
                                       sum(OTHER_LOC)))
#Plot the share of work done at the workplace. Blue is men, red is women.
ggplot() +
  geom_point(data = dat2, aes(x = YEAR, y = OFFICE_SHARE), color = "blue") + 
  geom_point(data = dat4, aes(x = YEAR, y = OFFICE_SHARE), color = "red")  

#Plot the share of work done at the workplace. Blue is men, red is women.
ggplot() +
  geom_point(data = dat1, aes(x = YEAR, y = HOME_SHARE), color = "blue") + 
  geom_point(data = dat3, aes(x = YEAR, y = HOME_SHARE), color = "red")
#Show share of work done on transportation for each year
# data2%>%filter(SEX==2)%>%group_by(YEAR)%>%filter(WORK ==1)%>%summarize(TRANSPORT_SHARE = 
#                                        sum(TRANSPORT)/(sum(HOME) +
#                                        sum(OFFICE) + sum(TRANSPORT) + 
#                                        sum(OTHER_LOC)))%>%
#                                        ggplot(aes(x=YEAR, y=TRANSPORT_SHARE)) + geom_point()
# #Show share of work done in other locations or unrecorded locations for each year
# data2%>%filter(SEX==2)%>%group_by(YEAR)%>%filter(WORK ==1)%>%summarize(OTHER_SHARE = 
#                                        sum(OTHER_LOC)/(sum(HOME) +
#                                        sum(OFFICE) + sum(TRANSPORT) + 
#                                        sum(OTHER_LOC)))%>%
#                                        ggplot(aes(x=YEAR, y=OTHER_SHARE)) + geom_point()



#See time distributions of less than college in the office each year
dat1 = data2%>%filter(EDUC<=21)%>%group_by(YEAR)%>%filter(WORK == 1)%>%summarize(OFFICE_SHARE = 
                                         sum(OFFICE)/(sum(HOME) +
                                         sum(OFFICE) + sum(TRANSPORT) + 
                                         sum(OTHER_LOC)))
#Show time distributions of college in the office for each year
dat2 = data2%>%filter(EDUC>21)%>%group_by(YEAR)%>%filter(WORK == 1)%>%summarize(OFFICE_SHARE = 
                                         sum(OFFICE)/(sum(HOME) +
                                         sum(OFFICE) + sum(TRANSPORT) + 
                                         sum(OTHER_LOC)))
#Show time shares of work done at the workplace for those with at least college
#and less than college
ggplot() +
  geom_point(data = dat1, aes(x = YEAR, y = OFFICE_SHARE), color = "blue") + 
  geom_point(data = dat2, aes(x = YEAR, y = OFFICE_SHARE), color = "red")

#Show time distributions of less than college working in the home each year
dat1 = data2%>%filter(EDUC <= 21)%>%group_by(YEAR)%>%filter(WORK == 1)%>%summarize(HOME_SHARE = 
                                         sum(HOME)/(sum(HOME) +
                                         sum(OFFICE) + sum(TRANSPORT) + 
                                         sum(OTHER_LOC)))
#Show time distributions of at least college working in the home each year
dat2 = data2%>%filter(EDUC > 21)%>%group_by(YEAR)%>%filter(WORK == 1)%>%summarize(HOME_SHARE = 
                                         sum(HOME)/(sum(HOME) +
                                         sum(OFFICE) + sum(TRANSPORT) + 
                                         sum(OTHER_LOC)))

#Show, for each year, time distributions of working in the home for those with
#less than college and at least college
ggplot() +
  geom_point(data = dat1, aes(x = YEAR, y = HOME_SHARE), color = "blue") + 
  geom_point(data = dat2, aes(x = YEAR, y = HOME_SHARE), color = "red")


#See distributions by age-group for work activities
#share of work done in the office each year for youngest age group 16-24
dat1 = data2%>%filter(AGE>=16 & AGE<=34)%>%group_by(YEAR)%>%filter(WORK == 1)%>%summarize(OFFICE_SHARE = 
                                         sum(OFFICE)/(sum(HOME) +
                                         sum(OFFICE) + sum(TRANSPORT) + 
                                         sum(OTHER_LOC)))
#share of work done in the office each year for age group 35-54
dat2 = data2%>%filter(AGE>=35 & AGE <=54)%>%group_by(YEAR)%>%filter(WORK == 1)%>%summarize(OFFICE_SHARE = 
                                         sum(OFFICE)/(sum(HOME) +
                                         sum(OFFICE) + sum(TRANSPORT) + 
                                         sum(OTHER_LOC)))
#share of work done in the office each year for age group 55+
dat3 = data2%>%filter(AGE>=55)%>%group_by(YEAR)%>%filter(WORK == 1)%>%summarize(OFFICE_SHARE = 
                                         sum(OFFICE)/(sum(HOME) +
                                         sum(OFFICE) + sum(TRANSPORT) + 
                                         sum(OTHER_LOC)))
#Plot share of work done in the office for each year and each age group.
#Blue is 16-34, red is 35-54, black is 55+
ggplot() +
  geom_point(data = dat1, aes(x = YEAR, y = OFFICE_SHARE), color = "blue") + 
  geom_point(data = dat2, aes(x = YEAR, y = OFFICE_SHARE), color = "red") +
  geom_point(data = dat3, aes(x = YEAR, y = OFFICE_SHARE), color = "black")
  

#share of work done at home each year for youngest age group 16-24
dat1 = data2%>%filter(AGE >= 16 & AGE <= 34)%>%group_by(YEAR)%>%filter(WORK == 1)%>%summarize(HOME_SHARE = 
                                         sum(HOME)/(sum(HOME) +
                                         sum(OFFICE) + sum(TRANSPORT) + 
                                         sum(OTHER_LOC)))
#share of work done at home each year for age group 35-54
dat2 = data2%>%filter(AGE >= 35 & AGE <= 54)%>%group_by(YEAR)%>%filter(WORK == 1)%>%summarize(HOME_SHARE = 
                                         sum(HOME)/(sum(HOME) +
                                         sum(OFFICE) + sum(TRANSPORT) + 
                                         sum(OTHER_LOC)))
#share of work done at home each year for age group 55+
dat3 = data2%>%filter(AGE >= 55)%>%group_by(YEAR)%>%filter(WORK == 1)%>%summarize(HOME_SHARE = 
                                         sum(HOME)/(sum(HOME) +
                                         sum(OFFICE) + sum(TRANSPORT) + 
                                           sum(OTHER_LOC)))
#Plot share of work done in the home for each year and each age group.
#Blue is 16-34, red is 35-54, black is 55+                                         
ggplot() +
  geom_point(data = dat1, aes(x = YEAR, y = HOME_SHARE), color = "blue") + 
  geom_point(data = dat2, aes(x = YEAR, y = HOME_SHARE), color = "red") +
  geom_point(data = dat3, aes(x = YEAR, y = HOME_SHARE), color = "black")


#RACIAL DISPARITIES
dat1 = data2%>%filter(RACE==100)%>%group_by(YEAR)%>%filter(WORK == 1)%>%summarize(HOME_SHARE = 
                                          sum(HOME)/(sum(HOME) +
                                          sum(OFFICE) + sum(TRANSPORT) + 
                                          sum(OTHER_LOC)))
#share of work done at home each year for age group 35-54
dat2 = data2%>%filter(RACE==110)%>%group_by(YEAR)%>%filter(WORK == 1)%>%summarize(HOME_SHARE = 
                                          sum(HOME)/(sum(HOME) +
                                          sum(OFFICE) + sum(TRANSPORT) + 
                                          sum(OTHER_LOC)))
#share of work done in the office each year for youngest age group 16-24
dat3 = data2%>%filter(RACE==100)%>%group_by(YEAR)%>%filter(WORK == 1)%>%summarize(OFFICE_SHARE = 
                                          sum(OFFICE)/(sum(HOME) +
                                          sum(OFFICE) + sum(TRANSPORT) + 
                                          sum(OTHER_LOC)))
#share of work done in the office each year for age group 35-54
dat4 = data2%>%filter(RACE==110)%>%group_by(YEAR)%>%filter(WORK == 1)%>%summarize(OFFICE_SHARE = 
                                          sum(OFFICE)/(sum(HOME) +
                                          sum(OFFICE) + sum(TRANSPORT) + 
                                          sum(OTHER_LOC)))

#BLUE IS ONLY WHITE, RED IS NON-WHITE
#Proportion of time worked in office
ggplot() +
  geom_point(data = dat3, aes(x = YEAR, y = OFFICE_SHARE), color = "blue") + 
  geom_point(data = dat4, aes(x = YEAR, y = OFFICE_SHARE), color = "red")  

#Proportion of time worked at home
ggplot() +
  geom_point(data = dat1, aes(x = YEAR, y = HOME_SHARE), color = "blue") + 
  geom_point(data = dat2, aes(x = YEAR, y = HOME_SHARE), color = "red")




#DATA CROSS-SECTIONS
#1 GENDER AND AGE
dat1 = data2%>%filter(SEX==1,AGE >= 16 & AGE <= 34)%>%group_by(YEAR)%>%filter(WORK == 1)%>%summarize(HOME_SHARE = 
                                                      sum(HOME)/(sum(HOME) +
                                                      sum(OFFICE) + sum(TRANSPORT) + 
                                                      sum(OTHER_LOC)))
#share of work done at home each year for age group 35-54
dat2 = data2%>%filter(SEX==1,AGE >= 35 & AGE <= 54)%>%group_by(YEAR)%>%filter(WORK == 1)%>%summarize(HOME_SHARE = 
                                                      sum(HOME)/(sum(HOME) +
                                                      sum(OFFICE) + sum(TRANSPORT) + 
                                                      sum(OTHER_LOC)))
#share of work done at home each year for age group 55+
dat3 = data2%>%filter(SEX==1,AGE >= 55)%>%group_by(YEAR)%>%filter(WORK == 1)%>%summarize(HOME_SHARE = 
                                                      sum(HOME)/(sum(HOME) +                                                        
                                                      sum(OFFICE) + sum(TRANSPORT) + 
                                                      sum(OTHER_LOC)))

dat4 = data2%>%filter(SEX==2,AGE >= 16 & AGE <= 34)%>%group_by(YEAR)%>%filter(WORK == 1)%>%summarize(HOME_SHARE = 
                                                      sum(HOME)/(sum(HOME) +
                                                      sum(OFFICE) + sum(TRANSPORT) + 
                                                      sum(OTHER_LOC)))
#share of work done at home each year for age group 35-54
dat5 = data2%>%filter(SEX==2,AGE >= 35 & AGE <= 54)%>%group_by(YEAR)%>%filter(WORK == 1)%>%summarize(HOME_SHARE = 
                                                      sum(HOME)/(sum(HOME) +
                                                      sum(OFFICE) + sum(TRANSPORT) + 
                                                      sum(OTHER_LOC)))
#share of work done at home each year for age group 55+
dat6 = data2%>%filter(SEX==2,AGE >= 55)%>%group_by(YEAR)%>%filter(WORK == 1)%>%summarize(HOME_SHARE = 
                                                      sum(HOME)/(sum(HOME) +                                                        
                                                      sum(OFFICE) + sum(TRANSPORT) + 
                                                      sum(OTHER_LOC)))
#Young men, middle aged men, older men, young women, middle-aged women, older women
ggplot() +
  geom_point(data = dat1, aes(x = YEAR, y = HOME_SHARE), color = "red") + 
  geom_point(data = dat2, aes(x = YEAR, y = HOME_SHARE), color = "orange") + 
  geom_point(data = dat3, aes(x = YEAR, y = HOME_SHARE), color = "green") +
  geom_point(data = dat4, aes(x = YEAR, y = HOME_SHARE), color = "blue") +
  geom_point(data = dat5, aes(x = YEAR, y = HOME_SHARE), color = "purple") + 
  geom_point(data = dat6, aes(x = YEAR, y = HOME_SHARE), color = "black") 

dat1 = data2%>%filter(SEX==1,AGE >= 16 & AGE <= 34)%>%group_by(YEAR)%>%filter(WORK == 1)%>%summarize(OFFICE_SHARE = 
                                                                                                       sum(OFFICE)/(sum(HOME) +
                                                                                                                    sum(OFFICE) + sum(TRANSPORT) + 
                                                                                                                    sum(OTHER_LOC)))
#share of work done at home each year for age group 35-54
dat2 = data2%>%filter(SEX==1,AGE >= 35 & AGE <= 54)%>%group_by(YEAR)%>%filter(WORK == 1)%>%summarize(OFFICE_SHARE = 
                                                                                                       sum(OFFICE)/(sum(HOME) +
                                                                                                                    sum(OFFICE) + sum(TRANSPORT) + 
                                                                                                                    sum(OTHER_LOC)))
#share of work done at home each year for age group 55+
dat3 = data2%>%filter(SEX==1,AGE >= 55)%>%group_by(YEAR)%>%filter(WORK == 1)%>%summarize(OFFICE_SHARE = 
                                                                                           sum(OFFICE)/(sum(HOME) +                                                        
                                                                                                        sum(OFFICE) + sum(TRANSPORT) + 
                                                                                                        sum(OTHER_LOC)))

dat4 = data2%>%filter(SEX==2,AGE >= 16 & AGE <= 34)%>%group_by(YEAR)%>%filter(WORK == 1)%>%summarize(OFFICE_SHARE = 
                                                                                                       sum(OFFICE)/(sum(HOME) +
                                                                                                                    sum(OFFICE) + sum(TRANSPORT) + 
                                                                                                                    sum(OTHER_LOC)))
#share of work done at home each year for age group 35-54
dat5 = data2%>%filter(SEX==2,AGE >= 35 & AGE <= 54)%>%group_by(YEAR)%>%filter(WORK == 1)%>%summarize(OFFICE_SHARE = 
                                                                                                       sum(OFFICE)/(sum(HOME) +
                                                                                                                    sum(OFFICE) + sum(TRANSPORT) + 
                                                                                                                    sum(OTHER_LOC)))
#share of work done at home each year for age group 55+
dat6 = data2%>%filter(SEX==2,AGE >= 55)%>%group_by(YEAR)%>%filter(WORK == 1)%>%summarize(OFFICE_SHARE = 
                                                                                           sum(OFFICE)/(sum(HOME) +                                                        
                                                                                                        sum(OFFICE) + sum(TRANSPORT) + 
                                                                                                        sum(OTHER_LOC)))
#Young men, middle aged men, older men, young women, middle-aged women, older women
ggplot() +
  geom_point(data = dat1, aes(x = YEAR, y = OFFICE_SHARE), color = "red") + 
  geom_point(data = dat2, aes(x = YEAR, y = OFFICE_SHARE), color = "orange") + 
  geom_point(data = dat3, aes(x = YEAR, y = OFFICE_SHARE), color = "green") +
  geom_point(data = dat4, aes(x = YEAR, y = OFFICE_SHARE), color = "blue") +
  geom_point(data = dat5, aes(x = YEAR, y = OFFICE_SHARE), color = "purple") + 
  geom_point(data = dat6, aes(x = YEAR, y = OFFICE_SHARE), color = "black") 

#2 RACE AND EDUCATION
dat1 = data2%>%filter(RACE==100,EDUC<=21)%>%group_by(YEAR)%>%filter(WORK == 1)%>%summarize(HOME_SHARE = 
                                                        sum(HOME)/(sum(HOME) +
                                                        sum(OFFICE) + sum(TRANSPORT) + 
                                                        sum(OTHER_LOC)))
#share of work done at home each year for age group 35-54
dat2 = data2%>%filter(RACE==100,EDUC > 21)%>%group_by(YEAR)%>%filter(WORK == 1)%>%summarize(HOME_SHARE = 
                                                        sum(HOME)/(sum(HOME) +
                                                        sum(OFFICE) + sum(TRANSPORT) + 
                                                        sum(OTHER_LOC)))
#share of work done at home each year for age group 55+
dat3 = data2%>%filter(RACE==110,EDUC <= 21)%>%group_by(YEAR)%>%filter(WORK == 1)%>%summarize(HOME_SHARE = 
                                                        sum(HOME)/(sum(HOME) +                                                        
                                                        sum(OFFICE) + sum(TRANSPORT) + 
                                                        sum(OTHER_LOC)))

dat4 = data2%>%filter(RACE==110,EDUC>21)%>%group_by(YEAR)%>%filter(WORK == 1)%>%summarize(HOME_SHARE = 
                                                        sum(HOME)/(sum(HOME) +
                                                        sum(OFFICE) + sum(TRANSPORT) + 
                                                        sum(OTHER_LOC)))

#WHITE LESS THAN COLLEGE, WHITE AT LEAST COLLEGE, BLACK LESS THAN COLLEGE, BLACK AT LEAST
#COLLEGE
ggplot() +
  geom_point(data = dat1, aes(x = YEAR, y = HOME_SHARE), color = "red") + 
  geom_point(data = dat2, aes(x = YEAR, y = HOME_SHARE), color = "orange") + 
  geom_point(data = dat3, aes(x = YEAR, y = HOME_SHARE), color = "green") +
  geom_point(data = dat4, aes(x = YEAR, y = HOME_SHARE), color = "blue") 

dat1 = data2%>%filter(RACE==100,EDUC<=21)%>%group_by(YEAR)%>%filter(WORK == 1)%>%summarize(OFFICE_SHARE = 
                                                                                             sum(OFFICE)/(sum(HOME) +
                                                                                                          sum(OFFICE) + sum(TRANSPORT) + 
                                                                                                          sum(OTHER_LOC)))
#share of work done at home each year for age group 35-54
dat2 = data2%>%filter(RACE==100,EDUC > 21)%>%group_by(YEAR)%>%filter(WORK == 1)%>%summarize(OFFICE_SHARE = 
                                                                                              sum(OFFICE)/(sum(HOME) +
                                                                                                           sum(OFFICE) + sum(TRANSPORT) + 
                                                                                                           sum(OTHER_LOC)))
#share of work done at home each year for age group 55+
dat3 = data2%>%filter(RACE==110,EDUC <= 21)%>%group_by(YEAR)%>%filter(WORK == 1)%>%summarize(OFFICE_SHARE = 
                                                                                               sum(OFFICE)/(sum(HOME) +                                                        
                                                                                                            sum(OFFICE) + sum(TRANSPORT) + 
                                                                                                            sum(OTHER_LOC)))

dat4 = data2%>%filter(RACE==110,EDUC>21)%>%group_by(YEAR)%>%filter(WORK == 1)%>%summarize(OFFICE_SHARE = 
                                                                                            sum(OFFICE)/(sum(HOME) +
                                                                                                         sum(OFFICE) + sum(TRANSPORT) + 
                                                                                                         sum(OTHER_LOC)))

#WHITE LESS THAN COLLEGE, WHITE AT LEAST COLLEGE, BLACK LESS THAN COLLEGE, BLACK AT LEAST
#COLLEGE
ggplot() +
  geom_point(data = dat1, aes(x = YEAR, y = OFFICE_SHARE), color = "red") + 
  geom_point(data = dat2, aes(x = YEAR, y = OFFICE_SHARE), color = "orange") + 
  geom_point(data = dat3, aes(x = YEAR, y = OFFICE_SHARE), color = "green") +
  geom_point(data = dat4, aes(x = YEAR, y = OFFICE_SHARE), color = "blue") 
#3 EDUCATION AND AGE
dat1 = data2%>%filter(EDUC<=21,AGE >= 16 & AGE <= 34)%>%group_by(YEAR)%>%filter(WORK == 1)%>%summarize(HOME_SHARE = 
                                                        sum(HOME)/(sum(HOME) +
                                                        sum(OFFICE) + sum(TRANSPORT) + 
                                                        sum(OTHER_LOC)))
#share of work done at home each year for age group 35-54
dat2 = data2%>%filter(EDUC<=21,AGE >= 35 & AGE <= 54)%>%group_by(YEAR)%>%filter(WORK == 1)%>%summarize(HOME_SHARE = 
                                                        sum(HOME)/(sum(HOME) +
                                                        sum(OFFICE) + sum(TRANSPORT) + 
                                                        sum(OTHER_LOC)))
#share of work done at home each year for age group 55+
dat3 = data2%>%filter(EDUC<=21,AGE >= 55)%>%group_by(YEAR)%>%filter(WORK == 1)%>%summarize(HOME_SHARE = 
                                                        sum(HOME)/(sum(HOME) +                                                        
                                                        sum(OFFICE) + sum(TRANSPORT) + 
                                                        sum(OTHER_LOC)))

dat4 = data2%>%filter(EDUC>21,AGE >= 16 & AGE <= 34)%>%group_by(YEAR)%>%filter(WORK == 1)%>%summarize(HOME_SHARE = 
                                                        sum(HOME)/(sum(HOME) +
                                                        sum(OFFICE) + sum(TRANSPORT) + 
                                                        sum(OTHER_LOC)))
#share of work done at home each year for age group 35-54
dat5 = data2%>%filter(EDUC>21,AGE >= 35 & AGE <= 54)%>%group_by(YEAR)%>%filter(WORK == 1)%>%summarize(HOME_SHARE = 
                                                        sum(HOME)/(sum(HOME) +
                                                        sum(OFFICE) + sum(TRANSPORT) + 
                                                        sum(OTHER_LOC)))
#share of work done at home each year for age group 55+
dat6 = data2%>%filter(EDUC>21,AGE >= 55)%>%group_by(YEAR)%>%filter(WORK == 1)%>%summarize(HOME_SHARE = 
                                                        sum(HOME)/(sum(HOME) +                                                        
                                                        sum(OFFICE) + sum(TRANSPORT) + 
                                                        sum(OTHER_LOC)))
#Young less than college, middle aged less than college, older less than college,
#young at least college, middle-aged at least college, older at least college
ggplot() +
  geom_point(data = dat1, aes(x = YEAR, y = HOME_SHARE), color = "red") + 
  geom_point(data = dat2, aes(x = YEAR, y = HOME_SHARE), color = "orange") + 
  geom_point(data = dat3, aes(x = YEAR, y = HOME_SHARE), color = "green") +
  geom_point(data = dat4, aes(x = YEAR, y = HOME_SHARE), color = "blue") +
  geom_point(data = dat5, aes(x = YEAR, y = HOME_SHARE), color = "purple") + 
  geom_point(data = dat6, aes(x = YEAR, y = HOME_SHARE), color = "black") 


dat1 = data2%>%filter(EDUC<=21,AGE >= 16 & AGE <= 34)%>%group_by(YEAR)%>%filter(WORK == 1)%>%summarize(OFFICE_SHARE = 
                                                                                                         sum(OFFICE)/(sum(HOME) +
                                                                                                                      sum(OFFICE) + sum(TRANSPORT) + 
                                                                                                                      sum(OTHER_LOC)))
#share of work done at home each year for age group 35-54
dat2 = data2%>%filter(EDUC<=21,AGE >= 35 & AGE <= 54)%>%group_by(YEAR)%>%filter(WORK == 1)%>%summarize(OFFICE_SHARE = 
                                                                                                         sum(OFFICE)/(sum(HOME) +
                                                                                                                      sum(OFFICE) + sum(TRANSPORT) + 
                                                                                                                      sum(OTHER_LOC)))
#share of work done at home each year for age group 55+
dat3 = data2%>%filter(EDUC<=21,AGE >= 55)%>%group_by(YEAR)%>%filter(WORK == 1)%>%summarize(OFFICE_SHARE = 
                                                                                             sum(OFFICE)/(sum(HOME) +                                                        
                                                                                                          sum(OFFICE) + sum(TRANSPORT) + 
                                                                                                          sum(OTHER_LOC)))

dat4 = data2%>%filter(EDUC>21,AGE >= 16 & AGE <= 34)%>%group_by(YEAR)%>%filter(WORK == 1)%>%summarize(OFFICE_SHARE = 
                                                                                                        sum(OFFICE)/(sum(HOME) +
                                                                                                                     sum(OFFICE) + sum(TRANSPORT) + 
                                                                                                                     sum(OTHER_LOC)))
#share of work done at home each year for age group 35-54
dat5 = data2%>%filter(EDUC>21,AGE >= 35 & AGE <= 54)%>%group_by(YEAR)%>%filter(WORK == 1)%>%summarize(OFFICE_SHARE = 
                                                                                                        sum(OFFICE)/(sum(HOME) +
                                                                                                                     sum(OFFICE) + sum(TRANSPORT) + 
                                                                                                                     sum(OTHER_LOC)))
#share of work done at home each year for age group 55+
dat6 = data2%>%filter(EDUC>21,AGE >= 55)%>%group_by(YEAR)%>%filter(WORK == 1)%>%summarize(OFFICE_SHARE = 
                                                                                            sum(OFFICE)/(sum(HOME) +                                                        
                                                                                                         sum(OFFICE) + sum(TRANSPORT) + 
                                                                                                         sum(OTHER_LOC)))
#Young less than college, middle aged less than college, older less than college,
#young at least college, middle-aged at least college, older at least college
ggplot() +
  geom_point(data = dat1, aes(x = YEAR, y = OFFICE_SHARE), color = "red") + 
  geom_point(data = dat2, aes(x = YEAR, y = OFFICE_SHARE), color = "orange") + 
  geom_point(data = dat3, aes(x = YEAR, y = OFFICE_SHARE), color = "green") +
  geom_point(data = dat4, aes(x = YEAR, y = OFFICE_SHARE), color = "blue") +
  geom_point(data = dat5, aes(x = YEAR, y = OFFICE_SHARE), color = "purple") + 
  geom_point(data = dat6, aes(x = YEAR, y = OFFICE_SHARE), color = "black")
#4 GENDER AND EDUCATION
dat1 = data2%>%filter(SEX==1,EDUC<=21)%>%group_by(YEAR)%>%filter(WORK == 1)%>%summarize(HOME_SHARE = 
                                                         sum(HOME)/(sum(HOME) +
                                                         sum(OFFICE) + sum(TRANSPORT) + 
                                                         sum(OTHER_LOC)))
#share of work done at home each year for age group 35-54
dat2 = data2%>%filter(SEX==1,EDUC>21)%>%group_by(YEAR)%>%filter(WORK == 1)%>%summarize(HOME_SHARE = 
                                                         sum(HOME)/(sum(HOME) +
                                                         sum(OFFICE) + sum(TRANSPORT) + 
                                                         sum(OTHER_LOC)))
#share of work done at home each year for age group 55+
dat3 = data2%>%filter(SEX==2, EDUC<=21)%>%group_by(YEAR)%>%filter(WORK == 1)%>%summarize(HOME_SHARE = 
                                                         sum(HOME)/(sum(HOME) +                                                        
                                                         sum(OFFICE) + sum(TRANSPORT) + 
                                                         sum(OTHER_LOC)))

dat4 = data2%>%filter(SEX==2,AGE>21)%>%group_by(YEAR)%>%filter(WORK == 1)%>%summarize(HOME_SHARE = 
                                                         sum(HOME)/(sum(HOME) +
                                                         sum(OFFICE) + sum(TRANSPORT) + 
                                                         sum(OTHER_LOC)))
#MEN LESS THAN COLLEGE, MEN AT LEAST COLLEGE, WOMEN LESS THAN COLLEGE, WOMEN AT LEAST COLLEGE
ggplot() +
  geom_point(data = dat1, aes(x = YEAR, y = HOME_SHARE), color = "red") + 
  geom_point(data = dat2, aes(x = YEAR, y = HOME_SHARE), color = "orange") + 
  geom_point(data = dat3, aes(x = YEAR, y = HOME_SHARE), color = "green") +
  geom_point(data = dat4, aes(x = YEAR, y = HOME_SHARE), color = "blue")

dat1 = data2%>%filter(SEX==1,EDUC<=21)%>%group_by(YEAR)%>%filter(WORK == 1)%>%summarize(OFFICE_SHARE = 
                                                                                          sum(OFFICE)/(sum(HOME) +
                                                                                                       sum(OFFICE) + sum(TRANSPORT) + 
                                                                                                       sum(OTHER_LOC)))
#share of work done at home each year for age group 35-54
dat2 = data2%>%filter(SEX==1,EDUC>21)%>%group_by(YEAR)%>%filter(WORK == 1)%>%summarize(OFFICE_SHARE = 
                                                                                         sum(OFFICE)/(sum(HOME) +
                                                                                                      sum(OFFICE) + sum(TRANSPORT) + 
                                                                                                      sum(OTHER_LOC)))
#share of work done at home each year for age group 55+
dat3 = data2%>%filter(SEX==2, EDUC<=21)%>%group_by(YEAR)%>%filter(WORK == 1)%>%summarize(OFFICE_SHARE = 
                                                                                           sum(OFFICE)/(sum(HOME) +                                                        
                                                                                                        sum(OFFICE) + sum(TRANSPORT) + 
                                                                                                        sum(OTHER_LOC)))

dat4 = data2%>%filter(SEX==2,AGE>21)%>%group_by(YEAR)%>%filter(WORK == 1)%>%summarize(OFFICE_SHARE = 
                                                                                        sum(OFFICE)/(sum(HOME) +
                                                                                                     sum(OFFICE) + sum(TRANSPORT) + 
                                                                                                     sum(OTHER_LOC)))
#MEN LESS THAN COLLEGE, MEN AT LEAST COLLEGE, WOMEN LESS THAN COLLEGE, WOMEN AT LEAST COLLEGE
ggplot() +
  geom_point(data = dat1, aes(x = YEAR, y = OFFICE_SHARE), color = "red") + 
  geom_point(data = dat2, aes(x = YEAR, y = OFFICE_SHARE), color = "orange") + 
  geom_point(data = dat3, aes(x = YEAR, y = OFFICE_SHARE), color = "green") +
  geom_point(data = dat4, aes(x = YEAR, y = OFFICE_SHARE), color = "blue")

#5 GENDER AND RACE
dat1 = data2%>%filter(SEX==1,RACE==100)%>%group_by(YEAR)%>%filter(WORK == 1)%>%summarize(HOME_SHARE = 
                                                          sum(HOME)/(sum(HOME) +
                                                          sum(OFFICE) + sum(TRANSPORT) + 
                                                          sum(OTHER_LOC)))
#share of work done at home each year for age group 35-54
dat2 = data2%>%filter(SEX==1,RACE==110)%>%group_by(YEAR)%>%filter(WORK == 1)%>%summarize(HOME_SHARE = 
                                                          sum(HOME)/(sum(HOME) +
                                                          sum(OFFICE) + sum(TRANSPORT) + 
                                                          sum(OTHER_LOC)))
#share of work done at home each year for age group 55+
dat3 = data2%>%filter(SEX==2, RACE==100)%>%group_by(YEAR)%>%filter(WORK == 1)%>%summarize(HOME_SHARE = 
                                                          sum(HOME)/(sum(HOME) +                                                        
                                                          sum(OFFICE) + sum(TRANSPORT) + 
                                                          sum(OTHER_LOC)))

dat4 = data2%>%filter(SEX==2,RACE==110)%>%group_by(YEAR)%>%filter(WORK == 1)%>%summarize(HOME_SHARE = 
                                                          sum(HOME)/(sum(HOME) +
                                                          sum(OFFICE) + sum(TRANSPORT) + 
                                                          sum(OTHER_LOC)))
#WHITE MEN, BLACK MEN, WHITE WOMEN, BLACK WOMEN
ggplot() +
  geom_point(data = dat1, aes(x = YEAR, y = HOME_SHARE), color = "red") + 
  geom_point(data = dat2, aes(x = YEAR, y = HOME_SHARE), color = "orange") + 
  geom_point(data = dat3, aes(x = YEAR, y = HOME_SHARE), color = "green") +
  geom_point(data = dat4, aes(x = YEAR, y = HOME_SHARE), color = "blue")

dat1 = data2%>%filter(SEX==1,RACE==100)%>%group_by(YEAR)%>%filter(WORK == 1)%>%summarize(OFFICE_SHARE = 
                                                                                           sum(OFFICE)/(sum(HOME) +
                                                                                                        sum(OFFICE) + sum(TRANSPORT) + 
                                                                                                        sum(OTHER_LOC)))
#share of work done at home each year for age group 35-54
dat2 = data2%>%filter(SEX==1,RACE==110)%>%group_by(YEAR)%>%filter(WORK == 1)%>%summarize(OFFICE_SHARE = 
                                                                                           sum(OFFICE)/(sum(HOME) +
                                                                                                        sum(OFFICE) + sum(TRANSPORT) + 
                                                                                                        sum(OTHER_LOC)))
#share of work done at home each year for age group 55+
dat3 = data2%>%filter(SEX==2, RACE==100)%>%group_by(YEAR)%>%filter(WORK == 1)%>%summarize(OFFICE_SHARE = 
                                                                                            sum(OFFICE)/(sum(HOME) +                                                        
                                                                                                         sum(OFFICE) + sum(TRANSPORT) + 
                                                                                                         sum(OTHER_LOC)))

dat4 = data2%>%filter(SEX==2,RACE==110)%>%group_by(YEAR)%>%filter(WORK == 1)%>%summarize(OFFICE_SHARE = 
                                                                                           sum(OFFICE)/(sum(HOME) +
                                                                                                        sum(OFFICE) + sum(TRANSPORT) + 
                                                                                                        sum(OTHER_LOC)))
#WHITE MEN, BLACK MEN, WHITE WOMEN, BLACK WOMEN
ggplot() +
  geom_point(data = dat1, aes(x = YEAR, y = OFFICE_SHARE), color = "red") + 
  geom_point(data = dat2, aes(x = YEAR, y = OFFICE_SHARE), color = "orange") + 
  geom_point(data = dat3, aes(x = YEAR, y = OFFICE_SHARE), color = "green") +
  geom_point(data = dat4, aes(x = YEAR, y = OFFICE_SHARE), color = "blue")


#CONSTRUCTING DATAFRAME WITH SOCIALIZATION TIME SHARES
#Use only rows that have activities and corresponding who's (order not changed)
ACTWHO = data1%>%filter(RECTYPE==3|RECTYPE==4)
#Create a dummy that is 1 when the activity was done with a friend and 0 o.w.
ACTWHO$W_FRIENDS = ifelse(ACTWHO$RELATEW<=408 &
                                 ACTWHO$RELATEW>=403,1,0)
#Get rid or all people who are not friends
ACTWHO = ACTWHO%>%filter(is.na(W_FRIENDS)|W_FRIENDS != 0)
#Replace all null in the W_FRIENDS column with the empty string
ACTWHO$W_FRIENDS[is.na(ACTWHO$W_FRIENDS)]= ""
#Replace all empty strings directly above a friend who record with a 1 - 
#Now every activity done with a friend every friend should have a 1 in this column
ACTWHO = ACTWHO %>%
  mutate(W_FRIENDS = replace(W_FRIENDS, which(W_FRIENDS != "") - 1, NA)) %>%
  tidyr::fill(W_FRIENDS, .direction = "up")
#Get rid of all activities that were not done with friends
#Socializing 4
ACTWHO4=ACTWHO%>%filter(W_FRIENDS==1, ACTIVITY<160000 & ACTIVITY>=110000)
# #Socializing 1
ACTWHO1 = ACTWHO%>%filter(W_FRIENDS==1, ACTIVITY<120300 & ACTIVITY>=120100)
#Socializing 2
ACTWHO2 = ACTWHO%>%filter(W_FRIENDS==1, (ACTIVITY<120300&ACTIVITY>=120100)|
                            (ACTIVITY>=120500&ACTIVITY<120600)|ACTIVITY==129900)
ACTWHO3 = ACTWHO%>%filter(W_FRIENDS==1, ACTIVITY<130000 & ACTIVITY>=120000)
#Socializing 3

# #Replace all nulls in the duration column with zero to prepare for aggregation
# ACTWHO$DURATION[is.na(ACTWHO$DURATION)]= 0
#Create dataframe with person identifiers and duration of time spent w/ friends
friend_dur1 = ACTWHO4%>%group_by(CASEID, YEAR)%>%summarize(TIME_W_FRIENDS4=sum(DURATION)) 
friend_dur2 = ACTWHO1%>%group_by(CASEID, YEAR)%>%summarize(TIME_W_FRIENDS1=sum(DURATION))
friend_dur3 = ACTWHO2%>%group_by(CASEID, YEAR)%>%summarize(TIME_W_FRIENDS2=sum(DURATION))
friend_dur4 = ACTWHO3%>%group_by(CASEID, YEAR)%>%summarize(TIME_W_FRIENDS3=sum(DURATION))
#Merge that dataframe with the primary person dataframe
merged_df = left_join(data3, friend_dur1, by = join_by("CASEID","YEAR"))
merged_df = left_join(merged_df, friend_dur2, by = join_by("CASEID","YEAR"))
merged_df = left_join(merged_df, friend_dur3, by = join_by("CASEID","YEAR"))
merged_df = left_join(merged_df, friend_dur4, by=join_by("CASEID", "YEAR"))
# should I impute nulls with zeroes?
# prevent division by zero
merged_df$SOC_WITH_FRIENDS4 = ifelse(merged_df$SOC_4 == 0, 0,
                                   merged_df$TIME_W_FRIENDS4 / merged_df$SOC_4)
#replace null values with zeroes for aggregation
merged_df$SOC_WITH_FRIENDS4 = ifelse(is.na(merged_df$SOC_WITH_FRIENDS4),0,
                                   merged_df$SOC_WITH_FRIENDS4)
#prevent division by zero
merged_df$SOC_WITH_FRIENDS1 = ifelse(merged_df$SOC_1 == 0, 0,
                                     merged_df$TIME_W_FRIENDS1 / merged_df$SOC_1)
#replace null values with zeroes for aggregation
merged_df$SOC_WITH_FRIENDS1 = ifelse(is.na(merged_df$SOC_WITH_FRIENDS1),0,
                                     merged_df$SOC_WITH_FRIENDS1)
#prevent division by zero
merged_df$SOC_WITH_FRIENDS2 = ifelse(merged_df$SOC_2 == 0, 0,
                                     merged_df$TIME_W_FRIENDS2 / merged_df$SOC_2)
#replace null values with zeroes for aggregation
merged_df$SOC_WITH_FRIENDS2 = ifelse(is.na(merged_df$SOC_WITH_FRIENDS2),0,
                                     merged_df$SOC_WITH_FRIENDS2)
#prevent division by zero
merged_df$SOC_WITH_FRIENDS3 = ifelse(merged_df$SOC_3 == 0, 0,
                                     merged_df$TIME_W_FRIENDS3 / merged_df$SOC_3)
#replace null values with zeroes for aggregation
merged_df$SOC_WITH_FRIENDS3 = ifelse(is.na(merged_df$SOC_WITH_FRIENDS3),0,
                                     merged_df$SOC_WITH_FRIENDS3)
#calculate the mean time spent with friends over all social activities per
#person for each year
merged_df%>%group_by(YEAR)%>%summarize(avgswf = mean(SOC_WITH_FRIENDS4))%>%
  ggplot(aes(x=YEAR, y=avgswf)) + geom_point()
#calculate the mean time spent with friends over all social activities per
#person for each year
merged_df%>%group_by(YEAR)%>%summarize(avgswf = mean(SOC_WITH_FRIENDS1))%>%
  ggplot(aes(x=YEAR, y=avgswf)) + geom_point()
#calculate the mean time spent with friends over all social activities per
#person for each year
merged_df%>%group_by(YEAR)%>%summarize(avgswf = mean(SOC_WITH_FRIENDS2))%>%
  ggplot(aes(x=YEAR, y=avgswf)) + geom_point()
merged_df%>%group_by(YEAR)%>%summarize(avgswf = mean(SOC_WITH_FRIENDS3))%>%
  ggplot(aes(x=YEAR, y=avgswf)) + geom_point()




#convert the activity variable from numeric to character for text analysis
data2$ACTIVITY<-as.character(data2$ACTIVITY)
#Use text analysis to find only work activities and label them with a 1
data2$WORK = ifelse(str_detect(data2$ACTIVITY, '^501'),1,0)
#Find activities that were both work and done at home and label them with a 1
data2$WORK_FROM_HOME = ifelse(data2$WORK == 1 & 
                               data2$WHERE == 101,1,0)
#take duration, caseid, and year columns for activities where a person worked
#from home
data4 = data2%>%filter(WORK_FROM_HOME==1)%>%select(DURATION,CASEID,YEAR)
#Group the data by caseid and year and then add a column that sums the
#duration of time spent working from home for each different combination of
#caseid and year
data4 = data4%>%group_by(CASEID,YEAR)%>%summarize(wfmdur = sum(DURATION))
#Join these three columns with the person data by caseid and year
merged_df = left_join(merged_df, data4, by = join_by("CASEID","YEAR"))
#Replace the null values in the column that indicates the duration of time
#a given person worked from home, with zeroes
merged_df$wfmdur = ifelse(is.na(merged_df$wfmdur),0,merged_df$wfmdur)
#Calculate the proportion of time each person spent working from home
#versus not working from home
merged_df$wfhfraction = ifelse(merged_df$WORKING==0, 0,
                               merged_df$wfmdur/merged_df$WORKING)

#Find the average time spent working from home for each year and put it in
#a dataframe
#Graph average time people in the dataset worked from home versus year
#(many people had no work data and were imputed with zeros)
merged_df%>%group_by(YEAR)%>%summarize(avgwfh = mean(wfhfraction))%>%
  ggplot(aes(x=YEAR, y=avgwfh)) + geom_point()


#COMPARING TIME SPENT DOING SOCIAL ACTIVITIES WITH FRIENDS AMONGST DEMOGRAPHIC GROUPS
merged_df$TIME_W_FRIENDS4 = ifelse(is.na(merged_df$TIME_W_FRIENDS4),0,
                                     merged_df$TIME_W_FRIENDS4)
#AGE
dat1 = merged_df%>%filter(AGE>=16 & AGE <=34)%>%group_by(YEAR)%>%summarize(avgswf = mean(SOC_WITH_FRIENDS4))
dat2 = merged_df%>%filter(AGE>=35 & AGE <=54)%>%group_by(YEAR)%>%summarize(avgswf = mean(SOC_WITH_FRIENDS4))
dat3 = merged_df%>%filter(AGE>=55)%>%group_by(YEAR)%>%summarize(avgswf = mean(SOC_WITH_FRIENDS4))

ggplot() +
  geom_point(data = dat1, aes(x = YEAR, y = avgswf), color = "red") + 
  geom_point(data = dat2, aes(x = YEAR, y = avgswf), color = "blue") + 
  geom_point(data = dat3, aes(x = YEAR, y = avgswf), color = "green")
#RACE
dat1 = merged_df%>%filter(RACE == 100)%>%group_by(YEAR)%>%summarize(avgswf = mean(SOC_WITH_FRIENDS4))
dat2 = merged_df%>%filter(RACE == 110)%>%group_by(YEAR)%>%summarize(avgswf = mean(SOC_WITH_FRIENDS4))

ggplot() +
  geom_point(data = dat1, aes(x = YEAR, y = avgswf), color = "red") + 
  geom_point(data = dat2, aes(x = YEAR, y = avgswf), color = "green")
#EDUCATION
dat1 = merged_df%>%filter(EDUC <= 21)%>%group_by(YEAR)%>%summarize(avgswf = mean(SOC_WITH_FRIENDS4))
dat2 = merged_df%>%filter(EDUC > 21)%>%group_by(YEAR)%>%summarize(avgswf = mean(SOC_WITH_FRIENDS4))
ggplot() +
  geom_point(data = dat1, aes(x = YEAR, y = avgswf), color = "red") + 
  geom_point(data = dat2, aes(x = YEAR, y = avgswf), color = "green")
#GENDER
dat1 = merged_df%>%filter(SEX == 1)%>%group_by(YEAR)%>%summarize(avgswf = mean(SOC_WITH_FRIENDS4))
dat2 = merged_df%>%filter(SEX == 2)%>%group_by(YEAR)%>%summarize(avgswf = mean(SOC_WITH_FRIENDS4))
ggplot() +
  geom_point(data = dat1, aes(x = YEAR, y = avgswf), color = "red") + 
  geom_point(data = dat2, aes(x = YEAR, y = avgswf), color = "green")


merged_df$TIME_W_FRIENDS1 = ifelse(is.na(merged_df$TIME_W_FRIENDS1),0,
                                   merged_df$TIME_W_FRIENDS1)
merged_df$TIME_W_FRIENDS2 = ifelse(is.na(merged_df$TIME_W_FRIENDS2),0,
                                   merged_df$TIME_W_FRIENDS2)
merged_df$TIME_W_FRIENDS3 = ifelse(is.na(merged_df$TIME_W_FRIENDS3),0,
                                   merged_df$TIME_W_FRIENDS3)

write.csv(merged_df,"C:/Users/jmca9/Documents/worksoc.csv", row.names = FALSE)

