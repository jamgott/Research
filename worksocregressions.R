library(readr)
library(tidyverse)
library(stargazer)
rm(list = ls())
setwd("C:/Users/jmca9/Documents")
data <- read_csv("worksoc.csv")
 
data = data%>%filter(AGE>=18 & AGE<=64)
# data = data%>%filter(WORKING > 0)
data = data%>%filter(WORKING>=240)

#Create foreign born dummy (should Puerto Rico be considered foreign?)
data$for_born = ifelse(data$BPL==9900,0,1)

#Turn sex into dummy (1 IS MAN, 0 IS WOMAN)
data$MALE = ifelse(data$SEX==1,1,0)

#Turn HISPAN into dummy (1 is hispanic, 0 is not. May need to check for 9999 NIU)
data$hispanic = ifelse(data$HISPAN == 100,0,1)

#Create a dummy variable that indicates whether or not a person is white
data$WHITE = ifelse(data$RACE == 100,1,0)

#Create a dummy variable that indicates whether or not a person is black
data$BLACK = ifelse(data$RACE == 110,1,0)

#Create a dummy variable that indicates whether or not a person is Asian
data$ASIAN = ifelse(data$RACE >=130 & data$RACE <= 132, 1,0)

#Create a dummy variable that indicates whether or not a person is of another race
data$OTHER_RACE = ifelse(data$RACE >= 200,1,0)

#Create a dummy variable that indicates whether or not a person is married
data$MARRIED = ifelse(data$MARST == 1 | data$MARST == 2, 1,0)

#Create a dummy variable that indicates whether or not a person is divorced/separated
data$DIV_SEP = ifelse(data$MARST == 4 | data$MARST == 5, 1,0)

#Create a dummy variable that indicates whether or not a person is widowed
data$WIDOWED = ifelse(data$MARST == 3, 1,0)

#Create a dummy variable that indicates whether or not a person is never married/single
data$NEVMAR_SING = ifelse(data$MARST == 6 | data$MARST == 99, 1,0)
#is NIU single? Should we include single in this or will there be multicollinearity


#Create a dummy that indicates whether or not a person's youngest child is <=6
data$YNGCHLEQ6 = ifelse(data$YNGCH <= 6,1,0)

#Create a dummy that indicates whether or not a person's youngest child is 7-18
data$YNGCH7_18 = ifelse(data$YNGCH <= 18 & data$YNGCH >=7,1,0)

#Create a dummy that indicates whether or not a person's youngest child is >18
data$YNGCHG18 = ifelse(data$YNGCH > 18 & data$YNGCH != 99, 1,0)

#Create a dummy that indicates whether or not a person has no child (1) or at least 
#one child (0)
data$NOCHILD = ifelse(data$YNGCH == 99,1,0)

#Create a dummy that indicates whether or not a person dropped out before high school diploma
data$NDDROPOUT = ifelse(data$EDUC <= 17,1,0)

#Create a dummy that indicates whether or not a person has finished high school
#but not gone to college
data$HSGRAD = ifelse(data$EDUC == 20 | data$EDUC == 21,1,0)

#Create a dummy that indicates whether or not a person has done some college
data$SOMECOL = ifelse(data$EDUC <= 32 & data$EDUC >= 30,1,0)

#Create a dummy that indicates whether or not a person has finished college
#and gone no further
data$COLFINISHED = ifelse(data$EDUC == 40,1,0)

#Create a dummy that indicates whether or not a person has a college degree and
#more education afterwards
data$COLPLUS = ifelse(data$EDUC <= 43 & data$EDUC >= 41,1,0)

#Create a dummy that indicates whether or not the day is a weekend or a holiday
data$WEEKEND_HOLIDAY = ifelse((data$DAY==1 | data$DAY==7) | data$HOLIDAY == 1,1,0)

#Create a dummy that indicates whether or not it is pre-covid or post-covid(1) (POST MARCH 2020)
data$POST_COVID = ifelse(data$YEAR >= 2020,1,0)

#Create a dummy that is 1 if a person performs any work from home and 0 otherwise
data$ANY_wfh = ifelse(data$wfmdur > 0, 1,0)

#identical(data[['TIME_W_FRIENDS1']],data[['TIME_W_FRIENDS2']]) SOC1 AND SOC 2
#NOT ALWAYS THE SAME. EX. SERIAL NO 370

#REGRESSIONS WITH NO CONTROLS OTHER THAN FIXED EFFECTS FOR EACH MEASURE OF SOCIALITY
#AND WORK FROM HOME DURATION
reg1 = lm(TIME_W_FRIENDS1 ~ wfmdur + factor(YEAR)+factor(MONTH)+
            factor(WEEKEND_HOLIDAY) + WORKING,data=data)
#summary(reg1)

# reg2 = lm(TIME_W_FRIENDS2 ~ wfmdur + factor(YEAR)+factor(MONTH)+
#             factor(WEEKEND_HOLIDAY),data=data)
#summary(reg2)

reg3 = lm(TIME_W_FRIENDS3 ~ wfmdur + factor(YEAR)+factor(MONTH)+
            factor(WEEKEND_HOLIDAY) + WORKING,data=data)
#summary(reg3)

reg4 = lm(TIME_W_FRIENDS4 ~ wfmdur + factor(YEAR)+factor(MONTH)+
            factor(WEEKEND_HOLIDAY) + WORKING,data=data)
#summary(reg4)
#REGRESSIONS WITH NO CONTROLS OTHER THAN FIXED EFFECTS FOR EACH MEASURE OF
#SOCIALITY AND WORK FROM HOME PROPORTION
reg5 = lm(TIME_W_FRIENDS1 ~ wfhfraction + factor(YEAR)+factor(MONTH)+
            factor(WEEKEND_HOLIDAY),data=data)
#summary(reg5)

# reg6 = lm(TIME_W_FRIENDS2 ~ wfhfraction + factor(YEAR)+factor(MONTH)+
#             factor(WEEKEND_HOLIDAY),data=data)
#summary(reg6)

reg7 = lm(TIME_W_FRIENDS3 ~ wfhfraction + factor(YEAR)+factor(MONTH)+
            factor(WEEKEND_HOLIDAY),data=data)
#summary(reg7)

reg8 = lm(TIME_W_FRIENDS4 ~ wfhfraction + factor(YEAR)+factor(MONTH)+
            factor(WEEKEND_HOLIDAY),data=data)
#summary(reg8)

#REGRESSIONS WITH NO CONTROLS OTHER THAN FIXED EFFECTS FOR EACH MEASURE OF SOCIALITY
#AND WORK FROM HOME DURATION interacted with post covid
reg9 = lm(TIME_W_FRIENDS1 ~ wfmdur * POST_COVID + factor(YEAR)+factor(MONTH)+
            factor(WEEKEND_HOLIDAY) + WORKING,data=data)
#summary(reg9)

# reg10 = lm(TIME_W_FRIENDS2 ~ wfmdur * POST_COVID + factor(YEAR)+factor(MONTH)+
#             factor(WEEKEND_HOLIDAY),data=data)
#summary(reg10)

reg11 = lm(TIME_W_FRIENDS3 ~ wfmdur * POST_COVID + factor(YEAR)+factor(MONTH)+
            factor(WEEKEND_HOLIDAY) + WORKING,data=data)
#summary(reg11)

reg12 = lm(TIME_W_FRIENDS4 ~ wfmdur * POST_COVID + factor(YEAR)+factor(MONTH)+
            factor(WEEKEND_HOLIDAY) + WORKING,data=data)
#summary(reg12)
#REGRESSIONS WITH NO CONTROLS OTHER THAN FIXED EFFECTS FOR EACH MEASURE OF
#SOCIALITY AND WORK FROM HOME PROPORTION INTERACTED WITH POST COVID
reg13 = lm(TIME_W_FRIENDS1 ~ wfhfraction * POST_COVID + factor(YEAR)+factor(MONTH)+
            factor(WEEKEND_HOLIDAY),data=data)
#summary(reg13)
# 
# reg14 = lm(TIME_W_FRIENDS2 ~ wfhfraction * POST_COVID + factor(YEAR)+factor(MONTH)+
#             factor(WEEKEND_HOLIDAY),data=data)
#summary(reg14)

reg15 = lm(TIME_W_FRIENDS3 ~ wfhfraction * POST_COVID + factor(YEAR)+factor(MONTH)+
            factor(WEEKEND_HOLIDAY),data=data)
#summary(reg15)

reg16 = lm(TIME_W_FRIENDS4 ~ wfhfraction * POST_COVID + factor(YEAR)+factor(MONTH)+
            factor(WEEKEND_HOLIDAY),data=data)
#summary(reg16)

#DO ALL OF THESE AGAIN WITH ALL CONTROLS AND THEN SPLIT THE SAMPLE UP BETWEEN MEN
#WOMEN
reg17 = lm(TIME_W_FRIENDS1 ~ wfmdur + factor(YEAR)+factor(MONTH)+
            factor(WEEKEND_HOLIDAY) + AGE + I(AGE^2) + MALE + HSGRAD + SOMECOL + 
            COLFINISHED + COLPLUS + WHITE + BLACK + ASIAN + hispanic + for_born + 
            MARRIED + DIV_SEP + WIDOWED + YNGCHG18 + YNGCH7_18 + YNGCHLEQ6 + WEEKEND_HOLIDAY + WORKING,data=data)
#summary(reg17)

# reg18 = lm(TIME_W_FRIENDS2 ~ wfmdur + factor(YEAR)+factor(MONTH)+
#              + AGE + I(AGE^2) + MALE + HSGRAD + SOMECOL +
#             COLFINISHED + COLPLUS + WHITE + BLACK + ASIAN + hispanic + for_born + 
#             MARRIED + DIV_SEP + WIDOWED + YNGCHG18 + YNGCH7_18 + YNGCHLEQ6 + WEEKEND_HOLIDAY + WORKING,data=data)
# #summary(reg18)

reg19 = lm(TIME_W_FRIENDS3 ~ wfmdur + factor(YEAR)+factor(MONTH)+
             + AGE + I(AGE^2) + MALE + HSGRAD + SOMECOL +     
            COLFINISHED + COLPLUS + WHITE + BLACK + ASIAN + hispanic + for_born + 
            MARRIED + DIV_SEP + WIDOWED + YNGCHG18 + YNGCH7_18 + YNGCHLEQ6 + WEEKEND_HOLIDAY + WORKING,data=data)
#summary(reg19)

reg20 = lm(TIME_W_FRIENDS4 ~ wfmdur + factor(YEAR)+factor(MONTH)+
             + AGE + I(AGE^2) + MALE + HSGRAD + SOMECOL +      
            COLFINISHED + COLPLUS + WHITE + BLACK + ASIAN + hispanic + for_born + 
            MARRIED + DIV_SEP + WIDOWED + YNGCHG18 + YNGCH7_18 + YNGCHLEQ6 + WEEKEND_HOLIDAY + WORKING,data=data)
#summary(reg20)
#REGRESSIONS WITH NO CONTROLS OTHER THAN FIXED EFFECTS FOR EACH MEASURE OF
#SOCIALITY AND WORK FROM HOME PROPORTION
reg21 = lm(TIME_W_FRIENDS1 ~ wfhfraction + factor(YEAR)+factor(MONTH)+
             + AGE + I(AGE^2) + MALE + HSGRAD + SOMECOL +
            COLFINISHED + COLPLUS + WHITE + BLACK + ASIAN + hispanic + for_born + 
            MARRIED + DIV_SEP + WIDOWED + YNGCHG18 + YNGCH7_18 + YNGCHLEQ6 + WEEKEND_HOLIDAY,data=data)
#summary(reg21)

# reg22 = lm(TIME_W_FRIENDS2 ~ wfhfraction + factor(YEAR)+factor(MONTH)+
#              + AGE + I(AGE^2) + MALE + HSGRAD + SOMECOL + 
#             COLFINISHED + COLPLUS + WHITE + BLACK + ASIAN + hispanic + for_born + 
#             MARRIED + DIV_SEP + WIDOWED + YNGCHG18 + YNGCH7_18 + YNGCHLEQ6 + WEEKEND_HOLIDAY,data=data)
# #summary(reg22)

reg23 = lm(TIME_W_FRIENDS3 ~ wfhfraction + factor(YEAR)+factor(MONTH)+
             + AGE + I(AGE^2) + MALE + HSGRAD + SOMECOL + 
            COLFINISHED + COLPLUS + WHITE + BLACK + ASIAN + hispanic + for_born + 
            MARRIED + DIV_SEP + WIDOWED + YNGCHG18 + YNGCH7_18 + YNGCHLEQ6 + WEEKEND_HOLIDAY,data=data)
#summary(reg23)

reg24 = lm(TIME_W_FRIENDS4 ~ wfhfraction + factor(YEAR)+factor(MONTH)+
             + AGE + I(AGE^2) + MALE + HSGRAD + SOMECOL +      
            COLFINISHED + COLPLUS + WHITE + BLACK + ASIAN + hispanic + for_born +   
            MARRIED + DIV_SEP + WIDOWED + YNGCHG18 + YNGCH7_18 + YNGCHLEQ6 + WEEKEND_HOLIDAY,data=data)
#summary(reg24)

#REGRESSIONS WITH NO CONTROLS OTHER THAN FIXED EFFECTS FOR EACH MEASURE OF SOCIALITY
#AND WORK FROM HOME DURATION interacted with post covid
reg25 = lm(TIME_W_FRIENDS1 ~ wfmdur * POST_COVID + factor(YEAR)+factor(MONTH)+
             + AGE + I(AGE^2) + MALE + HSGRAD + SOMECOL +        
            COLFINISHED + COLPLUS + WHITE + BLACK + ASIAN + hispanic + for_born +    
            MARRIED + DIV_SEP + WIDOWED + YNGCHG18 + YNGCH7_18 + YNGCHLEQ6 + WEEKEND_HOLIDAY + WORKING,data=data)
#summary(reg25)

# reg26 = lm(TIME_W_FRIENDS2 ~ wfmdur * POST_COVID + factor(YEAR)+factor(MONTH)+
#               + AGE + I(AGE^2) + MALE + HSGRAD + SOMECOL +  
#              COLFINISHED + COLPLUS + WHITE + BLACK + ASIAN + hispanic + for_born +  
#              MARRIED + DIV_SEP + WIDOWED + YNGCHG18 + YNGCH7_18 + YNGCHLEQ6 + WEEKEND_HOLIDAY + WORKING,data=data)
# #summary(reg26)

reg27 = lm(TIME_W_FRIENDS3 ~ wfmdur * POST_COVID + factor(YEAR)+factor(MONTH)+
              + AGE + I(AGE^2) + MALE + HSGRAD + SOMECOL +       
             COLFINISHED + COLPLUS + WHITE + BLACK + ASIAN + hispanic + for_born + 
             MARRIED + DIV_SEP + WIDOWED + YNGCHG18 + YNGCH7_18 + YNGCHLEQ6 + WEEKEND_HOLIDAY + WORKING,data=data)
#summary(reg27)

reg28 = lm(TIME_W_FRIENDS4 ~ wfmdur * POST_COVID + factor(YEAR)+factor(MONTH)+
              + AGE + I(AGE^2) + MALE + HSGRAD + SOMECOL +          
             COLFINISHED + COLPLUS + WHITE + BLACK + ASIAN + hispanic + for_born + 
             MARRIED + DIV_SEP + WIDOWED + YNGCHG18 + YNGCH7_18 + YNGCHLEQ6 + WEEKEND_HOLIDAY + WORKING,data=data)
#summary(reg28)
#REGRESSIONS WITH NO CONTROLS OTHER THAN FIXED EFFECTS FOR EACH MEASURE OF
#SOCIALITY AND WORK FROM HOME PROPORTION INTERACTED WITH POST COVID
reg29 = lm(TIME_W_FRIENDS1 ~ wfhfraction * POST_COVID + factor(YEAR)+factor(MONTH)+
              + AGE + I(AGE^2) + MALE + HSGRAD + SOMECOL +   
             COLFINISHED + COLPLUS + WHITE + BLACK + ASIAN + hispanic + for_born +   
             MARRIED + DIV_SEP + WIDOWED + YNGCHG18 + YNGCH7_18 + YNGCHLEQ6 + WEEKEND_HOLIDAY,data=data)
#summary(reg29)

# reg30 = lm(TIME_W_FRIENDS2 ~ wfhfraction * POST_COVID + factor(YEAR)+factor(MONTH)+
#               + AGE + I(AGE^2) + MALE + HSGRAD + SOMECOL +     
#              COLFINISHED + COLPLUS + WHITE + BLACK + ASIAN + hispanic + for_born +  
#              MARRIED + DIV_SEP + WIDOWED + YNGCHG18 + YNGCH7_18 + YNGCHLEQ6 + WEEKEND_HOLIDAY,data=data)
# #summary(reg30)

reg31 = lm(TIME_W_FRIENDS3 ~ wfhfraction * POST_COVID + factor(YEAR)+factor(MONTH)+
              + AGE + I(AGE^2) + MALE + HSGRAD + SOMECOL +   
             COLFINISHED + COLPLUS + WHITE + BLACK + ASIAN + hispanic + for_born +  
             MARRIED + DIV_SEP + WIDOWED + YNGCHG18 + YNGCH7_18 + YNGCHLEQ6 + WEEKEND_HOLIDAY,data=data)
#summary(reg31)

reg32 = lm(TIME_W_FRIENDS4 ~ wfhfraction * POST_COVID + factor(YEAR)+factor(MONTH)+
              + AGE + I(AGE^2) + MALE + HSGRAD + SOMECOL +    
             COLFINISHED + COLPLUS + WHITE + BLACK + ASIAN + hispanic + for_born +
             MARRIED + DIV_SEP + WIDOWED + YNGCHG18 + YNGCH7_18 + YNGCHLEQ6 + WEEKEND_HOLIDAY,data=data)
#summary(reg32)

#REGRESSIONS WITH NO CONTROLS OTHER THAN FIXED EFFECTS FOR EACH MEASURE OF SOCIALITY
#AND WORK FROM HOME DURATION
reg33 = lm(TIME_W_FRIENDS1 ~ wfmdur + factor(YEAR)+factor(MONTH)+
            factor(WEEKEND_HOLIDAY) + WORKING,data=data%>%filter(MALE==1))
#summary(reg33)
# 
# reg34 = lm(TIME_W_FRIENDS2 ~ wfmdur + factor(YEAR)+factor(MONTH)+
#             factor(WEEKEND_HOLIDAY) + WORKING,data=data%>%filter(MALE==1))
# #summary(reg34)

reg35 = lm(TIME_W_FRIENDS3 ~ wfmdur + factor(YEAR)+factor(MONTH)+
            factor(WEEKEND_HOLIDAY) + WORKING,data=data%>%filter(MALE==1))
#summary(reg35)

reg36 = lm(TIME_W_FRIENDS4 ~ wfmdur + factor(YEAR)+factor(MONTH)+
            factor(WEEKEND_HOLIDAY) + WORKING,data=data%>%filter(MALE==1))
#summary(reg36)
#REGRESSIONS WITH NO CONTROLS OTHER THAN FIXED EFFECTS FOR EACH MEASURE OF
#SOCIALITY AND WORK FROM HOME PROPORTION
reg37 = lm(TIME_W_FRIENDS1 ~ wfhfraction + factor(YEAR)+factor(MONTH)+
            factor(WEEKEND_HOLIDAY),data=data%>%filter(MALE==1))
#summary(reg37)

# reg38 = lm(TIME_W_FRIENDS2 ~ wfhfraction + factor(YEAR)+factor(MONTH)+
#             factor(WEEKEND_HOLIDAY),data=data%>%filter(MALE==1))
# #summary(reg38)

reg39 = lm(TIME_W_FRIENDS3 ~ wfhfraction + factor(YEAR)+factor(MONTH)+
            factor(WEEKEND_HOLIDAY),data=data%>%filter(MALE==1))
#summary(reg39)

reg40 = lm(TIME_W_FRIENDS4 ~ wfhfraction + factor(YEAR)+factor(MONTH)+
            factor(WEEKEND_HOLIDAY),data=data%>%filter(MALE==1))
#summary(reg40)

#REGRESSIONS WITH NO CONTROLS OTHER THAN FIXED EFFECTS FOR EACH MEASURE OF SOCIALITY
#AND WORK FROM HOME DURATION interacted with post covid
reg41 = lm(TIME_W_FRIENDS1 ~ wfmdur * POST_COVID + factor(YEAR)+factor(MONTH)+
            factor(WEEKEND_HOLIDAY) + WORKING,data=data%>%filter(MALE==1))
#summary(reg41)

# reg42 = lm(TIME_W_FRIENDS2 ~ wfmdur * POST_COVID + factor(YEAR)+factor(MONTH)+
#              factor(WEEKEND_HOLIDAY) + WORKING,data=data%>%filter(MALE==1))
# #summary(reg42)

reg43 = lm(TIME_W_FRIENDS3 ~ wfmdur * POST_COVID + factor(YEAR)+factor(MONTH)+
             factor(WEEKEND_HOLIDAY) + WORKING,data=data%>%filter(MALE==1))
#summary(reg43)

reg44 = lm(TIME_W_FRIENDS4 ~ wfmdur * POST_COVID + factor(YEAR)+factor(MONTH)+
             factor(WEEKEND_HOLIDAY) + WORKING,data=data%>%filter(MALE==1))
#summary(reg44)
#REGRESSIONS WITH NO CONTROLS OTHER THAN FIXED EFFECTS FOR EACH MEASURE OF
#SOCIALITY AND WORK FROM HOME PROPORTION INTERACTED WITH POST COVID
reg45 = lm(TIME_W_FRIENDS1 ~ wfhfraction * POST_COVID + factor(YEAR)+factor(MONTH)+
             factor(WEEKEND_HOLIDAY),data=data%>%filter(MALE==1))
#summary(reg45)

# reg46 = lm(TIME_W_FRIENDS2 ~ wfhfraction * POST_COVID + factor(YEAR)+factor(MONTH)+
#              factor(WEEKEND_HOLIDAY),data=data%>%filter(MALE==1))
# #summary(reg46)

reg47 = lm(TIME_W_FRIENDS3 ~ wfhfraction * POST_COVID + factor(YEAR)+factor(MONTH)+
             factor(WEEKEND_HOLIDAY),data=data%>%filter(MALE==1))
#summary(reg47)

reg48 = lm(TIME_W_FRIENDS4 ~ wfhfraction * POST_COVID + factor(YEAR)+factor(MONTH)+
             factor(WEEKEND_HOLIDAY),data=data%>%filter(MALE==1))
#summary(reg48)

#DO ALL OF THESE AGAIN WITH ALL CONTROLS AND THEN SPLIT THE SAMPLE UP BETWEEN MEN
#WOMEN
reg49 = lm(TIME_W_FRIENDS1 ~ wfmdur + factor(YEAR)+factor(MONTH)+
             factor(WEEKEND_HOLIDAY) + AGE + I(AGE^2) + MALE + HSGRAD + SOMECOL + 
             COLFINISHED + COLPLUS + WHITE + BLACK + ASIAN + hispanic + for_born + 
             MARRIED + DIV_SEP + WIDOWED + YNGCHG18 + YNGCH7_18 + YNGCHLEQ6 + WEEKEND_HOLIDAY + WORKING,data=data%>%filter(MALE==1))
#summary(reg49)

# reg50 = lm(TIME_W_FRIENDS2 ~ wfmdur + factor(YEAR)+factor(MONTH)+
#              + AGE + I(AGE^2) + MALE + HSGRAD + SOMECOL +
#              COLFINISHED + COLPLUS + WHITE + BLACK + ASIAN + hispanic + for_born + 
#              MARRIED + DIV_SEP + WIDOWED + YNGCHG18 + YNGCH7_18 + YNGCHLEQ6 + WEEKEND_HOLIDAY + WORKING,data=data%>%filter(MALE==1))
# #summary(reg50)

reg51 = lm(TIME_W_FRIENDS3 ~ wfmdur + factor(YEAR)+factor(MONTH)+
             + AGE + I(AGE^2) + MALE + HSGRAD + SOMECOL +     
             COLFINISHED + COLPLUS + WHITE + BLACK + ASIAN + hispanic + for_born + 
             MARRIED + DIV_SEP + WIDOWED + YNGCHG18 + YNGCH7_18 + YNGCHLEQ6 + WEEKEND_HOLIDAY + WORKING,data=data%>%filter(MALE==1))
#summary(reg51)

reg52 = lm(TIME_W_FRIENDS4 ~ wfmdur + factor(YEAR)+factor(MONTH)+
             + AGE + I(AGE^2) + MALE + HSGRAD + SOMECOL +      
             COLFINISHED + COLPLUS + WHITE + BLACK + ASIAN + hispanic + for_born + 
             MARRIED + DIV_SEP + WIDOWED + YNGCHG18 + YNGCH7_18 + YNGCHLEQ6 + WEEKEND_HOLIDAY + WORKING,data=data%>%filter(MALE==1))
#summary(reg52)
#REGRESSIONS WITH NO CONTROLS OTHER THAN FIXED EFFECTS FOR EACH MEASURE OF
#SOCIALITY AND WORK FROM HOME PROPORTION
reg53 = lm(TIME_W_FRIENDS1 ~ wfhfraction + factor(YEAR)+factor(MONTH)+
             + AGE + I(AGE^2) + MALE + HSGRAD + SOMECOL +
             COLFINISHED + COLPLUS + WHITE + BLACK + ASIAN + hispanic + for_born + 
             MARRIED + DIV_SEP + WIDOWED + YNGCHG18 + YNGCH7_18 + YNGCHLEQ6 + WEEKEND_HOLIDAY,data=data%>%filter(MALE==1))
#summary(reg53)

# reg54 = lm(TIME_W_FRIENDS2 ~ wfhfraction + factor(YEAR)+factor(MONTH)+
#              + AGE + I(AGE^2) + MALE + HSGRAD + SOMECOL + 
#              COLFINISHED + COLPLUS + WHITE + BLACK + ASIAN + hispanic + for_born + 
#              MARRIED + DIV_SEP + WIDOWED + YNGCHG18 + YNGCH7_18 + YNGCHLEQ6 + WEEKEND_HOLIDAY,data=data%>%filter(MALE==1))
# #summary(reg54)

reg55 = lm(TIME_W_FRIENDS3 ~ wfhfraction + factor(YEAR)+factor(MONTH)+
             + AGE + I(AGE^2) + MALE + HSGRAD + SOMECOL + 
             COLFINISHED + COLPLUS + WHITE + BLACK + ASIAN + hispanic + for_born + 
             MARRIED + DIV_SEP + WIDOWED + YNGCHG18 + YNGCH7_18 + YNGCHLEQ6 + WEEKEND_HOLIDAY,data=data%>%filter(MALE==1))
#summary(reg55)

reg56 = lm(TIME_W_FRIENDS4 ~ wfhfraction + factor(YEAR)+factor(MONTH)+
             + AGE + I(AGE^2) + MALE + HSGRAD + SOMECOL +      
             COLFINISHED + COLPLUS + WHITE + BLACK + ASIAN + hispanic + for_born +   
             MARRIED + DIV_SEP + WIDOWED + YNGCHG18 + YNGCH7_18 + YNGCHLEQ6 + WEEKEND_HOLIDAY,data=data%>%filter(MALE==1))
#summary(reg56)

#REGRESSIONS WITH NO CONTROLS OTHER THAN FIXED EFFECTS FOR EACH MEASURE OF SOCIALITY
#AND WORK FROM HOME DURATION interacted with post covid
reg57 = lm(TIME_W_FRIENDS1 ~ wfmdur * POST_COVID + factor(YEAR)+factor(MONTH)+
             + AGE + I(AGE^2) + MALE + HSGRAD + SOMECOL +        
             COLFINISHED + COLPLUS + WHITE + BLACK + ASIAN + hispanic + for_born +    
             MARRIED + DIV_SEP + WIDOWED + YNGCHG18 + YNGCH7_18 + YNGCHLEQ6 + WEEKEND_HOLIDAY + WORKING,data=data%>%filter(MALE==1))
#summary(reg57)

# reg58 = lm(TIME_W_FRIENDS2 ~ wfmdur * POST_COVID + factor(YEAR)+factor(MONTH)+
#              + AGE + I(AGE^2) + MALE + HSGRAD + SOMECOL +  
#              COLFINISHED + COLPLUS + WHITE + BLACK + ASIAN + hispanic + for_born +  
#              MARRIED + DIV_SEP + WIDOWED + YNGCHG18 + YNGCH7_18 + YNGCHLEQ6 + WEEKEND_HOLIDAY + WORKING,data=data%>%filter(MALE==1))
# #summary(reg58)

reg59 = lm(TIME_W_FRIENDS3 ~ wfmdur * POST_COVID + factor(YEAR)+factor(MONTH)+
             + AGE + I(AGE^2) + MALE + HSGRAD + SOMECOL +       
             COLFINISHED + COLPLUS + WHITE + BLACK + ASIAN + hispanic + for_born + 
             MARRIED + DIV_SEP + WIDOWED + YNGCHG18 + YNGCH7_18 + YNGCHLEQ6 + WEEKEND_HOLIDAY + WORKING,data=data%>%filter(MALE==1))
#summary(reg59)

reg60 = lm(TIME_W_FRIENDS4 ~ wfmdur * POST_COVID + factor(YEAR)+factor(MONTH)+
             + AGE + I(AGE^2) + MALE + HSGRAD + SOMECOL +          
             COLFINISHED + COLPLUS + WHITE + BLACK + ASIAN + hispanic + for_born + 
             MARRIED + DIV_SEP + WIDOWED + YNGCHG18 + YNGCH7_18 + YNGCHLEQ6 + WEEKEND_HOLIDAY + WORKING,data=data%>%filter(MALE==1))
#summary(reg60)
#REGRESSIONS WITH NO CONTROLS OTHER THAN FIXED EFFECTS FOR EACH MEASURE OF
#SOCIALITY AND WORK FROM HOME PROPORTION INTERACTED WITH POST COVID
reg61 = lm(TIME_W_FRIENDS1 ~ wfhfraction * POST_COVID + factor(YEAR)+factor(MONTH)+
             + AGE + I(AGE^2) + MALE + HSGRAD + SOMECOL +   
             COLFINISHED + COLPLUS + WHITE + BLACK + ASIAN + hispanic + for_born +   
             MARRIED + DIV_SEP + WIDOWED + YNGCHG18 + YNGCH7_18 + YNGCHLEQ6 + WEEKEND_HOLIDAY,data=data%>%filter(MALE==1))
#summary(reg61)

# reg62 = lm(TIME_W_FRIENDS2 ~ wfhfraction * POST_COVID + factor(YEAR)+factor(MONTH)+
#              + AGE + I(AGE^2) + MALE + HSGRAD + SOMECOL +     
#              COLFINISHED + COLPLUS + WHITE + BLACK + ASIAN + hispanic + for_born +  
#              MARRIED + DIV_SEP + WIDOWED + YNGCHG18 + YNGCH7_18 + YNGCHLEQ6 + WEEKEND_HOLIDAY,data=data%>%filter(MALE==1))
# #summary(reg62)

reg63 = lm(TIME_W_FRIENDS3 ~ wfhfraction * POST_COVID + factor(YEAR)+factor(MONTH)+
             + AGE + I(AGE^2) + MALE + HSGRAD + SOMECOL +   
             COLFINISHED + COLPLUS + WHITE + BLACK + ASIAN + hispanic + for_born +  
             MARRIED + DIV_SEP + WIDOWED + YNGCHG18 + YNGCH7_18 + YNGCHLEQ6 + WEEKEND_HOLIDAY,data=data%>%filter(MALE==1))
#summary(reg63)

reg64 = lm(TIME_W_FRIENDS4 ~ wfhfraction * POST_COVID + factor(YEAR)+factor(MONTH)+
             + AGE + I(AGE^2) + MALE + HSGRAD + SOMECOL +    
             COLFINISHED + COLPLUS + WHITE + BLACK + ASIAN + hispanic + for_born +
             MARRIED + DIV_SEP + WIDOWED + YNGCHG18 + YNGCH7_18 + YNGCHLEQ6 + WEEKEND_HOLIDAY,data=data%>%filter(MALE==1))
#summary(reg64)


#REGRESSIONS WITH NO CONTROLS OTHER THAN FIXED EFFECTS FOR EACH MEASURE OF SOCIALITY
#AND WORK FROM HOME DURATION
reg65 = lm(TIME_W_FRIENDS1 ~ wfmdur + factor(YEAR)+factor(MONTH)+
             factor(WEEKEND_HOLIDAY) + WORKING,data=data%>%filter((MALE==0)))
#summary(reg65)

# reg66 = lm(TIME_W_FRIENDS2 ~ wfmdur + factor(YEAR)+factor(MONTH)+
#              factor(WEEKEND_HOLIDAY) + WORKING,data=data%>%filter((MALE==0)))
# #summary(reg66)

reg67 = lm(TIME_W_FRIENDS3 ~ wfmdur + factor(YEAR)+factor(MONTH)+
             factor(WEEKEND_HOLIDAY) + WORKING,data=data%>%filter((MALE==0)))
#summary(reg67)

reg68 = lm(TIME_W_FRIENDS4 ~ wfmdur + factor(YEAR)+factor(MONTH)+
             factor(WEEKEND_HOLIDAY) + WORKING,data=data%>%filter((MALE==0)))
#summary(reg68)
#REGRESSIONS WITH NO CONTROLS OTHER THAN FIXED EFFECTS FOR EACH MEASURE OF
#SOCIALITY AND WORK FROM HOME PROPORTION
reg69 = lm(TIME_W_FRIENDS1 ~ wfhfraction + factor(YEAR)+factor(MONTH)+
             factor(WEEKEND_HOLIDAY),data=data%>%filter((MALE==0)))
#summary(reg69)

# reg70 = lm(TIME_W_FRIENDS2 ~ wfhfraction + factor(YEAR)+factor(MONTH)+
#              factor(WEEKEND_HOLIDAY),data=data%>%filter((MALE==0)))
# #summary(reg70)

reg71 = lm(TIME_W_FRIENDS3 ~ wfhfraction + factor(YEAR)+factor(MONTH)+
             factor(WEEKEND_HOLIDAY),data=data%>%filter((MALE==0)))
#summary(reg71)

reg72 = lm(TIME_W_FRIENDS4 ~ wfhfraction + factor(YEAR)+factor(MONTH)+
             factor(WEEKEND_HOLIDAY),data=data%>%filter((MALE==0)))
#summary(reg72)

#REGRESSIONS WITH NO CONTROLS OTHER THAN FIXED EFFECTS FOR EACH MEASURE OF SOCIALITY
#AND WORK FROM HOME DURATION interacted with post covid
reg73 = lm(TIME_W_FRIENDS1 ~ wfmdur * POST_COVID + factor(YEAR)+factor(MONTH)+
             factor(WEEKEND_HOLIDAY) + WORKING,data=data%>%filter((MALE==0)))
#summary(reg73)

# reg74 = lm(TIME_W_FRIENDS2 ~ wfmdur * POST_COVID + factor(YEAR)+factor(MONTH)+
#              factor(WEEKEND_HOLIDAY) + WORKING,data=data%>%filter((MALE==0)))
# #summary(reg74)

reg75 = lm(TIME_W_FRIENDS3 ~ wfmdur * POST_COVID + factor(YEAR)+factor(MONTH)+
             factor(WEEKEND_HOLIDAY) + WORKING,data=data%>%filter((MALE==0)))
#summary(reg75)

reg76 = lm(TIME_W_FRIENDS4 ~ wfmdur * POST_COVID + factor(YEAR)+factor(MONTH)+
             factor(WEEKEND_HOLIDAY) + WORKING,data=data%>%filter((MALE==0)))
#summary(reg76)
#REGRESSIONS WITH NO CONTROLS OTHER THAN FIXED EFFECTS FOR EACH MEASURE OF
#SOCIALITY AND WORK FROM HOME PROPORTION INTERACTED WITH POST COVID
reg77 = lm(TIME_W_FRIENDS1 ~ wfhfraction * POST_COVID + factor(YEAR)+factor(MONTH)+
             factor(WEEKEND_HOLIDAY),data=data%>%filter((MALE==0)))
#summary(reg77)

# reg78 = lm(TIME_W_FRIENDS2 ~ wfhfraction * POST_COVID + factor(YEAR)+factor(MONTH)+
#               factor(WEEKEND_HOLIDAY),data=data%>%filter((MALE==0)))
# #summary(reg78)

reg79 = lm(TIME_W_FRIENDS3 ~ wfhfraction * POST_COVID + factor(YEAR)+factor(MONTH)+
             factor(WEEKEND_HOLIDAY),data=data%>%filter((MALE==0)))
#summary(reg79)

reg80 = lm(TIME_W_FRIENDS4 ~ wfhfraction * POST_COVID + factor(YEAR)+factor(MONTH)+
             factor(WEEKEND_HOLIDAY),data=data%>%filter((MALE==0)))
#summary(reg80)

#DO ALL OF THESE AGAIN WITH ALL CONTROLS AND THEN SPLIT THE SAMPLE UP BETWEEN MEN
#WOMEN
reg81 = lm(TIME_W_FRIENDS1 ~ wfmdur + factor(YEAR)+factor(MONTH)+
             factor(WEEKEND_HOLIDAY) + AGE + I(AGE^2) + MALE + HSGRAD + SOMECOL + 
             COLFINISHED + COLPLUS + WHITE + BLACK + ASIAN + hispanic + for_born + 
             MARRIED + DIV_SEP + WIDOWED + YNGCHG18 + YNGCH7_18 + YNGCHLEQ6 + WEEKEND_HOLIDAY + WORKING,data=data%>%filter((MALE==0)))
#summary(reg81)

# reg82 = lm(TIME_W_FRIENDS2 ~ wfmdur + factor(YEAR)+factor(MONTH)+
#              + AGE + I(AGE^2) + MALE + HSGRAD + SOMECOL +
#              COLFINISHED + COLPLUS + WHITE + BLACK + ASIAN + hispanic + for_born + 
#              MARRIED + DIV_SEP + WIDOWED + YNGCHG18 + YNGCH7_18 + YNGCHLEQ6 + WEEKEND_HOLIDAY + WORKING,data=data%>%filter((MALE==0)))
# #summary(reg82)

reg83 = lm(TIME_W_FRIENDS3 ~ wfmdur + factor(YEAR)+factor(MONTH)+
             + AGE + I(AGE^2) + MALE + HSGRAD + SOMECOL +     
             COLFINISHED + COLPLUS + WHITE + BLACK + ASIAN + hispanic + for_born + 
             MARRIED + DIV_SEP + WIDOWED + YNGCHG18 + YNGCH7_18 + YNGCHLEQ6 + WEEKEND_HOLIDAY + WORKING,data=data%>%filter((MALE==0)))
#summary(reg83)

reg84 = lm(TIME_W_FRIENDS4 ~ wfmdur + factor(YEAR)+factor(MONTH)+
             + AGE + I(AGE^2) + MALE + HSGRAD + SOMECOL +      
             COLFINISHED + COLPLUS + WHITE + BLACK + ASIAN + hispanic + for_born + 
             MARRIED + DIV_SEP + WIDOWED + YNGCHG18 + YNGCH7_18 + YNGCHLEQ6 + WEEKEND_HOLIDAY + WORKING,data=data%>%filter((MALE==0)))
#summary(reg84)
#REGRESSIONS WITH NO CONTROLS OTHER THAN FIXED EFFECTS FOR EACH MEASURE OF
#SOCIALITY AND WORK FROM HOME PROPORTION
reg85 = lm(TIME_W_FRIENDS1 ~ wfhfraction + factor(YEAR)+factor(MONTH)+
             + AGE + I(AGE^2) + MALE + HSGRAD + SOMECOL +
             COLFINISHED + COLPLUS + WHITE + BLACK + ASIAN + hispanic + for_born + 
             MARRIED + DIV_SEP + WIDOWED + YNGCHG18 + YNGCH7_18 + YNGCHLEQ6 + WEEKEND_HOLIDAY,data=data%>%filter((MALE==0)))
#summary(reg85)

# reg86 = lm(TIME_W_FRIENDS2 ~ wfhfraction + factor(YEAR)+factor(MONTH)+
#              + AGE + I(AGE^2) + MALE + HSGRAD + SOMECOL + 
#              COLFINISHED + COLPLUS + WHITE + BLACK + ASIAN + hispanic + for_born + 
#              MARRIED + DIV_SEP + WIDOWED + YNGCHG18 + YNGCH7_18 + YNGCHLEQ6 + WEEKEND_HOLIDAY,data=data%>%filter((MALE==0)))
# #summary(reg86)

reg87 = lm(TIME_W_FRIENDS3 ~ wfhfraction + factor(YEAR)+factor(MONTH)+
             + AGE + I(AGE^2) + MALE + HSGRAD + SOMECOL + 
             COLFINISHED + COLPLUS + WHITE + BLACK + ASIAN + hispanic + for_born + 
             MARRIED + DIV_SEP + WIDOWED + YNGCHG18 + YNGCH7_18 + YNGCHLEQ6 + WEEKEND_HOLIDAY,data=data%>%filter((MALE==0)))
#summary(reg87)

reg88 = lm(TIME_W_FRIENDS4 ~ wfhfraction + factor(YEAR)+factor(MONTH)+
             + AGE + I(AGE^2) + MALE + HSGRAD + SOMECOL +      
             COLFINISHED + COLPLUS + WHITE + BLACK + ASIAN + hispanic + for_born +   
             MARRIED + DIV_SEP + WIDOWED + YNGCHG18 + YNGCH7_18 + YNGCHLEQ6 + WEEKEND_HOLIDAY,data=data%>%filter((MALE==0)))
#summary(reg88)

#REGRESSIONS WITH NO CONTROLS OTHER THAN FIXED EFFECTS FOR EACH MEASURE OF SOCIALITY
#AND WORK FROM HOME DURATION interacted with post covid
reg89 = lm(TIME_W_FRIENDS1 ~ wfmdur * POST_COVID + factor(YEAR)+factor(MONTH)+
             + AGE + I(AGE^2) + MALE + HSGRAD + SOMECOL +        
             COLFINISHED + COLPLUS + WHITE + BLACK + ASIAN + hispanic + for_born +    
             MARRIED + DIV_SEP + WIDOWED + YNGCHG18 + YNGCH7_18 + YNGCHLEQ6 + WEEKEND_HOLIDAY + WORKING,data=data%>%filter((MALE==0)))
#summary(reg89)

# reg90 = lm(TIME_W_FRIENDS2 ~ wfmdur * POST_COVID + factor(YEAR)+factor(MONTH)+
#              + AGE + I(AGE^2) + MALE + HSGRAD + SOMECOL +  
#              COLFINISHED + COLPLUS + WHITE + BLACK + ASIAN + hispanic + for_born +  
#              MARRIED + DIV_SEP + WIDOWED + YNGCHG18 + YNGCH7_18 + YNGCHLEQ6 + WEEKEND_HOLIDAY + WORKING,data=data%>%filter((MALE==0)))
# #summary(reg90)

reg91 = lm(TIME_W_FRIENDS3 ~ wfmdur * POST_COVID + factor(YEAR)+factor(MONTH)+
             + AGE + I(AGE^2) + MALE + HSGRAD + SOMECOL +       
             COLFINISHED + COLPLUS + WHITE + BLACK + ASIAN + hispanic + for_born + 
             MARRIED + DIV_SEP + WIDOWED + YNGCHG18 + YNGCH7_18 + YNGCHLEQ6 + WEEKEND_HOLIDAY + WORKING,data=data%>%filter((MALE==0)))
#summary(reg91)

reg92 = lm(TIME_W_FRIENDS4 ~ wfmdur * POST_COVID + factor(YEAR)+factor(MONTH)+
             + AGE + I(AGE^2) + MALE + HSGRAD + SOMECOL +          
             COLFINISHED + COLPLUS + WHITE + BLACK + ASIAN + hispanic + for_born + 
             MARRIED + DIV_SEP + WIDOWED + YNGCHG18 + YNGCH7_18 + YNGCHLEQ6 + WEEKEND_HOLIDAY + WORKING,data=data%>%filter((MALE==0)))
#summary(reg92)
#REGRESSIONS WITH NO CONTROLS OTHER THAN FIXED EFFECTS FOR EACH MEASURE OF
#SOCIALITY AND WORK FROM HOME PROPORTION INTERACTED WITH POST COVID
reg93 = lm(TIME_W_FRIENDS1 ~ wfhfraction * POST_COVID + factor(YEAR)+factor(MONTH)+
             + AGE + I(AGE^2) + MALE + HSGRAD + SOMECOL +   
             COLFINISHED + COLPLUS + WHITE + BLACK + ASIAN + hispanic + for_born +   
             MARRIED + DIV_SEP + WIDOWED + YNGCHG18 + YNGCH7_18 + YNGCHLEQ6 + WEEKEND_HOLIDAY,data=data%>%filter((MALE==0)))
#summary(reg93)

# reg94 = lm(TIME_W_FRIENDS2 ~ wfhfraction * POST_COVID + factor(YEAR)+factor(MONTH)+
#              + AGE + I(AGE^2) + MALE + HSGRAD + SOMECOL +     
#              COLFINISHED + COLPLUS + WHITE + BLACK + ASIAN + hispanic + for_born +  
#              MARRIED + DIV_SEP + WIDOWED + YNGCHG18 + YNGCH7_18 + YNGCHLEQ6 + WEEKEND_HOLIDAY,data=data%>%filter((MALE==0)))
# #summary(reg94)

reg95 = lm(TIME_W_FRIENDS3 ~ wfhfraction * POST_COVID + factor(YEAR)+factor(MONTH)+
             + AGE + I(AGE^2) + MALE + HSGRAD + SOMECOL +   
             COLFINISHED + COLPLUS + WHITE + BLACK + ASIAN + hispanic + for_born +  
             MARRIED + DIV_SEP + WIDOWED + YNGCHG18 + YNGCH7_18 + YNGCHLEQ6 + WEEKEND_HOLIDAY,data=data%>%filter((MALE==0)))
#summary(reg95)

reg96 = lm(TIME_W_FRIENDS4 ~ wfhfraction * POST_COVID + factor(YEAR)+factor(MONTH)+
             + AGE + I(AGE^2) + MALE + HSGRAD + SOMECOL +    
             COLFINISHED + COLPLUS + WHITE + BLACK + ASIAN + hispanic + for_born +
             MARRIED + DIV_SEP + WIDOWED + YNGCHG18 + YNGCH7_18 + YNGCHLEQ6 + WEEKEND_HOLIDAY,data=data%>%filter((MALE==0)))
#summary(reg96)

#REGRESSIONS WHERE POST COVID IS INTERACTED WITH ALL CONTROL VARIABLES and wfmdur
reg97 = lm(TIME_W_FRIENDS1 ~ wfmdur * POST_COVID + factor(YEAR)* POST_COVID+factor(MONTH)* POST_COVID+
                     + AGE* POST_COVID + I(AGE^2)* POST_COVID + MALE* POST_COVID + HSGRAD* POST_COVID + SOMECOL* POST_COVID +    
                     COLFINISHED* POST_COVID + COLPLUS* POST_COVID + WHITE* POST_COVID + BLACK * POST_COVID+ ASIAN* POST_COVID + hispanic* POST_COVID + for_born* POST_COVID +
                     MARRIED* POST_COVID + DIV_SEP* POST_COVID + WIDOWED* POST_COVID + YNGCHG18* POST_COVID + YNGCH7_18* POST_COVID + YNGCHLEQ6* POST_COVID + WEEKEND_HOLIDAY* POST_COVID
           + WORKING * POST_COVID,data=data)

reg98 = lm(TIME_W_FRIENDS3 ~ wfmdur * POST_COVID + factor(YEAR)* POST_COVID+factor(MONTH)* POST_COVID+
             + AGE* POST_COVID + I(AGE^2)* POST_COVID + MALE* POST_COVID + HSGRAD* POST_COVID + SOMECOL* POST_COVID +    
             COLFINISHED* POST_COVID + COLPLUS* POST_COVID + WHITE* POST_COVID + BLACK * POST_COVID+ ASIAN* POST_COVID + hispanic* POST_COVID + for_born* POST_COVID +
             MARRIED* POST_COVID + DIV_SEP* POST_COVID + WIDOWED* POST_COVID + YNGCHG18* POST_COVID + YNGCH7_18* POST_COVID + YNGCHLEQ6* POST_COVID + WEEKEND_HOLIDAY* POST_COVID
           + WORKING * POST_COVID,data=data)

reg99 = lm(TIME_W_FRIENDS4 ~ wfmdur * POST_COVID + factor(YEAR)* POST_COVID+factor(MONTH)* POST_COVID+
             + AGE* POST_COVID + I(AGE^2)* POST_COVID + MALE* POST_COVID + HSGRAD* POST_COVID + SOMECOL* POST_COVID +    
             COLFINISHED* POST_COVID + COLPLUS* POST_COVID + WHITE* POST_COVID + BLACK * POST_COVID+ ASIAN* POST_COVID + hispanic* POST_COVID + for_born* POST_COVID +
             MARRIED* POST_COVID + DIV_SEP* POST_COVID + WIDOWED* POST_COVID + YNGCHG18* POST_COVID + YNGCH7_18* POST_COVID + YNGCHLEQ6* POST_COVID + WEEKEND_HOLIDAY* POST_COVID
           + WORKING * POST_COVID,data=data)

reg100 = lm(TIME_W_FRIENDS1 ~ wfmdur * POST_COVID + factor(YEAR)* POST_COVID+factor(MONTH)* POST_COVID+
             + AGE* POST_COVID + I(AGE^2)* POST_COVID + MALE* POST_COVID + HSGRAD* POST_COVID + SOMECOL* POST_COVID +    
             COLFINISHED* POST_COVID + COLPLUS* POST_COVID + WHITE* POST_COVID + BLACK * POST_COVID+ ASIAN* POST_COVID + hispanic* POST_COVID + for_born* POST_COVID +
             MARRIED* POST_COVID + DIV_SEP* POST_COVID + WIDOWED* POST_COVID + YNGCHG18* POST_COVID + YNGCH7_18* POST_COVID + YNGCHLEQ6* POST_COVID + WEEKEND_HOLIDAY* POST_COVID
            + WORKING * POST_COVID,data=data%>%filter(MALE ==1))

reg101 = lm(TIME_W_FRIENDS3 ~ wfmdur * POST_COVID + factor(YEAR)* POST_COVID+factor(MONTH)* POST_COVID+
             + AGE* POST_COVID + I(AGE^2)* POST_COVID + MALE* POST_COVID + HSGRAD* POST_COVID + SOMECOL* POST_COVID +    
             COLFINISHED* POST_COVID + COLPLUS* POST_COVID + WHITE* POST_COVID + BLACK * POST_COVID+ ASIAN* POST_COVID + hispanic* POST_COVID + for_born* POST_COVID +
             MARRIED* POST_COVID + DIV_SEP* POST_COVID + WIDOWED* POST_COVID + YNGCHG18* POST_COVID + YNGCH7_18* POST_COVID + YNGCHLEQ6* POST_COVID + WEEKEND_HOLIDAY* POST_COVID
            + WORKING * POST_COVID,data=data%>%filter(MALE ==1))

reg102 = lm(TIME_W_FRIENDS4 ~ wfmdur * POST_COVID + factor(YEAR)* POST_COVID+factor(MONTH)* POST_COVID+
             + AGE* POST_COVID + I(AGE^2)* POST_COVID + MALE* POST_COVID + HSGRAD* POST_COVID + SOMECOL* POST_COVID +    
             COLFINISHED* POST_COVID + COLPLUS* POST_COVID + WHITE* POST_COVID + BLACK * POST_COVID+ ASIAN* POST_COVID + hispanic* POST_COVID + for_born* POST_COVID +
             MARRIED* POST_COVID + DIV_SEP* POST_COVID + WIDOWED* POST_COVID + YNGCHG18* POST_COVID + YNGCH7_18* POST_COVID + YNGCHLEQ6* POST_COVID + WEEKEND_HOLIDAY* POST_COVID
            + WORKING * POST_COVID,data=data%>%filter(MALE ==1))

reg103 = lm(TIME_W_FRIENDS1 ~ wfmdur * POST_COVID + factor(YEAR)* POST_COVID+factor(MONTH)* POST_COVID+
             + AGE* POST_COVID + I(AGE^2)* POST_COVID + MALE* POST_COVID + HSGRAD* POST_COVID + SOMECOL* POST_COVID +    
             COLFINISHED* POST_COVID + COLPLUS* POST_COVID + WHITE* POST_COVID + BLACK * POST_COVID+ ASIAN* POST_COVID + hispanic* POST_COVID + for_born* POST_COVID +
             MARRIED* POST_COVID + DIV_SEP* POST_COVID + WIDOWED* POST_COVID + YNGCHG18* POST_COVID + YNGCH7_18* POST_COVID + YNGCHLEQ6* POST_COVID + WEEKEND_HOLIDAY* POST_COVID
            + WORKING * POST_COVID,data=data%>%filter(MALE ==0))

reg104 = lm(TIME_W_FRIENDS3 ~ wfmdur * POST_COVID + factor(YEAR)* POST_COVID+factor(MONTH)* POST_COVID+
             + AGE* POST_COVID + I(AGE^2)* POST_COVID + MALE* POST_COVID + HSGRAD* POST_COVID + SOMECOL* POST_COVID +    
             COLFINISHED* POST_COVID + COLPLUS* POST_COVID + WHITE* POST_COVID + BLACK * POST_COVID+ ASIAN* POST_COVID + hispanic* POST_COVID + for_born* POST_COVID +
             MARRIED* POST_COVID + DIV_SEP* POST_COVID + WIDOWED* POST_COVID + YNGCHG18* POST_COVID + YNGCH7_18* POST_COVID + YNGCHLEQ6* POST_COVID + WEEKEND_HOLIDAY* POST_COVID
            + WORKING * POST_COVID,data=data%>%filter(MALE ==0))

reg105 = lm(TIME_W_FRIENDS4 ~ wfmdur * POST_COVID + factor(YEAR)* POST_COVID+factor(MONTH)* POST_COVID+
             + AGE* POST_COVID + I(AGE^2)* POST_COVID + MALE* POST_COVID + HSGRAD* POST_COVID + SOMECOL* POST_COVID +    
             COLFINISHED* POST_COVID + COLPLUS* POST_COVID + WHITE* POST_COVID + BLACK * POST_COVID+ ASIAN* POST_COVID + hispanic* POST_COVID + for_born* POST_COVID +
             MARRIED* POST_COVID + DIV_SEP* POST_COVID + WIDOWED* POST_COVID + YNGCHG18* POST_COVID + YNGCH7_18* POST_COVID + YNGCHLEQ6* POST_COVID + WEEKEND_HOLIDAY* POST_COVID
            ,data=data%>%filter(MALE ==0))

#REGRESSIONS WHERE POST COVID IS INTERACTED WITH ALL CONTROL VARIABLES AND WFHFRACTION
reg106 = lm(TIME_W_FRIENDS1 ~ wfhfraction * POST_COVID + factor(YEAR)* POST_COVID+factor(MONTH)* POST_COVID+
             + AGE* POST_COVID + I(AGE^2)* POST_COVID + MALE* POST_COVID + HSGRAD* POST_COVID + SOMECOL* POST_COVID +    
             COLFINISHED* POST_COVID + COLPLUS* POST_COVID + WHITE* POST_COVID + BLACK * POST_COVID+ ASIAN* POST_COVID + hispanic* POST_COVID + for_born* POST_COVID +
             MARRIED* POST_COVID + DIV_SEP* POST_COVID + WIDOWED* POST_COVID + YNGCHG18* POST_COVID + YNGCH7_18* POST_COVID + YNGCHLEQ6* POST_COVID + WEEKEND_HOLIDAY* POST_COVID,data=data)

reg107 = lm(TIME_W_FRIENDS3 ~ wfhfraction * POST_COVID + factor(YEAR)* POST_COVID+factor(MONTH)* POST_COVID+
             + AGE* POST_COVID + I(AGE^2)* POST_COVID + MALE* POST_COVID + HSGRAD* POST_COVID + SOMECOL* POST_COVID +    
             COLFINISHED* POST_COVID + COLPLUS* POST_COVID + WHITE* POST_COVID + BLACK * POST_COVID+ ASIAN* POST_COVID + hispanic* POST_COVID + for_born* POST_COVID +
             MARRIED* POST_COVID + DIV_SEP* POST_COVID + WIDOWED* POST_COVID + YNGCHG18* POST_COVID + YNGCH7_18* POST_COVID + YNGCHLEQ6* POST_COVID + WEEKEND_HOLIDAY* POST_COVID,data=data)

reg108 = lm(TIME_W_FRIENDS4 ~ wfhfraction * POST_COVID + factor(YEAR)* POST_COVID+factor(MONTH)* POST_COVID+
             + AGE* POST_COVID + I(AGE^2)* POST_COVID + MALE* POST_COVID + HSGRAD* POST_COVID + SOMECOL* POST_COVID +    
             COLFINISHED* POST_COVID + COLPLUS* POST_COVID + WHITE* POST_COVID + BLACK * POST_COVID+ ASIAN* POST_COVID + hispanic* POST_COVID + for_born* POST_COVID +
             MARRIED* POST_COVID + DIV_SEP* POST_COVID + WIDOWED* POST_COVID + YNGCHG18* POST_COVID + YNGCH7_18* POST_COVID + YNGCHLEQ6* POST_COVID + WEEKEND_HOLIDAY* POST_COVID,data=data)

reg109 = lm(TIME_W_FRIENDS1 ~ wfhfraction * POST_COVID + factor(YEAR)* POST_COVID+factor(MONTH)* POST_COVID+
              + AGE* POST_COVID + I(AGE^2)* POST_COVID + MALE* POST_COVID + HSGRAD* POST_COVID + SOMECOL* POST_COVID +    
              COLFINISHED* POST_COVID + COLPLUS* POST_COVID + WHITE* POST_COVID + BLACK * POST_COVID+ ASIAN* POST_COVID + hispanic* POST_COVID + for_born* POST_COVID +
              MARRIED* POST_COVID + DIV_SEP* POST_COVID + WIDOWED* POST_COVID + YNGCHG18* POST_COVID + YNGCH7_18* POST_COVID + YNGCHLEQ6* POST_COVID + WEEKEND_HOLIDAY* POST_COVID,data=data%>%filter(MALE ==1))

reg110 = lm(TIME_W_FRIENDS3 ~ wfhfraction * POST_COVID + factor(YEAR)* POST_COVID+factor(MONTH)* POST_COVID+
              + AGE* POST_COVID + I(AGE^2)* POST_COVID + MALE* POST_COVID + HSGRAD* POST_COVID + SOMECOL* POST_COVID +    
              COLFINISHED* POST_COVID + COLPLUS* POST_COVID + WHITE* POST_COVID + BLACK * POST_COVID+ ASIAN* POST_COVID + hispanic* POST_COVID + for_born* POST_COVID +
              MARRIED* POST_COVID + DIV_SEP* POST_COVID + WIDOWED* POST_COVID + YNGCHG18* POST_COVID + YNGCH7_18* POST_COVID + YNGCHLEQ6* POST_COVID + WEEKEND_HOLIDAY* POST_COVID,data=data%>%filter(MALE ==1))

reg111 = lm(TIME_W_FRIENDS4 ~ wfhfraction * POST_COVID + factor(YEAR)* POST_COVID+factor(MONTH)* POST_COVID+
              + AGE* POST_COVID + I(AGE^2)* POST_COVID + MALE* POST_COVID + HSGRAD* POST_COVID + SOMECOL* POST_COVID +    
              COLFINISHED* POST_COVID + COLPLUS* POST_COVID + WHITE* POST_COVID + BLACK * POST_COVID+ ASIAN* POST_COVID + hispanic* POST_COVID + for_born* POST_COVID +
              MARRIED* POST_COVID + DIV_SEP* POST_COVID + WIDOWED* POST_COVID + YNGCHG18* POST_COVID + YNGCH7_18* POST_COVID + YNGCHLEQ6* POST_COVID + WEEKEND_HOLIDAY* POST_COVID,data=data%>%filter(MALE ==1))

reg112 = lm(TIME_W_FRIENDS1 ~ wfhfraction * POST_COVID + factor(YEAR)* POST_COVID+factor(MONTH)* POST_COVID+
              + AGE* POST_COVID + I(AGE^2)* POST_COVID + MALE* POST_COVID + HSGRAD* POST_COVID + SOMECOL* POST_COVID +    
              COLFINISHED* POST_COVID + COLPLUS* POST_COVID + WHITE* POST_COVID + BLACK * POST_COVID+ ASIAN* POST_COVID + hispanic* POST_COVID + for_born* POST_COVID +
              MARRIED* POST_COVID + DIV_SEP* POST_COVID + WIDOWED* POST_COVID + YNGCHG18* POST_COVID + YNGCH7_18* POST_COVID + YNGCHLEQ6* POST_COVID + WEEKEND_HOLIDAY* POST_COVID,data=data%>%filter(MALE ==0))

reg113 = lm(TIME_W_FRIENDS3 ~ wfhfraction * POST_COVID + factor(YEAR)* POST_COVID+factor(MONTH)* POST_COVID+
              + AGE* POST_COVID + I(AGE^2)* POST_COVID + MALE* POST_COVID + HSGRAD* POST_COVID + SOMECOL* POST_COVID +    
              COLFINISHED* POST_COVID + COLPLUS* POST_COVID + WHITE* POST_COVID + BLACK * POST_COVID+ ASIAN* POST_COVID + hispanic* POST_COVID + for_born* POST_COVID +
              MARRIED* POST_COVID + DIV_SEP* POST_COVID + WIDOWED* POST_COVID + YNGCHG18* POST_COVID + YNGCH7_18* POST_COVID + YNGCHLEQ6* POST_COVID + WEEKEND_HOLIDAY* POST_COVID,data=data%>%filter(MALE ==0))

reg114 = lm(TIME_W_FRIENDS4 ~ wfhfraction * POST_COVID + factor(YEAR)* POST_COVID+factor(MONTH)* POST_COVID+
              + AGE* POST_COVID + I(AGE^2)* POST_COVID + MALE* POST_COVID + HSGRAD* POST_COVID + SOMECOL* POST_COVID +    
              COLFINISHED* POST_COVID + COLPLUS* POST_COVID + WHITE* POST_COVID + BLACK * POST_COVID+ ASIAN* POST_COVID + hispanic* POST_COVID + for_born* POST_COVID +
              MARRIED* POST_COVID + DIV_SEP* POST_COVID + WIDOWED* POST_COVID + YNGCHG18* POST_COVID + YNGCH7_18* POST_COVID + YNGCHLEQ6* POST_COVID + WEEKEND_HOLIDAY* POST_COVID,data=data%>%filter(MALE ==0))

#SIMPLER REGRERSSIONS WHERE POST COVID IS INTERACTED WITH EVERYTHING AND WFMDUR
reg115 = lm(TIME_W_FRIENDS1 ~ wfmdur * POST_COVID + factor(YEAR) * POST_COVID+factor(MONTH) * POST_COVID+
            factor(WEEKEND_HOLIDAY) * POST_COVID + WORKING * POST_COVID,data=data)

reg116 = lm(TIME_W_FRIENDS3 ~ wfmdur * POST_COVID + factor(YEAR) * POST_COVID+factor(MONTH) * POST_COVID+
            factor(WEEKEND_HOLIDAY) * POST_COVID + WORKING * POST_COVID,data=data)

reg117 = lm(TIME_W_FRIENDS4 ~ wfmdur * POST_COVID + factor(YEAR) * POST_COVID+factor(MONTH) * POST_COVID+
            factor(WEEKEND_HOLIDAY) * POST_COVID + WORKING * POST_COVID,data=data)

reg118 = lm(TIME_W_FRIENDS1 ~ wfmdur * POST_COVID + factor(YEAR) * POST_COVID+factor(MONTH) * POST_COVID+
              factor(WEEKEND_HOLIDAY) * POST_COVID + WORKING * POST_COVID,data=data%>%filter(MALE==1))

reg119 = lm(TIME_W_FRIENDS3 ~ wfmdur * POST_COVID + factor(YEAR) * POST_COVID+factor(MONTH) * POST_COVID+
              factor(WEEKEND_HOLIDAY) * POST_COVID + WORKING * POST_COVID,data=data%>%filter(MALE==1))

reg120 = lm(TIME_W_FRIENDS4 ~ wfmdur * POST_COVID + factor(YEAR) * POST_COVID+factor(MONTH) * POST_COVID+
              factor(WEEKEND_HOLIDAY) * POST_COVID + WORKING * POST_COVID,data=data%>%filter(MALE==1))

reg121 = lm(TIME_W_FRIENDS1 ~ wfmdur * POST_COVID + factor(YEAR) * POST_COVID+factor(MONTH) * POST_COVID+
              factor(WEEKEND_HOLIDAY) * POST_COVID + WORKING * POST_COVID,data=data%>%filter(MALE==0))

reg122 = lm(TIME_W_FRIENDS3 ~ wfmdur * POST_COVID + factor(YEAR) * POST_COVID+factor(MONTH) * POST_COVID+
              factor(WEEKEND_HOLIDAY) * POST_COVID + WORKING * POST_COVID,data=data%>%filter(MALE==0))

reg123 = lm(TIME_W_FRIENDS4 ~ wfmdur * POST_COVID + factor(YEAR) * POST_COVID+factor(MONTH) * POST_COVID+
              factor(WEEKEND_HOLIDAY) * POST_COVID + WORKING * POST_COVID,data=data%>%filter(MALE==0))

#SIMPLER REGRESSIONS WHERE POST COVID IS INTERACTED WITH EVERYTHING AND WFHFRACTION
reg124 = lm(TIME_W_FRIENDS1 ~ wfhfraction * POST_COVID + factor(YEAR) * POST_COVID+factor(MONTH) * POST_COVID+
              factor(WEEKEND_HOLIDAY) * POST_COVID,data=data)

reg125 = lm(TIME_W_FRIENDS3 ~ wfhfraction * POST_COVID + factor(YEAR) * POST_COVID+factor(MONTH) * POST_COVID+
              factor(WEEKEND_HOLIDAY) * POST_COVID,data=data)

reg126 = lm(TIME_W_FRIENDS4 ~ wfhfraction * POST_COVID + factor(YEAR) * POST_COVID+factor(MONTH) * POST_COVID+
              factor(WEEKEND_HOLIDAY) * POST_COVID,data=data)

reg127 = lm(TIME_W_FRIENDS1 ~ wfhfraction * POST_COVID + factor(YEAR) * POST_COVID+factor(MONTH) * POST_COVID+
              factor(WEEKEND_HOLIDAY) * POST_COVID,data=data%>%filter(MALE==1))

reg128 = lm(TIME_W_FRIENDS3 ~ wfhfraction * POST_COVID + factor(YEAR) * POST_COVID+factor(MONTH) * POST_COVID+
              factor(WEEKEND_HOLIDAY) * POST_COVID,data=data%>%filter(MALE==1))

reg129 = lm(TIME_W_FRIENDS4 ~ wfhfraction * POST_COVID + factor(YEAR) * POST_COVID+factor(MONTH) * POST_COVID+
              factor(WEEKEND_HOLIDAY) * POST_COVID,data=data%>%filter(MALE==1))

reg130 = lm(TIME_W_FRIENDS1 ~ wfhfraction * POST_COVID + factor(YEAR) * POST_COVID+factor(MONTH) * POST_COVID+
              factor(WEEKEND_HOLIDAY) * POST_COVID,data=data%>%filter(MALE==0))

reg131 = lm(TIME_W_FRIENDS3 ~ wfhfraction * POST_COVID + factor(YEAR) * POST_COVID+factor(MONTH) * POST_COVID+
              factor(WEEKEND_HOLIDAY) * POST_COVID,data=data%>%filter(MALE==0))

reg132 = lm(TIME_W_FRIENDS4 ~ wfhfraction * POST_COVID + factor(YEAR) * POST_COVID+factor(MONTH) * POST_COVID+
              factor(WEEKEND_HOLIDAY) * POST_COVID,data=data%>%filter(MALE==0))

#RECREATE ALL REGRESSIONS FOR ANY_wfh
reg133 = lm(TIME_W_FRIENDS1 ~ ANY_wfh + factor(YEAR)+factor(MONTH)+
            factor(WEEKEND_HOLIDAY) + WORKING,data=data)
#summary(reg1)

reg134 = lm(TIME_W_FRIENDS3 ~ ANY_wfh + factor(YEAR)+factor(MONTH)+
            factor(WEEKEND_HOLIDAY) + WORKING,data=data)
#summary(reg3)

reg135 = lm(TIME_W_FRIENDS4 ~ ANY_wfh + factor(YEAR)+factor(MONTH)+
            factor(WEEKEND_HOLIDAY) + WORKING,data=data)

reg136 = lm(TIME_W_FRIENDS1 ~ ANY_wfh * POST_COVID + factor(YEAR)+factor(MONTH)+
            factor(WEEKEND_HOLIDAY) + WORKING,data=data)
#summary(reg9)

reg137 = lm(TIME_W_FRIENDS3 ~ ANY_wfh * POST_COVID + factor(YEAR)+factor(MONTH)+
             factor(WEEKEND_HOLIDAY) + WORKING,data=data)
#summary(reg11)

reg138 = lm(TIME_W_FRIENDS4 ~ ANY_wfh * POST_COVID + factor(YEAR)+factor(MONTH)+
             factor(WEEKEND_HOLIDAY) + WORKING,data=data)

reg139 = lm(TIME_W_FRIENDS1 ~ ANY_wfh + factor(YEAR)+factor(MONTH)+
             factor(WEEKEND_HOLIDAY) + AGE + I(AGE^2) + MALE + HSGRAD + SOMECOL + 
             COLFINISHED + COLPLUS + WHITE + BLACK + ASIAN + hispanic + for_born + 
             MARRIED + DIV_SEP + WIDOWED + YNGCHG18 + YNGCH7_18 + YNGCHLEQ6 + WEEKEND_HOLIDAY + WORKING,data=data)
#summary(reg17)

reg140 = lm(TIME_W_FRIENDS3 ~ ANY_wfh + factor(YEAR)+factor(MONTH)+
             + AGE + I(AGE^2) + MALE + HSGRAD + SOMECOL +     
             COLFINISHED + COLPLUS + WHITE + BLACK + ASIAN + hispanic + for_born + 
             MARRIED + DIV_SEP + WIDOWED + YNGCHG18 + YNGCH7_18 + YNGCHLEQ6 + WEEKEND_HOLIDAY + WORKING,data=data)
#summary(reg19)

reg141 = lm(TIME_W_FRIENDS4 ~ ANY_wfh + factor(YEAR)+factor(MONTH)+
             + AGE + I(AGE^2) + MALE + HSGRAD + SOMECOL +      
             COLFINISHED + COLPLUS + WHITE + BLACK + ASIAN + hispanic + for_born + 
             MARRIED + DIV_SEP + WIDOWED + YNGCHG18 + YNGCH7_18 + YNGCHLEQ6 + WEEKEND_HOLIDAY + WORKING,data=data)
#summary(reg20)

reg142 = lm(TIME_W_FRIENDS1 ~ ANY_wfh * POST_COVID + factor(YEAR)+factor(MONTH)+
             + AGE + I(AGE^2) + MALE + HSGRAD + SOMECOL +        
             COLFINISHED + COLPLUS + WHITE + BLACK + ASIAN + hispanic + for_born +    
             MARRIED + DIV_SEP + WIDOWED + YNGCHG18 + YNGCH7_18 + YNGCHLEQ6 + WEEKEND_HOLIDAY + WORKING,data=data)
#summary(reg25)

reg143 = lm(TIME_W_FRIENDS3 ~ ANY_wfh * POST_COVID + factor(YEAR)+factor(MONTH)+
             + AGE + I(AGE^2) + MALE + HSGRAD + SOMECOL +       
             COLFINISHED + COLPLUS + WHITE + BLACK + ASIAN + hispanic + for_born + 
             MARRIED + DIV_SEP + WIDOWED + YNGCHG18 + YNGCH7_18 + YNGCHLEQ6 + WEEKEND_HOLIDAY + WORKING,data=data)
#summary(reg27)

reg144 = lm(TIME_W_FRIENDS4 ~ ANY_wfh * POST_COVID + factor(YEAR)+factor(MONTH)+
             + AGE + I(AGE^2) + MALE + HSGRAD + SOMECOL +          
             COLFINISHED + COLPLUS + WHITE + BLACK + ASIAN + hispanic + for_born + 
             MARRIED + DIV_SEP + WIDOWED + YNGCHG18 + YNGCH7_18 + YNGCHLEQ6 + WEEKEND_HOLIDAY + WORKING,data=data)
#summary(reg28)

reg145 = lm(TIME_W_FRIENDS1 ~ ANY_wfh + factor(YEAR)+factor(MONTH)+
            factor(WEEKEND_HOLIDAY) + WORKING,data=data%>%filter(MALE == 1))
#summary(reg1)

reg146 = lm(TIME_W_FRIENDS3 ~ ANY_wfh + factor(YEAR)+factor(MONTH)+
            factor(WEEKEND_HOLIDAY) + WORKING,data=data%>%filter(MALE == 1))
#summary(reg3)

reg147 = lm(TIME_W_FRIENDS4 ~ ANY_wfh + factor(YEAR)+factor(MONTH)+
            factor(WEEKEND_HOLIDAY) + WORKING,data=data%>%filter(MALE == 1))

reg148 = lm(TIME_W_FRIENDS1 ~ ANY_wfh * POST_COVID + factor(YEAR)+factor(MONTH)+
            factor(WEEKEND_HOLIDAY) + WORKING,data=data%>%filter(MALE == 1))
#summary(reg9)

reg149 = lm(TIME_W_FRIENDS3 ~ ANY_wfh * POST_COVID + factor(YEAR)+factor(MONTH)+
             factor(WEEKEND_HOLIDAY) + WORKING,data=data%>%filter(MALE == 1))
#summary(reg11)

reg150 = lm(TIME_W_FRIENDS4 ~ ANY_wfh * POST_COVID + factor(YEAR)+factor(MONTH)+
             factor(WEEKEND_HOLIDAY) + WORKING,data=data%>%filter(MALE == 1))

reg151 = lm(TIME_W_FRIENDS1 ~ ANY_wfh + factor(YEAR)+factor(MONTH)+
             factor(WEEKEND_HOLIDAY) + AGE + I(AGE^2) + MALE + HSGRAD + SOMECOL + 
             COLFINISHED + COLPLUS + WHITE + BLACK + ASIAN + hispanic + for_born + 
             MARRIED + DIV_SEP + WIDOWED + YNGCHG18 + YNGCH7_18 + YNGCHLEQ6 + WEEKEND_HOLIDAY + WORKING,data=data%>%filter(MALE == 1))
#summary(reg17)

reg152 = lm(TIME_W_FRIENDS3 ~ ANY_wfh + factor(YEAR)+factor(MONTH)+
             + AGE + I(AGE^2) + MALE + HSGRAD + SOMECOL +     
             COLFINISHED + COLPLUS + WHITE + BLACK + ASIAN + hispanic + for_born + 
             MARRIED + DIV_SEP + WIDOWED + YNGCHG18 + YNGCH7_18 + YNGCHLEQ6 + WEEKEND_HOLIDAY + WORKING,data=data%>%filter(MALE == 1))
#summary(reg19)

reg153 = lm(TIME_W_FRIENDS4 ~ ANY_wfh + factor(YEAR)+factor(MONTH)+
             + AGE + I(AGE^2) + MALE + HSGRAD + SOMECOL +      
             COLFINISHED + COLPLUS + WHITE + BLACK + ASIAN + hispanic + for_born + 
             MARRIED + DIV_SEP + WIDOWED + YNGCHG18 + YNGCH7_18 + YNGCHLEQ6 + WEEKEND_HOLIDAY + WORKING,data=data%>%filter(MALE == 1))
#summary(reg20)

reg154 = lm(TIME_W_FRIENDS1 ~ ANY_wfh * POST_COVID + factor(YEAR)+factor(MONTH)+
             + AGE + I(AGE^2) + MALE + HSGRAD + SOMECOL +        
             COLFINISHED + COLPLUS + WHITE + BLACK + ASIAN + hispanic + for_born +    
             MARRIED + DIV_SEP + WIDOWED + YNGCHG18 + YNGCH7_18 + YNGCHLEQ6 + WEEKEND_HOLIDAY + WORKING,data=data%>%filter(MALE == 1))
#summary(reg25)

reg155 = lm(TIME_W_FRIENDS3 ~ ANY_wfh * POST_COVID + factor(YEAR)+factor(MONTH)+
             + AGE + I(AGE^2) + MALE + HSGRAD + SOMECOL +       
             COLFINISHED + COLPLUS + WHITE + BLACK + ASIAN + hispanic + for_born + 
             MARRIED + DIV_SEP + WIDOWED + YNGCHG18 + YNGCH7_18 + YNGCHLEQ6 + WEEKEND_HOLIDAY + WORKING,data=data%>%filter(MALE == 1))
#summary(reg27)

reg156 = lm(TIME_W_FRIENDS4 ~ ANY_wfh * POST_COVID + factor(YEAR)+factor(MONTH)+
             + AGE + I(AGE^2) + MALE + HSGRAD + SOMECOL +          
             COLFINISHED + COLPLUS + WHITE + BLACK + ASIAN + hispanic + for_born + 
             MARRIED + DIV_SEP + WIDOWED + YNGCHG18 + YNGCH7_18 + YNGCHLEQ6 + WEEKEND_HOLIDAY + WORKING,data=data%>%filter(MALE == 1))
#summary(reg28)

reg157 = lm(TIME_W_FRIENDS1 ~ ANY_wfh + factor(YEAR)+factor(MONTH)+
            factor(WEEKEND_HOLIDAY) + WORKING,data=data%>%filter(MALE == 0))
#summary(reg1)

reg158 = lm(TIME_W_FRIENDS3 ~ ANY_wfh + factor(YEAR)+factor(MONTH)+
            factor(WEEKEND_HOLIDAY) + WORKING,data=data%>%filter(MALE == 0))
#summary(reg3)

reg159 = lm(TIME_W_FRIENDS4 ~ ANY_wfh + factor(YEAR)+factor(MONTH)+
            factor(WEEKEND_HOLIDAY) + WORKING,data=data%>%filter(MALE == 0))

reg160 = lm(TIME_W_FRIENDS1 ~ ANY_wfh * POST_COVID + factor(YEAR)+factor(MONTH)+
            factor(WEEKEND_HOLIDAY) + WORKING,data=data%>%filter(MALE == 0))
#summary(reg9)

reg161 = lm(TIME_W_FRIENDS3 ~ ANY_wfh * POST_COVID + factor(YEAR)+factor(MONTH)+
             factor(WEEKEND_HOLIDAY) + WORKING,data=data%>%filter(MALE == 0))
#summary(reg11)

reg162 = lm(TIME_W_FRIENDS4 ~ ANY_wfh * POST_COVID + factor(YEAR)+factor(MONTH)+
             factor(WEEKEND_HOLIDAY) + WORKING,data=data%>%filter(MALE == 0))

reg163 = lm(TIME_W_FRIENDS1 ~ ANY_wfh + factor(YEAR)+factor(MONTH)+
             factor(WEEKEND_HOLIDAY) + AGE + I(AGE^2) + MALE + HSGRAD + SOMECOL + 
             COLFINISHED + COLPLUS + WHITE + BLACK + ASIAN + hispanic + for_born + 
             MARRIED + DIV_SEP + WIDOWED + YNGCHG18 + YNGCH7_18 + YNGCHLEQ6 + WEEKEND_HOLIDAY + WORKING,data=data%>%filter(MALE == 0))
#summary(reg17)

reg164 = lm(TIME_W_FRIENDS3 ~ ANY_wfh + factor(YEAR)+factor(MONTH)+
             + AGE + I(AGE^2) + MALE + HSGRAD + SOMECOL +     
             COLFINISHED + COLPLUS + WHITE + BLACK + ASIAN + hispanic + for_born + 
             MARRIED + DIV_SEP + WIDOWED + YNGCHG18 + YNGCH7_18 + YNGCHLEQ6 + WEEKEND_HOLIDAY + WORKING,data=data%>%filter(MALE == 0))
#summary(reg19)

reg165 = lm(TIME_W_FRIENDS4 ~ ANY_wfh + factor(YEAR)+factor(MONTH)+
             + AGE + I(AGE^2) + MALE + HSGRAD + SOMECOL +      
             COLFINISHED + COLPLUS + WHITE + BLACK + ASIAN + hispanic + for_born + 
             MARRIED + DIV_SEP + WIDOWED + YNGCHG18 + YNGCH7_18 + YNGCHLEQ6 + WEEKEND_HOLIDAY + WORKING,data=data%>%filter(MALE == 0))
#summary(reg20)

reg166 = lm(TIME_W_FRIENDS1 ~ ANY_wfh * POST_COVID + factor(YEAR)+factor(MONTH)+
             + AGE + I(AGE^2) + MALE + HSGRAD + SOMECOL +        
             COLFINISHED + COLPLUS + WHITE + BLACK + ASIAN + hispanic + for_born +    
             MARRIED + DIV_SEP + WIDOWED + YNGCHG18 + YNGCH7_18 + YNGCHLEQ6 + WEEKEND_HOLIDAY + WORKING,data=data%>%filter(MALE == 0))
#summary(reg25)

reg167 = lm(TIME_W_FRIENDS3 ~ ANY_wfh * POST_COVID + factor(YEAR)+factor(MONTH)+
             + AGE + I(AGE^2) + MALE + HSGRAD + SOMECOL +       
             COLFINISHED + COLPLUS + WHITE + BLACK + ASIAN + hispanic + for_born + 
             MARRIED + DIV_SEP + WIDOWED + YNGCHG18 + YNGCH7_18 + YNGCHLEQ6 + WEEKEND_HOLIDAY + WORKING,data=data%>%filter(MALE == 0))
#summary(reg27)

reg168 = lm(TIME_W_FRIENDS4 ~ ANY_wfh * POST_COVID + factor(YEAR)+factor(MONTH)+
             + AGE + I(AGE^2) + MALE + HSGRAD + SOMECOL +          
             COLFINISHED + COLPLUS + WHITE + BLACK + ASIAN + hispanic + for_born + 
             MARRIED + DIV_SEP + WIDOWED + YNGCHG18 + YNGCH7_18 + YNGCHLEQ6 + WEEKEND_HOLIDAY + WORKING,data=data%>%filter(MALE == 0))
#summary(reg28)

reg169 = lm(TIME_W_FRIENDS1 ~ ANY_wfh * POST_COVID + factor(YEAR)* POST_COVID+factor(MONTH)* POST_COVID+
             + AGE* POST_COVID + I(AGE^2)* POST_COVID + MALE* POST_COVID + HSGRAD* POST_COVID + SOMECOL* POST_COVID +    
             COLFINISHED* POST_COVID + COLPLUS* POST_COVID + WHITE* POST_COVID + BLACK * POST_COVID+ ASIAN* POST_COVID + hispanic* POST_COVID + for_born* POST_COVID +
             MARRIED* POST_COVID + DIV_SEP* POST_COVID + WIDOWED* POST_COVID + YNGCHG18* POST_COVID + YNGCH7_18* POST_COVID + YNGCHLEQ6* POST_COVID + WEEKEND_HOLIDAY* POST_COVID
           + WORKING * POST_COVID,data=data)

reg170 = lm(TIME_W_FRIENDS3 ~ ANY_wfh * POST_COVID + factor(YEAR)* POST_COVID+factor(MONTH)* POST_COVID+
             + AGE* POST_COVID + I(AGE^2)* POST_COVID + MALE* POST_COVID + HSGRAD* POST_COVID + SOMECOL* POST_COVID +    
             COLFINISHED* POST_COVID + COLPLUS* POST_COVID + WHITE* POST_COVID + BLACK * POST_COVID+ ASIAN* POST_COVID + hispanic* POST_COVID + for_born* POST_COVID +
             MARRIED* POST_COVID + DIV_SEP* POST_COVID + WIDOWED* POST_COVID + YNGCHG18* POST_COVID + YNGCH7_18* POST_COVID + YNGCHLEQ6* POST_COVID + WEEKEND_HOLIDAY* POST_COVID
           + WORKING * POST_COVID,data=data)

reg171 = lm(TIME_W_FRIENDS4 ~ ANY_wfh * POST_COVID + factor(YEAR)* POST_COVID+factor(MONTH)* POST_COVID+
             + AGE* POST_COVID + I(AGE^2)* POST_COVID + MALE* POST_COVID + HSGRAD* POST_COVID + SOMECOL* POST_COVID +    
             COLFINISHED* POST_COVID + COLPLUS* POST_COVID + WHITE* POST_COVID + BLACK * POST_COVID+ ASIAN* POST_COVID + hispanic* POST_COVID + for_born* POST_COVID +
             MARRIED* POST_COVID + DIV_SEP* POST_COVID + WIDOWED* POST_COVID + YNGCHG18* POST_COVID + YNGCH7_18* POST_COVID + YNGCHLEQ6* POST_COVID + WEEKEND_HOLIDAY* POST_COVID
           + WORKING * POST_COVID,data=data)

reg172 = lm(TIME_W_FRIENDS1 ~ ANY_wfh * POST_COVID + factor(YEAR)* POST_COVID+factor(MONTH)* POST_COVID+
              + AGE* POST_COVID + I(AGE^2)* POST_COVID + MALE* POST_COVID + HSGRAD* POST_COVID + SOMECOL* POST_COVID +    
              COLFINISHED* POST_COVID + COLPLUS* POST_COVID + WHITE* POST_COVID + BLACK * POST_COVID+ ASIAN* POST_COVID + hispanic* POST_COVID + for_born* POST_COVID +
              MARRIED* POST_COVID + DIV_SEP* POST_COVID + WIDOWED* POST_COVID + YNGCHG18* POST_COVID + YNGCH7_18* POST_COVID + YNGCHLEQ6* POST_COVID + WEEKEND_HOLIDAY* POST_COVID
            + WORKING * POST_COVID,data=data%>%filter(MALE ==1))

reg173 = lm(TIME_W_FRIENDS3 ~ ANY_wfh * POST_COVID + factor(YEAR)* POST_COVID+factor(MONTH)* POST_COVID+
              + AGE* POST_COVID + I(AGE^2)* POST_COVID + MALE* POST_COVID + HSGRAD* POST_COVID + SOMECOL* POST_COVID +    
              COLFINISHED* POST_COVID + COLPLUS* POST_COVID + WHITE* POST_COVID + BLACK * POST_COVID+ ASIAN* POST_COVID + hispanic* POST_COVID + for_born* POST_COVID +
              MARRIED* POST_COVID + DIV_SEP* POST_COVID + WIDOWED* POST_COVID + YNGCHG18* POST_COVID + YNGCH7_18* POST_COVID + YNGCHLEQ6* POST_COVID + WEEKEND_HOLIDAY* POST_COVID
            + WORKING * POST_COVID,data=data%>%filter(MALE ==1))

reg174 = lm(TIME_W_FRIENDS4 ~ ANY_wfh * POST_COVID + factor(YEAR)* POST_COVID+factor(MONTH)* POST_COVID+
              + AGE* POST_COVID + I(AGE^2)* POST_COVID + MALE* POST_COVID + HSGRAD* POST_COVID + SOMECOL* POST_COVID +    
              COLFINISHED* POST_COVID + COLPLUS* POST_COVID + WHITE* POST_COVID + BLACK * POST_COVID+ ASIAN* POST_COVID + hispanic* POST_COVID + for_born* POST_COVID +
              MARRIED* POST_COVID + DIV_SEP* POST_COVID + WIDOWED* POST_COVID + YNGCHG18* POST_COVID + YNGCH7_18* POST_COVID + YNGCHLEQ6* POST_COVID + WEEKEND_HOLIDAY* POST_COVID
            + WORKING * POST_COVID,data=data%>%filter(MALE ==1))

reg175 = lm(TIME_W_FRIENDS1 ~ ANY_wfh * POST_COVID + factor(YEAR)* POST_COVID+factor(MONTH)* POST_COVID+
              + AGE* POST_COVID + I(AGE^2)* POST_COVID + MALE* POST_COVID + HSGRAD* POST_COVID + SOMECOL* POST_COVID +    
              COLFINISHED* POST_COVID + COLPLUS* POST_COVID + WHITE* POST_COVID + BLACK * POST_COVID+ ASIAN* POST_COVID + hispanic* POST_COVID + for_born* POST_COVID +
              MARRIED* POST_COVID + DIV_SEP* POST_COVID + WIDOWED* POST_COVID + YNGCHG18* POST_COVID + YNGCH7_18* POST_COVID + YNGCHLEQ6* POST_COVID + WEEKEND_HOLIDAY* POST_COVID
            + WORKING * POST_COVID,data=data%>%filter(MALE ==0))

reg176 = lm(TIME_W_FRIENDS3 ~ ANY_wfh * POST_COVID + factor(YEAR)* POST_COVID+factor(MONTH)* POST_COVID+
              + AGE* POST_COVID + I(AGE^2)* POST_COVID + MALE* POST_COVID + HSGRAD* POST_COVID + SOMECOL* POST_COVID +    
              COLFINISHED* POST_COVID + COLPLUS* POST_COVID + WHITE* POST_COVID + BLACK * POST_COVID+ ASIAN* POST_COVID + hispanic* POST_COVID + for_born* POST_COVID +
              MARRIED* POST_COVID + DIV_SEP* POST_COVID + WIDOWED* POST_COVID + YNGCHG18* POST_COVID + YNGCH7_18* POST_COVID + YNGCHLEQ6* POST_COVID + WEEKEND_HOLIDAY* POST_COVID
            + WORKING * POST_COVID,data=data%>%filter(MALE ==0))

reg177 = lm(TIME_W_FRIENDS4 ~ ANY_wfh * POST_COVID + factor(YEAR)* POST_COVID+factor(MONTH)* POST_COVID+
              + AGE* POST_COVID + I(AGE^2)* POST_COVID + MALE* POST_COVID + HSGRAD* POST_COVID + SOMECOL* POST_COVID +    
              COLFINISHED* POST_COVID + COLPLUS* POST_COVID + WHITE* POST_COVID + BLACK * POST_COVID+ ASIAN* POST_COVID + hispanic* POST_COVID + for_born* POST_COVID +
              MARRIED* POST_COVID + DIV_SEP* POST_COVID + WIDOWED* POST_COVID + YNGCHG18* POST_COVID + YNGCH7_18* POST_COVID + YNGCHLEQ6* POST_COVID + WEEKEND_HOLIDAY* POST_COVID
            ,data=data%>%filter(MALE ==0))


#SIMPLER REGRERSSIONS WHERE POST COVID IS INTERACTED WITH EVERYTHING AND WFMDUR
reg178 = lm(TIME_W_FRIENDS1 ~ ANY_wfh * POST_COVID + factor(YEAR) * POST_COVID+factor(MONTH) * POST_COVID+
              factor(WEEKEND_HOLIDAY) * POST_COVID + WORKING * POST_COVID,data=data)

reg179 = lm(TIME_W_FRIENDS3 ~ ANY_wfh * POST_COVID + factor(YEAR) * POST_COVID+factor(MONTH) * POST_COVID+
              factor(WEEKEND_HOLIDAY) * POST_COVID + WORKING * POST_COVID,data=data)

reg180 = lm(TIME_W_FRIENDS4 ~ ANY_wfh * POST_COVID + factor(YEAR) * POST_COVID+factor(MONTH) * POST_COVID+
              factor(WEEKEND_HOLIDAY) * POST_COVID + WORKING * POST_COVID,data=data)

reg181 = lm(TIME_W_FRIENDS1 ~ ANY_wfh * POST_COVID + factor(YEAR) * POST_COVID+factor(MONTH) * POST_COVID+
              factor(WEEKEND_HOLIDAY) * POST_COVID + WORKING * POST_COVID,data=data%>%filter(MALE==1))

reg182 = lm(TIME_W_FRIENDS3 ~ ANY_wfh * POST_COVID + factor(YEAR) * POST_COVID+factor(MONTH) * POST_COVID+
              factor(WEEKEND_HOLIDAY) * POST_COVID + WORKING * POST_COVID,data=data%>%filter(MALE==1))

reg183 = lm(TIME_W_FRIENDS4 ~ ANY_wfh * POST_COVID + factor(YEAR) * POST_COVID+factor(MONTH) * POST_COVID+
              factor(WEEKEND_HOLIDAY) * POST_COVID + WORKING * POST_COVID,data=data%>%filter(MALE==1))

reg184 = lm(TIME_W_FRIENDS1 ~ ANY_wfh * POST_COVID + factor(YEAR) * POST_COVID+factor(MONTH) * POST_COVID+
              factor(WEEKEND_HOLIDAY) * POST_COVID + WORKING * POST_COVID,data=data%>%filter(MALE==0))

reg185 = lm(TIME_W_FRIENDS3 ~ ANY_wfh * POST_COVID + factor(YEAR) * POST_COVID+factor(MONTH) * POST_COVID+
              factor(WEEKEND_HOLIDAY) * POST_COVID + WORKING * POST_COVID,data=data%>%filter(MALE==0))

reg186 = lm(TIME_W_FRIENDS4 ~ ANY_wfh * POST_COVID + factor(YEAR) * POST_COVID+factor(MONTH) * POST_COVID+
              factor(WEEKEND_HOLIDAY) * POST_COVID + WORKING * POST_COVID,data=data%>%filter(MALE==0))
#REGRESSION TABLES FOR FULL SAMPLE (FIRST WORK FROM HOME DURATION NO CONTROLS, THEN
#WITH INTERACTION AND NO CONTROLS, THEN BOTH WITH CONTROLS. THEN SAME FOR WORK FROM HOME
#PROPORTION)
q_wfmdur = t(quantile(data$wfmdur, c(0,0.1,0.25,0.5,0.75,0.9,1)))
q_wfhfraction = t(quantile(data$wfhfraction, c(0,0.1,0.25,0.5,0.75,0.9,1)))
q_ANY_wfh =t(quantile(data$ANY_wfh, c(0,0.1,0.25,0.5,0.75,0.9,1)))
q_TIME_W_FRIENDS1 = t(quantile(data$TIME_W_FRIENDS1, c(0,0.1,0.25,0.5,0.75,0.9,1)))
q_TIME_W_FRIENDS3 = t(quantile(data$TIME_W_FRIENDS3, c(0,0.1,0.25,0.5,0.75,0.9,1)))
q_TIME_W_FRIENDS4 = t(quantile(data$TIME_W_FRIENDS4, c(0,0.1,0.25,0.5,0.75,0.9,1)))

wfmdur_stats =  data%>%summarize(mean = mean(wfmdur), sd = sd(wfmdur),quantiles = q_wfmdur)
wfhfraction_stats = data%>%summarize(mean = mean(wfhfraction), sd = sd(wfhfraction),quantiles = q_wfhfraction)
ANY_wfh_stats = data%>%summarize(mean = mean(ANY_wfh), sd = sd(ANY_wfh),quantiles = q_ANY_wfh)
TIME_W_FRIENDS1_stats = data%>%summarize(mean = mean(TIME_W_FRIENDS1), sd = sd(TIME_W_FRIENDS1),quantiles = q_TIME_W_FRIENDS1) 
TIME_W_FRIENDS3_stats = data%>%summarize(mean = mean(TIME_W_FRIENDS3), sd = sd(TIME_W_FRIENDS3),quantiles = q_TIME_W_FRIENDS3) 
TIME_W_FRIENDS4_stats = data%>%summarize(mean = mean(TIME_W_FRIENDS4), sd = sd(TIME_W_FRIENDS4),quantiles = q_TIME_W_FRIENDS4) 

q_wfmdur_n0 = t(quantile((data%>%filter(wfmdur != 0))$wfmdur, c(0,0.1,0.25,0.5,0.75,0.9,1)))
q_wfhfraction_n0 = t(quantile((data%>%filter(wfhfraction != 0))$wfhfraction, c(0,0.1,0.25,0.5,0.75,0.9,1)))
q_TIME_W_FRIENDS1_n0 = t(quantile((data%>%filter(TIME_W_FRIENDS1 != 0))$TIME_W_FRIENDS1, c(0,0.1,0.25,0.5,0.75,0.9,1)))
q_TIME_W_FRIENDS3_n0 = t(quantile((data%>%filter(TIME_W_FRIENDS3 != 0))$TIME_W_FRIENDS3, c(0,0.1,0.25,0.5,0.75,0.9,1)))
q_TIME_W_FRIENDS4_n0 = t(quantile((data%>%filter(TIME_W_FRIENDS4 != 0))$TIME_W_FRIENDS4, c(0,0.1,0.25,0.5,0.75,0.9,1)))

wfmdur_stats_n0 =  data%>%filter(wfmdur!=0)%>%summarize(mean = mean(wfmdur), sd = sd(wfmdur),quantiles = q_wfmdur_n0)
wfhfraction_stats_n0 = data%>%filter(wfhfraction!=0)%>%summarize(mean = mean(wfhfraction), sd = sd(wfhfraction),quantiles = q_wfhfraction_n0)
TIME_W_FRIENDS1_stats_n0 = data%>%filter(TIME_W_FRIENDS1!=0)%>%summarize(mean = mean(TIME_W_FRIENDS1), sd = sd(TIME_W_FRIENDS1),quantiles = q_TIME_W_FRIENDS1_n0) 
TIME_W_FRIENDS3_stats_n0 = data%>%filter(TIME_W_FRIENDS3!=0)%>%summarize(mean = mean(TIME_W_FRIENDS3), sd = sd(TIME_W_FRIENDS3),quantiles = q_TIME_W_FRIENDS3_n0) 
TIME_W_FRIENDS4_stats_n0 = data%>%filter(TIME_W_FRIENDS4!=0)%>%summarize(mean = mean(TIME_W_FRIENDS4), sd = sd(TIME_W_FRIENDS4),quantiles = q_TIME_W_FRIENDS4_n0)


stargazer(reg1, reg9, reg115, reg17, reg25, reg97, reg5, reg13, reg124, reg21, reg29, reg106,reg133,reg136,reg178,reg139,reg142,reg169,  type="text",title = "SOC 1 Coeffs for ALL SAMPLED", keep = c("\\bwfmdur\\b", "\\bwfhfraction\\b","\\bANY_wfh\\b"),
          font.size = "small",
          align = TRUE,
          omit.stat=c("f", "ser"),
          column.sep.width = "1pt")
# stargazer(reg2, reg10, reg18, reg26, reg6, reg14, reg22, reg30, type="text", title = "SOC 2 Coeffs for ALL SAMPLED", keep = c("\\bwfmdur\\b", "\\bwfhfraction\\b"),
#           font.size = "small",
#           align = TRUE,
#           omit.stat=c("f", "ser"),
#           column.sep.width = "-15pt")
stargazer(reg3, reg11, reg116, reg19, reg27, reg98, reg7, reg15, reg125, reg23, reg31, reg107,reg134,reg137,reg179,reg140,reg143,reg170, type="text", title = "SOC 3 Coeffs for ALL SAMPLED", keep = c("\\bwfmdur\\b", "\\bwfhfraction\\b","\\bANY_wfh\\b"),
          font.size = "small",
          align = TRUE,
          omit.stat=c("f", "ser"),
          column.sep.width = "-15pt")
stargazer(reg4, reg12, reg117, reg20, reg28, reg99, reg8, reg16, reg126, reg24, reg32, reg108,reg135,reg138,reg180,reg141,reg144,reg171, type="text", title = "SOC 4 Coeffs for ALL SAMPLED", keep = c("\\bwfmdur\\b", "\\bwfhfraction\\b","\\bANY_wfh\\b"),
          font.size = "small",
          align = TRUE,
          omit.stat=c("f", "ser"),
          column.sep.width = "-15pt")

#REGRESSION TABLES FOR MEN SAMPLED
stargazer(reg33, reg41, reg118, reg49, reg57, reg100, reg37, reg45, reg127, reg53, reg61, reg109,reg145,reg148,reg181,reg151,reg154,reg172, type="text", title = "SOC 1 Coeffs for MEN SAMPLED", keep = c("\\bwfmdur\\b", "\\bwfhfraction\\b","\\bANY_wfh\\b"),
          font.size = "small",
          align = TRUE,
          omit.stat=c("f", "ser"),
          column.sep.width = "-15pt")
# stargazer(reg34, reg42, reg50, reg58, reg38, reg46,reg54, reg62, type="text", title = "SOC 2 Coeffs for MEN SAMPLED", keep = c("\\bwfmdur\\b", "\\bwfhfraction\\b"),
#           font.size = "small",
#           align = TRUE,
#           omit.stat=c("f", "ser"),
#           column.sep.width = "-15pt")
stargazer(reg35, reg43, reg119, reg51, reg59, reg101, reg39, reg47, reg128, reg55, reg63, reg110, reg146,reg149,reg182,reg152,reg155,reg173, type="text", title = "SOC 3 Coeffs for MEN SAMPLED", keep = c("\\bwfmdur\\b", "\\bwfhfraction\\b","\\bANY_wfh\\b"),
          font.size = "small",
          align = TRUE,
          omit.stat=c("f", "ser"),
          column.sep.width = "-15pt")
stargazer(reg36, reg44, reg120, reg52, reg60, reg102, reg40, reg48, reg129, reg56, reg64, reg111, reg147,reg150,reg183,reg153,reg156,reg174, type ="text", title = "SOC 4 Coeffs for MEN SAMPLED", keep = c("\\bwfmdur\\b", "\\bwfhfraction\\b","\\bANY_wfh\\b"),
          font.size = "small",
          align = TRUE,
          omit.stat=c("f", "ser"),
          column.sep.width = "-15pt")

#REGRESSION TABLES FOR WOMEN SAMPLED
stargazer(reg65, reg73, reg121, reg81, reg89, reg103, reg69, reg77, reg130, reg85, reg93, reg112, reg157,reg160,reg184,reg163,reg166,reg175,type="text", title = "SOC 1 Coeffs for WOMEN SAMPLED", keep = c("\\bwfmdur\\b", "\\bwfhfraction\\b","\\bANY_wfh\\b"),
          font.size = "small",
          align = TRUE,
          omit.stat=c("f", "ser"),
          column.sep.width = "-15pt")
# stargazer(reg66, reg74, reg82, reg90, reg70, reg78,reg86, reg94, type="text", title = "SOC 2 Coeffs for WOMEN SAMPLED", keep = c("\\bwfmdur\\b", "\\bwfhfraction\\b"),
#           font.size = "small",
#           align = TRUE,
#           omit.stat=c("f", "ser"),
#           column.sep.width = "-15pt")
stargazer(reg67, reg75, reg122, reg83, reg91, reg104, reg71, reg79, reg131, reg87, reg95, reg113,reg158,reg161,reg185,reg164,reg167,reg176, type="text", title = "SOC 3 Coeffs for WOMEN SAMPLED", keep = c("\\bwfmdur\\b", "\\bwfhfraction\\b","\\bANY_wfh\\b"),
          font.size = "small",
          align = TRUE,
          omit.stat=c("f", "ser"),
          column.sep.width = "-15pt")
stargazer(reg68, reg76, reg123, reg84, reg92, reg105, reg72, reg80, reg132, reg88, reg96, reg114,reg159,reg162,reg186,reg165,reg168,reg177, type ="text", title = "SOC 4 Coeffs for WOMEN SAMPLED", keep = c("\\bwfmdur\\b", "\\bwfhfraction\\b","\\bANY_wfh\\b"),
          font.size = "small",
          align = TRUE,
          omit.stat=c("f", "ser"),
          column.sep.width = "-15pt")
print(wfmdur_stats)
print(wfhfraction_stats)
print(ANY_wfh_stats)
print(TIME_W_FRIENDS1_stats)
print(TIME_W_FRIENDS3_stats)
print(TIME_W_FRIENDS4_stats)
print(wfmdur_stats_n0)
print(wfhfraction_stats_n0)
print(TIME_W_FRIENDS1_stats_n0)
print(TIME_W_FRIENDS3_stats_n0)
print(TIME_W_FRIENDS4_stats_n0)
