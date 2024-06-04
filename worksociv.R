library(readr)
library(tidyverse)
library(stringr)
library(fixest)
rm(list = ls())
setwd("C:/Users/jmca9/Downloads")
data1 <- read_csv("socpumscross.csv")
data2 <- read_csv("onet_teleworkable_blscodes.csv")
emp_nos_data <- read_csv("national_M2018_dl.csv")

#Possible issue with managers due to one of the new codes not being included

#Examples of mistakes in Nieman and Dingel (first one below)

#Buyers and Purchasing agents 13-1020

#Cannot find 25-9011

#27-3011,27-3012

#29-2054

#35-3021,35-3022

#39-3031

#43-9011

#49-9093

#51-9122

#53-7111
#Join the Dingel and Nieman set with the 2018 employment numbers data
data2 = left_join(data2,emp_nos_data,join_by("OCC_CODE"))

data2$OCC_CODE_CW = data2$OCC_CODE

#2018 Encoding (make adjustments to D+N data to align it with 2018 codes)
data2$OCC_CODE_CW = sub("11-2031",replacement = "11-2030",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("11-3011",replacement = "11-3012",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("11-903.",replacement = "11-9030",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("11-9061",replacement = "11-9171",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("11-9071",replacement = "11-9070",x =data2$OCC_CODE_CW)

#all of these jobs can be done from home apparently, so I mapped them all
#to a single sub-category
data2$OCC_CODE_CW = sub("13-1020",replacement = "13-1021",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("13-103.",replacement = "13-1030",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("13-107.",replacement = "13-1070",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("13-2021",replacement = "13-2020",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("13-207.",replacement = "13-2070",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("13-2099",replacement = "13-20XX",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("15-1111",replacement = "15-1221",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("15-1121",replacement = "15-1211",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("15-1122",replacement = "15-1299",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("15-1131",replacement = "15-1251",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("15-113[23]",replacement = "15-1252",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("15-1134",replacement = "15-1254",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("15-1141",replacement = "15-124X",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("15-1142",replacement = "15-1244",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("15-1143",replacement = "15-1241",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("15-115[12]",replacement = "15-1230",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("15-1199",replacement = "15-1299",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("17-102[12]",replacement = "17-1020",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("17-207[12]",replacement = "17-2070",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("17-211[12]",replacement = "17-2110",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("17-301[2-3]",replacement = "17-301X",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("17-302[^3]",replacement = "17-302X",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("19-101[123]",replacement = "19-1010",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("19-102.",replacement = "19-1020",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("19-103.",replacement = "19-1030",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("19-104.",replacement = "19-1040",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("19-201.",replacement = "19-2010",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("19-203.",replacement = "19-2030",x =data2$OCC_CODE_CW)

#Can't determine split properly
data2$OCC_CODE_CW = sub("19-3031",replacement = "19-3033",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("19-204[2-3]",replacement = "19-204X",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("19-303[29]", replacement = "19-303X", x = data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("19-309.",replacement = "19-3090",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("19-4011",replacement = "19-4010",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("19-4041",replacement = "19-4040",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("19-4091",replacement = "19-4040",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("19-409[^1]",replacement = "19-40XX",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("19-4071",replacement = "19-40XX",x =data2$OCC_CODE_CW)

#couldn't separate
data2$OCC_CODE_CW = sub("21-1018",replacement = "21-1011",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("21-109[^23]",replacement = "21-109X",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("23-102.",replacement = "23-1020",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("23-2091",replacement = "27-3092",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("25-1[01][0-9][0-9]",replacement = "25-1000",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("25-201.",replacement = "25-2010",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("25-202.",replacement = "25-2020",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("25-203.",replacement = "25-2030",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("25-205.",replacement = "25-2050",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("25-30..",replacement = "25-30XX",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("25-401.",replacement = "25-4010",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("25-4021",replacement = "25-4022",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("25-9041",replacement = "25-9040",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("25-90[^1]1",replacement = "25-90XX",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("27-101.",replacement = "27-1010",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("27-203.",replacement = "27-2030",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("27-1027",replacement = "27-102X",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("27-302[12]",replacement = "27-3023",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("27-401.",replacement = "27-4010",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("27-403.",replacement = "27-4030",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("29-102.",replacement = "29-1020",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("29-106[79]",replacement = "29-1240",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("29-106.",replacement = "29-12XX",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("29-1199",replacement = "29-1299",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("29-2021",replacement = "29-1292",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("29-2033",replacement = "29-203X",x =data2$OCC_CODE_CW)

#couldn't separate
data2$OCC_CODE_CW = sub("29-2041",replacement = "29-2042",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("29-205[17]",replacement = "29-205X",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("29-2071",replacement = "29-2072",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("29-209.",replacement = "29-2090",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("29-901.",replacement = "19-5010",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("29-909.",replacement = "29-9000",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("31-1011",replacement = "31-1121",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("31-1014",replacement = "31-1131",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("31-101[35]",replacement = "31-113X",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("31-201.",replacement = "31-2010",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("31-202.",replacement = "31-2020",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("31-909[39]",replacement = "31-909X",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("33-202.",replacement = "33-2020",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("33-305.",replacement = "33-3050",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("33-903.",replacement = "33-9030",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("33-909[29]",replacement = "33-909X",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("35-201.",replacement = "35-2010",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("37-201.",replacement = "37-201X",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("37-3012",replacement = "37-301X",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("39-1...",replacement = "39-1000",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("39-301.",replacement = "39-3010",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("39-3021",replacement = "39-30XX",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("39-309.",replacement = "39-30XX",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("39-40[12].",replacement = "39-40XX",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("39-509[13]",replacement = "39-509X",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("39-601.",replacement = "39-6010",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("39-9021",replacement = "31-1122",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("41-201.",replacement = "41-2010",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("41-3099",replacement = "41-3091",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("41-401.",replacement = "41-4010",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("41-901.",replacement = "41-9010",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("41-902.",replacement = "41-9020",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("43-5081",replacement = "53-7065",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("45-209.",replacement = "45-2090",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("45-30..",replacement = "45-3031",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("45-402.",replacement = "45-4020",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("47-202.",replacement = "47-2020",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("47-204.",replacement = "47-2040",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("47-205.",replacement = "47-2050",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("47-207.",replacement = "47-2070",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("47-208.",replacement = "47-2080",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("47-213.",replacement = "47-2130",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("47-214.",replacement = "47-2140",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("47-301.",replacement = "47-3010",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("47-501.",replacement = "47-5010",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("47-5021",replacement = "47-5023",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("47-5031",replacement = "47-5032",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("47-504.",replacement = "47-5040",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("47-5081",replacement = "47-50XX",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("47-5051",replacement = "47-50XX",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("47-5061",replacement = "47-5040",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("47-5099",replacement = "47-50XX",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("49-202.",replacement = "49-2020",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("49-209[45]",replacement = "49-209X",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("49-304.",replacement = "49-3040",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("49-305.",replacement = "49-3050",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("49-309.",replacement = "49-3090",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("49-901.",replacement = "49-9010",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("49-904[15]",replacement = "49-904X",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("49-906.",replacement = "49-9060",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("49-909[79]",replacement = "49-909X",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("51-202.",replacement = "51-2020",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("51-209.",replacement = "51-20XX",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("51-302.",replacement = "51-3020",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("51-401.",replacement = "51-9160",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("51-402.",replacement = "51-4020",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("51-403[245]",replacement = "51-403X",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("51-405.",replacement = "51-4050",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("51-406.",replacement = "51-4060",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("51-407.",replacement = "51-4070",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("51-412.",replacement = "51-4120",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("51-4081",replacement = "51-4XXX",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("51-419.",replacement = "51-4XXX",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("51-604.",replacement = "51-6040",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("51-605.",replacement = "51-6050",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("51-606.",replacement = "51-6060",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("51-609[12]",replacement = "51-609X",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("51-703.",replacement = "51-70XX",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("51-801.",replacement = "51-8010",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("51-809.",replacement = "51-8090",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("51-901.",replacement = "51-9010",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("51-902.",replacement = "51-9020",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("51-903.",replacement = "51-9030",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("51-908.",replacement = "51-9080",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("51-912[13]",replacement = "51-9120",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("51-919[23]",replacement = "51-919X",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("51-9141",replacement = "51-91XX",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("51-9199",replacement = "51-91XX",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("53-10..",replacement = "53-1000",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("53-201.",replacement = "53-2010",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("53-202.",replacement = "53-2020",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("53-3021",replacement = "53-3052",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("53-3022",replacement = "53-3051",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("53-401[13]",replacement = "53-4010",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("53-303.",replacement = "53-3030",x =data2$OCC_CODE_CW)

#could not split the mapping
data2$OCC_CODE_CW = sub("53-3041",replacement = "53-3053",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("53-401[13]",replacement = "53-4010",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("53-4012",replacement = "53-40XX",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("53-4021",replacement = "53-40XX",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("53-4041",replacement = "53-40XX",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("53-502.",replacement = "53-5020",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("53-603[12]",replacement = "53-6030",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("53-60[14]1",replacement = "53-60XX",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("53-70[134]1",replacement = "53-70XX",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("53-707[123]",replacement = "53-7070",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("53-7121",replacement = "53-71XX",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("53-713[23]",replacement = "53-71XX",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("53-7151",replacement = "53-71XX",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("53-716[1234]",replacement = "53-71XX",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("53-7181",replacement = "53-71XX",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("53-7033",replacement = "47-5040",x =data2$OCC_CODE_CW)

data2$OCC_CODE_CW = sub("53-7032",replacement = "47-5022",x =data2$OCC_CODE_CW)

#Group the data by the OCC codes taking the weighted average of teleworkable
#(weighted by employment numbers) for any categories that are conglomerated

data2 = data2%>%group_by(OCC_CODE_CW)%>%summarize(weighted_avg_onet =
                                                                sum(teleworkable*TOT_EMP)/
                                                                sum(TOT_EMP),
                                                  TOT_EMP=sum(TOT_EMP))
joined_data = left_join(data1,data2,by=join_by("OCC_CODE" == "OCC_CODE_CW"),keep=TRUE)
joined_data = joined_data%>%select(OCC,TOT_EMP,weighted_avg_onet)
joined_data$OCC = str_remove(joined_data$OCC, "^0+")



#2010 + 2002 encoding
setwd("C:/Users/jmca9/Documents")
data3 <- read_csv("2018_2010SOCBRIDGE.csv")
data4 <- read_csv("2010_2002SOCBRIDGE.csv")

#Delete all rows where all columns are null valued
data3 = data3 %>% filter_all(any_vars(!is.na(.)))
#Turn all nulls in 2010 census code column into strings
data3$`2010 Census Code`[is.na(data3$`2010 Census Code`)]= ""
#Fill down any row with empty strings below it in 2010 census code

data3$`2010 Census Code` = str_remove(data3$`2010 Census Code`, "^0+")
data3$`2018 Census Code` = str_remove(data3$`2018 Census Code`, "^0+")

#police
data3=data3 %>%
  mutate(
    `2018 Census Code` = replace(`2018 Census Code`, (seq_along(`2018 Census Code`)
                                 > which(`2018 Census Code` == 3870) & 
                                   seq_along(`2018 Census Code`)
                                 < which(`2018 Census Code` == 3900)) &
                                   is.na(`2018 Census Code`), 3870))

#Ensure the many to 1 relationships map correctly
#fast food
data3=data3 %>%
  mutate(
    `2018 Census Code` = replace(`2018 Census Code`, (seq_along(`2018 Census Code`)
                                                      > which(`2018 Census Code` == 4055) & 
                                                        seq_along(`2018 Census Code`)
                                                      < which(`2018 Census Code` == 4110)) &
                                   is.na(`2018 Census Code`), 4055))
#forming machine setters
data3=data3 %>%
  mutate(
    `2018 Census Code` = replace(`2018 Census Code`, (seq_along(`2018 Census Code`)
                                                      > which(`2018 Census Code` == 7925) & 
                                                        seq_along(`2018 Census Code`)
                                                      < which(`2018 Census Code` == 7950)) &
                                   is.na(`2018 Census Code`), 7925))
#other machine tool
data3=data3 %>%
  mutate(
    `2018 Census Code` = replace(`2018 Census Code`, (seq_along(`2018 Census Code`)
                                                      > which(`2018 Census Code` == 8025) & 
                                                        seq_along(`2018 Census Code`)
                                                      < which(`2018 Census Code` == 8030)) &
                                   is.na(`2018 Census Code`), 8025))
#other metal workers
data3=data3 %>%
  mutate(
    `2018 Census Code` = replace(`2018 Census Code`, (seq_along(`2018 Census Code`)
                                                      > which(`2018 Census Code` == 8225) & 
                                                        seq_along(`2018 Census Code`)
                                                      < which(`2018 Census Code` == 8250)) &
                                   is.na(`2018 Census Code`), 8225))
#fishing and hunting
data3=data3 %>%
  mutate(
    `2018 Census Code` = replace(`2018 Census Code`, (seq_along(`2018 Census Code`)
                                                      > which(`2018 Census Code` == 6115) & 
                                                        seq_along(`2018 Census Code`)
                                                      < which(`2018 Census Code` == 6120)) &
                                   is.na(`2018 Census Code`), 6115))
#shoe and leather
data3=data3 %>%
  mutate(
    `2018 Census Code` = replace(`2018 Census Code`, (seq_along(`2018 Census Code`)
                                                      > which(`2018 Census Code` == 8335) & 
                                                        seq_along(`2018 Census Code`)
                                                      < which(`2018 Census Code` == 8350)) &
                                   is.na(`2018 Census Code`), 8335))
#construction equipment
data3=data3 %>%
  mutate(
    `2018 Census Code` = replace(`2018 Census Code`, (seq_along(`2018 Census Code`)
                                                      > which(`2018 Census Code` == 6305) & 
                                                        seq_along(`2018 Census Code`)
                                                      < which(`2018 Census Code` == 6330)) &
                                   is.na(`2018 Census Code`), 6305))

#painters and paper changers
data3=data3 %>%
  mutate(
    `2018 Census Code` = replace(`2018 Census Code`, (seq_along(`2018 Census Code`)
                                                      > which(`2018 Census Code` == 6410) & 
                                                        seq_along(`2018 Census Code`)
                                                      < which(`2018 Census Code` == 6441)) &
                                   is.na(`2018 Census Code`), 6410))
#mining machine
data3=data3 %>%
  mutate(
    `2018 Census Code` = replace(`2018 Census Code`, (seq_along(`2018 Census Code`)
                                                      > which(`2018 Census Code` == 6850) & 
                                                        seq_along(`2018 Census Code`)
                                                      < which(`2018 Census Code` == 6920)) &
                                   is.na(`2018 Census Code`), 6850))

#textile machine operators
data3=data3 %>%
  mutate(
    `2018 Census Code` = replace(`2018 Census Code`, (seq_along(`2018 Census Code`)
                                                      > which(`2018 Census Code` == 8365) & 
                                                        seq_along(`2018 Census Code`)
                                                      < which(`2018 Census Code` == 8450)) &
                                   is.na(`2018 Census Code`), 8365))
#other textile apparel
data3=data3 %>%
  mutate(
    `2018 Census Code` = replace(`2018 Census Code`, (seq_along(`2018 Census Code`)
                                                      > which(`2018 Census Code` == 8465) & 
                                                        seq_along(`2018 Census Code`)
                                                      < which(`2018 Census Code` == 8500)) &
                                   is.na(`2018 Census Code`), 8465))
#other woodworkers
data3=data3 %>%
  mutate(
    `2018 Census Code` = replace(`2018 Census Code`, (seq_along(`2018 Census Code`)
                                                      > which(`2018 Census Code` == 8555) & 
                                                        seq_along(`2018 Census Code`)
                                                      < which(`2018 Census Code` == 8600)) &
                                   is.na(`2018 Census Code`), 8555))
#other production equipment
data3=data3 %>%
  mutate(
    `2018 Census Code` = replace(`2018 Census Code`, (seq_along(`2018 Census Code`)
                                                      > which(`2018 Census Code` == 8865) & 
                                                        seq_along(`2018 Census Code`)
                                                      < which(`2018 Census Code` == 8990)) &
                                   is.na(`2018 Census Code`), 8865))

#other production workers
data3=data3 %>%
  mutate(
    `2018 Census Code` = replace(`2018 Census Code`, (seq_along(`2018 Census Code`)
                                                      > which(`2018 Census Code` == 8990) & 
                                                        seq_along(`2018 Census Code`)
                                                      < which(`2018 Census Code` == 9005)) &
                                   is.na(`2018 Census Code`), 9005))
#other rail transport
data3=data3 %>%
  mutate(
    `2018 Census Code` = replace(`2018 Census Code`, (seq_along(`2018 Census Code`)
                                                      > which(`2018 Census Code` == 9265) & 
                                                        seq_along(`2018 Census Code`)
                                                      < which(`2018 Census Code` == 9300)) &
                                   is.na(`2018 Census Code`), 9265))

#other transportation workers
data3=data3 %>%
  mutate(
    `2018 Census Code` = replace(`2018 Census Code`, (seq_along(`2018 Census Code`)
                                                      > which(`2018 Census Code` == 9430) & 
                                                        seq_along(`2018 Census Code`)
                                                      < which(`2018 Census Code` == 9510)) &
                                   is.na(`2018 Census Code`), 9430))
#conveyor, dredge, and hoist
data3=data3 %>%
  mutate(
    `2018 Census Code` = replace(`2018 Census Code`, (seq_along(`2018 Census Code`)
                                                      > which(`2018 Census Code` == 9570) & 
                                                        seq_along(`2018 Census Code`)
                                                      < which(`2018 Census Code` == 9600)) &
                                   is.na(`2018 Census Code`), 9570))

#other material moving
data3=data3 %>%
  mutate(
    `2018 Census Code` = replace(`2018 Census Code`, (seq_along(`2018 Census Code`)
                                                      > which(`2018 Census Code` == 9760) & 
                                                        seq_along(`2018 Census Code`)
                                                      < which(`2018 Census Code` == 9800)) &
                                   is.na(`2018 Census Code`), 9760))

data3=data3 %>%
  mutate(
    `2018 Census Code` = replace(`2018 Census Code`, (seq_along(`2018 Census Code`)
                                                      > which(`2018 Census Code` == 7640) & 
                                                        seq_along(`2018 Census Code`)
                                                      < which(`2018 Census Code` == 7700)) &
                                   is.na(`2018 Census Code`), 7640))

#
data3=data3 %>%
  mutate(
    `2018 Census Code` = replace(`2018 Census Code`, (seq_along(`2018 Census Code`)
                                                      > which(`2018 Census Code` == 6920) & 
                                                        seq_along(`2018 Census Code`)
                                                      < which(`2018 Census Code` == 6950)) &
                                   is.na(`2018 Census Code`), 6950))

#delete useless rows
data3 = data3 %>% filter(!(`2010 Census Code` == "" & `2018 Census Code` == 3870))

data3 = data3 %>% filter(!(`2010 Census Code` == "" & `2018 Census Code` == 4055))

data3 = data3 %>% filter(!(`2010 Census Code` == "" & `2018 Census Code` == 7925))

data3 = data3 %>% filter(!(`2010 Census Code` == "" & `2018 Census Code` == 8025))

data3 = data3 %>% filter(!(`2010 Census Code` == "" & `2018 Census Code` == 8225))

data3 = data3 %>% filter(!(`2010 Census Code` == "" & `2018 Census Code` == 6115))

data3 = data3 %>% filter(!(`2010 Census Code` == "" & `2018 Census Code` == 8335))

data3 = data3 %>% filter(!(`2010 Census Code` == "" & `2018 Census Code` == 6305))

data3 = data3 %>% filter(!(`2010 Census Code` == "" & `2018 Census Code` == 6410))

data3 = data3 %>% filter(!(`2010 Census Code` == "" & `2018 Census Code` == 6850))

data3 = data3 %>% filter(!(`2010 Census Code` == "" & `2018 Census Code` == 8365))

data3 = data3 %>% filter(!(`2010 Census Code` == "" & `2018 Census Code` == 8025))

data3 = data3 %>% filter(!(`2010 Census Code` == "" & `2018 Census Code` == 8465))

data3 = data3 %>% filter(!(`2010 Census Code` == "" & `2018 Census Code` == 8555))

data3 = data3 %>% filter(!(`2010 Census Code` == "" & `2018 Census Code` == 8865))

data3 = data3 %>% filter(!(`2010 Census Code` == "" & `2018 Census Code` == 8025))

data3 = data3 %>% filter(!(`2010 Census Code` == "" & `2018 Census Code` == 9005))

data3 = data3 %>% filter(!(`2010 Census Code` == "" & `2018 Census Code` == 9265))

data3 = data3 %>% filter(!(`2010 Census Code` == "" & `2018 Census Code` == 9430))

data3 = data3 %>% filter(!(`2010 Census Code` == "" & `2018 Census Code` == 9570))

data3 = data3 %>% filter(!(`2010 Census Code` == "" & `2018 Census Code` == 8025))

data3 = data3 %>% filter(!(`2010 Census Code` == "" & `2018 Census Code` == 9760))

data3 = data3 %>% filter(!(`2010 Census Code` == "" & `2018 Census Code` == 7640))

data3 = data3 %>% filter(!(`2010 Census Code` == "" & `2018 Census Code` == 6950))


#Ensure one to many relationships map correctly
data3 = data3 %>%
  mutate(`2010 Census Code` = replace(`2010 Census Code`, 
                                      which(`2010 Census Code` == "") , NA)) %>%
  tidyr::fill(`2010 Census Code`, .direction = "down")
#delete one unnecessary row
data3 = data3 %>% filter(!(`2010 Census Code` == 6440 & `2018 Census Code` == 6410))

#Remove any remaining columns with nulls (don't have mappings)
data3 = na.omit(data3)

#Add a frequency variable to the 2018 2010 crosswalk
data3 = data3 %>% add_count(`2010 Census Code`, sort = FALSE, name= "freq_2010")




#same logic as above
data4 = data4 %>% filter_all(any_vars(!is.na(.)))
data4$`2002 Census Code`[is.na(data4$`2002 Census Code`)]= ""
data4$`2010 Census Code` = str_remove(data4$`2010 Census Code`, "^0+")
data4$`2002 Census Code` = str_remove(data4$`2002 Census Code`, "^0+")


#Ensure many to one relationships map correctly
data4=data4 %>%
  mutate(
    `2010 Census Code` = replace(`2010 Census Code`, (seq_along(`2010 Census Code`)
                                                      > which(`2010 Census Code` == 205) & 
                                                        seq_along(`2010 Census Code`)
                                                      < which(`2010 Census Code` == 220)) &
                                   is.na(`2010 Census Code`), 205))

data4=data4 %>%
  mutate(
    `2010 Census Code` = replace(`2010 Census Code`, (seq_along(`2010 Census Code`)
                                                      > which(`2010 Census Code` == 4540) & 
                                                        seq_along(`2010 Census Code`)
                                                      < which(`2010 Census Code` == 4600)) &
                                   is.na(`2010 Census Code`), 4540))

#Ensure one to many relationships map correctly
data4 = data4 %>%
  mutate(`2002 Census Code` = replace(`2002 Census Code`, 
                                      which(`2002 Census Code` == "") , NA)) %>%
  tidyr::fill(`2002 Census Code`, .direction = "down")
#delete useless rows
data4 = na.omit(data4)
#add frequency variable 
data4 = data4 %>% add_count(`2002 Census Code`, sort = FALSE, name= "freq_2002")

#join the crosswalks 2002 and 2010 and 2010 and 2018
full_bridge = full_join(data3,data4, join_by("2010 Census Code"), relationship =
                          "many-to-many")

#join the crosswalks with the employment counts and teleworkability index
joined_data = left_join(full_bridge, joined_data, join_by("2018 Census Code"=="OCC"))




# #Join the Nieman and Dingel mapping with the 2018 to 2010 mapping
# joined_data = left_join(joined_data, data3,by=join_by("OCC"=="2018 Census Code"))
# #Join the 2018 to 2010 mapping with the 2010 to 2002 mapping
# joined_data = left_join(joined_data, data4,by=join_by("2010 Census Code"))

# #Remove the leading 0s in all SOC codes to match with the IPUMS output
# joined_data$OCC = str_remove(joined_data$OCC, "^0+")
# 
# joined_data$`2010 Census Code` = str_remove(joined_data$`2010 Census Code`, "^0+")
# 
# joined_data$`2002 Census Code` = str_remove(joined_data$`2002 Census Code`, "^0+")

joined_data = na.omit(joined_data)

joined_data_2018 = joined_data%>%group_by(`2018 Census Code`)%>%summarize(weighted_avg_onet =
                                                 sum(weighted_avg_onet*TOT_EMP)/
                                                 sum(TOT_EMP))

joined_data_2010 = joined_data%>%group_by(`2010 Census Code`)%>%summarize(weighted_avg_onet =
                                                             sum(weighted_avg_onet*TOT_EMP)/
                                                             sum(TOT_EMP))

joined_data_2002 = joined_data%>%group_by(`2002 Census Code`)%>%summarize(weighted_avg_onet =
                                                             sum(weighted_avg_onet*TOT_EMP)/
                                                             sum(TOT_EMP))


#Final merges
setwd("C:/Users/jmca9/Documents")
data <- read_csv("worksoc_reg.csv")
data$OCC = as.character(data$OCC)

#Split the data up according to the years the census codes were used
data_2002 = data%>%filter(YEAR>=2003 & YEAR<=2010)
data_2010 = data%>%filter(YEAR>=2011 & YEAR<=2019)
data_2018 = data%>%filter(YEAR>=2020)

#Join the data subsets with the appropriate census codes
df1 = left_join(data_2002, joined_data_2002, join_by("OCC" == "2002 Census Code"))
df2 = left_join(data_2010, joined_data_2010, join_by("OCC" == "2010 Census Code"))
df3 = left_join(data_2018, joined_data_2018, join_by("OCC" == "2018 Census Code"))


#Put the three dataframes back together into 1 big one
df = rbind(df1,df2,df3)
df = df %>% filter(OCC != 99999)
df = df%>%filter(!is.na(weighted_avg_onet))
summary(df)
# df = unique(df%>%filter(is.na(weighted_avg_onet))%>%select(OCC,YEAR))

# 2SLS WITH ALL CONTROLS, NO INTERACTIONS (COLUMNS 2 AND 3)
FE2SLS1 <- feols(TIME_W_FRIENDS1 ~ factor(MONTH)+factor(YEAR)
                   + AGE + I(AGE^2) + MALE + HSGRAD + SOMECOL +    
                   COLFINISHED + COLPLUS + WHITE + BLACK + ASIAN + 
                   hispanic + for_born + MARRIED + DIV_SEP + 
                   WIDOWED + YNGCHG18 + YNGCH7_18 + YNGCHLEQ6 + 
                   WEEKEND_HOLIDAY + WORKING |
                   wfmdur ~ weighted_avg_onet,
                   cluster = "OCC",
                   data = df)

summary(FE2SLS1)





FE2SLS2 <- feols(TIME_W_FRIENDS3 ~ factor(MONTH)+
                   + AGE + I(AGE^2) + MALE + HSGRAD + SOMECOL +    
                   COLFINISHED + COLPLUS + WHITE + BLACK + ASIAN + 
                   hispanic + for_born + MARRIED + DIV_SEP + 
                   WIDOWED + YNGCHG18 + YNGCH7_18 + YNGCHLEQ6 + 
                   WEEKEND_HOLIDAY + WORKING |
                   factor(OCC) + factor(YEAR)|
                   wfmdur ~ weighted_avg_onet,
                 data = df)

summary(FE2SLS2)

FE2SLS3 <- feols(TIME_W_FRIENDS4 ~ factor(MONTH)+
                   + AGE + I(AGE^2) + MALE + HSGRAD + SOMECOL +    
                   COLFINISHED + COLPLUS + WHITE + BLACK + ASIAN + 
                   hispanic + for_born + MARRIED + DIV_SEP + 
                   WIDOWED + YNGCHG18 + YNGCH7_18 + YNGCHLEQ6 + 
                   WEEKEND_HOLIDAY + WORKING |
                   factor(OCC) + factor(YEAR)|
                   wfmdur ~ weighted_avg_onet,
                 data = df)

summary(FE2SLS3)


FE2SLS4 <- feols(TIME_W_FRIENDS1 ~ factor(MONTH)+
                   + AGE + I(AGE^2) + MALE + HSGRAD + SOMECOL +    
                   COLFINISHED + COLPLUS + WHITE + BLACK + ASIAN + 
                   hispanic + for_born + MARRIED + DIV_SEP + 
                   WIDOWED + YNGCHG18 + YNGCH7_18 + YNGCHLEQ6 + 
                   WEEKEND_HOLIDAY|
                   factor (OCC) + factor(YEAR)|
                   wfhfraction ~ weighted_avg_onet,
                 data = df)

summary(FE2SLS4)

help(feols)

FE2SLS5 <- feols(TIME_W_FRIENDS3 ~ factor(MONTH)+
                   + AGE + I(AGE^2) + MALE + HSGRAD + SOMECOL +    
                   COLFINISHED + COLPLUS + WHITE + BLACK + ASIAN + 
                   hispanic + for_born + MARRIED + DIV_SEP + 
                   WIDOWED + YNGCHG18 + YNGCH7_18 + YNGCHLEQ6 + 
                   WEEKEND_HOLIDAY|
                   factor(OCC) + factor(YEAR)|
                   wfhfraction ~ weighted_avg_onet,
                 data = df)

summary(FE2SLS5)

FE2SLS6 <- feols(TIME_W_FRIENDS4 ~ factor(MONTH)+
                   + AGE + I(AGE^2) + MALE + HSGRAD + SOMECOL +    
                   COLFINISHED + COLPLUS + WHITE + BLACK + ASIAN + 
                   hispanic + for_born + MARRIED + DIV_SEP + 
                   WIDOWED + YNGCHG18 + YNGCH7_18 + YNGCHLEQ6 + 
                   WEEKEND_HOLIDAY|
                   factor(OCC) + factor(YEAR)|
                   wfhfraction ~ weighted_avg_onet,
                 data = df)

summary(FE2SLS6)

FE2SLS7 <- feols(TIME_W_FRIENDS1 ~ factor(MONTH)+
                   + AGE + I(AGE^2) + MALE + HSGRAD + SOMECOL +    
                   COLFINISHED + COLPLUS + WHITE + BLACK + ASIAN + 
                   hispanic + for_born + MARRIED + DIV_SEP + 
                   WIDOWED + YNGCHG18 + YNGCH7_18 + YNGCHLEQ6 + 
                   WEEKEND_HOLIDAY + WORKING|
                   factor(OCC) + factor(YEAR)|
                   ANY_wfh ~ weighted_avg_onet,
                 data = df)

summary(FE2SLS7)

FE2SLS8 <- feols(TIME_W_FRIENDS3 ~ factor(MONTH)+
                   + AGE + I(AGE^2) + MALE + HSGRAD + SOMECOL +    
                   COLFINISHED + COLPLUS + WHITE + BLACK + ASIAN + 
                   hispanic + for_born + MARRIED + DIV_SEP + 
                   WIDOWED + YNGCHG18 + YNGCH7_18 + YNGCHLEQ6 + 
                   WEEKEND_HOLIDAY +WORKING|
                   factor(OCC) + factor(YEAR)|
                   ANY_wfh ~ weighted_avg_onet,
                 data = df)

summary(FE2SLS8)

FE2SLS9 <- feols(TIME_W_FRIENDS4 ~ factor(MONTH)+
                   + AGE + I(AGE^2) + MALE + HSGRAD + SOMECOL +    
                   COLFINISHED + COLPLUS + WHITE + BLACK + ASIAN + 
                   hispanic + for_born + MARRIED + DIV_SEP + 
                   WIDOWED + YNGCHG18 + YNGCH7_18 + YNGCHLEQ6 + 
                   WEEKEND_HOLIDAY + WORKING|
                   factor(OCC) + factor(YEAR)|
                   ANY_wfh ~ weighted_avg_onet,
                 data = df)

summary(FE2SLS9)




#2SLS WITH ALL CONTROLS AND INTERACTIONS (COLUMNS 6 AND 7)

FE2SLS10 <- feols(TIME_W_FRIENDS1 ~ factor(MONTH) * POST_COVID+factor(YEAR)
                    + AGE * POST_COVID + I(AGE^2) * POST_COVID + 
                    MALE * POST_COVID + HSGRAD * POST_COVID + 
                    SOMECOL * POST_COVID + COLFINISHED * POST_COVID + 
                    COLPLUS * POST_COVID + WHITE * POST_COVID +
                    BLACK * POST_COVID + ASIAN * POST_COVID + 
                    hispanic * POST_COVID + for_born * POST_COVID +
                    MARRIED * POST_COVID + DIV_SEP * POST_COVID + 
                    WIDOWED * POST_COVID + YNGCHG18 * POST_COVID + 
                    YNGCH7_18 * POST_COVID + YNGCHLEQ6 * POST_COVID + 
                    WEEKEND_HOLIDAY * POST_COVID + WORKING * POST_COVID|
                    wfmdur + wfmdur:POST_COVID ~
                    weighted_avg_onet + weighted_avg_onet:POST_COVID,
                  cluster = "OCC",
                 data = df)
summary(FE2SLS10)
FE2SLS10$collin.var

FE2SLS11 <- feols(TIME_W_FRIENDS3 ~ factor(MONTH) * POST_COVID+
                    + AGE * POST_COVID + I(AGE^2) * POST_COVID + 
                    MALE * POST_COVID + HSGRAD * POST_COVID + 
                    SOMECOL * POST_COVID + COLFINISHED * POST_COVID + 
                    COLPLUS * POST_COVID + WHITE * POST_COVID +
                    BLACK * POST_COVID + ASIAN * POST_COVID + 
                    hispanic * POST_COVID + for_born * POST_COVID +
                    MARRIED * POST_COVID + DIV_SEP * POST_COVID + 
                    WIDOWED * POST_COVID + YNGCHG18 * POST_COVID + 
                    YNGCH7_18 * POST_COVID + YNGCHLEQ6 * POST_COVID + 
                    WEEKEND_HOLIDAY * POST_COVID + WORKING * POST_COVID
                  | factor(OCC) + factor(YEAR)|
                    wfmdur + wfmdur:POST_COVID ~
                    weighted_avg_onet + weighted_avg_onet:POST_COVID,
                 data = df)

summary(FE2SLS11)

FE2SLS12 <- feols(TIME_W_FRIENDS4 ~ factor(MONTH) * POST_COVID+
                    + AGE * POST_COVID + I(AGE^2) * POST_COVID + 
                    MALE * POST_COVID + HSGRAD * POST_COVID + 
                    SOMECOL * POST_COVID + COLFINISHED * POST_COVID + 
                    COLPLUS * POST_COVID + WHITE * POST_COVID +
                    BLACK * POST_COVID + ASIAN * POST_COVID + 
                    hispanic * POST_COVID + for_born * POST_COVID +
                    MARRIED * POST_COVID + DIV_SEP * POST_COVID + 
                    WIDOWED * POST_COVID + YNGCHG18 * POST_COVID + 
                    YNGCH7_18 * POST_COVID + YNGCHLEQ6 * POST_COVID + 
                    WEEKEND_HOLIDAY * POST_COVID + WORKING * POST_COVID
                  | factor(OCC) + factor(YEAR)|
                    wfmdur + wfmdur:POST_COVID ~
                    weighted_avg_onet + weighted_avg_onet:POST_COVID,
                 data = df)

summary(FE2SLS12)


FE2SLS13 <- feols(TIME_W_FRIENDS1 ~ factor(MONTH) * POST_COVID+
                    + AGE * POST_COVID + I(AGE^2) * POST_COVID + 
                    MALE * POST_COVID + HSGRAD * POST_COVID + 
                    SOMECOL * POST_COVID + COLFINISHED * POST_COVID + 
                    COLPLUS * POST_COVID + WHITE * POST_COVID +
                    BLACK * POST_COVID + ASIAN * POST_COVID + 
                    hispanic * POST_COVID + for_born * POST_COVID +
                    MARRIED * POST_COVID + DIV_SEP * POST_COVID + 
                    WIDOWED * POST_COVID + YNGCHG18 * POST_COVID + 
                    YNGCH7_18 * POST_COVID + YNGCHLEQ6 * POST_COVID + 
                    WEEKEND_HOLIDAY * POST_COVID + WORKING * POST_COVID
                  | factor(OCC) + factor(YEAR)|
                    wfhfraction + wfhfraction:POST_COVID ~
                    weighted_avg_onet + weighted_avg_onet:POST_COVID,
                 data = df)

summary(FE2SLS13)

FE2SLS14 <- feols(TIME_W_FRIENDS3 ~ factor(MONTH) * POST_COVID+
                    + AGE * POST_COVID + I(AGE^2) * POST_COVID + 
                    MALE * POST_COVID + HSGRAD * POST_COVID + 
                    SOMECOL * POST_COVID + COLFINISHED * POST_COVID + 
                    COLPLUS * POST_COVID + WHITE * POST_COVID +
                    BLACK * POST_COVID + ASIAN * POST_COVID + 
                    hispanic * POST_COVID + for_born * POST_COVID +
                    MARRIED * POST_COVID + DIV_SEP * POST_COVID + 
                    WIDOWED * POST_COVID + YNGCHG18 * POST_COVID + 
                    YNGCH7_18 * POST_COVID + YNGCHLEQ6 * POST_COVID + 
                    WEEKEND_HOLIDAY * POST_COVID + WORKING * POST_COVID
                  | factor(OCC) + factor(YEAR)|
                    wfhfraction + wfhfraction:POST_COVID ~
                    weighted_avg_onet + weighted_avg_onet:POST_COVID,
                 data = df)

summary(FE2SLS14)

FE2SLS15 <- feols(TIME_W_FRIENDS4 ~ factor(MONTH) * POST_COVID+
                    + AGE * POST_COVID + I(AGE^2) * POST_COVID + 
                    MALE * POST_COVID + HSGRAD * POST_COVID + 
                    SOMECOL * POST_COVID + COLFINISHED * POST_COVID + 
                    COLPLUS * POST_COVID + WHITE * POST_COVID +
                    BLACK * POST_COVID + ASIAN * POST_COVID + 
                    hispanic * POST_COVID + for_born * POST_COVID +
                    MARRIED * POST_COVID + DIV_SEP * POST_COVID + 
                    WIDOWED * POST_COVID + YNGCHG18 * POST_COVID + 
                    YNGCH7_18 * POST_COVID + YNGCHLEQ6 * POST_COVID + 
                    WEEKEND_HOLIDAY * POST_COVID + WORKING * POST_COVID
                  | factor(OCC) + factor(YEAR)|
                   wfhfraction + wfhfraction:POST_COVID ~
                    weighted_avg_onet + weighted_avg_onet:POST_COVID,
                 data = df)

summary(FE2SLS15)

FE2SLS16 <- feols(TIME_W_FRIENDS1 ~ factor(MONTH) * POST_COVID+
                    + AGE * POST_COVID + I(AGE^2) * POST_COVID + 
                    MALE * POST_COVID + HSGRAD * POST_COVID + 
                    SOMECOL * POST_COVID + COLFINISHED * POST_COVID + 
                    COLPLUS * POST_COVID + WHITE * POST_COVID +
                    BLACK * POST_COVID + ASIAN * POST_COVID + 
                    hispanic * POST_COVID + for_born * POST_COVID +
                    MARRIED * POST_COVID + DIV_SEP * POST_COVID + 
                    WIDOWED * POST_COVID + YNGCHG18 * POST_COVID + 
                    YNGCH7_18 * POST_COVID + YNGCHLEQ6 * POST_COVID + 
                    WEEKEND_HOLIDAY * POST_COVID + WORKING * POST_COVID
                  | factor(OCC) + factor(YEAR)|
                    ANY_wfh:POST_COVID + ANY_wfh ~ 
                    weighted_avg_onet:POST_COVID + weighted_avg_onet,
                  data = df)

summary(FE2SLS16)

FE2SLS17 <- feols(TIME_W_FRIENDS3 ~ factor(MONTH) * POST_COVID+
                    + AGE * POST_COVID + I(AGE^2) * POST_COVID + 
                    MALE * POST_COVID + HSGRAD * POST_COVID + 
                    SOMECOL * POST_COVID + COLFINISHED * POST_COVID + 
                    COLPLUS * POST_COVID + WHITE * POST_COVID +
                    BLACK * POST_COVID + ASIAN * POST_COVID + 
                    hispanic * POST_COVID + for_born * POST_COVID +
                    MARRIED * POST_COVID + DIV_SEP * POST_COVID + 
                    WIDOWED * POST_COVID + YNGCHG18 * POST_COVID + 
                    YNGCH7_18 * POST_COVID + YNGCHLEQ6 * POST_COVID + 
                    WEEKEND_HOLIDAY * POST_COVID + WORKING * POST_COVID
                  | factor(OCC) + factor(YEAR)|
                    ANY_wfh:POST_COVID + ANY_wfh ~ 
                    weighted_avg_onet:POST_COVID + weighted_avg_onet,
                  data = df)

summary(FE2SLS17)

FE2SLS18 <- feols(TIME_W_FRIENDS4 ~ factor(MONTH) * POST_COVID+
                   + AGE * POST_COVID + I(AGE^2) * POST_COVID + 
                   MALE * POST_COVID + HSGRAD * POST_COVID + 
                   SOMECOL * POST_COVID + COLFINISHED * POST_COVID + 
                   COLPLUS * POST_COVID + WHITE * POST_COVID +
                   BLACK * POST_COVID + ASIAN * POST_COVID + 
                   hispanic * POST_COVID + for_born * POST_COVID +
                   MARRIED * POST_COVID + DIV_SEP * POST_COVID + 
                   WIDOWED * POST_COVID + YNGCHG18 * POST_COVID + 
                   YNGCH7_18 * POST_COVID + YNGCHLEQ6 * POST_COVID + 
                   WEEKEND_HOLIDAY * POST_COVID + WORKING * POST_COVID
                   | factor(OCC) + factor(YEAR)|
                   ANY_wfh:POST_COVID + ANY_wfh ~ 
                    weighted_avg_onet:POST_COVID + weighted_avg_onet,
                 data = df)

summary(FE2SLS18)

#OLS WITHOUT INTERACTIONS (COLUMN 1)
reg19 = feols(TIME_W_FRIENDS1 ~ wfmdur +factor(MONTH)+
             factor(WEEKEND_HOLIDAY) + AGE + I(AGE^2) + MALE + HSGRAD + SOMECOL + 
             COLFINISHED + COLPLUS + WHITE + BLACK + ASIAN + hispanic + for_born + 
             MARRIED + DIV_SEP + WIDOWED + YNGCHG18 + YNGCH7_18 + YNGCHLEQ6 + 
             WEEKEND_HOLIDAY + WORKING| factor(OCC) + factor(YEAR),data=df)
summary(reg19)


reg20 = feols(TIME_W_FRIENDS3 ~ wfmdur +factor(MONTH)+
             + AGE + I(AGE^2) + MALE + HSGRAD + SOMECOL +     
             COLFINISHED + COLPLUS + WHITE + BLACK + ASIAN + hispanic + for_born + 
             MARRIED + DIV_SEP + WIDOWED + YNGCHG18 + YNGCH7_18 + YNGCHLEQ6 +
             WEEKEND_HOLIDAY + WORKING| factor(OCC) + factor(YEAR),data=df)


reg21 = feols(TIME_W_FRIENDS4 ~ wfmdur +factor(MONTH)+
             + AGE + I(AGE^2) + MALE + HSGRAD + SOMECOL +      
             COLFINISHED + COLPLUS + WHITE + BLACK + ASIAN + hispanic + for_born + 
             MARRIED + DIV_SEP + WIDOWED + YNGCHG18 + YNGCH7_18 + YNGCHLEQ6 + 
             WEEKEND_HOLIDAY + WORKING| factor(OCC) + factor(YEAR),data=df)

reg22 = feols(TIME_W_FRIENDS1 ~ wfhfraction +factor(MONTH)+
                factor(WEEKEND_HOLIDAY) + AGE + I(AGE^2) + MALE + HSGRAD + SOMECOL + 
                COLFINISHED + COLPLUS + WHITE + BLACK + ASIAN + hispanic + for_born + 
                MARRIED + DIV_SEP + WIDOWED + YNGCHG18 + YNGCH7_18 + YNGCHLEQ6 + 
                WEEKEND_HOLIDAY + WORKING| factor(OCC) + factor(YEAR),data=df)
summary(reg22)


reg23 = feols(TIME_W_FRIENDS3 ~ wfhfraction +factor(MONTH)+
                + AGE + I(AGE^2) + MALE + HSGRAD + SOMECOL +     
                COLFINISHED + COLPLUS + WHITE + BLACK + ASIAN + hispanic + for_born + 
                MARRIED + DIV_SEP + WIDOWED + YNGCHG18 + YNGCH7_18 + YNGCHLEQ6 +
                WEEKEND_HOLIDAY + WORKING| factor(OCC) + factor(YEAR),data=df)


reg24 = feols(TIME_W_FRIENDS4 ~ wfhfraction +factor(MONTH)+
                + AGE + I(AGE^2) + MALE + HSGRAD + SOMECOL +      
                COLFINISHED + COLPLUS + WHITE + BLACK + ASIAN + hispanic + for_born + 
                MARRIED + DIV_SEP + WIDOWED + YNGCHG18 + YNGCH7_18 + YNGCHLEQ6 + 
                WEEKEND_HOLIDAY + WORKING| factor(OCC) + factor(YEAR),data=df)

reg25 = feols(TIME_W_FRIENDS1 ~ ANY_wfh +factor(MONTH)+
                factor(WEEKEND_HOLIDAY) + AGE + I(AGE^2) + MALE + HSGRAD + SOMECOL + 
                COLFINISHED + COLPLUS + WHITE + BLACK + ASIAN + hispanic + for_born + 
                MARRIED + DIV_SEP + WIDOWED + YNGCHG18 + YNGCH7_18 + YNGCHLEQ6 + 
                WEEKEND_HOLIDAY + WORKING| factor(OCC) + factor(YEAR),data=df)
summary(reg25)


reg26 = feols(TIME_W_FRIENDS3 ~ ANY_wfh +factor(MONTH)+
                + AGE + I(AGE^2) + MALE + HSGRAD + SOMECOL +     
                COLFINISHED + COLPLUS + WHITE + BLACK + ASIAN + hispanic + for_born + 
                MARRIED + DIV_SEP + WIDOWED + YNGCHG18 + YNGCH7_18 + YNGCHLEQ6 +
                WEEKEND_HOLIDAY + WORKING| factor(OCC) + factor(YEAR),data=df)


reg27 = feols(TIME_W_FRIENDS4 ~ ANY_wfh +factor(MONTH)+
                + AGE + I(AGE^2) + MALE + HSGRAD + SOMECOL +      
                COLFINISHED + COLPLUS + WHITE + BLACK + ASIAN + hispanic + for_born + 
                MARRIED + DIV_SEP + WIDOWED + YNGCHG18 + YNGCH7_18 + YNGCHLEQ6 + 
                WEEKEND_HOLIDAY + WORKING| factor(OCC) + factor(YEAR),data=df)



#OLS WITH ALL INTERACTIONS AND CONTROLS (COLUMN 4)
reg28 = feols(TIME_W_FRIENDS1 ~ wfmdur * POST_COVID +factor(MONTH)* POST_COVID+
             + AGE* POST_COVID + I(AGE^2)* POST_COVID + MALE* POST_COVID + HSGRAD* POST_COVID + SOMECOL* POST_COVID +    
             COLFINISHED* POST_COVID + COLPLUS* POST_COVID + WHITE* POST_COVID + BLACK * POST_COVID+ ASIAN* POST_COVID + hispanic* POST_COVID + for_born* POST_COVID +
             MARRIED* POST_COVID + DIV_SEP* POST_COVID + WIDOWED* POST_COVID + YNGCHG18* POST_COVID + YNGCH7_18* POST_COVID + YNGCHLEQ6* POST_COVID + WEEKEND_HOLIDAY* POST_COVID
           + WORKING * POST_COVID |factor(OCC)+ factor(YEAR),data=df)

reg29 = feols(TIME_W_FRIENDS3 ~ wfmdur * POST_COVID +factor(MONTH)* POST_COVID+
             + AGE* POST_COVID + I(AGE^2)* POST_COVID + MALE* POST_COVID + HSGRAD* POST_COVID + SOMECOL* POST_COVID +    
             COLFINISHED* POST_COVID + COLPLUS* POST_COVID + WHITE* POST_COVID + BLACK * POST_COVID+ ASIAN* POST_COVID + hispanic* POST_COVID + for_born* POST_COVID +
             MARRIED* POST_COVID + DIV_SEP* POST_COVID + WIDOWED* POST_COVID + YNGCHG18* POST_COVID + YNGCH7_18* POST_COVID + YNGCHLEQ6* POST_COVID + WEEKEND_HOLIDAY* POST_COVID
           + WORKING * POST_COVID|factor(OCC) + factor(YEAR),data=df)

reg30 = feols(TIME_W_FRIENDS4 ~ wfmdur * POST_COVID +factor(MONTH)* POST_COVID+
             + AGE* POST_COVID + I(AGE^2)* POST_COVID + MALE* POST_COVID + HSGRAD* POST_COVID + SOMECOL* POST_COVID +    
             COLFINISHED* POST_COVID + COLPLUS* POST_COVID + WHITE* POST_COVID + BLACK * POST_COVID+ ASIAN* POST_COVID + hispanic* POST_COVID + for_born* POST_COVID +
             MARRIED* POST_COVID + DIV_SEP* POST_COVID + WIDOWED* POST_COVID + YNGCHG18* POST_COVID + YNGCH7_18* POST_COVID + YNGCHLEQ6* POST_COVID + WEEKEND_HOLIDAY* POST_COVID
           + WORKING * POST_COVID|factor(OCC) + factor(YEAR),data=df)

reg31 = feols(TIME_W_FRIENDS1 ~ wfhfraction * POST_COVID ++factor(MONTH)* POST_COVID+
             + AGE* POST_COVID + I(AGE^2)* POST_COVID + MALE* POST_COVID + HSGRAD* POST_COVID + SOMECOL* POST_COVID +    
             COLFINISHED* POST_COVID + COLPLUS* POST_COVID + WHITE* POST_COVID + BLACK * POST_COVID+ ASIAN* POST_COVID + hispanic* POST_COVID + for_born* POST_COVID +
             MARRIED* POST_COVID + DIV_SEP* POST_COVID + WIDOWED* POST_COVID + YNGCHG18* POST_COVID + YNGCH7_18* POST_COVID + YNGCHLEQ6* POST_COVID + WEEKEND_HOLIDAY* POST_COVID
           + WORKING * POST_COVID|factor(OCC)  + factor(YEAR),data=df)

reg32 = feols(TIME_W_FRIENDS3 ~ wfhfraction * POST_COVID +factor(MONTH)* POST_COVID+
             + AGE* POST_COVID + I(AGE^2)* POST_COVID + MALE* POST_COVID + HSGRAD* POST_COVID + SOMECOL* POST_COVID +    
             COLFINISHED* POST_COVID + COLPLUS* POST_COVID + WHITE* POST_COVID + BLACK * POST_COVID+ ASIAN* POST_COVID + hispanic* POST_COVID + for_born* POST_COVID +
             MARRIED* POST_COVID + DIV_SEP* POST_COVID + WIDOWED* POST_COVID + YNGCHG18* POST_COVID + YNGCH7_18* POST_COVID + YNGCHLEQ6* POST_COVID + WEEKEND_HOLIDAY* POST_COVID
           + WORKING * POST_COVID|factor(OCC) + factor(YEAR),data=df)

reg33 = feols(TIME_W_FRIENDS4 ~ wfhfraction * POST_COVID +factor(MONTH)* POST_COVID+
             + AGE* POST_COVID + I(AGE^2)* POST_COVID + MALE* POST_COVID + HSGRAD* POST_COVID + SOMECOL* POST_COVID +    
             COLFINISHED* POST_COVID + COLPLUS* POST_COVID + WHITE* POST_COVID + BLACK * POST_COVID+ ASIAN* POST_COVID + hispanic* POST_COVID + for_born* POST_COVID +
             MARRIED* POST_COVID + DIV_SEP* POST_COVID + WIDOWED* POST_COVID + YNGCHG18* POST_COVID + YNGCH7_18* POST_COVID + YNGCHLEQ6* POST_COVID + WEEKEND_HOLIDAY* POST_COVID
           + WORKING * POST_COVID|factor(OCC) + factor(YEAR),data=df)

reg34 = feols(TIME_W_FRIENDS1 ~ ANY_wfh * POST_COVID +factor(MONTH)* POST_COVID+
             + AGE* POST_COVID + I(AGE^2)* POST_COVID + MALE* POST_COVID + HSGRAD* POST_COVID + SOMECOL* POST_COVID +    
             COLFINISHED* POST_COVID + COLPLUS* POST_COVID + WHITE* POST_COVID + BLACK * POST_COVID+ ASIAN* POST_COVID + hispanic* POST_COVID + for_born* POST_COVID +
             MARRIED* POST_COVID + DIV_SEP* POST_COVID + WIDOWED* POST_COVID + YNGCHG18* POST_COVID + YNGCH7_18* POST_COVID + YNGCHLEQ6* POST_COVID + WEEKEND_HOLIDAY* POST_COVID
           + WORKING * POST_COVID|factor(OCC) + factor(YEAR),data=df)

reg35 = feols(TIME_W_FRIENDS3 ~ ANY_wfh * POST_COVID+factor(MONTH)* POST_COVID+
             + AGE* POST_COVID + I(AGE^2)* POST_COVID + MALE* POST_COVID + HSGRAD* POST_COVID + SOMECOL* POST_COVID +    
             COLFINISHED* POST_COVID + COLPLUS* POST_COVID + WHITE* POST_COVID + BLACK * POST_COVID+ ASIAN* POST_COVID + hispanic* POST_COVID + for_born* POST_COVID +
             MARRIED* POST_COVID + DIV_SEP* POST_COVID + WIDOWED* POST_COVID + YNGCHG18* POST_COVID + YNGCH7_18* POST_COVID + YNGCHLEQ6* POST_COVID + WEEKEND_HOLIDAY* POST_COVID
           + WORKING * POST_COVID|factor(OCC) + factor(YEAR),data=df)

reg36 = feols(TIME_W_FRIENDS4 ~ ANY_wfh * POST_COVID +factor(MONTH)* POST_COVID+
             + AGE* POST_COVID + I(AGE^2)* POST_COVID + MALE* POST_COVID + HSGRAD* POST_COVID + SOMECOL* POST_COVID +    
             COLFINISHED* POST_COVID + COLPLUS* POST_COVID + WHITE* POST_COVID + BLACK * POST_COVID+ ASIAN* POST_COVID + hispanic* POST_COVID + for_born* POST_COVID +
             MARRIED* POST_COVID + DIV_SEP* POST_COVID + WIDOWED* POST_COVID + YNGCHG18* POST_COVID + YNGCH7_18* POST_COVID + YNGCHLEQ6* POST_COVID + WEEKEND_HOLIDAY* POST_COVID
           + WORKING * POST_COVID|factor(OCC) + factor(YEAR),data=df)



etable(list(reg19, FE2SLS1, reg28,FE2SLS10),stage = 1:2, 
       keep = c("wfmdur","wfmdur:POST_COVID","weighted_avg_onet","weighted_avg_onet x POST_COVID"),
       order = c("!wfmdur:POST_COVID", "wfmdur","!weighted_avg_onet x POST_COVID")
       ,tex=FALSE,fitstat='ivf')

etable(list(reg20, FE2SLS2, reg29,FE2SLS11),stage = 1:2, 
       keep = c("wfmdur","wfmdur:POST_COVID","weighted_avg_onet","weighted_avg_onet x POST_COVID"),
       order = c("!wfmdur:POST_COVID", "wfmdur","!weighted_avg_onet x POST_COVID")
       ,tex=FALSE,fitstat='ivf')

etable(list(reg21, FE2SLS3, reg30,FE2SLS12),stage = 1:2, 
       keep = c("wfmdur","wfmdur:POST_COVID","weighted_avg_onet","weighted_avg_onet x POST_COVID"),
       order = c("!wfmdur:POST_COVID", "wfmdur","!weighted_avg_onet x POST_COVID")
       ,tex=FALSE,fitstat='ivf')

etable(list(reg22, FE2SLS4, reg31,FE2SLS13),stage = 1:2, 
       keep = c("wfhfraction","wfhfraction:POST_COVID","weighted_avg_onet","weighted_avg_onet x POST_COVID"),
       order = c("!wfhfraction:POST_COVID", "wfhfraction","!weighted_avg_onet x POST_COVID"),
       tex=FALSE,fitstat='ivf')

etable(list(reg23, FE2SLS5, reg32,FE2SLS14),stage = 1:2, 
       keep = c("wfhfraction","wfhfraction:POST_COVID","weighted_avg_onet","weighted_avg_onet x POST_COVID"),
       order = c("!wfhfraction:POST_COVID", "wfhfraction","!weighted_avg_onet x POST_COVID")
       ,tex=FALSE,fitstat='ivf')

etable(list(reg24, FE2SLS6, reg33,FE2SLS15),stage = 1:2, 
       keep = c("wfhfraction","wfhfraction:POST_COVID","weighted_avg_onet","weighted_avg_onet x POST_COVID"),
       order = c("!wfhfraction:POST_COVID", "wfhfraction","!weighted_avg_onet x POST_COVID")
       ,tex=FALSE,fitstat='ivf')

etable(list(reg25, FE2SLS7, reg34,FE2SLS16),stage = 1:2, 
       keep = c("ANY_wfh","ANY_wfh:POST_COVID","weighted_avg_onet","weighted_avg_onet x POST_COVID"),
       order = c("!ANY_wfh:POST_COVID", "ANY_wfh","!weighted_avg_onet x POST_COVID")
       ,tex=FALSE,fitstat='ivf')

etable(list(reg26, FE2SLS8, reg35,FE2SLS17),stage = 1:2, 
       keep = c("ANY_wfh","ANY_wfh:POST_COVID","weighted_avg_onet","weighted_avg_onet x POST_COVID"),
       order = c("!ANY_wfh:POST_COVID", "ANY_wfh","!weighted_avg_onet x POST_COVID")
       ,tex=FALSE,fitstat='ivf')

etable(list(reg27, FE2SLS9, reg36,FE2SLS18),stage = 1:2, 
       keep = c("ANY_wfh","ANY_wfh:POST_COVID","weighted_avg_onet","weighted_avg_onet x POST_COVID"),
       order = c("!ANY_wfh:POST_COVID", "ANY_wfh","!weighted_avg_onet x POST_COVID")
       ,tex=FALSE,fitstat='ivf')

