library(readr)
library(tidyverse)
library(stargazer)
rm(list = ls())
setwd("C:/Users/jmca9/Downloads")
data1 <- read_csv("socpumscross.csv")
data2 <- read_csv("onet_teleworkable_blscodes.csv")

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

data2$OCC_CODE = sub("11-2031",replacement = "11-2030",x =data2$OCC_CODE)

data2$OCC_CODE = sub("11-3011",replacement = "11-3012",x =data2$OCC_CODE)

data2$OCC_CODE = sub("11-903.",replacement = "11-9030",x =data2$OCC_CODE)

data2$OCC_CODE = sub("11-9061",replacement = "11-9171",x =data2$OCC_CODE)

data2$OCC_CODE = sub("11-9071",replacement = "11-9070",x =data2$OCC_CODE)

#all of these jobs can be done from home apparently, so I mapped them all
#to a single sub-category
data2$OCC_CODE = sub("13-1020",replacement = "13-1021",x =data2$OCC_CODE)

data2$OCC_CODE = sub("13-103.",replacement = "13-1030",x =data2$OCC_CODE)

data2$OCC_CODE = sub("13-107.",replacement = "13-1070",x =data2$OCC_CODE)

data2$OCC_CODE = sub("13-2021",replacement = "13-2020",x =data2$OCC_CODE)

data2$OCC_CODE = sub("13-207.",replacement = "13-2070",x =data2$OCC_CODE)

data2$OCC_CODE = sub("13-2099",replacement = "13-20XX",x =data2$OCC_CODE)

data2$OCC_CODE = sub("15-1111",replacement = "15-1221",x =data2$OCC_CODE)

data2$OCC_CODE = sub("15-1121",replacement = "15-1211",x =data2$OCC_CODE)

data2$OCC_CODE = sub("15-1122",replacement = "15-1299",x =data2$OCC_CODE)

data2$OCC_CODE = sub("15-1131",replacement = "15-1251",x =data2$OCC_CODE)

data2$OCC_CODE = sub("15-113[23]",replacement = "15-1252",x =data2$OCC_CODE)

data2$OCC_CODE = sub("15-1134",replacement = "15-1254",x =data2$OCC_CODE)

data2$OCC_CODE = sub("15-1141",replacement = "15-124X",x =data2$OCC_CODE)

data2$OCC_CODE = sub("15-1142",replacement = "15-1244",x =data2$OCC_CODE)

data2$OCC_CODE = sub("15-1143",replacement = "15-1241",x =data2$OCC_CODE)

data2$OCC_CODE = sub("15-115[12]",replacement = "15-1230",x =data2$OCC_CODE)

data2$OCC_CODE = sub("15-1199",replacement = "15-1299",x =data2$OCC_CODE)

data2$OCC_CODE = sub("17-102[12]",replacement = "17-1020",x =data2$OCC_CODE)

data2$OCC_CODE = sub("17-207[12]",replacement = "17-2070",x =data2$OCC_CODE)

data2$OCC_CODE = sub("17-211[12]",replacement = "17-2110",x =data2$OCC_CODE)

data2$OCC_CODE = sub("17-301[2-3]",replacement = "17-301X",x =data2$OCC_CODE)

data2$OCC_CODE = sub("17-302[^3]",replacement = "17-302X",x =data2$OCC_CODE)

data2$OCC_CODE = sub("19-101[123]",replacement = "19-1010",x =data2$OCC_CODE)

data2$OCC_CODE = sub("19-102.",replacement = "19-1020",x =data2$OCC_CODE)

data2$OCC_CODE = sub("19-103.",replacement = "19-1030",x =data2$OCC_CODE)

data2$OCC_CODE = sub("19-104.",replacement = "19-1040",x =data2$OCC_CODE)

data2$OCC_CODE = sub("19-201.",replacement = "19-2010",x =data2$OCC_CODE)

data2$OCC_CODE = sub("19-203.",replacement = "19-2030",x =data2$OCC_CODE)

#Can't determine split properly
data2$OCC_CODE = sub("19-3031",replacement = "19-3033",x =data2$OCC_CODE)

data2$OCC_CODE = sub("19-204[2-3]",replacement = "19-204X",x =data2$OCC_CODE)

data2$OCC_CODE = sub("19-303[29]", replacement = "19-303X", x = data2$OCC_CODE)

data2$OCC_CODE = sub("19-309.",replacement = "19-3090",x =data2$OCC_CODE)

data2$OCC_CODE = sub("19-4011",replacement = "19-4010",x =data2$OCC_CODE)

data2$OCC_CODE = sub("19-4041",replacement = "19-4040",x =data2$OCC_CODE)

data2$OCC_CODE = sub("19-4091",replacement = "19-4040",x =data2$OCC_CODE)

data2$OCC_CODE = sub("19-409[^1]",replacement = "19-40XX",x =data2$OCC_CODE)

data2$OCC_CODE = sub("19-4071",replacement = "19-40XX",x =data2$OCC_CODE)

#couldn't separate
data2$OCC_CODE = sub("21-1018",replacement = "21-1011",x =data2$OCC_CODE)

data2$OCC_CODE = sub("21-109[^23]",replacement = "21-109X",x =data2$OCC_CODE)

data2$OCC_CODE = sub("23-102.",replacement = "23-1020",x =data2$OCC_CODE)

data2$OCC_CODE = sub("23-2091",replacement = "27-3092",x =data2$OCC_CODE)

data2$OCC_CODE = sub("25-1[01][0-9][0-9]",replacement = "25-1000",x =data2$OCC_CODE)

data2$OCC_CODE = sub("25-201.",replacement = "25-2010",x =data2$OCC_CODE)

data2$OCC_CODE = sub("25-202.",replacement = "25-2020",x =data2$OCC_CODE)

data2$OCC_CODE = sub("25-203.",replacement = "25-2030",x =data2$OCC_CODE)

data2$OCC_CODE = sub("25-205.",replacement = "25-2050",x =data2$OCC_CODE)

data2$OCC_CODE = sub("25-30..",replacement = "25-30XX",x =data2$OCC_CODE)

data2$OCC_CODE = sub("25-401.",replacement = "25-4010",x =data2$OCC_CODE)

data2$OCC_CODE = sub("25-4021",replacement = "25-4022",x =data2$OCC_CODE)

data2$OCC_CODE = sub("25-9041",replacement = "25-9040",x =data2$OCC_CODE)

data2$OCC_CODE = sub("25-90[^1]1",replacement = "25-90XX",x =data2$OCC_CODE)

data2$OCC_CODE = sub("27-101.",replacement = "27-1010",x =data2$OCC_CODE)

data2$OCC_CODE = sub("27-203.",replacement = "27-2030",x =data2$OCC_CODE)

data2$OCC_CODE = sub("27-1027",replacement = "27-102X",x =data2$OCC_CODE)

data2$OCC_CODE = sub("27-302[12]",replacement = "27-3023",x =data2$OCC_CODE)

data2$OCC_CODE = sub("27-401.",replacement = "27-4010",x =data2$OCC_CODE)

data2$OCC_CODE = sub("27-403.",replacement = "27-4030",x =data2$OCC_CODE)

data2$OCC_CODE = sub("29-102.",replacement = "29-1020",x =data2$OCC_CODE)

data2$OCC_CODE = sub("29-106[79]",replacement = "29-1240",x =data2$OCC_CODE)

data2$OCC_CODE = sub("29-106.",replacement = "29-12XX",x =data2$OCC_CODE)

data2$OCC_CODE = sub("29-1199",replacement = "29-1299",x =data2$OCC_CODE)

data2$OCC_CODE = sub("29-2021",replacement = "29-1292",x =data2$OCC_CODE)

data2$OCC_CODE = sub("29-2033",replacement = "29-203X",x =data2$OCC_CODE)

#couldn't separate
data2$OCC_CODE = sub("29-2041",replacement = "29-2042",x =data2$OCC_CODE)

data2$OCC_CODE = sub("29-205[17]",replacement = "29-205X",x =data2$OCC_CODE)

data2$OCC_CODE = sub("29-2071",replacement = "29-2072",x =data2$OCC_CODE)

data2$OCC_CODE = sub("29-209.",replacement = "29-2090",x =data2$OCC_CODE)

data2$OCC_CODE = sub("29-901.",replacement = "19-5010",x =data2$OCC_CODE)

data2$OCC_CODE = sub("29-909.",replacement = "29-9000",x =data2$OCC_CODE)

data2$OCC_CODE = sub("31-1011",replacement = "31-1121",x =data2$OCC_CODE)

data2$OCC_CODE = sub("31-1014",replacement = "31-1131",x =data2$OCC_CODE)

data2$OCC_CODE = sub("31-101[35]",replacement = "31-113X",x =data2$OCC_CODE)

data2$OCC_CODE = sub("31-201.",replacement = "31-2010",x =data2$OCC_CODE)

data2$OCC_CODE = sub("31-202.",replacement = "31-2020",x =data2$OCC_CODE)

data2$OCC_CODE = sub("31-909[39]",replacement = "31-909X",x =data2$OCC_CODE)

data2$OCC_CODE = sub("33-202.",replacement = "33-2020",x =data2$OCC_CODE)

data2$OCC_CODE = sub("33-305.",replacement = "33-3050",x =data2$OCC_CODE)

data2$OCC_CODE = sub("33-903.",replacement = "33-9030",x =data2$OCC_CODE)

data2$OCC_CODE = sub("33-909[29]",replacement = "33-909X",x =data2$OCC_CODE)

data2$OCC_CODE = sub("35-201.",replacement = "35-2010",x =data2$OCC_CODE)

data2$OCC_CODE = sub("37-201.",replacement = "37-201X",x =data2$OCC_CODE)

data2$OCC_CODE = sub("37-3012",replacement = "37-301X",x =data2$OCC_CODE)

data2$OCC_CODE = sub("39-1...",replacement = "39-1000",x =data2$OCC_CODE)

data2$OCC_CODE = sub("39-301.",replacement = "39-3010",x =data2$OCC_CODE)

data2$OCC_CODE = sub("39-3021",replacement = "39-30XX",x =data2$OCC_CODE)

data2$OCC_CODE = sub("39-309.",replacement = "39-30XX",x =data2$OCC_CODE)

data2$OCC_CODE = sub("39-40[12].",replacement = "39-40XX",x =data2$OCC_CODE)

data2$OCC_CODE = sub("39-509[13]",replacement = "39-509X",x =data2$OCC_CODE)

data2$OCC_CODE = sub("39-601.",replacement = "39-6010",x =data2$OCC_CODE)

data2$OCC_CODE = sub("39-9021",replacement = "31-1122",x =data2$OCC_CODE)

data2$OCC_CODE = sub("41-201.",replacement = "41-2010",x =data2$OCC_CODE)

data2$OCC_CODE = sub("41-3099",replacement = "41-3091",x =data2$OCC_CODE)

data2$OCC_CODE = sub("41-401.",replacement = "41-4010",x =data2$OCC_CODE)

data2$OCC_CODE = sub("41-901.",replacement = "41-9010",x =data2$OCC_CODE)

data2$OCC_CODE = sub("41-902.",replacement = "41-9020",x =data2$OCC_CODE)

data2$OCC_CODE = sub("43-5081",replacement = "53-7065",x =data2$OCC_CODE)

data2$OCC_CODE = sub("45-209.",replacement = "45-2090",x =data2$OCC_CODE)

data2$OCC_CODE = sub("45-30..",replacement = "45-3031",x =data2$OCC_CODE)

data2$OCC_CODE = sub("45-402.",replacement = "45-4020",x =data2$OCC_CODE)

data2$OCC_CODE = sub("47-202.",replacement = "47-2020",x =data2$OCC_CODE)

data2$OCC_CODE = sub("47-204.",replacement = "47-2040",x =data2$OCC_CODE)

data2$OCC_CODE = sub("47-205.",replacement = "47-2050",x =data2$OCC_CODE)

data2$OCC_CODE = sub("47-207.",replacement = "47-2070",x =data2$OCC_CODE)

data2$OCC_CODE = sub("47-208.",replacement = "47-2080",x =data2$OCC_CODE)

data2$OCC_CODE = sub("47-213.",replacement = "47-2130",x =data2$OCC_CODE)

data2$OCC_CODE = sub("47-214.",replacement = "47-2140",x =data2$OCC_CODE)

data2$OCC_CODE = sub("47-301.",replacement = "47-3010",x =data2$OCC_CODE)

data2$OCC_CODE = sub("47-501.",replacement = "47-5010",x =data2$OCC_CODE)

data2$OCC_CODE = sub("47-5021",replacement = "47-5023",x =data2$OCC_CODE)

data2$OCC_CODE = sub("47-5031",replacement = "47-5032",x =data2$OCC_CODE)

data2$OCC_CODE = sub("47-504.",replacement = "47-5040",x =data2$OCC_CODE)

data2$OCC_CODE = sub("47-5081",replacement = "47-50XX",x =data2$OCC_CODE)

data2$OCC_CODE = sub("47-5051",replacement = "47-50XX",x =data2$OCC_CODE)

data2$OCC_CODE = sub("47-5061",replacement = "47-5040",x =data2$OCC_CODE)

data2$OCC_CODE = sub("47-5099",replacement = "47-50XX",x =data2$OCC_CODE)

data2$OCC_CODE = sub("49-202.",replacement = "49-2020",x =data2$OCC_CODE)

data2$OCC_CODE = sub("49-209[45]",replacement = "49-209X",x =data2$OCC_CODE)

data2$OCC_CODE = sub("49-304.",replacement = "49-3040",x =data2$OCC_CODE)

data2$OCC_CODE = sub("49-305.",replacement = "49-3050",x =data2$OCC_CODE)

data2$OCC_CODE = sub("49-309.",replacement = "49-3090",x =data2$OCC_CODE)

data2$OCC_CODE = sub("49-901.",replacement = "49-9010",x =data2$OCC_CODE)

data2$OCC_CODE = sub("49-904[15]",replacement = "49-904X",x =data2$OCC_CODE)

data2$OCC_CODE = sub("49-906.",replacement = "49-9060",x =data2$OCC_CODE)

data2$OCC_CODE = sub("49-909[79]",replacement = "49-909X",x =data2$OCC_CODE)

data2$OCC_CODE = sub("51-202.",replacement = "51-2020",x =data2$OCC_CODE)

data2$OCC_CODE = sub("51-209.",replacement = "51-20XX",x =data2$OCC_CODE)

data2$OCC_CODE = sub("51-302.",replacement = "51-3020",x =data2$OCC_CODE)

data2$OCC_CODE = sub("51-401.",replacement = "51-9160",x =data2$OCC_CODE)

data2$OCC_CODE = sub("51-402.",replacement = "51-4020",x =data2$OCC_CODE)

data2$OCC_CODE = sub("51-403[245]",replacement = "51-403X",x =data2$OCC_CODE)

data2$OCC_CODE = sub("51-405.",replacement = "51-4050",x =data2$OCC_CODE)

data2$OCC_CODE = sub("51-406.",replacement = "51-4060",x =data2$OCC_CODE)

data2$OCC_CODE = sub("51-407.",replacement = "51-4070",x =data2$OCC_CODE)

data2$OCC_CODE = sub("51-412.",replacement = "51-4120",x =data2$OCC_CODE)

data2$OCC_CODE = sub("51-4081",replacement = "51-4XXX",x =data2$OCC_CODE)

data2$OCC_CODE = sub("51-419.",replacement = "51-4XXX",x =data2$OCC_CODE)

data2$OCC_CODE = sub("51-604.",replacement = "51-6040",x =data2$OCC_CODE)

data2$OCC_CODE = sub("51-605.",replacement = "51-6050",x =data2$OCC_CODE)

data2$OCC_CODE = sub("51-606.",replacement = "51-6060",x =data2$OCC_CODE)

data2$OCC_CODE = sub("51-609[12]",replacement = "51-609X",x =data2$OCC_CODE)

data2$OCC_CODE = sub("51-703.",replacement = "51-70XX",x =data2$OCC_CODE)

data2$OCC_CODE = sub("51-801.",replacement = "51-8010",x =data2$OCC_CODE)

data2$OCC_CODE = sub("51-809.",replacement = "51-8090",x =data2$OCC_CODE)

data2$OCC_CODE = sub("51-901.",replacement = "51-9010",x =data2$OCC_CODE)

data2$OCC_CODE = sub("51-902.",replacement = "51-9020",x =data2$OCC_CODE)

data2$OCC_CODE = sub("51-903.",replacement = "51-9030",x =data2$OCC_CODE)

data2$OCC_CODE = sub("51-908.",replacement = "51-9080",x =data2$OCC_CODE)

data2$OCC_CODE = sub("51-912[13]",replacement = "51-9120",x =data2$OCC_CODE)

data2$OCC_CODE = sub("51-919[23]",replacement = "51-919X",x =data2$OCC_CODE)

data2$OCC_CODE = sub("51-9141",replacement = "51-91XX",x =data2$OCC_CODE)

data2$OCC_CODE = sub("51-9199",replacement = "51-91XX",x =data2$OCC_CODE)

data2$OCC_CODE = sub("53-10..",replacement = "53-1000",x =data2$OCC_CODE)

data2$OCC_CODE = sub("53-201.",replacement = "53-2010",x =data2$OCC_CODE)

data2$OCC_CODE = sub("53-202.",replacement = "53-2020",x =data2$OCC_CODE)

data2$OCC_CODE = sub("53-3021",replacement = "53-3052",x =data2$OCC_CODE)

data2$OCC_CODE = sub("53-3022",replacement = "53-3051",x =data2$OCC_CODE)

data2$OCC_CODE = sub("53-401[13]",replacement = "53-4010",x =data2$OCC_CODE)

data2$OCC_CODE = sub("53-303.",replacement = "53-3030",x =data2$OCC_CODE)

#could not split the mapping
data2$OCC_CODE = sub("53-3041",replacement = "53-3053",x =data2$OCC_CODE)

data2$OCC_CODE = sub("53-401[13]",replacement = "53-4010",x =data2$OCC_CODE)

data2$OCC_CODE = sub("53-4012",replacement = "53-40XX",x =data2$OCC_CODE)

data2$OCC_CODE = sub("53-4021",replacement = "53-40XX",x =data2$OCC_CODE)

data2$OCC_CODE = sub("53-4041",replacement = "53-40XX",x =data2$OCC_CODE)

data2$OCC_CODE = sub("53-502.",replacement = "53-5020",x =data2$OCC_CODE)

data2$OCC_CODE = sub("53-603[12]",replacement = "53-6030",x =data2$OCC_CODE)

data2$OCC_CODE = sub("53-60[14]1",replacement = "53-60XX",x =data2$OCC_CODE)

data2$OCC_CODE = sub("53-70[134]1",replacement = "53-70XX",x =data2$OCC_CODE)

data2$OCC_CODE = sub("53-707[123]",replacement = "53-7070",x =data2$OCC_CODE)

data2$OCC_CODE = sub("53-7121",replacement = "53-71XX",x =data2$OCC_CODE)

data2$OCC_CODE = sub("53-713[23]",replacement = "53-71XX",x =data2$OCC_CODE)

data2$OCC_CODE = sub("53-7151",replacement = "53-71XX",x =data2$OCC_CODE)

data2$OCC_CODE = sub("53-716[1234]",replacement = "53-71XX",x =data2$OCC_CODE)

data2$OCC_CODE = sub("53-7181",replacement = "53-71XX",x =data2$OCC_CODE)

data2$OCC_CODE = sub("53-7033",replacement = "47-5040",x =data2$OCC_CODE)

data2$OCC_CODE = sub("53-7032",replacement = "47-5022",x =data2$OCC_CODE)

data2 = data2%>%group_by(OCC_CODE)%>%summarize(avg_onet = mean(teleworkable))

joined_data = full_join(data1,data2,by=join_by("OCC_CODE"),keep=TRUE)

help(left_join)

setwd("C:/Users/jmca9/Documents")
data <- read_csv("worksoc.csv")
data$OCC = as.character(data$OCC)

df = left_join(data, joined_data, join_by("OCC"))

help(left_join)
