#Importing libarary and data
library(Quandl)
library(stringr)
library(readr)
library(stringr)
library(dplyr)
library(tidyr)
library(lubridate)
library(plyr)
library(ggplot2)
library(rvest)
library(ggmap)
library(mapdata)
library(scales)

######################  Load Raw Data ####################

#Step1:filtering the lookup codes
#Part1: lookup codes for ZILL
primary = c("Alameda","Contra Costa","San Mateo","Marin",
            "Napa","Sacramento","Santa Clara","San Francisco")
city_code = read_csv("lookup_codes/city_codes.csv") %>%
  mutate(code = str_extract(.[[4]],"[0-9].*")) %>%
  filter(State == "CA") %>%
  mutate(County = str_extract(.[[4]],"[a-zA-Z ]*")) %>%
  filter(County %in% primary) %>%
  filter(!Region %in% c("San Pablo",'North Highlands','North Fair Oaks',
                        'Discovery Bay','Rodeo','Saint Helena','Fairfax'))

state_code = read_csv("lookup_codes/state_codes.csv")
names(state_code) = c("state")
state_code = state_code %>%
  mutate(code = str_extract(state,"[0-9].*")) %>%
  mutate(state = str_extract(state,"[a-zA-Z ]*")) %>%
  mutate(abbr = c("CA","TX","NY","FL","IL","PA","OH","MI","GA","NC",
                  "NJ","VA","WA","MA","IN","AZ","TN","MO","MD","WI",
                  "MN","CO","AL","SC","LA","KY","OR","OK","CT","IA",
                  "MS","AR","UT","NV","NM","WV","NE","ID","HI","NH",
                  "RI","MT","DE","SD","AK","ND","VT","DC","KS","WY","ME"))

types = c("A","SF","MVSF","2B","3B","4B","BT","MT",'TT','RMP','RAH',
          'RZSF','PRR','MLP','MSP','MLPSF','MSPSF','LPC','MPC','SLPR',
          'SFL','SFG','IV','DV','SPY','HR','HF','FR')
names(types) = c("All Homes",'Single Family Residences','Price per Square Foot',
                 '2 Bedroom','3 Bedroom','4 Bedroom','Bottom Tier','Middle Tier',
                 'Top Tier','Median Rent, Homes Listed for Rent',
                 'Estimated Rent, All Homes in Region','Estimated Rent per Square Foot',
                 'Price-to-Rent Ratio',"Median List Price",'Median Sale Price',
                 "Median List Price per Square Foot",'Median Salse Price per Square Foot',
                 "Listings with Price Cut in Last 30 Days",'Median Price Cut',
                 "Ratio of Sale Price to List Price ",'Sold for Loss','Sold for Gain',
                 'Increasing Values',"Decreasing Values",
                 "Turnover in Housing Market, Past 1 Year", 
                 "Number of Homes for Rent","Monthly Foreclosures per 10,00 Homes",
                 "Percentage of Sales that were Foreclosures")

#Part2: lookup codes for FRED
FRED_code = read_csv("lookup_codes/FRED-datasets-codes.csv") %>%
  filter(str_match(FRED_code,"(, CA)"))
scheme = str_detect(FRED_code[[2]],"(, CA)")
FRED_code1 = data_frame(code = FRED_code[[1]][scheme],define = FRED_code[[2]][scheme])
scheme = list()
for (i in 1:length(primary)){
  p = str_c('(',primary[i],')')
  print(p)
  scheme[[i]]= str_detect(FRED_code1[[2]],p)
}
scheme = Reduce("|",scheme)
FRED_code2 = data_frame(code = FRED_code1[[1]][scheme],define = FRED_code1[[2]][scheme])