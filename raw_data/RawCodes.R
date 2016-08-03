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

#Importing the raw dataset from Quandl

#Importing hoursing data
#variables: 
#       CA_pricing: dataframe, storing the price for California by different types
#       City_pricing: dataframe, storing the price for each city of California by different tpyes
CA_pricing = NULL
City_pricing = NULL
for(j in types){
  scheme = str_c("ZILL/S",state_code$code[1],"_",j)
  print(scheme)
  temp = Quandl(scheme) %>% 
    mutate(State=state_code$abbr[1],Type = j)
  CA_pricing = rbind(CA_pricing,temp)
}

for(i in 1:length(city_code$code)){
  for(j in c_types){
    scheme = str_c("ZILL/C",city_code$code[i],"_",j)
    #print(scheme)
    temp=NULL
    tryCatch({temp = Quandl(scheme)},
             finally = {
               if (!is.null(temp)){
                 temp = mutate(temp,City=city_code$Region[i],County = city_code$County[i], Metro = city_code$Metro[i], Type = j)
               }
               City_pricing = rbind(City_pricing,temp)
             })
  }
}

write.csv(City_pricing,"raw_data/pricing_by_city.csv")
write.csv(CA_pricing,"raw_data/pricing_by_state.csv")

#Importing the population data
#variables:
#       popudata: dataframe, population for each county by year
#       gdppcdata: dataframe, Income per person for each county by year

#Taking out the population data, and attached the county name
FRED_codepopu <- filter(FRED_code2, grepl('Population', define)) %>%
  mutate(county = str_replace(define, ".+ in (.+) County.+", "\\1"))
pop_df <- NULL
for (i in 1:8) {
  temp <- Quandl(FRED_codepopu$code[i]) %>%
    mutate(county = FRED_codepopu$county[i])
  pop_df <- rbind(pop_df, temp)
}
write.csv(pop_df, "raw_data/popudata.csv")

#Taking out the GDP per capita data, and attached the county name
FRED_codegdppc <- filter(FRED_code2, grepl('Per Capita', define)) %>%
  mutate(county = str_replace(define, ".+ in (.+) County.+", "\\1")) %>%
  .[c(1:5, 8, 10), ]
gdp_df <- NULL
for (i in 1:7) {
  temp <- Quandl(FRED_codegdppc$code[2]) %>%
    mutate(county = FRED_codegdppc$county[2])
  gdp_df <- rbind(gdp_df, temp)
}
write.csv(gdp_df, "raw_data/gdppcdata.csv")