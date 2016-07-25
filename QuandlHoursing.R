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

city_code = read_csv("lookup_codes/city_codes.csv")
city_code = mutate(city_code,code = str_extract(city_code[[4]],"[0-9].*"))
city_code = city_code %>% 
  filter(State == "CA")
city_code = mutate(city_code,County = str_extract(city_code[[4]],"[a-zA-Z ]*"))
primary = c("Alameda","Contra Costa","San Mateo","Marin","Napa","Sacramento","Santa_Clara","San Francisco")
city_code = filter(city_code,County %in% primary)
city_code = filter(city_code,!Region %in% c("San Pablo",'North Highlands','North Fair Oaks','Discovery Bay','Rodeo','Saint Helena','Fairfax'))
# county_code = read_csv("datas/county_codes.csv",col_names=F)
# county_code = mutate(county_code,code = str_extract(county_code[[3]],"[0-9].*"))

# nei_code = read_csv("datas/hood_codes.csv")
# nei_code = mutate(nei_code,code = str_extract(nei_code[[5]],"[0-9].*"))

state_code = read_csv("lookup_codes/state_codes.csv")
names(state_code) = c("state")
state_code = mutate(state_code,code = str_extract(state,"[0-9].*"))
state_code = mutate(state_code,state = str_extract(state,"[a-zA-Z ]*"))
state_code = mutate(state_code,abbr = c("CA","TX","NY","FL","IL","PA","OH","MI","GA","NC","NJ","VA","WA","MA","IN","AZ","TN","MO","MD","WI","MN","CO","AL","SC","LA","KY","OR","OK","CT","IA","MS","AR","UT","NV","NM","WV","NE","ID","HI","NH","RI","MT","DE","SD","AK","ND","VT","DC","KS","WY","ME"))

types = c("A","SF","MVSF","2B","3B","4B","BT","MT",'TT','RMP','RAH','RZSF','PRR','MLP','MSP','MLPSF','MSPSF','LPC','MPC','SLPR','SFL','SFG','IV','DV','SPY','HR','HF','FR')
names(types) = c("All Homes",'Single Family Residences','Price per Square Foot','2 Bedroom','3 Bedroom','4 Bedroom','Bottom Tier','Middle Tier','Top Tier','Median Rent, Homes Listed for Rent','Estimated Rent, All Homes in Region','Estimated Rent per Square Foot','Price-to-Rent Ratio',"Median List Price",'Median Sale Price',"Median List Price per Square Foot",'Median Salse Price per Square Foot',"Listings with Price Cut in Last 30 Days",'Median Price Cut',"Ratio of Sale Price to List Price ",'Sold for Loss','Sold for Gain','Increasing Values',"Decreasing Values","Turnover in Housing Market, Past 1 Year", "Number of Homes for Rent","Monthly Foreclosures per 10,00 Homes","Percentage of Sales that were Foreclosures")
url = "https://www.quandl.com/blog/api-for-housing-data"

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
  for(j in types){
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
  n = i/length(city_code$code)
  print(n)
}


for(j in types){
  scheme = str_c("ZILL/C00010","_",j)
  #print(scheme)
  temp=NULL
  tryCatch({temp = Quandl(scheme)},
           finally = {
             if (!is.null(temp)){
               temp = mutate(temp,City=city_code$Region[1],County = city_code$County[1], Metro = city_code$Metro[1], Type = j)
             }
             City_pricing = rbind(City_pricing,temp)
           })
}

City_pricing = unique(City_pricing)
write.csv(City_pricing,"datas/pricing_by_city.csv")
write.csv(CA_pricing,"datas/pricing_by_state.csv")
length(unique(City_pricing$City))


City_pricing = read_csv("datas/pricing_by_city.csv") %>% 
  .[-c(1)]
CA_pricing = read_csv("datas/pricing_by_state.csv") %>% 
  .[-c(1)]
#Importing the population data
#variables:
#  
#

FRED_code = read_csv("lookup_codes/FRED-datasets-codes.csv")
FRED_code = filter(FRED_code,str_match(FRED_code,"(, CA)"))
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

#Taking out the population data, and attached the county name
FRED_codepopu <- filter(FRED_code2, grepl('Population', define)) %>%
  mutate(county = str_replace(define, ".+ in (.+) County.+", "\\1"))
popudata <- NULL
for (i in 1:7) {
  popudata1 <- Quandl(FRED_codepopu$code[i]) %>%
    mutate(county = FRED_codepopu$county[i])
  popudata <- rbind(popudata, popudata1)
}
write.csv(popudata, "popudata.csv")

#Taking out the GDP per capita data, and attached the county name
FRED_codegdppc <- filter(FRED_code2, grepl('Per Capita', define)) %>%
  mutate(county = str_replace(define, ".+ in (.+) County.+", "\\1")) %>%
  .[c(1:5, 8, 10), ]
gdppcdata <- NULL
for (i in 1:7) {
  gdppcdata1 <- Quandl(FRED_codegdppc$code[i]) %>%
    mutate(county = FRED_codegdppc$county[i])
  gdppcdata <- rbind(gdppcdata, gdppcdata1)
}
write.csv(gdppcdata, "gdppcdata.csv")

pop_df = read_csv("datas/popudata.csv") %>% 
  .[-c(1)]
gdp_df = read_csv("datas/gdppcdata.csv") %>% 
  .[-c(1)]


#Graphing the data
pop_plt = ggplot(pop_df,aes(x=DATE,y=VALUE,color=county)) +
  geom_smooth() +
  geom_line() + 
  labs(title="Population VS. Year", x = "Year(s)",y ="Population(thousand)") +
  scale_x_date(breaks = date_breaks("5 years"), date_labels = "%Y")
pop_plt


gdp_plt = ggplot(gdp_df,aes(x=DATE,y=VALUE,color=county)) +
  geom_smooth() +
  geom_line() + 
  labs(title="Income VS. Year", x = "Year(s)",y ="Income per Capita (dollar)") +
  scale_x_date(breaks = date_breaks("5 years"), date_labels = "%Y")
gdp_plt

#Cleaning Data
c_types = c("A","SF","MVSF","2B","3B","4B")
City_pricing = mutate(City_pricing,year = year(Date))
pop_df = mutate(pop_df,year = year(DATE))
gdp_df = mutate(gdp_df,year = year(DATE))
colnames(pop_df) = c("Date","Pop","County","year")
colnames(gdp_df) = c("Date","Income","County","year")
temp = inner_join(pop_df,gdp_df) %>% 
  mutate(Pop = Pop*1000) %>% 
  .[-1]
df = inner_join(temp,City_pricing)

