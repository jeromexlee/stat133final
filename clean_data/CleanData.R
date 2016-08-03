#Importing library and data
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

######################   Load Saved Data ####################
#variables: 
#       CA_pricing: dataframe, storing the price for California by different types
#       City_pricing: dataframe, storing the price for each city of California by different tpyes
#       pop_df: dataframe, population for each county by year
#       gdp_df: dataframe, Income per person for each county by year
City_pricing = read_csv("../raw_data/pricing_by_city.csv") %>% 
  .[-c(1)]
CA_pricing = read_csv("../raw_data/pricing_by_state.csv") %>% 
  .[-c(1)]

pop_df = read_csv("../raw_data/popudata.csv") %>% 
  .[-c(1)] %>% 
  mutate(county = tolower(county))
gdp_df = read_csv("../raw_data/gdppcdata.csv") %>% 
  .[-c(1)] %>% 
  mutate(county = tolower(county))

######################   Load Cleaning Data ####################
c_types = c("A","SF","MVSF","2B","3B","4B")
City_pricing = mutate(City_pricing,year = year(Date))
City_pricing = mutate(City_pricing,County = tolower(County))
pop_df = mutate(pop_df,year = year(DATE))
gdp_df = mutate(gdp_df,year = year(DATE))
colnames(pop_df) = c("Date","Pop","County","year")
colnames(gdp_df) = c("Date","Income","County","year")

#Processing the data for bubbling
bubble_data = inner_join(zs) %>% 
  mutate(Pop = Pop*1000) %>% 
  .[-1] %>% 
  inner_join(City_pricing) %>% 
  write_csv("./cleaned_bubble_data.csv")



#Processing the map datas
map_data = City_pricing %>%
  group_by(year, County,Type) %>% 
  dplyr::summarise(Mean = mean(Value)) %>% 
  mutate(County = tolower(County))

states = map_data("state")
ca_df = filter(states, region == "california")
counties = map_data("county")
ca_county = filter(counties, region == "california")

ca_base = ggplot(ca_df, aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) + 
  geom_polygon(color = "black", fill = "gray")

new_map_data = inner_join(ca_county, map_data, by = c("subregion" = "County")) %>% 
  inner_join(pop_df,by = c("subregion" = "County","year")) %>% 
  inner_join(gdp_df,by = c("subregion" = "County","year")) %>% 
  select(-Date.x,-Date.y) %>% 
  spread(new_map_data,Type,Mean)
write_csv(map_data,"./cleaned_map_data.csv")