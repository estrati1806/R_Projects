# Choropleth Map in R
# Visualizing average House Price Data for Different ZIP Codes in R

# Choropleth maps are maps that use differences in shading, coloring, 
# or the placing of symbols within predefined areas to indicate the 
# average values of a property or quantity in those areas.

# While working with a data of house prices from Kaggle (https://www.kaggle.com/datasets/shree1992/housedata?select=data.csv),
# I decided to visualize the average house prices for the different ZIP codes in the dataset.

library(readr)
hp  <- read_csv("data.csv")
View(hp)
head(hp)

# Preliminary
library(tidyverse)
glimpse(hp)

# "statezip" includes both state code and zip in 1 cell
# needs mutation
hp <- separate(hp, statezip, into = c("state", "zip"), sep = " ")

# Group zip codes by avg house price
zip_prices <- hp %>%
  group_by(zip) %>%
  summarise(avg_price = mean(price))
zip_prices

# Choropleth of Avg House Price per ZIP code
# For this graph we need to use the "choroplethrZip" package from GitHub
library(devtools)
options(timeout=9999999)
install_github('arilamstein/choroplethrZip@v1.5.0')
library(choroplethr)
library(choroplethrZip)

# Choropleth needs a data.frame with a column named "region" for ZIP codes and 
# a column named "value" for our desired values
# renaming columns
zip_prices <- zip_prices %>%
  rename(region = zip, value = avg_price)
zip_prices

zip_vector <- zip_prices$region
zip_vector

zip_map <- zip_choropleth(zip_prices, 
                          title = "Average House Price per Zip Code",
                          legend = "Average House Price",
                          state_zoom = "washington",
                          zip_zoom = zip_vector) + coord_map()
zip_map
# map uploaded in folder

