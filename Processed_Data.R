library(data.table)
library(tidyverse)
library(dplyr)
library(knitr)
library(ggplot2)
library(leaflet)
library(kableExtra)
library(plotly)

# Import the data
df_raw <- data.table::fread("Data/Airbnb_Open_Data.csv")

# Convert missing values to NAs
df_raw$country[df_raw$country == ""] <- NA
df_raw$`country code`[df_raw$`country code` == ""] <- NA
df_raw$host_identity_verified[df_raw$host_identity_verified == ""] <- NA
df_raw$`neighbourhood group`[df_raw$`neighbourhood group` == ""] <- NA
df_raw$neighbourhood[df_raw$neighbourhood == ""] <- NA
df_raw$cancellation_policy[df_raw$cancellation_policy == ""] <- NA
df_raw$price[df_raw$price == ""] <- NA
df_raw$`service fee`[df_raw$`service fee` == ""] <- NA

# Since the missing rates are low, remove the observations associated with the missing values
df <- df_raw %>%
  filter(complete.cases(country, `country code`, host_identity_verified, `neighbourhood group`,
                        neighbourhood, lat, long, instant_bookable, `Construction year`, 
                        price, `service fee`, `minimum nights`, `number of reviews`, 
                        `reviews per month`, `review rate number`, 
                        `calculated host listings count`, `availability 365`))

# By checking the values of neighborhood group, we see that "Brooklyn" and "brookln" belong to the same neighborhood group
df<- df %>%
  mutate(`neighbourhood group` = ifelse(`neighbourhood group` == "Brooklyn" | 
                                          tolower(`neighbourhood group`) == "brookln", 
                                        "Brooklyn", `neighbourhood group`))

# After checking the values for country and country code, we see that the only country is USA so we remove these columns as they are not informative
df <- subset(df, select = -c(country, `country code`))

# Convert Price and Service Fee to numerical values + remove "$" and ","
df$price <- as.numeric(gsub("\\$|,", "", df$price))
df$`service fee` <- as.numeric(gsub("\\$|,", "", df$`service fee`))

# We remove the non-informative/non-key variables
df <- subset(df, select = -c(NAME, `host name`, `last review`, house_rules, license, `calculated host listings count`))

# Convert variable to factors as appropriate
df$host_identity_verified <- as.factor(df$host_identity_verified)
df$`neighbourhood group` <- as.factor(df$`neighbourhood group`)
df$neighbourhood <- as.factor(df$neighbourhood)
df$instant_bookable <- as.factor(df$instant_bookable)
df$cancellation_policy <- as.factor(df$cancellation_policy)
df$`room type` <- as.factor(df$`room type`)

# Identify outliers for minimum nights
# Remove observations outside of the IQR
Q1 <- quantile(df$`minimum nights`, 0.25)
Q3 <- quantile(df$`minimum nights`, 0.75)

# Calculate the Interquartile Range (IQR)
IQR <- Q3 - Q1

# Define lower and upper bounds for 'minimum nights'
lower_bound <- 0
upper_bound <- Q3 + 1.5 * IQR

# Filter out data outside the lower and upper bounds
df <- df %>%
  filter(`minimum nights` >= lower_bound & `minimum nights` <= upper_bound)

# Filter out outliers for availability 365
df <- df %>%
  filter(`availability 365` >= 0 & `availability 365` <= 365)