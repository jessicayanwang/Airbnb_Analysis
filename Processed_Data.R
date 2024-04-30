library(data.table)
library(tidyverse)
library(dplyr)
library(knitr)
library(ggplot2)
library(leaflet)
library(kableExtra)
library(webshot)
library(htmlwidgets)
library(gtsummary)
library(gt)
library(gridExtra)
library(tidytext)
library(stopwords)
library(plotly)
library(widgetframe)

# Import the data
df_raw <- data.table::fread("Data/Airbnb_Open_Data.csv")
census_raw <- data.table::fread("Data/nyc_census_2020.csv")

# Only keep the interested census variables
census_raw <- subset(census_raw, select = c(Name, Pop1, HUnits, OcHU_1, VacHUs))
# Rename the variables for clarity
census <- census_raw %>% 
  rename(
    neighbourhood = Name,
    total_population = Pop1,
    occupied_HUnits = OcHU_1,
    vacant_HUnits = VacHUs
  )
# Convert the variables to numeric variables
census$total_population <- as.numeric(gsub(",", "", census$total_population))
census$HUnits <- as.numeric(gsub(",", "", census$HUnits))
census$occupied_HUnits <- as.numeric(gsub(",", "", census$occupied_HUnits))
census$vacant_HUnits <- as.numeric(gsub(",", "", census$vacant_HUnits))

# Sum up statistics if one neighbourhood is divided into multiple subparts. 
# E.g.,  Bushwick (East) and Bushwick (West) would be summed up together as Bushwick.
census <- census %>%
  mutate(
    # Handle the special case for Bedford-Stuyvesant
    neighbourhood = if_else(
      str_detect(neighbourhood, "Bedford-Stuyvesant"),
      "Bedford-Stuyvesant",
      if_else(
        # Condition: If no "-" is detected AND brackets are detected
        !str_detect(neighbourhood, "-") & str_detect(neighbourhood, "\\("),
        # Then: Extract the name before the brackets and trim whitespace
        str_trim(str_extract(neighbourhood, "^[^(]+")),
        # Else: Keep the original neighbourhood name
        neighbourhood
      )
    )
  ) %>%
  group_by(neighbourhood) %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE))  # Summing up numeric statistics columns

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
df <- subset(df, select = -c(`host name`, `last review`, house_rules, license, `calculated host listings count`))

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

# Convert the join columns to lower case for both data frames
census <- census %>% 
  mutate(neighbourhood = tolower(neighbourhood))
df <- df %>% 
  mutate(neighbourhood = tolower(neighbourhood))

# Merge datasets
merged_data <- inner_join(census, df, by = c("neighbourhood"))