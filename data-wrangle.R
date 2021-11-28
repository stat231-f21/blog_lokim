############
# Packages #
############

library(tidyverse)
library(kableExtra)
library(robotstxt) 
library(rvest) 
library(purrr)
library(janitor)
library(tidytext)
library(readr)


############################################
# Data Import and Large Data Set wrangling #
############################################

# First checking if data is allowed to be scraped from website: http://insideairbnb.com/get-the-data.html
paths_allowed("http://insideairbnb.com/get-the-data.html")

# Read in all csv from airbnb_raw_data
austin <- read_csv("airbnb_raw_data/Austin.csv")
boston <- read_csv("airbnb_raw_data/Boston.csv")
chicago <- read_csv("airbnb_raw_data/Chicago.csv")
denver <- read_csv("airbnb_raw_data/Denver.csv")
hawaii <- read_csv("airbnb_raw_data/Hawaii.csv")
losangeles <- read_csv("airbnb_raw_data/LosAngeles.csv")
nashville <- read_csv("airbnb_raw_data/Nashville.csv")
neworleans <- read_csv("airbnb_raw_data/NewOrleans.csv")
newyork <- read_csv("airbnb_raw_data/NewYork.csv")
sandiego <- read_csv("airbnb_raw_data/SanDiego.csv")
sanfrancisco <- read_csv("airbnb_raw_data/SanFrancisco.csv")
seattle <- read_csv("airbnb_raw_data/Seattle.csv")

# Add City column to each data set so that I know which city the AirBnb property is located in.
# Remove neighborbhood variables so that the data sets can be joined. These variables have inconsistent datatypes
austin <- austin %>%
  mutate(locale = "Austin") %>%
  select(-neighbourhood_cleansed, -neighbourhood_group_cleansed, - neighbourhood, -neighborhood_overview)
boston <- boston %>%
  mutate(locale = "Boston") %>%
  select(-neighbourhood_cleansed, -neighbourhood_group_cleansed, - neighbourhood, -neighborhood_overview)
chicago <- chicago %>%
  mutate(locale = "Chicago") %>%
  select(-neighbourhood_cleansed, -neighbourhood_group_cleansed, - neighbourhood, -neighborhood_overview)
denver <- denver %>%
  mutate(locale = "Denver") %>%
  select(-neighbourhood_cleansed, -neighbourhood_group_cleansed, - neighbourhood, -neighborhood_overview)
hawaii <- hawaii %>%
  mutate(locale = "Hawaii") %>%
  select(-neighbourhood_cleansed, -neighbourhood_group_cleansed, - neighbourhood, -neighborhood_overview)
losangeles <- losangeles %>%
  mutate(locale = "Los Angeles") %>%
  select(-neighbourhood_cleansed, -neighbourhood_group_cleansed, - neighbourhood, -neighborhood_overview)
nashville <- nashville %>%
  mutate(locale = "Nashville") %>%
  select(-neighbourhood_cleansed, -neighbourhood_group_cleansed, - neighbourhood, -neighborhood_overview)
neworleans <- neworleans %>%
  mutate(locale = "New Orleans") %>%
  select(-neighbourhood_cleansed, -neighbourhood_group_cleansed, - neighbourhood, -neighborhood_overview)
newyork <- newyork %>%
  mutate(locale = "New York") %>%
  select(-neighbourhood_cleansed, -neighbourhood_group_cleansed, - neighbourhood, -neighborhood_overview)
sandiego <- sandiego %>%
  mutate(locale = "San Diego") %>%
  select(-neighbourhood_cleansed, -neighbourhood_group_cleansed, - neighbourhood, -neighborhood_overview)
sanfrancisco <- sanfrancisco %>%
  mutate(locale = "San Francisco") %>%
  select(-neighbourhood_cleansed, -neighbourhood_group_cleansed, - neighbourhood, -neighborhood_overview)
seattle <- seattle %>%
  mutate(locale = "Seattle") %>%
  select(-neighbourhood_cleansed, -neighbourhood_group_cleansed, - neighbourhood, -neighborhood_overview)

# Join all rows together from csv to create a dataset with every Airbnb Property and its details
all_properties = rbind(austin, 
                       boston,
                       chicago,
                       denver,
                       hawaii,
                       losangeles,
                       nashville,
                       neworleans,
                       newyork,
                       sandiego,
                       sanfrancisco,
                       seattle)

# Remove unwanted variables
all_properties <- all_properties %>%
  select(-c("scrape_id",
            "last_scraped",
            "host_location",
            "host_url",
            "host_name",
            "host_about",
            "host_response_time",
            "host_is_superhost",
            "host_thumbnail_url",
            "host_picture_url",
            "host_neighbourhood",
            "host_total_listings_count",
            "host_verifications",
            "host_has_profile_pic",
            "host_identity_verified",
            "property_type",
            "bathrooms",
            "bathrooms_text",
            "amenities",
            "minimum_minimum_nights",
            "maximum_minimum_nights",
            "minimum_maximum_nights",
            "maximum_maximum_nights",
            "minimum_nights_avg_ntm",
            "maximum_nights_avg_ntm",
            "calendar_updated",
            "has_availability",
            "availability_30",
            "availability_60",
            "availability_90",
            "availability_365",
            "calendar_last_scraped",
            "first_review",
            "last_review",
            "review_scores_accuracy",
            "review_scores_communication",
            "review_scores_value",
            "license",
            "instant_bookable")) 

# Rename variables for stylistic reasons
all_properties <- all_properties %>%
  rename(
    listing_id = id,
    listing_name = name,
    listing_description = description,
    listing_picture_url = picture_url,
    occupancy_size = accommodates,
    overall_rating = review_scores_rating,
    cleanliness_rating = review_scores_cleanliness,
    checkin_rating = review_scores_checkin,
    location_rating = review_scores_location,
    host_listings_count_total = calculated_host_listings_count,
    host_listings_count_entire_homes = calculated_host_listings_count_entire_homes,
    host_listings_count_private_rooms = calculated_host_listings_count_private_rooms,
    host_listings_count_shared_rooms = calculated_host_listings_count_shared_rooms)

# Drop NA's
all_properties <- all_properties %>%
  drop_na() 

# Convert Currency Price to numeric values (needed for plots later)
all_properties <- all_properties %>%
  mutate(price_per_night = as.numeric(parse_number(price)),
         month_host_joined = format(as.POSIXct(host_since),"%m"))
     
# Remove the previous price variable and convert Percentage variables to proportions
all_properties <- all_properties %>%
  select(-price) %>%
  mutate(host_response_proportion = (as.numeric(parse_number(host_response_rate)) / 100),
         host_acceptance_proportion = (as.numeric(parse_number(host_acceptance_rate)) / 100))

# Remove previous host response/acceptance rates since we have proportions
all_properties <- all_properties %>%
  drop_na() %>%
  select(-host_response_rate, -host_acceptance_rate)

# Write a csv of the all_properties dataset.
write_csv(all_properties, "airbnb_master.csv")


###################################
# Data Set Creation for Host Data #
###################################

# Select Host related variables only
host_data <- all_properties %>%
  select(host_id,
         host_since,
         host_listings_count_total,
         host_listings_count_entire_homes,
         host_listings_count_private_rooms,
         host_listings_count_shared_rooms,
         locale,
         month_host_joined,
         host_response_proportion,
         host_acceptance_proportion)

# Get rid of duplicate hosts since rows are identified by properties currently
host_data <- host_data[!duplicated(host_data$host_id), ]

# Write a csv for host_data 
write_csv(host_data, "host_master.csv")



