# Cameron Cabo
# GAFL 531: Data Science for Public Policy
# April 29, 2020
# GitHub: @cacabo
# Twitter: @cameroncabo
# Analyzing Airbnb's Impact on Rent and Housing in NYC

# ================================================================
# Dependencies
# ================================================================

library(ggplot2)
library(dplyr)
library(stringr)
library(leaflet)

# ================================================================
# Read in data
# ================================================================

# Source: http://insideairbnb.com/get-the-data.html
# jan_listings <- read.csv('./data/jan-listings.csv')
listings <- read.csv('./data/listings.csv')

# Source: https://data.cityofnewyork.us/Housing-Development/Housing-New-York-Units-by-Building/hg8x-zxpr
housing <- read.csv('./data/Housing_New_York_Units_by_Building.csv')

# nrow(jan_listings) # -> 51361
nrow(listings) # -> 50378

# ================================================================
# Constants and Themes
# ================================================================

GREEN <- "#00A699"
CORAL <- "#FF5A5F"
ORANGE <- "#FC642D"

theme <- theme_minimal(base_size = 12)
blank_theme <- theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=12, face="bold")
  )

theme_set(theme)

# ================================================================
# Feature Engineering
# ================================================================

# Convert to date
listings$first_review_date <- as.Date(listings$first_review, format="%Y-%m-%d")
listings$last_review_date  <- as.Date(listings$last_review, format="%Y-%m-%d")
listings$last_scraped_date <- as.Date(listings$last_scraped, format="%Y-%m-%d")

# Convert from text like "$5,000.00" to numbers like 5000
price_as_double <- function (data) {
  return(
    data %>%
      as.character() %>%
      { gsub(",", "", .) } %>%
      { gsub("\\$", "", .)} %>%
      str_trim() %>%
      as.numeric()
  )
}

listings$price_double         <- price_as_double(listings$price)
listings$weekly_price_double  <- price_as_double(listings$weekly_price)
listings$monthly_price_double <- price_as_double(listings$monthly_price)

listings$host_has_multi <- !is.na(listings$host_total_listings_count) &
  listings$host_total_listings_count > 1

# ================================================================
# Helper functions
# ================================================================

plot_hist <- function (data, title, xlabel, ylabel, bins=60) {
  qplot(
    data,
    geom="histogram",
    bins=bins,
    fill=I(GREEN),
  ) +
    ggtitle(title) +
    xlab(xlabel) +
    ylab(ylabel)
}

# TODO neighborhoods data and geojson

# ================================================================
# Understanding the Airbnb Data
# ================================================================

colnames(listings)

#   [1] "id"                                           "listing_url"                                 
#   [3] "scrape_id"                                    "last_scraped"                                
#   [5] "name"                                         "summary"                                     
#   [7] "space"                                        "description"                                 
#   [9] "experiences_offered"                          "neighborhood_overview"                       
#  [11] "notes"                                        "transit"                                     
#  [13] "access"                                       "interaction"                                 
#  [15] "house_rules"                                  "thumbnail_url"                               
#  [17] "medium_url"                                   "picture_url"                                 
#  [19] "xl_picture_url"                               "host_id"                                     
#  [21] "host_url"                                     "host_name"                                   
#  [23] "host_since"                                   "host_location"                               
#  [25] "host_about"                                   "host_response_time"                          
#  [27] "host_response_rate"                           "host_acceptance_rate"                        
#  [29] "host_is_superhost"                            "host_thumbnail_url"                          
#  [31] "host_picture_url"                             "host_neighbourhood"                          
#  [33] "host_listings_count"                          "host_total_listings_count"                   
#  [35] "host_verifications"                           "host_has_profile_pic"                        
#  [37] "host_identity_verified"                       "street"                                      
#  [39] "neighbourhood"                                "neighbourhood_cleansed"                      
#  [41] "neighbourhood_group_cleansed"                 "city"                                        
#  [43] "state"                                        "zipcode"                                     
#  [45] "market"                                       "smart_location"                              
#  [47] "country_code"                                 "country"                                     
#  [49] "latitude"                                     "longitude"                                   
#  [51] "is_location_exact"                            "property_type"                               
#  [53] "room_type"                                    "accommodates"                                
#  [55] "bathrooms"                                    "bedrooms"                                    
#  [57] "beds"                                         "bed_type"                                    
#  [59] "amenities"                                    "square_feet"                                 
#  [61] "price"                                        "weekly_price"                                
#  [63] "monthly_price"                                "security_deposit"                            
#  [65] "cleaning_fee"                                 "guests_included"                             
#  [67] "extra_people"                                 "minimum_nights"                              
#  [69] "maximum_nights"                               "minimum_minimum_nights"                      
#  [71] "maximum_minimum_nights"                       "minimum_maximum_nights"                      
#  [73] "maximum_maximum_nights"                       "minimum_nights_avg_ntm"                      
#  [75] "maximum_nights_avg_ntm"                       "calendar_updated"                            
#  [77] "has_availability"                             "availability_30"                             
#  [79] "availability_60"                              "availability_90"                             
#  [81] "availability_365"                             "calendar_last_scraped"                       
#  [83] "number_of_reviews"                            "number_of_reviews_ltm"                       
#  [85] "first_review"                                 "last_review"                                 
#  [87] "review_scores_rating"                         "review_scores_accuracy"                      
#  [89] "review_scores_cleanliness"                    "review_scores_checkin"                       
#  [91] "review_scores_communication"                  "review_scores_location"                      
#  [93] "review_scores_value"                          "requires_license"                            
#  [95] "license"                                      "jurisdiction_names"                          
#  [97] "instant_bookable"                             "is_business_travel_ready"                    
#  [99] "cancellation_policy"                          "require_guest_profile_picture"               
# [101] "require_guest_phone_verification"             "calculated_host_listings_count"              
# [103] "calculated_host_listings_count_entire_homes"  "calculated_host_listings_count_private_rooms"
# [105] "calculated_host_listings_count_shared_rooms"  "reviews_per_month"

# ================================================================
# Filtering the data to only relevant listings
# ================================================================

length(listings$id) # -> 50378

levels(listings$room_type)
# -> "Entire home/apt" "Hotel room"      "Private room"    "Shared room"

listings %>% count(room_type)
#   room_type           n
#   <fct>           <int>
# 1 Entire home/apt 26274
# 2 Hotel room        402
# 3 Private room    22895
# 4 Shared room      1225

ggplot(listings, aes(x="", fill=room_type)) +
  geom_bar(stat="count", width=1, color="white") +
  coord_polar("y", start=0) +
  blank_theme +
  theme(axis.text.x=element_blank())

unfiltered_listings <- listings
listings <- unfiltered_listings %>% filter(
  room_type %in% c("Entire home/apt", "Private room"))

# Also focus on specific property types
levels(listings$property_type)

#  [1] "Aparthotel"             "Apartment"              "Barn"                  
#  [4] "Bed and breakfast"      "Boat"                   "Boutique hotel"        
#  [7] "Bungalow"               "Bus"                    "Cabin"                 
# [10] "Camper/RV"              "Casa particular (Cuba)" "Castle"                
# [13] "Cave"                   "Condominium"            "Cottage"               
# [16] "Dome house"             "Dorm"                   "Earth house"           
# [19] "Farm stay"              "Guest suite"            "Guesthouse"            
# [22] "Hostel"                 "Hotel"                  "House"                 
# [25] "Houseboat"              "In-law"                 "Island"                
# [28] "Lighthouse"             "Loft"                   "Other"                 
# [31] "Resort"                 "Serviced apartment"     "Tent"                  
# [34] "Timeshare"              "Tiny house"             "Townhouse"             
# [37] "Train"                  "Treehouse"              "Villa"                 
# [40] "Yurt"

listings %>% count(property_type) %>% arrange(desc(n))
#    property_type          n
#    <fct>              <int>
#  1 Apartment          38751
#  2 House               3969
#  3 Townhouse           1744
#  4 Condominium         1680
#  5 Loft                1334
#  6 Guest suite          420
#  7 Serviced apartment   406
#  8 Boutique hotel       270
#  9 Hotel                113
# 10 Other                 95

listings <- listings %>% filter(property_type %in% c(
  "Apartment",
  "House",
  "Townhouse",
  "Condominium",
  "Loft",
  "Guest suite",
  "Serviced apartment"
))

# Filter to only include properties which are not uniquely used
# for travel but are often rented / used for long term housing

# Plot monthly price per listing
plot_hist(
  listings$price_double,
  'Listing Prices',
  'Price ($)',
  'Frequency',
  60)
plot_hist(
  listings$monthly_price_double,
  'Monthly Listing Prices',
  'Monthly Price ($)',
  'Frequency',
  60)
MAX_PRICE <- 1000
MAX_MONTHLY_PRICE <- 12000
sum(is.na(listings$monthly_price_double)) # -> 43713 (vast majority)
sum(is.na(listings$weekly_price_double))  # -> 42658 (not much better)
sum(is.na(listings$price))                # -> 0

max((listings %>% filter(!is.na(beds)))$beds) # -> 40 beds!
listings %>% count(beds)
#      beds     n
#     <int> <int>
#   1     0  1518
#   2     1 29961
#   3     2 10183
#   4     3  3618
#   5     4  1522
#   6     5   550
#   7     6   264
#   8     7   100
#   9     8    54
#  10     9    29
sum(is.na(listings$beds)) # -> 434
MAX_NUM_BEDS <- 8

ggplot(data = listings %>% filter(is.na(beds) | beds <= MAX_NUM_BEDS), aes(x = as.character(beds))) +
  geom_bar(stat="count") +
  xlab("Number of Beds") +
  ylab("Number of Listings")

ggplot(data = listings %>%
         filter(beds <= MAX_NUM_BEDS) %>%
         filter(monthly_price_double <= MAX_MONTHLY_PRICE)) +
  geom_boxplot(aes(x=as.factor(beds), y=monthly_price_double, fill=beds)) +
  coord_flip() +
  ylab('Monthly Price ($)') +
  xlab('Number of Beds') +
  ggtitle(
    'Monthly Price for Listing by Number of Beds',
    subtitle = 'Excluding num beds > 8 or price > $12,000') +
  guides(fill=FALSE)

ggplot(data = listings %>%
         filter(beds <= MAX_NUM_BEDS) %>%
         filter(price_double <= MAX_PRICE)) +
  geom_boxplot(aes(x=as.factor(beds), y=price_double, fill=beds)) +
  coord_flip() +
  ylab('Price ($)') +
  xlab('Number of Beds') +
  ggtitle(
    'Price for Listing by Number of Beds',
    subtitle = 'Excluding num beds > 8 or price > $1,000') +
  guides(fill=FALSE)

"
We see a long tail distribution with slow ramp up
Interesting that there is no bimodal distribution for categories like `normal` and `luxury`
Outliers are above but not below, this makes sense

TODO maybe look at prices per bedroom?
TODO add line for average rental price in NYC?
"

head((listings %>% filter(is.na(beds)))$description, 10)
head((listings %>% filter(beds == 0))$description, 10)
# We see that beds marked as NA or 0 tend to be normal bedroom listings
# We will not filter them out

# These plots look more reasonable -> filter on these
listings <- listings %>%
  filter(is.na(beds) | beds < MAX_NUM_BEDS) %>%
  filter(is.na(monthly_price_double) | monthly_price_double <= MAX_MONTHLY_PRICE) %>%
  filter(price_double <= MAX_PRICE)

# Makes sense to focus on reviewed listings
sum(!is.na(listings$first_review_date)) # -> 38627
sum(is.na(listings$first_review_date))  # -> 9677
sum(is.na(listings$last_review_date))   # -> 9677 (sanity check)

sum(is.na(listings$number_of_reviews)) # -> 0 (all filled in, nice)
sum(listings$number_of_reviews == 0) # -> 9537
listings %>%
  filter(number_of_reviews == 0) %>%
  head(10) %>%
  select(description)
# These seem to be real-ish though might be duplicates

plot_hist(listings$number_of_reviews, 'Number of Reviews', 'Count', 'Frequency')

# NOTE "ltm" means "last twelve months", "ntm" means "next twelve months"
plot_hist(listings$number_of_reviews_ltm, 'Number of Reviews', 'Count', 'Frequency')

# Proxy for when these listings were created
plot_hist(listings$first_review_date, 'First Review Dates', 'Date', 'Frequency', 60)


plot_review_dates <- function(data) {
  ggplot(data) +
    geom_density(show.legend = FALSE, aes(x=first_review_date), colour=GREEN, fill=GREEN, alpha=0.2) +
    geom_density(show.legend = FALSE, aes(x=last_review_date), colour=CORAL, fill=CORAL, alpha=0.2) +
    xlab('Review Date') +
    ylab('Density') +
    ggtitle('First and Last Review Dates on Listings over Time')
}

plot_review_dates(unfiltered_listings)

# Filter out listings last reviewed more than 1 year ago
get_date_1_yr_ago <- function() {
  d <- as.POSIXlt(Sys.Date())
  d$year <- d$year - 1
  return(as.Date(d))
}

listings <- listings %>%
  filter(is.na(last_review_date) | last_review_date > get_date_1_yr_ago())

plot_review_dates(listings)

# ================================================================
# Understanding variables in the data
# ================================================================

length(listings$id) # -> 36394

# Everything was scraped in April (all current listings)
plot_hist(listings$last_scraped_date, title="Last Scraped", xlabel="Date", ylabel="Frequency")

listings %>% count(host_identity_verified)
#    host_identity_verified     n
#    <fct>                  <int>
#  1 ""                         4
#  2 "f"                    21818
#  3 "t"                    14572

unique(listings$license)
sum(listings$license == "") # -> 36392 (vast majority don't have a license)
summary(listings$license)

"
Note we are using first review date as a proxy for when the listing was created

The vast majority are still quite active into the start of 2020

We also see that the last reviews curve is far more volatile than the first reviews curve
indicating some notion of seasonality
"
ggplot(listings[!is.na(listings$host_has_multi), ]) +
  geom_density(
    show.legend = TRUE,
    aes(x=first_review_date, colour=host_has_multi, fill=host_has_multi),
    position="stack",
    alpha=0.5) +
  guides(
    fill=guide_legend(title="Multi-listing host?"),
    colour=guide_legend(title="Multi-listing host?")) +
  xlab('First Review Date') +
  ylab('Density') +
  ggtitle('First Review Date of Listings by Host Type') +
  scale_colour_manual(values = c("TRUE"= CORAL, "FALSE" = GREEN)) +
  scale_fill_manual(values = c("TRUE"= CORAL, "FALSE" = GREEN))

# ================================================================
# Looking at host data, specifically
# ================================================================

hosts <- listings[, c(
  "host_id",
  "host_url",
  "host_name",
  "host_since",
  "host_location",
  "host_about",
  "host_response_time",
  "host_response_rate",
  "host_acceptance_rate",
  "host_is_superhost",
  "host_thumbnail_url",
  "host_picture_url",
  "host_neighbourhood",
  "host_listings_count",
  "host_total_listings_count",
  "host_verifications",
  "host_has_profile_pic"
)] %>%
  filter(!duplicated(host_id))

# Print out hosts with the most listings
hosts %>%
  arrange(desc(host_listings_count)) %>%
  select(c('host_id', 'host_name', 'host_listings_count')) %>%
  head(20)
#       host_id host_name    host_listings_count
#   1  48005494 Zeus                        2345
#   2  10981379 Jan                         1717
#   3 107434423 Blueground                  1437
#   4 194953121 Christian                   1364
#   5  12243051 Sonder                       883
#   6  24831061 Hosteeva                     691
#   7  30283594 Kara                         684
#   8  30787515 Brooke                       653
#   9   9419684 Mike                         440
#  10  76104209 Rated                        435
# ...
# We see a mix of agencies and people with oddly simple names...aka agencies under cover

sum(is.na(hosts$host_listings_count)) # -> 5
sum(hosts$host_listings_count == 0, na.rm = TRUE)
# 3974 hosts have no listings...maybe there is some sort of federated system

hosts_with_listings <- hosts[hosts$host_listings_count > 0, ]

qplot(
  hosts_with_listings$host_listings_count,
  geom="histogram",
  bins=60,
  fill=I(GREEN)) +
  scale_y_continuous(trans='log10') +
  ggtitle('Listings per Host on Log Scale') +
  xlab('Number of Listings') +
  ylab('Frequency')
"
The vast majority of hosts own only a handlful of rentals
Those who own on the order of hundreds are largely businesses, sometimes with actual business
names and sometimes with host names which under the hood are enterprises.
"

# Consider pricing differences between those with multiple listings and not
ggplot(data = listings) +
  geom_boxplot(aes(x=as.factor(host_has_multi), y=monthly_price_double, fill=host_has_multi)) +
  coord_flip() +
  ylab('Monthly Price ($)') +
  xlab('Host has Multiple Listings') +
  ggtitle('Monthly Price split on if Host has Multiple Listings') +
  guides(fill=FALSE)
# There is no clear statistical trend

# ================================================================
# Spatial analysis
# ================================================================

# Plotting all Airbnb locations
# NOTE this is quite laggy since we are plotting so many points
plot_listings <- function (data) {
  count <- nrow(data)
  
  leaflet(data) %>%
    setView(lng = -73.9589107, lat = 40.7492607, zoom = 11) %>%
    addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
    addCircleMarkers(
      lng= ~ longitude,
      lat= ~ latitude,
      radius = if (count > 10000) 1.25 else (if (count > 1000) 1.5 else 2),
      color = CORAL,
      stroke = FALSE,
      fillOpacity = if (count > 1000) 0.05 else 0.5,
    )
}

plot_listings(listings)
"
Highly concentrated in Manhattan and Brooklyn which is disproportionate to actual housing
distribution across boroughs
"

listings_by_multihosts <- listings[listings$host_total_listings_count > 1, ]
nrow(listings_by_multihosts)
# -> 18985
plot_listings(listings_by_multihosts)
# Follows a similar distribution

# TODO maybe just plot these in different colors

plot_listings(listings[listings$license != "", ])
# Very few properties have a "license"

zeus_host_id <- 48005494
plot_listings(listings[listings$host_id == zeus_host_id, ])
# Zues is more strongly clustered in and around Manhattan
# Unique bucket in the financial district
# Locations are clustered together, perhaps making management and upkeep easier

# ================================================================
# NYC housing data
# ================================================================

# Rename strangely labelled columns
housing$`Latitude.Internal` <- housing$`Latitude..Internal.`
housing$`Longitude.Internal` <- housing$`Longitude..Internal.`
housing$`NTA.Neighborhood.Tabulation.Area` <- housing$`NTA...Neighborhood.Tabulation.Area`
drops <- c(
  "Latitude..Internal.",
  "Longitude..Internal.",
  "NTA...Neighborhood.Tabulation.Area",
)
housing <- housing[ , !(names(housing) %in% drops)]

colnames(housing)

"
Plot these new project buildings in New York
Radius proportional to number of units in the building
"
leaflet(housing) %>%
  setView(lng = -73.9589107, lat = 40.7492607, zoom = 11) %>%
  addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
  addCircles(
    lng= ~ Longitude,
    lat= ~ Latitude,
    radius = ~ sqrt(All.Counted.Units) * 10,
    color = GREEN,
    stroke = FALSE,
    fillOpacity = 0.5,
  )

# Airbnb data:

# TODO look at average number of nights occupancy histogram
# TODO properties by new hosts vs properties by "commercial" hosts
# TODO revenue of commercial hosts vs. non-commercial hosts
# TODO some Airbnbs effectively function as rentals, make sure we are not penalizing those
# TODO feature engineering: how much does this hurt the local economy?
# TODO break things up by the neighborhood or census tract level

# Requires more data:

# TODO overlay price controlled housing and Airbnb locations?
# TODO potentially look at homelessness?

# TODO look at neighborhoods
# TODO look at availability
