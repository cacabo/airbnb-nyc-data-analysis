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
library(reshape2)
library(rgdal)

# ================================================================
# Read in data
# ================================================================

# Source: http://insideairbnb.com/get-the-data.html
jan_listings <- read.csv('./data/jan-listings.csv')
listings <- read.csv('./data/listings.csv')
nyc_inside_airbnb_data <- read.csv('./data/nyc-inside-airbnb.csv')
neighborhoods <- read.csv('./data/neighbourhoods.csv')
neighborhoodsgeo <- readOGR('./data/neighbourhoods.geojson')

# Source: https://data.cityofnewyork.us/Housing-Development/Housing-New-York-Units-by-Building/hg8x-zxpr
housing <- read.csv('./data/Housing_New_York_Units_by_Building.csv')

# Listings are removed over time for a variety of reasons, though main ones:
# 1. Regulation
# 2. Spam filtering
# 3. Hosts leave Airbnb for economic reasons (perhaps Coronavirus-related)
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
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(size = 12, face = "bold")
  )

theme_set(theme)

# ================================================================
# Feature Engineering
# ================================================================

# Convert to date
listings$first_review_date <-
  as.Date(listings$first_review, format = "%Y-%m-%d")
listings$last_review_date  <-
  as.Date(listings$last_review, format = "%Y-%m-%d")
listings$last_scraped_date <-
  as.Date(listings$last_scraped, format = "%Y-%m-%d")

# Convert from text like "$5,000.00" to numbers like 5000
price_as_double <- function (data) {
  return(data %>%
           as.character() %>%
           {
             gsub(",", "", .)
           } %>%
           {
             gsub("\\$", "", .)
           } %>%
           str_trim() %>%
           as.numeric())
}

listings$price_double         <- price_as_double(listings$price)
listings$weekly_price_double  <-
  price_as_double(listings$weekly_price)
listings$monthly_price_double <-
  price_as_double(listings$monthly_price)

listings$host_has_multi <-
  !is.na(listings$host_total_listings_count) &
  listings$host_total_listings_count > 1

# ================================================================
# Helper functions
# ================================================================

plot_hist <- function (data, title, xlabel, ylabel, bins = 60) {
  qplot(data,
        geom = "histogram",
        bins = bins,
        fill = I(GREEN),
  ) +
    ggtitle(title) +
    xlab(xlabel) +
    ylab(ylabel)
}

plot_pie <- function (data, column, title) {
  ggplot(data, aes(x = "", fill = column)) +
    geom_bar(stat = "count",
             width = 1,
             color = "white") +
    coord_polar("y", start = 0) +
    blank_theme +
    theme(axis.text.x = element_blank()) +
    guides(fill = guide_legend(title = title))
}

# For use in leaflet
addMap <- function(map) {
  return(map %>% addProviderTiles(providers$Esri.WorldGrayCanvas))
}

# ================================================================
# Understanding the Airbnb Data
# ================================================================

colnames(neighborhoods)
# [1] "neighbourhood_group" "neighbourhood"
# NOTE this dataset is effectivelt just for making joins/other aggregations easier

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

plot_pie(listings, listings$room_type, "Room Type")

unfiltered_listings <- listings
listings <-
  unfiltered_listings %>% filter(room_type %in% c("Entire home/apt", "Private room"))

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

listings <- listings %>% filter(
  property_type %in% c(
    "Apartment",
    "House",
    "Townhouse",
    "Condominium",
    "Loft",
    "Guest suite",
    "Serviced apartment"
  )
)

# Filter to only include properties which are not uniquely used
# for travel but are often rented / used for long term housing

# Plot monthly price per listing
plot_hist(listings$price_double,
          'Listing Prices',
          'Price ($)',
          'Frequency',
          60)
plot_hist(
  listings$monthly_price_double,
  'Monthly Listing Prices',
  'Monthly Price ($)',
  'Frequency',
  60
)
MAX_PRICE <- 1000
MAX_MONTHLY_PRICE <- 12000
sum(is.na(listings$monthly_price_double)) # -> 43713 (vast majority)
sum(is.na(listings$weekly_price_double))  # -> 42658 (not much better)
sum(is.na(listings$price))                # -> 0
# TODO there is more work to be done here...

# What variables correlate most strongly with price?
# http://www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-heatmap-r-software-and-data-visualization
get_upper_tri <- function(cormat) {
  cormat[lower.tri(cormat)] <- NA
  return(cormat)
}

# Looking at correlations between variables in the data
cor(x = (listings %>%
           select(
             c(
               "price_double",
               "accommodates",
               "beds",
               "square_feet",
               "number_of_reviews",
               "number_of_reviews_ltm",
               "reviews_per_month",
               "review_scores_rating",
               "review_scores_cleanliness",
               "review_scores_communication",
               "review_scores_value",
               "review_scores_accuracy",
               "review_scores_checkin",
               "review_scores_location"
             )
           )),
    y = NULL,
    use = "complete.obs") %>%
  get_upper_tri() %>%
  melt() %>%
  ggplot(aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "blue",
    high = "red",
    mid = "white",
    midpoint = 0,
    limit = c(-1, 1),
    space = "Lab",
    name = "complete.obs",
    na.value = "white"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(
    angle = 90,
    vjust = 0,
    hjust = 0
  )) +
  scale_x_discrete(position = "top") +
  xlab("") +
  ylab("") +
  guides(fill = guide_legend(title = "Correlation")) +
  geom_tile()
"
Nothing correlates particularly well with price except maybe square feet, number of beds,
and the number of people the listing accommodates

That is, ratings don't correlate particularly strongly with listing price, likely because
hosts have a lot of agency over the price that they list at (though Airbnb will
recommend certain prices).
"

# Digging into availability
listings %>% count(has_availability) # All have availability...not clear what this means
plot_hist(listings$availability_365, "365 Day Availability", "Availability", "Frequency")
plot_hist(listings$availability_30, "30 Day Availability", "Availability", "Frequency", 30)
listings %>% count(availability_365) %>% arrange(availability_365) %>% head(10)
"
Many have 0 availability going forwards...are they booked or off the market for other
reasons?
"
listings %>%
  filter(availability_365 == 0) %>%
  select(availability_30, availability_60, availability_90) %>%
  sample_n(10)
# These are all 0 as well, so the columns are as we would expect
listings %>%
  filter(availability_365 == 0) %>%
  select(listing_url) %>%
  sample_n(10)

# This is not due to the virus as the trend persists in January data:
plot_hist(jan_listings$availability_365, "365 Day Availability January", "Availability", "Frequency")



listings %>% count(accommodates)
#     accommodates     n
#            <int> <int>
#   1            1  5110
#   2            2 16421
#   3            3  3943
#   4            4  5766
#   5            5  1588
#   6            6  2063
#   7            7   401
#   8            8   631
#   9            9    87
#  10           10   189
#  11           11    36
#  12           12    75
#  13           13     8
#  14           14    16
#  15           15     4
#  16           16    56
listings %>%
  filter(accommodates >= 15) %>%
  select(c(name, summary)) %>%
  sample_n(10)
# The majority of these listings are not proper homes or apartments
# and therefore would likely not be rentals

MAX_ACCOMMODATES = 14
listings <- listings %>% filter(accommodates <= MAX_ACCOMMODATES)

cor.test(listings$accommodates, listings$beds)
# data:  listings$accommodates and listings$beds
# t = 232.11, df = 35876, p-value < 2.2e-16
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#   0.7706028 0.7788757
# sample estimates:
#   cor
# 0.7747724

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

ggplot(data = listings %>% filter(is.na(beds) |
                                    beds <= MAX_NUM_BEDS),
       aes(x = as.character(beds))) +
  geom_bar(stat = "count") +
  xlab("Number of Beds") +
  ylab("Number of Listings")

ggplot(
  data = listings %>%
    filter(beds <= MAX_NUM_BEDS) %>%
    filter(monthly_price_double <= MAX_MONTHLY_PRICE)
) +
  geom_boxplot(aes(x = as.factor(beds), y = monthly_price_double, fill =
                     beds)) +
  coord_flip() +
  ylab('Monthly Price ($)') +
  xlab('Number of Beds') +
  ggtitle('Monthly Price for Listing by Number of Beds',
          subtitle = 'Excluding num beds > 8 or price > $12,000') +
  guides(fill = FALSE)

ggplot(data = listings %>%
         filter(beds <= MAX_NUM_BEDS) %>%
         filter(price_double <= MAX_PRICE)) +
  geom_boxplot(aes(x = as.factor(beds), y = price_double, fill = beds)) +
  coord_flip() +
  ylab('Price ($)') +
  xlab('Number of Beds') +
  ggtitle('Price for Listing by Number of Beds',
          subtitle = 'Excluding num beds > 8 or price > $1,000') +
  guides(fill = FALSE)

"
We see a long tail distribution with slow ramp up
Interesting that there is no bimodal distribution for categories like `normal` and `luxury`
Outliers are above but not below, this makes sense

TODO add line for average rental price in NYC?
"

head((listings %>% filter(is.na(beds)))$description, 10)
head((listings %>% filter(beds == 0))$description, 10)
# We see that beds marked as NA or 0 tend to be normal bedroom listings
# We will not filter them out

# These plots look more reasonable -> filter on these
listings <- listings %>%
  filter(is.na(beds) | beds < MAX_NUM_BEDS) %>%
  filter(is.na(monthly_price_double) |
           monthly_price_double <= MAX_MONTHLY_PRICE) %>%
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

plot_hist(listings$number_of_reviews,
          'Number of Reviews',
          'Count',
          'Frequency')

# NOTE "ltm" means "last twelve months", "ntm" means "next twelve months"
plot_hist(listings$number_of_reviews_ltm,
          'Number of Reviews',
          'Count',
          'Frequency')

# Proxy for when these listings were created
plot_hist(listings$first_review_date,
          'First Review Dates',
          'Date',
          'Frequency',
          60)


plot_review_dates <- function(data) {
  ggplot(data) +
    geom_density(
      show.legend = FALSE,
      aes(x = first_review_date),
      colour = GREEN,
      fill = GREEN,
      alpha = 0.2
    ) +
    geom_density(
      show.legend = FALSE,
      aes(x = last_review_date),
      colour = CORAL,
      fill = CORAL,
      alpha = 0.2
    ) +
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
  filter(is.na(last_review_date) |
           last_review_date > get_date_1_yr_ago())

plot_review_dates(listings)

# ================================================================
# Understanding variables in the data
# ================================================================

length(listings$id) # -> 36394

# Everything was scraped in April (all current listings)
plot_hist(
  listings$last_scraped_date,
  title = "Last Scraped",
  xlabel = "Date",
  ylabel = "Frequency"
)

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
    aes(x = first_review_date, colour = host_has_multi, fill = host_has_multi),
    position = "stack",
    alpha = 0.5
  ) +
  guides(
    fill = guide_legend(title = "Multi-listing host?"),
    colour = guide_legend(title = "Multi-listing host?")
  ) +
  xlab('First Review Date') +
  ylab('Density') +
  ggtitle('First Review Date of Listings by Host Type') +
  scale_colour_manual(values = c("TRUE" = CORAL, "FALSE" = GREEN)) +
  scale_fill_manual(values = c("TRUE" = CORAL, "FALSE" = GREEN))

# ================================================================
# Looking at host data, specifically
# ================================================================

hosts <- listings %>%
  select(
    c(
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
    )
  ) %>%
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
  geom = "histogram",
  bins = 60,
  fill = I(GREEN)
) +
  scale_y_continuous(trans = 'log10') +
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
  geom_boxplot(aes(
    x = as.factor(host_has_multi),
    y = monthly_price_double,
    fill = host_has_multi
  )) +
  coord_flip() +
  ylab('Monthly Price ($)') +
  xlab('Host has Multiple Listings') +
  ggtitle('Monthly Price split on if Host has Multiple Listings') +
  guides(fill = FALSE)
# There is no clear statistical trend

# ================================================================
# Spatial analysis
# ================================================================

# Plotting all Airbnb locations
# NOTE this is quite laggy since we are plotting so many points
plot_listings <- function (data) {
  count <- nrow(data)
  
  leaflet(data) %>%
    setView(lng = -73.9589107,
            lat = 40.7492607,
            zoom = 11) %>%
    addMap() %>%
    addCircleMarkers(
      lng = ~ longitude,
      lat = ~ latitude,
      radius = if (count > 10000)
        1.25
      else
        (if (count > 1000)
          1.5
         else
           2)
      ,
      color = CORAL,
      stroke = FALSE,
      fillOpacity = if (count > 1000)
        0.05
      else
        0.5,
    )
}

plot_listings(listings)
"
Highly concentrated in Manhattan and Brooklyn which is disproportionate to actual housing
distribution across boroughs
"

listings_by_multihosts <-
  listings[listings$host_total_listings_count > 1, ]
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
# Neighborhood analysis
# ================================================================

levels(listings$neighbourhood)
# [1] ""                              "Allerton"
# [3] "Alphabet City"                 "Annadale"
# [5] "Astoria"                       "Bath Beach"
# [7] "Battery Park City"             "Bay Ridge"
# ...

levels(listings$neighbourhood_cleansed)
# [1] "Allerton"                   "Arden Heights"              "Arrochar"
# [4] "Arverne"                    "Astoria"                    "Bath Beach"
# [7] "Battery Park City"          "Bay Ridge"                  "Bay Terrace"
# ...

length(unique(listings$neighbourhood))          # -> 193
length(unique(listings$neighbourhood_cleansed)) # -> 221
sum(listings$neighbourhood == "")               # -> 10
sum(listings$neighbourhood_cleansed == "")      # -> 0; this is a better feature to use
length(unique(listings$neighbourhood_group_cleansed)) # -> 5

listings %>% count(neighbourhood_group_cleansed)
#     neighbourhood_group_cleansed     n
#     <fct>                        <int>
#   1 Bronx                         1008
#   2 Brooklyn                     14610
#   3 Manhattan                    15611
#   4 Queens                        4842
#   5 Staten Island                  323

plot_pie(listings,
         listings$neighbourhood_group_cleansed,
         "Neighborhood Group")

listings %>% count(neighbourhood_cleansed) %>% arrange(desc(n)) %>% head(10)
#     neighbourhood_cleansed     n
#     <fct>                  <int>
#   1 Bedford-Stuyvesant      2908
#   2 Williamsburg            2668
#   3 Harlem                  2013
#   4 Bushwick                1780
#   5 Hell's Kitchen          1590
#   6 Upper West Side         1294
#   7 Upper East Side         1272
#   8 East Village            1255
#   9 Crown Heights           1123
#  10 Midtown                 1042

# ================================================================
# NYC housing data
# ================================================================

# Rename strangely labelled columns
housing$`Latitude.Internal` <- housing$`Latitude..Internal.`
housing$`Longitude.Internal` <- housing$`Longitude..Internal.`
housing$`NTA.Neighborhood.Tabulation.Area` <-
  housing$`NTA...Neighborhood.Tabulation.Area`
housing$`X6.BR.Units` <- housing$`X6.BR..Units`
drops <- c(
  "Latitude..Internal.",
  "Longitude..Internal.",
  "NTA...Neighborhood.Tabulation.Area",
  "X6.BR..Units"
)
housing <- housing[, !(names(housing) %in% drops)]

colnames(housing)

"
Plot these new project buildings in New York
Radius proportional to number of units in the building
"
leaflet(housing) %>%
  setView(lng = -73.9589107,
          lat = 40.7492607,
          zoom = 11) %>%
  addMap() %>%
  addCircles(
    lng = ~ Longitude,
    lat = ~ Latitude,
    radius = ~ sqrt(All.Counted.Units) * 10,
    color = GREEN,
    stroke = FALSE,
    fillOpacity = 0.5,
  )

# TODO continue with this

# Airbnb data:

# TODO properties by new hosts vs properties by "commercial" hosts
# TODO revenue of commercial hosts vs. non-commercial hosts
# TODO feature engineering: how much does this hurt the local economy?

# Requires more data:

# TODO overlay price controlled housing and Airbnb locations?

# TODO look at availability
# TODO...what is monthly price? This is not what I thought it was...
# maybe just rip this for estimating income: http://insideairbnb.com/new-york-city/#activity
# TODO reviews_per_month as proxy for frequency of use?


# Plotting neighborhoods
pal <-
  colorFactor("viridis", domain = neighborhoodsgeo$neighbourhood_group)
leaflet(neighborhoodsgeo) %>%
  addMap() %>%
  addPolygons(
    stroke = FALSE,
    smoothFactor = 0.3,
    fillOpacity = 1,
    fillColor =  ~ pal(neighbourhood_group),
    label =  ~ neighbourhood_group
  ) %>%
  addLegend(
    pal = pal,
    values =  ~ neighbourhood_group,
    opacity = 1,
    title = "Neighborhood Group"
  )

pal <-
  colorFactor("viridis", domain = neighborhoodsgeo$neighbourhood)
leaflet(neighborhoodsgeo) %>%
  addMap() %>%
  addPolygons(
    stroke = FALSE,
    smoothFactor = 0.3,
    fillOpacity = 1,
    fillColor =  ~ pal(neighbourhood),
    label =  ~ neighbourhood
  )

# TODO rental data on a per neighborhood basis?

plot_listings_in_shapes <- function(listings, shapes) {
  num_shapes <- nrow(shapes)
  
  leaflet() %>%
    addMap() %>%
    addPolygons(
      data = shapes,
      stroke = TRUE,
      color = GREEN,
      fillOpacity = 0.02,
      weight = if (num_shapes == 1)
        3
      else
        1.5,
      label = if (num_shapes == 1)
        NULL
      else
        ~ neighbourhood
    ) %>%
    addCircleMarkers(
      data = listings,
      lng =  ~ longitude,
      lat =  ~ latitude,
      radius = if (num_shapes == 1)
        3
      else
        1,
      color = CORAL,
      stroke = FALSE,
      fillOpacity = if (num_shapes == 1)
        0.5
      else
        0.25
    )
}

plot_listings_in_neighborhood <- function(n) {
  neighborhood_listings <-
    listings %>% filter(neighbourhood_cleansed == n)
  neighborhood_geo <-
    neighborhoodsgeo[neighborhoodsgeo$neighbourhood == n, ]
  if (nrow(neighborhood_listings) == 0 ||
      nrow(neighborhood_geo) == 0) {
    warning("Unknown neighborhood parameter")
    return()
  }
  
  plot_listings_in_shapes(neighborhood_listings, neighborhood_geo)
}

plot_listings_in_neighborhood_group <- function(g) {
  neighborhood_listings <-
    listings %>% filter(neighbourhood_group_cleansed == g)
  geo <-
    neighborhoodsgeo[neighborhoodsgeo$neighbourhood_group == g,]
  if (nrow(neighborhood_listings) == 0 ||
      nrow(geo) == 0) {
    warning("Unknown neighborhood group parameter")
    return()
  }
  
  plot_listings_in_shapes(neighborhood_listings, geo)
}

plot_listings_in_neighborhood("Flatiron District")
plot_listings_in_neighborhood("Williamsburg")
plot_listings_in_neighborhood("East Harlem")

plot_listings_in_neighborhood_group("Brooklyn")
plot_listings_in_neighborhood_group("Manhattan")



# TODO

colnames(nyc_inside_airbnb_data)
nyc_inside_airbnb_data %>% select(value) %>% head(10)
