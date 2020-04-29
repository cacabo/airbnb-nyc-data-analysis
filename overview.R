# Cameron Cabo
# GAFL 531: Data Science for Public Policy
# April 29, 2020
# GitHub: @cacabo
# Twitter: @cameroncabo
# Analyzing Airbnb's Impact on Rent and Housing in NYC

# ================================================================================
# Dependencies
# ================================================================================

library(ggplot2)
library(dplyr)
library(stringr)
library(leaflet)
library(reshape2)
library(rgdal)
library(htmltools)
library(sp)

# ================================================================================
# Read in data
# ================================================================================

# Source: http://insideairbnb.com/get-the-data.html
# jan_listings <- read.csv('./data/jan-listings.csv')
listings <- read.csv('./data/listings.csv')
neighborhoods <- read.csv('./data/neighbourhoods.csv')
neighborhoodsgeo <- readOGR('./data/neighbourhoods.geojson')

# Source: https://insideairbnb.com/new-york-city/
nyc_inside_airbnb_data <- read.csv('./data/nyc-inside-airbnb.csv')

# Source: https://data.cityofnewyork.us/Housing-Development/Housing-New-York-Units-by-Building/hg8x-zxpr
# Read more: https://www1.nyc.gov/site/housing/index.page
housing <- read.csv('./data/Housing_New_York_Units_by_Building.csv')

# Listings are removed over time for a variety of reasons, though main ones:
# 1. Regulation
# 2. Spam filtering
# 3. Hosts leave Airbnb for economic reasons (perhaps Coronavirus-related)
# nrow(jan_listings) # -> 51361
nrow(listings) # -> 50378

# Source: https://streeteasy.com/blog/data-dashboard
rent_inventory <- read.csv('./data/rent-inventory.csv')
rent_asking <- read.csv('./data/rent-median-asking-price.csv')

# ================================================================================
# Constants and Themes
# ================================================================================

# Colors
GREEN       <- "#00A699"
LIGHT_CORAL <- "#F495BB"
CORAL       <- "#FF5A5F"
DEEP_CORAL  <- "#BF2256"
ORANGE      <- "#FC642D"

# Theming for ggplot
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

# ================================================================================
# Helper Functions
# ================================================================================

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

# Rename a column in the provided dataframe with name specified by old to name
# specified by new. Returns the altered dataframe and does NOT change the
# provided dataframe in place
rename_column <- function (data, old, new) {
  names(data)[names(data) == old] <- new
  return(data)
}

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
  ggplot(data, aes(x = "", fill = data[[column]])) +
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

# Plot listings and housing projects on a map along with bounding shapes
plot_data <-
  function(listings = NULL,
           housing = NULL,
           shapes = NULL) {
    l <- leaflet() %>%
      addMap()
    
    if (!is.null(shapes)) {
      num_shapes <- nrow(shapes)
      
      l <- l %>% addPolygons(
        data = shapes,
        stroke = TRUE,
        color = ORANGE,
        fillOpacity = 0.02,
        weight = if (num_shapes == 1)
          3
        else
          1.5,
        label = if (num_shapes == 1)
          NULL
        else
          ~ neighborhood
      )
    }
    
    if (!is.null(listings)) {
      listingColor <-
        l <- l %>%
        addCircles(
          data = listings,
          lng =  ~ longitude,
          lat =  ~ latitude,
          radius = ~ (log2(accommodates) + 1) * 8,
          color = ~ (ifelse(
            availability_365 == 0,
            LIGHT_CORAL,
            ifelse(host_has_multi, DEEP_CORAL, CORAL)
          )),
          stroke = FALSE,
          fillOpacity = 0.5,
          popup = ~ paste0(
            "<b>",
            name,
            "</b>",
            "<table><tbody>",
            "<tr><td>Price</td><td>",
            price,
            "</td></tr>",
            "<tr><td>Host name</td><td>",
            host_name,
            "</td></tr>",
            "<tr><td>Host listings count</td><td>",
            host_listings_count,
            "</td></tr>",
            "<tr><td>Street</td><td>",
            street,
            "</td></tr>",
            "<tr><td>Property type</td><td>",
            property_type,
            "</td></tr>",
            "<tr><td>Room type</td><td>",
            room_type,
            "</td></tr>",
            "<tr><td>Accommodates</td><td>",
            accommodates,
            "</td></tr>",
            "<tr><td>Beds</td><td>",
            beds,
            "</td></tr>",
            "<tr><td>Avail next yr</td><td>",
            availability_365,
            "</td></tr>",
            "<tr><td>Listing ID</td><td>",
            '<a href="',
            listing_url,
            '">',
            id,
            "</a></td></tr>",
            "</tbody></table>"
          )
        )
    }
    
    if (!is.null(housing)) {
      l <- l %>%
        addCircles(
          data = housing,
          radius = ~ (log2(Total.Units) + 1) * 8,
          color = GREEN,
          stroke = FALSE,
          fillOpacity = 0.5,
          popup = ~ paste0(
            "<b>",
            Project.Name,
            "</b>",
            "<table><tbody>",
            "<tr><td>Street</td><td>",
            Street,
            "</td></tr>",
            "<tr><td>Project start date</td><td>",
            Project.Start.Date,
            "<tr><td>Project completion date</td><td>",
            Project.Completion.Date,
            "</td></tr>",
            "<tr><td>Extremely low income units</td><td>",
            Extremely.Low.Income.Units,
            "</td></tr>",
            "<tr><td>Low income units</td><td>",
            Low.Income.Units,
            "</td></tr>",
            "<tr><td>Moderate income units</td><td>",
            Moderate.Income.Units,
            "</td></tr>",
            "<tr><td>Middle income units</td><td>",
            Middle.Income.Units,
            "</td></tr>",
            "<tr><td>Total units</td><td>",
            Total.Units,
            "</td></tr>",
            "</tbody></table>"
          )
        )
    }
    
    l <- l %>% addLegend(
      colors = c(GREEN, LIGHT_CORAL, CORAL, DEEP_CORAL),
      labels = c(
        "Housing NYC Project",
        "Listing no availability",
        "Listing by 1-listing host",
        "Listing by multi-listing host"))
    
    return(l)
  }

get_neighborhood_geo <- function(n) {
  neighborhood_geo <-
    neighborhoodsgeo[neighborhoodsgeo$neighborhood == n,]
  if (nrow(neighborhood_geo) == 0) {
    warning("Unknown neighborhood parameter")
    return()
  }
  return(neighborhood_geo)
}

get_neighborhood_group_geo <- function(g) {
  geo <-
    neighborhoodsgeo[neighborhoodsgeo$neighborhood_group == g,]
  if (nrow(geo) == 0) {
    warning("Unknown neighborhood group parameter")
    return()
  }
  return(geo)
}

get_listings_in_neighborhood <- function(n) {
  return(listings %>% filter(neighborhood_cleansed == n))
}

get_listings_in_neighborhood_group <- function(g) {
  return(listings %>% filter(neighborhood_group_cleansed == g))
}

plot_listings_in_neighborhood <- function(n) {
  plot_data(listings = get_listings_in_neighborhood(n),
            shapes = get_neighborhood_geo(n))
}

plot_listings_in_neighborhood_group <- function(g) {
  plot_data(listings = get_listings_in_neighborhood_group(g),
            shapes = get_neighborhood_group_geo(g))
}

plot_data_in_neighborhood <- function(n) {
  plot_data(listings = get_listings_in_neighborhood(n),
            housing = get_housing_in_neighborhood(n),
            shapes = get_neighborhood_geo(n))
}

plot_data_in_neighborhood_group <- function(g) {
  plot_data(listings = get_listings_in_neighborhood_group(g),
            housing = get_housing_in_neighborhood_group(g),
            shapes = get_neighborhood_group_geo(g))
}

get_housing_in_neighborhood_group <- function(g) {
  neighborhood_group_housing_mask <- (
    !is.na(housing_idx_to_neighborhood$neighborhood_group) &
      housing_idx_to_neighborhood$neighborhood_group == g
  )
  neighborhood_group_housing <-
    housing_with_coords[neighborhood_group_housing_mask, ]
  return(neighborhood_group_housing)
}

get_housing_in_neighborhood <- function(n) {
  neighborhood_housing_mask <- (
    !is.na(housing_idx_to_neighborhood$neighborhood) &
      housing_idx_to_neighborhood$neighborhood == n
  )
  neighborhood_housing <-
    housing_with_coords[neighborhood_housing_mask, ]
  return(neighborhood_housing)
}

plot_housing_in_neighborhood <- function(n) {
  plot_data(housing = get_housing_in_neighborhood(n),
            shapes = get_neighborhood_geo(n))
}

plot_housing_in_neighborhood_group <- function(g) {
  plot_data(housing = get_housing_in_neighborhood_group(g),
            shapes = get_neighborhood_group_geo(g))
}

# Looking at correlations between variables in the data
plot_cor_matrix <- function (data, columns) {
  # http://www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-heatmap-r-software-and-data-visualization
  get_upper_tri <- function(cormat) {
    cormat[lower.tri(cormat)] <- NA
    return(cormat)
  }

  cor(x = (data %>% select(columns)),
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
}

# ================================================================================
# Feature Engineering
# ================================================================================

# Listings
# --------------------------------------------------------------------------------

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
# [107] "first_review_date"                            "last_review_date"                            
# [109] "last_scraped_date"                            "price_double"                                
# [111] "weekly_price_double"                          "monthly_price_double"                        
# [113] "host_has_multi"

listings <- listings %>%
  rename_column("neighbourhood", "neighborhood") %>%
  rename_column("neighbourhood_cleansed", "neighborhood_cleansed") %>%
  rename_column("neighbourhood_group_cleansed", "neighborhood_group_cleansed") %>%
  rename_column("host_neighbourhood", "host_neighborhood")

names(listings)

# Convert to date
listings$first_review_date <-
  as.Date(listings$first_review, format = "%Y-%m-%d")
listings$last_review_date  <-
  as.Date(listings$last_review, format = "%Y-%m-%d")
listings$last_scraped_date <-
  as.Date(listings$last_scraped, format = "%Y-%m-%d")

# Convert to TRUE/FALSE
listings$has_availability <- listings$has_availability == "t"

listings$price_double         <- price_as_double(listings$price)
listings$weekly_price_double  <-
  price_as_double(listings$weekly_price)
listings$monthly_price_double <-
  price_as_double(listings$monthly_price)

listings$host_has_multi <-
  !is.na(listings$host_total_listings_count) &
  listings$host_total_listings_count > 1

# NYC Housing Projects Open Data
# --------------------------------------------------------------------------------

# Rename strangely labelled columns
housing <- housing %>%
  rename_column('Latitude..Internal.', 'Latitude.Internal') %>%
  rename_column('Longitude..Internal.', 'Longitude.Internal') %>%
  rename_column('NTA...Neighborhood.Tabulation.Area', 'NTA.Neighborhood.Tabulation.Area') %>%
  rename_column('X6.BR..Units', 'X6.BR.Units')

plot_hist(
  (housing %>% filter(is.na(Latitude)))$Total.Units,
  "Units of Housing Projects with No Latitude Specified",
  "Number of Units",
  "Frequency"
)
# These are generally very small projects -> we can ignore these projects for mapping
housing_with_coords <-
  housing %>% filter(!is.na(Latitude) & !is.na(Longitude))

housing %>% select(Total.Units, All.Counted.Units) %>% sample_n(10)
# These are the same, just stick with total units

# Merging scraped data with neighborhood data
# --------------------------------------------------------------------------------

names(neighborhoodsgeo)
# [1] "neighbourhood_group" "neighbourhood"

# Rename to be consistent with other data
neighborhoodsgeo <- neighborhoodsgeo %>%
  rename_column("neighbourhood", "neighborhood") %>%
  rename_column("neighbourhood_group", "neighborhood_group")

neighborhoodsgeo$neighborhood
neighborhoodsgeo$neighborhood_group

colnames(neighborhoods)
# [1] "neighbourhood_group" "neighbourhood"
# NOTE this dataset is effectively just for making joins/other aggregations easier

# Rename to be consistent with other data
neighborhoods <- neighborhoods %>%
  rename_column("neighbourhood", "neighborhood") %>%
  rename_column("neighbourhood_group", "neighborhood_group")

# Sanity check
neighborhoods %>% sample_n(8) %>% select("neighborhood")
neighborhoods %>% sample_n(8) %>% select("neighborhood_group")

colnames(nyc_inside_airbnb_data)
# [1] "value"               "numListings"         "entireHomePercent"   "pricePerNight"
# [5] "privateRoomPercent"  "sharedRoomPercent"   "nightsPerYear"       "occupancy"
# [9] "monthlyIncome"       "numHighAvailability" "avgAvailability365"

# Subset data based on geographic scope (whole city vs. borough vs. neighborhood)
neighborhood_group_data <-
  nyc_inside_airbnb_data %>% filter(value %in% neighborhoods$neighborhood_group)
neighborhood_data       <-
  nyc_inside_airbnb_data %>% filter(value %in% neighborhoods$neighborhood)
city_data               <-
  nyc_inside_airbnb_data %>% filter(value == 'New York City')

neighborhood_data %>% nrow() # -> 228
neighborhoods %>% nrow()     # -> 230
neighborhoods %>% filter(!(neighborhood %in% neighborhood_data$value)) %>%
  select(neighborhood) %>% tail(1)
#                neighborhood
# 1 Bay Terrace, Staten Island
# 2     Chelsea, Staten Island

plot_listings_in_neighborhood("Bay Terrace, Staten Island") # 1 listing
plot_listings_in_neighborhood("Chelsea, Staten Island")     # 0 listings
# -> these are not significant Airbnb markets and are very distant from NYC proper
# Investigating more in Google Maps, these are more suburban neighborhoods

neighborhoodsgeo <- merge(neighborhoodsgeo,
                          neighborhood_data,
                          by.x = "neighborhood",
                          by.y = "value")

# Merging with rental data
# --------------------------------------------------------------------------------

colnames(rent_inventory)
# [1] "areaName" "Borough"  "areaType" "X2010.01" "X2010.02" ... "X2020.03"

levels(rent_inventory$Borough)
# [1] ""  "Bronx"  "Brooklyn"  "Manhattan"  "Queens"  "Staten Island"

levels(rent_inventory$areaName)
# [1] "All Downtown"                    "All Midtown"                     "All Upper East Side"            
# [4] "All Upper Manhattan"             "All Upper West Side"             "Astoria"                        
# [7] "Auburndale"                      "Bath Beach"                      "Battery Park City"
# ...

colnames(rent_asking)
# [1] "areaName" "Borough"  "areaType" "X2010.01" "X2010.02" ... "X2020.03"

rent_data <- data.frame(rent_asking$areaName, rent_asking$Borough, rent_asking$X2020.03, rent_inventory$X2020.03)
colnames(rent_data) <- c("neighborhood", "neighborhood_group", "rent_asking", "rent_inventory")
rent_data %>% sample_n(4)
#    neighborhood neighborhood_group rent_asking rent_inventory
# 1       Gowanus           Brooklyn        3300             71
# 2      Edenwald              Bronx          NA              0
# 3  Coney Island           Brooklyn        2415             42
# 4 Schuylerville              Bronx          NA              1

# Filter to be neighborhoods we have Airbnb data for
rent_data <- rent_data %>% filter(neighborhood %in% neighborhoods$neighborhood)

names(neighborhoodsgeo)

neighborhoodsgeo <- merge(neighborhoodsgeo,
      rent_data %>% select("neighborhood", "rent_asking", "rent_inventory"),
      by.x = "neighborhood",
      by.y = "neighborhood")

names(neighborhoodsgeo)
#  [1] "neighborhood"        "neighborhood_group"  "numListings"         "entireHomePercent"  
#  [5] "pricePerNight"       "privateRoomPercent"  "sharedRoomPercent"   "nightsPerYear"      
#  [9] "occupancy"           "monthlyIncome"       "numHighAvailability" "avgAvailability365" 
# [13] "rent_asking"         "rent_inventory" 

# ================================================================================
# Understanding the Neighborhood Data
# ================================================================================

neighborhood_group_pal <-
  colorFactor("viridis", domain = neighborhoodsgeo$neighborhood_group)
leaflet(neighborhoodsgeo) %>%
  addMap() %>%
  addPolygons(
    stroke = FALSE,
    smoothFactor = 0.3,
    fillOpacity = 1,
    fillColor =  ~ neighborhood_group_pal(neighborhood_group),
    label =  ~ neighborhood_group
  ) %>%
  addLegend(
    pal = neighborhood_group_pal,
    values =  ~ neighborhood_group,
    opacity = 1,
    title = "Neighborhood Group"
  )

neighborhood_pal <-
  colorFactor("viridis", domain = neighborhoodsgeo$neighborhood)
leaflet(neighborhoodsgeo) %>%
  addMap() %>%
  addPolygons(
    stroke = FALSE,
    smoothFactor = 0.3,
    fillOpacity = 1,
    fillColor =  ~ neighborhood_pal(neighborhood),
    label =  ~ neighborhood
  )

# ================================================================================
# Filtering the data to only relevant listings
# ================================================================================

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

plot_pie(listings, "room_type", "Room Type")

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
sum(is.na(listings$monthly_price_double)) # -> 43454 (vast majority)
sum(is.na(listings$weekly_price_double))  # -> 42757 (not much better)
sum(is.na(listings$price_double))         # -> 0

listings %>% filter(!is.na(weekly_price_double)) %>% select(c(weekly_price_double, price_double)) %>%
  sample_n(10)
# These are prices that users pick to give special rates for longer term rentals
# These are NOT expected monthly/weekly revenues

# What variables correlate most strongly with price?
plot_cor_matrix(listings, c(
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
))
"
Nothing correlates particularly well with price except maybe square feet, number of beds,
and the number of people the listing accommodates

That is, ratings don't correlate particularly strongly with listing price, likely because
hosts have a lot of agency over the price that they list at (though Airbnb will
recommend certain prices).
"

# Digging into availability
listings %>% count(has_availability) # All have availability...not clear what this means
plot_hist(listings$availability_365,
          "365 Day Availability",
          "Availability",
          "Frequency")
plot_hist(listings$availability_30,
          "30 Day Availability",
          "Availability",
          "Frequency",
          30)
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
# plot_hist(
#   jan_listings$availability_365,
#   "365 Day Availability January",
#   "Availability",
#   "Frequency"
# )

listings %>% count(accommodates)
#    accommodates     n
#           <int> <int>
#  1            1  6907
#  2            2 22114
#  3            3  5275
#  4            4  7408
#  5            5  1967
#  6            6  2430
#  7            7   476
#  8            8   730
#  9            9    99
# 10           10   242
# 11           11    44
# 12           12   105
# 13           13    20
# 14           14    31
# 15           15    12
# 16           16   140
# 17           19     1
# 18           20     1
# 19           22     1
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

ggplot(data = listings %>%
         filter(is.na(beds) | beds <= MAX_NUM_BEDS),
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
  head(8) %>%
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

# ================================================================================
# Understanding variables in the data
# ================================================================================

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

# ================================================================================
# Looking at host data, specifically
# ================================================================================

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
      "host_neighborhood",
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
    y = price_double,
    fill = host_has_multi
  )) +
  coord_flip() +
  ylab('Price ($)') +
  xlab('Host has Multiple Listings') +
  ggtitle('Price split on if Host has Multiple Listings') +
  guides(fill = FALSE)
# There is no clear statistical trend

# ================================================================================
# Spatial analysis
# ================================================================================

# Plotting all Airbnb locations
plot_data(listings = listings)
"
Highly concentrated in Manhattan and Brooklyn which is disproportionate to actual housing
distribution across boroughs
"

listings_by_multihosts <-
  listings[listings$host_total_listings_count > 1, ]
nrow(listings_by_multihosts)
# -> 18985
plot_data(listings = listings_by_multihosts)
# Follows a similar distribution

plot_data(listings = listings[listings$license != "", ])
# Very few properties have a "license"

zeus_host_id <- 48005494
plot_data(listings = listings[listings$host_id == zeus_host_id, ])
# Zues is more strongly clustered in and around Manhattan
# Unique bucket in the financial district
# Locations are clustered together, perhaps making management and upkeep easier

# ================================================================================
# Neighborhood analysis
# ================================================================================

levels(listings$neighborhood)
# [1] ""                              "Allerton"
# [3] "Alphabet City"                 "Annadale"
# [5] "Astoria"                       "Bath Beach"
# [7] "Battery Park City"             "Bay Ridge"
# ...

levels(listings$neighborhood_cleansed)
# [1] "Allerton"                   "Arden Heights"              "Arrochar"
# [4] "Arverne"                    "Astoria"                    "Bath Beach"
# [7] "Battery Park City"          "Bay Ridge"                  "Bay Terrace"
# ...

length(unique(listings$neighborhood))          # -> 193
length(unique(listings$neighborhood_cleansed)) # -> 221
sum(listings$neighborhood == "")               # -> 10
sum(listings$neighborhood_cleansed == "")      # -> 0; this is a better feature to use
length(unique(listings$neighborhood_group_cleansed)) # -> 5

listings %>% count(neighborhood_group_cleansed)
#   neighborhood_group_cleansed     n
#   <fct>                       <int>
# 1 Bronx                        1002
# 2 Brooklyn                    14570
# 3 Manhattan                   15597
# 4 Queens                       4826
# 5 Staten Island                 323

plot_pie(listings,
         "neighborhood_group_cleansed",
         "Neighborhood Group")

listings %>% count(neighborhood_cleansed) %>% arrange(desc(n)) %>% head(10)
#     neighborhood_cleansed     n
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

# ================================================================================
# NYC housing data
# ================================================================================

colnames(housing)
#  [1] "Project.ID"                       "Project.Name"
#  [3] "Program.Group"                    "Project.Start.Date"
#  [5] "Project.Completion.Date"          "Building.ID"
#  [7] "Number"                           "Street"
#  [9] "Borough"                          "Postcode"
# [11] "BBL"                              "BIN"
# [13] "Community.Board"                  "Council.District"
# [15] "Census.Tract"                     "Latitude"
# [17] "Longitude"                        "Building.Completion.Date"
# [19] "Reporting.Construction.Type"      "Extended.Affordability.Only"
# [21] "Prevailing.Wage.Status"           "Extremely.Low.Income.Units"
# [23] "Very.Low.Income.Units"            "Low.Income.Units"
# [25] "Moderate.Income.Units"            "Middle.Income.Units"
# [27] "Other.Income.Units"               "Studio.Units"
# [29] "X1.BR.Units"                      "X2.BR.Units"
# [31] "X3.BR.Units"                      "X4.BR.Units"
# [33] "X5.BR.Units"                      "Unknown.BR.Units"
# [35] "Counted.Rental.Units"             "Counted.Homeownership.Units"
# [37] "All.Counted.Units"                "Total.Units"
# [39] "Latitude.Internal"                "Longitude.Internal"
# [41] "NTA.Neighborhood.Tabulation.Area" "X6.BR.Units"

levels(housing$Borough)
# [1] "Bronx"         "Brooklyn"      "Manhattan"     "Queens"        "Staten Island"

levels(neighborhoods$neighborhood_group)
# [1] "Bronx"         "Brooklyn"      "Manhattan"     "Queens"        "Staten Island"
# -> they match up!

housing %>% select(
  Extremely.Low.Income.Units,
  Low.Income.Units,
  Moderate.Income.Units,
  Middle.Income.Units
) %>%
  sample_n(10)

"
Plot these new project buildings in New York
Radius proportional to number of units in the building
"
plot_data(housing = housing)

colnames(housing)
housing %>% select(Postcode) %>% sample_n(10)
housing %>% nrow()

# Spatial join from housing projects to neighborhoods
sp::coordinates(housing_with_coords) <- ~ Longitude + Latitude
sp::proj4string(housing_with_coords) <-
  sp::proj4string(neighborhoodsgeo)

housing_idx_to_neighborhood <-
  sp::over(housing_with_coords, neighborhoodsgeo)
housing_idx_to_neighborhood %>% filter(!is.na(neighborhood)) %>% count(neighborhood)

plot_housing_in_neighborhood("Astoria")
plot_housing_in_neighborhood("SoHo")
plot_housing_in_neighborhood("Hell's Kitchen")
plot_housing_in_neighborhood_group("Manhattan")

# ================================================================================
# Synthesizing Data
# ================================================================================

# Mapping out some neightborhoods
plot_listings_in_neighborhood("Flatiron District")
plot_listings_in_neighborhood("Williamsburg")
plot_listings_in_neighborhood("East Harlem")

# Mapping out some boroughs
plot_listings_in_neighborhood_group("Brooklyn")
plot_listings_in_neighborhood_group("Manhattan")

plot_data_in_neighborhood("Stuyvesant Town")
plot_data_in_neighborhood("Upper East Side")
plot_data_in_neighborhood_group("Manhattan")

# Plot average income per Airbnb listing per neighborhood
plot_neighborhoods <- function (data, col, title) {
  pal <- colorNumeric("viridis", NULL)
  filtered_data <- data[!is.na(data[[col]]),]
  
  leaflet(filtered_data) %>%
    addMap() %>%
    addPolygons(
      stroke = FALSE,
      smoothFactor = 0.3,
      fillOpacity = 0.5,
      fillColor = ~ pal(filtered_data[[col]]),
      popup = ~ paste0(
        "<b>",
        neighborhood,
        "</b>",
        "<table><tbody>",
        "<tr><td>Avg. monthly income</td><td>$",
        monthlyIncome,
        "</td></tr>",
        "<tr><td>Avg. nightly price</td><td>$",
        pricePerNight,
        "</td></tr>",
        "<tr><td>Est. # listings</td><td>",
        numListings,
        "</td></tr>",
        "<tr><td>Est. # highly available</td><td>",
        numHighAvailability,
        "</td></tr>",
        "<tr><td>Est. occupancy rate</td><td>",
        occupancy,
        "</td></tr>",
        "<tr><td>Avg nights occupied/year</td><td>",
        nightsPerYear,
        "</td></tr>",
        "<tr><td><hr /></td><td><hr /></td></tr>",
        "<tr><td>Rent inventory</td><td>",
        ifelse(is.na(rent_inventory), "unknown", rent_inventory),
        "</td></tr>",
        "<tr><td>Avg. rent asking</td><td>",
        ifelse(is.na(rent_asking), "unknown", paste0("$", rent_asking)),
        "</td></tr>",
        "</tbody></table>"
      )
    ) %>%
    addLegend(
      pal = pal,
      values = ~ filtered_data[[col]],
      opacity = 1.0,
      title = title
    )
}

plot_neighborhoods(neighborhoodsgeo, "monthlyIncome", "Avg. Airbnb Income/Month")
plot_neighborhoods(neighborhoodsgeo, "rent_inventory", "StreetEasy Rent Inventory")
plot_neighborhoods(neighborhoodsgeo, "rent_asking", "StreetEasy Avg. Rent Asking")

plot_cor_matrix(neighborhoodsgeo@data, c("monthlyIncome", "rent_inventory", "rent_asking"))
# Strong correlation between asking price and Airbnb expected income
