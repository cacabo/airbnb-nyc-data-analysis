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
library(shiny)
library(shinythemes)

# ================================================================================
# Read in data
# ================================================================================

# Source: http://insideairbnb.com/get-the-data.html
listings <- read.csv('./data/listings.csv')
raw_num_listings <- nrow(listings)

neighborhoods <- read.csv('./data/neighbourhoods.csv')
neighborhoodsgeo <- readOGR('./data/neighbourhoods.geojson')

# Source: https://insideairbnb.com/new-york-city/
nyc_inside_airbnb_data <- read.csv('./data/nyc-inside-airbnb.csv')

# Source: https://data.cityofnewyork.us/Housing-Development/Housing-New-York-Units-by-Building/hg8x-zxpr
# Read more: https://www1.nyc.gov/site/housing/index.page
housing <- read.csv('./data/Housing_New_York_Units_by_Building.csv')

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
        "Listing by multi-listing host"
      )
    )
    
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

listings <- listings %>%
  rename_column("neighbourhood", "neighborhood") %>%
  rename_column("neighbourhood_cleansed", "neighborhood_cleansed") %>%
  rename_column("neighbourhood_group_cleansed",
                "neighborhood_group_cleansed") %>%
  rename_column("host_neighbourhood", "host_neighborhood")

# Convert to date
listings$first_review_date <-
  as.Date(listings$first_review, format = "%Y-%m-%d")
listings$last_review_date  <-
  as.Date(listings$last_review, format = "%Y-%m-%d")
listings$last_scraped_date <-
  as.Date(listings$last_scraped, format = "%Y-%m-%d")

# Convert to TRUE/FALSE
listings$has_availability <- listings$has_availability == "t"

# Format price columns as numbers
listings$price_double         <- price_as_double(listings$price)
listings$weekly_price_double  <-
  price_as_double(listings$weekly_price)
listings$monthly_price_double <-
  price_as_double(listings$monthly_price)

# Add boolean column for if host has several listings
listings$host_has_multi <-
  !is.na(listings$host_total_listings_count) &
  listings$host_total_listings_count > 1

# NYC Housing Projects Open Data
# --------------------------------------------------------------------------------

# Rename strangely labelled columns
housing <- housing %>%
  rename_column('Latitude..Internal.', 'Latitude.Internal') %>%
  rename_column('Longitude..Internal.', 'Longitude.Internal') %>%
  rename_column('NTA...Neighborhood.Tabulation.Area',
                'NTA.Neighborhood.Tabulation.Area') %>%
  rename_column('X6.BR..Units', 'X6.BR.Units')

housing_no_latitude_plot <- plot_hist(
  (housing %>% filter(is.na(Latitude)))$Total.Units,
  "Units of Housing Projects with No Latitude Specified",
  "Number of Units",
  "Frequency"
)

# These are generally very small projects -> we can ignore these projects for mapping
housing_with_coords <-
  housing %>% filter(!is.na(Latitude) & !is.na(Longitude))

# Merging scraped data with neighborhood data
# --------------------------------------------------------------------------------

# Rename to be consistent with other data
neighborhoodsgeo <- neighborhoodsgeo %>%
  rename_column("neighbourhood", "neighborhood") %>%
  rename_column("neighbourhood_group", "neighborhood_group")

# Rename to be consistent with other data
neighborhoods <- neighborhoods %>%
  rename_column("neighbourhood", "neighborhood") %>%
  rename_column("neighbourhood_group", "neighborhood_group")

# Subset data based on geographic scope (whole city vs. borough vs. neighborhood)
neighborhood_group_data <-
  nyc_inside_airbnb_data %>% filter(value %in% neighborhoods$neighborhood_group)
neighborhood_data       <-
  nyc_inside_airbnb_data %>% filter(value %in% neighborhoods$neighborhood)
city_data               <-
  nyc_inside_airbnb_data %>% filter(value == 'New York City')

neighborhoods %>% filter(!(neighborhood %in% neighborhood_data$value)) %>%
  select(neighborhood) %>% tail(1)
#                neighborhood
# 1 Bay Terrace, Staten Island
# 2     Chelsea, Staten Island

bay_terrace_staten_island_map <- plot_listings_in_neighborhood("Bay Terrace, Staten Island") # 1 listing
chelsea_staten_island_map <- plot_listings_in_neighborhood("Chelsea, Staten Island")     # 0 listings
# -> these are not significant Airbnb markets and are very distant from NYC proper
# Investigating more in Google Maps, these are more suburban neighborhoods

neighborhoodsgeo <- merge(neighborhoodsgeo,
                          neighborhood_data,
                          by.x = "neighborhood",
                          by.y = "value")

# Merging with rental data
# --------------------------------------------------------------------------------

rent_data <-
  data.frame(
    rent_asking$areaName,
    rent_asking$Borough,
    rent_asking$X2020.03,
    rent_inventory$X2020.03
  )
colnames(rent_data) <-
  c("neighborhood",
    "neighborhood_group",
    "rent_asking",
    "rent_inventory")

# Filter to be neighborhoods we have Airbnb data for
rent_data <-
  rent_data %>% filter(neighborhood %in% neighborhoods$neighborhood)

# Merge data together
neighborhoodsgeo <- merge(
  neighborhoodsgeo,
  rent_data %>% select("neighborhood", "rent_asking", "rent_inventory"),
  by.x = "neighborhood",
  by.y = "neighborhood"
)

# ================================================================================
# Understanding the Neighborhood Data
# ================================================================================

neighborhood_group_pal <-
  colorFactor("viridis", domain = neighborhoodsgeo$neighborhood_group)
neighborhood_group_map <- leaflet(neighborhoodsgeo) %>%
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
neighborhood_map <- leaflet(neighborhoodsgeo) %>%
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

room_type_table <- listings %>% count(room_type)
room_type_plot <- plot_pie(listings %>% select(room_type), "room_type", "Room Type")

listings <-
  listings %>% filter(room_type %in% c("Entire home/apt", "Private room"))

# Also focus on specific property types
property_type_table <- listings %>% count(property_type) %>% arrange(desc(n)) %>% head(10)

relevant_property_types <- c(
  "Apartment",
  "House",
  "Townhouse",
  "Condominium",
  "Loft",
  "Guest suite",
  "Serviced apartment"
)

listings <- listings %>% filter(
  property_type %in% relevant_property_types
)

# Filter to only include properties which are not uniquely used
# for travel but are often rented / used for long term housing

# Plot monthly price per listing
price_plot <- plot_hist(listings$price_double,
          'Listing Prices',
          'Price ($)',
          'Frequency',
          60)
monthly_price_plot <- plot_hist(
  listings$monthly_price_double,
  'Monthly Listing Prices',
  'Monthly Price ($)',
  'Frequency',
  60
)

MAX_PRICE <- 1000
MAX_MONTHLY_PRICE <- 12000


# What variables correlate most strongly with price?
listings_cor_plot <- plot_cor_matrix(listings, c(
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

# Digging into availability
availability_plot <- plot_hist(listings$availability_365,
          "365 Day Availability",
          "Availability",
          "Frequency")

no_availability_table <- listings %>%
  filter(availability_365 == 0) %>%
  select(listing_url) %>%
  sample_n(10)

accommodates_table <- listings %>% count(accommodates) 
MAX_ACCOMMODATES = 14
listings <- listings %>% filter(accommodates <= MAX_ACCOMMODATES)

MAX_NUM_BEDS <- 8

beds_price_plot <- ggplot(data = listings %>%
         filter(beds <= MAX_NUM_BEDS) %>%
         filter(price_double <= MAX_PRICE)) +
  geom_boxplot(aes(x = as.factor(beds), y = price_double, fill = beds)) +
  coord_flip() +
  ylab('Price ($)') +
  xlab('Number of Beds') +
  ggtitle('Price for Listing by Number of Beds',
          subtitle = 'Excluding num beds > 8 or price > $1,000') +
  guides(fill = FALSE)

# We will not filter them out
listings <- listings %>%
  filter(is.na(beds) | beds < MAX_NUM_BEDS) %>%
  filter(is.na(monthly_price_double) |
           monthly_price_double <= MAX_MONTHLY_PRICE) %>%
  filter(price_double <= MAX_PRICE)

no_reviews_table <- listings %>%
  filter(number_of_reviews == 0) %>%
  head(5) %>%
  select(id, description)

num_reviews_plot <- plot_hist(listings$number_of_reviews,
          'Number of Reviews',
          'Count',
          'Frequency')

# NOTE "ltm" means "last twelve months", "ntm" means "next twelve months"
num_reviews_ltm_plot <- plot_hist(listings$number_of_reviews_ltm,
          'Number of Reviews in Last Year',
          'Count',
          'Frequency')
 
# First review date is a proxy for when these listings were created
first_review_plot <- plot_hist(listings$first_review_date,
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

# Filter out listings last reviewed more than 1 year ago
get_date_1_yr_ago <- function() {
  d <- as.POSIXlt(Sys.Date())
  d$year <- d$year - 1
  return(as.Date(d))
}

listings <- listings %>%
  filter(is.na(last_review_date) |
           last_review_date > get_date_1_yr_ago())

review_dates_plot <- plot_review_dates(listings)

# ================================================================================
# Looking at host data, specifically
# ================================================================================

multihost_date_plot <- ggplot(listings[!is.na(listings$host_has_multi), ]) +
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
hosts_table <- hosts %>%
  arrange(desc(host_listings_count)) %>%
  select(c('host_id', 'host_name', 'host_listings_count')) %>%
  head(20)

sum(is.na(hosts$host_listings_count)) # -> 5
sum(hosts$host_listings_count == 0, na.rm = TRUE)
# 3974 hosts have no listings...maybe there is some sort of federated system

hosts_with_listings <- hosts[hosts$host_listings_count > 0, ]

hosts_listings_count_plot <- qplot(
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
multihost_price_plot <- ggplot(data = listings) +
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

listings_neighborhood_group_table <- listings %>% count(neighborhood_group_cleansed)
listings_neighborhood_group_plot <- plot_pie(listings,
         "neighborhood_group_cleansed",
         "Neighborhood Group")
listings_neighborhood_table <- listings %>% count(neighborhood_cleansed) %>% arrange(desc(n)) %>% head(10)

# ================================================================================
# NYC housing data
# ================================================================================

# Spatial join from housing projects to neighborhoods
sp::coordinates(housing_with_coords) <- ~ Longitude + Latitude
sp::proj4string(housing_with_coords) <-
  sp::proj4string(neighborhoodsgeo)

housing_idx_to_neighborhood <-
  sp::over(housing_with_coords, neighborhoodsgeo)
housing_idx_to_neighborhood %>% filter(!is.na(neighborhood)) %>% count(neighborhood)

# TODO keep copying things

# ================================================================================
# Shiny App
# ================================================================================

server <- function(input, output) {
  output$room_type_table <- renderTable(room_type_table)
  output$room_type_plot <- renderPlot({
    room_type_plot
  })
  output$property_type_table <- renderTable(property_type_table)
  output$price_plot <- renderPlot({ price_plot })
  output$monthly_price_plot <- renderPlot({ monthly_price_plot })

  output$bay_terrace_staten_island_map <- renderLeaflet({
    bay_terrace_staten_island_map
  })
  output$chelsea_staten_island_map <- renderLeaflet({
    chelsea_staten_island_map
  })

  output$listings_cor_plot <- renderPlot({ listings_cor_plot })
  output$availability_plot <- renderPlot({ availability_plot })
  output$no_availability_table <- renderTable(no_availability_table)
  output$accommodates_table <- renderTable(accommodates_table)
  output$beds_price_plot <- renderPlot({ beds_price_plot })
  output$no_reviews_table <- renderTable(no_reviews_table)

  output$num_reviews_plot <- renderPlot({ num_reviews_plot })
  output$num_reviews_ltm_plot <- renderPlot({ num_reviews_ltm_plot })
  output$first_review_plot <- renderPlot({ first_review_plot })
  output$review_dates_plot <- renderPlot({ review_dates_plot })
  output$multihost_date_plot <- renderPlot({ multihost_date_plot })

  output$hosts_table <- renderTable(hosts_table)
  output$hosts_listings_count_plot <- renderPlot({ hosts_listings_count_plot })
  output$multihost_price_plot <- renderPlot({ multihost_price_plot })

  output$listings_neighborhood_group_table <- renderTable(listings_neighborhood_group_table)
  output$listings_neighborhood_group_plot <- renderPlot({ listings_neighborhood_group_plot })
  output$listings_neighborhood_table <- renderTable(listings_neighborhood_table)

  output$neighborhood_group_map <-
    renderLeaflet({
      neighborhood_group_map
    })
  output$neighborhood_map <- renderLeaflet({
    neighborhood_map
  })
  
  output$housing_no_latitude_plot <-
    renderPlot({
      housing_no_latitude_plot
    })

  output$neighborhood_group_data <- DT::renderDataTable(DT::datatable({
    neighborhood_group_data %>%
      select(
        c(
          "value",
          "numListings",
          "entireHomePercent",
          "privateRoomPercent",
          "pricePerNight",
          "nightsPerYear",
          "monthlyIncome",
          "numHighAvailability",
          "avgAvailability365"
        )
      )
  }))
  
  output$rent_data <- DT::renderDataTable(DT::datatable({
    neighborhoodsgeo@data %>%
      select(
        c(
          "neighborhood",
          "numListings",
          "pricePerNight",
          "monthlyIncome",
          "rent_asking",
          "rent_inventory"
        )
      )
  }))

  output$housing_data <- DT::renderDataTable(DT::datatable({
    housing %>%
      select(
        c(
          "Project.Name",
          "Project.Start.Date",
          "Project.Completion.Date",
          "Street",
          "Borough",
          "Latitude",
          "Longitude",
          "Reporting.Construction.Type",
          "Low.Income.Units",
          "Total.Units"
        )
      )
  }))
}

container <- function (...)
  fluidRow(column(8, offset = 2, ...))

ui <- shinyUI(fluidPage(
  theme = shinytheme("flatly"),
  navbarPage(
    "Airbnb and Rentals in NYC",
    tabPanel(
      "Data Overview",
      container(
        h1("Data Overview"),
        
        br(),
        
        h3("Inside Airbnb NYC Listing Data"),
        p(
          "Inside Airbnb is a website which scrapes Airbnb listing data for cities around the world. The data
          contains almost all of the information which one can glean from looking at a listing on the Airbnb
          website in addition to information about the listing's host, namely how many other listings they
          operate."
        ),
        p(
          "A major challenge was filtering this data such that the listings are actually representative of the
          market in NYC and of properties which could be more traditional rentals. This analysis is explained in
          depth on the next page."
        ),
        p('There are ', raw_num_listings, ' unfiltered listings in the NYC dataset. ',
          'I filtered the listings down to ', listings %>% nrow(), ' more relevant listings.'
        ),
        p(
          a(href = "http://insideairbnb.com/get-the-data.html", "Source")
        ),
        
        br(),
        
        h3("Scraped Inside Airbnb Neighborhood Data"),
        p(
          "Inside Airbnb does not provide its aggregated data in a downloadable form as it does for its
          normal listing data. One advantage of the aggregated data is they have a model for tracking
          and predicting how often Airbnbs are booked over time and can use this to predict how much
          revenue a host makes per month. This makes it easier to compare Airbnb listings with more
          traditional rental listings in the city."),
        p(
          "To access this data, I wrote a simple JavaScript which runs in the browser console to scrape
          this data from the website and write it to a CSV string."),
        p(a(href = "https://insideairbnb.com/new-york-city/", "Source"))
      ),
      DT::dataTableOutput("neighborhood_group_data"),

      container(
        p(
          "The scraped data was missing two neighborhoods, however, as show in the plots below, these
          neighborhoods have very low Airbnb activity and are also not large rental markets:")
      ),

      fluidRow(column(
        6,
        h4("Bay Terrace, Staten Island"),
        leafletOutput("bay_terrace_staten_island_map")
      ),
      column(
        6,
        h4("Chelsea, Staten Island"),
        leafletOutput("chelsea_staten_island_map")
      )),

      br(),
        
      container(
        h3("NYC Geospatial Data"),
        p(
          "GeoJSON data for all boroughs (neighborhood groups) and neighborhoods in New York City."
        ),
        p(
          paste0(
            "There are ",
            length(neighborhoodsgeo$neighborhood),
            " neighborhoods and 5 boroughs in the dataset."
          )
        ),
        p(
          a(href = "http://insideairbnb.com/get-the-data.html", "Source")
        )
      ),
      fluidRow(column(
        6,
        h4("Boroughs"),
        leafletOutput("neighborhood_group_map")
      ),
      column(
        6,
        h4("Neighborhoods"),
        leafletOutput("neighborhood_map")
      )),
      
      br(),
      
      container(
        h3("StreetEasy NYC Rental Data"),
        p(
          "StreetEasy is a data aggregator for rentals and housing sales. They offer up some of their data in the
          open domain. For this project I looked at the rental inventory in a given neighborhood and the average
          asking price for rental properties."
        ),
        p(
          a(href = "https://streeteasy.com/blog/data-dashboard", "Source")
        ),
        p(
          "Joining this with the neighborhood data and scraped data above, the data looks like (omitting some columns):"
        )
      ),
      DT::dataTableOutput("rent_data"),

      br(),

      container(
        h3("Housing NYC Projects Data"),
        p(
          "This dataset is provided via NYC's OpenData initiative. The dataset contains projects began in 2014 or later
          which are part of the Housing New York plan enacted by Mayor Bill de Blasio to \"create and preserve 200,000
          high-quality, affordable homes over ten years.\" Thus, these data points provide a window into where and how
          much the city is investing in affordable housing for lower income groups."
        ),
        p(
          "The dataset breaks down the location of each project, when it occurred, various identifiers, and the
          breakdown of units by intended income group (for example, how many units are for very low income individuals?)."
        ),
        p(
          a(href = "https://data.cityofnewyork.us/Housing-Development/Housing-New-York-Units-by-Building/hg8x-zxpr", "Source"),
          br(),
          a(href = "https://www1.nyc.gov/site/housing/index.page", "Read More")
        ),
        br(),
        fluidRow(column(
          6,
          p(strong("A note on locations")),
          p(
            "Some of the projects in the dataset have no longitude or latitude coordinates provided. From the histogram
            to the right we see these projects are of far lower scale than the majority of projects in the dataset, so
            it is OK if we do not plot these points alongside the Airbnb listings."
          )
        ),
        column(
          6, plotOutput("housing_no_latitude_plot")
        ))
      ),
      DT::dataTableOutput("housing_data")
    ),

    tabPanel("Filtering Listings",
      container(
        h1("Filtering Listings"),
        p(
          "Through this section of analysis I aim to answer the question of what
          subset of the listings could make valid rental properties. Futher, I
          look into pricing and classification trends in these listings to see if
          that can inform our analysis on the impact Airbnb has on housing and
          rentals in NYC."
        ),
        br(),
        p("First, we consider the types of listings:"),
        fluidRow(
          column(6, tableOutput("room_type_table")),
          column(6, plotOutput("room_type_plot"))
        ),
        br(),
        p("Next, we consider the property type that the listing is on:"),
        tableOutput("property_type_table"),
        p(paste0("For listings to be most representative of traditional rentals, I honed in on: ", paste(
          relevant_property_types, collapse=", "
        ), ".")),
        br(),
        p("For this subset of listings, we consider their pricing distribution
          both nightly and monthly (note that not all listings have a monthly
          price, this is up to hosts if they want a unique monthly rate):")
      ),

      plotOutput("price_plot"),
      plotOutput("monthly_price_plot"),

      container(
        p(paste0(
          "From these histograms, I removed expensive outliers which likely could
          not serve as rental properties. Namely I enforced a max price of $",
          MAX_PRICE, " and a max monthly price of $", MAX_MONTHLY_PRICE, "."))
      ),
      br(),
      container(
        p("For context, here are the correlation between numeric columns in the data:"),
        plotOutput("listings_cor_plot", height = "600px"),
        br(),
        p(
          "Nothing correlates particularly well with price except maybe square feet, number of beds,
          and the number of people the listing accommodates. That is, ratings don't correlate
          particularly strongly with listing price, likely because hosts have a lot of agency over 
          the price that they list at (though Airbnb will recommend certain prices)."),
        br(),
        p("Next, I looked into the availability of listings over the next year:")
      ),
      plotOutput("availability_plot"),
      container(
        p("Interestingly, many of the listings have 0 availability over the next year. Looking into
          samples of the data, however, these appear to be legitimate listings. Perhaps there are
          just different protocols for booking these listings or they have already been booked by
          businesses or long term renters:"),
        tableOutput("no_availability_table"),
        p("I also looked at data from January (before the Coronavirus was a widespread issue in the
          US, and this same trend held, thus the pandemic is not a cause for this."),

        br(),

        p(
          "Another relevant feature in the data is how many people a listing can accommodate. This
          is not the most trustworthy data point since hosts select it themselves, though it does
          correlate with actual size. I wanted to filter out listings which were larger than a
          traditional rental and are instead intended for large gatherings:"),
        tableOutput("accommodates_table"),
        p(paste0(
          "Looking at a sample of listings which accommodate many people, the majority of these are
          not proper homes or apartments and therefore would likely not be rentals. A threshold of ",
          MAX_ACCOMMODATES, " seems to be the best line between large homes and effectively venues.")),
        p(paste0("Under similar analysis, I put a threshold on number of beds in the listing to be ",
          MAX_NUM_BEDS, ".")),
        p("Under these constraints, we have a better picture of pricing of listings which fall within
          rental-property size:")
      ),

      plotOutput("beds_price_plot"),

      container(
        p(
          "Sampling the data, we see that beds marked as NA or 0 tend to be normal bedroom listings.
          Most likely, the hosts just did not provide this number in these cases."),

        br(),

        p("Next, I wanted to gauge which listings are authentic and active as based
          on their review data. To begin, I dove into listings with no reviews:"),
        tableOutput("no_reviews_table"),
        p(
          "These seem to be real enough to potentially not be duplicates or fake.
          Airbnb actively works to remove fraudulent listings, so they likely can
          do a better job of weeding such listings out."
        ),
        p("For listings which do have reviews, the distributions look like:")
      ),

      fluidRow(
        column(6, plotOutput("num_reviews_plot")),
        column(6, plotOutput("num_reviews_ltm_plot"))
      ),
      plotOutput("first_review_plot"),

      container(
        p(
          "To remove some inactive listings, I filtered out listings which have been revied but
          were last reviewed more than 1 year ago. This gives us the following distribution:"),
        plotOutput("review_dates_plot"),
        p(
          "Note we are using first review date as a proxy for when the listing was created. The
          vast majority are still quite active into the start of 2020. We also see that the last
          reviews curve is far more volatile than the first reviews curve indicating some notion
          of seasonality."),
        p("Having pored over the columns in the data, we now have listings which are certainly
          more representative of potential rental listings than the original raw dataset.")
      )
    ),

    tabPanel("Airbnb Hosts",
      container(
        h1("Airbnb Hosts"),
        plotOutput("multihost_date_plot"),
        tableOutput("hosts_table"),
        p("We see a mix of agencies and people with oddly simple names...basically agencies under cover."),
        plotOutput("hosts_listings_count_plot"),
        p(
          "The vast majority of hosts own only a handlful of rentals. Those who own on the order of hundreds
          are largely businesses, sometimes with actual business names and sometimes with host names which
          under the hood are enterprises."),
        plotOutput("multihost_price_plot"),
        p("There is no clear statistical trend in pricing for hosts with mutliple listings versus hosts
          with a single listing."),

        tableOutput("listings_neighborhood_group_table"),
        plotOutput("listings_neighborhood_group_plot"),
        tableOutput("listings_neighborhood_table"),

        p("TODO add text between plots")
      )
    ),

    tabPanel("Housing and Listings", headerPanel("TODO")),
    tabPanel("Aggregations", headerPanel("TODO")),
    tabPanel("Policy Recommendations", headerPanel("TODO"))
  ),

  div(
    p(
      "Created by ",
      a(href = "https://www.cameroncabo.com", "Cameron Cabo"),
      " for GAFL 531: Data Science for Public Policy at the University of Pennsylvania in April 2020. ",
      a(href = "https://github.com/cacabo/airbnb-nyc-data-analysis", "GitHub."),
      style = "color: rgba(0, 0, 0, 0.64); font-size: 80%;"
    ),
    style = "width: 100%; padding-top: 1rem; border-top: 1px solid rgba(0, 0, 0, 0.1); margin-top: 1rem;"
  )
))

shinyApp(ui = ui, server = server)

# TODO reactive stuff
# TODO css or font size for tables