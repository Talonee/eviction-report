# Analysis script: compute values and create graphics of interest
library("dplyr")
library("ggplot2")
library("lubridate")
library("tidyr")
library("ggmap")

install.packages("lubridate")
install.packages("tidyr")
install.packages("ggmap")

# Load in your data
evictions <- read.csv("data/Eviction_Notices.csv", 
                      stringsAsFactors = FALSE)

# Compute some values of interest and store them in variables for the report


# How many evictions were there?
num_evictions <- nrow(evictions); num_evictions
num_features <- ncol(evictions); num_features

# Create a table (data frame) of evictions by zip code (sort descending)
by_zip <- evictions %>% 
  group_by(Eviction.Notice.Source.Zipcode) %>% 
  count() %>% 
  arrange(-n) %>% # sort last column
  ungroup() %>% 
  top_n(10, wt=n)
by_zip

# Create a plot of the number of evictions each month in the dataset
as.Date("10/6/17", format = "%m/%d/%y")
by_month <- evictions %>% 
  mutate(date = as.Date(File.Date, format = "%m/%d/%y")) %>% 
  mutate(month = floor_date(date, unit = "month")) %>% 
  group_by(month) %>% 
  count()
by_month

# Store plot in a variable
month_plot <- ggplot(data=by_month) + 
  geom_line(mapping = aes(x=month, y=n), color="blue", alpha =.5) + 
  labs(x="Date",y="Number of Evictions", title = "Evictions over time in SF")
month_plot

# Map evictions in 2017 

# Format the lat/long variables, filter to 2017
evictions_2017 <- evictions %>% 
  mutate(date = as.Date(File.Date, format="%m/%d/%y")) %>% 
  filter(format(date, "%Y") == "2017") %>%
  separate(Location, c("lat", "long"), ", ") %>% # split the column at the comma
  mutate(
    lat = as.numeric(gsub("\\(", "", lat)), # remove starting parentheses
    long = as.numeric(gsub("\\)", "", long)) # remove closing parentheses
  ) 
nrow(evictions_2017)
View(evictions_2017)


# Create a maptile background
base_plot <- qmplot(
  data = evictions_2017,        # name of the data frame
  x = long,                     # data feature for longitude
  y = lat,                      # data feature for latitude
  geom = "blank",               # don't display data points (yet)
  maptype = "toner-background", # map tiles to query
  darken = .7,                  # darken the map tiles
  legend = "topleft"            # location of legend on page
)
base_plot

# Add a layer of points on top of the map tiles
evictions_plot <- base_plot +
  geom_point(mapping = aes(x = long, y = lat), color = "red", alpha = .3) +
  labs(title = "Evictions in San Francisco, 2017") +
  theme(plot.margin = margin(.3, 0, 0, 0, "cm")) # adjust spacing around the map
evictions_plot
