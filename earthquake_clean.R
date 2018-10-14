library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(sp)
library(geojsonio)
library(maps)
library(ggthemes)
library(gganimate)

#Import data from NOAA on significant earthquakes in recorded geologic history
signif_earthquakes <- read_delim("signif (3).txt", 
                                 "\t", escape_double = FALSE, trim_ws = TRUE)

#Save as data frame
signif_earthquakes <- as.data.frame(signif_earthquakes, stringsAsFactors = FALSE)

#Select relevant headings
signif_earthquakes <- select(signif_earthquakes, "YEAR", "MONTH", "DAY", "HOUR", "MINUTE", "SECOND", "FOCAL_DEPTH", "EQ_MAG_UNK", "COUNTRY", "LOCATION_NAME", "LATITUDE", "LONGITUDE")

#Change headings to lowercase
signif_earthquakes <- setNames(signif_earthquakes, tolower(names(signif_earthquakes)))

#Select data from 1935 to present
signif_earthquakes <- filter(signif_earthquakes, year > 1964)

#Remove any magnitude values that are NA (indicates a recorded earthquake with no measurement)
signif_earthquakes$eq_mag_unk[is.na(signif_earthquakes$eq_mag_unk)] <- 0
signif_earthquakes <- filter(signif_earthquakes, eq_mag_unk > 0)

head(signif_earthquakes)

#Replace NA with zero value in hour, minute, second columns
signif_earthquakes$hour[is.na(signif_earthquakes$hour)] <- 0
signif_earthquakes$minute[is.na(signif_earthquakes$minute)] <- 0
signif_earthquakes$second[is.na(signif_earthquakes$second)] <- 0

#Create date column by joining Year, Month, Day 
signif_earthquakes <- unite(signif_earthquakes, date, c("year", "month", "day"), sep = "-")

#Create Time Column by joining Hour, Minute, Second
signif_earthquakes <- unite(signif_earthquakes, time, c("hour", "minute", "second"), sep = ":")
signif_earthquakes$time <- hms::as.hms(signif_earthquakes$time)

#Rename eq_mag_unk column as magnitude
colnames(signif_earthquakes)[4] <- "magnitude"

head(signif_earthquakes)



#Import data from USGS on earthquakes larger than 5.5 (as measured on the Richter Scale) from 1965-2016
USGS_earthquakes <- read_csv("database.csv")

#Save USGS data set as a data frame (from csv)
USGS_df <- as.data.frame(USGS_earthquakes)

#Select relevant headings
USGS_df <- select(USGS_df, "Date", "Time", "Latitude", "Longitude", "Depth", "Magnitude")

#Set column headings to lowercase
USGS_df <- setNames(USGS_df, tolower(names(USGS_df)))

#Format date to YYYY-MM-DD
USGS_df$date <- format(as.Date(USGS_df$date, "%m/%d/%Y"), "%Y-%m-%d")

#Rename depth as "focal_depth"
colnames(USGS_df)[5] <- "focal_depth"

head(USGS_df)

#Join tables together via date, time, latitude, and longitude
earthquakes <- full_join(signif_earthquakes, USGS_df, by = c("date", "time", "latitude", "longitude", "magnitude", "focal_depth"))

summary(earthquakes)

# Import data for tectonic plate boundaries
plate_data <- "PB2002_plates.json"
plates <- geojson_read(plate_data, what = "sp")  #SpatialPointsDataFrame

plot(plates)

# convert list of earthquake points into a SpatialPointsDataFrame
coordinates(earthquakes) <- ~ longitude + latitude

# convert earthquakes to use the same coordinate system as plates (needed for overlay)
proj4string(earthquakes) <- proj4string(plates)

# Create overlay
earthquakes_plates <- over(earthquakes, plates)


# Attach the resulting columns that we got from over to the rows of eqs
earthquakes$LAYER <- earthquakes_plates$LAYER
earthquakes$Code <- earthquakes_plates$Code
earthquakes$PlateName <- earthquakes_plates$PlateName

earthquakes <- as.data.frame(earthquakes)


head(earthquakes, n = 50L)

#separate date column into year, month, and day
earthquakes <- separate(earthquakes, date, into = c("year", "month", "day"))

#Filter outliers (1 earthquake recorded in 2017, 2018)
earthquakes <- filter(earthquakes, year < 2017)
earthquakes <- filter(earthquakes, magnitude > 5.5)



#Bar graph of year vs # of major earthquakes (1965-2016)
ggplot(earthquakes, aes(factor(year))) + 
  geom_bar(stat = "count", fill = "dark red") + 
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5), text=element_text(size=9)) + 
  ggtitle("Number of Major Earthquakes per Year")


#Boxplot of Plate name vs magnitude
ggplot(earthquakes, aes(x = PlateName, y = magnitude)) + 
  geom_boxplot() + 
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5), text=element_text(size=9)) +
  ggtitle("Outliers in magnitude on tectonic plates")



#Map Earthquake locations by magnitude (colored per tectonic plate location)
world <- ggplot() +
  borders("world", colour = "gray85", fill = "gray80") +
  theme_map() 

plate_map <- world +
  geom_point(aes(x = longitude, y = latitude, size = magnitude, color = PlateName), data = earthquakes, alpha = .5) +
  geom_point(data = plates, aes(x = long, y = lat), fill = 'black', stroke = 1) +
  scale_size_continuous(range = c(1, 8), breaks = c(6, 7, 8, 9)) +
  labs(size = 'magnitude', color = 'Plate Name') +
  theme(legend.position = "right")

plot(plate_map)

#Map earthquake locations by year and magnitude (colored by year, size = magnitude)
year_map <- world +
  geom_point(aes(x = longitude, y = latitude, size = magnitude, color = year), data = earthquakes, alpha = .5) +
  geom_point(data = plates, aes(x = long, y = lat), fill = 'black', stroke = 1) +
  scale_size_continuous(range = c(1, 8), breaks = c(6, 7, 8, 9)) +
  labs(size = 'magnitude', color = 'Year') +
  theme(legend.position = "right")

plot(year_map)

#Earthquakes from 2006-2016
earthquakes2006_2016 <- filter(earthquakes, year > 2005)

decade_map <- world +
  geom_point(aes(x = longitude, y = latitude, size = magnitude, color = year), data = earthquakes2006_2016, alpha = .5) +
  geom_point(data = plates, aes(x = long, y = lat), fill = 'black', stroke = 1) +
  scale_size_continuous(range = c(1, 8), breaks = c(6, 7, 8, 9)) +
  labs(size = 'magnitude', color = 'Year') +
  theme(legend.position = "right")

plot(decade_map)


#Unite month, day, year columns
earthquakes <- unite(earthquakes, quake_date, c("year", "month", "day"), sep = "-")

#Create scatterplot of Magnitude vs Date
ggplot(earthquakes, aes(x = magnitude, y = quake_date)) +
  geom_jitter() +
  facet_grid(.~ PlateName)+
  ggtitle("Magnitude vs. Date")


#Animate progression of earthquakes
ghost_points_ini <- tibble(
  quake_date = as.Date('1965-01-01'),
  magnitude = 0, longitude = 0, latitude = 0)

ghost_points_fin <- tibble(
  quake_date = seq(as.Date('2017-01-01'),
                   as.Date('2017-01-02'),
                   by = 'days'),
  magnitude = 0, longitude = 0, latitude = 0)

animated_map <- world +
  geom_point(aes(x = longitude, y = latitude, size = magnitude, color = PlateName), data = earthquakes, alpha = .5) +
  geom_point(data = plates, aes(x = long, y = lat), fill = 'black', stroke = 1) +
  geom_point(aes(x = longitude, y = latitude, size = magnitude, color = PlateName, # this is the init transparent frame
                 frame = quake_date,
                 cumulative = FALSE),
             data = ghost_points_ini, alpha = 0) +
  geom_point(aes(x = longitude, y = latitude, size = magnitude, color = PlateName, # this is the final transparent frames
                 frame = quake_date,
                 cumulative = FALSE),
             data = ghost_points_fin, alpha = 0) +
  scale_size_continuous(range = c(1, 8), breaks = c(6, 7, 8, 9)) +
  labs(size = 'magnitude', color = 'Plate Name') +
  theme(legend.position = "right")

gganimate::gg_animate(animated_map)