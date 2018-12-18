
# import packages ---------------------------------------------------------

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
library(caTools)


# Import NOAA data set ----------------------------------------------------


#Import data from NOAA on significant earthquakes in recorded geologic history
signif_earthquakes <- read_delim("signif (3).txt", 
                                 "\t", escape_double = FALSE, trim_ws = TRUE)

#Save as data frame
signif_earthquakes <- as.data.frame(signif_earthquakes, 
                                    stringsAsFactors = FALSE)

#Select relevant headings
signif_earthquakes <- select(signif_earthquakes, "YEAR", "MONTH", "DAY", "HOUR",
                             "MINUTE", "SECOND", "FOCAL_DEPTH", "EQ_MAG_UNK", 
                             "COUNTRY", "LOCATION_NAME", "LATITUDE", "LONGITUDE")

#Change headings to lowercase
signif_earthquakes <- setNames(signif_earthquakes, 
                               tolower(names(signif_earthquakes)))

#Select data from 1935 to present
signif_earthquakes <- filter(signif_earthquakes, year > 1964)

#Remove any magnitude values that are NA 
signif_earthquakes$eq_mag_unk[is.na(signif_earthquakes$eq_mag_unk)] <- 0
signif_earthquakes <- filter(signif_earthquakes, eq_mag_unk > 0)

head(signif_earthquakes)

#Replace NA with zero value in hour, minute, second columns
signif_earthquakes$hour[is.na(signif_earthquakes$hour)] <- 0
signif_earthquakes$minute[is.na(signif_earthquakes$minute)] <- 0
signif_earthquakes$second[is.na(signif_earthquakes$second)] <- 0

#Create date column by joining Year, Month, Day 
signif_earthquakes <- unite(signif_earthquakes, date, c("year", "month", "day"),
                            sep = "-")

#Create Time Column by joining Hour, Minute, Second
signif_earthquakes <- unite(signif_earthquakes, time, c("hour", "minute",
                                                        "second"), sep = ":")
signif_earthquakes$time <- hms::as.hms(signif_earthquakes$time)

#Rename eq_mag_unk column as magnitude
colnames(signif_earthquakes)[4] <- "magnitude"

head(signif_earthquakes)



# Import and clean USGS data set ------------------------------------------


#Import data from USGS on earthquakes larger than 5.5 
#(as measured on the Richter Scale) from 1965-2016
USGS_earthquakes <- read_csv("database.csv")

#Save USGS data set as a data frame (from csv)
USGS_df <- as.data.frame(USGS_earthquakes)

#Select relevant headings
USGS_df <- select(USGS_df, "Date", "Time", "Latitude", "Longitude", "Depth",
                  "Magnitude")

#Set column headings to lowercase
USGS_df <- setNames(USGS_df, tolower(names(USGS_df)))

#Format date to YYYY-MM-DD
USGS_df$date <- format(as.Date(USGS_df$date, "%m/%d/%Y"), "%Y-%m-%d")

#Rename depth as "focal_depth"
colnames(USGS_df)[5] <- "focal_depth"

head(USGS_df)


# Formation of Earthquakes data set (world) -------------------------------



#Join tables together via date, time, latitude, and longitude
earthquakes <- full_join(signif_earthquakes, USGS_df, by = c("date", "time", 
                                                             "latitude", 
                                                             "longitude", 
                                                             "magnitude", 
                                                             "focal_depth"))

summary(earthquakes)


# geojson file import for mapping plate boundaries ------------------------


# Import data for tectonic plate boundaries
plate_data <- "PB2002_plates.json"
plates <- geojson_read(plate_data, what = "sp")  #SpatialPointsDataFrame

plot(plates)

# convert list of earthquake points into a SpatialPointsDataFrame
coordinates(earthquakes) <- ~ longitude + latitude

# convert earthquakes to use the same coordinate system as plates (for overlay)
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



# Graph Exploration and Analysis of Data ----------------------------------


#Bar graph of year vs # of major earthquakes (1965-2016)
ggplot(earthquakes, aes(factor(year))) + 
  geom_bar(stat = "count", fill = "dark red") + 
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5), 
        text=element_text(size=9)) + 
  ggtitle("Number of Major Earthquakes per Year")


#Boxplot of Plate name vs magnitude
ggplot(earthquakes, aes(x = PlateName, y = magnitude)) + 
  geom_boxplot() + 
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5), 
        text=element_text(size=9)) +
  ggtitle("Outliers in magnitude on tectonic plates")


#Boxplot of Year vs. Magnitude
ggplot(earthquakes, aes(x = factor(year), y = magnitude)) +
  geom_boxplot() +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5), 
        text=element_text(size=9)) +
  ggtitle("Year vs. Magnitude")


#Map Earthquake locations by magnitude (colored per tectonic plate location)
world <- ggplot() +
  borders("world", colour = "gray85", fill = "gray80") +
  theme_map() 

plate_map <- world +
  geom_point(aes(x = longitude, y = latitude, size = magnitude,
                 color = PlateName), data = earthquakes, alpha = .5) +
  geom_point(data = plates, aes(x = long, y = lat), fill = 'black', stroke = 1)+
  scale_size_continuous(range = c(1, 8), breaks = c(6, 7, 8, 9)) +
  labs(size = 'magnitude', color = 'Plate Name') +
  theme(legend.position = "right")

plot(plate_map)

#Map earthquake locations by year and magnitude (color = year, size = magnitude)
year_map <- world +
  geom_point(aes(x = longitude, y = latitude, size = magnitude, color = year), 
             data = earthquakes, alpha = .5) +
  geom_point(data = plates, aes(x = long, y = lat), fill = 'black', stroke = 1)+
  scale_size_continuous(range = c(1, 8), breaks = c(6, 7, 8, 9)) +
  labs(size = 'magnitude', color = 'Year') +
  theme(legend.position = "right")

plot(year_map)

#Earthquakes from 2006-2016
earthquakes2006_2016 <- filter(earthquakes, year > 2005)

decade_map <- world +
  geom_point(aes(x = longitude, y = latitude, size = magnitude, color = year), 
            data = earthquakes2006_2016, alpha = .5) +
  geom_point(data = plates, aes(x = long, y = lat), fill = 'black', stroke = 1)+
  scale_size_continuous(range = c(1, 8), breaks = c(6, 7, 8, 9)) +
  labs(size = 'magnitude', color = 'Year') +
  theme(legend.position = "right")

plot(decade_map)


# Pacific Plate charts ----------------------------------------------------


#Filter for Pacific plate earthquakes
pacific_earthquakes <- filter(earthquakes, PlateName == "Pacific")

#Bar graph of year vs # of major earthquakes (1965-2016)
ggplot(pacific_earthquakes, aes(factor(year))) + 
  geom_bar(stat = "count", fill = "dark red") + 
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5), 
        text=element_text(size=9)) + 
  ggtitle("Number of Major Earthquakes per Year")

#Boxplot of year vs magnitude on Pacific plate
ggplot(pacific_earthquakes, aes(x = year, y = magnitude)) + 
  geom_boxplot() +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5),
        text=element_text(size=9)) +
  ggtitle("Year vs Magnitude of earthquakes on Pacific plate")

#Unite month, day, year columns
pacific_earthquakes <- unite(pacific_earthquakes, quake_date, 
                             c("year", "month", "day"), sep = "-")

#Animate progression of earthquakes for Pacific Plate
ghost_points_ini <- tibble(
  quake_date = as.Date('1965-01-01'),
  magnitude = 0, longitude = 0, latitude = 0)

ghost_points_fin <- tibble(
  quake_date = seq(as.Date('2017-01-01'),
                   as.Date('2017-01-02'),
                   by = 'days'),
  magnitude = 0, longitude = 0, latitude = 0)

animated_map <- world +
  geom_point(data = plates, aes(x = long, y = lat), fill = 'black', stroke = 1)+
  geom_point(aes(x = longitude, y = latitude, size = magnitude, 
                 color = PlateName, frame = as.Date(quake_date), 
                 cumulative = FALSE), data = head(pacific_earthquakes, 
                                                  n = 100L), alpha = .5) +
  geom_point(aes(x = longitude, y = latitude, size = magnitude, 
                 frame = quake_date,
                 cumulative = FALSE),
             data = ghost_points_ini, alpha = 0) +
  geom_point(aes(x = longitude, y = latitude, size = magnitude,  
                 frame = quake_date,
                 cumulative = FALSE),
             data = ghost_points_fin, alpha = 0) +
  scale_size_continuous(range = c(1, 8), breaks = c(6, 7, 8, 9)) +
  labs(size = 'magnitude', color = 'Plate Name') +
  theme(legend.position = "right")

gganimate::gg_animate(animated_map)


# North American Plate Charts ---------------------------------------------


#Filter for North American plate earthquakes
na_earthquakes <- filter(earthquakes, PlateName == "North America")

#Bar graph of year vs # of major earthquakes (1965-2016)
ggplot(na_earthquakes, aes(factor(year))) + 
  geom_bar(stat = "count", fill = "dark red") + 
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5),
        text=element_text(size=9)) + 
  ggtitle("Number of Major Earthquakes per Year on North American plate")

#Boxplot of year vs magnitude on Pacific plate
ggplot(na_earthquakes, aes(x = year, y = magnitude)) + 
  geom_boxplot() +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5), 
        text=element_text(size=9)) +
  ggtitle("Year vs Magnitude of earthquakes on North American plate")

#Unite month, day, year columns
na_earthquakes <- unite(na_earthquakes, quake_date, c("year", "month", "day"), 
                        sep = "-")

#Animate progression of earthquakes for N. American Plate
ghost_points_ini <- tibble(
  quake_date = as.Date('1965-01-01'),
  magnitude = 0, longitude = 0, latitude = 0)

ghost_points_fin <- tibble(
  quake_date = seq(as.Date('2017-01-01'),
                   as.Date('2017-01-02'),
                   by = 'days'),
  magnitude = 0, longitude = 0, latitude = 0)

na_animated_map <- world +
  geom_point(data = plates, aes(x = long, y = lat), fill = 'black', stroke = 1)+
  geom_point(aes(x = longitude, y = latitude, size = magnitude, 
                 color = PlateName, frame = as.Date(quake_date), 
                 cumulative = FALSE), data = head(na_earthquakes, n = 100L), 
             alpha = .5) +
  geom_point(aes(x = longitude, y = latitude, size = magnitude,  
                 frame = quake_date,
                 cumulative = FALSE),
             data = ghost_points_ini, alpha = 0) +
  geom_point(aes(x = longitude, y = latitude, size = magnitude,  
                 frame = quake_date,
                 cumulative = FALSE),
             data = ghost_points_fin, alpha = 0) +
  scale_size_continuous(range = c(1, 8), breaks = c(6, 7, 8, 9)) +
  labs(size = 'magnitude', color = 'Plate Name') +
  theme(legend.position = "right")

gganimate::gg_animate(na_animated_map)


# South American & Nazca Plate charts --------------------------------------


#Filter for South American plate earthquakes
sa_earthquakes <- filter(earthquakes, PlateName == c("South America", "Nazca"))

#Bar graph of year vs # of major earthquakes (1965-2016)
ggplot(sa_earthquakes, aes(factor(year))) + 
  geom_bar(stat = "count", fill = "dark red") + 
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5), 
        text=element_text(size=9)) + 
  ggtitle("Number of Major Earthquakes per Year on South American plate")

#Boxplot of year vs magnitude on South American and Nazca plates
ggplot(sa_earthquakes, aes(x = year, y = magnitude)) + 
  geom_boxplot() +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5), 
        text=element_text(size=9)) +
  ggtitle("Year vs Magnitude of earthquakes on South American and Nazca plates")

#Unite month, day, year columns
sa_earthquakes <- unite(sa_earthquakes, quake_date, c("year", "month", "day"), 
                        sep = "-")

#Animate progression of earthquakes for South American and Nazca Plates
ghost_points_ini <- tibble(
  quake_date = as.Date('1965-01-01'),
  magnitude = 0, longitude = 0, latitude = 0)

ghost_points_fin <- tibble(
  quake_date = seq(as.Date('2017-01-01'),
                   as.Date('2017-01-02'),
                   by = 'days'),
  magnitude = 0, longitude = 0, latitude = 0)

sa_animated_map <- world +
  geom_point(data = plates, aes(x = long, y = lat), fill = 'black', stroke = 1)+
  geom_point(aes(x = longitude, y = latitude, size = magnitude, 
                 color = PlateName, frame = as.Date(quake_date), 
                 cumulative = FALSE), data = head(sa_earthquakes, 
                                                  n = 100L), alpha = .5) +
  geom_point(aes(x = longitude, y = latitude, size = magnitude,  
                 frame = quake_date,
                 cumulative = FALSE),
             data = ghost_points_ini, alpha = 0) +
  geom_point(aes(x = longitude, y = latitude, size = magnitude,  
                 frame = quake_date,
                 cumulative = FALSE),
             data = ghost_points_fin, alpha = 0) +
  scale_size_continuous(range = c(1, 8), breaks = c(6, 7, 8, 9)) +
  labs(size = 'magnitude', color = 'Plate Name') +
  theme(legend.position = "right")

gganimate::gg_animate(sa_animated_map)




# Eurasian Plate charts ---------------------------------------------------


#Filter for Eurasian plate earthquakes
eua_earthquakes <- filter(earthquakes, PlateName == "Eurasia")

#Bar graph of year vs # of major earthquakes (1965-2016)
ggplot(eua_earthquakes, aes(factor(year))) + 
  geom_bar(stat = "count", fill = "dark red") + 
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5), 
        text=element_text(size=9)) + 
  ggtitle("Number of Major Earthquakes per Year on Eurasian plate")

#Boxplot of year vs magnitude on Pacific plate
ggplot(eua_earthquakes, aes(x = year, y = magnitude)) + 
  geom_boxplot() +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5), 
        text=element_text(size=9)) +
  ggtitle("Year vs Magnitude of earthquakes on Eurasian plate")

#Unite month, day, year columns
eua_earthquakes <- unite(eua_earthquakes, quake_date, c("year", "month", "day"),
                         sep = "-")

#Animate progression of earthquakes for Pacific Plate
ghost_points_ini <- tibble(
  quake_date = as.Date('1965-01-01'),
  magnitude = 0, longitude = 0, latitude = 0)

ghost_points_fin <- tibble(
  quake_date = seq(as.Date('2017-01-01'),
                   as.Date('2017-01-02'),
                   by = 'days'),
  magnitude = 0, longitude = 0, latitude = 0)

eua_animated_map <- world +
  geom_point(data = plates, aes(x = long, y = lat), fill = 'black', stroke = 1)+
  geom_point(aes(x = longitude, y = latitude, size = magnitude, 
                 color = PlateName, frame = as.Date(quake_date), 
                 cumulative = FALSE), data = head(eua_earthquakes, n = 100L),
             alpha = .5) +
  geom_point(aes(x = longitude, y = latitude, size = magnitude, 
                 frame = quake_date,
                 cumulative = FALSE),
             data = ghost_points_ini, alpha = 0) +
  geom_point(aes(x = longitude, y = latitude, size = magnitude, 
                 frame = quake_date,
                 cumulative = FALSE),
             data = ghost_points_fin, alpha = 0) +
  scale_size_continuous(range = c(1, 8), breaks = c(6, 7, 8, 9)) +
  labs(size = 'magnitude', color = 'Plate Name') +
  theme(legend.position = "right")

gganimate::gg_animate(eua_animated_map)




# N. America & Pacific plate charts ---------------------------------------


#Filter for North American and Pacific plate earthquakes
nap_earthquakes <- filter(earthquakes, 
                          PlateName == c("North America", "Pacific"))

#Bar graph of year vs # of major earthquakes (1965-2016)
ggplot(nap_earthquakes, aes(factor(year))) + 
  geom_bar(stat = "count", fill = "dark red") + 
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5),
        text=element_text(size=9)) + 
  ggtitle("# of Major Earthquakes per Year on N. American & Pacific plates")

#Boxplot of year vs magnitude on North American and Pacific plates
ggplot(nap_earthquakes, aes(x = year, y = magnitude)) + 
  geom_boxplot() +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5), 
        text=element_text(size=9)) +
  ggtitle("Year vs Magnitude on North American and Pacific plates")

#Unite month, day, year columns
nap_earthquakes <- unite(nap_earthquakes, quake_date, c("year", "month", "day"),
                         sep = "-")

#Animate progression of earthquakes for North American and Pacific Plates
ghost_points_ini <- tibble(
  quake_date = as.Date('1965-01-01'),
  magnitude = 0, longitude = 0, latitude = 0)

ghost_points_fin <- tibble(
  quake_date = seq(as.Date('2017-01-01'),
                   as.Date('2017-01-02'),
                   by = 'days'),
  magnitude = 0, longitude = 0, latitude = 0)

nap_animated_map <- world +
  geom_point(data = plates, aes(x = long, y = lat), fill = 'black', stroke = 1)+
  geom_point(aes(x = longitude, y = latitude, size = magnitude,
                 color = PlateName, frame = as.Date(quake_date), 
                 cumulative = FALSE), data = head(nap_earthquakes, n = 100L), 
             alpha = .5) +
  geom_point(aes(x = longitude, y = latitude, size = magnitude,  
                 frame = quake_date,
                 cumulative = FALSE),
             data = ghost_points_ini, alpha = 0) +
  geom_point(aes(x = longitude, y = latitude, size = magnitude,  
                 frame = quake_date,
                 cumulative = FALSE),
             data = ghost_points_fin, alpha = 0) +
  scale_size_continuous(range = c(1, 8), breaks = c(6, 7, 8, 9)) +
  labs(size = 'magnitude', color = 'Plate Name') +
  theme(legend.position = "right")

gganimate::gg_animate(nap_animated_map)

##Checking for skew of variables


hist(earthquakes$focal_depth)
hist(log1p(earthquakes$focal_depth))


ggplot(earthquakes1, aes(x=magnitude)) + geom_density()

ggplot(earthquakes1, aes(x=magnitude)) + geom_density() + 
  scale_x_continuous(trans="log1p")

ggplot(earthquakes1, aes(x=scale(magnitude, center=TRUE, scale=TRUE))) + 
  geom_density()

mean(earthquakes1$magnitude)
median(earthquakes1$magnitude)


# Interactions between variables ------------------------------------------


pairs(latitude ~ PlateName + log1p(focal_depth), data = earthquakes)


pairs(latitude + PlateName * log1p(focal_depth), data = earthquakes)


pairs(longitude ~ PlateName + log1p(focal_depth), data = earthquakes)


pairs(longitude ~ PlateName * log1p(focal_depth), data = earthquakes)


pairs(magnitude ~ latitude + longitude + PlateName + log1p(focal_depth), 
     data = earthquakes)


pairs(magnitude ~ latitude * longitude + PlateName + log1p(focal_depth), 
     data = earthquakes)


pairs(magnitude ~ latitude * longitude + PlateName * log1p(focal_depth), 
     data = earthquakes)


pairs(magnitude ~ latitude * longitude * PlateName * log1p(focal_depth), 
     data = earthquakes)


#Interactions between variables on North_American and Pacific plates


pairs(latitude ~ PlateName + log1p(focal_depth), data = nap_earthquakes)


pairs(latitude ~ PlateName * log1p(focal_depth), data = nap_earthquakes)


pairs(longitude ~ PlateName + log1p(focal_depth), data = nap_earthquakes)


pairs(longitude ~ PlateName * log1p(focal_depth), data = nap_earthquakes)


pairs(magnitude ~ latitude + longitude + PlateName + log1p(focal_depth), 
     data = nap_earthquakes)


pairs(magnitude ~ latitude * longitude + PlateName + log1p(focal_depth), 
     data = nap_earthquakes)


pairs(magnitude ~ latitude * longitude + PlateName * log1p(focal_depth), 
     data = nap_earthquakes)


pairs(magnitude ~ latitude * longitude * PlateName * log1p(focal_depth), 
     data = nap_earthquakes)



##Creating predictive models


#Unite month, day, year columns
earthquakes1 <- unite(earthquakes, quake_date, c("year", "month", "day"), 
                      sep = "-")



# Linear Regression Models ------------------------------------------------



LRModel1 <-lm(magnitude ~ latitude + longitude, data = earthquakes1)
summary(LRModel1)

LRModel2 <-lm(magnitude ~ latitude * longitude, data = nap_earthquakes)
summary(LRModel2)

LRModel3 <-lm(magnitude ~ latitude * longitude + log1p(focal_depth), 
              data = earthquakes1)
summary(LRModel3)

LRModel4 <-lm(magnitude ~ latitude * longitude + log1p(focal_depth) + PlateName, 
              data = earthquakes1)
summary(LRModel4)

LRModel5 <-lm(magnitude ~ latitude * longitude + log1p(focal_depth) * PlateName, 
              data = earthquakes1)
summary(LRModel5)

LRModel6 <-lm(magnitude ~ latitude * longitude * PlateName, data = earthquakes1)
summary(LRModel6)

LRModel7 <-lm(magnitude ~ latitude * longitude + log1p(focal_depth), 
              data = nap_earthquakes)
summary(LRModel7)

LRModel8 <-lm(magnitude ~ latitude * longitude * log1p(focal_depth) * PlateName, 
              data = earthquakes1)
summary(LRModel8)

LRModel9 <-lm(magnitude ~ latitude * longitude * log1p(focal_depth) * PlateName, 
              data = nap_earthquakes)
summary(LRModel9)



#Create binomial column for major earthquake classification

earthquakes1$MajorEarthquakes = ifelse(earthquakes1$magnitude >= 7.0, 1, 0)
head(earthquakes1)

table(earthquakes1$MajorEarthquakes)

#Null model for earthquakes1

787/18846

#null model is 0.04175952


# Logistic Regression Models ----------------------------------------------


QuakeLog1 <- glm(MajorEarthquakes ~ latitude * longitude * log1p(focal_depth) ,
                data = earthquakes1, family = binomial)
summary(QuakeLog1)


QuakeLog2 <- glm(MajorEarthquakes ~ longitude * log1p(focal_depth), 
                data = earthquakes1, family = binomial)
summary(QuakeLog2)


QuakeLog3 <- glm(MajorEarthquakes ~ log1p(focal_depth), data = earthquakes1, 
                family = binomial)
summary(QuakeLog3)


QuakeLog4 <- glm(MajorEarthquakes ~ latitude * longitude, data = earthquakes1, 
                family = binomial)
summary(QuakeLog4)


#Create binomial column for major earthquake classification for NAP data

nap_earthquakes$MajorEarthquakes = 
  ifelse(nap_earthquakes$magnitude >= 7.0, 1, 0)
head(nap_earthquakes)

table(nap_earthquakes$MajorEarthquakes)

#Null model for nap_earthquakes

84/1479

#null model is 0.05679513

NapQuakeLog1 <- glm(MajorEarthquakes ~ latitude * longitude, 
                   data = nap_earthquakes, family = binomial)
summary(NapQuakeLog1)

NapQuakeLog2 <-glm(MajorEarthquakes ~ latitude * longitude * log1p(focal_depth), 
                   data = nap_earthquakes, family = binomial)
summary(NapQuakeLog2)

NapQuakeLog3 <-glm(MajorEarthquakes ~ latitude + longitude + log1p(focal_depth), 
                   data = nap_earthquakes, family = binomial)
summary(NapQuakeLog3)

NapQuakeLog4 <-glm(MajorEarthquakes ~ longitude * log1p(focal_depth), 
                   data = nap_earthquakes, family = binomial)
summary(NapQuakeLog4)

NapQuakeLog5 <-glm(MajorEarthquakes ~ log1p(focal_depth), 
                   data = nap_earthquakes, family = binomial)
summary(NapQuakeLog5)

anova(NapQuakeLog1, NapQuakeLog2, NapQuakeLog3, NapQuakeLog4, NapQuakeLog5)

#Predicting using the best model(NapQuakeLog4)

predictTrain = predict(NapQuakeLog4, type = "response")
summary(predictTrain)

#Are we predicting higher probabilities for Major Earthquakes (>7.0 magnitude?)

tapply(predictTrain, nap_earthquakes$MajorEarthquakes, mean)


# binomial distributions --------------------------------------------------

#binomial distribution for earthquakes1
eq_agg_df <- earthquakes1 %>% group_by(PlateName) %>%
  summarize(TotalMajor = sum(MajorEarthquakes), TotalEarthquakes = n())

glm(cbind(TotalMajor, TotalEarthquakes - TotalMajor) ~ PlateName,
    family=binomial("logit"), data=eq_agg_df)


#binomial distribution for nap_earthquakes
eq_agg_df_nap <- nap_earthquakes %>% group_by(PlateName) %>% 
  summarize(TotalMajor = sum(MajorEarthquakes), TotalEarthquakes = n())

glm(cbind(TotalMajor, TotalEarthquakes - TotalMajor) ~ PlateName, 
    family = binomial("logit"), data = eq_agg_df_nap)

# Poisson Models ----------------------------------------------------------

#Poisson distribution for earthquakes1

glm(TotalMajor ~ PlateName, family=poisson("log"), data=eq_agg_df)


#Poisson distribution for nap_earthquakes

glm(TotalMajor ~ PlateName, family = poisson("log"), data = eq_agg_df_nap)


