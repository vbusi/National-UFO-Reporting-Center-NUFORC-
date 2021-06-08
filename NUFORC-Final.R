library(ggplot2);
library(dplyr);
library(lubridate);
library(mclust);
library(maps);
library(mapdata);

## Cleaning the data ##

ufo <- read.csv(file="C:/Users/vinee/OneDrive/Desktop/AIT Project/ufo.csv", header=T);

ufo_clear <- dataset %>%
  dplyr::select(datetime,
                date.posted,
                city, 
                state, 
                country, 
                latitude, 
                longitude, 
                shape, 
                duration = duration..seconds.);

ufo_clear$datetime    <- mdy_hm(ufo_clear$datetime);
ufo_clear$date.posted <- mdy(ufo_clear$date.posted);
ufo_clear$latitude    <- as.numeric(as.character(ufo_clear$latitude));
ufo_clear$longitude   <- as.numeric(as.character(ufo_clear$longitude));
ufo_clear$duration    <- as.numeric(as.character(ufo_clear$duration));
ufo_clear$country <- as.factor(dataset$country);

ufo_clear <- na.omit(ufo_clear);
ufo_usa <- filter(ufo_clear, country=="us" & !(state %in% c("ak", "hi", "pr")));
head(ufo);

head(ufo_clear)

## Hypothesis ##
## UFO Sightings by country ##
levels(ufo_clear$country) <- c("Remaining countries", "Canada", "Germany", "Australia", "UK", "US");
ggplot(ufo_clear, aes(x=reorder(country, country, FUN=length), fill=country)) +
  stat_count() + 
  theme_bw() + xlab("Country Name") + ylab("COunt of UFO Sightings") + ggtitle("UFO sightings per country");


##Correlation ANalysis ##
##time (of the day/season) and the shape ##
## Histogram ##

ggplot(ufo_clear, aes(x=hour(datetime))) + 
  geom_histogram(bins=15) +
   xlab("24 hrs time") + ylab("Appearance count") + ggtitle("UFO sightings day time");


# UFO Sightings every year
# Appearances per year
appearance_year <- 
  ufo_clear %>% group_by(year=year(datetime)) %>% 
  summarize(count=n());

# Reports per year
reports_year <- 
  ufo_clear %>% group_by(year=year(date.posted)) %>% 
  summarize(count=n());
ggplot(appearance_year, aes(x=year, y=count)) + 
  geom_line(size=1, colour="orange") + 
  geom_line(data=reports_year, aes(y=count), size=1, colour="blue") + 
  geom_smooth(method="lm") +
  theme_bw() + xlab("Year(s)") + ylab("Count (Orange=appearances, Blue=reports)") + 
  ggtitle("Comparison of UFO appearances and UFO reports each year")

##Box Plot ##
# Remove outliers (based on duration > 1 month)
reportedtime_cleared <-
  report_time %>%
  filter(duration < 30);

ggplot(reportedtime_cleared, aes(x=country, y=duration)) + 
  geom_boxplot() + 
  theme_bw() + xlab("Name of the Country") + ylab("Duration time of UFO Sighting") + 
  ggtitle("Duration of UFO sightings of each country");

# Remove outliers (which will sight more than few hours)
# day_to_seconds = 24 * 60 * 60 = 86400
durations_state <- 
  ufo_usa %>% 
  filter(duration < 86400) %>% 
  group_by(state) %>% 
  summarize(mean=mean(duration));
ggplot(durations_state, aes(x=state, y=mean)) + 
  geom_point() + 
  theme_bw() + theme(axis.text.x = element_text(angle=50, size=8, hjust=1)) +
  xlab("US States") + ylab("mean of the UFO sightings") + ggtitle("Duration of UFO sightings of US states");