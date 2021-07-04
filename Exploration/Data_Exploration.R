# #############################################################
# Student Name: Abhi Gambhir
# Student ID: 31072100

# Note plot codes have been commented out because we have used
# Tableau for few plots and some from ggplot2 for our report

# The commented out portion will be helpful to us for next part
# of the project.
# #############################################################






# #############################################################
# Packages
# #############################################################
# install.packages(c("GGally","padr","lubridate","reshape2")

library(dplyr)
library(ggplot2)
library(GGally)
library(lubridate)
library(padr)
library(reshape2)
library(corrplot)



# #############################################################
# Policing Data
# #############################################################
policing_activity <- read.csv("police_project.csv")
head(policing_activity)

nrow(policing_activity)
ncol(policing_activity)
#str(policing_activity)

sapply(policing_activity, function(x) sum(is.na(x)))

null_val <- data.frame(col_name=names(policing_activity) ,no_na=sapply(policing_activity, function(x) sum(is.na(x))) )

# ggplot(data=null_val, aes(x=col_name, y=no_na)) +
#   geom_bar(stat="identity", fill="steelblue", width=0.6)+
#   geom_text(aes(label=no_na), vjust=-0.3, size=3.5) +
#   xlab("Columns with NA values") +
#   ylab("No. of NA values") +
#   theme_minimal() +
#   scale_x_discrete(limits=c("county_name", "driver_age","driver_age_raw"))


policing_activity$county_name <- NULL
sapply(policing_activity, function(x) sum(is.na(x)))

policing_activity <- policing_activity[complete.cases(policing_activity), ]
sapply(policing_activity, function(x) sum(is.na(x)))

# unique(policing_activity[c("driver_gender")])


# Data Type Correction
sapply(policing_activity, class)

policing_activity['driver_gender'] = as.factor(policing_activity$driver_gender)
policing_activity['driver_race'] = as.factor(policing_activity$driver_race)
policing_activity['violation'] = as.factor(policing_activity$violation)
policing_activity['stop_outcome'] = as.factor(policing_activity$stop_outcome)
policing_activity['stop_duration'] = as.factor(policing_activity$stop_duration)

policing_activity['search_conducted'] = as.logical(policing_activity$search_conducted)
policing_activity['is_arrested'] = as.logical(policing_activity$is_arrested)
policing_activity['drugs_related_stop'] = as.logical(policing_activity$drugs_related_stop)

# Date Time column creation
policing_activity$stop_date_time <- paste(policing_activity$stop_date,policing_activity$stop_time)
policing_activity$stop_time <- NULL

sapply(policing_activity, function(x) sum(is.na(x)))
policing_activity <- policing_activity[complete.cases(policing_activity$is_arrested), ]
sapply(policing_activity, function(x) sum(is.na(x)))

nrow(policing_activity)
# policing_activity['new'] <- year(policing_activity$stop_date_time) - policing_activity$driver_age_raw

policing_activity$stop_date_time <- as.POSIXct(policing_activity$stop_date_time, tz = "EST")
policing_activity$stop_date <- as.POSIXct(policing_activity$stop_date)

# corrs <- policing_activity %>%
#   select(which(!sapply(., is.character),))
# 
# corrs <- corrs[, names(corrs) != "stop_date_time"]
# corrs <- corrs[, names(corrs) != "stop_date"]
# 
# corrs_data <- data.matrix(corrs)
# ggcorr(corrs_data, label=TRUE)


# #############################################################
# Exploring Relation between Policing and Gender
# #############################################################

print("Overall")
round(prop.table(table(policing_activity$violation)),6)

print("Female")
female_data <- filter(policing_activity, driver_gender=='F')
round(prop.table(table(female_data$violation)),6)

print("Male")
male_data <- filter(policing_activity, driver_gender=='M')
round(prop.table(table(male_data$violation)),6)

# Analyzing relation between speeding violation outcomes with gender

print("Female")
female_speeding <- filter(female_data, violation=='Speeding')
female_speeding %>%
  group_by(stop_outcome) %>%
  summarise(n=n(), prop=(n/nrow(.)*100))


print("Male")
male_speeding <- filter(male_data, violation=='Speeding')
male_speeding %>%
  group_by(stop_outcome) %>%
  summarise(n=n(), prop=(n/nrow(.)*100))


# search rates by gender
unique(policing_activity[c("search_conducted")])
policing_activity %>%
  filter(driver_gender %in% c('F','M')) %>%
  group_by(driver_gender) %>%
  summarise(search_perc = mean(search_conducted))

policing_activity %>%
  filter(driver_gender %in% c('F','M')) %>%
  group_by(violation, driver_gender) %>%
  summarise(search_perc = mean(search_conducted))


# Does gender affect who is frisked during a search?
table(policing_activity$search_type)
frisked_df <- policing_activity %>% 
              mutate(frisked_or_not = if_else(grepl("Frisk", policing_activity$search_type, ignore.case = TRUE), TRUE, FALSE))

sum(frisked_df$frisked_or_not)

frisked_df %>%
  filter(search_conducted == TRUE) %>%
  filter(driver_gender %in% c('F','M')) %>%
  group_by(driver_gender) %>%
  summarise(frisked_perc = mean(frisked_or_not))




# #############################################################
# Relation between stoppage and Race
# #############################################################

policing_activity %>%
  count(driver_race)

policing_activity %>%
  count(driver_race) %>%
  mutate(prop_race = round(n/sum(n),4))


policing_activity %>%
  count(year = year(stop_date_time), driver_race)

# policing_activity %>%
#   count(year = year(stop_date_time), driver_race) %>%
#   ggplot(aes(year, n, color=driver_race)) +
#   geom_point() +
#   geom_line()


RI_2010 <- data.frame(
  race = c("White", "Black", "Asian", "Hispanic", "Others"),
  approx_number = c(804161, 59996, 30524, 130518, 27367)
)


RI_2010 %>%
  mutate(prop = approx_number/sum(approx_number))


policing_activity %>%
  filter(year(stop_date) == 2010) %>%
  count(driver_race) %>%
  left_join(
    RI_2010,
    by = c("driver_race" = "race")
  ) %>%
  mutate(stop_prop = round(n/approx_number,4))


frisked_df %>%
  filter(year(stop_date) == 2010) %>%
  group_by(driver_race) %>%
  summarise(
    search_prop = mean(search_conducted), 
    frisked_prop = mean(frisked_or_not),
    arrest_prop = mean(is_arrested)
  )




# #############################################################
# Visual exploratory data analysis
# #############################################################

# Calculating the hourly arrest rate
mean(policing_activity$is_arrested)

# Save the hourly arrest rate
hourly_arrest_rate <- policing_activity %>%
  group_by(hour = hour(stop_date_time)) %>%
  summarise(arrest_rate = mean(search_conducted))
hourly_arrest_rate

# ggplot(hourly_arrest_rate, aes(x=hour, y=arrest_rate*100)) +
#   geom_line(color="#E15759") +
#   xlab("Hour of Day") +
#   ylab("Arrest Rate (in %)") +
#   theme_minimal() +
#   geom_point(color="#E15759")


# Plotting drug-related stops
# policing_activity %>% 
#   thicken('year') %>%
#   group_by(stop_date_time_year) %>%
#   summarise(year_avg = mean(drugs_related_stop)) %>%
#   pad() %>%
#   fill_by_value() %>% 
#   ggplot(aes(stop_date_time_year, year_avg)) + geom_line()


# Time you will be stopped for a violation
policing_activity$stop_duration <- recode( policing_activity$stop_duration , "0-15 Min" = 8, "16-30 Min" = 23, "30+ Min" = 45, "1"=1, "2"=2) 

policing_activity %>%
  group_by(violation_raw) %>%
  summarise(avg_stoppage = mean(stop_duration)) %>%
  arrange(desc(avg_stoppage))





# #############################################################
# Weather Dataset
# #############################################################

weather_df <- read.csv("weather.csv")
nrow(weather_df)
ncol(weather_df)

weather_df$DATE <- as.POSIXct(weather_df$DATE)

sapply(weather_df, function(x) sum(is.na(x)))

# weatherdf_null_val <- data.frame(col_name=names(weather_df) ,no_na=sapply(weather_df, function(x) sum(is.na(x))) )

# ggplot(data=weatherdf_null_val, aes(x=col_name, y=no_na)) +
#   geom_bar(stat="identity", fill="steelblue", width=0.6)+
#   geom_text(aes(label=no_na), vjust=-0.3, size=3.5) +
#   xlab("Columns with NA values") +
#   ylab("No. of NA values") +
#   theme_minimal()
#   # scale_x_discrete(limits=c("county_name", "driver_age","driver_age_raw"))

#  Wind Speed
summary(weather_df[c("AWND","WSF2")])

# ggplot(melt(weather_df[c("AWND","WSF2")]), 
#        aes(x=variable, y=value, fill=variable)) +
#   geom_boxplot() +
#   xlab("") +
#   ylab("Speed") +
#   theme_minimal()
  
weather_df['WIND_DIFF'] <- weather_df$WSF2 - weather_df$AWND  

# ggplot(weather_df, aes(WIND_DIFF)) +
#   geom_histogram(bins = 20, fill="#4379AB") +
#   geom_vline(aes(xintercept=mean(WIND_DIFF)),
#              linetype="dashed", size=0.5) +
#   xlab("Wind Difference") +
#   ylab("Count") +
#   theme_minimal()


# Temperature
sapply(weather_df, function(x) sum(is.na(x)))
weather_df$TAVG <- ifelse(is.na(weather_df$TAVG), ceiling((weather_df$TMIN + weather_df$TMAX)/2), weather_df$TAVG)
sapply(weather_df, function(x) sum(is.na(x)))

summary(weather_df[c("TMIN","TAVG","TMAX")])

# ggplot(melt(weather_df[c("TMIN","TAVG","TMAX")]), aes(x=variable, y=value, fill=variable)) +
#   geom_boxplot() +
#   xlab("") +
#   ylab("Temperature") +
#   theme_minimal()

weather_df['T_DIFF'] <- weather_df$TMAX - weather_df$TMIN
summary(weather_df$T_DIFF)

# ggplot(weather_df, aes(T_DIFF)) +
#   geom_histogram(bins = 20, fill="#4379AB") +
#   geom_vline(aes(xintercept=mean(T_DIFF)),
#               linetype="dashed", size=0.5) + 
#   xlab("Temperature Difference") +
#   ylab("Count") +
#   theme_minimal()

weather_bool <- weather_df[, 8:27]
head(weather_bool)

weather_bool[is.na(weather_bool)] <- 0
weather_df['bad_weather'] <- rowSums(weather_bool)

write.csv(weather_df, "weatherdf_rate.csv",row.names = FALSE)

weather_df %>%
  count(bad_weather)

# ggplot(weather_df, aes(x=bad_weather)) +
#   geom_histogram(bins = 20) +
#   theme_light()


# creating a rating system for the weather

rating_map <- c(`0`="good",`1`="bad", `2`="bad", `3`="bad",`4`="bad",`5`="worse",`6`="worse",`7`="worse",`8`="worse",`9`="worse")
weather_df$weather_rating <- recode(weather_df$bad_weather, !!!rating_map)
weather_df['weather_rating'] = as.factor(weather_df$weather_rating)
weather_df$weather_rating <- factor(weather_df$weather_rating, levels = c("good","bad","worse"), ordered = TRUE)

policing_weather_df <- merge(
  x=policing_activity, 
  y=weather_df[c("DATE", "weather_rating")], 
  by.x = "stop_date", 
  by.y = "DATE", 
  all.x = TRUE)

nrow(policing_activity)
nrow(policing_weather_df)

ncol(policing_activity)
ncol(policing_weather_df)

sapply(policing_weather_df, function(x) sum(is.na(x)))



# #############################################################
# Relationship between weather conditions and police activity
# #############################################################

# Comparing arrest rates by weather rating
mean(policing_weather_df$is_arrested)

policing_weather_df %>%
  group_by(weather_rating) %>%
  summarise(arrest_perc = mean(is_arrested))

policing_weather_df %>%
  group_by(violation , weather_rating) %>%
  summarise(arrest_perc = mean(is_arrested))

