## Installing packages
library(dplyr)
library(moments)
library(DescTools)
library(plotly)
library(ggplot2)
library(maps) 
library(mapview) 
library(sf)


## statistical analysis
df<-read.csv("listings.csv")
print(ncol(df))
print(nrow(df))
print(names(df))
print(summary(df))


print(colSums(is.na(df)))
# bathrooms, calendar_updated, license => all null values => columns are deleted
df <- df[,!names(df) %in% c("bathrooms", "calendar_updated", "license")]

# a lot of the rows have Na values in the review section
print(nrow(filter(df, is.na(df$review_scores_rating) & is.na(df$review_scores_accuracy) & is.na(df$reviews_per_month))))

# => high number of null values( more that 10k ) =>a lot of them are recurring
f<-factor(rowSums(is.na(df)))
print(levels(f))
# there are rows with high number of null values => delete if more than 9
df <- df[!(rowSums(is.na(df)) >= 9), ]



# to find the number of distinct values in each column
print(sapply(df, function(x) n_distinct(x)))

# columns which can be categorized
print(table(df$host_response_time))
print(table(df$neighbourhood_group_cleansed))
print(table(df$room_type))

# information about the location
mean(df$latitude)
mean(df$longitude)
sd(df$latitude)
sd(df$longitude)

# range in which the locations are 
min(df$latitude)
max(df$latitude)
min(df$longitude)
max(df$longitude)

# rating stats
mean(df$review_scores_rating, na.rm = TRUE)
median(df$review_scores_accuracy, na.rm = TRUE)

# convert the percentages to numeric
# reveals that some rows had "N/A" as text in these columns
df$host_acceptance_rate <- as.numeric(sub("%", "", df$host_acceptance_rate))
df$host_response_rate <- as.numeric(sub("%", "", df$host_response_rate))

# convert price to numeric
df$price <- as.numeric(gsub("\\$", "", df$price))

# group by superhost status
df %>% group_by(host_is_superhost) %>% summarise(avg = mean(host_acceptance_rate, na.rm = TRUE))
df %>% group_by(host_is_superhost) %>% summarise(avg = mean(review_scores_rating, na.rm = TRUE))

# replacing NA 
df$bedrooms <- replace(df$bedrooms, is.na(df$bedrooms), 0)
df$review_scores_value <- replace(df$review_scores_value, is.na(df$review_scores_value), mean(df$review_scores_value, na.rm = TRUE))
df$review_scores_location <- replace(df$review_scores_location, is.na(df$review_scores_location), mean(df$review_scores_location, na.rm = TRUE))

df$host_listings_count <- replace(df$host_listings_count, is.na(df$host_listings_count), Mode(df$host_listings_count, na.rm = TRUE)[1])
df$host_total_listings_count <- replace(df$host_total_listings_count, is.na(df$host_total_listings_count), Mode(df$host_total_listings_count, na.rm = TRUE)[1])



##visual analysis

#Scatter plot
plot(y = df$accommodates, x = df$price, xlab = "price", ylab = "accomodates", main = "price VS accomodates")

# to make the visual analysis neat we can use sample for the representation
set.seed(123)
samp <- df[sample(nrow(df), size = round(0.1 * nrow(df))), ]

#boxplot
boxplot(df$availability_30, col = "blue")
boxplot(df$availability_60, col = "blue")

# multiple boxplots
boxplot(availability_365 ~ accommodates, data = df)

# Bar plot
A<-table(df$room_type)
barplot(A, main = "Room Types", xlab = "Room Type", ylab = "Frequency", col = c("skyblue"))

# group bar plot
df$host_response_time <- factor(df$host_response_time, levels = c("within an hour", "within a day", "a few days or more"))
filtered_data <- df[df$host_is_superhost %in% c("t", "f") & !df$host_response_time %in% c(" ", "N/A"), ]
ggplot(filtered_data, aes(x = host_response_time, fill = host_is_superhost)) +
geom_bar(position = "dodge", stat = "count") +
labs(title = "Response Time Comparison for Superhosts vs Others",x = "Response Time", y = "Count") +
scale_fill_manual(values = c("t" = "skyblue", "f" = "coral"))

# stacked bar plot for the same
ggplot(filtered_data, aes(x = host_response_time, fill = host_is_superhost)) +
  geom_bar(position = "stack", stat = "count") +
  labs(title = "Response Time Comparison for Superhosts vs Others",x = "Response Time", y = "Count") +
  scale_fill_manual(values = c("t" = "skyblue", "f" = "coral"))

# pie chart
pie(c(table(df$neighbourhood_group_cleansed)), main ="neighbourhood_group_cleansed")


# histogram
ggplot(df, aes(x=review_scores_value)) + geom_histogram(color="white", fill="deeppink") + ggtitle("Review scores rating")


# map = commented for the compiler
sf_points <- st_as_sf(samp, coords = c("longitude", "latitude"), crs=4269)
h<-hcl.colors(10, palette = "viridis")
mapview(sf_points, zcol = "id", lwd = 3, col.regions = h)



# histogram with colors
qplot(price, data = df, fill = neighbourhood_group_cleansed, binwidth = 20)


## Insights
#1. multiple boxplot = houses which accomodate more people have a higher chance of being available
#these places are being sought out less
boxplot(availability_365 ~ accommodates, data = df)

#2. prices across the map peak at 100-110
# Manhattan and Brooklyn are range to higher prices
qplot(price, data = df, fill = neighbourhood_group_cleansed, binwidth = 20)

#3. for equal number of superhosts and others => superhosts tend to have a lower response time
# this could be the type of hospitality which gets them the status of a super host
t<-subset(df, host_is_superhost=="t")
f<-subset(df, host_is_superhost=="f")
sampt <- t[sample(nrow(t), 50), ]
sampf <- f[sample(nrow(f), 50), ]
filtered_data <- rbind(sampt, sampf)
ggplot(filtered_data, aes(x = host_response_time, fill = host_is_superhost)) +
geom_bar(position = "dodge", stat = "count") +
labs(title = "Response Time Comparison for Superhosts vs Others",x = "Response Time", y = "Count") +
scale_fill_manual(values = c("t" = "skyblue", "f" = "coral"))


##extra findings
#4. superhosts tend to have a higher rating scores
#5. price heavily depends on the accommodations