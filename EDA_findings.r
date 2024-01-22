library(dplyr)
library(ggplot2)
file_path <- "C:\\Users\\chinm\\Downloads\\lisƟngs.csv" # Replace with the actual path to your CSV
file
data <- read.csv(file_path)
head(data)
tail(data)
print(filter(data, is.na(data$price)))
# summary staƟsƟcs
summary(data)
# checking for missing values
sum(is.na(data))
# checking for missing values in raƟngs and price
sum(is.na(data$review_scores_raƟng))
sum(is.na(data$price))
head(data$price)
# removing the "$" sign and converƟng to numeric
data$price <- as.numeric(gsub("\\$", "", data$price))
mean(data$price)
# removing rows with missing values in raƟngs and price
data <- subset(data, !(is.na(data$review_scores_raƟng) & is.na(data$price)))
# outlier detecƟon for price which are less than Q1 – 1.5 IQR and above Q3 + 1.5 IQR
s_price <- summary(data$price)
data <- filter(data, data$price >= (s_price[2] - 1.5 * IQR(data$price, na.rm = TRUE)) &
 data$price <= (s_price[5] + 1.5 * IQR(data$price, na.rm = TRUE)))
# outlier detecƟon for review_scores_raƟng which are below 0 and above 5
data <- filter(data, data$review_scores_raƟng >= 0 & data$review_scores_raƟng <= 5)
# visualizaƟon of review scores
ggplot(data, aes(x = review_scores_raƟng)) +
 geom_histogram(bins = 20, fill = "pink", color = "black") +
 labs(Ɵtle = "DistribuƟon of Review Scores", x = "Review Scores", y = "Frequency")
# calculaƟng avg price by neighbourhood group
avg_price_ng <- data %>%
 group_by(neighbourhood_group_cleansed) %>%
 summarise(avg_price = mean(price, na.rm = TRUE))%>%
arrange(desc(avg_price))
avg_price_ng
# we see that Bronx has the lowest average price and ManhaƩan has the highest average price
# finding the most common property types
most_common_property_type <- data %>%
 group_by(property_type) %>%
 summarise(lisƟng_count = n()) %>%
 arrange(desc(lisƟng_count)) %>%
 head(5)
print(most_common_property_type)
# we see that the most common property type is an enƟre rental unit
# finding the neighbourhoods with most number of lisƟngs
pop_neighborhoods <- data %>%
 group_by(neighbourhood_cleansed) %>%
 summarise(lisƟng_count = n()) %>%
 arrange(desc(lisƟng_count)) %>%
 head(5)
print(pop_neighborhoods)
# we see that Bedforf-Stuyvesant has the most number of lisƟngs
