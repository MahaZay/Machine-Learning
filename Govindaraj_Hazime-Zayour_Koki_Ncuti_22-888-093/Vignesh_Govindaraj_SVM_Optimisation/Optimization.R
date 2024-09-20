library(dplyr)
library(caret)


housing_data <- read.csv("melb_data.csv")


#Optimizing Business Question : Where can I afford to buy a house based on my pocket : south, east, west or north  

budget <- 1000000  
affordable_houses <- housing_data %>%
  filter(Price <= budget & Regionname %in% c("Southern Metropolitan", "Eastern Metropolitan", "Western Metropolitan", "Northern Metropolitan","South-Eastern Metropolitan"))
region_counts <- affordable_houses %>%
  group_by(Regionname) %>%
  summarise(house_count = n())
sorted_regions <- region_counts %>%
  arrange(desc(house_count))
affordable_region <- sorted_regions$Regionname[1]
print(affordable_region)




#Optimizing Business Question : Which type is being mostly sold

type_counts <- housing_data %>%
  group_by(Type) %>%
  summarise(count = n()) %>%
  arrange(desc(count))
print(type_counts)

most_sold_type <- type_counts$Type[1]
print(most_sold_type)
library(ggplot2)
ggplot(type_counts, aes(x = Type, y = count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  xlab("Type") +
  ylab("Count") +
  ggtitle("Distribution of Sold Types")


#Optimizing Business Question : Which suburbs are the best to buy in?

suburb_prices <- housing_data %>%
  group_by(Suburb) %>%
  summarise(avg_price = mean(Price, na.rm = TRUE)) %>%
  arrange(desc(avg_price))
print(suburb_prices)

best_suburbs <- suburb_prices$Suburb[1:5]  # Get the top 5 suburbs
print(best_suburbs)


#Optimizing Business Question : Where's the expensive side of town? 

suburb_prices <- housing_data %>%
  group_by(Suburb) %>%
  summarise(avg_price = mean(Price, na.rm = TRUE)) %>%
  arrange(desc(avg_price))

expensive_suburb <- suburb_prices$Suburb[1]

print(expensive_suburb)


#Optimizing Business Question : Where should I buy a 2 bedroom unit?

filtered_data <- housing_data %>%
  filter(Bedroom2 == 2)
ggplot(filtered_data, aes(x = Suburb)) +
  geom_bar(fill = "steelblue") +
  labs(x = "Suburb", y = "Count", title = "Distribution of 2 Bedroom Units by Suburb")

top_suburbs <- filtered_data %>%
  group_by(Suburb) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  head(10)

print(top_suburbs)





