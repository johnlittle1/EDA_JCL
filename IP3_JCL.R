##load dataset
library(tidyverse)
data('txhousing')
data <- txhousing

##head and tail of data
head(data, 13)
tail(data, 13)

##dimensions of dataset
dim(data)

##variable names
names(data)

##change months from numeric values 
data$month <- month.name[data$month]

##data summary
str(data)
summary(data)

##check for missing values
sapply(data, function(x)sum(is.na(x)))

##view sample of rows with missing data
missing_data <- txhousing[is.na(txhousing$sales),]
print(head(missing_data))

##delete rows that have missing data
CleanData <- na.omit(txhousing)

##check for missing values
sapply(CleanData, function(x)sum(is.na(x)))

##create plots to analyze data
##univariate plot 1
ggplot(data, aes(x = median)) +
  geom_histogram(binwidth = 10000, fill = 'green', color = "black") +
  labs(title = "Median Sale Prices", x = "Median Sale Price", y = "Frequency")

##univariate plot 2
ggplot(data, aes(x = month)) +
  geom_bar(fill = "orange", color = "black") +
  labs(title = "Sales by Month", x = "Month", y = "Count")

##univariate plot 3
ggplot(data, aes(y = sales)) +
  geom_boxplot(fill = "red", color = "black") +
  labs(title = "Box Plot of Sales", y = "Sales")

##univariate plot 4 
ggplot(data, aes(x = inventory)) +
  geom_density(fill = "purple",color = "black", alpha = 0.5) +
  labs(title = "Density of Inventory", x = "Inventory", y = "Density")

##bivariate plot 1
ggplot(data, aes(x = median, y = sales)) +
  geom_point(color = "blue", color = "black", alpha = 0.5) +
  labs(title = "Median Sale Price vs Sales", x = "Median Sale Price", y = "Sales")

##bivariate plot 2
ggplot(data, aes(x = factor(year), y = median)) +
  geom_boxplot(fill = "orange", color = "black") +
  labs(title = "Median Sale Price by Year", x = "Year", y = "Median Sale Price")

##bivariate plot 3
ggplot(data, aes(x = factor(year), y = inventory)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Inventory by Year", x = "Year", y = "Inventory (Months)")

##bivariate plot 4
ggplot(data, aes(x = volume, y = sales)) +
  geom_point(color = "darkred") +
  labs(title = "Volume vs Sales", x = "Volume ($)", y = "Sales (units)")

##bivariate plot 5
ggplot(data, aes(x = city, y = sales)) +
  geom_boxplot(fill = "cyan") +
  labs(title = "Sales by City", x = "City", y = "Sales (units)") +
  theme(axis.text.x = element_text(angle = 90))

##multivariate plot 1 
ggplot(data, aes(x = median, y = sales, color = year)) +
  geom_point(alpha = 0.6) +
  labs(title = "Median Sale Price vs Sales by Year", x = "Median Sale Price", y = "Sales", color = "Year")

##multivariate plot 2 
cor_matrix <- cor(data %>% select_if(is.numeric), use = "complete.obs")
print(cor_matrix)

##multivariate plot 3
ggplot(data=as.data.frame(as.table(cor_matrix)), aes(Var1, Var2, fill=Freq))+
  geom_tile(color='white')+
  scale_fill_gradient2(low='blue', high='red', mid= 'white', midpoint=0, limit= c(-1,1))
