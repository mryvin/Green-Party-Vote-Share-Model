# Section 1: Set up environment and load libraries
# -----------------------------------------------------------------------------------------------------------------------------------------------------------

# Set working directory if needed
# setwd()

# Load required libraries
library(dplyr)
library(tidyverse)
library(caret)
library(leaps)
library(MASS)

# Section 2: Read and prepare data for EU countries
# -----------------------------------------------------------------------------------------------------------------------------------------------------------

# Read the CSV data
data <- read.csv("2019Europe.csv")

# Subset the data to include only the European Union countries
EU <- subset(data, cntry_AN %in% c("AT", "BG", "HR", "CY", "CZ", "DK", "EE", "FI", "FR", "DE",
                                    "GR", "HU", "IT", "LT", "NL", "PL", "PT", "RO", "SK", "SI",
                                    "ES", "SE", "GB"))

# Malta, Belgium, Luxembourg, Ireland, and Latvia are not included

# Create a new dataframe that contains the weighted average for all numerical values for each country
for (t in unique(EU$cntry_AN)) { # For each country
  values <- c()
  names <- c()
  weights <- c()
  
  # Subset data to just the current country
  nation <- subset(EU, cntry_AN == t)
  
  # Create a vector with the weight that each individual should have
  for (i in nation$gwght) {
    weights <- append(weights, i)
  }
  
  j = 1
  # For each value
  for (i in colnames(nation)) {
    # Skip first 40 columns as they do not contain test values
    if (j > 40) {
      # Skip non-numerical columns
      if (class(nation[[i]]) == "integer") {
        # Multiply numerical values by weights
        nation[i] <- nation[i] * weights[j]
        
        # Calculate the mean of positive values (negative ones imply no response/don't know response)
        values <- append(values, mean(nation[[i]][nation[[i]] > 0]))
        names <- append(names, i)
      }
    } else if (i == 'cntry_AN') {
      # Make the first column the country IDs
      values <- append(values, t)
      names <- append(names, 'Country')
    }
    j <- j + 1
  }
  
  # Austria is the first country, make a new dataframe
  if (t == "AT") {
    dataframe <- as.data.frame(t(values))
    colnames(dataframe) <- names
  } else {
    # For all other countries, add to the dataframe
    temp <- as.data.frame(t(values))
    colnames(temp) <- names
    dataframe <- rbind(dataframe, temp)
  }
}

# Section 3: Data processing for EU countries
# -----------------------------------------------------------------------------------------------------------------------------------------------------------

# Create a mapping for Country IDs to Country Names
country_mapping <- c('AT' = 'Austria', 'BG' = 'Bulgaria', 'HR' = 'Croatia',
                     'CY' = 'Cyprus', 'CZ' = 'Czech Republic', 'DK' = 'Denmark',
                     'EE' = 'Estonia', 'FI' = 'Finland', 'FR' = 'France',
                     'DE' = 'Germany', 'GR' = 'Greece', 'HU' = 'Hungary',
                     'IT' = 'Italy', 'LT' = 'Lithuania', 'NL' = 'Netherlands',
                     'PL' = 'Poland', 'PT' = 'Portugal', 'RO' = 'Romania',
                     'SK' = 'Slovakia', 'SI' = 'Slovenia', 'ES' = 'Spain',
                     'SE' = 'Sweden', 'GB' = 'UK')

# Change Country IDs to Country Names using the mapping
dataframe$Country <- country_mapping[dataframe$Country]


# Manually insert number of Green Seats and Total Seats in 2019 European Parliamentary Elections
dataframe$GreenSeats <- c(2, 0, 0, 0, 3, 2, 0, 2, 12, 21, 0, 0, 0, 2, 3, 0, 1, 0, 0, 0, 2, 2, 11)
dataframe$TotalSeats <- c(18, 17, 11, 6, 21, 13, 6, 13, 74, 96, 21, 21, 73, 11, 26, 51, 21, 32, 13, 8, 54, 20, 73)
dataframe$GreenPercent <- dataframe$GreenSeats / dataframe$TotalSeats

# Change all columns other than Country to numeric values
for (i in colnames(dataframe)) {
  if (i != "Country") {
    dataframe[[i]] <- as.numeric(dataframe[[i]])
  }
}

# Section 4: Perform Step-wise Regression for EU countries
# -----------------------------------------------------------------------------------------------------------------------------------------------------------

# Perform Step-wise Regression
redata <- subset(dataframe, select = -c(Country, GreenSeats, TotalSeats))

# Print out Adjusted R-squared and p-value for each column
for (i in colnames(redata)) {
  f <- summary(lm(GreenPercent ~ eval(parse(text = i)), data = redata))$fstatistic
  p <- pf(f[1], f[2], f[3], lower.tail = FALSE)
  attributes(p) <- NULL
  if (p < 0.15) {
    cat("Variable: ", i, "\n")
    cat("Adjusted R-squared: ", summary(lm(GreenPercent ~ eval(parse(text = i)), data = redata))$adj.r.squared, "\n")
    cat("p-value ", p, "\n\n")
  }
}

# Remove columns with high p-values.
# This is necessary as the number of columns cannot be more than the number of countries for step-wise regression
for (i in colnames(redata)) {
  f <- summary(lm(GreenPercent ~ eval(parse(text = i)), data = redata))$fstatistic
  p <- pf(f[1], f[2], f[3], lower.tail = FALSE)
  attributes(p) <- NULL
  if (p > 0.15) {
    redata <- redata[, !(names(redata) %in% i)]
  }
  # if(i=="V097EF") {                             # Possibly remove, as it is a categorical variable which debatably can be considered ranked
  #   redata <- redata[,!(names(redata) %in% i)]
  # }
}

# Section 5: Fit linear models for EU countries
# -----------------------------------------------------------------------------------------------------------------------------------------------------------

# Fit linear models
modstart = lm(dataframe$GreenPercent ~ 1, data = redata)
mod = lm(dataframe$GreenPercent ~ ., data = redata)
summary(modstart)
summary(mod)

# Perform step-wise regression using the step function
step(modstart, direction = 'forward', scope = formula(mod))

# Fit models with and without a questionable categorical variable (V097EF)
newmod1 = lm(formula = dataframe$GreenPercent ~ F118 + E117 + G007_36_B + 
              V097EF + A039 + A032 + F122 + E069_18 + A003 + E069_07 + 
              C002 + G256, data = redata)
summary(newmod1)

newmod2 = lm(formula = dataframe$GreenPercent ~ F118 + E117 + G007_36_B + 
              A039 + E263 + C002_01 + log(A032) + E069_07, data = redata)
summary(newmod2)

# Section 6: Prediction and visualization for EU countries
# -----------------------------------------------------------------------------------------------------------------------------------------------------------

# Prediction using Model 1
prediction1 <- dataframe[c("Country", "F118", "E117", "G007_36_B", "V097EF", "A039", "A032", "F122", "E069_18", "A003", "E069_07", "C002", "G256", "GreenSeats", "TotalSeats", "GreenPercent")]
prediction1$predictedGreenPercent <- predict(newmod1, newdata = prediction1)
prediction1$predictedGreenSeats <- prediction1$predictedGreenPercent * prediction1$TotalSeats
prediction1$predictiondiscrepancy <- prediction1$predictedGreenSeats - prediction1$GreenSeats
mean_discrepancy1 <- mean(abs(prediction1$predictiondiscrepancy))

# Visualization for Model 1
ggplot(prediction1, aes(x = reorder(Country, -abs(predictiondiscrepancy)), y = abs(predictiondiscrepancy))) + 
  geom_bar(stat = "identity", fill = "#FF6666", width = 0.6) +
  theme(aspect.ratio = 3/4) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 15)) +
  xlab("Country") +
  theme(axis.title.x = element_text(size = 15)) +
  ylab("Residual of Actual Green Seat % vs Predicted Green Seat %") +
  theme(axis.title.y = element_text(size = 15)) +
  theme(axis.text.y = element_text(size = 10)) +
  ggtitle("Residual of Actual Green Seat % vs Predicted Green Seat % Per Country - Model 1")

# Prediction using Model 2
prediction2 <- dataframe[c("Country", "F118", "E117", "G007_36_B", "A039", "E263", "C002_01", "A032", "E069_07", "GreenSeats", "TotalSeats", "GreenPercent")]
prediction2$predictedGreenPercent <- predict(newmod2, newdata = prediction2)
prediction2$predictedGreenSeats <- prediction2$predictedGreenPercent * prediction2$TotalSeats
prediction2$predictiondiscrepency <- prediction2$predictedGreenSeats - prediction2$GreenSeats
mean_discrepancy2 <- mean(abs(prediction2$predictiondiscrepency))

# Visualization for Model 2
ggplot(prediction2, aes(x = reorder(Country, -abs(predictiondiscrepency)), y = abs(predictiondiscrepency))) + 
  geom_bar(stat = "identity", fill = "#FF6666", width = 0.6) +
  theme(aspect.ratio = 3/4) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 15)) +
  xlab("Country") +
  theme(axis.title.x = element_text(size = 15)) +
  ylab("Residual of Actual Green Seat % vs Predicted Green Seat %") +
  theme(axis.title.y = element_text(size = 15)) +
  theme(axis.text.y = element_text(size = 10)) +
  ggtitle("Residual of Actual Green Seat % vs Predicted Green Seat % Per Country - Model 2")

# Another Visualization for Model 2
ggplot(prediction2, aes(x = reorder(Country, predictiondiscrepency), y = predictiondiscrepency)) + 
  geom_bar(stat = "identity", fill = "#FF6666", width = 0.6) +
  theme(aspect.ratio = 3/4) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 15)) +
  xlab("Country") +
  theme(axis.title.x = element_text(size = 15)) +
  ylab("Residual of Actual Green Seat % vs Predicted Green Seat %") +
  theme(axis.title.y = element_text(size = 15)) +
  theme(axis.text.y = element_text(size = 10)) +
  ggtitle("Residual of Actual Green Seat % vs Predicted Green Seat % Per Country - Model 2")


# Section 7: Mapping of EU countries
# -----------------------------------------------------------------------------------------------------------------------------------------------------------


# Mapping of Actual Results of the 2019 EU Parliament Elections
eucountries <- map_data("world", region = c("Austria", "Italy", "France", "Spain", "Portugal", "Ireland", "Luxembourg", "Belgium",
                                           "Netherlands", "Germany", "Denmark", "Czech Republic", "Poland", 
                                           "Slovakia", "Hungary", "Slovenia", "Croatia", "Sweden", "Finland", "Latvia",
                                           "Estonia", "Lithuania", "Greece", "Malta", "Bulgaria", "Romania",
                                           "UK", "Cyprus"))

greenvote <- data.frame(region = unique(prediction1$Country),
                        greenseats = prediction1$GreenPercent)

eucountries1 <- left_join(eucountries, greenvote, by = "region")

ggplot() +
  geom_polygon(data = eucountries1, aes(x = long, y = lat, group = group, fill = greenseats),
               colour = "white") +
  scale_fill_gradient(name = "Green Seat Share %", low = "grey93", high = "green3") +
  theme_void() +
  ggtitle("Actual 2019 EU Parliament Election Green Results by Country") +
  theme(plot.title = element_text(size = 22), legend.key.size = unit(2, 'cm'), 
        legend.text = element_text(size = 15), legend.title = element_text(size = 15)) +
  coord_quickmap()

# Map of Predicted Results with the Questionable Categorical Variable
greenvotepredict1 <- data.frame(region = unique(prediction1$Country),
                                greenseats = prediction1$predictedGreenPercent)
greenvotepredict1$greenseats <- ifelse(greenvotepredict1$greenseats < 0, 0, greenvotepredict1$greenseats)

eucountries2 <- left_join(eucountries, greenvotepredict1, by = "region")

ggplot() +
  geom_polygon(data = eucountries2, aes(x = long, y = lat, group = group, fill = greenseats),
               colour = "white") +
  scale_fill_gradient(name = "Green Seat Share %", low = "grey93", high = "green3") +
  theme_void() +
  ggtitle("Predicted 2019 EU Parliament Election Green Results (Model 1) by Country") +
  theme(plot.title = element_text(size = 22), legend.key.size = unit(2, 'cm'), 
        legend.text = element_text(size = 15), legend.title = element_text(size = 15)) +
  coord_quickmap()

# Map of Predicted Results without the Questionable Categorical Variable
greenvotepredict2 <- data.frame(region = unique(prediction2$Country),
                                greenseats = prediction2$predictedGreenPercent)
greenvotepredict2$greenseats <- ifelse(greenvotepredict2$greenseats < 0, 0, greenvotepredict2$greenseats)

eucountries3 <- left_join(eucountries, greenvotepredict2, by = "region")

ggplot() +
  geom_polygon(data = eucountries3, aes(x = long, y = lat, group = group, fill = greenseats),
               colour = "white") +
  scale_fill_gradient(name = "Green Seat Share %", low = "grey93", high = "green3", limits = c(0, 0.2)) +
  theme_void() +
  ggtitle("Predicted 2019 EU Parliament Election Green Results (Model 2) by Country") +
  theme(plot.title = element_text(size = 22), legend.key.size = unit(2, 'cm'), 
        legend.text = element_text(size = 15), legend.title = element_text(size = 15)) +
  coord_quickmap()

# Section 8: Apply the same process to countries outside of the EU
# -----------------------------------------------------------------------------------------------------------------------------------------------------------

# Initialize an empty dataframe for countries outside of the EU
dataframeWorld <- NULL

# Iterate over unique country IDs outside of the EU
for (country_id in unique(data$cntry_AN)) {
  
  # Initialize vectors to store values, names, and weights
  values <- c()
  names <- c()
  weights <- c()
  
  # Subset data for the current country
  nation <- subset(data, cntry_AN == country_id)
  
  # Extract weights for individuals in the country
  weights <- nation$gwght
  
  # Counter for weights
  j <- 1
  
  # Iterate over columns of the nation dataframe
  for (col_name in colnames(nation)) {
    
    # Skip first 40 columns as they do not contain test values
    if (j > 40) {
      
      # Check if the column is of integer type
      if (class(nation[[col_name]]) == "integer") {
        
        # Multiply values by weights and calculate the mean of positive values
        nation[col_name] <- nation[col_name] * weights[j]
        values <- append(values, mean(nation[[col_name]][nation[[col_name]] > 0]))
        names <- append(names, col_name)
      } 
    } else if (col_name == 'cntry_AN') {
      
      # Make the first column the country IDs
      values <- append(values, country_id)
      names <- append(names, 'Country')
    }
    
    # Increment the weight counter
    j <- j + 1
  }
  
  # Create or append to the dataframe for countries outside of the EU
  if (country_id == "AL") {
    dataframeWorld <- as.data.frame(t(values))
    colnames(dataframeWorld) <- names
  } else {
    temp <- as.data.frame(t(values))
    colnames(temp) <- names
    dataframeWorld <- rbind(dataframeWorld, temp)
  }
}

# Convert non-Country columns to numeric
for (col_name in colnames(dataframeWorld)) {
  if (col_name != "Country") {
    dataframeWorld[[col_name]] <- as.numeric(dataframeWorld[[col_name]])
  } 
}

# Create a named vector for country name replacements
country_name_replacements <- c(
  'AL' = 'Albania', 'AD' = 'Andorra', 'AZ' = 'Azerbaijan', 
  'AR' = NULL, 'AU' = 'Australia', 'AT' = 'Austria', 
  'BD' = 'Bangladesh', 'AM' = 'Armenia', 'BO' = 'Bolivia', 
  'BA' = 'Bosnia and Herzegovina', 'BR' = NULL, 'BG' = 'Bulgaria', 
  'MM' = 'Myanmar', 'BY' = 'Belarus', 'CA' = 'Canada', 
  'CL' = 'Chile', 'CN' = 'China', 'TW' = 'Taiwan', 
  'CO' = 'Colombia', 'HR' = 'Croatia', 'CY' = 'Cyprus', 
  'CZ' = 'Czech Republic', 'DK' = 'Denmark', 'EC' = 'Ecuador', 
  'ET' = 'Ethopia', 'EE' = 'Estonia', 'FI' = 'Finland', 
  'FR' = 'France', 'GE' = 'Georgia', 'DE' = 'Germany', 
  'GR' = 'Greece', 'GT' = 'Guatemala', 'HK' = 'Hong Kong', 
  'HU' = 'Hungary', 'IS' = 'Iceland', 'ID' = 'Indonesia', 
  'IR' = 'Iran', 'IQ' = 'Iraq', 'IT' = 'Italy', 
  'JP' = 'Japan', 'JO' = 'Jordan', 'KR' = 'South Korea', 
  'KG' = 'Kyrgzstan', 'LB' = 'Lebanon', 'KZ' = NULL, 
  'LT' = 'Lithuania', 'MO' = 'Macau', 'MY' = 'Malaysia', 
  'MX' = 'Mexico', 'ME' = 'Montenegro', 'NL' = 'Netherlands', 
  'NZ' = 'New Zealand', 'NI' = 'Nicaragua', 'NO' = 'Norway', 
  'PK' = 'Pakistan', 'PE' = 'Peru', 'PH' = 'Philippines', 
  'NG' = NULL, 'PL' = 'Poland', 'PT' = 'Portugal', 
  'PR' = 'Puerto Rico', 'RO' = 'Romania', 'RU' = 'Russia', 
  'RS' = 'Serbia', 'SG' = NULL, 'SK' = 'Slovakia', 
  'VN' = 'Vietnam', 'SI' = 'Slovenia', 'ZW' = 'Zimbabwe', 
  'ES' = 'Spain', 'SE' = 'Sweden', 'CH' = 'Switzerland', 
  'TN' = 'Tunisia', 'TR' = 'Turkey', 'UA' = 'Ukraine', 
  'MK' = 'North Macedonia', 'TH' = NULL, 'TJ' = NULL, 
  'EG' = NULL, 'GB' = 'UK', 'US' = NULL
)

# Replace country names in dataframeWorld
dataframeWorld$Country <- country_name_replacements[dataframeWorld$Country]

# Remove rows with NULL values (countries with NULLs are removed from the dataset)
dataframeWorld <- dataframeWorld[!is.null(dataframeWorld$Country), ]
# Note: Rows with NULL values correspond to countries that were removed due to having null values for some of the variables needed in the prediction.

# Load world map data
worldcountries <- map_data("world")

# Prepare data for prediction
prediction3 <- dataframeWorld[c("Country", "F118", "E117", "G007_36_B", "A039", "E263", "C002_01", "A032", "E069_07")]

# Predict Green Percent using the second model
prediction3$predictedGreenPercent <- predict(newmod2, newdata = prediction3)
prediction3$predictedGreenPercent <- ifelse(prediction3$predictedGreenPercent < 0, 0, prediction3$predictedGreenPercent)

# Prepare data for world map visualization
greenvotepredict3 <- data.frame(region = unique(prediction3$Country), greenseats = prediction3$predictedGreenPercent)

# Merge map data with predicted Green Percent data
worldcountries <- left_join(worldcountries, greenvotepredict3, by = "region")

# Plot the predicted 2019 EU Parliament Election Green Results globally
ggplot() +
  geom_polygon(data = worldcountries, aes(x = long, y = lat, group = group, fill = greenseats),
               colour = "white") +
  scale_fill_gradient(name = "Green Seat Share %", low = "grey93", high = "green3", limits = c(0, 0.2)) +
  theme_void() +
  ggtitle("Predicted 2019 EU Parliament Election Green Results Globally") +
  theme(plot.title = element_text(size = 22), legend.key.size = unit(2, 'cm'),
        legend.text = element_text(size = 15), legend.title = element_text(size = 15)) +
  coord_quickmap()
