setwd()

library(dplyr)
library(tidyverse)
library(caret)
library(leaps)
library(MASS)

data <- read.csv("2019Europe.csv")

## SUBSET THE DATA TO JUST INCLUDE THE EUROPEAN UNION

EU <- subset(data, cntry_AN == "AT" | cntry_AN == "BG" | 
               cntry_AN == "HR" | cntry_AN == "CY" | 
               cntry_AN == "CZ" | cntry_AN == "DK" |
               cntry_AN == "EE" | cntry_AN == "FI" |
               cntry_AN == "FR" | cntry_AN == "DE" |
               cntry_AN == "GR" | cntry_AN == "HU" |
               cntry_AN == "IT" | cntry_AN == "LT" |
               cntry_AN == "NL" | cntry_AN == "PL" |
               cntry_AN == "PT" | cntry_AN == "RO" |
               cntry_AN == "SK" | cntry_AN == "SI" |
               cntry_AN == "ES" | cntry_AN == "SE" |
               cntry_AN == "GB")

  #Malta, Belgium, Luxembourg, Ireland, and Latvia not Included

## CREATE A NEW DATAFRAME THAT CONTAINS THE WEIGHTED AVERAGE FOR ALL NUMERICAL VALUES FOR EACH COUNTRY

for (t in unique(EU$cntry_AN)) { #For each country
  values <- c()
  names <- c()
  weights <- c()
  nation <- subset(EU, cntry_AN == t) #Subset data to just the current country
  for (i in nation$gwght) { #Create a vector with the weight that each individual should have
    weights <- append(weights, i)
  }
  j = 1
  for (i in colnames(nation)) { #For each value 
    if (j > 40) { #Skip first 40 columns as they do not test values
      if (class(nation[[i]]) == "integer") { #Skip non-numerical columns
        nation[i] <- nation[i] * weights[j]
        values <- append(values, mean(nation[[i]][nation[[i]]>0])) #Only mean of positive values because negative ones meant no response/don't know response
        names <- append(names, i)
      } 
    } else if (i == 'cntry_AN') { #Make first column the country IDs
      values <- append(values, t)
      names <- append(names, 'Country')
    }
    j <- j + 1
  }
  if (t == "AT") { #Austria is the first country, make a new dataframe
    dataframe <- as.data.frame(t(values))
    colnames(dataframe) <- names
  }
  else { #For all other countries, add to the dataframe
    temp <- as.data.frame(t(values))
    colnames(temp) <- names
    dataframe <- rbind(dataframe,temp)
  }
}

## cHANGE COUNTRY IDs TO COUNTRY NAMES TO BE USED FOR EASE OF USE AND FOR MAPPING

dataframe$Country[dataframe$Country=='AT'] <- 'Austria'
dataframe$Country[dataframe$Country=='BG'] <- 'Bulgaria'
dataframe$Country[dataframe$Country=='HR'] <- 'Croatia'
dataframe$Country[dataframe$Country=='CY'] <- 'Cyprus'
dataframe$Country[dataframe$Country=='CZ'] <- 'Czech Republic'
dataframe$Country[dataframe$Country=='DK'] <- 'Denmark'
dataframe$Country[dataframe$Country=='EE'] <- 'Estonia'
dataframe$Country[dataframe$Country=='FI'] <- 'Finland'
dataframe$Country[dataframe$Country=='FR'] <- 'France'
dataframe$Country[dataframe$Country=='DE'] <- 'Germany'
dataframe$Country[dataframe$Country=='GR'] <- 'Greece'
dataframe$Country[dataframe$Country=='HU'] <- 'Hungary'
dataframe$Country[dataframe$Country=='IT'] <- 'Italy'
dataframe$Country[dataframe$Country=='LT'] <- 'Lithuania'
dataframe$Country[dataframe$Country=='NL'] <- 'Netherlands'
dataframe$Country[dataframe$Country=='PL'] <- 'Poland'
dataframe$Country[dataframe$Country=='PT'] <- 'Portugal'
dataframe$Country[dataframe$Country=='RO'] <- 'Romania'
dataframe$Country[dataframe$Country=='SK'] <- 'Slovakia'
dataframe$Country[dataframe$Country=='SI'] <- 'Slovenia'
dataframe$Country[dataframe$Country=='ES'] <- 'Spain'
dataframe$Country[dataframe$Country=='SE'] <- 'Sweden'
dataframe$Country[dataframe$Country=='GB'] <- 'UK'

## MANUALLY INSERT NUMBER OF GREEN SEATS AND TOTAL SEATS IN 2019 EUROPEAN PARLIAMENTARY ELECTIONS

dataframe$GreenSeats <- c(2,0,0,0,3,2,0,2,12,21,0,0,0,2,3,0,1,0,0,0,2,2,11)
dataframe$TotalSeats <- c(18,17,11,6,21,13,6,13,74,96,21,21,73,11,26,51,21,32,13,8,54,20,73)
dataframe$GreenPercent <- dataframe$GreenSeats/dataframe$TotalSeats

## CHANGE ALL COLUMNS OTHER THAN COUNTRY TO NUMERIC VALUES

for (i in colnames(dataframe)) {
  if (i!="Country") {
    dataframe[[i]] <- as.numeric(dataframe[[i]])
  } 
}

## PERFORM STEP-WISE REGRESSSION

redata = subset(dataframe, select=-c(Country,GreenSeats,TotalSeats))

  #Print out Adjusted R-squared and p-value for each column

for (i in colnames(redata)) {
  f <- summary(lm(GreenPercent ~ eval(parse(text = i)),data=redata))$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  if (p<0.15) {
    cat("Variable: ",i,"\n")
    cat("Adjusted R-squared: ",summary(lm(GreenPercent ~ eval(parse(text = i)),data=redata))$adj.r.squared, "\n")
    cat("p-value ",p, "\n\n")
  }
}

  #Remove columns with high p-values. This is necessary as the number of columns cannot be more than the number of countries in order to do step-wise

for (i in colnames(redata)) {
  f <- summary(lm(GreenPercent ~ eval(parse(text = i)),data=redata))$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  if (p>0.15) {
    redata <- redata[,!(names(redata) %in% i)]
  }
  # if(i=="V097EF") {                             #Possibly remove, as it is a categorical variable which debatably can be considered ranked
  #   redata <- redata[,!(names(redata) %in% i)]
  # }
}

modstart = lm(dataframe$GreenPercent ~ 1, data = redata)
mod = lm(dataframe$GreenPercent ~ ., data = redata)
summary(modstart)
summary(mod)

step(modstart, direction = 'forward', scope = formula(mod))

#Questionable Categorical V097EF
newmod1 = lm(formula = dataframe$GreenPercent ~ F118 + E117 + G007_36_B + 
              V097EF + A039 + A032 + F122 + E069_18 + A003 + E069_07 + 
              C002 + G256, data = redata)
summary(newmod1)

#Without Categorical
newmod2 =lm(formula = dataframe$GreenPercent ~ F118 + E117 + G007_36_B + 
             A039 + E263 + C002_01 + log(A032) + E069_07, data = redata)
summary(newmod2)

## DISCREPENCY BETWEEN PREDICTIONS AND REALITY

prediction1 <- dataframe[c("Country", "F118", "E117", "G007_36_B", "V097EF", "A039", "A032", "F122", "E069_18", "A003", "E069_07", "C002", "G256","GreenSeats", "TotalSeats", "GreenPercent")]
prediction1$predictedGreenPercent <- predict(newmod1,newdata=prediction1)
prediction1$predictedGreenSeats <- prediction1$predictedGreenPercent * prediction1$TotalSeats
prediction1$predictiondiscrepency <- prediction1$predictedGreenSeats - prediction1$GreenSeats
mean(abs(prediction1$predictiondiscrepency))

ggplot(prediction1, aes(x=reorder(Country,-abs(predictiondiscrepency)),y=abs(predictiondiscrepency))) + 
  geom_bar(stat="identity", fill = "#FF6666",width=0.6) +
  theme(aspect.ratio = 3/4) +
  theme(axis.text.x = element_text(angle = 90,vjust=0.5,hjust=1,size=15)) +
  xlab("Country") +
  theme(axis.title.x = element_text(size=15)) +
  ylab("Residual of Actual Green Seat % vs Predicted Green Seat %") +
  theme(axis.title.y = element_text(size=15)) +
  theme(axis.text.y = element_text(size=10)) +
  ggtitle("Residual of Actual Green Seat % vs Predicted Green Seat % Per Country")

prediction2 <- dataframe[c("Country", "F118", "E117", "G007_36_B","A039", "E263", "C002_01", "A032", "E069_07","GreenSeats", "TotalSeats", "GreenPercent")]
prediction2$predictedGreenPercent <- predict(newmod2,newdata=prediction2)
prediction2$predictedGreenSeats <- prediction2$predictedGreenPercent * prediction2$TotalSeats
prediction2$predictiondiscrepency <- prediction2$predictedGreenSeats - prediction2$GreenSeats
mean(abs(prediction2$predictiondiscrepency))

ggplot(prediction2, aes(x=reorder(Country,-abs(predictiondiscrepency)),y=abs(predictiondiscrepency))) + 
  geom_bar(stat="identity", fill = "#FF6666",width=0.6) +
  theme(aspect.ratio = 3/4) +
  theme(axis.text.x = element_text(angle = 90,vjust=0.5,hjust=1,size=15)) +
  xlab("Country") +
  theme(axis.title.x = element_text(size=15)) +
  ylab("Residual of Actual Green Seat % vs Predicted Green Seat %") +
  theme(axis.title.y = element_text(size=15)) +
  theme(axis.text.y = element_text(size=10)) +
  ggtitle("Residual of Actual Green Seat % vs Predicted Green Seat % Per Country")

ggplot(prediction2, aes(x=reorder(Country,predictiondiscrepency),y=predictiondiscrepency)) + 
  geom_bar(stat="identity", fill = "#FF6666",width=0.6) +
  theme(aspect.ratio = 3/4) +
  theme(axis.text.x = element_text(angle = 90,vjust=0.5,hjust=1,size=15)) +
  xlab("Country") +
  theme(axis.title.x = element_text(size=15)) +
  ylab("Residual of Actual Green Seat % vs Predicted Green Seat %") +
  theme(axis.title.y = element_text(size=15)) +
  theme(axis.text.y = element_text(size=10)) +
  ggtitle("Residual of Actual Green Seat % vs Predicted Green Seat % Per Country")


#################################################

## MAPPING

#################################################


## MAP OF ACTUAL RESULTS OF THE 2019 EU PARLIAMENT ELECTIONS

eucountries <- map_data("world", region= c("Austria","Italy","France","Spain","Portugal","Ireland", "Luxembourg", "Belgium",
                                           "Netherlands", "Germany","Denmark","Czech Republic","Poland", 
                                           "Slovakia","Hungary","Slovenia","Croatia", "Sweden", "Finland","Latvia",
                                           "Estonia","Lithuania","Greece","Malta","Bulgaria","Romania",
                                           "UK","Cyprus"))

greenvote <- data.frame(region = unique(prediction1$Country),
                        greenseats = prediction1$GreenPercent)

eucountries1 <- left_join(eucountries,greenvote,by="region")

ggplot()+
  
  geom_polygon(data=eucountries1, aes(x=long, y=lat, 
                                     group=group, 
                                     fill=greenseats),
               colour="white")+
  scale_fill_gradient(name="Green Seat Share %", low="grey93", high="green3")+
  
  ## remove background
  theme_void()+
  ggtitle("Actual 2019 EU Parliament Election Green Results by Country")+
  theme(plot.title = element_text(size=22),legend.key.size = unit(2,'cm'),legend.text = element_text(size=15),legend.title = element_text(size=15) )+
  coord_quickmap()

## MAP OF RESULTS OF THE 2019 EU PARLIAMENT ELECTIONS PREDICTED BY THE MODEL INCLUDING THE QUESTIONABLE CATEGORICAL VARIABLE

greenvotepredict1 <- data.frame(region = unique(prediction1$Country),
                        greenseats = prediction1$predictedGreenPercent)
greenvotepredict1$greenseats <- ifelse(greenvotepredict1$greenseats < 0, 0, greenvotepredict1$greenseats)

eucountries2 <- left_join(eucountries,greenvotepredict1,by="region")

ggplot()+
  
  geom_polygon(data=eucountries2, aes(x=long, y=lat, 
                                     group=group, 
                                     fill=greenseats),
               colour="white")+
  scale_fill_gradient(name="Green Seat Share %", low="grey93", high="green3")+
  
  ## remove background
  theme_void()+
  ggtitle("Predicted 2019 EU Parliament Election Green Results by Country")+
  theme(plot.title = element_text(size=22),legend.key.size = unit(2,'cm'),legend.text = element_text(size=15),legend.title = element_text(size=15) )+
  coord_quickmap()

## MAP OF RESULTS OF THE 2019 EU PARLIAMENT ELECTIONS PREDICTED BY THE MODEL WITHOUT THE QUESTIONABLE CATEGORICAL VARIABLE

greenvotepredict2 <- data.frame(region = unique(prediction2$Country),
                                greenseats = prediction2$predictedGreenPercent)
greenvotepredict2$greenseats <- ifelse(greenvotepredict2$greenseats < 0, 0, greenvotepredict2$greenseats)

eucountries3 <- left_join(eucountries,greenvotepredict2,by="region")

ggplot()+
  
  geom_polygon(data=eucountries3, aes(x=long, y=lat, 
                                      group=group, 
                                      fill=greenseats),
               colour="white")+
  scale_fill_gradient(name="Green Seat Share %", low="grey93", high="green3",limits = c(0,0.2))+
  
  ## remove background
  theme_void()+
  ggtitle("Predicted 2019 EU Parliament Election Green Results by Country")+
  theme(plot.title = element_text(size=22),legend.key.size = unit(2,'cm'),legend.text = element_text(size=15),legend.title = element_text(size=15) )+
  coord_quickmap()


#################################################

## APPLYING ALL OF THE PREVIOUS STEPS TO COUNTRIES OUTSIDE OF THE EU

#################################################

for (t in unique(data$cntry_AN)) {
  values <- c()
  names <- c()
  weights <- c()
  nation <- subset(data, cntry_AN == t)
  for (i in nation$gwght) {
    weights <- append(weights, i)
  }
  j = 1
  for (i in colnames(nation)) {
    if (j > 40) {
      if (class(nation[[i]]) == "integer") {
        nation[i] <- nation[i] * weights[j]
        values <- append(values, mean(nation[[i]][nation[[i]]>0]))
        names <- append(names, i)
      } 
    } else if (i == 'cntry_AN') {
      values <- append(values, t)
      names <- append(names, 'Country')
    }
    j <- j + 1
  }
  if (t == "AL") {
    dataframeWorld <- as.data.frame(t(values))
    colnames(dataframeWorld) <- names
  }
  else {
    temp <- as.data.frame(t(values))
    colnames(temp) <- names
    dataframeWorld <- rbind(dataframeWorld,temp)
  }
}

for (i in colnames(dataframeWorld)) {
  if (i!="Country") {
    dataframeWorld[[i]] <- as.numeric(dataframeWorld[[i]])
  } 
}

dataframeWorld$Country[dataframeWorld$Country=='AL'] <- 'Albania'
dataframeWorld$Country[dataframeWorld$Country=='AD'] <- 'Andorra'
dataframeWorld$Country[dataframeWorld$Country=='AZ'] <- 'Azerbaijan'
dataframeWorld <- subset(dataframeWorld, Country!="AR") #Countries removed had null values for some of the variables needed in the prediction
dataframeWorld$Country[dataframeWorld$Country=='AU'] <- 'Australia'
dataframeWorld$Country[dataframeWorld$Country=='AT'] <- 'Austria'
dataframeWorld$Country[dataframeWorld$Country=='BD'] <- 'Bangladesh'
dataframeWorld$Country[dataframeWorld$Country=='AM'] <- 'Armenia'
dataframeWorld$Country[dataframeWorld$Country=='BO'] <- 'Bolivia'
dataframeWorld$Country[dataframeWorld$Country=='BA'] <- 'Bosnia and Herzegovina'
dataframeWorld <- subset(dataframeWorld, Country!="BR")
dataframeWorld$Country[dataframeWorld$Country=='BG'] <- 'Bulgaria'
dataframeWorld$Country[dataframeWorld$Country=='MM'] <- 'Myanmar'
dataframeWorld$Country[dataframeWorld$Country=='BY'] <- 'Belarus'
dataframeWorld$Country[dataframeWorld$Country=='CA'] <- 'Canada'
dataframeWorld$Country[dataframeWorld$Country=='CL'] <- 'Chile'
dataframeWorld$Country[dataframeWorld$Country=='CN'] <- 'China'
dataframeWorld$Country[dataframeWorld$Country=='TW'] <- 'Taiwan'
dataframeWorld$Country[dataframeWorld$Country=='CO'] <- 'Colombia'
dataframeWorld$Country[dataframeWorld$Country=='HR'] <- 'Croatia'
dataframeWorld$Country[dataframeWorld$Country=='CY'] <- 'Cyprus'
dataframeWorld$Country[dataframeWorld$Country=='CZ'] <- 'Czech Republic'
dataframeWorld$Country[dataframeWorld$Country=='DK'] <- 'Denmark'
dataframeWorld$Country[dataframeWorld$Country=='EC'] <- 'Ecuador'
dataframeWorld$Country[dataframeWorld$Country=='ET'] <- 'Ethopia'
dataframeWorld$Country[dataframeWorld$Country=='EE'] <- 'Estonia'
dataframeWorld$Country[dataframeWorld$Country=='FI'] <- 'Finland'
dataframeWorld$Country[dataframeWorld$Country=='FR'] <- 'France'
dataframeWorld$Country[dataframeWorld$Country=='GE'] <- 'Georgia'
dataframeWorld$Country[dataframeWorld$Country=='DE'] <- 'Germany'
dataframeWorld$Country[dataframeWorld$Country=='GR'] <- 'Greece'
dataframeWorld$Country[dataframeWorld$Country=='GT'] <- 'Guatemala'
dataframeWorld$Country[dataframeWorld$Country=='HK'] <- 'Hong Kong'
dataframeWorld$Country[dataframeWorld$Country=='HU'] <- 'Hungary'
dataframeWorld$Country[dataframeWorld$Country=='IS'] <- 'Iceland'
dataframeWorld$Country[dataframeWorld$Country=='ID'] <- 'Indonesia'
dataframeWorld$Country[dataframeWorld$Country=='IR'] <- 'Iran'
dataframeWorld$Country[dataframeWorld$Country=='IQ'] <- 'Iraq'
dataframeWorld$Country[dataframeWorld$Country=='IT'] <- 'Italy'
dataframeWorld$Country[dataframeWorld$Country=='JP'] <- 'Japan'
dataframeWorld$Country[dataframeWorld$Country=='JO'] <- 'Jordan'
dataframeWorld$Country[dataframeWorld$Country=='KR'] <- 'South Korea'
dataframeWorld$Country[dataframeWorld$Country=='KG'] <- 'Kyrgzstan'
dataframeWorld$Country[dataframeWorld$Country=='LB'] <- 'Lebanon'
dataframeWorld <- subset(dataframeWorld, Country!="KZ")
dataframeWorld$Country[dataframeWorld$Country=='LT'] <- 'Lithuania'
dataframeWorld$Country[dataframeWorld$Country=='MO'] <- 'Macau'
dataframeWorld$Country[dataframeWorld$Country=='MY'] <- 'Malaysia'
dataframeWorld$Country[dataframeWorld$Country=='MX'] <- 'Mexico'
dataframeWorld$Country[dataframeWorld$Country=='ME'] <- 'Montenegro'
dataframeWorld$Country[dataframeWorld$Country=='NL'] <- 'Netherlands'
dataframeWorld$Country[dataframeWorld$Country=='NZ'] <- 'New Zealand'
dataframeWorld$Country[dataframeWorld$Country=='NI'] <- 'Nicaragua'
dataframeWorld$Country[dataframeWorld$Country=='NO'] <- 'Norway'
dataframeWorld$Country[dataframeWorld$Country=='PK'] <- 'Pakistan'
dataframeWorld$Country[dataframeWorld$Country=='PE'] <- 'Peru'
dataframeWorld$Country[dataframeWorld$Country=='PH'] <- 'Philippines'
dataframeWorld <- subset(dataframeWorld, Country!="NG")
dataframeWorld$Country[dataframeWorld$Country=='PL'] <- 'Poland'
dataframeWorld$Country[dataframeWorld$Country=='PT'] <- 'Portugal'
dataframeWorld$Country[dataframeWorld$Country=='PR'] <- 'Puerto Rico'
dataframeWorld$Country[dataframeWorld$Country=='RO'] <- 'Romania'
dataframeWorld$Country[dataframeWorld$Country=='RU'] <- 'Russia'
dataframeWorld$Country[dataframeWorld$Country=='RS'] <- 'Serbia'
dataframeWorld <- subset(dataframeWorld, Country!="SG")
dataframeWorld$Country[dataframeWorld$Country=='SK'] <- 'Slovakia'
dataframeWorld$Country[dataframeWorld$Country=='VN'] <- 'Vietnam'
dataframeWorld$Country[dataframeWorld$Country=='SI'] <- 'Slovenia'
dataframeWorld$Country[dataframeWorld$Country=='ZW'] <- 'Zimbabwe'
dataframeWorld$Country[dataframeWorld$Country=='ES'] <- 'Spain'
dataframeWorld$Country[dataframeWorld$Country=='SE'] <- 'Sweden'
dataframeWorld$Country[dataframeWorld$Country=='CH'] <- 'Switzerland'
dataframeWorld$Country[dataframeWorld$Country=='TN'] <- 'Tunisia'
dataframeWorld$Country[dataframeWorld$Country=='TR'] <- 'Turkey'
dataframeWorld$Country[dataframeWorld$Country=='UA'] <- 'Ukraine'
dataframeWorld$Country[dataframeWorld$Country=='MK'] <- 'North Macedonia'
dataframeWorld <- subset(dataframeWorld, Country!="TH")
dataframeWorld <- subset(dataframeWorld, Country!="TJ")
dataframeWorld <- subset(dataframeWorld, Country!="EG")
dataframeWorld$Country[dataframeWorld$Country=='GB'] <- 'UK'
dataframeWorld <- subset(dataframeWorld, Country!="US")

worldcountries <- map_data("world")

prediction3 <- dataframeWorld[c("Country", "F118", "E117", "G007_36_B","A039", "E263", "C002_01", "A032", "E069_07")]
prediction3$predictedGreenPercent <- predict(newmod2,newdata=prediction3) #The Second model was used as the categorical variable was deemed to not be a true rank


greenvotepredict3 <- data.frame(region = unique(prediction3$Country),
                                greenseats = prediction3$predictedGreenPercent)
greenvotepredict3$greenseats <- ifelse(greenvotepredict3$greenseats < 0, 0, greenvotepredict3$greenseats)

worldcountries <- left_join(worldcountries,greenvotepredict3,by="region")

ggplot()+
  
  geom_polygon(data=worldcountries, aes(x=long, y=lat, 
                                      group=group, 
                                      fill=greenseats),
               colour="white")+
  scale_fill_gradient(name="Green Seat Share %", low="grey93", high="green3",limits = c(0,0.2))+
  
  ## remove background
  theme_void()+
  ggtitle("Predicted 2019 EU Parliament Election Green Results by Country Globally")+
  theme(plot.title = element_text(size=22),legend.key.size = unit(2,'cm'),legend.text = element_text(size=15),legend.title = element_text(size=15) )+
  coord_quickmap()
