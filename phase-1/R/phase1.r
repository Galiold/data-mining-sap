# data read
data = read.csv('Data/SAP.csv', na.strings=c("", "N/A")) # To replace null values in the data with "NA" so we can find them
data

# data cleaning

# Handle missing data
missing_data <- data[!complete.cases(data),]  # returns 0 rows, so there are no missing values
data <- na.omit(data) # Omits the null values, here no value is ommitted
data


# Handle noisy data

plotinfo <- boxplot(data$raisedhands, data$VisITedResources, data$AnnouncementsView, data$Discussion,
main = "Testing data for outliers",
names = c("raisedhands", "VisITedResources", "AnnouncementsView", "Discussion"),
ylab = "Attributes",
col = c("lightblue", "green", "darkred", "orange"),
border = "brown",
horizontal = TRUE,
notch = TRUE
)

#check for outliers

data[which(data$raisedhands %in% plotinfo$out), ] # output -> null
data[which(data$VisITedResources %in% plotinfo$out), ] # output -> null
data[which(data$AnnouncementsView %in% plotinfo$out), ] # output -> null
data[which(data$Discussion %in% plotinfo$out), ] # output -> null

#missing data

data[!complete.cases(data),]
newdata <- na.omit(data)
newdata[!complete.cases(newdata),]
