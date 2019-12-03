# Libraries
library("ggpubr")
library("dplyr")


# data read
data = read.csv('phase-1/Data/SAP.csv', na.strings=c("", "N/A")) # To replace null values in the data with "NA" so we can find them
data

# 1. Data Cleaning

# 1.1. Handle missing data
missing_data <- data[!complete.cases(data),]  # returns 0 rows, so there are no missing values
data <- na.omit(data) # Omits the null values, here no value is ommitted
data


# 1.2. Handle noisy data

# 1.2.1. Create box plot
plotinfo <- boxplot(data$raisedhands, data$VisITedResources, data$AnnouncementsView, data$Discussion,
main = "Testing data for outliers",
names = c("raisedhands", "VisITedResources", "AnnouncementsView", "Discussion"),
ylab = "Attributes",
col = c("lightblue", "green", "darkred", "orange"),
border = "brown",
horizontal = TRUE,
notch = TRUE
)

# 1.2.2. Check for outliers
data[which(data$raisedhands %in% plotinfo$out), ] # output -> null
data[which(data$VisITedResources %in% plotinfo$out), ] # output -> null
data[which(data$AnnouncementsView %in% plotinfo$out), ] # output -> null
data[which(data$Discussion %in% plotinfo$out), ] # output -> null

# 2. Data Integration

# 2.1. Handling Redundancy
data <- data[!duplicated(data),]

# 2.2. Handling Correlation
ggqqplot(data$AnnouncementsView, ylab = "dis")

res <- cor.test(data$raisedhands, data$raisedhands, method = "pearson")
res

newData <- table(data$Class, data$StudentAbsenceDays)
newData