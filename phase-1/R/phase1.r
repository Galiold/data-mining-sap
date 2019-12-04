#Title: Phase 1 Data Mining
#Authors: Ali Goldani 9512762
#         Mohammad Kahani 9512762447

# Libraries
library("ggpubr")
library("dplyr")
library("ltm")
library("caret")
library("FactoMineR") # for PCA function
library("clusterSim")


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
ggqqplot(data$AnnouncementsVie, ylab = "dis")

res <- cor.test(data$raisedhands, data$raisedhands, method = "pearson")
res

newData <- table(data$Class, data$StudentAbsenceDays)
newData2 <- table(data$Class, data$gender)
newData3 <- table(data$Class, data$GradeID)
newData3

chisq.test(newData, correct=F)
chisq.test(newData3, correct=F)

#Data Reduction

#Feature Selection

selected <- data[,c('raisedhands','VisITedResources','AnnouncementsView', 'Discussion')]
selected

#pca variance order for numeric attributes
data_pca <- prcomp(selected, scale.=T)
data_pca
summary(data_pca)
#amount of variation for each pc
pca.var <- data_pca$sdev^2
#get the percentage of data variation for each pca
pca.var.per <- round(pca.var/sum(pca.var)*100,1)
#visualiztin the percentage of variation (can be used for answering questions)
barplot(pca.var.per, main="Scree plot", xlab="PC", ylab="percentage of variation")
#another visualization
plot(data_pca$x[,1],data_pca$x[,2])


#another pca function for more detailed output
res.pca <- PCA(selected, graph = FALSE)
#information needed for variation percentage can be better found here
summary(res.pca)


# ensure the results are repeatable
set.seed(7)
# load the data
data(data)
help(data)
# calculate correlation matrix
correlationMatrix <- cor(data[10:13])
# summarize the correlation matrix
print(correlationMatrix)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5)
# print indexes of highly correlated attributes
print(highlyCorrelated)

#TODO Data Reduction: dimensionality reduction
#biserial.cor(data$raisedhands, data$NationalITy)
#biserial.cor()

#TODO feature selection need to be enhanced

#TODO feature generation

#data transformation

#TODO data discretization
#TODO data aggregation

#data normalization for numeric att
normal_data <- data.Normalization (selected,type="n4",normalization="column")
normal_data
