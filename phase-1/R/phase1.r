# Title: Phase 1 Data Mining
# Authors: Ali Goldani 9512762107
#         Mohammad Kahani 9512762447

# Libraries
library("ggpubr")
library("dplyr")
library("ltm")
library("caret")
library("FactoMineR") # for PCA function
library("clusterSim")
library("caTools")
library(randomForest)
library(MASS)
library(ggbiplot)

# Functions
range0_1 <- function(x){(x-min(x))/(max(x)-min(x))}

nominal_to_numeric <- function (vector) {
  unique <- unique(vector)
  unique <- unique[order(unique)]
  numeric <- data.frame(x=unique)
  numeric
}

# data read
data = read.csv('Data/SAP_normal.csv', na.strings=c("", "N/A")) # To replace null values in the data with "NA" so we can find them
head(data)

plot(data$StudentAbsenceDays, data$Class, main="Scatterplot Example", xlab="Car Weight ", ylab="Miles Per Gallon ", pch=19)


# Information about attributes
# Nominal Attributes: gender - nationality - place of birth - stageID - course topic - sectionID - semester - regarding parent attributes - absence days
# Ordinal Attributes: stageID - gradeID - class
# numeric Attributes: raisedhands - visitedResources - viewing announcement - discussions

nominal_to_numeric(data$gender)
# 1 F
# 2 M

nominal_to_numeric(data$NationalITy)
# 1        Egypt
# 2         Iran
# 3         Iraq
# 4       Jordan
# 5           KW
# 6      lebanon
# 7        Lybia
# 8      Morocco
# 9    Palestine
# 10 SaudiArabia
# 11       Syria
# 12       Tunis
# 13         USA
# 14    venzuela

nominal_to_numeric(data$StageID)
# 1   HighSchool
# 2   lowerlevel
# 3 MiddleSchool

nominal_to_numeric(data$PlaceofBirth)
# 1        Egypt
# 2         Iran
# 3         Iraq
# 4       Jordan
# 5       KuwaIT
# 6      lebanon
# 7        Lybia
# 8      Morocco
# 9    Palestine
# 10 SaudiArabia
# 11       Syria
# 12       Tunis
# 13         USA
# 14    venzuela

nominal_to_numeric(data$GradeID)
# 1  G-02
# 2  G-04
# 3  G-05
# 4  G-06
# 5  G-07
# 6  G-08
# 7  G-09
# 8  G-10
# 9  G-11
# 10 G-12

nominal_to_numeric(data$SectionID)
# 1 A
# 2 B
# 3 C

nominal_to_numeric(data$Topic)
# 1     Arabic
# 2    Biology
# 3  Chemistry
# 4    English
# 5     French
# 6    Geology
# 7    History
# 8         IT
# 9       Math
# 10     Quran
# 11   Science
# 12   Spanish

nominal_to_numeric(data$Semester)
# 1 F
# 2 S

nominal_to_numeric(data$Relation)
# 1 Father
# 2    Mum

nominal_to_numeric(data$ParentAnsweringSurvey)
# 1  No
# 2 Yes

nominal_to_numeric(data$ParentschoolSatisfaction)
# 1  Bad
# 2 Good

nominal_to_numeric(data$StudentAbsenceDays)
# 1 Above-7
# 2 Under-7

nominal_to_numeric(data$Class)
# 1 H
# 2 L
# 3 M

data_numeric <- data.matrix(data)
data_numeric <- as.data.frame(data_numeric)
#adding new numeric csv file
#write.csv(data_numeric,'phase-1/Data/SAP_numeric.csv')
head(data_numeric)

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

res <- cor.test(data$Class, data$Class, method = "pearson")
res

res <- cov(data$Class, data$StudentAbsenceDays)
res

newData <- table(data$Class, data$StudentAbsenceDays)
newData2 <- table(data$Class, data$gender)
newData3 <- table(data$Class, data$GradeID)
newData3

chisq_class_absence <- chisq.test(newData)
chisq_class_absence$p.value

chisq.test(newData3, correct=F)

#Data Reduction

#Feature Selection

selected <- data[,c('raisedhands','VisITedResources','AnnouncementsView', 'Discussion')]
selected

#pca variance order for numeric attributes
data_pca <- prcomp(data, center = T,scale.=T)
data_pca
summary(data_pca)
strin <- str(data_pca)

data.score <- c(rep("Bad", 3), rep("Meh",4), rep("Good", 7),rep("Meh",3), "Good", rep("Bad", 3), rep("Meh",4), rep("Good", 3), "Meh", rep("Good", 3))
data.score <- c("Good", "Meh", "Bad")
ggbiplot(data_pca, ellipse = T, labels = data$StudentAbsenceDays, groups = data$Class)
#amount of variation for each pc
pca.var <- data_pca$sdev^2
#get the percentage of data variation for each pca
pca.var.per <- round(pca.var/sum(pca.var)*100,1)
#visualizing the percentage of variation (can be used for answering questions)
barplot(pca.var.per, main="Scree plot", xlab="PCA", ylab="percentage of variation")
#another visualization
plot(data_pca$x[,1],data_pca$x[,2])


#another pca function for more detailed output
res.pca <- PCA(data, graph = T)
res.pca$var
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

#data normalization
normal_data <- data.Normalization (data_numeric,type="n4",normalization="column")
#write.csv(normal_data,'phase-1/Data/SAP_normal.csv')
summary(normal_data)

# machine learning & modeling

#not used yet
#can be used for creating train and test data
ind <- sample(2, nrow(normal_data), replace=TRUE, prob=c(0.67, 0.33))
sap_training <- normal_data[ind==1, 1:17]
sap_test <- normal_data[ind==2, 1:17]
summary(data_numeric)

#find attribute importance with logistic regression modeling & random forest
#fit_glm <- glm(Class~.,normal_data,family = "binomial")
#fit_rf <- randomForest(Class~., data=normal_data)
#varImp(fit_glm, scale=FALSE)
#varImp(fit_rf, scale=FALSE) #Error



# stepwise regression

# Fit the full model
full.model <- lm(Class ~., data = normal_data)
# Stepwise regression model
step.model <- stepAIC(full.model, direction = "both",
                      trace = FALSE)
summary(step.model)

models <- regsubsets(Class~., data = normal_data, nvmax = 6,
                     method = "backward")
summary(models)

# Set seed for reproducibility
set.seed(123)
# Set up repeated k-fold cross-validation
train.control <- trainControl(method = "cv", number = 5)
# Train the model
step.model <- train(Class ~., data = normal_data,
                    method = "leapSeq",
                    tuneGrid = data.frame(nvmax = 1:10),
                    trControl = train.control
                    )
step.model$results
step.model$bestTune
coef(step.model$finalModel, 2)

lm(Class ~ Relation + StudentAbsenceDays,
   data = normal_data)