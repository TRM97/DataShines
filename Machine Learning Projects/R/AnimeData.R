#Exploring the dataset
a=read.csv('E:\\OneDrive\\DS\\Documents\\R\\Case Study\\1\\Anime_Data.csv',na.strings=c('[]',""))
View(a)
dim(a) #--7029 rows by 16 columns

#Our target variable is rating (continuous). Thus it is a regression problem.
TargetVariableName='rating'
TargetVariable=a[, c(TargetVariableName)]
TargetVariable
#Converting the categorical variables to factors
a$mediaType=as.factor(a$mediaType)
a$sznOfRelease=as.factor(a$sznOfRelease)
a$ongoing=as.factor(a$ongoing)
str(a)
#Dropping title column
a$title=NULL

#Checking for missing values
colSums(is.na(a))
#*CONTINUOUS Columns:
#*duration: 1696/7029 (24.12%)
#*watched: 87/7029 (1.24%)
#*We will perform imputation of these columns with their respective medians (as missing value percentage is less than 30%)
median(a$duration,na.rm=TRUE) #--median(duration)=5
median(a$watched,na.rm=TRUE) #--median(watched)=385.5
ceiling(median(a$watched,na.rm=TRUE))

a$duration[is.na(a$duration)] = median(a$duration,na.rm=TRUE)
a$watched[is.na(a$watched)] = ceiling(median(a$watched,na.rm=TRUE))
#*QUALITATIVE Columns:
#*mediaType: 30/7029 (0.43%)
#*contentWarn: 6242/7029 (88.8%)
#*studios: 2239/7029 (31.8%)
#*sznOfRelease: 4916/7029 (69.94%)
#*description: 3083/7029 (43.86%)
#*tags: 229/7029 (3.26%)
#*We will drop contentWarn, studios, sznOfRelease, description columns as the percentage of missing values in these columns is more than 30%. Also contentWarn, description will not aid in the prediction process. We will drop tags even if the missing value percentage is less than 30% because it is not helpful from the prediction perspective.
a$contentWarn=NULL
a$studios=NULL
a$sznOfRelease=NULL
a$description=NULL
a$tags=NULL
#*However we will impute mediaType column with it's mode value
FunctionMode=function(i){
  ModeValue=names(table(i)[table(i)==max(table(i))])
  return(ModeValue)
}
FunctionMode(a$mediaType)
a$mediaType[is.na(a$mediaType)] = "TV"
colSums(is.na(a))
#Now there are no missing values anymore
str(a)

#Checking for outliers in the categorical variables and treating them (if any are present)
dev.off()
boxplot(a$eps, horizontal = T)
boxplot(a$eps, horizontal = T,grid(10,NA), xaxp = c(1, 1000, 10))
a$eps = ifelse(a$eps > 28 , 28, a$eps)

boxplot(a$duration, horizontal = T)
boxplot(a$duration, horizontal = T,grid(20,NA), xaxp = c(1, 300, 30))
a$duration = ifelse(a$duration > 130 , 130, a$duration)

boxplot(a$watched, horizontal = T)
boxplot(a$watched, horizontal = T,grid(20,NA), xaxp = c(1, 60000, 10))
a$watched = ifelse(a$watched > 42500 , 42500, a$watched)

boxplot(a$watching, horizontal = T)
boxplot(a$watching, horizontal = T,grid(20,NA), xaxp = c(1, 12000, 30))
a$watching = ifelse(a$watching > 11500 , 11500, a$watching)

boxplot(a$wantWatch,horizontal=T)
a$wantWatch = ifelse(a$wantWatch > 13500 , 13500, a$wantWatch)

boxplot(a$dropped,horizontal = T)
a$dropped = ifelse(a$dropped > 1500 , 1500, a$dropped)

boxplot(a$votes, horizontal = T)
a$votes = ifelse(a$votes > 25000 , 25000, a$votes)

boxplot(a$rating, horizontal = T)
#The boxplot for rating shows that there are no outliers


#UNIVARIATE ANALYSIS
summary(a)
#*CONTINUOUS COLUMNS: eps, duration, watched, watching, wantWatch, dropped, votes
#*CATEGORICAL COLUMNS: mediaType, ongoing
#*TARGET VARIABLE: rating (continuous)

con_cols=c("rating","eps", "duration", "watched", "watching", "wantWatch", "dropped", "votes")
cat_cols=c("mediaType", "ongoing")

#measures of central tendency
sapply(a[,con_cols],mean)
sapply(a[,con_cols],median)
sapply(a[,cat_cols],FunctionMode)
##measures of dispersion/spread:
sapply(a[,con_cols],range)
sapply(a[,con_cols],sd)
sapply(a[,con_cols],var)

##measures of location:
sapply(a[,con_cols],quantile)

dev.off()
par(mfrow=c(4,2))
library(RColorBrewer) 
for (i in con_cols){
  hist(a[,c(i)], main=paste('Histogram of:',i), 
       col=("blue"))
}
hist(a$rating,col="blue")

for (j in cat_cols){
  barplot(table(a[,c(j)]), main=paste('Barplot of:',j), 
          col="orange")
}

#BIVARIATE ANALYSIS
#Continuous Vs Continuous ---- Scatter Plot
plot(a[, con_cols], col='blue')
plot(x=a$watched,y=a$rating,col="blue")
plot(x=a$log_watched,y=a$rating,col="green")

plot(x=a$wantWatch,y=a$rating,col="blue")

plot(x=a$votes,y=a$rating,col="blue")
plot(x=a$log_votes,y=a$rating,col="green")

#Continuous Vs Categorical --- Box Plot
boxplot(rating ~ mediaType, data = a, col="orange")
boxplot(rating ~ ongoing, data = a, col="orange")

#RELATIONSHIP STRENGTH TEST

#CONTINUOUS VS CONTINUOUS : Correlation Test
CorrData=cor(a[, con_cols], use = "complete.obs")
CorrData
names(CorrData['rating',][abs(CorrData['rating',])>0.4])
##Thus POTENTIAL CONTINUOUS PREDICTORS: watched, wantWatch, votes

#CONTINUOUS VS CATEGORICAL : ANOVA Test
##H0: the variables (rating and mediaType) are NOT related
summary(aov(rating ~ a$mediaType, data = a))
##p<0.05 and very low: rating and mediaType are RELATED STRONGLY
###H0: the variables (rating and ongoing) are NOT related
summary(aov(rating ~ a$ongoing, data = a))
###p<0.05 and very low: rating and ongoing are RELATED STRONGLY

#GENERATING GENERIC FORMAT
TargetVariable
BestPredictorName= c("watched", "wantWatch", "votes", "mediaType","ongoing")
BestPredictorName

PredictorVariable=a[, BestPredictorName]
PredictorVariable
str(PredictorVariable)


##Creating the final data to be used for ML
DataForML=data.frame(TargetVariable,PredictorVariable)
str(DataForML)
head(DataForML)


#SAMPLING
set.seed(123)
TrainingSampleIndex=sample(1:nrow(DataForML), size=0.7 * nrow(DataForML))
length(TrainingSampleIndex) #1005 row indexes
DataForMLTrain=DataForML[TrainingSampleIndex, ]
head(DataForMLTrain)
DataForMLTest=DataForML[-TrainingSampleIndex, ]
head(DataForMLTest)
dim(DataForMLTrain)
dim(DataForMLTest)

#DEPLOYING LINEAR REGRESSION MODEL
Model_Reg=lm(TargetVariable~.,data=DataForMLTrain)
summary(Model_Reg)

Model_Reg_2=lm(TargetVariable~wantWatch+votes+mediaType+ongoing,data=DataForMLTrain)
summary(Model_Reg_2)

library(car)
VIF=vif(Model_Reg_2)
data.frame(VIF)
#Adjusted R-squared:  0.4342

head(DataForMLTest)
DataForMLTest$Pred_LM=predict(Model_Reg_2, DataForMLTest)
head(DataForMLTest)

DataForMLTest$LM_APE= 100 *(abs(DataForMLTest$TargetVariable-DataForMLTest$Pred_LM)/DataForMLTest$TargetVariable)
head(DataForMLTest)


MeanAPE=mean(DataForMLTest$LM_APE)
MedianAPE=median(DataForMLTest$LM_APE)
print(paste('### Mean Accuracy of Linear Regression Model is: ', 100 - MeanAPE)) #76.5%
print(paste('### Median Accuracy of Linear Regression Model is: ', 100 - MedianAPE)) #84.4%


##WITH LOG TRANSFORMATION
a$log_watched=log(a$watched)
a$log_votes=log(a$votes)
a$log_watched

hist(a$log_watched,col=("green"))
cor(x=a$rating,y=a$watched)
cor(x=a$rating,y=log(a$watched))
min(a$watched)
a$watched[a$watched==0]=1

hist(a$votes)
hist(a$log_votes,col=("green"))
cor(x=a$rating,y=a$votes)
cor(x=a$rating,y=log(a$votes))

BestPredictorNameNew= c("log_watched", "wantWatch", "log_votes","mediaType","ongoing")
BestPredictorNameNew

PredictorVariable=a[, BestPredictorNameNew]
PredictorVariable
str(PredictorVariable)

##Creating the final data to be used for ML
DataForML=data.frame(TargetVariable,PredictorVariable)
str(DataForML)
head(DataForML)


#SAMPLING
set.seed(123)
TrainingSampleIndex=sample(1:nrow(DataForML), size=0.7 * nrow(DataForML))
length(TrainingSampleIndex) #1005 row indexes
DataForMLTrain=DataForML[TrainingSampleIndex, ]
head(DataForMLTrain)
DataForMLTest=DataForML[-TrainingSampleIndex, ]
head(DataForMLTest)
dim(DataForMLTrain)
dim(DataForMLTest)

#DEPLOYING LINEAR REGRESSION MODEL
Model_Reg=lm(TargetVariable~.,data=DataForMLTrain)
summary(Model_Reg)

Model_Reg_2=lm(TargetVariable~log_watched+wantWatch+log_votes+I(mediaType=="Movie")+I(mediaType=="Music Video")+I(mediaType=="Movie")+I(mediaType=="Other")+I(mediaType=="OVA")+I(mediaType=="TV Special")+I(mediaType=="Web")+ongoing,data=DataForMLTrain)
summary(Model_Reg_2)

Model_Reg_3=lm(TargetVariable~log_watched+wantWatch+log_votes+I(mediaType=="Movie")+I(mediaType=="Other")+I(mediaType=="OVA")+I(mediaType=="TV Special")+I(mediaType=="Web")+ongoing,data=DataForMLTrain)
summary(Model_Reg_3)

library(car)
VIF=vif(Model_Reg_3)
data.frame(VIF)

Model_Reg_4=lm(TargetVariable~log_watched+wantWatch+I(mediaType=="Movie")+I(mediaType=="Other")+I(mediaType=="OVA")+I(mediaType=="TV Special")+I(mediaType=="Web")+ongoing,data=DataForMLTrain)
summary(Model_Reg_4)

VIF=vif(Model_Reg_4)
data.frame(VIF)
#Adjusted R-squared:  0.5708

head(DataForMLTest)
DataForMLTest$Pred_LM=predict(Model_Reg_4, DataForMLTest)
head(DataForMLTest)

DataForMLTest$LM_APE= 100 *(abs(DataForMLTest$TargetVariable-DataForMLTest$Pred_LM)/DataForMLTest$TargetVariable)
head(DataForMLTest)

MeanAPE=mean(DataForMLTest$LM_APE)
MedianAPE=median(DataForMLTest$LM_APE)
print(paste('### Mean Accuracy of Linear Regression Model is: ', 100 - MeanAPE)) #80.44%
print(paste('### Median Accuracy of Linear Regression Model is: ', 100 - MedianAPE)) #87.2%


all(is.na(log_votes)) #log_watched+wantWatch+log_votes
all(is.na(y))
all(is.na(y^trans))
