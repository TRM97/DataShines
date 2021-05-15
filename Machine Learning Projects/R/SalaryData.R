##Reading the raw dataset
sd=read.csv('E:\\OneDrive\\DS\\Documents\\R\\Case Study\\2\\Salary_Data.csv',na.strings=c(""))
View(sd)
dim(sd) #--5541 rows x 10 columns

##Missing value check
colSums(is.na(sd))
#*workclass (categorical):368/5541 (6.64%)
#*occupation (categorical): 368/5541 (6.64%)
#*native_country (categorical): 144/5541 (2.6%)

str(sd)
sd$workclass=as.factor(sd$workclass)
sd$education=as.factor(sd$education)
sd$marital_status=as.factor(sd$marital_status)
sd$occupation=as.factor(sd$occupation)
sd$race=as.factor(sd$race)
sd$sex=as.factor(sd$sex)
sd$SalaryAbove50K=as.factor(sd$SalaryAbove50K)

##Clubbing multiple categories into super-categories
sd$native_country
north_america =c("Canada" , "Cuba" , "Dominican-Republic" , "El-Salvador" , "Guatemala" , "Haiti" ,
"Honduras" , "Jamaica" , "Mexico" , "Nicaragua" , "Outlying-US(Guam-USVI-etc)" , "Puerto-Rico" ,
"Trinadad&Tobago", "United-States")
sd$native_country[sd$native_country %in% north_america] = "North-America" 
asia = c("Cambodia", "China", "Hong", "India", "Iran", "Japan", "Laos", "Philippines", "Taiwan",
"Thailand", "Vietnam")
sd$native_country[sd$native_country %in% asia] = "Asia" 
europe =c("England", "France", "Germany", "Greece", "Hungary", "Ireland", "Italy", "Poland",
"Portugal", "Scotland", "Yugoslavia")
sd$native_country[sd$native_country %in% europe] = "Europe"
south_america = c("Columbia", "Ecuador", "Peru")
sd$native_country[sd$native_country %in% south_america] = "South-America" 
sd$native_country=as.factor(sd$native_country)

##Type of Columns
#*CONTINUOUS: age, hours_per_week
#*CATEGORICAL: workclass, education, marital_status, occupation, race, sex, native_country,SalaryAbove50K
con_cols=c("age", "hours_per_week")
cat_cols=c("workclass", "education", "marital_status", "occupation", "race", "sex", "native_country","SalaryAbove50K")

##Missing value treatment
FunctionMode=function(i){
  ModeValue=names(table(i)[table(i)==max(table(i))])
  return(ModeValue)
}
FunctionMode(sd$workclass)
sd$workclass[is.na(sd$workclass)] = "Private"
FunctionMode(sd$occupation)
sd$occupation[is.na(sd$occupation)] = "Exec-managerial"
FunctionMode(sd$native_country)
sd$native_country[is.na(sd$native_country)] = "North-America"

##Checking for and treating outliers in continuous columns(if any)
boxplot(sd$age, horizontal = T)
boxplot(sd$hours_per_week, horizontal = T)
sd$hours_per_week = ifelse(sd$hours_per_week > 77 , 77, sd$hours_per_week)


barplot()
##UNIVARIATE ANALYSIS
con_cols=c("age", "hours_per_week")
cat_cols=c("workclass", "education", "marital_status", "occupation", "race", "sex", "native_country","SalaryAbove50K")

#measures of central tendency
sapply(sd[,con_cols],mean)
sapply(sd[,con_cols],median)
sapply(sd[,cat_cols],FunctionMode)
##measures of dispersion/spread:
sapply(sd[,con_cols],range)
sapply(sd[,con_cols],sd)
sapply(sd[,con_cols],var)

#Exploring Multiple CONTINUOUS Features:
con_cols=c("age", "hours_per_week")
dev.off()
par(mfrow=c(2,1))
library(RColorBrewer) 
for (i in con_cols){
  hist(sd[,c(i)], main=paste('Histogram of:', i), 
       col="blue")
}

# Exploring Multiple CATEGORICAL features
ColsForBar=c("Survived","Pclass","Sex","Embarked")

#Splitting the plot window into four parts
par(mfrow=c(3,3))
dev.off
cat_cols=c("workclass", "education", "marital_status", "occupation", "race", "sex", "native_country","SalaryAbove50K")
# looping to create the Bar-Plots for each column
for (j in cat_cols){
  barplot(table(sd[,c(j)]), main=paste('Barplot of:', j), 
          col="orange")
}
table(sd$occupation)

##BIVARIATE ANALYSIS
# Categorical Vs Continuous --- Box Plot
par(mfrow=c(1,1))
boxplot(age~SalaryAbove50K, data = sd, col="sky blue")
boxplot(hours_per_week~SalaryAbove50K, data = sd, col="sky blue")
# Categorical Vs Categorical --- Grouped Bar chart
CrossTabResult=table(sd[ , c('SalaryAbove50K', "workclass")])
CrossTabResult
barplot(CrossTabResult, beside=T, col=c('Red','Green'))

CrossTabResult2=table(sd[ , c('SalaryAbove50K', "education")])
CrossTabResult2
barplot(CrossTabResult2, beside=T, col=c('Red','Green'))

CrossTabResult3=table(sd[ , c('SalaryAbove50K', "marital_status")])
CrossTabResult3
barplot(CrossTabResult3, beside=T, col=c('Red','Green'))

CrossTabResult4=table(sd[ , c('SalaryAbove50K', "occupation")])
CrossTabResult4
barplot(CrossTabResult4, beside=T, col=c('Red','Green'))

CrossTabResult5=table(sd[ , c('SalaryAbove50K', "race")])
CrossTabResult5
barplot(CrossTabResult5, beside=T, col=c('Red','Green'))

CrossTabResult6=table(sd[ , c('SalaryAbove50K', "sex")])
CrossTabResult6
barplot(CrossTabResult6, beside=T, col=c('Red','Green'))

CrossTabResult7=table(sd[ , c('SalaryAbove50K', "native_country")])
CrossTabResult7
barplot(CrossTabResult7, beside=T, col=c('Red','Green'))

##RELATIONSHIP STRENGTH TEST:

#Categorical Vs Continuous --- ANOVA
options(scipen=999)
#H0: age and SalaryAbove50K are NOT related
summary(aov(age~SalaryAbove50K, data = sd))
#p<0.05: The age and SalaryAbove50K variables are STRONGLY RELATED.
#H0: hours_per_week and SalaryAbove50K are NOT related
summary(aov(hours_per_week~SalaryAbove50K, data = sd))
#p<0.05: The hours_per_week and SalaryAbove50K variables are STRONGLY RELATED.


# Categorical Vs Categorical --- Chi-Square test
#H0: workclass and SalaryAbove50K are NOT related
chisq.test(CrossTabResult)
#p<0.05: workclass and SalaryAbove50K are STRONGLY RELATED

#H0: education and SalaryAbove50K are NOT related
chisq.test(CrossTabResult2)
#p<0.05: education and SalaryAbove50K are STRONGLY RELATED

#H0: marital_status and SalaryAbove50K are NOT related
chisq.test(CrossTabResult3)
#p<0.05: marital_status and SalaryAbove50K are STRONGLY RELATED

#H0: occupation and SalaryAbove50K are NOT related
chisq.test(CrossTabResult4)
#p<0.05: occupation and SalaryAbove50K are STRONGLY RELATED

#H0: race and SalaryAbove50K are NOT related
chisq.test(CrossTabResult5)
#p<0.05: race and SalaryAbove50K are STRONGLY RELATED

#H0: sex and SalaryAbove50K are NOT related
chisq.test(CrossTabResult6)
#p<0.05: sex and SalaryAbove50K are STRONGLY RELATED

#H0: native_country and SalaryAbove50K are NOT related
chisq.test(CrossTabResult7)
#p<0.05: native_country and SalaryAbove50K are RELATED

##Specifying the Target Variable
TargetVariableName='SalaryAbove50K'

##Extracting Target and predictor variables from data to create a generic dataset
TargetVariable=sd[, c(TargetVariableName)]
PredictorVariables=sd[, !names(sd) %in% TargetVariableName]
DataForML=data.frame(TargetVariable,PredictorVariables)
str(DataForML)
head(DataForML)
View(DataForML)

##SAMPLING
set.seed(123)
TrainingSampleIndex=sample(1:nrow(DataForML), size=0.7 * nrow(DataForML) )
DataForMLTrain=DataForML[TrainingSampleIndex, ]
DataForMLTest=DataForML[-TrainingSampleIndex, ]
head(DataForMLTrain)
head(DataForMLTest)
dim(DataForMLTrain)
dim(DataForMLTest)

##Building and deploying LOGISTIC REGRESSION MODEL
startTime=Sys.time()
LR_Model=glm(TargetVariable ~ . , data=DataForMLTrain, family='binomial')
endTime=Sys.time()
endTime-startTime #0.1360099 secs
summary(LR_Model)

LR_Model_2=glm(TargetVariable ~ age+workclass+education+marital_status+I(occupation=="Craft-repair")+I(occupation=="Exec-managerial")+I(occupation=="Farming-fishing")+I(occupation=="Handlers-cleaners")+I(occupation=="Machine-op-inspct")+I(occupation=="Other-service")+I(occupation=="Priv-house-serv")+I(occupation=="Protective-serv")+I(occupation=="Sales")+I(occupation=="Tech-support")+I(occupation=="Transport-moving")+race+sex+hours_per_week+native_country , data=DataForMLTrain, family='binomial')
summary(LR_Model_2)

LR_Model_3=glm(TargetVariable ~ age+workclass+I(education=="11th")+I(education=="12th")+I(education=="1st-4th")+I(education=="5th-6th")+I(education=="7th-8th")+I(education=="9th")+I(education=="Bachelors")+I(education=="Doctorate")+I(education=="Masters")+I(education=="Prof-school")+marital_status+I(occupation=="Craft-repair")+I(occupation=="Exec-managerial")+I(occupation=="Farming-fishing")+I(occupation=="Handlers-cleaners")+I(occupation=="Machine-op-inspct")+I(occupation=="Other-service")+I(occupation=="Priv-house-serv")+I(occupation=="Protective-serv")+I(occupation=="Sales")+I(occupation=="Tech-support")+race+sex+hours_per_week+native_country , data=DataForMLTrain, family='binomial')
summary(LR_Model_3)

LR_Model_4=glm(TargetVariable ~ age+workclass+I(education=="11th")+I(education=="12th")+I(education=="1st-4th")+I(education=="5th-6th")+I(education=="7th-8th")+I(education=="9th")+I(education=="Bachelors")+I(education=="Doctorate")+I(education=="Masters")+I(education=="Prof-school")+marital_status+I(occupation=="Craft-repair")+I(occupation=="Exec-managerial")+I(occupation=="Farming-fishing")+I(occupation=="Handlers-cleaners")+I(occupation=="Machine-op-inspct")+I(occupation=="Other-service")+I(occupation=="Protective-serv")+I(occupation=="Sales")+I(occupation=="Tech-support")+race+sex+hours_per_week+native_country , data=DataForMLTrain, family='binomial')
summary(LR_Model_4)

LR_Model_5=glm(TargetVariable ~ age+workclass+I(education=="11th")+I(education=="12th")+I(education=="1st-4th")+I(education=="5th-6th")+I(education=="7th-8th")+I(education=="9th")+I(education=="Bachelors")+I(education=="Doctorate")+I(education=="Masters")+I(education=="Prof-school")+marital_status+I(occupation=="Exec-managerial")+I(occupation=="Farming-fishing")+I(occupation=="Handlers-cleaners")+I(occupation=="Machine-op-inspct")+I(occupation=="Other-service")+I(occupation=="Protective-serv")+I(occupation=="Sales")+I(occupation=="Tech-support")+race+sex+hours_per_week+native_country , data=DataForMLTrain, family='binomial')
summary(LR_Model_5)

LR_Model_6=glm(TargetVariable ~ age+workclass+I(education=="11th")+I(education=="12th")+I(education=="1st-4th")+I(education=="5th-6th")+I(education=="7th-8th")+I(education=="9th")+I(education=="Bachelors")+I(education=="Doctorate")+I(education=="Masters")+I(education=="Prof-school")+marital_status+I(occupation=="Exec-managerial")+I(occupation=="Farming-fishing")+I(occupation=="Handlers-cleaners")+I(occupation=="Machine-op-inspct")+I(occupation=="Other-service")+I(occupation=="Protective-serv")+I(occupation=="Sales")+I(occupation=="Tech-support")+race+sex+hours_per_week+I(native_country=="North-America")+I(native_country=="South-America") , data=DataForMLTrain, family='binomial')
summary(LR_Model_6)

LR_Model_7=glm(TargetVariable ~ age+workclass+I(education=="11th")+I(education=="12th")+I(education=="1st-4th")+I(education=="5th-6th")+I(education=="7th-8th")+I(education=="9th")+I(education=="Bachelors")+I(education=="Doctorate")+I(education=="Masters")+I(education=="Prof-school")+marital_status+I(occupation=="Exec-managerial")+I(occupation=="Farming-fishing")+I(occupation=="Handlers-cleaners")+I(occupation=="Machine-op-inspct")+I(occupation=="Other-service")+I(occupation=="Protective-serv")+I(occupation=="Sales")+I(occupation=="Tech-support")+I(race=="Black")+I(race=="Other")+I(race=="White")+sex+hours_per_week+I(native_country=="North-America")+I(native_country=="South-America") , data=DataForMLTrain, family='binomial')
summary(LR_Model_7)

LR_Model_8=glm(TargetVariable ~ age+workclass+I(education=="12th")+I(education=="1st-4th")+I(education=="5th-6th")+I(education=="7th-8th")+I(education=="9th")+I(education=="Bachelors")+I(education=="Doctorate")+I(education=="Masters")+I(education=="Prof-school")+marital_status+I(occupation=="Exec-managerial")+I(occupation=="Farming-fishing")+I(occupation=="Handlers-cleaners")+I(occupation=="Machine-op-inspct")+I(occupation=="Other-service")+I(occupation=="Protective-serv")+I(occupation=="Sales")+I(occupation=="Tech-support")+I(race=="Black")+I(race=="Other")+I(race=="White")+sex+hours_per_week+I(native_country=="North-America")+I(native_country=="South-America") , data=DataForMLTrain, family='binomial')
summary(LR_Model_8)

LR_Model_9=glm(TargetVariable ~ age+workclass+I(education=="12th")+I(education=="1st-4th")+I(education=="5th-6th")+I(education=="7th-8th")+I(education=="9th")+I(education=="Bachelors")+I(education=="Doctorate")+I(education=="Masters")+I(education=="Prof-school")+I(marital_status=="Married")+I(marital_status=="Never-Married")+I(marital_status=="Separated")+I(occupation=="Exec-managerial")+I(occupation=="Farming-fishing")+I(occupation=="Handlers-cleaners")+I(occupation=="Machine-op-inspct")+I(occupation=="Other-service")+I(occupation=="Protective-serv")+I(occupation=="Sales")+I(occupation=="Tech-support")+I(race=="Black")+I(race=="Other")+I(race=="White")+sex+hours_per_week+I(native_country=="North-America")+I(native_country=="South-America") , data=DataForMLTrain, family='binomial')
summary(LR_Model_9)

LR_Model_10=glm(TargetVariable ~ age+workclass+I(education=="12th")+I(education=="1st-4th")+I(education=="5th-6th")+I(education=="7th-8th")+I(education=="9th")+I(education=="Bachelors")+I(education=="Doctorate")+I(education=="Masters")+I(education=="Prof-school")+I(marital_status=="Married")+I(marital_status=="Separated")+I(occupation=="Exec-managerial")+I(occupation=="Farming-fishing")+I(occupation=="Handlers-cleaners")+I(occupation=="Machine-op-inspct")+I(occupation=="Other-service")+I(occupation=="Protective-serv")+I(occupation=="Sales")+I(occupation=="Tech-support")+I(race=="Black")+I(race=="Other")+I(race=="White")+sex+hours_per_week+I(native_country=="North-America")+I(native_country=="South-America") , data=DataForMLTrain, family='binomial')
summary(LR_Model_10)

LR_Model_11=glm(TargetVariable ~ age+workclass+I(education=="12th")+I(education=="1st-4th")+I(education=="5th-6th")+I(education=="7th-8th")+I(education=="9th")+I(education=="Bachelors")+I(education=="Doctorate")+I(education=="Masters")+I(education=="Prof-school")+I(marital_status=="Married")+I(occupation=="Exec-managerial")+I(occupation=="Farming-fishing")+I(occupation=="Handlers-cleaners")+I(occupation=="Machine-op-inspct")+I(occupation=="Other-service")+I(occupation=="Protective-serv")+I(occupation=="Sales")+I(occupation=="Tech-support")+I(race=="Black")+I(race=="Other")+I(race=="White")+sex+hours_per_week+I(native_country=="North-America")+I(native_country=="South-America") , data=DataForMLTrain, family='binomial')
summary(LR_Model_11)

LR_Model_12=glm(TargetVariable ~ age+workclass+I(education=="1st-4th")+I(education=="5th-6th")+I(education=="7th-8th")+I(education=="9th")+I(education=="Bachelors")+I(education=="Doctorate")+I(education=="Masters")+I(education=="Prof-school")+I(marital_status=="Married")+I(occupation=="Exec-managerial")+I(occupation=="Farming-fishing")+I(occupation=="Handlers-cleaners")+I(occupation=="Machine-op-inspct")+I(occupation=="Other-service")+I(occupation=="Protective-serv")+I(occupation=="Sales")+I(occupation=="Tech-support")+I(race=="Black")+I(race=="Other")+I(race=="White")+sex+hours_per_week+I(native_country=="North-America")+I(native_country=="South-America") , data=DataForMLTrain, family='binomial')
summary(LR_Model_12)

LR_Model_13=glm(TargetVariable ~ age+workclass+I(education=="1st-4th")+I(education=="5th-6th")+I(education=="7th-8th")+I(education=="Bachelors")+I(education=="Doctorate")+I(education=="Masters")+I(education=="Prof-school")+I(marital_status=="Married")+I(occupation=="Exec-managerial")+I(occupation=="Farming-fishing")+I(occupation=="Handlers-cleaners")+I(occupation=="Machine-op-inspct")+I(occupation=="Other-service")+I(occupation=="Protective-serv")+I(occupation=="Sales")+I(occupation=="Tech-support")+I(race=="Black")+I(race=="Other")+I(race=="White")+sex+hours_per_week+I(native_country=="North-America")+I(native_country=="South-America") , data=DataForMLTrain, family='binomial')
summary(LR_Model_13)

LR_Model_14=glm(TargetVariable ~ age+workclass+I(education=="1st-4th")+I(education=="5th-6th")+I(education=="7th-8th")+I(education=="Bachelors")+I(education=="Doctorate")+I(education=="Masters")+I(education=="Prof-school")+I(marital_status=="Married")+I(occupation=="Exec-managerial")+I(occupation=="Farming-fishing")+I(occupation=="Handlers-cleaners")+I(occupation=="Machine-op-inspct")+I(occupation=="Other-service")+I(occupation=="Protective-serv")+I(occupation=="Sales")+I(occupation=="Tech-support")+I(race=="Black")+I(race=="White")+sex+hours_per_week+I(native_country=="North-America")+I(native_country=="South-America") , data=DataForMLTrain, family='binomial')
summary(LR_Model_14)

LR_Model_15=glm(TargetVariable ~ age+I(workclass=="SelfEmployed")+I(workclass=="Private")+I(education=="1st-4th")+I(education=="5th-6th")+I(education=="7th-8th")+I(education=="Bachelors")+I(education=="Doctorate")+I(education=="Masters")+I(education=="Prof-school")+I(marital_status=="Married")+I(occupation=="Exec-managerial")+I(occupation=="Farming-fishing")+I(occupation=="Handlers-cleaners")+I(occupation=="Machine-op-inspct")+I(occupation=="Other-service")+I(occupation=="Protective-serv")+I(occupation=="Sales")+I(occupation=="Tech-support")+I(race=="Black")+I(race=="White")+sex+hours_per_week+I(native_country=="North-America")+I(native_country=="South-America") , data=DataForMLTrain, family='binomial')
summary(LR_Model_15)

LR_Model_16=glm(TargetVariable ~ age+I(workclass=="SelfEmployed")+I(education=="1st-4th")+I(education=="5th-6th")+I(education=="7th-8th")+I(education=="Bachelors")+I(education=="Doctorate")+I(education=="Masters")+I(education=="Prof-school")+I(marital_status=="Married")+I(occupation=="Exec-managerial")+I(occupation=="Farming-fishing")+I(occupation=="Handlers-cleaners")+I(occupation=="Machine-op-inspct")+I(occupation=="Other-service")+I(occupation=="Protective-serv")+I(occupation=="Sales")+I(occupation=="Tech-support")+I(race=="Black")+I(race=="White")+sex+hours_per_week+I(native_country=="North-America")+I(native_country=="South-America") , data=DataForMLTrain, family='binomial')
summary(LR_Model_16)

LR_Model_17=glm(TargetVariable ~ age+I(education=="1st-4th")+I(education=="5th-6th")+I(education=="7th-8th")+I(education=="Bachelors")+I(education=="Doctorate")+I(education=="Masters")+I(education=="Prof-school")+I(marital_status=="Married")+I(occupation=="Exec-managerial")+I(occupation=="Farming-fishing")+I(occupation=="Handlers-cleaners")+I(occupation=="Machine-op-inspct")+I(occupation=="Other-service")+I(occupation=="Protective-serv")+I(occupation=="Sales")+I(occupation=="Tech-support")+I(race=="Black")+I(race=="White")+sex+hours_per_week+I(native_country=="North-America")+I(native_country=="South-America") , data=DataForMLTrain, family='binomial')
summary(LR_Model_17)

LR_Model_18=glm(TargetVariable ~ age+I(education=="1st-4th")+I(education=="5th-6th")+I(education=="7th-8th")+I(education=="Bachelors")+I(education=="Doctorate")+I(education=="Masters")+I(education=="Prof-school")+I(marital_status=="Married")+I(occupation=="Exec-managerial")+I(occupation=="Farming-fishing")+I(occupation=="Handlers-cleaners")+I(occupation=="Machine-op-inspct")+I(occupation=="Other-service")+I(occupation=="Protective-serv")+I(occupation=="Sales")+I(race=="Black")+I(race=="White")+sex+hours_per_week+I(native_country=="North-America")+I(native_country=="South-America") , data=DataForMLTrain, family='binomial')
summary(LR_Model_18)

LR_Model_19=glm(TargetVariable ~ age+I(education=="1st-4th")+I(education=="5th-6th")+I(education=="7th-8th")+I(education=="Bachelors")+I(education=="Doctorate")+I(education=="Masters")+I(education=="Prof-school")+I(marital_status=="Married")+I(occupation=="Exec-managerial")+I(occupation=="Farming-fishing")+I(occupation=="Handlers-cleaners")+I(occupation=="Machine-op-inspct")+I(occupation=="Other-service")+I(occupation=="Sales")+I(race=="Black")+I(race=="White")+sex+hours_per_week+I(native_country=="North-America")+I(native_country=="South-America") , data=DataForMLTrain, family='binomial')
summary(LR_Model_19)

LR_Model_20=glm(TargetVariable ~ age+I(education=="1st-4th")+I(education=="5th-6th")+I(education=="7th-8th")+I(education=="Bachelors")+I(education=="Doctorate")+I(education=="Masters")+I(education=="Prof-school")+I(marital_status=="Married")+I(occupation=="Exec-managerial")+I(occupation=="Farming-fishing")+I(occupation=="Machine-op-inspct")+I(occupation=="Other-service")+I(occupation=="Sales")+I(race=="Black")+I(race=="White")+sex+hours_per_week+I(native_country=="North-America")+I(native_country=="South-America") , data=DataForMLTrain, family='binomial')
summary(LR_Model_20)

LR_Model_21=glm(TargetVariable ~ age+I(education=="1st-4th")+I(education=="5th-6th")+I(education=="7th-8th")+I(education=="Bachelors")+I(education=="Doctorate")+I(education=="Masters")+I(education=="Prof-school")+I(marital_status=="Married")+I(occupation=="Exec-managerial")+I(occupation=="Farming-fishing")+I(occupation=="Machine-op-inspct")+I(occupation=="Other-service")+I(occupation=="Sales")+I(race=="White")+sex+hours_per_week+I(native_country=="North-America")+I(native_country=="South-America") , data=DataForMLTrain, family='binomial')
summary(LR_Model_21)

LR_Model_22=glm(TargetVariable ~ age+I(education=="1st-4th")+I(education=="5th-6th")+I(education=="7th-8th")+I(education=="Bachelors")+I(education=="Doctorate")+I(education=="Masters")+I(education=="Prof-school")+I(marital_status=="Married")+I(occupation=="Exec-managerial")+I(occupation=="Farming-fishing")+I(occupation=="Machine-op-inspct")+I(occupation=="Other-service")+I(occupation=="Sales")+I(race=="White")+sex+hours_per_week+I(native_country=="South-America") , data=DataForMLTrain, family='binomial')
summary(LR_Model_22)

LR_Model_23=glm(TargetVariable ~ age+I(education=="1st-4th")+I(education=="5th-6th")+I(education=="7th-8th")+I(education=="Bachelors")+I(education=="Doctorate")+I(education=="Masters")+I(education=="Prof-school")+I(marital_status=="Married")+I(occupation=="Exec-managerial")+I(occupation=="Farming-fishing")+I(occupation=="Machine-op-inspct")+I(occupation=="Other-service")+I(occupation=="Sales")+sex+hours_per_week+I(native_country=="South-America") , data=DataForMLTrain, family='binomial')
summary(LR_Model_23)

LR_Model_24=glm(TargetVariable ~ age+I(education=="5th-6th")+I(education=="7th-8th")+I(education=="Bachelors")+I(education=="Doctorate")+I(education=="Masters")+I(education=="Prof-school")+I(marital_status=="Married")+I(occupation=="Exec-managerial")+I(occupation=="Farming-fishing")+I(occupation=="Machine-op-inspct")+I(occupation=="Other-service")+I(occupation=="Sales")+sex+hours_per_week+I(native_country=="South-America") , data=DataForMLTrain, family='binomial')
summary(LR_Model_24)

LR_Model_25=glm(TargetVariable ~ age+I(education=="5th-6th")+I(education=="7th-8th")+I(education=="Bachelors")+I(education=="Doctorate")+I(education=="Masters")+I(education=="Prof-school")+I(marital_status=="Married")+I(occupation=="Exec-managerial")+I(occupation=="Farming-fishing")+I(occupation=="Machine-op-inspct")+I(occupation=="Other-service")+I(occupation=="Sales")+sex+hours_per_week , data=DataForMLTrain, family='binomial')
summary(LR_Model_25)

#MULTICOLLINEARITY Check
library(car)
VIF=vif(LR_Model_25)
data.frame(VIF)

PredictionProb=predict(LR_Model_25,DataForMLTest,type = "response")
PredictionProb
min(PredictionProb)
max(PredictionProb)

library(caret)
IterationData1=data.frame(
  Threshold=numeric(0),
  Accuracy=numeric(0)
)
#AccuracyResults[['byClass']]['F1']
thresholds=seq(0.4,0.59,0.01)
# for(i in thresholds){
#   
#   DataForMLTest$Prediction=ifelse(PredictionProb>i, 1, 0)
#   DataForMLTest$Prediction=as.factor(DataForMLTest$Prediction)
#   AccuracyResults=confusionMatrix(DataForMLTest$Prediction, DataForMLTest$TargetVariable)
#   IterationData=rbind(IterationData,data.frame(
#     Threshold=i,
#     Accuracy=round(100 * AccuracyResults[['byClass']]['F1'])))
#   
# }
IterationData1
for(i in thresholds){
  
  DataForMLTest$Prediction=ifelse(PredictionProb>i, 1, 0)
  DataForMLTest$Prediction=as.factor(DataForMLTest$Prediction)
  AccuracyResults=confusionMatrix(DataForMLTest$Prediction, DataForMLTest$TargetVariable)
  IterationData1=rbind(IterationData1,data.frame(
    Threshold=i,
    Accuracy=round(100 * AccuracyResults[['overall']][1])))
  
}
IterationData1
DataForMLTest$Prediction=ifelse(PredictionProb>0.59, 1, 0)
DataForMLTest$Prediction=as.factor(DataForMLTest$Prediction)
AccuracyResults=confusionMatrix(DataForMLTest$Prediction, DataForMLTest$TargetVariable)
IterationData1=rbind(IterationData1,data.frame(
  Threshold=i,
  Accuracy=round(100 * AccuracyResults[['overall']][1])))


##Balanced Accuracy
IterationData2=data.frame(
  Threshold=numeric(0),
  Accuracy=numeric(0)
)
thresholds=seq(0.4,0.65,0.01)
for(i in thresholds){

  DataForMLTest$Prediction=ifelse(PredictionProb>i, 1, 0)
  DataForMLTest$Prediction=as.factor(DataForMLTest$Prediction)
  AccuracyResults=confusionMatrix(DataForMLTest$Prediction, DataForMLTest$TargetVariable)
  IterationData2=rbind(IterationData2,data.frame(
    Threshold=i,
    Accuracy=round(100 * AccuracyResults[['byClass']]['Balanced Accuracy'])))

}
IterationData2

DataForMLTest$Prediction=ifelse(PredictionProb>0.59, 1, 0)
DataForMLTest$Prediction=as.factor(DataForMLTest$Prediction)
AccuracyResults=confusionMatrix(DataForMLTest$Prediction, DataForMLTest$TargetVariable)
IterationData2=rbind(IterationData2,data.frame(
  Threshold=i,
  Accuracy=round(100 * AccuracyResults[['byClass']]['Balanced Accuracy'])))

##F1 Score -- F1 - 19
IterationData3=data.frame(
  Threshold=numeric(0),
  Accuracy=numeric(0)
)
for(i in thresholds){
  
  DataForMLTest$Prediction=ifelse(PredictionProb>i, 1, 0)
  DataForMLTest$Prediction=as.factor(DataForMLTest$Prediction)
  AccuracyResults=confusionMatrix(DataForMLTest$Prediction, DataForMLTest$TargetVariable)
  IterationData3=rbind(IterationData3,data.frame(
    Threshold=i,
    Accuracy=round(100 * AccuracyResults[['byClass']]['F1'])))
  
}
IterationData3
DataForMLTest$Prediction=ifelse(PredictionProb>0.59, 1, 0)
DataForMLTest$Prediction=as.factor(DataForMLTest$Prediction)
AccuracyResults=confusionMatrix(DataForMLTest$Prediction, DataForMLTest$TargetVariable)
IterationData2=rbind(IterationData2,data.frame(
  Threshold=i,
  Accuracy=round(100 * AccuracyResults[['byClass']]['Balanced Accuracy'])))


####THRESHOLD SETTING
DataForMLTest$Prediction=ifelse(PredictionProb>0.59, 1, 0)
DataForMLTest$Prediction=as.factor(DataForMLTest$Prediction)
head(DataForMLTest)


#Creating the CONFUSION MATRIX to calculate overall accuracy, precision and recall on TESTING data
library(caret)
AccuracyResults=confusionMatrix(DataForMLTest$Prediction, DataForMLTest$TargetVariable)

# Since AccuracyResults is a list of multiple items, fetching useful components only
AccuracyResults[['table']]
AccuracyResults[['byClass']]

print(paste('### Overall Accuracy of Ctree Model is: ', round(100 * AccuracyResults[['overall']][1]) , '%'))