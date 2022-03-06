
##### LOADING LIBRARIES #####
library(tidyverse)
library(dplyr)
library(ggplot2)
library(caTools)

#Get and print current working directory.
print(getwd())

#load the dataset from csv
data_heart <- read.csv("heart.csv")
head(data_heart)
tail(data_heart)

#get rough idea about dataset
glimpse(data_heart)

cat("No. of Columns = " , ncol(data_heart))
cat("No. of Rows = " , nrow(data_heart))

print(colnames(data_heart))

summary(data_heart)










##### DATA TRANSFORMATION #####

#change name of age column
names(data_heart)[1] <- "age"
colnames(data_heart)

df <- data_heart %>%
  mutate(sex = if_else(sex==1, "MALE", "FEMALE"),
         fbs = if_else(fbs==1, ">120", "<=120"),
         exang = if_else(exang==1, "YES", "NO"),
         cp = if_else(cp==1, "ATYPICAL ANGINA",
                      if_else(cp==2, "NON-ANGINAL PAIN", "ASYMPTOMATIC")),
         restecg = if_else(restecg==0, "NORMAL",
                           if_else(restecg==1, "ABNORMAL", "PROBABLE OR DEFINITE")),
         slope = as.factor(slope),
         ca = as.factor(ca),
         thal = as.factor(thal),
         target = if_else(target==1, "YES", "NO")
         ) %>%
  mutate_if(is.character, as.factor) %>%
  dplyr::select(target, sex, fbs, exang, cp, restecg, slope, ca, thal, everything())

#check modified dataset
print(df)








##### DATA VISUALIZATION #####

#getting frequency table of target
table(df$target)


#Barplot of target (heart disease)

library(ggplot2)

ggplot(df, aes(x=df$target, fill=df$target))+
  geom_bar()+
  xlab("heart Disease")+
  ylab("Count")+
  ggtitle("Presence & Absence of Heart Disease")+
  scale_fill_discrete(name="Heart Disease", labels=c("Absence", "Presence"))


#count frequency of target vs age

df %>%
  group_by(age) %>%
  count() %>%
  filter(n>10) %>% #these people are at higher risk
  ggplot()+
  geom_col(aes(age, n), fill="green")+
  ggtitle("Age Analysis")+
  xlab("Age")+
  ylab("Age count")



#compare blood pressure with chest pain wrt sex

ggplot(df, aes(x=sex, y=trestbps))+
  geom_boxplot(fill="purple")+
  xlab("Sex")+
  ylab("BP")+
  facet_grid(~cp)


#compare cholestrol with chest pain wrt sex

ggplot(df, aes(x=sex, y=chol))+
  geom_boxplot(fill="purple")+
  xlab("Sex")+
  ylab("Cholestrol")+
  facet_grid(~cp)




##### CORRELATION #####

library(corrplot)

cor1 <- cor(df[,10:14])
print(cor1)

#plot the correlation
corrplot(cor1, method = "square", type = "upper")






##### LOGISTIC REGRESSION #####


#Creating the model using logistic LR
Model = glm(target ~ age + sex + thalach, data = df, 
            family = "binomial")

#extract the summary of the model
summary(Model)

#get probability of predictions
probability = predict(Model, df, type = "response")

#decision rule definition
df$pred = if_else(probability>=0.5, "Higher risk of HD", 
                  "Lower risk of HD")

#sample data to test Model
sample = data.frame(age=55, sex="MALE", thalach=150)

#prediction for sample data
p_new = predict(Model, sample, type = "response")
p_new

#check the dataset for predictions made by the Model
df



###### PREDICTION VISUALIZATIONS #####

ggplot(df, aes(pred, thalach)) +
  geom_boxplot(fill="pink") +
  ggtitle("Predictions wrt Thalmium test for Males and Females")+
  xlab("Model Predictions")+
  ylab("Thalmium Test Value")+
  facet_grid(~sex)

#we can replace thalach with age 

ggplot(df, aes(pred, age)) +
  geom_boxplot(fill="yellow") +
  ggtitle("Predictions wrt Age of Males and Females")+
  xlab("Model Predictions")+
  ylab("Patient Age")+
  facet_grid(~sex)



###### EVALUATING THE MODEL #####

library(Metrics)

#modifying target vaiable as per pred values
df$target = if_else(df$target=="YES", "Higher risk of HD", "Lower risk of HD")

#calculating Accuracy, Classification error
acc = accuracy(df$target, df$pred)
class_err = ce(df$target, df$pred)

print(paste("Accuracy = ",acc))
print(paste("Classification error = ",class_err))



#Confusion Matrix
table(df$target, df$pred, dnn = c("True Status", "Predicted Status"))

