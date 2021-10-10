# Term-Deposit-Subscript
library(readxl)
library(corrplot)
library(ggplot2)
library(lattice)
library(caret)
library(plyr)
library(dplyr)
library(car)
library(pROC)
library(VIM)
library(MASS)
library(readxl)
Bank_Customer <- read_excel("C:/Users/teh_a/Downloads/Bank Customer.xlsx")
view(Bank_Customer)

#The str() function is used to explore the dataset.
str(Bank_Customer)

# The sapply() function is used to check for missing values in the data set.
sapply(Bank_Customer, function(x) sum(is.na(x)))

# The column "y" is renamed to "termdeposit" to make the charts and data more readable and comparable.
names(Bank_Customer)[names(Bank_Customer)=="y"] <-"termdeposit"

#The columns emp.var.rate, euribor3m, cons.price.IDX, cons.conf.idx, and nr.employed are dropped 
Bank_Customer <- Bank_Customer[,-c(16:20)]

# To remove the extra space
Bank_Customer %>%
  mutate_all(trimws)

# To check for case errors and categorical levels
unique(Bank_Customer$job)
unique(Bank_Customer$marital)
unique(Bank_Customer$education)
unique(Bank_Customer$default)
unique(Bank_Customer$housing)
unique(Bank_Customer$loan)
unique(Bank_Customer$contact)
unique(Bank_Customer$month)
unique(Bank_Customer$day_of_week)
unique(Bank_Customer$termdeposit)

# Copy the dataset and repalcing unknown with NA
dataunknown<-Bank_Customer[complete.cases(Bank_Customer),]
dataunknown[dataunknown=='unknown'] <- NA

# To further investigate the categoriacal level unknown
aggr_plot <- aggr(dataunknown, col = c('blue', 'red'), 
                  numbers = TRUE, sortVars = TRUE,
                  labels = names(dataunknown))
# Data split
set.seed(1)
sample_size <- floor(0.7 * nrow(Bank_Customer))
separate <- sample(seq_len(nrow(Bank_Customer)), size = sample_size)
train <- Bank_Customer[separate,]
Valid <- Bank_Customer[-separate,]


# Explorartory Analysis
# Termdeposit
ggplot(train, aes(x= termdeposit, fill= termdeposit))+
  ggtitle("Percentage of Term deposit") + xlab("Term deposit") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.3) + ylab("Percentage")

train%>%
  group_by(termdeposit) %>%
  summarise(n=n())

#Job Types
ggplot(train, aes(x= job, fill= job))+
  ggtitle("Percentage of Customers based on Job Types") + xlab("Job Types") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.3) + ylab("Percentage")

ggplot(train, aes(x= job, fill= termdeposit))+
  ggtitle("Job Types Vs Term Deposit") + xlab("Job Types") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.3, position = "dodge") + ylab("Percentage")

train%>%
  group_by(job, termdeposit) %>%
  summarise(n=n())
View(job)

# Marital Status
ggplot(train, aes(x= marital, fill= marital))+
  ggtitle("Percentage of Customers based on Marital Status") + xlab("Marital Status") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.3) + ylab("Percentage")

ggplot(train, aes(x= marital, fill= termdeposit))+
  ggtitle("Marital Status Vs Term Deposit") + xlab("Marital Status") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.3, position = "dodge") + ylab("Percentage")

train%>%
  group_by(marital, termdeposit) %>%
  summarise(n=n())

# Education
ggplot(train, aes(x= education, fill= education))+
  ggtitle("Percentage of Customers based Education") + xlab("Education") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.3) + ylab("Percentage")

ggplot(train, aes(x= marital, fill= termdeposit))+
  ggtitle("Educaton Vs Term Deposit") + xlab("Education") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.3, position = "dodge") + ylab("Percentage")


train%>%
  group_by(education, termdeposit) %>%
  summarise(n=n())

# Default
ggplot(train, aes(x= default, fill= default))+
  ggtitle("Percentage of Customers based on Loan Default") + xlab("Loan Default") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.3) + ylab("Percentage")

ggplot(train, aes(x= default, fill= termdeposit))+
  ggtitle("Loan Default Vs Term Deposit") + xlab("Loan Default") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.3, position = "dodge") + ylab("Percentage")

train%>%
  group_by(default, termdeposit) %>%
  summarise(n=n())

# Housing
ggplot(train, aes(x= housing, fill= housing))+
  ggtitle("Percentage of Customers based on Housing") + xlab("Housing") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.3) + ylab("Percentage")

ggplot(train, aes(x= housing, fill= termdeposit))+
  ggtitle("Housing Vs Term Deposit") + xlab("Housing") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.3, position = "dodge") + ylab("Percentage")

train%>%
  group_by(housing, termdeposit) %>%
  summarise(n=n())

# Loan
ggplot(train, aes(x= loan, fill= loan))+
  ggtitle("Percentage of Customers based on Loan") + xlab("Loan") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.3) + ylab("Percentage")

ggplot(train, aes(x= loan, fill= termdeposit))+
  ggtitle("Loan Vs Term Deposit") + xlab("Loan") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.3, position = "dodge") + ylab("Percentage")

train%>%
  group_by(loan, termdeposit) %>%
  summarise(n=n())

# Contact
ggplot(train, aes(x= contact, fill= contact))+
  ggtitle("Percentage of Customers based on Contact Type") + xlab("Contact Type") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.3) + ylab("Percentage")

ggplot(train, aes(x= contact, fill= termdeposit))+
  ggtitle("Contact Type Vs Term Deposit") + xlab("Contact Type") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.3, position = "dodge") + ylab("Percentage")

train%>%
  group_by(contact, termdeposit) %>%
  summarise(n=n())

# Month
ggplot(train, aes(x= month, fill= month))+
  ggtitle("Percentage of Customers based on Month") + xlab("Month") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.3) + ylab("Percentage")

ggplot(train, aes(x= month, fill= termdeposit))+
  ggtitle("Month Vs Term Deposit") + xlab("Month") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.3, position = "dodge") + ylab("Percentage")

train%>%
  group_by(month, termdeposit) %>%
  summarise(n=n())

# Day
ggplot(train, aes(x= day_of_week, fill= day_of_week))+
  ggtitle("Percentage of Customers based on Day") + xlab("Day") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.3) + ylab("Percentage")

ggplot(train, aes(x= day_of_week, fill= termdeposit))+
  ggtitle("Day Vs Term Deposit") + xlab("Day") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.3, position="dodge") + ylab("Percentage")

train%>%
  group_by(day_of_week, termdeposit) %>%
  summarise(n=n())

# Exploring Numerical Variables

Corr<-cor(train[sapply(train, is.numeric)])
corrplot(Corr, method = "number", mar=c(1,1,1,1))



# Dropping pdays because pdays and previous are highly correlated and basically show the same things
dropp <- c("pdays")
train <- train[,!(names(train) %in% dropp)]
rm(dropp)

# Base/intercept model

InterceptModel <- glm(as.factor(termdeposit) ~1, family = binomial(link ="logit"),
                      data=train)
print(summary(InterceptModel))

#Full Model
FullModel <- glm(as.factor(termdeposit) ~., family = binomial(link ="logit"),
                 data=train)

# Final Model
FinalModel <- stepAIC(FullModel, confsetsize=1)
print(summary(FinalModel))

# Drop pdays in valid
dropp <- c("pdays")
Valid <- Valid[,!(names(Valid) %in% dropp)]
rm(dropp)

# Check Accuracy and ROC

termdepositpredict <- predict(FinalModel, newdata=Valid, type = "response")

roc.plot <- plot.roc(Valid$termdeposit, termdepositpredict,
                 identity.col="black",
                 print.auc=TRUE, auc.polygon = TRUE,
                 main = "ROC Curve for Logistic Regression Term Deposit", xlab = "Specificit(1-False Positive Rate)",
                 ylab= "True Positive Rate", col = 6, col.main=8, col.lab = 4, font.axis=4,
                 font.lab=4, font.main=4)
termdepositpredict <- predict(FinalModel, newdata=valid, type = "respone")
Valid$termdeposit <-as.factor(Valid$termdeposit)
termdepositpredict <- ifelse(termdepositpredict >0.5, "yes", "no")
termdepositpredict <- as.factor(termdepositpredict)
confusionMatrix((termdepositpredict), Valid$termdeposit, positive= 'yes')
