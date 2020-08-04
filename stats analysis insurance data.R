
# Importing packages
library(tidyverse) 
library(car) 
library(zoo)
library(lmtest) 
library(dplyr) 
library(stringr)
library(caret)
library(ggplot2) 
library(timeDate)
library(varhandle)
library(gridExtra)
#install.packages("recipes")
#install.packages("timeDate")

Insurance <- read.csv("E:/neha/studies/trent study material/extra projects/kaggle/Insurance dataset statistical analysis for CLV/insurance_data.csv")

head(Insurance)

#Cleaning and Manipulating

Insurance <- Insurance[,-c(1)]
colnames(Insurance)
colnames(Insurance) <- str_replace_all(colnames(Insurance),"[.]","_")
colnames(Insurance)

dim(Insurance)
str(Insurance)

na_counts <- sapply(Insurance, function(y) sum(is.na(y)))
na_counts <- data.frame(na_counts)
na_counts

#Find Unique values
unique_values <- lapply(Insurance, unique)
count_unique <- lengths(unique_values)
View(count_unique)

#Exploratory Analysis

#For Customer lifetime value

summary(Insurance$Customer_Lifetime_Value)
sd(Insurance$Customer_Lifetime_Value)
histogram1<-ggplot(Insurance, aes(x=Customer_Lifetime_Value)) + 
  geom_histogram(color="Black", fill="Yellow") + 
  geom_vline(aes(xintercept=mean(Customer_Lifetime_Value)),
             color="blue", linetype="dashed", size=1) +
  ggtitle("Histogram of Customer Lifetime Value") +
  theme(plot.title = element_text(hjust = 0.5))
histogram1

#Monthly Premium Auto

summary(Insurance$Monthly_Premium_Auto)
sd(Insurance$Monthly_Premium_Auto)
histogram2<-ggplot(Insurance, aes(x=Monthly_Premium_Auto)) + 
  geom_histogram(color="Black", fill="Yellow") + 
  geom_vline(aes(xintercept=mean(Monthly_Premium_Auto)),
             color="blue", linetype="dashed", size=1) +
  ggtitle("Histogram of Monthly Premium Auto") +
  theme(plot.title = element_text(hjust = 0.5))
histogram2

ggplot(Insurance, aes(x = Monthly_Premium_Auto, y = Customer_Lifetime_Value)) +
  geom_point(color="Orange") +
  stat_smooth(method = "lm",
              col = "#C42126",
              se = FALSE,
              size = 1) +
  ggtitle("Scatterplot of CLV Vs MPA") +
  theme(plot.title = element_text(hjust = 0.5))

# Total Claim Amount

summary(Insurance$Total_Claim_Amount)
sd(Insurance$Total_Claim_Amount)
histogram3<-ggplot(Insurance, aes(x=Total_Claim_Amount)) + 
  geom_histogram(color="Black", fill="Yellow") + 
  geom_vline(aes(xintercept=mean(Total_Claim_Amount)),
             color="blue", linetype="dashed", size=1) +
  ggtitle("Histogram of Total Claim Amount") +
  theme(plot.title = element_text(hjust = 0.5))
histogram3

ggplot(Insurance, aes(x = Total_Claim_Amount, y = Customer_Lifetime_Value)) +
  geom_point(color="Orange") +
  stat_smooth(method = "lm",
              col = "#C42126",
              se = FALSE,
              size = 1) +
  ggtitle("Scatterplot of CLV Vs TCA") +
  theme(plot.title = element_text(hjust = 0.5))

#Income

ggplot(Insurance, aes(x = Income, y = Customer_Lifetime_Value)) +
  geom_point(color="Orange") +
  stat_smooth(method = "lm",
              col = "#C42126",
              se = FALSE,
              size = 1) +
  ggtitle("Scatterplot of CLV Vs Income") +
  theme(plot.title = element_text(hjust = 0.5))

#Months since last claim

ggplot(Insurance, aes(x = Months_Since_Last_Claim, y = Customer_Lifetime_Value)) +
  geom_point(color="Orange") +
  stat_smooth(method = "lm",
              col = "#C42126",
              se = FALSE,
              size = 1) +
  ggtitle("Scatterplot of CLV Vs Months Since Last Claim") +
  theme(plot.title = element_text(hjust = 0.5))

#Months since Policy Inception

ggplot(Insurance, aes(x = Months_Since_Policy_Inception, y = Customer_Lifetime_Value)) +
  geom_point(color="Orange") +
  stat_smooth(method = "lm",
              col = "#C42126",
              se = FALSE,
              size = 1) +
  ggtitle("Scatterplot of CLV Vs Months Since Policy Inception") +
  theme(plot.title = element_text(hjust = 0.5))

#Number of Open Complaints

ggplot(Insurance, aes(x = Number_of_Open_Complaints, y = Customer_Lifetime_Value)) +
  geom_point(color="Orange") +
  stat_smooth(method = "lm",
              col = "#C42126",
              se = FALSE,
              size = 1) +
  ggtitle("Scatterplot of CLV Vs Number of Open Complaints") +
  theme(plot.title = element_text(hjust = 0.5))

#Number of Policies

ggplot(Insurance, aes(x = Number_of_Policies, y = Customer_Lifetime_Value)) +
  geom_point(color="Orange") +
  stat_smooth(method = "lm",
              col = "#C42126",
              se = FALSE,
              size = 1) +
  ggtitle("Scatterplot of CLV Vs Number of Policies") +
  theme(plot.title = element_text(hjust = 0.5))

#Correlations of all columns with Customer lifetime Value


cor(Insurance$Income, Insurance$Customer_Lifetime_Value)
cor(Insurance$Monthly_Premium_Auto, Insurance$Customer_Lifetime_Value)
cor(Insurance$Months_Since_Last_Claim, Insurance$Customer_Lifetime_Value)
cor(Insurance$Months_Since_Policy_Inception, Insurance$Customer_Lifetime_Value)
cor(Insurance$Total_Claim_Amount, Insurance$Customer_Lifetime_Value)
cor(Insurance$Number_of_Open_Complaints, Insurance$Customer_Lifetime_Value)

#Inferential Statistics

#Bar Plots

aggData <- aggregate(x = Insurance$Customer_Lifetime_Value, by=list(Coverage = Insurance$Coverage), FUN = sum)
aggData
p1 <- ggplot(data = aggData, aes(x = Coverage, y = prop.table(stat(aggData$x)), fill = Coverage, label = scales::percent(prop.table(stat(aggData$x))))) +
  geom_bar(stat="identity", position = "dodge") + 
  geom_text(stat = 'identity', position = position_dodge(.9),  vjust = -0.5, size = 3) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x = 'Coverage', y = 'CLV in Percentage', fill = 'Coverage') + 
  ggtitle("CLV Distribution by Coverage")


aggData <- aggregate(x = Insurance$Customer_Lifetime_Value, by=list(Education = Insurance$Education), FUN = sum)
p2 <- ggplot(data = aggData, aes(x = Education, y = prop.table(stat(aggData$x)), fill = Education, label = scales::percent(prop.table(stat(aggData$x))))) +
  geom_bar(stat="identity", position = "dodge") + 
  geom_text(stat = 'identity', position = position_dodge(.9),  vjust = -0.5, size = 3) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x = 'Education', y = 'CLV in Percentage', fill = 'Education') + 
  ggtitle("CLV Distribution by Education")

aggData <- aggregate(x = Insurance$Customer_Lifetime_Value, by=list(Employment_Status = Insurance$EmploymentStatus), FUN = sum)
p3 <- ggplot(data = aggData, aes(x = Employment_Status, y = prop.table(stat(aggData$x)), fill = Employment_Status, label = scales::percent(prop.table(stat(aggData$x))))) +
  geom_bar(stat="identity", position = "dodge") + 
  geom_text(stat = 'identity', position = position_dodge(.9),  vjust = -0.5, size = 3) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x = 'Education', y = 'CLV in Percentage', fill = 'Employment_Status') + 
  ggtitle("CLV Distribution by Employment Status")

aggData <- aggregate(x = Insurance$Customer_Lifetime_Value, by=list(Gender = Insurance$Gender), FUN = sum)
p13 <- ggplot(data = aggData, aes(x = Gender, y = prop.table(stat(aggData$x)), fill = Gender, label = scales::percent(prop.table(stat(aggData$x))))) +
  geom_bar(stat="identity", position = "dodge") + 
  geom_text(stat = 'identity', position = position_dodge(.9),  vjust = -0.5, size = 3) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x = 'Education', y = 'CLV in Percentage', fill = 'Gender') + 
  ggtitle("CLV Distribution by Gender")

aggData <- aggregate(x = Insurance$Customer_Lifetime_Value, by=list(Location = Insurance$Location_Code), FUN = sum)
p4 <- ggplot(data = aggData, aes(x = Location, y = prop.table(stat(aggData$x)), fill = Location, label = scales::percent(prop.table(stat(aggData$x))))) +
  geom_bar(stat="identity", position = "dodge") + 
  geom_text(stat = 'identity', position = position_dodge(.9),  vjust = -0.5, size = 3) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x = 'Education', y = 'CLV in Percentage', fill = 'Location') + 
  ggtitle("CLV Distribution by Location")

aggData <- aggregate(x = Insurance$Customer_Lifetime_Value, by=list(Marital_Status = Insurance$Marital_Status), FUN = sum)
p5 <- ggplot(data = aggData, aes(x = Marital_Status, y = prop.table(stat(aggData$x)), fill = Marital_Status, label = scales::percent(prop.table(stat(aggData$x))))) +
  geom_bar(stat="identity", position = "dodge") + 
  geom_text(stat = 'identity', position = position_dodge(.9),  vjust = -0.5, size = 3) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x = 'Education', y = 'CLV in Percentage', fill = 'Marital Status') + 
  ggtitle("CLV Distribution by Marital Status")

aggData <- aggregate(x = Insurance$Customer_Lifetime_Value, by=list(Policy_Type = Insurance$Policy_Type), FUN = sum)
p6 <- ggplot(data = aggData, aes(x = Policy_Type, y = prop.table(stat(aggData$x)), fill = Policy_Type, label = scales::percent(prop.table(stat(aggData$x))))) +
  geom_bar(stat="identity", position = "dodge") + 
  geom_text(stat = 'identity', position = position_dodge(.9),  vjust = -0.5, size = 3) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x = 'Policy_Type', y = 'CLV in Percentage', fill = 'Policy Type') + 
  ggtitle("CLV Distribution by Policy Type")

aggData <- aggregate(x = Insurance$Customer_Lifetime_Value, by=list(Policy_Type = Insurance$Policy_Type), FUN = sum)
p7 <- ggplot(data = aggData, aes(x = Policy_Type, y = prop.table(stat(aggData$x)), fill = Policy_Type, label = scales::percent(prop.table(stat(aggData$x))))) +
  geom_bar(stat="identity", position = "dodge") + 
  geom_text(stat = 'identity', position = position_dodge(.9),  vjust = -0.5, size = 3) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x = 'Policy_Type', y = 'CLV in Percentage', fill = 'Policy Type') + 
  ggtitle("CLV Distribution by Policy Type")

aggData <- aggregate(x = Insurance$Customer_Lifetime_Value, by=list(Renew_Offer_Type = Insurance$Renew_Offer_Type), FUN = sum)
p8 <- ggplot(data = aggData, aes(x = Renew_Offer_Type, y = prop.table(stat(aggData$x)), fill = Renew_Offer_Type, label = scales::percent(prop.table(stat(aggData$x))))) +
  geom_bar(stat="identity", position = "dodge") + 
  geom_text(stat = 'identity', position = position_dodge(.9),  vjust = -0.5, size = 3) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x = 'Renew_Offer_Type', y = 'CLV in Percentage', fill = 'Renew_Offer_Type') + 
  ggtitle("CLV Distribution by Renew Offer Type")

aggData <- aggregate(x = Insurance$Customer_Lifetime_Value, by=list(Sales_Channel = Insurance$Sales_Channel), FUN = sum)
p9 <- ggplot(data = aggData, aes(x = Sales_Channel, y = prop.table(stat(aggData$x)), fill = Sales_Channel, label = scales::percent(prop.table(stat(aggData$x))))) +
  geom_bar(stat="identity", position = "dodge") + 
  geom_text(stat = 'identity', position = position_dodge(.9),  vjust = -0.5, size = 3) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x = 'Sales_Channel', y = 'CLV in Percentage', fill = 'Sales_Channel') + 
  ggtitle("CLV Distribution by Sales_Channel")

aggData <- aggregate(x = Insurance$Customer_Lifetime_Value, by=list(Vehicle_Class = Insurance$Vehicle_Class), FUN = sum)
p10 <- ggplot(data = aggData, aes(x = Vehicle_Class, y = prop.table(stat(aggData$x)), fill = Vehicle_Class, label = scales::percent(prop.table(stat(aggData$x))))) +
  geom_bar(stat="identity", position = "dodge") + 
  geom_text(stat = 'identity', position = position_dodge(.9),  vjust = -0.5, size = 3) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x = 'Vehicle_Class', y = 'CLV in Percentage', fill = 'Vehicle_Class') + 
  ggtitle("CLV Distribution by Vehicle Class")

aggData <- aggregate(x = Insurance$Customer_Lifetime_Value, by=list(State = Insurance$State), FUN = sum)
p11 <- ggplot(data = aggData, aes(x = State, y = prop.table(stat(aggData$x)), fill = State, label = scales::percent(prop.table(stat(aggData$x))))) +
  geom_bar(stat="identity", position = "dodge") + 
  geom_text(stat = 'identity', position = position_dodge(.9),  vjust = -0.5, size = 3) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x = 'State', y = 'CLV in Percentage', fill = 'State') + 
  ggtitle("CLV Distribution by State")

aggData <- aggregate(x = Insurance$Customer_Lifetime_Value, by=list(Policy = Insurance$Policy), FUN = sum)
p12 <- ggplot(data = aggData, aes(x = Policy, y = prop.table(stat(aggData$x)), fill = Policy, label = scales::percent(prop.table(stat(aggData$x))))) +
  geom_bar(stat="identity", position = "dodge") + 
  geom_text(stat = 'identity', position = position_dodge(.9),  vjust = -0.5, size = 3) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x = 'Policy', y = 'CLV in Percentage', fill = 'Policy') + 
  ggtitle("CLV Distribution by Policy")

#grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12, p13, nrow=3)

#Regression Analysis

Continous_data <- dplyr::select_if(Insurance, ~!is.factor(.))
str(Continous_data)

trainIndex <- createDataPartition(Continous_data$Customer_Lifetime_Value, p=0.70, list = FALSE)
#print(trainIndex)
# 70% Train dataset for regression analysis
insuranceTrain <- Continous_data[trainIndex,]
# Remaining 30% Test dataset for testing
insuranceTest <- Continous_data[-trainIndex,]

dim(Continous_data)
dim(insuranceTest)
dim(insuranceTrain)

#Linear Regression analysis using all continous variables

regression1 <- lm(insuranceTrain$Customer_Lifetime_Value ~., data = insuranceTrain) 
summary(regression1)

#Linear Regression with only significant variables

regression2 <- lm(insuranceTrain$Customer_Lifetime_Value ~ 
                Monthly_Premium_Auto + Number_of_Open_Complaints + Number_of_Policies + Total_Claim_Amount, 
              data = insuranceTrain) 
summary(regression2)

predicted_CLV <- predict(regression2)  
#print predicted CLV.
print(predicted_CLV[1:10])

#print actual CLV to compare it with above calculated predicted CLV.
print(insuranceTrain$Customer_Lifetime_Value[1:10])

residualsCLV <- residuals(regression2)
print(residualsCLV[1:10])

predicatedTestData=predict(regression2,insuranceTest)
print(predicatedTestData[1:10])

InsuranceTrainData <- cbind(insuranceTrain,predicted_CLV,residualsCLV)
head(InsuranceTrainData)

ErrorRate <- abs((InsuranceTrainData$Customer_Lifetime_Value - InsuranceTrainData$predicted_CLV)/(InsuranceTrainData$Customer_Lifetime_Value)*100)
print(ErrorRate[1:10])

InsuranceTrainData <- cbind(InsuranceTrainData, ErrorRate)
head(InsuranceTrainData)


hist(ErrorRate, col = "Red")
boxplot(ErrorRate)

shapiro.test(residualsCLV[0:5000])
plot(regression2, which=1, col=c("blue"))

ggplot(InsuranceTrainData, aes(x = Monthly_Premium_Auto, y = Customer_Lifetime_Value)) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +     # regression line  
  geom_segment(aes(xend = Monthly_Premium_Auto, yend = predicted_CLV), alpha = .2) +      # draw line from point to line
  geom_point(aes(color = abs(residualsCLV), size = abs(residualsCLV))) +  # size of the points
  scale_color_continuous(low = "green", high = "red") +             # colour of the points mapped to residual size - green smaller, red larger
  guides(color = FALSE, size = FALSE) +                             # Size legend removed
  geom_point(aes(y = predicted_CLV), shape = 1) +
  theme_bw()

ggplot(InsuranceTrainData,aes(x=Monthly_Premium_Auto,y=Customer_Lifetime_Value))+
  geom_point(color="red")+
  stat_smooth(method="lm")+
  scale_x_continuous(name="Monthly Premium")+
  scale_y_continuous(name="Prediction of CLV")+
  ggtitle("Prediction Curve with Monthly Premium")

ggplot(InsuranceTrainData,aes(x=Total_Claim_Amount,y=Customer_Lifetime_Value))+
  geom_point(color="red")+
  stat_smooth(method="lm")+
  scale_x_continuous(name="Total Claim Amount")+
  scale_y_continuous(name="Prediction of CLV")+
  ggtitle("Prediction Curve with Total Claim Amount")
