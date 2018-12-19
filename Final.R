#Profiling the Data
setwd("F:\\padhai\\SimpliLearn\\Data Science With R\\Project\\Projects for Submission\\Healthcare\\Healthcare")
df<- read.csv(file="HospitalCosts.csv", sep=",")
View(df)
head(x=df,n=10)


#Exploratory Data Analysis
#install.packages("DataExplorer")
library(DataExplorer)
plot_str(df)# plot the dimension and type of each variable
plot_missing(df)# plot the column with with and missing and how many missing values are there
sum(is.na(df$RACE))
head(df$RACE, n=50)
table(df$RACE)
# this will replace the missing values with the mode or the most frequent value
df$RACE= ifelse(test=is.na(df$RACE), yes=1, no= df$RACE)
sum(is.na(df$RACE))
plot_missing(df)
#Continuous Variables
plot_histogram(df[,-c(2,4)])
plot_density(df[,-c(2,4)])
#Discrete Variable
df$FEMALE<- as.factor(df$FEMALE)
df$RACE<- as.factor(df$RACE)
plot_bar(df)
#Multivariative Analysis
plot_correlation(df, type='continuous')

#Goals of Project


###To record the patient statistics, the agency wants to find the age category of people who frequent the hospital and has the maximum expenditure
library(ggplot2)
b1<- ggplot(df, aes(AGE, fill = cut(AGE, 30))) +
  geom_histogram(show.legend = FALSE) +
  scale_fill_discrete(h = c(240, 10))+
  labs(x="Age Groups",y="Frequency",title="Frequency plot or Histogram on Age")
df1<- aggregate(TOTCHG ~ AGE, data = df, sum)
b2<- ggplot(data=df, aes(x=AGE, y=TOTCHG, fill=AGE)) +
  geom_bar(stat="identity", show.legend = FALSE)+
  labs(x="Age Groups",y="Expenditure",title="Total Expenditure by Age")
theme_minimal()
#install.packages("cowplot") to plot multiple plots in a single line
library(cowplot)
plot_grid(b1, b2) # function from cowplot library

# now let's create 3 age groups out of the ages column and then do the analysis
df$AGE_CATE<-cut(df$AGE, seq(0,18,6), right=FALSE, 
                 labels=c("0-6", "7-12", "13-18"))# creating age categories with 3 cuts
freq<-table(df$AGE_CATE)# calculating frequency of each category
df1<- aggregate(TOTCHG ~ AGE_CATE, data = df, sum) # with aggregate function calculating total expenditure
df1$freq<- freq
p1<-ggplot(data=df1, aes(x=AGE_CATE, y=freq)) +
  geom_bar(stat="identity", fill="steelblue") + theme_classic() +
  labs(y="Frequency", x= "Age categories")
p2<-ggplot(data=df1, aes(x=AGE_CATE, y=TOTCHG)) +
  geom_bar(stat="identity", fill="steelblue") + theme_classic() +
  labs(y="Total Expenditure", x= "Age categories")
#install.packages("cowplot") to plot multiple plots in a single line
library(cowplot)
plot_grid(p1, p2, labels = c("Frequency","Expenditure"), label_size = 10, 
          label_x = 0.4, label_colour = "grey") # function from cowplot library


###In order of severity of the diagnosis and treatments and to find out the expensive treatments, the agency wants to find the diagnosis related group that has maximum hospitalization and expenditure.
table(df$APRDRG)
freq1<-as.data.frame(table(df$APRDRG))
freq1$Var1[which.max(freq1$Freq)]# Which group is most frequent to hospital
freq2<- aggregate(TOTCHG ~ APRDRG, data = df, sum)
head(freq2)
freq2$APRDRG[which.max(freq2$TOTCHG)]# which group has the highest expenditure


###To make sure that there is no malpractice, the agency needs to analyze if the race of the patient is related to the hospitalization costs.
head(df$RACE)
summary(df$RACE)
ggplot(df, aes(x = RACE, y = TOTCHG)) +
  geom_boxplot(fill = "grey80", colour = "blue") +
  scale_x_discrete() + xlab("Race of patient") +
  ylab("Hospitalisation Costs")
model<- lm(TOTCHG~RACE, data = df)
summary(model)
anova(model)
confint(model)
mod = data.frame(Fitted = fitted(model),
                 Residuals = resid(model), Race = df$RACE)
ggplot(mod, aes(Fitted, Residuals, colour = Race)) + geom_point()



###To properly utilize the costs, the agency has to analyze the severity of the hospital costs by age and gender for proper allocation of resources.
lin_Mod<- lm(TOTCHG~AGE+FEMALE, data= df)
summary(lin_Mod)



###Since the length of stay is the crucial factor for inpatients, the agency wants to find if the length of stay can be predicted from age, gender, and race.
lin_Mod1<- lm(LOS~AGE+FEMALE+RACE, data= df)
summary(lin_Mod)


###To perform a complete analysis, the agency wants to find the variable that mainly affects the hospital costs.
lin_Mod3<- lm(TOTCHG~., data= df)
summary(lin_Mod3)