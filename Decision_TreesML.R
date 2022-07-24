#Author DataFlair
library(rpart)
library(readr)
library(caTools)
library(dplyr)
library(party)
library(partykit)
library(rpart.plot)
library(datasets)
library(caTools)
library(party)
library(dplyr)
library(magrittr)


mydata <- read.csv("C:/Users/Juan Gonzalez/OneDrive/Documents/Machine Learning/2019_Racial_Profiling__RP__Arrests.csv", stringsAsFactors = TRUE)

df = mydata %>%
  select('APD_RACE_DESC','RACE_KNOWN', 'Person.Search.YN','Search.Based.On','Search.Found','Reason.for.Stop.â...TCOLE.form')
df = na.?mit(df)

sample_data = sample.split(df, SplitRatio = 0.7)
train_data <- subset(df, sample_data == TRUE)
test_data <- subset(df, sample_data == FALSE)

myFormula <- df$APD_RACE_DESC ~ RACE_KNOWN + Person.Search.YN + Search.Based.On + Search.Found + Reason.f?r.Stop.â...TCOLE.form

model<- ctree(APD_RACE_DESC ~ ., train_data)

ctree_ = ctree(myFormula, df)


predict_model<-predict(ctree_, test_data)


m_at <- table(test_data$APD_RACE_DESC, predict_model)
m_at



ac_Test =sum(diag(m_at)) / sum(m_at)
print(paste('Accuracy for test is found to be', ac_Test))
plot(model)
