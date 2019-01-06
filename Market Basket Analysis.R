
#install and load package arules
install.packages("arules")
library(arules)
#install and load arulesViz
install.packages("arulesViz")
library(arulesViz)
#install and load tidyverse
install.packages("tidyverse")
library(tidyverse)
#install and load readxml
install.packages("readxml")
library(readxl)
#install and load knitr
install.packages("knitr")
library(knitr)
#load ggplot2 as it comes in tidyverse
library(ggplot2)
#install and load lubridate
install.packages("lubridate")
library(lubridate)
#install and load plyr
install.packages("plyr")
library(plyr)
library(dplyr)

#Data Pre-Processing

retail <- read_excel("Online Retail.xlsx")
retail <- retail[complete.cases(retail), ]

retail %>% mutate(Description = as.factor(Description))
retail %>% mutate(Country = as.factor(Country))

#Store InvoiceDate as date in new variable
retail$Date = as.Date(retail$InvoiceDate)
#Extract time from InvoiceDate and store in another variable
TransTime<- format(retail$InvoiceDate,"%H:%M:%S")
#Convert and edit InvoiceNo into numeric
InvoiceNo <- as.numeric(as.character(retail$InvoiceNo))
cbind(retail,TransTime)
cbind(retail,InvoiceNo)

glimpse(retail)
transactionData <- ddply(retail,c("InvoiceNo","Date"),
                         function(df1)paste(df1$Description,
                                            collapse = ","))
#set column InvoiceNo of dataframe transactionData  
transactionData$InvoiceNo <- NULL
#set column Date of dataframe transactionData
transactionData$Date <- NULL
#Rename column to items
colnames(transactionData) <- c("items")
#Show Dataframe transactionData
transactionData
write.csv(transactionData,"C:/Users/DELL/Desktop/R Dir/data_set/market_basket_transactions.csv", quote = FALSE, row.names = TRUE)
tr <- read.transactions("C:/Users/DELL/Desktop/R Dir/data_set/market_basket_transactions.csv", format = 'basket', sep=',')

#sep tell how items are separated. In this case you have separated using ','
tr
summary(tr)
# Create an item frequency plot for the top 20 items
if (!require("RColorBrewer")) {
  # install color package of R
  install.packages("RColorBrewer")
  #include library RColorBrewer
  library(RColorBrewer)
}
itemFrequencyPlot(tr,topN=20,type="absolute",col=brewer.pal(8,'Pastel2'), main="Absolute Item Frequency Plot")

#Min Support as 0.001, confidence as 0.8.
association.rules <- apriori(tr, parameter = list(supp=0.001, conf=0.8,maxlen=10))
summary(association.rules)
inspect(association.rules[1:10])
metal.association.rules <- apriori(tr, parameter = list(supp=0.001, conf=0.8),appearance = list(default="lhs",rhs="METAL"))
inspect(head(metal.association.rules))

metal.association.rules <- apriori(tr, parameter = list(supp=0.001, conf=0.8),appearance = list(lhs="METAL",default="rhs"))
subRules<-association.rules[quality(association.rules)$confidence>0.4]
plot(subRules,method="two-key plot")
subRules2<-head(subRules, n=20, by="lift")
plot(subRules2, method="paracoord")
