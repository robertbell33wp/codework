
#Clipboard
bank <- readClipboard()
str(bank)

#Numbers with commas
setClass('numWithCommas')
setAs("character","numWithCommas", function(from) as.numeric(gsub(',','', from)))

#File
bank <- read.table(file = "clipboard",sep = "\t", header = T, 
                   colClasses = c("Amount"="numWithCommas"))
str(bank)
bank$Date <- as.Date(bank$Date, format = '%m/%d/%Y')
#bank$Amount <-as.numeric(bank$Amount)

library(lubridate)
library(dplyr)


#Q1
table(bank$AcctType)

bank %>% 
  group_by(Customer, Branch) %>% 
  summarise(Sum = sum(Amount), AveAmount = Sum / n() ,Cnt = n()) %>%
  mutate(pct = round((Cnt / 350)*100)) %>%
  arrange(Sum)



#Q2
bank %>% 
  group_by(Branch, AcctType) %>% 
  summarise(Sum = sum(Amount), count = n())


#Q3
boxplot(bank$Amount ~ bank$AcctType, ylim=c(0,25000))
summary(bank$Amount)
hist(bank$Amount)

par(mfrow=c(2,2))
for (i in levels(bank$AcctType)) 
  hist(bank$Amount[bank$AcctType==i], main = i, xlab = 'Amount', prob=T, breaks = 10)

par(mfrow=c(2,2))
for (i in levels(bank$AcctType)) 
  plot(density(bank$Amount[bank$AcctType==i], main = i, xlab = 'Amount', prob=T))

#ggplot
library(ggplot2)

par(mfrow=c(1,1))
ggplot(bank, aes(x=bank$Amount)) + geom_histogram(bins = 20) + 
      facet_wrap(~bank$AcctType, ncol=2)

#Q4
table(bank$AcctType, bank$OpenedBy)

library(tidyr)

bank %>%
  group_by(AcctType, OpenedBy) %>%
  summarise(Cnt = n()) %>%
  spread(OpenedBy, Cnt)

prop.table(table(bank$AcctType, bank$OpenedBy), 2) *100

bank %>%
  group_by(Customer, Branch) %>%
   summarise(Cnt = n()) %>%
  mutate(pct = 100*Cnt /  sum(Cnt)) %>%
  select(-Cnt) #%>%
  #spread(OpenedBy, pct) %>%
  #arrange(Teller)

#Q5
bank %>%
  group_by(Branch)
  summarise(sum(Amount))

#Q7
  bank %>% 
    group_by(AcctType) %>% 
    summarise(Sum = sum(Amount), AveAmount = Sum / n() ,Cnt = n()) %>%
    mutate(pct = round((Sum / sum(Sum))*100)) %>%
    arrange(Sum)  

