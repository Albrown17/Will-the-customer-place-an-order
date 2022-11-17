library(data.table)
library(tidyverse)
library(dplyr)
library(caret)
library(randomForest)


train <- fread("C:/Users/abrow/OneDrive/Assignments/2022/R/train.csv")
test <- fread("C:/Users/abrow/OneDrive/Assignments/2022/R/test.csv")
subm <- fread("C:/Users/abrow/OneDrive/Assignments/2022/R/sample_submission.csv")

Mode = function(x){
  ta = table(x)
  tam = max(ta)
  if (all(ta == tam))
    mod = NA
  else
    if(is.numeric(x))
      mod = as.numeric(names(ta)[ta == tam])
  else
    mod = names(ta)[ta == tam]
  return(mod)
}


train$order <- ifelse(train$order=='y',1,0)
train[is.na(train)] = 0

train1 <- train %>%
  group_by(sessionID) %>%
  summarize(hour = median(hour),
            weekday = last(weekday),
            duration = max(duration),
            clickCount = max(clickCount),
            clickMin = min(clickMin),
            clickMax = max(clickMax),
            clickTotal = max(clickTotal),
            cartCount = max(cartCount),
            cartMin = min(cartMin),
            cartMax = max(cartMax),
            cartTotal = max(cartTotal),
            cartStep = max(cartStep),
            status = Mode(status),
            availability = Mode(availability),
            customerID = Mode(customerID),
            purchase = max(purchase),
            score = max(score),
            account = max(account),
            payments = max(payments),
            age = median(age),
            salutation = Mode(salutation),
            lastOrder = max(lastOrder),
            order = median(order)
  )



set.seed(22)
rf <- randomForest(order~., data=train1, ntree=10, do.trace=T, importance=T)

importance(rf)

test1 <- test %>%
  group_by(sessionID) %>%
  summarize(hour = median(hour),
            weekday = last(weekday),
            duration = max(duration),
            clickCount = max(clickCount),
            clickMin = min(clickMin),
            clickMax = max(clickMax),
            clickTotal = max(clickTotal),
            cartCount = max(cartCount),
            cartMin = min(cartMin),
            cartMax = max(cartMax),
            cartTotal = max(cartTotal),
            cartStep = max(cartStep),
            status = Mode(status),
            availability = Mode(availability),
            customerID = Mode(customerID),
            purchase = max(purchase),
            score = max(score),
            account = max(account),
            payments = max(payments),
            age = median(age),
            salutation = Mode(salutation),
            lastOrder = max(lastOrder),
  )

pred <- predict(rf, newdata = test1)


subm[,2] <- pred[,2]
write.csv(subm, file='Contest3Attempt100.csv', row.names=FALSE)
