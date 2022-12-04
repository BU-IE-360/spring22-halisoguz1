valid_hours = seq(5, 19, 1)

validate_start = as.Date('2022-04-18')
validate_end = as.Date('2022-05-17')
validate_days = as.list(seq(validate_start, validate_end, by='1 day'))
hour <- 14
wmape <- c(1,1,1,1,1) #set 1 as beginning (the max value)
models <- list(1:5) #5 of the best models is going to be stored
set.seed(500) #initial seed is set to obtain the same results
errors <- c(1000,1000,1000,1000,1000) #bias of the best 5 models
abserrors <- c(1000,1000,1000,1000,1000) #abs bias of the best 5 models
for(i in 1:1000){#1000 model trials will be done
  CLOUDS <- c()
  DSWRF <- c()
  REL <- c()
  TEMP <- c()
  x <- c(4:12)
  y <- c(13:21)
  z <- c(22:30)
  t <- c(31:39)
  AT <- c(0:2)
  x1 <- sample(AT,1)
  x2 <- sample(c(1:2),1)
  x3 <- sample(AT,1)
  x4 <- sample(c(1:2),1)
  
  CLOUDS <- sample(x,x1,replace=FALSE)
  DSWRF <- sample(y,x2,replace=FALSE)
  REL <- sample(z,x3,replace=FALSE)
  TEMP <- sample(t,x4,replace=FALSE)
  realvalues <- c()
  predictions <- c()
  for (day in validate_days){
    train = hourfourteen[(date.x>='2021-02-01') & (date.x < day)]
    test = hourfourteen[date.x==day]
    columns <- c(CLOUDS,DSWRF,REL,TEMP)
    column_of_interest  = c(colnames(train)[columns],'capacity','production')
    train = train[, ..column_of_interest]
    test = test[, ..column_of_interest]
    target_data = train
    model = lm(production~., data = target_data)
    prediction = predict(model, test)
    realvalues= c(realvalues,test$production)
    predictions = c(predictions, prediction)
  }
  abserror <- sum(sum(abs(predictions-realvalues)))
  error <- sum(sum(predictions-realvalues))
  currentWMAPE <- sum(abs(predictions-realvalues))/sum(realvalues)
  for(j in 1:5){
    if(currentWMAPE<wmape[j]){
      wmape[j] <- currentWMAPE
      models[[j]] <- model
      abserrors[j] <- abserror
      errors[j] <- error
      break
    }
    else
      next
  }
}
wmape
models[[2]]
summary(models[[1]])
