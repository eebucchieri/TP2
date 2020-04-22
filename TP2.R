library(brglm)
library(ROSE)

data <- read.csv("train.csv")

nrow(data)
data[data == -1] <- NA
data <- na.omit(data)

nrow(data)

train.index <- sample(nrow(data), size = nrow(train)*.8)

train.data <- data[train.index, ]
test.data <- data[-train.index, ]



log.fit <- glm(target ~ . -id, data = train.data, family = binomial)

log.pred <- predict(log.fit, newdata = test.data, type = c("response"))
log.pred <- as.numeric(log.pred)

hist(log.pred, breaks = 50)
max(log.pred)

log.pred.predictions <- rep(1, length(log.pred))
log.pred.predictions[log.pred <= 0.10] <- 0

log.pred.predictions


confusion <- table(log.pred.predictions, test.data$target)
confusion


sum(train.data$target)/nrow(train.data)


mean((log.pred.predictions - log.pred)^2)


oversample <- ovun.sample(target ~ ., data = train.data, method = "over")

over.fit <- glm(target ~ . -id, data = oversample$data, family = binomial)

over.pred <- predict(over.fit, newdata = test.data, type = c("response"))

mean((as.numeric(over.pred) - test.data$target)^2)

confusion
