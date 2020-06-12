getwd()
Titanic_dataset <- read.csv(file = 'TitanicDataset.csv') #na.strings=c("")

# Кількість пропущених значень
sapply(Titanic_dataset, function(x) sum(is.na(x)))
library(Amelia)
missmap(Titanic_dataset, main = "Missing values vs observed")

# Кількість унікальних значень
sapply(Titanic_dataset, function(x) length(unique(x)))

# Вилучення стовпчика з id
#dataset <- Titanic_dataset[, c(2:4,6)]
dataset <- subset(Titanic_dataset,select=c(2,3,4,5,6))
head(dataset)  # перші 6 рядків

# table(dataset$Survived)

# 1 варіант (середнє значення фактора)
train1 <- dataset
train1$Age[is.na(train1$Age)] <- mean(train1$Age,na.rm=T) # na.rm - NA remove
missmap(train1, main = "Missing values vs observed (train1)")

# 2 варіант (відкинути рядки з пропущеними значеннями)
train2 <- dataset
# train2 <- train2[!is.na(train2$Age),]
train2 <- subset(train2, !is.na(train2$Age))
missmap(train2, main = "Missing values vs observed (train2)")

# Побудова моделей
model1 <- glm(Survived ~.,family=binomial,data=train1)
summary(model1)

model2 <- glm(Survived ~.,family=binomial,data=train2)
summary(model2)

# Побудова ROC-кривих

library(pROC)
par(pty = "s") # квадратний графік

dev.new()
roc(train1$Survived, model1$fitted.values, plot=TRUE, legacy.axes=TRUE,
    percent=TRUE, xlab="False Positive Percentage",
    ylab="True Postive Percentage", col="#377eb8", lwd=4, print.auc=TRUE)

plot.roc(train2$Survived, model2$fitted.values, percent=TRUE,
         col="#4daf4a", lwd=4, print.auc=TRUE, add=TRUE, print.auc.y=40)

legend("bottomright", legend=c("train1 dataset ", "train2 dataset"),
       col=c("#377eb8", "#4daf4a"), lwd=4, cex = 1)

roc.info <- roc(train2$Survived, model2$fitted.values, legacy.axes=TRUE)
# str(roc.info)

roc.df <- data.frame(
  tpp=roc.info$sensitivities*100, ## tpp = true positive percentage
  fpp=(1 - roc.info$specificities)*100, ## fpp = false positive precentage
  thresholds=roc.info$thresholds)

head(roc.df) ## head() will show us the values for the upper right-hand corner
## of the ROC graph, when the threshold is so low 
## (negative infinity) that every single sample is called "obese".
## Thus TPP = 100% and FPP = 100%

tail(roc.df) ## tail() will show us the values for the lower left-hand corner
## of the ROC graph, when the threshold is so high (infinity) 
## that every single sample is called "not obese". 
## Thus, TPP = 0% and FPP = 0%

## now let's look at the thresholds between TPP 60% and 80%...
roc.df[roc.df$tpp > 60 & roc.df$tpp < 90,]

# Порогові значення

library(ROCR)
p <- predict(model2, newdata=train2, type="response")
pr <- prediction(p, train2$Survived)
# TPR = sensitivity, FPR=specificity
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
dev.new()
plot(prf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1),
     text.adj=c(-0.2,1.7), xlab="False Positive Percentage",
     ylab="True Postive Percentage")

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

#dev.set(dev.prev())
#dev.set(dev.next())

qualityTrain <- train2

qualityTest <- train2[which((train2$Age <= 30) & (train2$Class==1)
                           & (train2$Sex=="female")), ]

predictTrain = predict(model2, type="response")

# Confusion matrix for threshold of 0.4
t_04 <- table(qualityTrain$Survived, predictTrain > 0.4)
t_05 <- table(qualityTrain$Survived, predictTrain > 0.5)

Accuracy1 <- (t_04[1] + t_04[4])/(sum(t_04))
Accuracy2 <- (t_05[1] + t_05[4])/(sum(t_05))

Accuracy1
Accuracy2
 

# Оцінити шанси на виживання жінки першого класу, віком до 30 років.
Ages <- seq(1, 30, by=1)
Ticket <- runif(length(Ages), min = min(train2$TicketPrice),
                max = max(train2$TicketPrice))
predictData <- data.frame("Class" = c(1), "Sex" = c("female"),
                          "Age" = Ages, "TicketPrice" = Ticket)
odds <- predict(model2, predictData, type="response")
min(odds)
max(odds)
 
