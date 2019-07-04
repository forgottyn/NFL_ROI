rm(list=ls())
installIfAbsentAndLoad <- function(neededVector) {
  for(thispackage in neededVector) {
    if( ! require(thispackage, character.only = T) )
    { install.packages(thispackage)}
    require(thispackage, character.only = T)
  }
}
needed <- c("ISLR", "MASS", "car", "class", "arm", "FNN", "boot")
installIfAbsentAndLoad(needed)

#set the working space

########################
#####Classification#####
########################
windata <- read.csv("GameDataforWins.csv")
windata <- windata[, 3:ncol(windata)]

set.seed(2005)
#devide dataset into train and test
trainprop <- 0.8  
n <- nrow(windata)

train  <-  sample(n, trainprop * n)
test  <-  setdiff(1:n, train)
train.set <- windata[train, ]
test.set <- windata[test, ]

#explore the data structure
head(windata, n = 5)

#first I would like to run a correlation test 
cor.data <- cor(windata[, 1:ncol(windata)-1])
cor.data[upper.tri(cor.data)] <- NA  
cor.data

#goals and goalspercentage are colinear.
#we should not involve them both in one model. But if I put them in seperate 
#classification model, I could not find a F-value for the whole model to compare two models, or do an
#anova analysis for comparison. Thus, even though I chech the correlation before I 
#build up my models, I still put them into one model, and delete variables that
#has a high p-value.


##########################
#logistic regression model
glm.fit <- glm(Wins ~ .,data = train.set, family = binomial)
summary(glm.fit)
#delete takeaways because it has the hightest p value
glm.fit1 <- glm(Wins ~ .-takeaways,data = train.set, family = binomial)
summary(glm.fit1)

#delete giveaways because it has the hightest p value
glm.fit2 <- glm(Wins ~.-giveaways-takeaways,data = train.set, family = binomial)
summary(glm.fit2)

#delete GoalsPercentage because it has the hightest p value
glm.fit3 <- glm(Wins ~.-GoalsPercentage-giveaways-takeaways,data = train.set, family = binomial)
summary(glm.fit3)

#delete shots because it has the hightest p value
glm.fit4 <- glm(Wins ~.-GoalsPercentage-giveaways-takeaways-shots,data = train.set, family = binomial)
summary(glm.fit4)
#delete PowerPlayPercentage because it has the hightest p value
glm.fit5 <- glm(Wins ~.-GoalsPercentage-giveaways-takeaways-shots-PowerPlayPercentage ,data = train.set, family = binomial)
summary(glm.fit5)

#make sure there is no multicolinearity
vif(glm.fit5)

#now every predictor is significant. so our final combination is  goals+ hits+ pim+ faceOffWinPercentage+ SavePercentage 
glm.fit5 <- glm(Wins ~goals+ hits+ pim+ faceOffWinPercentage+ SavePercentage,data = train.set, family = binomial)
#use the best one to make prediction
glm.probs <- predict(glm.fit5, test.set,type = "response")
glm.pred <- rep(0, nrow(test.set)) 
glm.pred[glm.probs > .5] <- 1

mytable <- table(test.set$Wins, glm.pred)
mytable

#the next step I want to check whether increase the polynomial degree would
#have a better fitting model. But in the meantime we want to avoid overfitting problem.
error_10 <- c()
polyset <- c(1:10)
for (i in 1:10) {
  glm.fit <- glm(Wins ~ poly(goals, polyset[i]) + poly(hits, polyset[i]) + poly(pim, polyset[i]) + 
                    poly(faceOffWinPercentage, polyset[i])+ 
                    poly(SavePercentage, polyset[i]), data = train.set, family = binomial)
  glm.probs <- predict(glm.fit, test.set,type = "response")
  glm.pred <- rep(0, nrow(test.set)) 
  glm.pred[glm.probs > .5] <- 1
  error_10[i] <- mean(glm.pred != test.set$Wins)
  }
plot(c(1:10), error_10, 
     xlim=c(1, 10), 
     ylim=c(0,max(error_10)),      
     type='n',  
     xlab='Degree of Polynomial', 
     ylab='Error Rate',
     main = "Error rate as a Function of Degree of Polynomial 
     for Logistic Regression")
lines(seq(1, 10, by=1), 
      error_10, 
      type='b', 
      col=1, 
      pch=16)
print(polyset[which.min(error_10)])
print(error_10[1] - error_10[which.min(error_10)])

#calculate the rates for accuracy
ov_co <- (mytable[1, 1] + mytable[2, 2])/sum(mytable)
ov_er <- 1 - ov_co
tp1_er <- mytable[1, 2]/sum(mytable[1, ])
tp2_er <- mytable[2, 1]/sum(mytable[2, ])
power <- mytable[2, 2]/sum(mytable[2,])
precision <- mytable[2, 2]/sum(mytable[,2])

#print(paste("The overall fraction of correct predictions:", ov_co))
#print(paste("The overall error rate:", ov_er))
#print(paste("The Type 1 error rate:", tp1_er))
#print(paste("The Type 2 error rate:", tp2_er))
#print(paste("The Power of the model:", power))
#print(paste("The Precision of the model:", precision))

Model<- c()
CorrectRate <- c()
ErrorRate <- c()
Type1 <- c()
Type2 <- c()
Power <- c()
Precision <- c()

Model[1] <- "GLM"
CorrectRate[1] <- ov_co
ErrorRate[1] <- ov_er
Type1[1] <- tp1_er
Type2[1] <- tp2_er
Power[1] <- power
Precision[1] <- precision

##########################
#LDA model
lda.fit4 =lda(Wins ~ goals+ hits+ pim+ faceOffWinPercentage+ SavePercentage, data = train.set)

#think about degree of polynomial
error_10 <- c()
polyset <- c(1:10)
for (i in 1:10) {
  lda.fit =lda(Wins ~poly(goals, polyset[i]) + poly(hits, polyset[i]) + poly(pim, polyset[i]) + 
                 poly(faceOffWinPercentage, polyset[i])+ 
                 poly(SavePercentage, polyset[i]), data = train.set)
  lda.probs <- predict(lda.fit, test.set,type = "response")
  lda.pred <- rep(0, nrow(test.set)) 
  lda.pred[glm.probs > .5] <- 1
  error_10[i] <- mean(lda.pred != test.set$Wins)
}
plot(c(1:10), error_10, 
     xlim=c(1, 10), 
     ylim=c(0,0.2),      
     type='n',  
     xlab='Degree of Polynomial', 
     ylab='Error Rate',
     main = "Error rate as a Function of 
     Degree of Polynomial for LDA")
lines(seq(1, 10, by=1), 
      error_10, 
      type='b', 
      col=1, 
      pch=16)
print(polyset[which.min(error_10)])

#make the prediction
lda.pred <- predict(lda.fit4,test.set)
mytable <- table(test.set$Wins, lda.pred$class)
mytable

#calculate the rates
ov_co <- (mytable[1, 1] + mytable[2, 2])/sum(mytable)
ov_er <- 1 - ov_co
tp1_er <- mytable[1, 2]/sum(mytable[1, ])
tp2_er <- mytable[2, 1]/sum(mytable[2, ])
power <- mytable[2, 2]/sum(mytable[2,])
precision <- mytable[2, 2]/sum(mytable[,2])

Model[2] <- "LDA"
CorrectRate[2] <- ov_co
ErrorRate[2] <- ov_er
Type1[2] <- tp1_er
Type2[2] <- tp2_er
Power[2] <- power
Precision[2] <- precision

##########################
#QDA model
qda.fit4 = qda(Wins ~ goals+ hits+ pim+ faceOffWinPercentage+ SavePercentage, data = train.set)

#think about degree of polynomial
error_7 <- c()
polyset <- c(1:7)
for (i in 1:7) {
  qda.fit = qda(Wins ~ poly(goals, polyset[i]) + poly(hits, polyset[i]) + poly(pim, polyset[i]) + 
                  poly(faceOffWinPercentage, polyset[i])+ 
                  poly(SavePercentage, polyset[i]), data = train.set)
  qda.pred <- predict(qda.fit, test.set) 
  error_7[i] <- mean(qda.pred$class != test.set$Wins)
}
plot(c(1:7), error_7, 
     xlim=c(1, 7), 
     ylim=c(0,max(error_7)),      
     type='n',  
     xlab='Degree of Polynomial', 
     ylab='Error Rate',
     main = "Error rate as a Function of 
     Degree of Polynomial for QDA")
lines(seq(1, 7, by=1), 
      error_7, 
      type='b', 
      col=1, 
      pch=16)
print(polyset[which.min(error_7)])


#make the prediction
qda.pred <- predict(qda.fit4, test.set)
mytable <- table(test.set$Wins, qda.pred$class)
mytable

#calculate the rates
ov_co <- (mytable[1, 1] + mytable[2, 2])/sum(mytable)
ov_er <- 1 - ov_co
tp1_er <- mytable[1, 2]/sum(mytable[1, ])
tp2_er <- mytable[2, 1]/sum(mytable[2, ])
power <- mytable[2, 2]/sum(mytable[2,])
precision <- mytable[2, 2]/sum(mytable[,2])

Model[3] <- "GDA"
CorrectRate[3] <- ov_co
ErrorRate[3] <- ov_er
Type1[3] <- tp1_er
Type2[3] <- tp2_er
Power[3] <- power
Precision[3] <- precision

##########################
#KNN model
#goals+hits+pim+faceOffWinPercentage+SavePercentage
train.wins <- train.set$Wins
test.wins <- test.set$Wins
train.x <- scale(data.frame(train.set$goals, train.set$hits, train.set$pim, train.set$faceOffWinPercentage, train.set$SavePercentage))
test.x <- scale(data.frame(test.set$goals, test.set$hits, test.set$pim, test.set$faceOffWinPercentage, test.set$SavePercentage))
#create a KNN model
kset <- seq(1, 49, by = 2)

#test knn model on different k values
trainErrors <- c()
testErrors <- c()
for (i in c(1:length(kset))){
  knn.pred <- knn(train.x, train.x, train.wins, k = kset[i])
  trainErrors[i] <- mean(train.wins!= knn.pred)
  knn.pred <- knn(train.x, test.x, train.wins, k = kset[i])
  testErrors[i] <- mean(test.wins!= knn.pred)
  }

plot(kset, testErrors, 
     xlim=c(49, 1), 
     ylim=c(0,max(c(testErrors, trainErrors))),      
     type='n',  
     xlab='Increasing Flexibility (Decreasing k)', 
     ylab='Error rate', 
     main='Error rate as a Function of Flexibility 
     for KNN Prediction')
lines(seq(49, 1, by=-2), 
      trainErrors[order(length(trainErrors):1)], 
      type='b', 
      col=2, 
      pch=16)
lines(seq(49, 1, by=-2), 
      testErrors[order(length(testErrors):1)], 
      type='b', 
      col=1, 
      pch=16)
legend("topleft", legend = c("testErrors", "trainErrors"), 
       col=c(1, 2), 
       cex=.75, 
       pch=16,
       lty = 1)
print(paste("The best training error rate occured with k =", kset[which.min(trainErrors)], 
            "and produced a training error rate of", trainErrors[which.min(trainErrors)]))
print(paste("The best test error rate occured with k =", kset[which.min(testErrors)], 
            "and produced a test error rate of", testErrors[which.min(testErrors)]))

#build the knn model
ktest <-  kset[which.min(testErrors)]
knn.pred <- knn(train.x, test.x, train.wins, k = ktest)
mytable <- table(test.wins , knn.pred)

#calculate the rates
ov_co <- (mytable[1, 1] + mytable[2, 2])/sum(mytable)
ov_er <- 1 - ov_co
tp1_er <- mytable[1, 2]/sum(mytable[1, ])
tp2_er <- mytable[2, 1]/sum(mytable[2, ])
power <- mytable[2, 2]/sum(mytable[2,])
precision <- mytable[2, 2]/sum(mytable[,2])

Model[4] <- "KNN"
CorrectRate[4] <- ov_co
ErrorRate[4] <- ov_er
Type1[4] <- tp1_er
Type2[4] <- tp2_er
Power[4] <- power
Precision[4] <- precision

comparison <- data.frame(Model, CorrectRate, ErrorRate, Type1, Type2, Power, Precision)
comparison

#business solutions
goals <- mean(windata$goals)
hits <- mean(windata$hits)
pim <- mean(windata$pim)
faceOffWinPercentage <- mean(windata$faceOffWinPercentage)
SavePercentage <- mean(windata$SavePercentage)

meanset <- data.frame(goals, hits, pim, faceOffWinPercentage, SavePercentage)

glm.probs <- predict(glm.fit5, meanset,type = "response")

# +$1,000,000 on goalies
SavePercentage <- mean(windata$SavePercentage)+0.00189
meanset <- data.frame(goals, hits, pim, faceOffWinPercentage, SavePercentage)
glm.probs <- predict(glm.fit5, meanset,type = "response")

# +$1,000,000 on skaters
goals <- mean(windata$goals)+0.01179
SavePercentage <- mean(windata$SavePercentage)
meanset <- data.frame(goals, hits, pim, faceOffWinPercentage, SavePercentage)
glm.probs <- predict(glm.fit5, meanset,type = "response")
