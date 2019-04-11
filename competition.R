#Loading libraries
library(tm)
library(rpart)
library(rpart.plot)
library(randomForest)
library(caTools)
library(flexclust)
library(ade4)
library(ROCR)
library(mice)

#Loading datasets
eBayTrain = read.csv("eBayiPadTrain.csv", stringsAsFactors=FALSE)
eBayFinalTest = read.csv("eBayiPadTest.csv", stringsAsFactors=FALSE)

#Creating important words frequencies and wordcount variables in eBayTrain and eBayFinalTest
#Also, removing description variable
CorpusDescription = Corpus(VectorSource(c(eBayTrain$description, eBayFinalTest$description)))
CorpusDescription = tm_map(CorpusDescription, content_transformer(tolower), lazy=TRUE)
CorpusDescription = tm_map(CorpusDescription, PlainTextDocument, lazy=TRUE)
CorpusDescription = tm_map(CorpusDescription, removePunctuation, lazy=TRUE)
CorpusDescription = tm_map(CorpusDescription, removeWords, stopwords("english"), lazy=TRUE)

dtm = DocumentTermMatrix(CorpusDescription)
sparse = removeSparseTerms(dtm, 0.96) #It returns words that appear in at least 4% of the rows
DescriptionWords = as.data.frame(as.matrix(sparse))
colnames(DescriptionWords) = make.names(colnames(DescriptionWords))
colnames(DescriptionWords) = paste0("desc.", colnames(DescriptionWords))
DescriptionWords$wordcount = rowSums(as.matrix(dtm))
DescriptionWords$desc.ipad = NULL
DescriptionWords$desc.this = NULL
DescriptionWords$desc.item = NULL

DescriptionWordsTrain = head(DescriptionWords, nrow(eBayTrain))
DescriptionWordsTrain$biddable = eBayTrain$biddable
DescriptionWordsTrain$startprice = eBayTrain$startprice
DescriptionWordsTrain$condition = eBayTrain$condition
DescriptionWordsTrain$cellular = eBayTrain$cellular
DescriptionWordsTrain$carrier = eBayTrain$carrier
DescriptionWordsTrain$color = eBayTrain$color
DescriptionWordsTrain$storage = eBayTrain$storage
DescriptionWordsTrain$productline = eBayTrain$productline
DescriptionWordsTrain$sold = eBayTrain$sold
DescriptionWordsTrain$UniqueID = eBayTrain$UniqueID
eBayTrain = DescriptionWordsTrain

DescriptionWordsTest = tail(DescriptionWords, nrow(eBayFinalTest))
DescriptionWordsTest$biddable = eBayFinalTest$biddable
DescriptionWordsTest$startprice = eBayFinalTest$startprice
DescriptionWordsTest$condition = eBayFinalTest$condition
DescriptionWordsTest$cellular = eBayFinalTest$cellular
DescriptionWordsTest$carrier = eBayFinalTest$carrier
DescriptionWordsTest$color = eBayFinalTest$color
DescriptionWordsTest$storage = eBayFinalTest$storage
DescriptionWordsTest$productline = eBayFinalTest$productline
DescriptionWordsTest$UniqueID = eBayFinalTest$UniqueID
eBayFinalTest = DescriptionWordsTest

#Creating aboveavgprice variable
#eBayTrain$aboveavgprice = ifelse(eBayTrain$startprice > mean(eBayTrain$startprice), 1, 0)
#eBayFinalTest$aboveavgprice = ifelse(eBayFinalTest$startprice > mean(eBayFinalTest$startprice), 1, 0)

#Creating isnew variable
#eBayTrain$isnew = ifelse(eBayTrain$condition == "New", 1, 0)
#eBayFinalTest$isnew = ifelse(eBayFinalTest$condition == "New", 1, 0)

#Adjusting variable types in eBayTrain and eBayFinalTest
eBayTrain$condition = as.factor(eBayTrain$condition)
eBayTrain$cellular = as.factor(eBayTrain$cellular)
eBayTrain$carrier = as.factor(eBayTrain$carrier)
eBayTrain$color = as.factor(eBayTrain$color)
eBayTrain$storage = as.factor(eBayTrain$storage)
eBayTrain$productline = as.factor(eBayTrain$productline)
eBayTrain$sold = as.factor(eBayTrain$sold)

eBayFinalTest$condition = as.factor(eBayFinalTest$condition)
eBayFinalTest$cellular = as.factor(eBayFinalTest$cellular)
eBayFinalTest$carrier = as.factor(eBayFinalTest$carrier)
eBayFinalTest$color = as.factor(eBayFinalTest$color)
eBayFinalTest$storage = as.factor(eBayFinalTest$storage)
eBayFinalTest$productline = as.factor(eBayFinalTest$productline)
levels(eBayFinalTest$productline) = levels(eBayTrain$productline) #training data frame has more types of products then testing data frame

###APPROACH 1
#Creating a random forest and making predictions on eBayTrain
#model = randomForest(sold ~ . - UniqueID, data = eBayTrain)
#pred = predict(model, type = "class")
#table(eBayTrain$sold, pred)

#Making predictions on eBayFinalTest using APPROACH 1 and preparing submission file for Kaggle
#You should upload "SubmissionDescriptionLog.csv" on the Kaggle
#PredTest = predict(model, newdata = eBayFinalTest, type = "class")
#MySubmission = data.frame(UniqueID = eBayFinalTest$UniqueID, Probability1 = PredTest)
#write.csv(MySubmission, "SubmissionApp1.csv", row.names=FALSE)

###APPROACH 2
#Creating two random forests, one for biddable goods and another for "Buy It Now" goods, and making predictions on training sets
#modelBid = randomForest(sold ~ . - UniqueID, data = subset(eBayTrain, eBayTrain$biddable == 1))
#modelBuyNow = randomForest(sold ~ . - UniqueID, data = subset(eBayTrain, eBayTrain$biddable == 0))
#predBid = predict(modelBid, type = "class")
#table(subset(eBayTrain, eBayTrain$biddable == 1)$sold, predBid)
#predBuyNow = predict(modelBuyNow, type = "class")
#table(subset(eBayTrain, eBayTrain$biddable == 0)$sold, predBuyNow)

#Making predictions on eBayFinalTest using APPROACH 2 and preparing submission file for Kaggle
#You should upload "SubmissionDescriptionLog.csv" on the Kaggle
#eBayFinalTestBid = subset(eBayFinalTest, eBayFinalTest$biddable == 1)
#eBayFinalTestBuyNow = subset(eBayFinalTest, eBayFinalTest$biddable == 0)
#PredTestBid = predict(modelBid, newdata = eBayFinalTestBid, type = "class")
#PredTestBuyNow = predict(modelBuyNow, newdata = eBayFinalTestBuyNow, type = "class")
#MySubmissionBid = data.frame(UniqueID = eBayFinalTestBid$UniqueID, Probability1 = PredTestBid)
#MySubmissionBuyNow = data.frame(UniqueID = eBayFinalTestBuyNow$UniqueID, Probability1 = PredTestBuyNow)
#MySubmission = rbind(MySubmissionBid, MySubmissionBuyNow)
#write.csv(MySubmission, "SubmissionApp3.csv", row.names=FALSE)

###APPROACH 3
#Creating a logistic regression for biddable goods and a random forest for "Buy It Now" goods, and making predictions on training sets
#modelBid = glm(sold ~ . - UniqueID, data = subset(eBayTrain, eBayTrain$biddable == 1), family = "binomial")
#modelBuyNow = randomForest(sold ~ . - UniqueID, data = subset(eBayTrain, eBayTrain$biddable == 0))
#predBid = predict(modelBid, type = "response")
#table(subset(eBayTrain, eBayTrain$biddable == 1)$sold, predBid)
#predBuyNow = predict(modelBuyNow, type = "class")
#table(subset(eBayTrain, eBayTrain$biddable == 0)$sold, predBuyNow)

#Making predictions on eBayFinalTest using APPROACH 3 and preparing submission file for Kaggle
#You should upload "SubmissionDescriptionLog.csv" on the Kaggle
#eBayFinalTestBid = subset(eBayFinalTest, eBayFinalTest$biddable == 1)
#eBayFinalTestBuyNow = subset(eBayFinalTest, eBayFinalTest$biddable == 0)
#PredTestBid = predict(modelBid, newdata = eBayFinalTestBid, type = "response")
#PredTestBuyNow = predict(modelBuyNow, newdata = eBayFinalTestBuyNow, type = "class")
#MySubmissionBid = data.frame(UniqueID = eBayFinalTestBid$UniqueID, Probability1 = PredTestBid)
#MySubmissionBuyNow = data.frame(UniqueID = eBayFinalTestBuyNow$UniqueID, Probability1 = PredTestBuyNow)
#MySubmission = rbind(MySubmissionBid, MySubmissionBuyNow)
#write.csv(MySubmission, "SubmissionApp3.1.csv", row.names=FALSE)

###APPROACH 4
#Creating two random forests, one for biddable goods and another for "Buy It Now" goods, using different variables, and making predictions on training sets
#modelBid = randomForest(sold ~ desc.great + desc.scratches + startprice + condition + storage + productline, data = subset(eBayTrain, eBayTrain$biddable == 1))
#modelBuyNow = randomForest(sold ~ startprice + condition + storage, data = subset(eBayTrain, eBayTrain$biddable == 0))
#predBid = predict(modelBid, type = "class")
#table(subset(eBayTrain, eBayTrain$biddable == 1)$sold, predBid)
#predBuyNow = predict(modelBuyNow, type = "class")
#table(subset(eBayTrain, eBayTrain$biddable == 0)$sold, predBuyNow)

#Making predictions on eBayFinalTest using APPROACH 4 and preparing submission file for Kaggle
#You should upload "SubmissionDescriptionLog.csv" on the Kaggle
#eBayFinalTestBid = subset(eBayFinalTest, eBayFinalTest$biddable == 1)
#eBayFinalTestBuyNow = subset(eBayFinalTest, eBayFinalTest$biddable == 0)
#PredTestBid = predict(modelBid, newdata = eBayFinalTestBid, type = "class")
#PredTestBuyNow = predict(modelBuyNow, newdata = eBayFinalTestBuyNow, type = "class")
#MySubmissionBid = data.frame(UniqueID = eBayFinalTestBid$UniqueID, Probability1 = PredTestBid)
#MySubmissionBuyNow = data.frame(UniqueID = eBayFinalTestBuyNow$UniqueID, Probability1 = PredTestBuyNow)
#MySubmission = rbind(MySubmissionBid, MySubmissionBuyNow)
#write.csv(MySubmission, "SubmissionApp4.csv", row.names=FALSE)

###APPROACH 5
#Creating a random forest for biddable goods, using desc.great + desc.scratches + startprice + condition + storage + productline
#Creating another random forest for "Buy It Now" goods
#modelBid = randomForest(sold ~ desc.great + desc.scratches + startprice + condition + storage + productline, data = subset(eBayTrain, eBayTrain$biddable == 1))
#predBid = predict(modelBid, type = "class")
#table(subset(eBayTrain, eBayTrain$biddable == 1)$sold, predBid)

#modelBuyNow = randomForest(sold ~ .-UniqueID, data = subset(eBayTrain, eBayTrain$biddable == 0))
#predBuyNow = predict(modelBuyNow, type = "class")
#table(subset(eBayTrain, eBayTrain$biddable == 0)$sold, predBuyNow)

#Making predictions on eBayFinalTest using APPROACH 5 and preparing submission file for Kaggle
#You should upload "SubmissionDescriptionLog.csv" on the Kaggle
#eBayFinalTestBid = subset(eBayFinalTest, eBayFinalTest$biddable == 1)
#eBayFinalTestBuyNow = subset(eBayFinalTest, eBayFinalTest$biddable == 0)
#PredTestBid = predict(modelBid, newdata = eBayFinalTestBid, type = "class")
#PredTestBuyNow = predict(modelBuyNow, newdata = eBayFinalTestBuyNow, type = "class")
#MySubmissionBid = data.frame(UniqueID = eBayFinalTestBid$UniqueID, Probability1 = PredTestBid)
#MySubmissionBuyNow = data.frame(UniqueID = eBayFinalTestBuyNow$UniqueID, Probability1 = PredTestBuyNow)
#MySubmission = rbind(MySubmissionBid, MySubmissionBuyNow)
#write.csv(MySubmission, "SubmissionApp5.csv", row.names=FALSE)

###APPROACH 6
#Creating a logistic regression for biddable goods using desc.great + desc.scratches + startprice + condition + storage + productline
#Creating a random forest for "Buy It Now" goods
#modelBid = glm(sold ~ desc.great + desc.scratches + startprice + condition + storage + productline, data = subset(eBayTrain, eBayTrain$biddable == 1), family = "binomial")
#predBid = predict(modelBid, type = "response")
#table(subset(eBayTrain, eBayTrain$biddable == 1)$sold, predBid)

#modelBuyNow = randomForest(sold ~ . - UniqueID, data = subset(eBayTrain, eBayTrain$biddable == 0))
#predBuyNow = predict(modelBuyNow, type = "class")
#table(subset(eBayTrain, eBayTrain$biddable == 0)$sold, predBuyNow)

#Making predictions on eBayFinalTest using APPROACH 6 and preparing submission file for Kaggle
#You should upload "SubmissionDescriptionLog.csv" on the Kaggle
#eBayFinalTestBid = subset(eBayFinalTest, eBayFinalTest$biddable == 1)
#eBayFinalTestBuyNow = subset(eBayFinalTest, eBayFinalTest$biddable == 0)
#PredTestBid = predict(modelBid, newdata = eBayFinalTestBid, type = "response")
#PredTestBuyNow = predict(modelBuyNow, newdata = eBayFinalTestBuyNow, type = "class")
#MySubmissionBid = data.frame(UniqueID = eBayFinalTestBid$UniqueID, Probability1 = PredTestBid)
#MySubmissionBuyNow = data.frame(UniqueID = eBayFinalTestBuyNow$UniqueID, Probability1 = PredTestBuyNow)
#MySubmission = rbind(MySubmissionBid, MySubmissionBuyNow)
#write.csv(MySubmission, "SubmissionApp6.csv", row.names=FALSE)

###APPROACH 7
#Creating a logistic regression for biddable goods and a random forest for 3 clusters of "buy it now" goods
#modelBid = glm(sold ~ . - UniqueID, data = subset(eBayTrain, eBayTrain$biddable == 1), family = "binomial")
#predBid = predict(modelBid, type = "response")
#table(subset(eBayTrain, eBayTrain$biddable == 1)$sold, predBid > 0.5)

#train = subset(eBayTrain, eBayTrain$biddable == 0)
#limitedtrain = train
#limitedtrain$desc.condition = NULL
#limitedtrain$desc.cosmetic = NULL
#limitedtrain$desc.good = NULL
#limitedtrain$desc.great = NULL
#limitedtrain$desc.minor = NULL
#limitedtrain$desc.new = NULL
#limitedtrain$desc.scratches = NULL
#limitedtrain$desc.screen = NULL
#limitedtrain$desc.used = NULL
#limitedtrain$desc.working = NULL
#limitedtrain$wordcount = NULL
#limitedtrain$biddable = NULL
#limitedtrain$startprice = NULL
#limitedtrain$sold = NULL
#limitedtrain$UniqueID = NULL
#limitedtrain = acm.disjonctif(limitedtrain)

#test = subset(eBayFinalTest, eBayFinalTest$biddable == 0)
#limitedtest = test
#limitedtest$desc.condition = NULL
#limitedtest$desc.cosmetic = NULL
#limitedtest$desc.good = NULL
#limitedtest$desc.great = NULL
#limitedtest$desc.minor = NULL
#limitedtest$desc.new = NULL
#limitedtest$desc.scratches = NULL
#limitedtest$desc.screen = NULL
#limitedtest$desc.used = NULL
#limitedtest$desc.working = NULL
#limitedtest$wordcount = NULL
#limitedtest$biddable = NULL
#limitedtest$startprice = NULL
#limitedtest$sold = NULL
#limitedtest$UniqueID = NULL
#limitedtest = acm.disjonctif(limitedtest)

#km = kmeans(limitedtrain, centers = 3)
#km.kcca = as.kcca(km, limitedtrain)
#clusterTrain = predict(km.kcca)
#clusterTest = predict(km.kcca, newdata=limitedtest)
#train1 = subset(train, clusterTrain == 1)
#train2 = subset(train, clusterTrain == 2)
#train3 = subset(train, clusterTrain == 3)
#test1 = subset(test, clusterTest == 1)
#test2 = subset(test, clusterTest == 2)
#test3 = subset(test, clusterTest == 3)
#rf1 = randomForest(sold ~ . -UniqueID, data = train1)
#rf2 = randomForest(sold ~ . -UniqueID, data = train2)
#rf3 = randomForest(sold ~ . -UniqueID, data = train3)
#predtrain1 = predict(rf1, type = "class")
#predtrain2 = predict(rf2, type = "class")
#predtrain3 = predict(rf3, type = "class")
#predBuyNow = c(predtrain1, predtrain2, predtrain3)
#trainoutcomes = c(train1$sold, train2$sold, train3$sold)
#table(trainoutcomes, predBuyNow > 0.5)

#Making predictions on eBayFinalTest using APPROACH 7 and preparing submission file for Kaggle
#You should upload "SubmissionDescriptionLog.csv" on the Kaggle
#eBayFinalTestBid = subset(eBayFinalTest, eBayFinalTest$biddable == 1)
#PredTestBid = predict(modelBid, newdata = eBayFinalTestBid, type = "response")

#eBayFinalTestBuyNow = subset(eBayFinalTest, eBayFinalTest$biddable == 0)
#pred1 = predict(rf1, newdata = test1, type = "prob")[,2]
#pred2 = predict(rf2, newdata = test2, type = "prob")[,2]
#pred3 = predict(rf3, newdata = test3, type = "prob")[,2]
#PredTestBuyNow = c(pred1, pred2, pred3)
#IDs = c(test1$UniqueID, test2$UniqueID, test3$UniqueID)

#MySubmissionBid = data.frame(UniqueID = eBayFinalTestBid$UniqueID, Probability1 = PredTestBid)
#MySubmissionBuyNow = data.frame(UniqueID = IDs, Probability1 = PredTestBuyNow)
#MySubmission = rbind(MySubmissionBid, MySubmissionBuyNow)
#write.csv(MySubmission, "SubmissionApp7.csv", row.names=FALSE)

###APPROACH 8
#Creating a random forest for 4 clusters
#It's a good idea to check the clusters to see what they represent

#train = eBayTrain
#limitedtrain = train
#limitedtrain$desc.condition = NULL
#limitedtrain$desc.cosmetic = NULL
#limitedtrain$desc.good = NULL
#limitedtrain$desc.great = NULL
#limitedtrain$desc.minor = NULL
#limitedtrain$desc.new = NULL
#limitedtrain$desc.scratches = NULL
#limitedtrain$desc.screen = NULL
#limitedtrain$desc.used = NULL
#limitedtrain$desc.working = NULL
#limitedtrain$wordcount = NULL
#limitedtrain$startprice = NULL
#limitedtrain$sold = NULL
#limitedtrain$UniqueID = NULL
#limitedtrain = acm.disjonctif(limitedtrain)

#test = eBayFinalTest
#limitedtest = test
#limitedtest$desc.condition = NULL
#limitedtest$desc.cosmetic = NULL
#limitedtest$desc.good = NULL
#limitedtest$desc.great = NULL
#limitedtest$desc.minor = NULL
#limitedtest$desc.new = NULL
#limitedtest$desc.scratches = NULL
#limitedtest$desc.screen = NULL
#limitedtest$desc.used = NULL
#limitedtest$desc.working = NULL
#limitedtest$wordcount = NULL
#limitedtest$startprice = NULL
#limitedtest$UniqueID = NULL
#limitedtest = acm.disjonctif(limitedtest)

#km = kmeans(limitedtrain, centers = 4)
#km.kcca = as.kcca(km, limitedtrain)
#clusterTrain = predict(km.kcca)
#train1 = subset(train, clusterTrain == 1)
#train2 = subset(train, clusterTrain == 2)
#train3 = subset(train, clusterTrain == 3)
#train4 = subset(train, clusterTrain == 4)
#rf1 = randomForest(sold ~ . -UniqueID, data = train1)
#rf2 = randomForest(sold ~ . -UniqueID, data = train2)
#rf3 = randomForest(sold ~ . -UniqueID, data = train3)
#rf4 = randomForest(sold ~ . -UniqueID, data = train4)
#predtrain1 = predict(rf1, type = "prob")[,2]
#predtrain2 = predict(rf2, type = "prob")[,2]
#predtrain3 = predict(rf3, type = "prob")[,2]
#predtrain4 = predict(rf4, type = "prob")[,2]
#predtrain = c(predtrain1, predtrain2, predtrain3, predtrain4)
#trainoutcomes = c(train1$sold, train2$sold, train3$sold, train4$sold)
#table(trainoutcomes, predtrain > 0.5)

#Making predictions on eBayFinalTest using APPROACH 8 and preparing submission file for Kaggle
#clusterTest = predict(km.kcca, newdata = limitedtest)
#test1 = subset(test, clusterTest == 1)
#test2 = subset(test, clusterTest == 2)
#test3 = subset(test, clusterTest == 3)
#test4 = subset(test, clusterTest == 4)
#predtest1 = predict(rf1, newdata = test1, type = "prob")[,2]
#predtest2 = predict(rf2, newdata = test2, type = "prob")[,2]
#predtest3 = predict(rf3, newdata = test3, type = "prob")[,2]
#predtest4 = predict(rf4, newdata = test4, type = "prob")[,2]
#predtest = c(predtest1, predtest2, predtest3, predtest4)
#ids = c(test1$UniqueID, test2$UniqueID, test3$UniqueID, test4$UniqueID)

#MySubmission = data.frame(UniqueID = ids, Probability1 = predtest)
#write.csv(MySubmission, "SubmissionApp8.3.csv", row.names=FALSE)

###APPROACH 9
#Creating a log regression for 3 clusters
#Creating a log regression for iPad 5

#ipad5train = eBayTrain
#ipad5test = subset(eBayFinalTest, eBayFinalTest$productline == "iPad 5")
#ipad5log = glm(sold ~ . -UniqueID, data = ipad5train, family = "binomial")
#ipad5predtrain = predict(ipad5log, type = "response")
#table(ipad5train$sold, ipad5predtrain > 0.5)

#train = eBayTrain
#limitedtrain = train
#limitedtrain$desc.condition = NULL
#limitedtrain$desc.cosmetic = NULL
#limitedtrain$desc.good = NULL
#limitedtrain$desc.great = NULL
#limitedtrain$desc.minor = NULL
#limitedtrain$desc.new = NULL
#limitedtrain$desc.scratches = NULL
#limitedtrain$desc.screen = NULL
#limitedtrain$desc.used = NULL
#limitedtrain$desc.working = NULL
#limitedtrain$wordcount = NULL
#limitedtrain$startprice = NULL
#limitedtrain$sold = NULL
#limitedtrain$UniqueID = NULL
#limitedtrain = acm.disjonctif(limitedtrain)

#test = subset(eBayFinalTest, eBayFinalTest$productline != "iPad 5")
#limitedtest = test
#limitedtest$desc.condition = NULL
#limitedtest$desc.cosmetic = NULL
#limitedtest$desc.good = NULL
#limitedtest$desc.great = NULL
#limitedtest$desc.minor = NULL
#limitedtest$desc.new = NULL
#limitedtest$desc.scratches = NULL
#limitedtest$desc.screen = NULL
#limitedtest$desc.used = NULL
#limitedtest$desc.working = NULL
#limitedtest$wordcount = NULL
#limitedtest$startprice = NULL
#limitedtest$UniqueID = NULL
#limitedtest = acm.disjonctif(limitedtest)

#km = kmeans(limitedtrain, centers = 3)
#km.kcca = as.kcca(km, limitedtrain)
#clusterTrain = predict(km.kcca)
#train1 = subset(train, clusterTrain == 1)
#train2 = subset(train, clusterTrain == 2)
#train3 = subset(train, clusterTrain == 3)
#log1 = glm(sold ~ . -UniqueID, data = train1, family = "binomial") #maybe you have to drop some variables in train1 before
#log2 = glm(sold ~ . -UniqueID, data = train2, family = "binomial") #maybe you have to drop some variables in train2 before
#log3 = glm(sold ~ . -UniqueID, data = train3, family = "binomial") #maybe you have to drop some variables in train3 before
#predtrain1 = predict(log1, type = "response")
#predtrain2 = predict(log2, type = "response")
#predtrain3 = predict(log3, type = "response")
#predtrain = c(predtrain1, predtrain2, predtrain3)
#trainoutcomes = c(train1$sold, train2$sold, train3$sold)
#table(trainoutcomes, predtrain > 0.5)

#Making predictions on eBayFinalTest using APPROACH 9 and preparing submission file for Kaggle
#ipad5predtest = predict(ipad5log, newdata = ipad5test, type = "response")

#clusterTest = predict(km.kcca, newdata = limitedtest)
#test1 = subset(test, clusterTest == 1)
#test2 = subset(test, clusterTest == 2)
#test3 = subset(test, clusterTest == 3)
#predtest1 = predict(log1, newdata = test1, type = "response")
#predtest2 = predict(log2, newdata = test2, type = "response")
#predtest3 = predict(log3, newdata = test3, type = "response")
#predtest = c(predtest1, predtest2, predtest3)
#ids = c(test1$UniqueID, test2$UniqueID, test3$UniqueID)

#ipad5Submission = data.frame(UniqueID = ipad5test$UniqueID, Probability1 = ipad5predtest)
#clusterSubmission = data.frame(UniqueID = ids, Probability1 = predtest)
#MySubmission = rbind(ipad5Submission, clusterSubmission)
#write.csv(MySubmission, "SubmissionApp9.csv", row.names=FALSE)

###APPROACH 10
#Creating a random forest for 4 clusters, not using text variables

#train = eBayTrain
#train$desc.condition = NULL
#train$desc.cosmetic = NULL
#train$desc.good = NULL
#train$desc.great = NULL
#train$desc.minor = NULL
#train$desc.new = NULL
#train$desc.scratches = NULL
#train$desc.screen = NULL
#train$desc.used = NULL
#train$desc.working = NULL
#train$wordcount = NULL

#limitedtrain = train
#limitedtrain$startprice = NULL
#limitedtrain$sold = NULL
#limitedtrain$UniqueID = NULL
#limitedtrain = acm.disjonctif(limitedtrain)

#test = eBayFinalTest
#test$desc.condition = NULL
#test$desc.cosmetic = NULL
#test$desc.good = NULL
#test$desc.great = NULL
#test$desc.minor = NULL
#test$desc.new = NULL
#test$desc.scratches = NULL
#test$desc.screen = NULL
#test$desc.used = NULL
#test$desc.working = NULL
#test$wordcount = NULL
#limitedtest = test
#limitedtest$startprice = NULL
#limitedtest$UniqueID = NULL
#limitedtest = acm.disjonctif(limitedtest)

#km = kmeans(limitedtrain, centers = 4)
#km.kcca = as.kcca(km, limitedtrain)
#clusterTrain = predict(km.kcca)
#train1 = subset(train, clusterTrain == 1)
#train2 = subset(train, clusterTrain == 2)
#train3 = subset(train, clusterTrain == 3)
#train4 = subset(train, clusterTrain == 4)
#rf1 = randomForest(sold ~ . -UniqueID, data = train1)
#rf2 = randomForest(sold ~ . -UniqueID, data = train2)
#rf3 = randomForest(sold ~ . -UniqueID, data = train3)
#rf4 = randomForest(sold ~ . -UniqueID, data = train4)
#predtrain1 = predict(rf1, type = "prob")[,2]
#predtrain2 = predict(rf2, type = "prob")[,2]
#predtrain3 = predict(rf3, type = "prob")[,2]
#predtrain4 = predict(rf4, type = "prob")[,2]
#predtrain = c(predtrain1, predtrain2, predtrain3, predtrain4)
#trainoutcomes = c(train1$sold, train2$sold, train3$sold, train4$sold)
#table(trainoutcomes, predtrain > 0.5)

#Making predictions on eBayFinalTest using APPROACH 10 and preparing submission file for Kaggle
#clusterTest = predict(km.kcca, newdata = limitedtest)
#test1 = subset(test, clusterTest == 1)
#test2 = subset(test, clusterTest == 2)
#test3 = subset(test, clusterTest == 3)
#test4 = subset(test, clusterTest == 4)
#predtest1 = predict(rf1, newdata = test1, type = "prob")[,2]
#predtest2 = predict(rf2, newdata = test2, type = "prob")[,2]
#predtest3 = predict(rf3, newdata = test3, type = "prob")[,2]
#predtest4 = predict(rf4, newdata = test4, type = "prob")[,2]
#predtest = c(predtest1, predtest2, predtest3, predtest4)
#ids = c(test1$UniqueID, test2$UniqueID, test3$UniqueID, test4$UniqueID)

#MySubmission = data.frame(UniqueID = ids, Probability1 = predtest)
#write.csv(MySubmission, "SubmissionApp10.csv", row.names=FALSE)

###APPROACH 11
#Creating a random forest for 4 clusters, not using text variables (except nodesc)
#eBayTrain$nodesc = ifelse(read.csv("eBayiPadTrain.csv", stringsAsFactors=FALSE)$description == "", 1, 0)
#eBayFinalTest$nodesc = ifelse(read.csv("eBayiPadTest.csv", stringsAsFactors=FALSE)$description == "", 1, 0)
#train = eBayTrain
#train$desc.condition = NULL
#train$desc.cosmetic = NULL
#train$desc.good = NULL
#train$desc.great = NULL
#train$desc.minor = NULL
#train$desc.new = NULL
#train$desc.scratches = NULL
#train$desc.screen = NULL
#train$desc.used = NULL
#train$desc.working = NULL
#train$wordcount = NULL

#limitedtrain = train
#limitedtrain$startprice = NULL
#limitedtrain$sold = NULL
#limitedtrain$UniqueID = NULL
#limitedtrain = acm.disjonctif(limitedtrain)

#test = eBayFinalTest
#test$desc.condition = NULL
#test$desc.cosmetic = NULL
#test$desc.good = NULL
#test$desc.great = NULL
#test$desc.minor = NULL
#test$desc.new = NULL
#test$desc.scratches = NULL
#test$desc.screen = NULL
#test$desc.used = NULL
#test$desc.working = NULL
#test$wordcount = NULL
#limitedtest = test
#limitedtest$startprice = NULL
#limitedtest$UniqueID = NULL
#limitedtest = acm.disjonctif(limitedtest)

#km = kmeans(limitedtrain, centers = 4)
#km.kcca = as.kcca(km, limitedtrain)
#clusterTrain = predict(km.kcca)
#train1 = subset(train, clusterTrain == 1)
#train2 = subset(train, clusterTrain == 2)
#train3 = subset(train, clusterTrain == 3)
#train4 = subset(train, clusterTrain == 4)
#rf1 = randomForest(sold ~ . -UniqueID, data = train1)
#rf2 = randomForest(sold ~ . -UniqueID, data = train2)
#rf3 = randomForest(sold ~ . -UniqueID, data = train3)
#rf4 = randomForest(sold ~ . -UniqueID, data = train4)
#predtrain1 = predict(rf1, type = "prob")[,2]
#predtrain2 = predict(rf2, type = "prob")[,2]
#predtrain3 = predict(rf3, type = "prob")[,2]
#predtrain4 = predict(rf4, type = "prob")[,2]
#predtrain = c(predtrain1, predtrain2, predtrain3, predtrain4)
#trainoutcomes = c(train1$sold, train2$sold, train3$sold, train4$sold)
#table(trainoutcomes, predtrain > 0.5)

#Making predictions on eBayFinalTest using APPROACH 11 and preparing submission file for Kaggle
#clusterTest = predict(km.kcca, newdata = limitedtest)
#test1 = subset(test, clusterTest == 1)
#test2 = subset(test, clusterTest == 2)
#test3 = subset(test, clusterTest == 3)
#test4 = subset(test, clusterTest == 4)
#predtest1 = predict(rf1, newdata = test1, type = "prob")[,2]
#predtest2 = predict(rf2, newdata = test2, type = "prob")[,2]
#predtest3 = predict(rf3, newdata = test3, type = "prob")[,2]
#predtest4 = predict(rf4, newdata = test4, type = "prob")[,2]
#predtest = c(predtest1, predtest2, predtest3, predtest4)
#ids = c(test1$UniqueID, test2$UniqueID, test3$UniqueID, test4$UniqueID)

#MySubmission = data.frame(UniqueID = ids, Probability1 = predtest)
#write.csv(MySubmission, "SubmissionApp11.csv", row.names=FALSE)

###APPROACH 12
#Creating a random forest for 4 clusters, not using text variables, except desc.working, desc.condition and wordcount

#train = eBayTrain

#train$desc.good = NULL
#train$desc.great = NULL
#train$desc.cosmetic = NULL
#train$desc.minor = NULL
#train$desc.scratches = NULL
#train$desc.screen = NULL
#train$desc.new = NULL
#train$desc.used = NULL


#limitedtrain = train
#limitedtrain$desc.condition = NULL
#limitedtrain$desc.cosmetic = NULL
#limitedtrain$desc.good = NULL
#limitedtrain$desc.great = NULL
#limitedtrain$desc.minor = NULL
#limitedtrain$desc.new = NULL
#limitedtrain$desc.scratches = NULL
#limitedtrain$desc.screen = NULL
#limitedtrain$desc.used = NULL
#limitedtrain$desc.working = NULL
#limitedtrain$wordcount = NULL
#limitedtrain$startprice = NULL
#limitedtrain$sold = NULL
#limitedtrain$UniqueID = NULL
#limitedtrain = acm.disjonctif(limitedtrain)

#test = eBayFinalTest
#limitedtest = test
#limitedtest$desc.condition = NULL
#limitedtest$desc.cosmetic = NULL
#limitedtest$desc.good = NULL
#limitedtest$desc.great = NULL
#limitedtest$desc.minor = NULL
#limitedtest$desc.new = NULL
#limitedtest$desc.scratches = NULL
#limitedtest$desc.screen = NULL
#limitedtest$desc.used = NULL
#limitedtest$desc.working = NULL
#limitedtest$wordcount = NULL
#limitedtest$startprice = NULL
#limitedtest$UniqueID = NULL
#limitedtest = acm.disjonctif(limitedtest)

#km = kmeans(limitedtrain, centers = 4)
#km.kcca = as.kcca(km, limitedtrain)
#clusterTrain = predict(km.kcca)

#train1 = subset(train, clusterTrain == 1)
#train2 = subset(train, clusterTrain == 2)
#train3 = subset(train, clusterTrain == 3)
#train4 = subset(train, clusterTrain == 4)

#rf1 = randomForest(sold ~ . -UniqueID, data = train1)
#rf2 = randomForest(sold ~ . -UniqueID, data = train2)
#rf3 = randomForest(sold ~ . -UniqueID, data = train3)
#rf4 = randomForest(sold ~ . -UniqueID, data = train4)
#predtrain1 = predict(rf1, type = "prob")[,2]
#predtrain2 = predict(rf2, type = "prob")[,2]
#predtrain3 = predict(rf3, type = "prob")[,2]
#predtrain4 = predict(rf4, type = "prob")[,2]
#predtrain = c(predtrain1, predtrain2, predtrain3, predtrain4)
#trainoutcomes = c(train1$sold, train2$sold, train3$sold, train4$sold)
#table(trainoutcomes, predtrain > 0.5)
#predictionTrain = prediction(predtrain, trainoutcomes)
#performance(predictionTrain, "auc")@y.values

#Making predictions on eBayFinalTest using APPROACH 12 and preparing submission file for Kaggle
#clusterTest = predict(km.kcca, newdata = limitedtest)
#test1 = subset(test, clusterTest == 1)
#test2 = subset(test, clusterTest == 2)
#test3 = subset(test, clusterTest == 3)
#test4 = subset(test, clusterTest == 4)
#predtest1 = predict(rf1, newdata = test1, type = "prob")[,2]
#predtest2 = predict(rf2, newdata = test2, type = "prob")[,2]
#predtest3 = predict(rf3, newdata = test3, type = "prob")[,2]
#predtest4 = predict(rf4, newdata = test4, type = "prob")[,2]
#predtest = c(predtest1, predtest2, predtest3, predtest4)
#ids = c(test1$UniqueID, test2$UniqueID, test3$UniqueID, test4$UniqueID)

#MySubmission = data.frame(UniqueID = ids, Probability1 = predtest)
#write.csv(MySubmission, "SubmissionApp12.csv", row.names=FALSE)

###APPROACH 13
#Creating a random forest and a logistic regression and taking the average probabilites
#Not using text variables, except desc.working, desc.condition and wordcount

#train = eBayTrain

#train$desc.good = NULL
#train$desc.great = NULL
#train$desc.cosmetic = NULL
#train$desc.minor = NULL
#train$desc.scratches = NULL
#train$desc.screen = NULL
#train$desc.new = NULL
#train$desc.used = NULL

#rf = randomForest(sold ~ . -UniqueID, data = train)
#log = glm(sold ~ .-UniqueID, data = train, family = "binomial")
#pred_train_rf = predict(rf, type = "prob")[,2]
#pred_train_log = predict(log, type = "response")
#pred_train = (pred_train_rf + pred_train_log) / 2
#table(train$sold, pred_train > 0.5)
#performance(prediction(pred_train, train$sold), "auc")@y.values

#test = eBayFinalTest
#pred_test_rf = predict(rf, newdata = test, type = "prob")[,2]
#pred_test_log = predict(log, newdata = test, type = "response")
#pred_test = (pred_test_rf + pred_test_log) / 2
#MySubmission = data.frame(UniqueID = test$UniqueID, Probability1 = pred_test)
#write.csv(MySubmission, "SubmissionApp13.csv", row.names=FALSE)

###APPROACH 14
#Creating a random forest and a logistic regression for 4 clusters, not using text variables, except desc.working, desc.condition and wordcount
#Final predictions are the average of random forest and regression model

#train = eBayTrain

#train$desc.good = NULL
#train$desc.great = NULL
#train$desc.cosmetic = NULL
#train$desc.minor = NULL
#train$desc.scratches = NULL
#train$desc.screen = NULL
#train$desc.new = NULL
#train$desc.used = NULL

#limitedtrain = train
#limitedtrain$desc.condition = NULL
#limitedtrain$desc.cosmetic = NULL
#limitedtrain$desc.good = NULL
#limitedtrain$desc.great = NULL
#limitedtrain$desc.minor = NULL
#limitedtrain$desc.new = NULL
#limitedtrain$desc.scratches = NULL
#limitedtrain$desc.screen = NULL
#limitedtrain$desc.used = NULL
#limitedtrain$desc.working = NULL
#limitedtrain$wordcount = NULL
#limitedtrain$startprice = NULL
#limitedtrain$sold = NULL
#limitedtrain$UniqueID = NULL
#limitedtrain = acm.disjonctif(limitedtrain)

#test = eBayFinalTest
#limitedtest = test
#limitedtest$desc.condition = NULL
#limitedtest$desc.cosmetic = NULL
#limitedtest$desc.good = NULL
#limitedtest$desc.great = NULL
#limitedtest$desc.minor = NULL
#limitedtest$desc.new = NULL
#limitedtest$desc.scratches = NULL
#limitedtest$desc.screen = NULL
#limitedtest$desc.used = NULL
#limitedtest$desc.working = NULL
#limitedtest$wordcount = NULL
#limitedtest$startprice = NULL
#limitedtest$UniqueID = NULL
#limitedtest = acm.disjonctif(limitedtest)

#km = kmeans(limitedtrain, centers = 4)
#km.kcca = as.kcca(km, limitedtrain)
#clusterTrain = predict(km.kcca)

#train1 = subset(train, clusterTrain == 1)
#train2 = subset(train, clusterTrain == 2)
#train3 = subset(train, clusterTrain == 3)
#train4 = subset(train, clusterTrain == 4)

#sort(colMeans(subset(limitedtrain, clusterTrain == 1)))
#sort(colMeans(subset(limitedtrain, clusterTrain == 2)))
#sort(colMeans(subset(limitedtrain, clusterTrain == 3)))
#sort(colMeans(subset(limitedtrain, clusterTrain == 4)))

#rf1 = randomForest(sold ~ . -UniqueID, data = train1)
#rf2 = randomForest(sold ~ . -UniqueID, data = train2)
#rf3 = randomForest(sold ~ . -UniqueID, data = train3)
#rf4 = randomForest(sold ~ . -UniqueID, data = train4)
#log1 = glm(sold ~ . -UniqueID, data = train1[, sapply(droplevels(train1), nlevels) > 1 | !sapply(train1, is.factor)], family = "binomial")
#log2 = glm(sold ~ . -UniqueID, data = train2[, sapply(droplevels(train2), nlevels) > 1 | !sapply(train2, is.factor)], family = "binomial")
#log3 = glm(sold ~ . -UniqueID, data = train3[, sapply(droplevels(train3), nlevels) > 1 | !sapply(train3, is.factor)], family = "binomial")
#log4 = glm(sold ~ . -UniqueID, data = train4[, sapply(droplevels(train4), nlevels) > 1 | !sapply(train4, is.factor)], family = "binomial")

#predtrain1 = (predict(rf1, type = "prob")[,2] + predict(log1, type = "response")) / 2
#predtrain2 = (predict(rf2, type = "prob")[,2] + predict(log2, type = "response")) / 2
#predtrain3 = (predict(rf3, type = "prob")[,2] + predict(log3, type = "response")) / 2
#predtrain4 = (predict(rf4, type = "prob")[,2] + predict(log4, type = "response")) / 2
#predtrain = c(predtrain1, predtrain2, predtrain3, predtrain4)
#trainoutcomes = c(train1$sold, train2$sold, train3$sold, train4$sold)
#table(trainoutcomes, predtrain > 0.5)
#predictionTrain = prediction(predtrain, trainoutcomes)
#performance(predictionTrain, "auc")@y.values

#Making predictions on eBayFinalTest using APPROACH 14 and preparing submission file for Kaggle
#clusterTest = predict(km.kcca, newdata = limitedtest)
#test1 = subset(test, clusterTest == 1)
#test2 = subset(test, clusterTest == 2)
#test3 = subset(test, clusterTest == 3)
#test4 = subset(test, clusterTest == 4)

#You have to change new factor levels to "Unknown" (example: test1[test1 == "iPad 5"] = "Unknown")
#predtest1 = (predict(rf1, newdata = test1, type = "prob")[,2] + predict(log1, newdata = test1, type = "response")) / 2
#predtest2 = (predict(rf2, newdata = test2, type = "prob")[,2] + predict(log2, newdata = test2, type = "response")) / 2
#predtest3 = (predict(rf3, newdata = test3, type = "prob")[,2] + predict(log3, newdata = test3, type = "response")) / 2
#predtest4 = (predict(rf4, newdata = test4, type = "prob")[,2] + predict(log4, newdata = test4, type = "response")) / 2

#predtest = c(predtest1, predtest2, predtest3, predtest4)
#ids = c(test1$UniqueID, test2$UniqueID, test3$UniqueID, test4$UniqueID)

#MySubmission = data.frame(UniqueID = ids, Probability1 = predtest)
#write.csv(MySubmission, "SubmissionApp14.csv", row.names=FALSE)

###APPROACH 15
#Creating a random forest and a logistic regression and taking the average probabilites

train = eBayTrain

#rf = randomForest(sold ~ . -UniqueID, data = train)
#log = glm(sold ~ .-UniqueID, data = train, family = "binomial")
#pred_train_rf = predict(rf, type = "prob")[,2]
#pred_train_log = predict(log, type = "response")
#pred_train = (pred_train_rf + pred_train_log) / 2
#table(train$sold, pred_train > 0.5)
#performance(prediction(pred_train, train$sold), "auc")@y.values

#test = eBayFinalTest
#pred_test_rf = predict(rf, newdata = test, type = "prob")[,2]
#pred_test_log = predict(log, newdata = test, type = "response")
#pred_test = (pred_test_rf + pred_test_log) / 2
#MySubmission = data.frame(UniqueID = test$UniqueID, Probability1 = pred_test)
#write.csv(MySubmission, "SubmissionApp15.csv", row.names=FALSE)

###APPROACH 16 - Winner
#Creating a random forest and a logistic regression for 4 clusters
#Final predictions are the average of random forest and regression model predictions

#train = eBayTrain

#limitedtrain = train
#limitedtrain$desc.condition = NULL
#limitedtrain$desc.cosmetic = NULL
#limitedtrain$desc.good = NULL
#limitedtrain$desc.great = NULL
#limitedtrain$desc.minor = NULL
#limitedtrain$desc.new = NULL
#limitedtrain$desc.scratches = NULL
#limitedtrain$desc.screen = NULL
#limitedtrain$desc.used = NULL
#limitedtrain$desc.working = NULL
#limitedtrain$wordcount = NULL
#limitedtrain$startprice = NULL
#limitedtrain$sold = NULL
#limitedtrain$UniqueID = NULL
#limitedtrain = acm.disjonctif(limitedtrain)

#test = eBayFinalTest
#limitedtest = test
#limitedtest$desc.condition = NULL
#limitedtest$desc.cosmetic = NULL
#limitedtest$desc.good = NULL
#limitedtest$desc.great = NULL
#limitedtest$desc.minor = NULL
#limitedtest$desc.new = NULL
#limitedtest$desc.scratches = NULL
#limitedtest$desc.screen = NULL
#limitedtest$desc.used = NULL
#limitedtest$desc.working = NULL
#limitedtest$wordcount = NULL
#limitedtest$startprice = NULL
#limitedtest$UniqueID = NULL
#limitedtest = acm.disjonctif(limitedtest)

#km = kmeans(limitedtrain, centers = 4)
#km.kcca = as.kcca(km, limitedtrain)
#clusterTrain = predict(km.kcca)

#train1 = subset(train, clusterTrain == 1)
#train2 = subset(train, clusterTrain == 2)
#train3 = subset(train, clusterTrain == 3)
#train4 = subset(train, clusterTrain == 4)

#sort(colMeans(subset(limitedtrain, clusterTrain == 1)))
#sort(colMeans(subset(limitedtrain, clusterTrain == 2)))
#sort(colMeans(subset(limitedtrain, clusterTrain == 3)))
#sort(colMeans(subset(limitedtrain, clusterTrain == 4)))

#rf1 = randomForest(sold ~ . -UniqueID, data = train1)
#rf2 = randomForest(sold ~ . -UniqueID, data = train2)
#rf3 = randomForest(sold ~ . -UniqueID, data = train3)
#rf4 = randomForest(sold ~ . -UniqueID, data = train4)
#log1 = glm(sold ~ . -UniqueID, data = train1[, sapply(droplevels(train1), nlevels) > 1 | !sapply(train1, is.factor)], family = "binomial")
#log2 = glm(sold ~ . -UniqueID, data = train2[, sapply(droplevels(train2), nlevels) > 1 | !sapply(train2, is.factor)], family = "binomial")
#log3 = glm(sold ~ . -UniqueID, data = train3[, sapply(droplevels(train3), nlevels) > 1 | !sapply(train3, is.factor)], family = "binomial")
#log4 = glm(sold ~ . -UniqueID, data = train4[, sapply(droplevels(train4), nlevels) > 1 | !sapply(train4, is.factor)], family = "binomial")

#predtrain1 = (predict(rf1, type = "prob")[,2] + predict(log1, type = "response")) / 2
#predtrain2 = (predict(rf2, type = "prob")[,2] + predict(log2, type = "response")) / 2
#predtrain3 = (predict(rf3, type = "prob")[,2] + predict(log3, type = "response")) / 2
#predtrain4 = (predict(rf4, type = "prob")[,2] + predict(log4, type = "response")) / 2
#predtrain = c(predtrain1, predtrain2, predtrain3, predtrain4)
#trainoutcomes = c(train1$sold, train2$sold, train3$sold, train4$sold)
#table(trainoutcomes, predtrain > 0.5)
#predictionTrain = prediction(predtrain, trainoutcomes)
#performance(predictionTrain, "auc")@y.values

#Making predictions on eBayFinalTest using APPROACH 14 and preparing submission file for Kaggle
#clusterTest = predict(km.kcca, newdata = limitedtest)
#test1 = subset(test, clusterTest == 1)
#test2 = subset(test, clusterTest == 2)
#test3 = subset(test, clusterTest == 3)
#test4 = subset(test, clusterTest == 4)

#You have to change new factor levels to "Unknown" (example: test1[test1 == "iPad 5"] = "Unknown")
#predtest1 = (predict(rf1, newdata = test1, type = "prob")[,2] + predict(log1, newdata = test1, type = "response")) / 2
#predtest2 = (predict(rf2, newdata = test2, type = "prob")[,2] + predict(log2, newdata = test2, type = "response")) / 2
#predtest3 = (predict(rf3, newdata = test3, type = "prob")[,2] + predict(log3, newdata = test3, type = "response")) / 2
#predtest4 = (predict(rf4, newdata = test4, type = "prob")[,2] + predict(log4, newdata = test4, type = "response")) / 2

#predtest = c(predtest1, predtest2, predtest3, predtest4)
#ids = c(test1$UniqueID, test2$UniqueID, test3$UniqueID, test4$UniqueID)

#MySubmission = data.frame(UniqueID = ids, Probability1 = predtest)
#write.csv(MySubmission, "SubmissionApp16.2.csv", row.names=FALSE)

###APPROACH 17
#Creating a random forest and a logistic regression for 5 clusters
#Final predictions are the average of random forest and regression model predictions

#train = eBayTrain

#limitedtrain = train
#limitedtrain$desc.condition = NULL
#limitedtrain$desc.cosmetic = NULL
#limitedtrain$desc.good = NULL
#limitedtrain$desc.great = NULL
#limitedtrain$desc.minor = NULL
#limitedtrain$desc.new = NULL
#limitedtrain$desc.scratches = NULL
#limitedtrain$desc.screen = NULL
#limitedtrain$desc.used = NULL
#limitedtrain$desc.working = NULL
#limitedtrain$wordcount = NULL
#limitedtrain$startprice = NULL
#limitedtrain$sold = NULL
#limitedtrain$UniqueID = NULL
#limitedtrain = acm.disjonctif(limitedtrain)

#test = eBayFinalTest
#limitedtest = test
#limitedtest$desc.condition = NULL
#limitedtest$desc.cosmetic = NULL
#limitedtest$desc.good = NULL
#limitedtest$desc.great = NULL
#limitedtest$desc.minor = NULL
#limitedtest$desc.new = NULL
#limitedtest$desc.scratches = NULL
#limitedtest$desc.screen = NULL
#limitedtest$desc.used = NULL
#limitedtest$desc.working = NULL
#limitedtest$wordcount = NULL
#limitedtest$startprice = NULL
#limitedtest$UniqueID = NULL
#limitedtest = acm.disjonctif(limitedtest)

#km = kmeans(limitedtrain, centers = 5)
#km.kcca = as.kcca(km, limitedtrain)
#clusterTrain = predict(km.kcca)

#train1 = subset(train, clusterTrain == 1)
#train2 = subset(train, clusterTrain == 2)
#train3 = subset(train, clusterTrain == 3)
#train4 = subset(train, clusterTrain == 4)
#train5 = subset(train, clusterTrain == 5)

#sort(colMeans(subset(limitedtrain, clusterTrain == 1)))
#sort(colMeans(subset(limitedtrain, clusterTrain == 2)))
#sort(colMeans(subset(limitedtrain, clusterTrain == 3)))
#sort(colMeans(subset(limitedtrain, clusterTrain == 4)))
#sort(colMeans(subset(limitedtrain, clusterTrain == 5)))

#rf1 = randomForest(sold ~ . -UniqueID, data = train1)
#rf2 = randomForest(sold ~ . -UniqueID, data = train2)
#rf3 = randomForest(sold ~ . -UniqueID, data = train3)
#rf4 = randomForest(sold ~ . -UniqueID, data = train4)
#rf5 = randomForest(sold ~ . -UniqueID, data = train5)
#log1 = glm(sold ~ . -UniqueID, data = train1[, sapply(droplevels(train1), nlevels) > 1 | !sapply(train1, is.factor)], family = "binomial")
#log2 = glm(sold ~ . -UniqueID, data = train2[, sapply(droplevels(train2), nlevels) > 1 | !sapply(train2, is.factor)], family = "binomial")
#log3 = glm(sold ~ . -UniqueID, data = train3[, sapply(droplevels(train3), nlevels) > 1 | !sapply(train3, is.factor)], family = "binomial")
#log4 = glm(sold ~ . -UniqueID, data = train4[, sapply(droplevels(train4), nlevels) > 1 | !sapply(train4, is.factor)], family = "binomial")
#log5 = glm(sold ~ . -UniqueID, data = train5[, sapply(droplevels(train5), nlevels) > 1 | !sapply(train5, is.factor)], family = "binomial")

#predtrain1 = (predict(rf1, type = "prob")[,2] + predict(log1, type = "response")) / 2
#predtrain2 = (predict(rf2, type = "prob")[,2] + predict(log2, type = "response")) / 2
#predtrain3 = (predict(rf3, type = "prob")[,2] + predict(log3, type = "response")) / 2
#predtrain4 = (predict(rf4, type = "prob")[,2] + predict(log4, type = "response")) / 2
#predtrain5 = (predict(rf5, type = "prob")[,2] + predict(log5, type = "response")) / 2
#predtrain = c(predtrain1, predtrain2, predtrain3, predtrain4, predtrain5)
#trainoutcomes = c(train1$sold, train2$sold, train3$sold, train4$sold, train5$sold)
#table(trainoutcomes, predtrain > 0.5)
#predictionTrain = prediction(predtrain, trainoutcomes)
#performance(predictionTrain, "auc")@y.values

#Making predictions on eBayFinalTest using APPROACH 17 and preparing submission file for Kaggle
#clusterTest = predict(km.kcca, newdata = limitedtest)
#test1 = subset(test, clusterTest == 1)
#test2 = subset(test, clusterTest == 2)
#test3 = subset(test, clusterTest == 3)
#test4 = subset(test, clusterTest == 4)
#test5 = subset(test, clusterTest == 5)

#You have to change new factor levels to "Unknown" (example: test1[test1 == "iPad 5"] = "Unknown")
#predtest1 = (predict(rf1, newdata = test1, type = "prob")[,2] + predict(log1, newdata = test1, type = "response")) / 2
#predtest2 = (predict(rf2, newdata = test2, type = "prob")[,2] + predict(log2, newdata = test2, type = "response")) / 2
#predtest3 = (predict(rf3, newdata = test3, type = "prob")[,2] + predict(log3, newdata = test3, type = "response")) / 2
#predtest4 = (predict(rf4, newdata = test4, type = "prob")[,2] + predict(log4, newdata = test4, type = "response")) / 2
#predtest5 = (predict(rf5, newdata = test5, type = "prob")[,2] + predict(log5, newdata = test5, type = "response")) / 2

#predtest = c(predtest1, predtest2, predtest3, predtest4, predtest5)
#ids = c(test1$UniqueID, test2$UniqueID, test3$UniqueID, test4$UniqueID, test5$UniqueID)

#MySubmission = data.frame(UniqueID = ids, Probability1 = predtest)
#write.csv(MySubmission, "SubmissionApp17.csv", row.names=FALSE)

###APPROACH 18
#Creating a random forest and a logistic regression using important variables only and taking the average probabilites

#train = eBayTrain

#rf = randomForest(sold ~ wordcount + biddable + startprice + condition + color + storage + productline, data = train)
#log = glm(sold ~ desc.working + biddable + startprice + condition + storage + productline, data = train, family = "binomial")
#pred_train_rf = predict(rf, type = "prob")[,2]
#pred_train_log = predict(log, type = "response")
#pred_train = (pred_train_rf + pred_train_log) / 2
#table(train$sold, pred_train > 0.5)
#performance(prediction(pred_train, train$sold), "auc")@y.values

#test = eBayFinalTest
#pred_test_rf = predict(rf, newdata = test, type = "prob")[,2]
#pred_test_log = predict(log, newdata = test, type = "response")
#pred_test = (pred_test_rf + pred_test_log) / 2
#MySubmission = data.frame(UniqueID = test$UniqueID, Probability1 = pred_test)
#write.csv(MySubmission, "SubmissionApp18.csv", row.names=FALSE)

#APPROACH 19
#Creating a logistic regression using important variables only

#train = eBayTrain

#log = glm(sold ~ desc.working + biddable + startprice + condition + storage + productline, data = train, family = "binomial")
#pred_train = predict(log, type = "response")
#table(train$sold, pred_train > 0.5)
#performance(prediction(pred_train, train$sold), "auc")@y.values

#test = eBayFinalTest
#pred_test = predict(log, newdata = test, type = "response")
#MySubmission = data.frame(UniqueID = test$UniqueID, Probability1 = pred_test)
#write.csv(MySubmission, "SubmissionApp19.csv", row.names=FALSE)

#APPROACH 20
#Creating a random forest using important variables only

#train = eBayTrain

#rf = randomForest(sold ~ wordcount + biddable + startprice + condition + color + storage + productline, data = train)
#pred_train = predict(rf, type = "prob")[,2]
#table(train$sold, pred_train > 0.5)
#performance(prediction(pred_train, train$sold), "auc")@y.values

#test = eBayFinalTest
#pred_test = predict(rf, newdata = test, type = "prob")[,2]
#MySubmission = data.frame(UniqueID = test$UniqueID, Probability1 = pred_test)
#write.csv(MySubmission, "SubmissionApp20.csv", row.names=FALSE)

#APPROACH 21
#Creating a random forest using modified variables and imputed data

#train = eBayTrain
#test = eBayFinalTest

#Imputation in training and testing sets
#simple = rbind(train[c("color", "storage", "productline")],test[c("color", "storage", "productline")])
#simple[simple == "Unknown"] = NA
#imputed = complete(mice(simple))

#imputed_train = head(imputed, nrow(train))
#train[c("color", "storage", "productline")] = imputed_train

#imputed_test = tail(imputed, nrow(test))
#test[c("color", "storage", "productline")] = imputed_test

#Modifying variables
#train$numstorage = as.numeric(as.character(train$storage))
#test$numstorage = as.numeric(as.character(test$storage))

#train$releaseyear[train$productline == "iPad 1"] = 2010
#train$releaseyear[train$productline == "iPad 2"] = 2011
#train$releaseyear[train$productline == "iPad 3"] = 2012
#train$releaseyear[train$productline == "iPad 4"] = 2013
#train$releaseyear[train$productline == "iPad 5"] = 2013
#train$releaseyear[train$productline == "iPad Air"] = 2013
#train$releaseyear[train$productline == "iPad Air 2"] = 2014
#train$releaseyear[train$productline == "iPad mini"] = 2012
#train$releaseyear[train$productline == "iPad mini 2"] = 2013
#train$releaseyear[train$productline == "iPad mini 3"] = 2014
#train$releaseyear[train$productline == "iPad mini Retina"] = 2013

#test$releaseyear[test$productline == "iPad 1"] = 2010
#test$releaseyear[test$productline == "iPad 2"] = 2011
#test$releaseyear[test$productline == "iPad 3"] = 2012
#test$releaseyear[test$productline == "iPad 4"] = 2013
#test$releaseyear[test$productline == "iPad 5"] = 2013
#test$releaseyear[test$productline == "iPad Air"] = 2013
#test$releaseyear[test$productline == "iPad Air 2"] = 2014
#test$releaseyear[test$productline == "iPad mini"] = 2012
#test$releaseyear[test$productline == "iPad mini 2"] = 2013
#test$releaseyear[test$productline == "iPad mini 3"] = 2014
#test$releaseyear[test$productline == "iPad mini Retina"] = 2013

#rf = randomForest(sold ~ wordcount + biddable + startprice + condition + releaseyear, data = train)
#pred_train = predict(rf, type = "prob")[,2]
#table(train$sold, pred_train > 0.5)
#performance(prediction(pred_train, train$sold), "auc")@y.values

#pred_test = predict(rf, newdata = test, type = "prob")[,2]
#MySubmission = data.frame(UniqueID = test$UniqueID, Probability1 = pred_test)
#write.csv(MySubmission, "SubmissionApp21.2.csv", row.names=FALSE)

#APPROACH 22
#Creating a logistic regression using modified variables and imputed data

#train = eBayTrain
#test = eBayFinalTest

#Imputation in training and testing sets
#simple = rbind(train[c("color", "storage", "productline")],test[c("color", "storage", "productline")])
#simple[simple == "Unknown"] = NA
#imputed = complete(mice(simple))

#imputed_train = head(imputed, nrow(train))
#train[c("color", "storage", "productline")] = imputed_train

#imputed_test = tail(imputed, nrow(test))
#test[c("color", "storage", "productline")] = imputed_test

#Modifying variables
#train$numstorage = as.numeric(as.character(train$storage))
#test$numstorage = as.numeric(as.character(test$storage))

#train$releaseyear[train$productline == "iPad 1"] = 2010
#train$releaseyear[train$productline == "iPad 2"] = 2011
#train$releaseyear[train$productline == "iPad 3"] = 2012
#train$releaseyear[train$productline == "iPad 4"] = 2013
#train$releaseyear[train$productline == "iPad 5"] = 2013
#train$releaseyear[train$productline == "iPad Air"] = 2013
#train$releaseyear[train$productline == "iPad Air 2"] = 2014
#train$releaseyear[train$productline == "iPad mini"] = 2012
#train$releaseyear[train$productline == "iPad mini 2"] = 2013
#train$releaseyear[train$productline == "iPad mini 3"] = 2014
#train$releaseyear[train$productline == "iPad mini Retina"] = 2013

#test$releaseyear[test$productline == "iPad 1"] = 2010
#test$releaseyear[test$productline == "iPad 2"] = 2011
#test$releaseyear[test$productline == "iPad 3"] = 2012
#test$releaseyear[test$productline == "iPad 4"] = 2013
#test$releaseyear[test$productline == "iPad 5"] = 2013
#test$releaseyear[test$productline == "iPad Air"] = 2013
#test$releaseyear[test$productline == "iPad Air 2"] = 2014
#test$releaseyear[test$productline == "iPad mini"] = 2012
#test$releaseyear[test$productline == "iPad mini 2"] = 2013
#test$releaseyear[test$productline == "iPad mini 3"] = 2014
#test$releaseyear[test$productline == "iPad mini Retina"] = 2013

#log = glm(sold ~ wordcount + biddable + startprice + condition + numstorage + releaseyear, data = train, family = "binomial")
#pred_train = predict(log, type = "response")
#table(train$sold, pred_train > 0.5)
#performance(prediction(pred_train, train$sold), "auc")@y.values

#pred_test = predict(log, newdata = test, type = "response")
#MySubmission = data.frame(UniqueID = test$UniqueID, Probability1 = pred_test)
#write.csv(MySubmission, "SubmissionApp22.csv", row.names=FALSE)

#APPROACH 23
#Creating a random forest and a logistic regression using modified variables and imputed data
#Taking the average probabilites

#train = eBayTrain
#test = eBayFinalTest

#Imputation in training and testing sets
#simple = rbind(train[c("color", "storage", "productline")],test[c("color", "storage", "productline")])
#simple[simple == "Unknown"] = NA
#imputed = complete(mice(simple))

#imputed_train = head(imputed, nrow(train))
#train[c("color", "storage", "productline")] = imputed_train

#imputed_test = tail(imputed, nrow(test))
#test[c("color", "storage", "productline")] = imputed_test

#Modifying variables
#train$numstorage = as.numeric(as.character(train$storage))
#test$numstorage = as.numeric(as.character(test$storage))

#train$releaseyear[train$productline == "iPad 1"] = 2010
#train$releaseyear[train$productline == "iPad 2"] = 2011
#train$releaseyear[train$productline == "iPad 3"] = 2012
#train$releaseyear[train$productline == "iPad 4"] = 2013
#train$releaseyear[train$productline == "iPad 5"] = 2013
#train$releaseyear[train$productline == "iPad Air"] = 2013
#train$releaseyear[train$productline == "iPad Air 2"] = 2014
#train$releaseyear[train$productline == "iPad mini"] = 2012
#train$releaseyear[train$productline == "iPad mini 2"] = 2013
#train$releaseyear[train$productline == "iPad mini 3"] = 2014
#train$releaseyear[train$productline == "iPad mini Retina"] = 2013

#test$releaseyear[test$productline == "iPad 1"] = 2010
#test$releaseyear[test$productline == "iPad 2"] = 2011
#test$releaseyear[test$productline == "iPad 3"] = 2012
#test$releaseyear[test$productline == "iPad 4"] = 2013
#test$releaseyear[test$productline == "iPad 5"] = 2013
#test$releaseyear[test$productline == "iPad Air"] = 2013
#test$releaseyear[test$productline == "iPad Air 2"] = 2014
#test$releaseyear[test$productline == "iPad mini"] = 2012
#test$releaseyear[test$productline == "iPad mini 2"] = 2013
#test$releaseyear[test$productline == "iPad mini 3"] = 2014
#test$releaseyear[test$productline == "iPad mini Retina"] = 2013

#log = glm(sold ~ wordcount + biddable + startprice + condition + numstorage + releaseyear, data = train, family = "binomial")
#rf = randomForest(sold ~ wordcount + biddable + startprice + condition + releaseyear, data = train)
#pred_train = (predict(log, type = "response") + predict(rf, type = "prob")[,2]) / 2
#table(train$sold, pred_train > 0.5)
#performance(prediction(pred_train, train$sold), "auc")@y.values

#pred_test = (predict(log, newdata = test, type = "response") + predict(rf, newdata = test, type = "prob")[,2]) / 2
#MySubmission = data.frame(UniqueID = test$UniqueID, Probability1 = pred_test)
#write.csv(MySubmission, "SubmissionApp23.csv", row.names=FALSE)

#APPROACH 24

#train = eBayTrain
#test = eBayFinalTest

#Some products have different names, but they are the same
#train$productline[train$productline == "iPad 5"] = "iPad Air"
#train$productline[train$productline == "iPad mini Retina"] = "iPad mini 2"

#test$productline[test$productline == "iPad 5"] = "iPad Air"
#test$productline[test$productline == "iPad mini Retina"] = "iPad mini 2"

#Imputation in training and testing sets
#simple = rbind(train[c("cellular", "color", "storage", "productline")],test[c("cellular", "color", "storage", "productline")])
#simple[simple == "Unknown"] = NA
#imputed = complete(mice(simple))

#imputed_train = head(imputed, nrow(train))
#train[c("cellular", "color", "storage", "productline")] = imputed_train

#imputed_test = tail(imputed, nrow(test))
#test[c("cellular", "color", "storage", "productline")] = imputed_test

#Modifying variables
#train$numstorage = as.numeric(as.character(train$storage))
#test$numstorage = as.numeric(as.character(test$storage))

#train$generation[train$productline == "iPad 1"] = 1
#train$generation[train$productline == "iPad 2"] = 2
#train$generation[train$productline == "iPad 3"] = 3
#train$generation[train$productline == "iPad 4"] = 4
#train$generation[train$productline == "iPad Air"] = 5
#train$generation[train$productline == "iPad Air 2"] = 6
#train$generation[train$productline == "iPad mini"] = 4
#train$generation[train$productline == "iPad mini 2"] = 5
#train$generation[train$productline == "iPad mini 3"] = 6

#test$generation[test$productline == "iPad 1"] = 1
#test$generation[test$productline == "iPad 2"] = 2
#test$generation[test$productline == "iPad 3"] = 3
#test$generation[test$productline == "iPad 4"] = 4
#test$generation[test$productline == "iPad Air"] = 5
#test$generation[test$productline == "iPad Air 2"] = 6
#test$generation[test$productline == "iPad mini"] = 4
#test$generation[test$productline == "iPad mini 2"] = 5
#test$generation[test$productline == "iPad mini 3"] = 6

#train$mini = ifelse(grepl("mini", train$productline), 1, 0)
#test$mini = ifelse(grepl("mini", test$productline), 1, 0)

#Creating the models and checking performance on training set
#log = glm(sold ~ desc.working + biddable + startprice + condition + numstorage + generation + mini, data = train, family = "binomial")
#summary(log)
#pred_train_log = predict(log, type = "response")
#table(train$sold, pred_train_log > 0.5)
#performance(prediction(pred_train_log, train$sold), "auc")@y.values

#rf = randomForest(sold ~ + biddable + startprice + condition + numstorage + generation, data = train)
#rf$importance
#pred_train_rf = predict(rf, type = "prob")[,2]
#table(train$sold, pred_train_rf > 0.5)
#performance(prediction(pred_train_rf, train$sold), "auc")@y.values

#pred_train = (pred_train_log + pred_train_rf) / 2
#table(train$sold, pred_train > 0.5)
#performance(prediction(pred_train, train$sold), "auc")@y.values

#pred_test = (predict(log, newdata = test, type = "response") + predict(rf, newdata = test, type = "prob")[,2]) / 2
#MySubmission = data.frame(UniqueID = test$UniqueID, Probability1 = pred_test)
#write.csv(MySubmission, "SubmissionApp24.csv", row.names=FALSE)