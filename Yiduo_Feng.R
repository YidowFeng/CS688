library(tm)
library(class)

Doc1.TrainPath <- system.file("texts", "20Newsgroups/20news-bydate-train/sci.space", package = "tm")
Doc1.TestPath <- system.file("texts", "20Newsgroups/20news-bydate-test/sci.space", package = "tm")
Doc2.TrainPath <- system.file("texts", "20Newsgroups/20news-bydate-train/rec.autos", package = "tm")
Doc2.TestPath <- system.file("texts", "20Newsgroups/20news-bydate-test/rec.autos", package = "tm")

Temp1 <- DirSource(Doc1.TrainPath)
Temp2 <- DirSource(Doc1.TestPath)
Temp3 <- DirSource(Doc2.TrainPath)
Temp4 <- DirSource(Doc2.TestPath)

newsgroup1 <- "sci.space"
newsgroup2 <- "rec.autos"
# Obtain the merged Corpus (of 400 documents)
Doc1.Train <- Corpus(URISource(Temp1$filelist[1:100]),readerControl=list(reader=readPlain))
Doc1.Test <- Corpus(URISource(Temp2$filelist[1:100]),readerControl=list(reader=readPlain))
Doc2.Train <- Corpus(URISource(Temp3$filelist[1:100]),readerControl=list(reader=readPlain))
Doc2.Test <- Corpus(URISource(Temp4$filelist[1:100]),readerControl=list(reader=readPlain))

Doc.Corpus <- c(Doc1.Train, Doc1.Test, Doc2.Train, Doc2.Test)
#Implement preprocessing
#transform can remove numbers
transformations <- tm_map(Doc.Corpus, removeNumbers)
#In a similar fashion the punctuation can be removed
transformations <- tm_map(transformations, removePunctuation)
#Stop words are common words found in a language. Words like 
#"for"," very", "and", "of", "are", etc, are
#common stop words in the English language
transformations <- tm_map(transformations, removeWords, stopwords("english"))
#stemming
transformations <- tm_map(transformations, stemDocument)

#Create the Document-Term Matrix
Document_Term_Matrix <- DocumentTermMatrix(transformations, control=list(wordLengths=c(2,Inf), bounds=list(global=c(5,Inf))))

#Split the Document-Term Matrix into proper test/train row ranges
Train_dataset <- Document_Term_Matrix[c(1:100, 201:300),]
Test_dataset <- Document_Term_Matrix[c(101:200, 301:400),]

#Use the words"Positive" and "Negative" as tag factors in your classification
Tags <- factor(c(rep("Positive",100), rep("Negative",100)), levels = c('Positive', 'Negative'))
table(Tags)

#Classify text using the kNN() function
prob.test<- knn(Train_dataset, Test_dataset, Tags, k = 2, prob=TRUE)

#Display classification results as a R dataframe
a <- 1:length(prob.test)
b <- levels(prob.test)[prob.test]
c <- attributes(prob.test)$prob
d <- prob.test == Tags
result <- data.frame(Doc=a, Predict=b, Prob=c, Correct=d)

# What is the percentage of correct (TRUE) classifications
sum(d)/ length(Tags)

#Estimate the effectiveness 
AutoCM <- table(prob.test, Tags)
AutoCM

TP <- AutoCM[1,1]
FP <- AutoCM[1,2]
FN <- AutoCM[2,1]
TN <- AutoCM[2,2]

precision <- TP/(TP+FP)
recall <- TP/(TP+FN)
f_score <- (2*precision*recall)/(precision+recall)