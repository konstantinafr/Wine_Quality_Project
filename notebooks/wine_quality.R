wine <- read.csv("winequality.csv", sep=";")
head(wine)

wine$quality_label <- cut(wine$quality, breaks=c(-Inf, 5, 7, Inf), labels=c("low", "medium", "high"))
class(wine$quality_label)

wine$quality <- NULL 

library(caret)

set.seed(42)
train_index <- createDataPartition(wine$quality_label, p=0.8, list=FALSE)
train <- wine[train_index, ]
test <- wine[-train_index, ]

library(e1071)
classifier <- naiveBayes(quality_label ~ ., data=train)

predictions <- predict(classifier, test)
print(predictions)

conf_matrix <- confusionMatrix(predictions, test$quality_label)
print(conf_matrix)

library(ggplot2)
df <- as.data.frame(conf_matrix$table)
ggplot(df, aes(Prediction, Reference, fill=Freq))+
  geom_tile()+
  geom_text(aes(label = Freq), color="white", size=6) +
  scale_fill_gradient(low="blue", high="red")+
  ggtitle("Confusion Matrix - Naive Bayes Wine Quality")+
  theme_minimal()

acc <- conf_matrix$overall["Accuracy"]
metrics <- conf_matrix$byClass[, c("Precision", "Recall", "F1")]
print(acc)
print(metrics)
