dataset <- read.csv("D:/FALL2023/IntroToDataScience/Student_Mental_health.csv", header = TRUE, sep = ",")
print(dataset)


missing_values <- is.na(dataset)
print(missing_values)


dataset <- na.omit(dataset)
print(dataset)


catage <- "Age"  
breaks <- c(0, 19, 25, Inf)  
labels <- c("Teenager", "Young Adult", "Adult")
dataset$CatAge <- cut(dataset[[catage]], breaks = breaks, labels = labels, include.lowest = TRUE)
print(dataset)


install.packages("e1071")
library(e1071)
install.packages("caret")
library(caret)


dataset <- data.frame(
  "what.is.your.cgpa" = sample(c("0-1.99", "2-2.49", "2.5-2.99", "3-3.49", "3.5-4.00"), 100, replace = TRUE),
  "Do.you.have.anxiety" = sample(c("Yes", "No"), 100, replace = TRUE)
)
contingency_table <- table(dataset$What.is.your.cgpa., dataset$Do.you.have.anxiety.)
chi_squared_test <- chisq.test(contingency_table)
print(chi_squared_test)


nb_model <- naiveBayes(What.is.your.course. ~ ., data = dataset)
print(nb_model)


pred <- predict(nb_model, dataset)
confusion_matrix <- table(dataset$What.is.your.course., pred)
print(confusion_matrix)


accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Accuracy:", accuracy))


train_indices <- sample(1:nrow(dataset), 0.7 * nrow(dataset))
train_data <- dataset[train_indices, ]
test_data <- dataset[-train_indices, ]


