install.packages("MASS")
library(MASS)
Boston
?Boston
View(Boston)
colnames(Boston)
str(Boston)

###Supervise Machine Learning
## Split the data into training and test
install.packages("caTools")
library(caTools) # this package is for splitting the dataset

set.seed(12233)
split <- sample.split(Boston$medv, SplitRatio = 0.70)
split
table(split)
training <- subset(Boston, split == TRUE)
nrow(training)
test <- subset(Boston, split == FALSE)
nrow(test)

dim(Boston)
colSums(is.na(Boston))
plot(Boston$crim, Boston$medv)

boxplot(Boston)

### Model Building (linear model - "lm")
names(Boston)
reg <- lm(medv~crim, data=training)
reg
# medv = 24.28 - 0.42*crim

summary(reg)

reg1 <- lm(medv~crim+zn+indus, data=training)
summary(reg1)


reg2 <- lm(medv~., data=training)
reg2
summary(reg2)


reg3 <- lm(medv~.-indus -age, data=training)
summary(reg3)

1/(1-0.78)
Multiple R-squared:  0.7904,	Adjusted R-squared:  0.7838
Multiple R-squared:  0.7901,	Adjusted R-squared:  0.7846

## Prediction with the Model Data

pred <- predict(reg3, newdata = test)
pred
test$medv

pred1 <- cbind(pred,test$medv)
View(pred1)
