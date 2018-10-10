library(pscl)

pR2(model)
final_data_coke <- fread("/Users/hagen/Downloads/final_data_coke.psv")

model <- glm(No_display_Ind ~.,family=binomial(link='logit'),data=train)
summary(model)
anova(model, test="Chisq")
pR2(model)
data <- subset(final_data_coke_clean_hagen,select=c(9,11,20))
data <- subset(final_data_coke_clean_hagen_two,select=c(9,11,20))
View(final_data_coke_clean_hagen_one)
train <- data[1:40,]
test_set <- data[41:46,]
model <- glm(No_display_Ind ~.,family=binomial(link='logit'),data=train)
summary(model)
head(data)
train <- final_data_coke_clean_hagen[1:40000,]
test_set <- final_data_coke_clean_hagen[40001:40401,]
data <- subset(final_data_coke_clean_hagen,select=c(9,11,15))
train <- data[1:40000,]
test_set <- data[40001:40401,]
model <- glm(No_display_Ind ~.,family=binomial(link='logit'),data=train)
head(train)
data <- subset(final_data_coke_clean_hagen,select=c(9,11,20))
test_set <- data[40001:40401,]
train <- data[1:40000,]
model <- glm(No_display_Ind ~.,family=binomial(link='logit'),data=train)
summary(model)
anova(model, test="Chisq")
pR2(model)
model_lin <- lm(No_display_Ind ~.,data=train)
plot(model_lin)
fitted.results <- predict(model,newdata=test_set, type = "response")
head(fitted.results)
fitted.results <- ifelse(fitted.results < 0.5,1,0)
head(test_set)
misClasificError <- mean(fitted.results != test_set$No_display_Ind)
print(paste('Accuracy',1-misClasificError))
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != test_set$No_display_Ind)
print(paste('Accuracy',1-misClasificError))
fitted.results <- ifelse(fitted.results > 0.2,1,0)
misClasificError <- mean(fitted.results != test_set$No_display_Ind)
print(paste('Accuracy',1-misClasificError))
fitted.results <- ifelse(fitted.results > 0.5,1,0)
library(ROCR)
p <- predict(model,newdata=test_set, type = "response")
pr <- prediction(p,test_set$No_display_Ind )
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
prf
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc
summary(final_data_coke_clean_hagen)
table(final_data_coke_clean_hagen$item_id)
fBasics::basicStats(final_data_coke_clean_hagen)
fBasics::basicStats(final_data_coke_clean_hagen$item_id)
savehistory("~/Nielson/Untitled11052017.Rhistory")
save.image("~/Nielson/Untitled11052017.RData")
table(train$No_display_Ind, predict > 0.5)
View(train)
table(train$corrected_sales_in_volume, predict > 0.5)
table(train$actual_price, predict > 0.5)
library(data.table)
table(train$actual_price, predict > 0.5)
train <- as.table(train)
train <- as.list(train$No_display_Ind)
table(train$actual_price, predict > 0.5)
train <- data[1:40000,]
predict <- predict(model, type = 'response')
table(train$No_display_Ind, predict > 0.5)
library(ggplot2)
ggplot(train, aes(x=Rating, y=Recommended)) + geom_point() +
stat_smooth(method="glm", family="binomial", se=FALSE)
ggplot(train, aes(x=corrected_sales_volume, y=No_display_IN)) + geom_point() +
stat_smooth(method="glm", family="binomial", se=FALSE)
ggplot(train, aes(x=corrected_sales_in_volume, y=No_display_IN)) + geom_point() +
stat_smooth(method="glm", family="binomial", se=FALSE)
ggplot(train, aes(x=corrected_sales_in_volume, y=No_display_Ind)) + geom_point() +
stat_smooth(method="glm", family="binomial", se=FALSE)
plot(prf)
final_data_coke_clean_hagen <- final_data_coke_clean
final_data_coke_clean$actual_price <- round(final_data_coke_clean$actual_price,2)
final_data_coke_clean_hagen[final_data_coke_clean_hagen$new_causal_name == "Catalogue or Feature,Major Display "] <- "Major Display"
final_data_coke_clean_hagen[final_data_coke_clean_hagen$new_causal_name = "Catalogue or Feature,Major Display "] <- "Major Display"
final_data_coke_clean_hagen[final_data_coke_clean_hagen$new_causal_name = "Catalogue or Feature,Major Display"] <- "Major Display"
final_data_coke_clean_hagen[final_data_coke_clean_hagen$new_causal_name = 'Catalogue or Feature,Major Display'] <- "Major Display"
final_data_coke_clean_hagen[final_data_coke_clean_hagen$new_causal_name = Catalogue or Feature,Major Display] <- 'Major Display'
final_data_coke_clean_hagen[final_data_coke_clean_hagen$new_causal_name = 'Catalogue or Feature,Major Display'] <- 'Major Display'
View(final_data_coke_clean_hagen)
final_data_coke_clean_hagen <- final_data_coke_clean_hagen[final_data_coke_clean_hagen$new_causal_name!= 'Catalogue or Feature']
final_data_coke_clean_hagen <- final_data_coke_clean_hagen[final_data_coke_clean_hagen$new_causal_name!= Catalogue or Feature]
final_data_coke_clean_hagen <- final_data_coke_clean
