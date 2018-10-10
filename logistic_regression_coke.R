#logistic regression on column No_display_Ind which has values 0 for Display and 1 for no display

library(foreign)
library(ggplot2)
library(GGally)
library(grid)
library(arm)
library(texreg)
library(fBasics)
library(data.table)
library(caret)
library(MASS)
library(stats)

# Load the raw training data from /tmp folder on nielsen server
final_data_coke <- fread("/Users/hagen/Downloads/final_data_coke.psv")

final_data_coke_clean <- final_data_coke
final_data_coke_clean$actual_price <- round(final_data_coke_clean$actual_price,2)

rows_to_keep <- which(final_data_coke_clean$actual_price>0)
final_data_coke_clean_hagen <- final_data_coke_clean[rows_to_keep,]


final_data_coke_clean_hagen$new_causal_name[final_data_coke_clean_hagen$new_causal_name == 'Catalogue or Feature,Major Display'] <- 'Major Display'
final_data_coke_clean_hagen$new_causal_name[final_data_coke_clean_hagen$new_causal_name == 'Catalogue or Feature,Minor Display'] <- 'Minor Display'
final_data_coke_clean_hagen$new_causal_name[final_data_coke_clean_hagen$new_causal_name == 'Catalogue or Feature,Major Display,Minor Display'] <- 'Major Display'

data <- final_data_coke_clean_hagen[final_data_coke_clean_hagen$new_causal_name!= 'Catalogue or Feature']
summary(data)
basicStats(data$item_id)
prop.table(table(data$No_display_Ind))

# Output the number of missing values for each column
sapply(data,function(x) sum(is.na(x)))

# Quick check for how many different values for each feature
sapply(data, function(x) length(unique(x)))

# A visual way to check for missing data
library(Amelia)
missmap(data, main = "Missing values vs observed")

# Subsetting the data
data <- subset(final_data_coke_clean_hagen,select=c(9,11,20))

# R should automatically code new_causal_name as a factor(). A factor is R's way of dealing with
# categorical variables
is.factor(data$new_causal_name)         # Returns TRUE

# Check categorical variables encoding for better understanding of the fitted model
contrasts(data$new_causal_name)

# Remove rows (in data$new_causal_name) with NAs
data <- data[!is.na(data$new_causal_name),]
rownames(data) <- NULL

# Train test splitting
train <- data[1:20000,]
test <- data[20001:40401,]

# Model fitting
model <- glm(No_display_Ind ~.,family=binomial(link='logit'),data=train)
summary(model)


# Analysis of deviance
anova(model,test="Chisq")

# McFadden R^2
library(pscl)
pR2(model)

#-------------------------------------------------------------------------------
# MEASURING THE PREDICTIVE ABILITY OF THE MODEL

# If prob > 0.5 then 1, else 0. Threshold can be set for better results
fitted.results <- predict(model,newdata=test, type = "response")
head(fitted.results)
fitted.results <- ifelse(fitted.results > 0.5,1,0)
head(test)
misClasificError <- mean(fitted.results != test$No_display_Ind)
print(paste('Accuracy',1-misClasificError))



# Confusion matrix
library(caret)
library(e1071)
       
confusionMatrix(data=fitted.results, reference=test$No_display_Ind)
predict <- predict(model, newdata= test, type = 'response')
table(train$No_display_Ind, predict > 0.5)

       #Confidence interval
confint(model)



# ROC and AUC
library(ROCR)
#p <- predict(model,newdata=test, type = "response")
pr <- prediction(fitted.results,test$No_display_Ind )
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

predict <- predict(model, newdata = test, type = 'response')      
ROCRpred <- prediction(predict, train$No_display_Ind)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))
ggplot(train, aes(x=corrected_sales_in_volume, y=No_display_Ind)) + geom_point() + stat_smooth(method="glm", se=FALSE)

       
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc


cbind(LogOdds = coef(model), Odds = exp(coef(model)))
#                               LogOdds      Odds
#(Intercept)                2.282511287 9.8012633
#corrected_sales_in_volume -0.001240614 0.9987602
#actual_price              -0.002457111 0.9975459

results <- cbind(LogOdds = coef(model), confint(model))
Waiting for profiling to be done...
results
#                             LogOdds       2.5 %     97.5 %
#actual_price              -1.2160651 -2.80890004 -0.1303801
#corrected_sales_in_volume  0.0840893 -0.03221141  0.2276654
       
screenreg(model, single.row = TRUE) 
 
#cross tabulation       
(tab <- xtabs(~No_display_Ind + actual_price+ corrected_sales_in_volume, data = train))   
chisq.test(tab)      
