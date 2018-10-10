library(foreign)
library(ggplot2)
library(GGally)
library(grid)
library(arm)
library(texreg)
library(caret)
library(corrplot)


r <- cov(x = sales_top_states_bunnings6$sales, y = sales_top_states_bunnings6$mean_max_temp, use = "na.or.complete") 
r <- r/(sd(sales_top_states_bunnings6$sales, na.rm = TRUE) * sd(sales_top_states_bunnings6$mean_max_temp, na.rm = TRUE))
r


cor <- cor(sales_top_states_bunnings20[c("mean_monthly_sales", "mean_max_temp", "mean_rain", "mean_evapo", "mean_solar", "mean_min_temp", "mean_max_humid", "mean_min_humid")], use = "na.or.complete")
cor <- cor(sales_area5[c(6,8:13,15,26,28:32,34:37,39:42)], use = "na.or.complete")
cor <- cor(sales_area8[c(4:11,20:51,55)], use = "na.or.complete")

#[c("mean_monthly_sales", "mean_max_temp", "mean_rain", "mean_evapo", "mean_solar", "mean_min_temp", "mean_max_humid", "mean_min_humid")], use = "na.or.complete")

corrplot(cor, method = "ellipse")

pairs(sales_top_states_bunnings6[c("sales", "mean_max_temp", "mean_rain", "mean_evapo", "mean_min_temp", "mean_max_humid")], use = "na.or.complete")

training_data <- sales_top_states_bunnings21[which(sales_top_states_bunnings21$start_date <= "2014-12-31"),]
test_data <- sales_top_states_bunnings21[which(sales_top_states_bunnings21$start_date >= "2015-01-01"),]

training_data <- sales12[which(sales12$start_date <= "2015-04-23"),]
test_data <- sales12[which(sales12$start_date > "2015-04-23"),]


#training_data <- sales_area5[which(sales_area5$sales_areadate < "2015-07-18"),]

training_data <- sales_area8[which(sales_area8$weeks1 < "2014 37"),]
#test_data <- sales_area5[which(sales_area5$sales_areadate >= "2015-07-18"),]
test_data <- sales_area10[which(sales_area10$weeks1 > "2014 36"),]

training_data <- sales_area10_chat[which(sales_area10_chat$weeks1 < "2016 20"),]
training_data <- sales_area10_chat[which(sales_area10_chat$weeks1 > "2016 09"),]
test_data <- sales_area10_chat[which(sales_area10_chat$weeks1 > "2016 19"),]

training_data <- sales_area10[which(sales_area10$sales_areadate <  "2015-07-18"),]
#training_data <- training_data[which(training_data$weeks1 > "2014 17"),]
test_data <- sales_area10[which(sales_area10$sales_areadate >= "2014-09-11"),]

training_data <- sales_area8[which(sales_area8$sales_areadate <  "2015-07-18"),]
#training_data <- training_data[which(training_data$weeks1 > "2014 17"),]
test_data <- sales_area8[which(sales_area8$sales_areadate >= "2014-09-11"),]


# library(alr3)
# library(car)
# par(mfrow=c(2,2))
# plot(m)
# m
#besr r===
#lm(formula = mean_monthly_sales ~ mean_humid + mean_evapo * mean_solar + 
#mean_wind + mean_temp + mean_monthly_temp * mean_monthly_rain + 
#  mean_quarterly_rain + mean_quarterly_temp + factor(weeks1) + 
#  factor(store) + factor(month), data = sales_top_states_bunnings20)

#m <- lm(mean_monthly_sales ~  mean_humid +  mean_solar+ mean_rain+  mean_wind + mean_temp + mean_monthly_temp  
#        +mean_monthly_rain + temp_index + mean_quarterly_temp  +factor(store)+factor(product)+factor(month), data = training_data )
# m1 <- glm(sales ~  factor(product) + factor(store) +  mean_min_humid + mean_evapo + mean_rain +  factor(month) + mean_wind + mean_max_humid +
#           mean_min_temp +  mean_solar  , data = training_data, family = "gaussian" )

summary(m)
anova(m)

fitted.results <- predict(m, newdata = test_data, type = "response")
fitted.results1 <- predict(m, newdata = test_data, inverval = "predict")


# ggpairs(sales_top_states_bunnings40[, c("sales", "mean_max_temp", "mean_evapo", 
#                                        "mean_rain" , "mean_max_humid",  "mean_solar")],
#          diag = list(continuous = "bar"),
#          lower = list(continuous = "smooth"),
#          title = "Scatter plot of continuous variables"  )


attributes(m)

which.max(sales_top_states_bunnings9$qty)
library(corrplot)
cor <- cor(sales_top_states_bunnings12[c("sales", "mean_max_temp", "mean_rain", "mean_evapo", "mean_solar", "mean_min_temp", "mean_max_humid", "mean_min_humid")], use = "na.or.complete")
install.packages("leaps")
library(leaps)

sub.fit = regsubsets(mean_monthly_sales ~  mean_humid +  mean_solar+ mean_rain+  mean_wind + mean_temp + mean_monthly_temp  +mean_monthly_rain + temp_index + mean_quarterly_temp + rain_index + factor(store) + factor(product) , data = training_data )
sub.fit = regsubsets(sales ~  mean_evapo + mean_humid + mean_solar+   mean_wind + mean_temp + mean_monthly_temp + mean_monthly_rain + temp_index + factor(store) + factor(product)  + factor(quarter)   + rain_5+ tmp10, data = training_data)
sub.fit = regsubsets(sales_per_area ~   area_tmp5 + area_tmp51 + area_tmp52 + area_tmp53 + area_tmp54 + weekly_min_temp_per_area + weekly_mean_solar_per_area + last_week_min_temp + min_temp_twoweeksago + min_temp_threeweeksago+ min_temp_fourweeksago+ solar_fourweeksago + solar_threeweeksago + solar_twoweeksago + last_week_solar +
  weekly_mean_humid_per_area + weekly_mean_evapo_per_area +weekly_rain_per_area.x + last_week_rel_humid + rel_humid_twoweeksago + rel_humid_threeweeksago + rel_humid_fourweeksago +last_week_evapo + evapo_twoweeksago + evapo_threeweeksago + evapo_fourweeksago+
  weekly_max_temp_per_area + weekly_mean_wind_per_area + last_week_rain +  rain_twoweeks_ago + rain_fourweeks_ago + rain_threeweeks_ago 
    , nbest= 25,
data = training_data)

sub.fit = regsubsets(sales ~  temp_index +  mean_temp + mean_monthly_temp  +mean_monthly_rain   +mean_solar+   mean_wind +  mean_quarterly_temp + rain_index + factor(store) + factor(product), data = training_data)
sales ~    mean_min_humid + mean_evapo + mean_rain +  mean_wind + mean_max_humid +
mean_min_temp +  mean_solar, data = training_data)
best.summary = summary(sub.fit)
names(best.summary)
which.min(best.summary$rss)

par(mfrow=c(1,2))
plot(best.summary$cp, xlab="number of features", ylab="cp")
plot(sub.fit, scale="Cp")
which.min(best.summary$bic)
which.max(best.summary$adjr2)
#best.fit <- lm(mean_monthly_sales ~   mean_humid + mean_solar + mean_rain +  mean_wind +   mean_monthly_rain , data = training_data )
best.fit <- lm(mean_monthly_sales ~    mean_solar+   mean_wind + mean_temp + mean_monthly_temp  +mean_monthly_rain + temp_index + mean_quarterly_temp + rain_index + factor(store) + factor(product), data = training_data)
best.fit = lm(sales ~  mean_evapo + mean_humid + mean_solar+   mean_wind + mean_temp + mean_monthly_temp + mean_monthly_rain + temp_index + factor(store) + factor(product)  + factor(quarter)   + rain_5+ tmp10, data = training_data)
summary(best.fit)
vif(best.fit)

par(mfrow=c(2,2))

plot(best.fit)
library(car)
vif(best.fit)
best.summary$adjr2

#m <- lm(sales ~  temp_index +  mean_temp + mean_monthly_temp  +mean_monthly_rain +  +mean_solar+   mean_wind +  mean_quarterly_temp + rain_index + factor(store) + factor(product), data = training_data)
#<- lm(mean_monthly_sales ~   mean_humid + mean_solar + mean_rain +  mean_wind +   mean_monthly_rain , data = training_data )
plot(m1$fitted.values, training_data$sales, xlab = "fitted", ylab = "actual", main = "pred vs actual sales")

par(mfrow=c(2,1))

plot(test_data$mean_monthly_sales)
actual_predicted <-as.data.frame(cbind(as.numeric(training_data$store),as.numeric(training_data$sales),as.numeric(fitted(m))))
names(actual_predicted) <-c("store","Actual","Predicted")

actual_predicted <-actual_predicted[order(actual_predicted$Actual),]
library(ggplot2)
ggplot(actual_predicted,aes(x =1:nrow(training_data),color=Series)) +
geom_line(data = actual_predicted, aes(x =1:nrow(training_data), y = Actual,color ="Actual")) +
geom_line(data = actual_predicted, aes(x =1:nrow(training_data), y = Predicted, color ="Predicted")) +
  xlab('House Number') +ylab('Sales')

pred <- predict(model165, mergedtest6d7d9d, type = 'response')
pred_list <- as.list(fitted.results)
pred_column <- do.call(rbind.data.frame, pred_list)
datapredictions <- cbind(test_data[,c(1,6,8,12)], pred_column)

names(datapredictions)[5]<-"predicted_monthly_sales"
#test_data_70_model128prediction <- cbind(test_data_70model128, fitted.results)

write.csv(datapredictions, file = "predictionsvsactual.csv")

pred1 <- predict(model136, test_data_copyv2)
tab1 <- table(fitted.results, test_data$mean_monthly_sales)
1-sum(diag(tab1))/sum(tab1)

importance = varImp(m1,scale = FALSE)
importance

par(mfrow=c(2,1))
plot(test_data$month, test_data$mean_monthly_sales, xlab ="month", ylab = 'actual monthly sales')

#plot(test_data$month, datapredictions$c.44.4766157807176..114.376258258797..5.22125243823753..13.1120113468931.., xlab = "month", ylab = 'predicted monthly sales')




datapredictionsplot <- datapredictions[,c(7,30:37,51)]
plot.ts(datapredictionsplot) 

sub.fit = regsubsets(mean_monthly_sales ~  mean_humid +  mean_solar+ mean_rain+  mean_wind + mean_temp + mean_monthly_temp  +mean_monthly_rain + temp_index + mean_quarterly_temp + rain_index , data = training_data )
b.sum = summary(sub.fit)
which.min(b.sum$bic)
plot(b.sum$bic, type="l", xlab="# of Features", ylab="BIC",  main="BIC score by Feature Inclusion")

datapred["Actual"] = training_data$mean_monthly_sales #create the vector Actual

training_data["Forecast"] = NA #create a vector for the predictions named Forecast, first using NA to create empty observations

test_data$Forecast = predict(m1, newdata =  test_data) #populate Forecast with the predicted values
#Next we will load the ggplot2 package and with one line of code produce a nicer graphic:
library(ggplot2)

ggplot(test_data, aes(x = Forecast, y=sales_per_area)) +geom_point() + geom_smooth(method=lm) + labs(title = "Forecast versus Actuals")


library(corrplot)
p.cor = cor(datapredictions[,c(30,31,33:37,51)])

corrplot.mixed(p.cor)


#m <- lm(mean_monthly_sales ~  mean_humid +  mean_solar+   mean_wind + mean_temp + mean_monthly_temp + mean_monthly_rain +  mean_quarterly_temp  +factor(store)+factor(product)+factor(month)+factor(quarter) , data = training_data )
# m1 <- glm(sales ~  factor(product) + factor(store) +  mean_min_humid + mean_evapo + mean_rain +  factor(month) + mean_wind + mean_max_humid +
#           mean_min_temp +  mean_solar  , data = training_data, family = "gaussian" )

m2 <- lm(sales_per_area ~  mean_min_temp +  max_temp_quarter + rain_5+ tmp10 + mean_monthly_sales + sum_rain_month +    mean_humid + mean_solar+   mean_wind + mean_temp  + factor(store) + factor(product) +factor(month) + factor(quarter)    , data = training_data)
#m <- lm(qty ~  mean_evapo + mean_humid + mean_solar+   mean_wind + mean_temp + mean_monthly_temp + mean_monthly_rain + temp_index + factor(store) + factor(product) +factor(month) + factor(quarter)   + rain_5+ tmp10, data = training_data)

#Brisbane caterpillar 
m1 <- lm(sales_per_area ~  weekly_min_temp_per_area + weekly_mean_solar_per_area + last_week_min_temp + min_temp_twoweeksago + min_temp_threeweeksago+ min_temp_fourweeksago+ solar_fourweeksago + solar_threeweeksago + solar_twoweeksago + last_week_solar +
           weekly_mean_humid_per_area + weekly_mean_evapo_per_area +weekly_rain_per_area.x + last_week_rel_humid + rel_humid_twoweeksago + rel_humid_threeweeksago + rel_humid_fourweeksago +last_week_evapo + evapo_twoweeksago + evapo_threeweeksago + evapo_fourweeksago+
           weekly_max_temp_per_area + weekly_mean_wind_per_area + last_week_rain +  rain_twoweeks_ago + rain_fourweeks_ago + rain_threeweeks_ago + last_week_mean_max_temp + max_temp_twoweeksago + max_temp_threeweeksago + max_temp_fourweeksago +mean_wind_fourweeksago + mean_wind_threeweeksago + mean_wind_twoweeksago + last_week_mean_wind +
           factor(month)    , 
         data = training_data)

#Brisbane fungi 
m1 <- lm(sales_per_area ~  area_rain_12 + area_tmp27 + area_tmp5 + area_tmp51 + area_tmp52 + area_tmp53 + area_tmp54 + weekly_min_temp_per_area + weekly_mean_solar_per_area + last_week_min_temp + min_temp_twoweeksago + min_temp_threeweeksago+ min_temp_fourweeksago+ solar_fourweeksago + solar_threeweeksago + solar_twoweeksago + last_week_solar +
           weekly_mean_humid_per_area + weekly_mean_evapo_per_area +weekly_rain_per_area.x + last_week_rel_humid + rel_humid_twoweeksago + rel_humid_threeweeksago + rel_humid_fourweeksago +last_week_evapo + evapo_twoweeksago + evapo_threeweeksago + evapo_fourweeksago+
           weekly_max_temp_per_area + weekly_mean_wind_per_area + last_week_rain +  rain_twoweeks_ago + rain_fourweeks_ago + rain_threeweeks_ago + last_week_mean_max_temp + max_temp_twoweeksago + max_temp_threeweeksago + max_temp_fourweeksago + 
           factor(month)  +factor(season) + factor(product) , 
         data = training_data)

#Brisbane fungi and ratsak forecast

m1 <- lm(sales_per_area ~   area_tmp51 + area_tmp52 + area_tmp53 + area_tmp54 +  
           last_week_min_temp + min_temp_twoweeksago + min_temp_threeweeksago+ 
           min_temp_fourweeksago+ solar_fourweeksago + solar_threeweeksago + 
           solar_twoweeksago + last_week_solar + last_week_rel_humid + rel_humid_twoweeksago + 
           rel_humid_threeweeksago + rel_humid_fourweeksago +last_week_evapo + 
           evapo_twoweeksago + evapo_threeweeksago + evapo_fourweeksago+
            last_week_mean_wind + last_week_rain + 
           rain_twoweeks_ago + rain_fourweeks_ago + rain_threeweeks_ago + 
           last_week_mean_max_temp + max_temp_twoweeksago + max_temp_threeweeksago + 
           max_temp_fourweeksago + mean_wind_fourweeksago + mean_wind_threeweeksago + 
           mean_wind_twoweeksago + 
           factor(month)   + factor(product) + factor(Area.x), 
         data = training_data)



#Melbourne caterpillar
m1 <- lm(sales_per_area ~ weekly_max_temp_per_area +   weekly_min_temp_per_area + weekly_mean_solar_per_area + last_week_mean_min_temp + min_temp_twoweeksago + min_temp_threeweeksago+ min_temp_fourweeksago+ solar_fourweeksago + solar_threeweeksago + solar_twoweeksago + last_week_solar +
           weekly_mean_humid_per_area + weekly_mean_evapo_per_area +weekly_rain_per_area.x + last_week_rel_humid + rel_humid_twoweeksago + rel_humid_threeweeksago + rel_humid_fourweeksago +last_week_evapo + evapo_twoweeksago + evapo_threeweeksago + evapo_fourweeksago+
            weekly_mean_wind_per_area + last_week_rain +  rain_twoweeks_ago + rain_fourweeks_ago + rain_threeweeks_ago + mean_wind_fourweeksago + mean_wind_threeweeksago + mean_wind_twoweeksago + last_week_mean_wind + last_week_mean_max_temp + max_temp_twoweeksago + max_temp_threeweeksago + max_temp_fourweeksago +
           area_rain_5 + area_tmp5 + area_tmp51 + area_tmp52 + area_tmp53 + area_tmp54 +factor(month)  +factor(season) + factor(product) , 
         data = training_data)
#melbourne fungi
m1 <- lm(sales_per_area ~ weekly_max_temp_per_area + weekly_mean_wind_per_area + area_rain_10 + area_tmp18 +  weekly_mean_humid_per_area + weekly_mean_evapo_per_area +weekly_rain_per_area.x+ weekly_min_temp_per_area + weekly_mean_solar_per_area + last_week_mean_min_temp + min_temp_twoweeksago + min_temp_threeweeksago+ min_temp_fourweeksago+ solar_fourweeksago + solar_threeweeksago + solar_twoweeksago + last_week_solar +
            + last_week_rel_humid + rel_humid_twoweeksago + rel_humid_threeweeksago + rel_humid_fourweeksago +last_week_evapo + evapo_twoweeksago + evapo_threeweeksago + evapo_fourweeksago+
            last_week_rain +  rain_twoweeks_ago + rain_fourweeks_ago + rain_threeweeks_ago + mean_wind_fourweeksago + mean_wind_threeweeksago + mean_wind_twoweeksago + last_week_mean_wind + last_week_mean_max_temp + max_temp_twoweeksago + max_temp_threeweeksago + max_temp_fourweeksago +
            area_tmp5 + area_tmp51 + area_tmp52 + area_tmp53 + area_tmp54 + factor(season) + factor(product) + factor(month)  , 
         data = training_data)

#melbourne ratsak
m1 <- lm(sales_per_area ~ weekly_max_temp_per_area + weekly_mean_wind_per_area + area_rain_10 + area_tmp18 +  weekly_mean_humid_per_area + weekly_mean_evapo_per_area +weekly_rain_per_area.x+ weekly_min_temp_per_area + weekly_mean_solar_per_area + last_week_mean_min_temp + min_temp_twoweeksago + min_temp_threeweeksago+ min_temp_fourweeksago+ solar_fourweeksago + solar_threeweeksago + solar_twoweeksago + last_week_solar +
           + last_week_rel_humid + rel_humid_twoweeksago + rel_humid_threeweeksago + rel_humid_fourweeksago +last_week_evapo + evapo_twoweeksago + evapo_threeweeksago + evapo_fourweeksago+
           last_week_rain +  rain_twoweeks_ago + rain_fourweeks_ago + rain_threeweeks_ago + mean_wind_fourweeksago + mean_wind_threeweeksago+   + mean_wind_twoweeksago + last_week_mean_wind + last_week_mean_max_temp + max_temp_twoweeksago + max_temp_threeweeksago + max_temp_fourweeksago +
           area_tmp5 + area_tmp51 + area_tmp52 + area_tmp53 + area_tmp54 + factor(season) + factor(product) + factor(month)+ factor(Area.x)  , 
         data = training_data)

#melbourne ratsak forecast one week
m1 <- lm(sales_per_area ~  last_week_sales + sales_twoweeksago + sales_threeweeksago + sales_fourweeksago + sales_fiveweeksago +  last_week_mean_min_temp + min_temp_twoweeksago + min_temp_threeweeksago+ min_temp_fourweeksago+ solar_fourweeksago + solar_threeweeksago + solar_twoweeksago + last_week_solar +
           + last_week_rel_humid + rel_humid_twoweeksago + rel_humid_threeweeksago + rel_humid_fourweeksago +last_week_evapo + evapo_twoweeksago + evapo_threeweeksago + evapo_fourweeksago+
           last_week_rain +  rain_twoweeks_ago + rain_fourweeks_ago + rain_threeweeks_ago + mean_wind_fourweeksago + mean_wind_threeweeksago + mean_wind_twoweeksago + last_week_mean_wind + last_week_mean_max_temp.x + max_temp_twoweeksago.x + max_temp_threeweeksago.x + max_temp_fourweeksago.x +
            area_tmp51 + area_tmp52 + area_tmp53 + area_tmp54 + area_tmp5 + mean_wind_fiveweeksago + evapo_fiveweeksago + rel_humid_fiveweeksago + min_temp_fiveweeksago + max_temp_fiveweeksago +rain_fiveweeks_ago + factor(season) + factor(product) + factor(month)  , 
         data = training_data)



#melbourne ratsak forecast 5 weeks
m1 <- lm(sales_per_area ~  sales_fiveweeksago +   min_temp_fiveweeksago+ solar_fiveweeksago 
         +  rel_humid_fiveweeksago  +  evapo_fiveweeksago+
           rain_fiveweeks_ago +  mean_wind_fiveweeksago  + max_temp_fiveweeksago +
           area_tmp55 + area_humid55 + factor(season) +  factor(month) + factor(Area.x) , 
         data = training_data)


#perth factor(product)!!!
m1 <- lm(sales_per_area ~  weekly_max_temp_per_area + weekly_mean_wind_per_area + weekly_mean_humid_per_area + 
           weekly_mean_evapo_per_area +weekly_rain_per_area.x+ weekly_min_temp_per_area + 
           weekly_mean_solar_per_area + last_week_mean_min_temp + min_temp_twoweeksago + min_temp_threeweeksago+ 
           min_temp_fourweeksago+ solar_fourweeksago + solar_threeweeksago + solar_twoweeksago + last_week_solar +
           + last_week_rel_humid + rel_humid_twoweeksago + rel_humid_threeweeksago + rel_humid_fourweeksago
         +last_week_evapo + evapo_twoweeksago + evapo_threeweeksago + evapo_fourweeksago+
           last_week_rain +  rain_twoweeks_ago + rain_fourweeks_ago + rain_threeweeks_ago + mean_wind_fourweeksago 
         + mean_wind_threeweeksago + mean_wind_twoweeksago + last_week_mean_wind + last_week_mean_max_temp
         + max_temp_twoweeksago + max_temp_threeweeksago + max_temp_fourweeksago + area_humid50 + area_humid51 + area_humid52 + area_humid53 + area_humid54 +
           area_tmp5 + area_tmp51 + area_tmp52 + area_tmp53 + area_tmp54 + area_rain_5 +  factor(product) +factor(season)+ factor(month)+ factor(Area.x)  , 
         data = training_data)

#melbourne ratsak forecast 4 weeks

m1 <- lm(sales_per_area ~  sales_fourweeksago +  min_temp_fourweeksago+ solar_fourweeksago 
         +  rel_humid_fourweeksago  +  evapo_fourweeksago+
           rain_fourweeks_ago +  mean_wind_fourweeksago  + max_temp_fourweeksago + count_fourweeksago +
           area_tmp54 + area_humid54 + factor(season) +  factor(month) + factor(Area.x)+ factor(product) , 
         data = training_data)
summary(m1)
anova(m1)
test_data$Forecast = predict(m1, newdata =  test_data) #populate Forecast with the predicted values
g <- with(test_data, aggregate(Forecast, list(Area.x = Area.x, product = product, Month_Yr = Month_Yr), mean, na.rm = TRUE))
test_data <- merge(test_data, g)
names(test_data)[names(test_data)=="x"] <- "mean_forecast_monthly_sales"

g <- with(test_data, aggregate(sales_per_area, list(Area.x = Area.x, product = product, Month_Yr = Month_Yr), mean, na.rm = TRUE))
test_data <- merge(test_data, g)
names(test_data)[names(test_data)=="x"] <- "mean_monthly_sales"

ranking <- varImp(m1, scale = FALSE)
ranking
write.csv(ranking, file = "feature_importance_fungus_melb.csv")
#summary(m2)
#anova(m2)


#fitted.results <- predict(m, newdata = test_data, type = "response")
#plot(fitted.results, test_data$mean_monthly_sales, xlab = "predicted", ylab = "actual", main = "pred vs actual")




g <- with(test_data, aggregate(Forecast, list(Area.x = Area.x, product = product, Month_Yr = Month_Yr), sum, na.rm = TRUE))
test_data <- merge(test_data, g)
names(test_data)[names(test_data)=="x"] <- "sum_forecast_monthly_sales"

g <- with(test_data, aggregate(sales_per_area, list(Area.x = Area.x, product = product, Month_Yr = Month_Yr), sum, na.rm = TRUE))
test_data <- merge(test_data, g)
names(test_data)[names(test_data)=="x"] <- "sum_monthly_sales"

# pred_list <- as.list(fitted.results)
# pred_column <- do.call(rbind.data.frame, pred_list)
# datapredictions <- cbind(test_data, pred_column)
# names(test_data)[names(test_data)=="x"] <- "forecast_monthly_sales"
names(datapredictions)[91]<-"predicted_sales"
#test_data_70_model128prediction <- cbind(test_data_70model128, fitted.results)
write.csv(test_data, file = "predictionsvsactual2607a.csv")

save(sales_area5, file = "sales_area_5.rda")
save(sales_area10, file = "sales_area_10.rda")


save(sales_area8, file = "sales_area8.rda")
save(test_data, file="test_data_area_cateronlyhist_bris1b.rda")

save(test_data, file="test_data_area_caterwithhist_melb.rda")
save(test_data, file="test_data_area_cateronlyhist_perth1.rda")

save(test_data, file="test_data_area_fungionlyhist_brisbane3.rda")
save(test_data, file="test_data_area_ratonlyhist_brisbane2.rda")

save(test_data, file="test_data_area_ratsakwithhist_brisbane.rda")
save(test_data, file="test_data_area_fungionlyhist_perth1.rda")

save(test_data, file="test_data_area_ratsakonlyhist_melb1.rda")
save(test_data, file="test_data_area_ratsakonlyhist_melb3.rda")
save(test_data, file="test_data_area_fungionlyhist_melb2.rda")
save(test_data, file="test_data_area_insectonlyhist_melb2.rda")

save(test_data, file="test_data_area_ratsakonlyhist_melb.rda")
save(test_data, file="test_data_area_ratsakonlyhist_perth1.rda")

save(yates_weather_pest_sales, file= "yates_weather_pest_sales.rda")
save(test_data, file= "pest_sales_test_data.rda")
save(test_data, file= "pest_sales_test_data_perth.rda")


write.csv(test_data, file = "test_data_weather_pest_sales.csv")

library(MPV)
PRESS(best.fit)
pred.subfit = predict(m, newdata = test_data)
resid.subfit = test_data$sales - pred.subfit
mean(resid.subfit^2)


train_ridge <- na.omit(train)
x =as.matrix(train_ridge[,c(1:4,22:27,34:37,39:40,60:67)])
y = train_ridge[,9]
ridge = glmnet(x, y, family="gaussian", alpha=0)


training_data <- sales_top_states_bunnings20[which(sales_top_states_bunnings20$start_date <= "2014-09-28"),]
test_data <- sales_top_states_bunnings20[which(sales_top_states_bunnings20$start_date > "2014-09-28"),]



#m <- lm(sales ~  mean_evapo + mean_humid + mean_solar+   mean_wind + mean_temp + mean_monthly_temp + mean_monthly_rain + temp_index + factor(store) + factor(product) +factor(month) + factor(quarter)   + rain_5+ tmp10, data = training_data)
m1 <- lm(sales_per_area ~  mean_evapo + mean_humid + mean_solar+   mean_wind + mean_temp + mean_monthly_temp + mean_monthly_rain + temp_index + factor(store) + factor(product) +factor(month) + factor(quarter)+ factor(Area.x)   + rain_5+ tmp10, data = training_data)

summary(m1)
anova(m1)
fitted.results <- predict(m1, newdata = test_data, type = "response")
#plot(fitted.results, test_data$mean_monthly_sales, xlab = "predicted", ylab = "actual", main = "pred vs actual")
pred_list <- as.list(fitted.results)
pred_column <- do.call(rbind.data.frame, pred_list)
datapredictions <- cbind(test_data, pred_column)
names(datapredictions)[89]<-"predicted_sales"
#test_data_70_model128prediction <- cbind(test_data_70model128, fitted.results)
write.csv(datapredictions, file = "predictionsvsactual1.csv")

(tab1 <- table(sales_top_states_bunnings20$product, sales_top_states_bunnings20$store))

par(mfrow=c(2,1))
hist(test_data$sales_per_area)
hist(test_data$Forecast)
)


live_chat <- fread("~/Downloads/livechat20151207-20160922.txt", sep = "|")

yates_weather_pest_sales1 <- yates_weather_pest_sales[!yates_weather_pest_sales$product %in% c("Bunnings Booval 8107", "Bunnings Epping 6038", "Bunnings Kalamunda 2094",
                                                                                                "Bunnings Maroochydore 8122", "xxCLOSEDxx Bunnings Claremont 2044"), ]
training_data <- yates_weather_pest_sales1[which(yates_weather_pest_sales1$store <= 25),]
test_data <- yates_weather_pest_sales1[which(yates_weather_pest_sales1$store > 25),]

yates_weather_pest_sales1 <-  yates_weather_pest_sales1[with(yates_weather_pest_sales1, order(store,product, season)),]


#m <- lm(sales ~  mean_evapo + mean_humid + mean_solar+   mean_wind + mean_temp + mean_monthly_temp + mean_monthly_rain + temp_index + factor(store) + factor(product) +factor(month) + factor(quarter)   + rain_5+ tmp10, data = training_data)
m1 <- lm(totalrain ~  maxhumidity + maxtemp + maxwindspeed + mintemp +  rownumber + pre14rain + .PlantAdvice + .PlantHealth + .PlantProtection + DiseaseAnthracnose + 
           DiseaseBlackspot + DiseaseBlossomEndRot + DiseaseCollarRot + DiseaseLeafCurl + DiseaseLeafSpot + DiseaseMyrtleRust + DiseaseSootyMould + DiseaseSunburn + HibiscusFlowerBeetle + PestAfricanBlackBeetle +
           PestAnt + PestAphid +PestArmyWorm + PestAzaleaLaceBug + PestBeetle + PestCaterpillar + PestCurlGrub + PestElmLeafBeetle + X44 +
           PestScale + PestSnailsSlugs + PestWoollyAphid + PhysicalDamageSunburn + PPlantProtection + factor(product) + factor(season) , data = training_data)
           
           
           mean_evapo + mean_humid + mean_solar+   mean_wind + mean_temp + mean_monthly_temp + mean_monthly_rain + max_temp + factor(store) + factor(product) +factor(month) + factor(quarter)+ factor(Area.x)   + rain_5+ tmp10, data = training_data)

test_data$Forecast = predict(m1, newdata =  test_data) #populate Forecast with the predicted values

coeff_stats <- coef(summary(m1))
write.csv(coefficients, file = "modelcoefficients.csv")

plot(training_data)


p <- ggplot(test_data1, aes(x = weeks1, y = Forecast)) + geom_point(aes(shape = factor(product), colour = factor(product)) )
ggplot(sales_caterpillar, aes(x = weeks1, y = sales)) + geom_point(aes(shape = factor(store), colour = factor(store)) )

training_data <- sales12[which(sales12$start_date <= "2015-04-23"),]
test_data <- sales12[which(sales12$start_date > "2015-04-23"),]



sales13 <- sales13[!sales13$material %in% c("52903", "54324", "55017"),]

training_data <- sales13[which(sales13$start_date <= "2016-05-09"),]
test_data <- sales13[which(sales13$start_date > "2016-05-09"),]
m1 <- lm(sales ~  sum_evapo + sum_rain + mean_max_temp + mean_min_temp + mean_min_humid + mean_max_humid + mean_wind +
           factor(Store) + factor(material)    , 
         data = training_data)

summary(m1)
anova(m1)


test_data$Forecast = predict(m1, newdata =  test_data) #populate Forecast with the predicted values

save(test_data, file= "sales13_test_data.rda")


