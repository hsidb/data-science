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
library(Amelia)
library(pscl)
library(caret)
library(e1071)
library(ROCR)
library(stats)
library(VGAM)

#read data

#final_data_ambient <- read.table(file = "/Users/hagen/Nielson/final_data_ambient.psv", sep = "|", header = TRUE, stringsAsFactors = TRUE)
#final_data_pet <- read.table(file = "/Users/hagen/Nielson/final_data_pet_food.psv", sep = "|", header = TRUE, stringsAsFactors = TRUE)

final_data_coke_70 <- read.table(file = "/Users/hagen/desktop/final_data_coke_70.psv", sep = "|", header = TRUE, stringsAsFactors = TRUE)

#sales_by_store <- read.table(file = "/Users/hagen/desktop/sales_by_store.psv", sep = "|", header = TRUE, stringsAsFactors = TRUE)
#sales <- sales_by_store_103_weeks[,sum_sales_all_itens := sum(sum_corrected_sales_in_volume), by = shop_id]


sapply(final_data_coke_70,function(x) sum(is.na(x)))
sapply(final_data_coke_70, function(x) length(unique(x)))

final_data_coke_70$shop_selected <- "list of 70"

final_data_coke_70$product <- factor(final_data_coke_70$product , ordered = FALSE )
#week_dates_copy$date  <- factor(week_dates_copy$date, ordered = TRUE)

#final_data_coke_30 <- read.table(file = "/Users/hagen/final_data_coke.psv", sep = "|", header = TRUE, stringsAsFactors = TRUE)
#rows_to_keep <- which(mergedtest6d7d9d$retailer == "NZ New World AK" || ""NZ New World WG" "NZ New World SI" ||NZ COUNTDOWM)



#final_data_coke_copy <- final_data_coke_copy[rows_to_keep,]
final_data_coke_70["new_causal_name2"] <- final_data_coke_70$new_causal_name
final_data_coke_70$new_causal_name2[final_data_coke_70$new_causal_name == 'Catalogue or Feature,Major Display'] <- 'Major Display'
final_data_coke_70$new_causal_name2[final_data_coke_70$new_causal_name == 'Catalogue or Feature,Minor Display'] <- 'Minor Display'
final_data_coke_70$new_causal_name2[final_data_coke_70$new_causal_name == 'Catalogue or Feature,Major Display,Minor Display'] <- 'Major Display'
#final_data_coke_70$new_causal_name2[final_data_coke_70$new_causal_name2 == 'NA'] <- 'No_display'


# training_data_copyv2["new_causal_name2"] <- training_data_copyv2$new_causal_name
# training_data_copyv2$new_causal_name2[training_data_copyv2$new_causal_name2 == 'Catalogue or Feature,Major Display'] <- 'Major Display'
# training_data_copyv2$new_causal_name2[training_data_copyv2$new_causal_name2 == 'Catalogue or Feature,Minor Display'] <- 'Minor Display'
# training_data_copyv2$new_causal_name2[training_data_copyv2$new_causal_name2 == 'Catalogue or Feature,Major Display,Minor Display'] <- 'Major Display'
# 
# training_data_copyv2$new_causal_name2 <- factor(training_data_copyv2$new_causal_name2, levels = c("No display", "Minor Display", "Major Display"), ordered = TRUE)
# 
# 
# test_data_copyv2["new_causal_name2"] <- test_data_copyv2$new_causal_name
# test_data_copyv2$new_causal_name2[test_data_copyv2$new_causal_name2 == 'Catalogue or Feature,Major Display'] <- 'Major Display'
# test_data_copyv2$new_causal_name2[test_data_copyv2$new_causal_name2 == 'Catalogue or Feature,Minor Display'] <- 'Minor Display'
# test_data_copyv2$new_causal_name2[test_data_copyv2$new_causal_name2 == 'Catalogue or Feature,Major Display,Minor Display'] <- 'Major Display'
# 
# test_data_copyv2$new_causal_name2 <- factor(test_data_copyv2$new_causal_name2, levels = c("No display", "Minor Display", "Major Display"), ordered = TRUE)

# 
# 
# final_data_coke_40$Display_Ind <- ifelse(final_data_coke_40$No_display_Ind==1, '0', '1')
# 
# 
# (tab <- xtabs(~No_display_Ind + y, data = training_data_copyv6 ))
# summary(final_data_coke_40)

final_data_coke_70$Display_Ind[final_data_coke_70$Catalogue_feature_Ind == '1' & final_data_coke_70$Major_Ind == '0' & final_data_coke_70$Minor_Ind =='0' ] <- '0'


final_data_coke_70$actual_price[final_data_coke_70$actual_price == '0']  <- NA
final_data_coke_70$corrected_sales_in_volume[final_data_coke_70$corrected_sales_in_volume == '0']  <- NA
final_data_coke_70$No_display_Ind[final_data_coke_70$corrected_sales_in_value == '0']  <- NA
final_data_coke_70$Display_Ind[final_data_coke_70$corrected_sales_in_value == '0']  <- NA

prop.table(table(final_data_coke_70$Display_Ind))


# think about this
#rows_to_keep <- which(final_data_coke_copy$new_causal_name != 'Catalogue or Feature')
#final_data_coke_copy <- final_data_coke_copy[rows_to_keep,]
# sapply(final_data_coke_copy,function(x) sum(is.na(x)))
# sapply(training_data, function(x) length(unique(x)))
# 
# final_data_coke_copy_v2 <- final_data_coke_copy
 final_data_coke_70["display2_level"]<- NA
 final_data_coke_70$display_level2[final_data_coke_70$new_causal_name2 == 'Major Display']  <- 4
 final_data_coke_70$display_level2[final_data_coke_70$new_causal_name2 == 'Minor Display']  <- 3
 final_data_coke_70$display_level2[final_data_coke_70$new_causal_name2 == 'Catalogue or Feature']  <- 2
 final_data_coke_70$display_level2[final_data_coke_70$new_causal_name2 == 'No display']  <- 1
 final_data_coke_70$display_level2 <- as.ordered(final_data_coke_70$display_level2)
 
 # test_data_copyv2["display_level"]<- NA
 # test_data_copyv2$display_level[test_data_copyv2$new_causal_name2 == 'Major Display']  <- 3
 # test_data_copyv2$display_level[test_data_copyv2$new_causal_name2 == 'Minor Display']  <- 2
 # #test_data_copyv2$display_level[test_data_copyv2$new_causal_name2 == 'Catalogue or Feature']  <- 2
 # test_data_copyv2$display_level[test_data_copyv2$new_causal_name2 == 'No display']  <- 1
 # test_data_copyv2$display_level <- as.ordered(test_data_copyv2$display_level)
 # 
 # training_data_copyv2["display_level"]<- NA
 # training_data_copyv2$display_level[training_data_copyv2$new_causal_name2 == 'Major Display']  <- 3
 # training_data_copyv2$display_level[training_data_copyv2$new_causal_name2 == 'Minor Display']  <- 2
 # #training_data_copyv2$display_level[training_data_copyv2$new_causal_name2 == 'Catalogue or Feature']  <- 2
 # training_data_copyv2$display_level[training_data_copyv2$new_causal_name2 == 'No display']  <- 1
 # training_data_copyv2$display_level <- as.ordered(training_data_copyv2$display_level)
 # 
 # 
 # #final_data_coke_copy$actual_price <- round(final_data_coke_copy$actual_price,2)
 # 
 # training_data_copyv2["display_level"]<- NA
 # training_data_copyv2$display_level[training_data_copyv2$Major_Ind == '1']  <- 3
 # training_data_copyv2$display_level[training_data_copyv2$Minor_Ind == '1']  <- 2
 # training_data_copyv2$display_level[training_data_copyv2$Display_Ind == '0']  <- 1
 # training_data_copyv2$display_level <- as.ordered(training_data_copyv2$display_level)
 # 
 # test_data_copyv2["display_level"]<- NA
 # test_data_copyv2$display_level[test_data_copyv2$Major_Ind == '1']  <- 3
 # test_data_copyv2$display_level[test_data_copyv2$Minor_Ind == '1']  <- 2
 # test_data_copyv2$display_level[test_data_copyv2$Display_Ind == '0']  <- 1
 # test_data_copyv2$display_level <- as.ordered(test_data_copyv2$display_level)
 # 
 # 
 # final_data_coke_copy$actual_price <- round(final_data_coke_copy$actual_price,2)
 # 
 # 
 
 
# library(MASS)
# final_data_coke_copy_v2 <- final_data_coke_copy_v2[!is.na(final_data_coke_copy_v2$display_level),]
# rownames(final_data_coke_copy_v2) <- NULL
# summary(final_data_coke_copy_v2)
# ind <- sample(2, nrow(final_data_coke_copy_v9), replace = TRUE, prob = c(0.5,0.5))
# training_data <- final_data_coke_copy_v9[ind==1,]
# test_data <- final_data_coke_copy_v9[ind==2,]
# training_data <- final_data_coke_70[which(final_data_coke_70$week_id < '889'),]
# test_data <- final_data_coke_70[which(final_data_coke_70$week_id > '888'),]
#   
# model90 <- polr(display_level ~ actual_price + corrected_sales_in_volume, training_data, Hess = TRUE)
# 
# (ctable <- coef(summary(model125)))
# p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
# (ctable <- cbind(ctable, "p value" = p ))
# (ci <- confint(model90))
# 
# (ctable <- coef(summary(model90)))
# p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
# (ctable <- cbind(ctable, "p value" = p ))
# pred <- predict(model90, training_data[1:5,], type = 'prob')
# pred
# print(pred, digits  = 3)
# 
# model90 <- polr(display_level ~ .-promotion_id-final_baseline_in_volume-weighted_projection_factor-numeric_projection_factor-weighted_distribution-Major_Ind-Minor_Ind-Catalogue_feature_Ind-No_display_Ind-week_id, training_data, Hess = TRUE)
# 
# 
# 
# 
# 
# pred <- predict(model82, training_data)
# (tab <- table(pred, training_data$display_level))
# 1-sum(diag(tab))/sum(tab)
# 
# pred1 <- predict(model125, test_data_copyv2)
# (tab1 <- table(pred1, test_data_copy_v2$display_level))
# 1-sum(diag(tab1))/sum(tab1)
# 
# #model90 <- polr(display_level ~ .-promotion_id-final_baseline_in_volume-weighted_projection_factor-numeric_projection_factor-weighted_distribution-Major_Ind-Minor_Ind-Catalogue_feature_Ind-No_display_Ind-week_id, training_data, Hess = TRUE)
# 
# library(VGAM)
# model91 <- vglm(display_level ~ 1 + corrected_sales_in_volume + week_id + item_id + retailer + product_group_description,family=cumulative(link='logit', parallel = TRUE, reverse = TRUE),data=training_data)
# fitted.results <- predict(model91, newdata=test_data, type = "response")

# add new featrues for price and volume
#ave_price = data.frame(final_data_coke_copy_v9$item_id, final_data_coke_copy_v9$actual_price)

#ave_price = data.frame(final_data_coke_copy_v2$item_id, final_data_coke_copy_v2$actual_price)
final_data_coke_70 <-  final_data_coke_70[with(final_data_coke_70, order(item_id, shop_id, week_id)),]

#z <- aggregate(ave_price[,-1], by=list(ave_price$final_data_coke_copy_v9.item_id), mean)
#h  <- aggregate(corrected_sales_in_value ~ item_id + shop_id, data = final_data_coke_70, mean, na.rm = TRUE)

g <- with(final_data_coke_70, aggregate(corrected_sales_in_value, list(item_id = item_id, shop_id = shop_id), mean, na.rm = TRUE))
final_data_coke_70 = merge(final_data_coke_70, h, by.x=c("item_id", "shop_id"), by.y=c("item_id", "shop_id"))
names(final_data_coke_70)[names(final_data_coke_70)=="x"] <- "mean_value"

test_data_copy <- test_data[c(-24,-25,-28,-36,-37)]
test_data_copy$price_index <- NULL

g <- with(training_data_70, aggregate(corrected_sales_in_value, list(item_id = item_id, shop_id = shop_id), mean, na.rm = TRUE))
training_data_70 = merge(training_data_70, g, by.x=c("item_id", "shop_id"), by.y=c("item_id", "shop_id"))
names(training_data_70)[names(training_data_70)=="x"] <- "mean_value"

g <- with(test_data_70, aggregate(corrected_sales_in_value, list(item_id = item_id, shop_id = shop_id), mean, na.rm = TRUE))
test_data_70 = merge(test_data_70, g, by.x=c("item_id", "shop_id"), by.y=c("item_id", "shop_id"))
names(test_data_70)[names(test_data_70)=="x"] <- "mean_value"

#c$mean  <- aggregate(actual_price ~ item_id + shop_id, data = final_data_coke_70, mean)
c <- with(final_data_coke_70, aggregate(actual_price, list(item_id = item_id, shop_id = shop_id), mean, na.rm = TRUE))
final_data_coke_70 = merge(final_data_coke_70, c, by.x=c("item_id", "shop_id"), by.y=c("item_id", "shop_id"))
names(final_data_coke_70)[names(final_data_coke_70)=="x"] <- "mean_price"

c <- with(training_data_70, aggregate(actual_price, list(item_id = item_id, shop_id = shop_id), mean, na.rm = TRUE))
training_data_70 = merge(training_data_70, c, by.x=c("item_id", "shop_id"), by.y=c("item_id", "shop_id"))
names(training_data_70)[names(training_data_70)=="x"] <- "mean_price"

c <- with(test_data_70, aggregate(actual_price, list(item_id = item_id, shop_id = shop_id), mean, na.rm = TRUE))
test_data_70 = merge(test_data_70, c, by.x=c("item_id", "shop_id"), by.y=c("item_id", "shop_id"))
names(test_data_70)[names(test_data_70)=="x"] <- "mean_price"

e <- with(final_data_coke_70, aggregate(corrected_sales_in_volume, list(item_id = item_id, shop_id = shop_id), mean, na.rm = TRUE))
final_data_coke_70 = merge(final_data_coke_70, e, by.x=c("item_id", "shop_id"), by.y=c("item_id", "shop_id"))
names(final_data_coke_70)[names(final_data_coke_70)=="x"] <- "mean_volume"

e <- with(training_data_70, aggregate(corrected_sales_in_volume, list(item_id = item_id, shop_id = shop_id), mean, na.rm = TRUE))
training_data_70 = merge(training_data_70, e, by.x=c("item_id", "shop_id"), by.y=c("item_id", "shop_id"))
names(training_data_70)[names(training_data_70)=="x"] <- "mean_volume"

e <- with(test_data_70, aggregate(corrected_sales_in_volume, list(item_id = item_id, shop_id = shop_id), mean, na.rm = TRUE))
test_data_70 = merge(test_data_70, e, by.x=c("item_id", "shop_id"), by.y=c("item_id", "shop_id"))
names(test_data_70)[names(test_data_70)=="x"] <- "mean_volume"

h  <- with(test_data_copyv2, aggregate(actual_price, list(item_id = item_id, shop_id = shop_id, quarter = quarter), max, na.rm = TRUE))
test_data_copyv2 = merge(test_data_copyv2, h, by.x=c("item_id", "shop_id", "quarter"), by.y=c("item_id", "shop_id", "quarter"))
names(test_data_copyv2)[names(test_data_copyv2)=="x"] <- "max_price"

k  <- with(training_data_copyv2, aggregate(actual_price, list(item_id = item_id, shop_id = shop_id, quarter = quarter), max, na.rm = TRUE))
training_data_copyv2 = merge(training_data_copyv2, k, by.x=c("item_id", "shop_id", "quarter"), by.y=c("item_id", "shop_id", "quarter"))
names(training_data_copyv2)[names(training_data_copyv2)=="x"] <- "max_price"

i  <- with(test_data_copyv2, aggregate(actual_price, list(item_id = item_id, shop_id = shop_id, month = month), max, na.rm = TRUE))
test_data_copyv2 = merge(test_data_copyv2, i, by.x=c("item_id", "shop_id", "month"), by.y=c("item_id", "shop_id", "month"))
names(test_data_copyv2)[names(test_data_copyv2)=="x"] <- "max_price_month"

j  <- with(training_data_copyv2, aggregate(actual_price, list(item_id = item_id, shop_id = shop_id, month = month), max, na.rm = TRUE))
training_data_copyv2 = merge(training_data_copyv2, j, by.x=c("item_id", "shop_id", "month"), by.y=c("item_id", "shop_id", "month"))
names(training_data_copyv2)[names(training_data_copyv2)=="x"] <- "max_price_month"


#final_data_coke_70 <- final_data_coke_70[rows_to_keep,] NaN values in mean
#rows_to_drop <- which(final_data_coke_70$item_id == '6429835' & final_data_coke_70$shop_id == '6400007219')
#final_data_coke_70 <- final_data_coke_70[-c(42480,42481,42482),-c(24,25)]

#estimate_mode <- function(x) {
 # d <- density(x, na.rm = TRUE)
#  d$x[which.max(d$y)]
#}

Mode <- function(x, na.rm = TRUE) {
  if(na.rm){
    x = x[!is.na(x)]
  }
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}

d <- with(final_data_coke_70, aggregate(actual_price, list(item_id = item_id, shop_id = shop_id), Mode))
final_data_coke_70 = merge(final_data_coke_70, d, by.x=c("item_id", "shop_id"), by.y=c("item_id", "shop_id"))
names(final_data_coke_70)[names(final_data_coke_70)=="x"] <- "mode_price"

f <- with(final_data_coke_70, aggregate(corrected_sales_in_volume, list(item_id = item_id, shop_id = shop_id), Mode))
final_data_coke_70 = merge(final_data_coke_70, f, by.x=c("item_id", "shop_id"), by.y=c("item_id", "shop_id"))
names(final_data_coke_70)[names(final_data_coke_70)=="x"] <- "mode_volume"


final_data_coke_70 <-  final_data_coke_70[with(final_data_coke_70, order(item_id, shop_id,week_id)),]
final_data_coke_70$price_difference <- diff(final_data_coke_70$actual_price)

# screenreg(model97, single.row = TRUE)
# #screenreg(list(model97, model80), single.row = TRUE)
# 
# 
# library(stats)


# Create the function.
#getmode <- function(v) {
#  uniqv <- unique(v, na.rm = TRUE)
#  tab <- tabulate(match(v, uniqv))
#  uniqv[tab == max(tab)]
#}
#  uniqv[which.max(tabulate(match(v, uniqv)))]
#}



#z <- aggregate(ave_price[,-1], by=list(ave_price$final_data_coke_copy_v2.item_id), getmode)
#z <- aggregate(ave_price[,-1], by=list(ave_price$final_data_coke_copy_v2.item_id), mean)

#names(z)[names(z)=="Group.1"] <- "item_id"

#final_data_coke_copy_v5 <- merge(final_data_coke_copy_v4, z[,c("item_id", "x") ], by = "item_id")
#names(final_data_coke_copy_v5)[names(final_data_coke_copy_v5)=="x"] <- "mode_price"
#names(final_data_coke_copy_v5)[names(final_data_coke_copy_v5)=="x"] <- "mean_price"

# final_data_coke_71 <- merge(final_data_coke_70, causal_shop_items[,c("item_id", "product","volume", "percentage") ], by = "item_id")
# 
# 
# pred <- predict(model97, training_data)
# (tab <- table(pred, training_data$display_level))
# 1-sum(diag(tab))/sum(tab)
# 
# pred1 <- predict(model83, test_data)
# (tab1 <- table(pred1, test_data$display_level))
# 1-sum(diag(tab1))/sum(tab1)
# confusionMatrix(data=pred, reference = training_data$display_level)
# confusionMatrix(data=pred1, reference = test_data$display_level)
# 

# pr <- profile(model97)
# pairs(pr)
# plot(pr)
# 1-pchisq(deviance(model97), df.residual(model97))
# stepAIC(model97)
# pred <- predict(model97, training_data, type = "probs")

final_data_coke_70 <-  final_data_coke_70[with(final_data_coke_70, order(item_id, shop_id, week_id)),]
test_data_copyv2 <-  test_data_copyv2[with(test_data_copyv2, order(item_id, shop_id, week_id)),]
training_data_copyv2 <-  training_data_copyv2[with(training_data_copyv2, order(item_id, shop_id, week_id)),]

last_price <-  final_data_coke_70$actual_price - price_difference
View(last_price)

names(last_price)[names(last_price)=="price_difference"] <- "last_price"

#z2 <- last_price$last_price[3:178138]
z <- training_data_copyv2$price_twoweeksago

z2 <- as.data.frame(z)
z2 <- rbind(NA,z2)
z2 <- z2[1:178138,]
final_data_coke_70 <- cbind(final_data_coke_70, z2)
names(final_data_coke_70)[names(final_data_coke_70)=="z2"] <- "price_threeweeksago"
final_data_coke_70 <- within(final_data_coke_70, price_threeweeksago[week_id == "834"] <-NA)

#z3 <- last_price$last_price[4:178138]
z3 <- as.data.frame(z)
z3 <- rbind(NA,z3)
z3 <- z3[1:178138,]
final_data_coke_70 <- cbind(final_data_coke_70, z3)
names(final_data_coke_70)[names(final_data_coke_70)=="z3"] <- "price_fourweeksago"
final_data_coke_70 <- within(final_data_coke_70, price_fourweeksago[week_id == "835"] <-NA)
training_data_copyv2 <- within(training_data_copyv2, price_fourweeksago[week_id == "835"] <-NA)



names(training_data_copyv2)[names(training_data_copyv2)=="final_data_coke_70$price_fourweeksago[1:97804]"] <- "price_fourweeksago"
names(training_data_copyv2)[names(training_data_copyv2)=="final_data_coke_70$price_threeweeksago[1:97804]"] <- "price_threeweeksago"
names(test_data_copyv2)[names(test_data_copyv2)=="final_data_coke_70$price_fourweeksago[97805:178138]"] <- "price_fourweeksago"
names(test_data_copyv2)[names(test_data_copyv2)=="final_data_coke_70$price_threeweeksago[97805:178138]"] <- "price_threeweeksago"


#final_data_coke_70$new_causal_name <- factor(final_data_coke_70$new_causal_name, levels = c("No display", "Minor Display", "Major Display"), ordered = TRUE)
final_data_coke_70$new_causal_name = factor(final_data_coke_70$new_causal_name, levels = c("No display","Catalogue or Feature","Minor Display" ,"Catalogue or Feature,Minor Display", "Major Display"  ,"Catalogue or Feature,Major Display" , "Major Display,Minor Display","Catalogue or Feature,Major Display,Minor Display" ))
training_data_copyv2$new_causal_name = factor(training_data_copyv2$new_causal_name, levels = c("No display","Catalogue or Feature","Minor Display" ,"Catalogue or Feature,Minor Display", "Major Display"  ,"Catalogue or Feature,Major Display" , "Major Display,Minor Display","Catalogue or Feature,Major Display,Minor Display" ))
test_data_copyv2$new_causal_name = factor(test_data_copyv2$new_causal_name, levels = c("No display","Catalogue or Feature","Minor Display" ,"Catalogue or Feature,Minor Display", "Major Display"  ,"Catalogue or Feature,Major Display" , "Major Display,Minor Display","Catalogue or Feature,Major Display,Minor Display" ))
#newrow = c(NA, NA)
#mergeddatapredictions1$product_class = factor(mergeddatapredictions1$product_class)
z <- last_price$last_price[2:178138]
z <- as.data.frame(z)
z <- rbind(NA,z)
z <- z[1:178138,]
final_data_coke_70 <- cbind(final_data_coke_70, z)
names(final_data_coke_70)[names(final_data_coke_70)=="z"] <- "price_twoweeksago"
final_data_coke_70 <- within(final_data_coke_70, price_twoweeksago[week_id == "833"] <-NA)

#final_data_coke_copy_v8 <- final_data_coke_copy_v7
#final_data_coke_copy_v8$last_volume <- final_data_coke_copy_v8$corrected_sales_in_volume
#final_data_coke_copy_v8 <-  final_data_coke_copy_v8[with(final_data_coke_copy_v8, order(item_id, week_id)),]
#final_data_coke_copy_v8 <- within(final_data_coke_copy_v8, last_price[week_id == "832"] <-NA)
final_data_coke_70 <- within(final_data_coke_70, price_twoweeksago[week_id == "833"] <-NA)


#z <- final_data_coke_copy_v8$corrected_sales_in_volume[1:40417]
#z  <- c(NA, z)
#z <- as.data.frame(z)

#final_data_coke_copy_v8 <- cbind(final_data_coke_copy_v8, z)
#names(final_data_coke_copy_v8)[names(final_data_coke_copy_v8)=="z"] <- "last_volume"
#final_data_coke_copy_v8 <- within(final_data_coke_copy_v8, last_volume[week_id == "832"] <-NA)


#final_data_coke_copy_v9 <- final_data_coke_copy_v8
#final_data_coke_copy_v9["price_difference"]  <- NA
#final_data_coke_copy_v9$price_difference <- final_data_coke_copy_v9$last_price - final_data_coke_copy_v9$actual_price
#View(final_data_coke_copy_v9)

test_data_70$price_difference <- round(test_data_70$price_difference,2)
test_data_70["price_drop"]  <- NULL
test_data_70["price_drop"]  <- NA
test_data_70$price_drop <- ifelse(test_data_70$price_difference < 0,1,0)

training_data_70$price_difference <- round(training_data_70$price_difference,2)
training_data_70["price_drop"]  <- NULL
training_data_70["price_drop"]  <- NA
training_data_70$price_drop <- ifelse(training_data_70$price_difference < 0,1,0)

#final_data_coke_copy_v9["volume_difference"]  <- NA
#final_data_coke_copy_v9$volume_difference <- final_data_coke_copy_v9$last_volume - final_data_coke_copy_v9$corrected_sales_in_volume
#View(final_data_coke_copy_v9)

#final_data_coke_copy_70 <- final_data_coke_copy_7
final_data_coke_70 <- final_data_coke_70[with(final_data_coke_70, order(item_id, shop_id, week_id)),]

final_data_coke_70$price_index <- final_data_coke_70$actual_price / final_data_coke_70$mean_price
final_data_coke_70$sales_index <- final_data_coke_70$corrected_sales_in_volume / final_data_coke_70$mean_volume
final_data_coke_70$sales_value_index <- final_data_coke_70$corrected_sales_in_value / final_data_coke_70$mean_value

training_data_70$price_index <- training_data_70$actual_price / training_data_70$mean_price
training_data_copyv2$price_index_max_month <- training_data_copyv2$actual_price / training_data_copyv2$max_price_month

training_data_copyv2$price_index_max <- training_data_copyv2$actual_price / training_data_copyv2$max_price
training_data_copyv2$price_index_mode <- training_data_copyv2$actual_price / training_data_copyv2$mode_price
training_data_70$sales_index <- training_data_70$corrected_sales_in_volume / training_data_70$mean_volume
training_data_copyv2$sales_index_base <- training_data_copyv2$corrected_sales_in_volume / training_data_copyv2$final_baseline_in_volume
training_data_70$sales_value_index <- training_data_70$corrected_sales_in_value / training_data_70$mean_value

test_data_copyv2$price_index_mode <- test_data_copyv2$actual_price / test_data_copyv2$mode_price
test_data_copyv2$price_index_max <- test_data_copyv2$actual_price / test_data_copyv2$max_price
test_data_copyv2$price_index_max_month <- test_data_copyv2$actual_price / test_data_copyv2$max_price_month

test_data_70$price_index <- test_data_70$actual_price / test_data_70$mean_price
test_data_70$sales_index <- test_data_70$corrected_sales_in_volume / test_data_70$mean_volume
test_data_copyv2$sales_index_base <- test_data_copyv2$corrected_sales_in_volume / test_data_copyv2$final_baseline_in_volume

test_data_70$sales_value_index <- test_data_70$corrected_sales_in_value / test_data_70$mean_value



price_difference <- diff(final_data_coke_70$actual_price)
price_difference <- c(NA,price_difference)
price_difference <- as.data.frame(price_difference)
final_data_coke_70 <- cbind(final_data_coke_70, price_difference)
final_data_coke_70 <- within(final_data_coke_70, price_difference[week_id == "832"] <-NA)

price_difference_2 <- diff(final_data_coke_70$price_difference)

final_data_coke_70 <-  final_data_coke_70[with(final_data_coke_70, order(item_id, shop_id, week_id)),]
volume_difference <- diff(final_data_coke_70$corrected_sales_in_volume)
volume_difference <- c(NA,volume_difference)

volume_difference <- as.data.frame(volume_difference)

final_data_coke_70 <- cbind(final_data_coke_70, volume_difference)
final_data_coke_70 <- within(final_data_coke_70, volume_difference[week_id == "832"]   <-  NA)



final_data_coke_70$last_volume <-  final_data_coke_70$corrected_sales_in_volume-final_data_coke_70$volume_difference
final_data_coke_70$last_price <-  final_data_coke_70$actual_price -final_data_coke_70$price_difference
final_data_coke_70$price_2weeksago <-  final_data_coke_70$actual_price -final_data_coke_70$price_difference

last_price <-  final_data_coke_70$actual_price - price_difference


# final_data_coke_32 <- final_data_coke_31
# final_data_coke_32$shop_selected <- ordered(final_data_coke_32$shop_selected, levels = c("bottom", "middle", "random_causal" ,"top"))
# final_data_coke_32$shop_selected
# 
# model107 <- glm(No_display_Ind ~  sales_index  * price_index + price_difference + corrected_sales_in_volume +   mode_volume + mean_volume + factor(retailer) + factor(product_group_description)  , training_data, family = binomial())


# final_data_coke_70 <-  final_data_coke_70[with(final_data_coke_copy_70, order(week_id)),]
# 
# 
# item_list_832 <- final_data_coke_copy_v5_copy$item_id[1:395]
# item_list_832 <- unique(item_list_832)
# item_list_832 <- as.data.frame(item_list_832)
# selectedRows <- (final_data_coke_copy_v5_copy$item_id %in% item_list_832)
# item_list_832 <- transpose(item_list_832$V1)
# final_data_coke_copy_v11 <-  final_data_coke_copy_v5_copy[selectedRows,]



#final_data_coke_30$sales_index <- final_data_coke_30$corrected_sales_in_volume / final_data_coke_30$mean_volume

shop_list70 <- read_csv("~/Desktop/nielsen/shop_list70.csv", 
#model107 <- glm(No_display_Ind ~ corrected_sales_in_volume +  sales_index * price_index  + factor(product_group_description) + volume_difference + price_difference, training_data, family = binomial())

#model107 <- glm(No_display_Ind ~ corrected_sales_in_volume + sales_index + price_index + mode_price +  volume_difference + price_difference + last_price + last_volume , training_data, family = binomial())
#model107 <- glm(No_display_Ind ~ corrected_sales_in_volume +  sales_index * price_index  + factor(product_group_description) + volume_difference + price_difference, training_data, family = binomial())

# model107 <- glm(No_display_Ind ~ corrected_sales_in_volume +  sales_index * price_index  + factor(product_group_description) + volume_difference + price_difference, training_data, family = binomial())
# model119 <- glm(No_display_Ind ~ corrected_sales_in_volume +  sales_index * price_index  + factor(product_group_description) + volume_difference + price_difference, training_data_copy, family = binomial())
# 
# 
# sales_by_store_103_weeks_3 <- sales_by_store_103_weeks[,sum_sales_all_itens := sum(sum_corrected_sales_in_volume), by = item_id]
# sales_by_store_103_weeks_4 <- sales_by_store_103_weeks_3[,sum_sales_all_itens := sum(sum_corrected_sales_in_volume), by = shop_id]
# tail(sales_by_store_103_weeks_4)
# model121 <- glm(No_display_Ind ~  corrected_sales_in_volume +  sales_index + price_index  + price_drop + 
#                   price_twoweeksago +factor(product_group_description) + last_price  + sales_value_index + percentage + Catalogue_feature_Ind + final_baseline_in_volume, training_data_copy,
# 
# rm(list = ls()[grep("scatter", ls())])
# rm(list = ls()[grep("final_data_coke_clean", ls())])
# rm(list = ls()[grep("final_data_coke_copy_v", ls())])

causal_shop_items <- read_excel("~/Desktop/nielsen/causal_shop_items.xlsx")
View(causal_shop_items)
final_data_coke_71 <- merge(final_data_coke_copy_70, causal_shop_items[,c("item_id", "product","volume", "percentage") ], by = "item_id")

week_dates <- read_delim("~/Desktop/nielsen/week_dates.csv", "\t", escape_double = FALSE, trim_ws = TRUE)
final_data_coke_70 <- merge(final_data_coke_70, week_dates_5[,c("week_id", "month") ], by = "week_id")
training_data_copyv2 <- merge(training_data_copyv2, week_dates_5[,c("week_id", "month") ], by = "week_id")
test_data_copyv2 <- merge(test_data_copyv2, week_dates_5[,c("week_id", "month") ], by = "week_id")
# 
#  final_data_coke_70$week_description <- NULL
# factor(final_data_coke_70$week_description, ordered = TRUE)
# training_data_copyv2$week_description <- NULL
# 
#   factor(training_data_copyv2$week_description, ordered = TRUE)
# test_data_copyv2$week_description <- NULL
#   factor(test_data_copyv2$week_description, ordered = TRUE)


#model109 <- glm(Major_Ind ~ corrected_sales_in_volume +  sales_index + price_index  + Catalogue_feature_Ind + actual_price + factor(retailer) + price_difference + last_price + price_twoweeksago + price_drop + percentage, training_data, family = binomial())


# test_data_copyv2$week_description <- relevel(test_data_copyv2$week_description, "W 03/01/16")
# test_data_copyv2$week_description <- as.ordered(test_data_copyv2$week_description)
# 
# 
# training_data_copyv2$week_description <- relevel(training_data_copyv2$week_description, "W 30/11/14")
# training_data_copyv2$week_description <- as.ordered(training_data_copyv2$week_description)

week_dates_copy <- data.frame(week_dates,rank(c( 1,   2,   3,   4 ,  5,   6,   7,   8,   9,  10,  11,  12,  13,  14,  15,  16,  17 , 18 , 19 , 20,  21 , 22,  23,
                                                 24,  25,  26,  27 , 28,  29 , 30,  31,  32,  33 , 34,  35 , 36 , 37 , 38,  39 , 40,  41 , 42,  43 , 44 , 45 , 46,
                                                 47,  48,  49,  50 , 51 , 52 , 53 , 54 , 55,  56 , 57,  58 , 59 , 60 , 61 , 62 , 63 , 64 , 65 , 66 , 67 , 68 , 69,
 
                                                70,  71 , 72,  73 , 74 , 75 , 76 , 77,  78 , 79 , 80 , 81 , 82 , 83 , 84 , 85 , 86 , 87 , 88 , 89 , 90 , 91  ,92,
                                                 93,  94 , 95 , 96 , 97 , 98 , 99 ,100, 101 ,102 ,103 ,104)))



weeks = gsub(pattern = "W", replacement = "", week_dates$date)
dates2 = gsub(pattern = "W", replacement = "", week_dates_5$date)
dates = gsub(pattern = "00", replacement = "", week_dates_5$weeks)
dates2 = gsub(pattern = "/14", replacement = "", dates2)


week_dates_cop <- cbind(week_dates,weeks)
week_dates_5 <- cbind(week_dates_5, dates2)

week_dates_5$weeks <- as.Date.default(week_dates_5$weeks)
week_dates_5$dates2 <- as.Date.default(week_dates_5$dates2)

week_dates_5 <- transform(week_dates_5, month= factor(format(weeks, format = "%B"), levels = month.name))
week_dates_6 <- transform(week_dates_5, month_1= factor(format(weeks, format = "%a %B")))

final_data_coke_70 <- merge(final_data_coke_70, week_dates_6[,c("week_id", "dates") ], by = "week_id")
training_data_copyv2 <- merge(training_data_copyv2, week_dates_6[,c("week_id", "dates") ], by = "week_id")
test_data_copyv2 <- merge(test_data_copyv2, week_dates_6[,c("week_id", "dates") ], by = "week_id")


week_dates_5$quarter <- quarters(week_dates_5$weeks)
training_data_copyv6b$quarter <- factor(training_data_copyv2a$quarter, levels = c("Q1", "Q2", "Q3", "Q4"))
test_data_copyv6b$quarter <- factor(test_data_copyv2a$quarter, levels = c("Q1", "Q2", "Q3", "Q4"))


final_data_coke_70 <- merge(final_data_coke_70, week_dates_5[,c("week_id", "quarter") ], by = "week_id")
training_data_copyv2 <- merge(training_data_copyv2, week_dates_5[,c("week_id", "quarter") ], by = "week_id")
test_data_copyv2 <- merge(test_data_copyv2, week_dates_5[,c("week_id", "quarter") ], by = "week_id")






trainig_data_copyv2$dates <- as.Date(training_data_copyv2$dates, format = "%d/%m")
training_data_copyv2$quarter <- quarters(training_data_copyv2$dates)

max_price <- tapply(training_data_copyv2$actual_price,training_data_copyv2$item_id,max, na.rm = TRUE)
max_price <- with(training_data_copyv2, tapply(actual_price, list(item_id,shop_id,quarter), max, na.rm = TRUE))


n_training <- final_data_coke_70$price_fourweeksago[which(final_data_coke_70$week_id < 889)]
n_test <- final_data_coke_70$price_fourweeksago[which(final_data_coke_70$week_id > 888)]

training_data_copyv2 = cbind(training_data_copyv2, n_training)
test_data_copyv2 = cbind(test_data_copyv2, n_test)


names(test_data_copyv2)[names(test_data_copyv2)=="n_test"] <- "price_fourweeksago"
names(training_data_copyv2)[names(training_data_copyv2)=="n_training"] <- "price_fourweeksago"


#id3 <- training_data_copyv2[c(1,2,5,13,15,24)]
# id <- id3[which(id3$No_display_Ind == '1'),]
id <- subset(training_data_copyv2, No_display_Ind == '1', select= c(1,2,5,13,15,24))

j <- with(id, aggregate(actual_price, list(item_id = item_id, shop_id = shop_id), mean, na.rm = TRUE))
training_data_copyv2a <- merge(training_data_copyv2, j, by.x=c("item_id", "shop_id"),by.y=c("item_id", "shop_id"), all.x =TRUE)
names(training_data_copyv2a)[names(training_data_copyv2a)=="x"] <- "mean_price1"

e <- with(id, aggregate(corrected_sales_in_volume, list(item_id = item_id, shop_id = shop_id), mean, na.rm = TRUE))
training_data_copyv2a = merge(training_data_copyv2a, e, by.x=c("item_id", "shop_id"), by.y=c("item_id", "shop_id"), all.x = TRUE)
names(training_data_copyv2a)[names(training_data_copyv2a)=="x"] <- "mean_volume1"

#id3 <- training_data_copyv2[c(1,2,5,13,15,24)]
# id <- id3[which(id3$No_display_Ind == '1'),]
id <- subset(test_data_copyv2, No_display_Ind == '1', select= c(1,2,5,13,15,24))

j <- with(id, aggregate(actual_price, list(item_id = item_id, shop_id = shop_id), mean, na.rm = TRUE))
test_data_copyv2a <- merge(test_data_copyv2, j, by.x=c("item_id", "shop_id"),by.y=c("item_id", "shop_id"), all.x =TRUE)
names(test_data_copyv2a)[names(test_data_copyv2a)=="x"] <- "mean_price1"

e <- with(id, aggregate(corrected_sales_in_volume, list(item_id = item_id, shop_id = shop_id), mean, na.rm = TRUE))
test_data_copyv2a = merge(test_data_copyv2, e, by.x=c("item_id", "shop_id"), by.y=c("item_id", "shop_id"), all.x = TRUE)
names(test_data_copyv2a)[names(test_data_copyv2a)=="x"] <- "mean_volume1"

#test_data_copyv2a = merge(test_data_copyv2a,  gave better results!!!!

ii  <- with(test_data_copyv2a, aggregate(actual_price, list(item_id = item_id, shop_id = shop_id, month = month), mean, na.rm = TRUE))
test_data_copyv2a = merge(test_data_copyv2a, ii, by.x=c("item_id", "shop_id", "month"), by.y=c("item_id", "shop_id", "month"))
names(test_data_copyv2a)[names(test_data_copyv2a)=="x"] <- "mean_price_month"

jj  <- with(training_data_copyv2a, aggregate(actual_price, list(item_id = item_id, shop_id = shop_id, month = month), mean, na.rm = TRUE))
training_data_copyv2a = merge(training_data_copyv2a, jj, by.x=c("item_id", "shop_id", "month"), by.y=c("item_id", "shop_id", "month"))
names(training_data_copyv2a)[names(training_data_copyv2a)=="x"] <- "mean_price_month"

im  <- with(test_data_copyv2a, aggregate(corrected_sales_in_volume, list(item_id = item_id, shop_id = shop_id, month = month), mean, na.rm = TRUE))
test_data_copyv2a = merge(test_data_copyv2a, im, by.x=c("item_id", "shop_id", "month"), by.y=c("item_id", "shop_id", "month"))
names(test_data_copyv2a)[names(test_data_copyv2a)=="x"] <- "mean_sales_month"

jn  <- with(training_data_copyv2a, aggregate(corrected_sales_in_volume, list(item_id = item_id, shop_id = shop_id, month = month), mean, na.rm = TRUE))
training_data_copyv2a = merge(training_data_copyv2a, jn, by.x=c("item_id", "shop_id", "month"), by.y=c("item_id", "shop_id", "month"))
names(training_data_copyv2a)[names(training_data_copyv2a)=="x"] <- "mean_sales_month"

test_data_copyv2a$price_index1 <- test_data_copyv2a$actual_price / test_data_copyv2a$mean_price1
test_data_copyv2a$sales_index1 <- test_data_copyv2a$corrected_sales_in_volume / test_data_copyv2a$mean_volume1
test_data_copyv2a$price_index2 <- test_data_copyv2a$actual_price / test_data_copyv2a$mean_price_month
test_data_copyv2a$sales_index2 <- test_data_copyv2a$corrected_sales_in_volume / test_data_copyv2a$mean_sales_month


training_data_copyv2a$price_index1 <- training_data_copyv2a$actual_price / training_data_copyv2a$mean_price1
training_data_copyv2a$sales_index1 <- training_data_copyv2a$corrected_sales_in_volume / training_data_copyv2a$mean_volume1
training_data_copyv2a$price_index2 <- training_data_copyv2a$actual_price / training_data_copyv2a$mean_price_month
training_data_copyv2a$sales_index2 <- training_data_copyv2a$corrected_sales_in_volume / training_data_copyv2a$mean_sales_month


training_data_copyv2$logprice <- 1/log(training_data_copyv2$actual_price) 

#center and scale
zVar <- (myVar - mean(myVar)) / sd(myVar)

library(caret)
# Assuming goal class is column 24
test_data_copyv6b<- test_data_copyv2a
test_data_copyv6b <- cbind(test_data_copyv6b, pred_column)
names(test_data_copyv6b)[66] <- "predicted_display"



#test_data_copyv6b$shop_id <- as.integer(test_data_copyv6b$shop_id)
# test_data_copyv6b$logprice <- as.integer(test_data_copyv6b$logprice)
# test_data_copyv6b$corrected_sales_in_volume <- as.numeric(test_data_copyv6b$corrected_sales_in_volume)
# test_data_copyv6b$last_volume <- as.numeric(test_data_copyv6b$last_volume)
# test_data_copyv6b$mode_volume <- as.numeric(test_data_copyv6b$mode_volume)

preObj <- preProcess(test_data_copyv6b[,-c(1:12,18:25,29,32,43:46,55,64:66)], method=c("scale"))
newData <- predict(preObj, test_data_copyv6b[,-c(1:12,18:25,29,32,43:46,55,64:66)])
test_data_copyv6b <- cbind(newData, test_data_copyv6b[, c(1:12,18:25,29,32,43:46,55,64:66)])

#print(preObj)
# preObj <- preProcess(training_data_copy4[, -c("")], method=c("center"))
# newData <- predict(preObj, data[, -10])

# for(i in 1:length(colnames(test_data_copyv6b))) {
#   if(class(test_data_copyv6b[,i]) == "numeric" ) {
#     test_data_copyv6b[,i] <- as.vector(scale(test_data_copyv6b[,i])) }
# }


training_data_copyv6b<- training_data_copyv2a
training_data_copyv6b <- cbind(training_data_copyv6b, pred_column)
names(test_data_copyv6b)[68] <- "predicted_display"


#training_data_copyv6b$shop_id <- as.integer(training_data_copyv6b$shop_id)
# training_data_copyv6b$logprice <- as.integer(training_data_copyv6b$logprice)
# 
# training_data_copyv6b$corrected_sales_in_volume <- as.numeric(training_data_copyv6b$corrected_sales_in_volume)
# training_data_copyv6b$last_volume <- as.numeric(training_data_copyv6b$last_volume)
# training_data_copyv6b$mode_volume <- as.numeric(training_data_copyv6b$mode_volume)

 preObj <- preProcess(training_data_copyv6b[, -c(1:12,18:25,29,32,43:46,55,64:66)], method=c("scale"))
 newData <- predict(preObj, training_data_copyv6b[, -c(1:12,18:25,29,32,43:46,55,64:66)])
 training_data_copyv6b <- cbind(newData, training_data_copyv6b[, c(1:12,18:25,29,32,43:46,55,64:66)])
# 
# for(i in 1:length(colnames(training_data_copyv6b))) {
#   if(class(training_data_copyv6b[,i]) == "numeric" ) {
#     training_data_copyv6b[,i] <- as.vector(scale(training_data_copyv6b[,i])) }
# }
#  

# training_data_copyv2a[,c(1,13,27,30:31)] <- sapply(training_data_copyv2a[,c(1,13,27,30:31)], as.numeric)
# test_data_copyv2a[,c(1,13,27,30:31)] <- sapply(test_data_copyv2a[,c(1,13,27,30:31)], as.numeric)
 
training_data_copyv6b$y = rep(1,97804)
training_data_copyv6b$y[training_data_copyv6b$new_causal_name2 == 'Major Display'] <- 0
training_data_copyv6b$y[training_data_copyv6b$new_causal_name == 'Catalogue or Feature'] <- 0

test_data_copyv6b$y = rep(1,80334)
test_data_copyv6b$y[test_data_copyv6b$new_causal_name2 == 'Major Display'] <- 0
test_data_copyv6b$y[test_data_copyv6b$new_causal_name == 'Catalogue or Feature'] <- 0



# cv <- training_data_copyv2a[,-c(1:12,18:25,43:46,55)]

training_data_copyv2a$logprice <- log(training_data_copyv2a$actual_price)
test_data_copyv2a$logprice <- log(test_data_copyv2a$actual_price)

training_data_copyv2a$sinsales <- sin(training_data_copyv2a$corrected_sales_in_volume)
test_data_copyv2a$sinsales <- sin(test_data_copyv2a$corrected_sales_in_volume)

#log(price + 0.01)
# id <-  training_data_copyv6$actual_price[training_data_copyv6$No_display_Ind == '1'] 
# final_data_test_coke_v6 <- final_data_coke_test[,id]
# r <- with(final_data_testv, aggregate(corrected_sales_in_volume, list(item_id = item_id, shop_id = shop_id), mean, na.rm = TRUE))
# final_data_coke_70 = merge(final_data_coke_70, e, by.x=c("item_id", "shop_id"), by.y=c("item_id", "shop_id"))
# names(final_data_coke_70)[names(final_data_coke_70)=="x"] <- "mean_volume"



mergeddatapredictions1 <- merge(x = mergeddatapredictions, y = pet_desc, by = "item_id", all.x = TRUE)
mergeddatapredictions1 <- merge(x = mergeddatapredictions1, y = ambient_desc, by = "item_id", all.x = TRUE)
mergeddatapredictions1 <- merge(x = mergeddatapredictions1, y = coke_desc, by = "item_id", all.x = TRUE)

mergeddatapredictions1$product_class <- with(mergeddatapredictions1, paste0(characteristic_value_short_description,characteristic_value_short_description.x , characteristic_value_short_description.y))

mergeddatapredictions1$product_class = factor(mergeddatapredictions1$product_class)

product_supergroup = gsub(pattern = "NANA", replacement = "", mergeddatapredictions1$product_class)
product_supergroup = gsub(pattern = "NA", replacement = "", product_supergroup$c..Carbonated.Mixers....Carbonated.Mixers....Carbonated.Mixers...)
product_supergroup <- as.list(product_supergroup)
product_supergroup <- do.call(rbind.data.frame, product_supergroup)

model139
model165

coefficients <- cbind(LogOdds = coef(model165), Odds = exp(coef(model165)))

write.csv(coefficients, file = "modelcoefficients.csv")

ranking <- varImp(model165, scale = FALSE)
write.csv(ranking, file = "feature_importance.csv")
coeff_stats <- coef(summary(model165))
write.csv(coeff_stats, file = "feature_coefficients.csv")

keep <- c("NZ NEW WORLD AK", "NZ NEW WORLD WG", "NZ NEW WORLD SI", "NZ COUNTDOWN")

mergedtest6d7d9dretailer <- mergedtest6d7d9d[mergedtest6d7d9d$retailer %in% keep,]
mergedtraining6d7d9dretailer <- mergedtraining6d7d9d[mergedtraining6d7d9d$retailer %in% keep, ]

