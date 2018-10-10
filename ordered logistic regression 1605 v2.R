# raw data has a column named display_level which has ordered levels coded to 1 (no display), 2 (minor) and 3 (major) 

final_data_coke_copy <- read.table(file = "/Users/hagen/final_data_coke.psv", sep = "|", header = TRUE, stringsAsFactors = TRUE)
rows_to_keep <- which(final_data_coke_copy$actual_price>0)
final_data_coke_copy <- final_data_coke_copy[rows_to_keep,]
final_data_coke_copy$new_causal_name[final_data_coke_copy$new_causal_name == 'Catalogue or Feature,Major Display'] <- 'Major Display'
final_data_coke_copy$new_causal_name[final_data_coke_copy$new_causal_name == 'Catalogue or Feature,Minor Display'] <- 'Minor Display'
final_data_coke_copy$new_causal_name[final_data_coke_copy$new_causal_name == 'Catalogue or Feature,Major Display,Minor Display'] <- 'Major Display'
rows_to_keep <- which(final_data_coke_copy$new_causal_name != 'Catalogue or Feature')
final_data_coke_copy <- final_data_coke_copy[rows_to_keep,]

sapply(final_data_coke_copy,function(x) sum(is.na(x)))

final_data_coke_copy_v2 <- final_data_coke_copy
final_data_coke_copy_v2["display_level"]<- NA
final_data_coke_copy_v2$display_level[final_data_coke_copy_v2$new_causal_name == 'Major Display']  <- 3
final_data_coke_copy_v2$display_level[final_data_coke_copy_v2$new_causal_name == 'Minor Display']  <- 2
final_data_coke_copy_v2$display_level[final_data_coke_copy_v2$new_causal_name == 'No display']  <- 1
final_data_coke_copy_v2$display_level <- as.ordered(final_data_coke_copy_v2$display_level)


library(MASS)
final_data_coke_copy_v2 <- final_data_coke_copy_v2[!is.na(final_data_coke_copy_v2$display_level),]
rownames(final_data_coke_copy_v2) <- NULL
summary(final_data_coke_copy_v2)
ind <- sample(2, nrow(final_data_coke_copy_v2), replace = TRUE, prob = c(0.8,0.2))
training_data <- final_data_coke_copy_v2[ind==1,]
test_data <- final_data_coke_copy_v2[ind==2,]

model90 <- polr(display_level ~ actual_price + corrected_sales_in_volume, training_data, Hess = TRUE)


(ctable <- coef(summary(model90)))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p ))
pred <- predict(model90, training_data[1:5,], type = 'prob')
pred
print(pred, digits  = 3)

model90 <- polr(display_level ~ .-promotion_id-final_baseline_in_volume-weighted_projection_factor-numeric_projection_factor-weighted_distribution-Major_Ind-Minor_Ind-Catalogue_feature_Ind-No_display_Ind-week_id, training_data, Hess = TRUE)

final_data_coke_copy$actual_price <- round(final_data_coke_copy$actual_price,2)


pred <- predict(model90, training_data, type = 'prob')
pred <- predict(model90, training_data)
(tab <- table(pred, training_data$display_level))

1-sum(diag(tab))/sum(tab)


pred1 <- predict(model90, test_data)
1-sum(diag(tab1))/sum(tab1)
(tab1 <- table(pred1, test_data$display_level))

#model90 <- polr(display_level ~ .-promotion_id-final_baseline_in_volume-weighted_projection_factor-numeric_projection_factor-weighted_distribution-Major_Ind-Minor_Ind-Catalogue_feature_Ind-No_display_Ind-week_id, training_data, Hess = TRUE)
