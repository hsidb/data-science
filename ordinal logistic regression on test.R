install.packages("VGAM")
library("VGAM")
test$display <- factor(test$display, levels = c("none", "Small", "Major", ordered = TRUE))


mo <- vglm(display ~ 1, family = cumulative(link = "logit", parallel = TRUE, reverse = TRUE), data = test)
summary(mo)
summary(mo)
plogis(-2.5974)
plogis(-2.7621)
plogis(-2.5974)- plogis(-2.7621)
1-plogis(-2.5974)
prop.table(table(test$display))
mo1 <- vglm(display ~ 1 + actual_price, family = cumulative(link = "logit", parallel = TRUE, reverse = TRUE), data = test)
summary(mo1)
cbind(LogOdds = coef(mo1), Odds = exp(coef(mo1)))
newdata <- data.frame(actual_price = seq(from = 6, to = 13, length.out = 700))
newdata <- cbind(newdata, predict(mo1, newdata = newdata, type = "response"))
library(reshape2)
newdata <- melt(newdata, id.vars = "actual_price")
p5 <- ggplot(newdata, aes(actual_price, value, color = variable, linetype= variable)) + geom_line(size = 1.5) + scale_x_continuous("actual_price", breaks = c(6,7,8,9,10,11,12,13)), labels = c(6,7,8,9,10,11,12,13)) + scale_y_continuous("Probability", labels = percent) +
theme_bw() +
theme(legend.key.width = unit(1.5, "cm"),
legend.position = "bottom",
legend.title = element_blank())
mo5 <- vglm(display ~ 1 + actual_price + corrected_sales_in_volume, family = cumulative(link = "logit", parallel = TRUE, reverse = TRUE), data = test)
cbind(LogOdds = coef(mo1), Odds = exp(coef(mo1)))
newdata2 <- data.frame(actual_price = seq(from = 6, to = 13, length.out = 700))
newdat1 <- data.frame( corrected_sales_in_volume = seq(from = 1, to = 45, length.out = 700))
newdata <- cbind(newdata2,newdat1)
newdata <- cbind(newdata, predict(mo11, newdata = newdata, type = "response"))
newdata <- melt(newdata, id.vars = "actual_price")
mo6 <- vglm(display ~ 1 + actual_price * corrected_sales_in_volume, family = cumulative(link = "logit", parallel = TRUE, reverse = TRUE), data = test)
newdata <- melt(newdata, id.vars = c("actual_price", "corrected_sales_in_volume"))

