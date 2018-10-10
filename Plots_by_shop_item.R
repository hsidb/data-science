#install.packages("ggplot2")
#install.packages("data.table")
#install.packages("RPostgreSQL")
#install.packages("reshape2")
#install.packages("bit64")

library(ggplot2)
library(data.table)
library(RPostgreSQL)
library(reshape2)
library(bit64)



######################################################
# Add color parameters for plots
######################################################
Palette_1 <- c("#78B45A","#58ABB8","#7F7F7F", "#000000","#51A7F9","#F4D792","#006000")

Palette_line <- c( "gray80",
"goldenrod1")

Palette_dots <- c(
"springgreen4",
"coral1",
"tomato4",
"red",
"blue2",
"darkviolet",
"royalblue3",
"black")

 

######################################################
# create a connection to the database
######################################################

# create a connection

pw <- {
  "michela123"
}
 
# loads the PostgreSQL driver
drv <- dbDriver("PostgreSQL")
# creates a connection to the postgres database
# note that "con" will be used later in each connection to the database
con <- dbConnect(drv, dbname = "rtp",
                 host = "localhost", port = 5432,
                 user = "michela", password = pw)
rm(pw) # removes the password
 
######################################################
# Check the connection
######################################################
dbExistsTable(con, c("nz_coke", "dex_dwh"))
#TRUE (answer has to be True, otherwise is not connected)



###########################################################
# Select causal information for then list of shops below
# The schema can be changed 
###########################################################

schema_cat <- "nz_coke"
causal_table <- "dex_cau"

causal_query <- paste0("select * from ",schema_cat,".",causal_table,
" where shop_id in (6400007229 ,6400007218, 6400008862, 6400005115, 6400008842, 6400007112 , 6400009911, 6400007255, 6400005110, 6400007208, 6400007110, 6400007925, 6400007623, 6400008947, 6400008928, 6400007203, 6400007211, 6400007228, 6400007210, 6400007201)")


nz_coke_dex_cau <- dbGetQuery(con, causal_query)

summary(nz_coke_dex_cau)
#    week_id         shop_id           item_id        causal_name       
# Min.   :832.0   Min.   :6.4e+09   Min.   :   9817   Length:776538     
# 1st Qu.:852.0   1st Qu.:6.4e+09   1st Qu.:  15518   Class :character  
# Median :875.0   Median :6.4e+09   Median :  22546   Mode  :character  
# Mean   :877.8   Mean   :6.4e+09   Mean   :1494497                     
# 3rd Qu.:902.0   3rd Qu.:6.4e+09   3rd Qu.:  33064                     
# Max.   :935.0   Max.   :6.4e+09   Max.   :6795805                     
#  causal_value
# Min.   :1    
# 1st Qu.:1    
# Median :1    
# Mean   :1    
# 3rd Qu.:1    
# Max.   :1    


table(nz_coke_dex_cau$causal_name)
 #     ADVERT/COUPON           LOW OTHER       MAJOR DISPLAY PROMO AISLE DISPLAY 
 #            687239               40976               33443               14880 

dim(nz_coke_dex_cau)
#[1] 776538      5

nz_coke_dex_cau <- data.table(nz_coke_dex_cau)

setkeyv(nz_coke_dex_cau, c("week_id","shop_id","item_id"))



###########################################################
# Correct the causal name
###########################################################

nz_coke_dex_cau[causal_name == "LOW OTHER", new_causal_name := "Minor Display"]
nz_coke_dex_cau[causal_name == "ADVERT/COUPON", new_causal_name := "Catalogue or Feature"]
nz_coke_dex_cau[causal_name == "MAJOR DISPLAY", new_causal_name := "Major Display"]
nz_coke_dex_cau[causal_name == "PROMO AISLE DISPLAY", new_causal_name := "Major Display"]


############################################################################################
# Colapses causal names in case there is more than one for the same shopping, item, week
# The output is nz_coke_dex_cau_simple
############################################################################################

cols_to_keep <- c("week_id","shop_id","item_id","new_causal_name")
nz_coke_dex_cau_simple <- nz_coke_dex_cau[,cols_to_keep, with=F] 

setkey(nz_coke_dex_cau_simple)
nz_coke_dex_cau_simple <- unique(nz_coke_dex_cau_simple)
dim(nz_coke_dex_cau_simple)
#776325      4

nz_coke_dex_cau_simple <- nz_coke_dex_cau_simple[,new_causal_name := paste(new_causal_name,collapse=","),by=c("week_id","shop_id","item_id")]

setkey(nz_coke_dex_cau_simple)
dim(nz_coke_dex_cau_simple)
#[1] 776325      4
nz_coke_dex_cau_simple <- unique(nz_coke_dex_cau_simple)
dim(nz_coke_dex_cau_simple)
#[1] 748545      4



############################################################################################
# Load a list of shop_ids to extract rows from dex_dwh
############################################################################################

list_of_shop_ids <- c(6400007229
,6400007218
,6400008862
,6400005115
,6400008842
,6400007112
,6400009911
,6400007255
,6400005110
,6400007208
,6400007110
,6400007925
,6400007623
,6400008947
,6400008928
,6400007203
,6400007211
,6400007228
,6400007210
,6400007201)


############################################################################################
# Quick analysis of the first and 20th - last - shop_id above to chosse the list of top 20 
# itens in sales volumes and merge these 2 lists
# The result is a list of itens that will be used to produce the plots
############################################################################################

#list of itens was defined by top 20 sales volume of store 6400007229 and store 6400007201
i=1 
the_query <- paste0("select * from nz_coke.dex_dwh where shop_id = ",list_of_shop_ids[i])
nz_coke_dex_dwh <- dbGetQuery(con, the_query)
nz_coke_dex_dwh <- data.table(nz_coke_dex_dwh)
test <- nz_coke_dex_dwh[, total_sales := sum(corrected_sales_in_volume), by = "item_id"]
cols_to_keep <- c("item_id","total_sales")
test <- unique(test[,cols_to_keep, with=F])
setorder(test, -total_sales)

head(test,20)
#    item_id total_sales
# 1: 6466019       53825
# 2:    9936       38793
# 3: 6241141       38351
# 4:   26653       34495
# 5:   25900       34386
# 6: 6466013       33001
# 7:   27836       28575
# 8:   13291       23449
# 9:   27876       23375
#10:   26045       22338
#11: 2975559       22104
#12:   27947       21648
#13: 6617200       21318
#14: 6453175       20828
#15:   25844       19683
#16: 6258612       19222
#17: 6357500       19187
#18:    9842       19145
#19:   15828       18899
#20: 6454670       18681
 

i=20 
the_query <- paste0("select * from nz_coke.dex_dwh where shop_id = ",list_of_shop_ids[i])
nz_coke_dex_dwh <- dbGetQuery(con, the_query)
nz_coke_dex_dwh <- data.table(nz_coke_dex_dwh)
test <- nz_coke_dex_dwh[, total_sales := sum(corrected_sales_in_volume), by = "item_id"]
cols_to_keep <- c("item_id","total_sales")
test <- unique(test[,cols_to_keep, with=F])
setorder(test, -total_sales)

head(test,20)
#    item_id total_sales
# 1:   13291       35925
# 2:   15828       23410
# 3:   15044       22417
# 4:   13317       21414
# 5:   13598       19120
# 6:   26653       18065
# 7:   29918       17762
# 8:    9936       15489
# 9: 6466019       15095
#10:   15827       14981
#11: 6241141       14190
#12:   22647       13818
#13:   22872       13570
#14:   29624       13517
#15:   13595       13514
#16:   25900       12104
#17:   15516       11457
#18:   15047       11217
#19:   16038       10624
#20:   25352        9489

############################################################################################
# Final list based on the analysys described above
# this list can be changed mannually below
############################################################################################

list_item <- c( 6466019 
,    9936 
, 6241141 
,   26653 
,   25900 
, 6466013 
,   27836 
,   13291 
,   27876 
,   26045 
, 2975559 
,   27947 
, 6617200 
, 6453175 
,   25844 
, 6258612 
, 6357500 
,    9842 
,   15828 
, 6454670 
,    13291
,   15828
,   15044
,   13317
,   13598
,   26653
,   29918
,    9936
, 6466019
,   15827
, 6241141
,   22647
,   22872
,   29624
,   13595
,  25900
,  15516
,  15047
,  16038
,  25352)
list_item <- unique(list_item)



############################################################################################
# Find product name and description
############################################################################################ 
the_query <- paste0("select * from nz_coke.dex_dim where item_id in ( ",paste(list_item,collapse=","),")")
product_description <- dbGetQuery(con, the_query)
product_description <- data.table(product_description)
cols_to_keep <- c("item_id", "product_group_description")
product_description <- product_description[,cols_to_keep, with=F ]
setkey(product_description, item_id)

#################################################################################################




Palette_1 <- c( "gray80",
"darkgoldenrod1",
"forestgreen",
"deepskyblue2",
"firebrick1",
"deeppink",
"darkorchid1",
"gold4",
"darkorange",
"cornsilk")


names(Palette_1) <- c("Sales volume",
"Actual price",
"Catalogue or Feature"
,"Catalogue or Feature,Major Display" 
,"Catalogue or Feature,Major Display,Minor Display" 
,"Catalogue or Feature,Minor Display" 
,"Major Display" 
,"Major Display,Minor Display"  
,"Minor Display"
,"No display" )




#################################################################################################
# Loop over the list of shopps and the list of itens above and generated one plot for each
# combination of shop+item
# The first plot is basically:
# - week number in axis X
# - 2 lines: price and volume (same axis Y)
# (worth mention that It's not possible to make 2 Y axis in ggplot2 for educational purpose)
# - coloured dots that show if there is any promotion ON in each week
# - Title with a shopp number and item number
################################################################################################


for(i in 1:length(list_of_shop_ids)) {
the_query <- paste0("select * from nz_coke.dex_dwh where shop_id = ",list_of_shop_ids[i],"  AND item_id in ( ",paste(list_item,collapse=","),")")
print(paste0("Start shopp ",i," number = ", list_of_shop_ids[i]))
nz_coke_dex_dwh <- dbGetQuery(con, the_query)
print(paste0("end query of shopp ",i," number = ", list_of_shop_ids[i]," with ", dim(nz_coke_dex_dwh)[1]," rows"))
nz_coke_dex_dwh <- data.table(nz_coke_dex_dwh)
setkeyv(nz_coke_dex_dwh, c("week_id","shop_id","item_id"))
setkeyv(nz_coke_dex_cau_simple, c("week_id","shop_id","item_id"))
data_to_plot <- nz_coke_dex_cau_simple[nz_coke_dex_dwh]
data_to_plot[is.na(new_causal_name), new_causal_name:= "No display"]
setkey(data_to_plot,item_id)
data_to_plot <- product_description[data_to_plot]

#data_to_plot <- fread("/Users/michelaguimaraes/Documents/Nielsen/test_15831.psv", sep = "|")
print(paste0("Start shopp ",i," number = ", list_of_shop_ids[i]))
cols_to_keep <- c("week_id","shop_id","item_id" ,"product_group_description", "new_causal_name"  ,"corrected_sales_in_volume","actual_price")
data_to_plot_final <- data_to_plot[,cols_to_keep,with=F]
data_to_plot_final$corrected_sales_in_volume <- as.numeric(data_to_plot_final$corrected_sales_in_volume)
data_to_plot_final <- melt(data_to_plot_final, id=1:5, measure=6:7)
data_to_plot_final[ variable == "corrected_sales_in_volume", variable := "Sales volume"]
data_to_plot_final[ variable == "actual_price", variable := "Actual price"]

print(paste0("Start plots shop ",i," out of ", length(list_of_shop_ids)))

for(j in 1:length(list_item)){ 
print(paste0("Start plots item ",j," out of ", length(list_item)))
data_to_plot_final_by_product <- data_to_plot_final[item_id == list_item[j]]
my_plot <- ggplot() + geom_point(data=data_to_plot_final_by_product, aes(x=week_id, y=value,group = new_causal_name, colour = new_causal_name)) + 
geom_line(data = data_to_plot_final_by_product, aes(x=week_id, y= value,  group = variable, colour = variable))
my_plot <- my_plot + scale_colour_manual(values = Palette_1)                              
my_plot <- my_plot + ggtitle (paste0("Weekly volume X price by promotion\n item ", data_to_plot_final_by_product$product_group_description[1]," shop ", data_to_plot_final_by_product$shop_id[1])) 
my_plot <- my_plot + theme(plot.title = element_text (size= as.numeric(16), face= "plain",hjust = 0.5),
                              panel.background=element_blank(), 
                              axis.line = element_line(size = 0.7, colour = "black"),
                              legend.position = "bottom",
                              legend.text = element_text(size = as.numeric(12)),
                              legend.title = element_blank(),
                              axis.text = element_text(size = as.numeric(10), colour = "black"),
                              axis.title = element_text(size = as.numeric(12), face = "plain"),
                              panel.grid.major.y = element_line(colour = "#D9D9D9", size=0.3),
                              panel.grid.minor.y = element_blank(),
                              panel.grid.major.x = element_blank(),
                              panel.grid.minor.x = element_blank(),
                              plot.background = element_blank(),
                              complete = FALSE)
ggsave((paste0('/home/michela/plots/plot_1_nz_coke_shop_',list_of_shop_ids[i],'_item_',list_item[j],'.pdf')), width=8.2, height=6)
print(paste0("End plots item ",j," out of ", length(list_item)," product ",i," out of ",length(list_of_shop_ids) ))}}


#################################################################################################
# INDEX plot
# Loop over the list of shopps and the list of itens above and generated one plot for each
# combination of shop+item
# This plot is basically:
# - week number in axis X
# - 2 lines: Index price (price / median(price)) and Index volume (sales volume / average(sales volume))
# (worth mention that It's not possible to make 2 Y axis in ggplot2 for educational purpose)
# - coloured dots that show if there is any promotion ON in each week
# - Title with a shopp number and item number
################################################################################################


for(i in 1:length(list_of_shop_ids)) {
the_query <- paste0("select * from nz_coke.dex_dwh where shop_id = ",list_of_shop_ids[i],"  AND item_id in ( ",paste(list_item,collapse=","),")")
print(paste0("Start shopp ",i," number = ", list_of_shop_ids[i]))
nz_coke_dex_dwh <- dbGetQuery(con, the_query)
print(paste0("end query of shopp ",i," number = ", list_of_shop_ids[i]," with ", dim(nz_coke_dex_dwh)[1]," rows"))
nz_coke_dex_dwh <- data.table(nz_coke_dex_dwh)
setkeyv(nz_coke_dex_dwh, c("week_id","shop_id","item_id"))
setkeyv(nz_coke_dex_cau_simple, c("week_id","shop_id","item_id"))
data_to_plot <- nz_coke_dex_cau_simple[nz_coke_dex_dwh]
data_to_plot[is.na(new_causal_name), new_causal_name:= "No display"]
data_to_plot <- data_to_plot[item_id %in% list_item]
setkey(data_to_plot,item_id)
data_to_plot <- product_description[data_to_plot]

#data_to_plot <- fread("/Users/michelaguimaraes/Documents/Nielsen/test_15831.psv", sep = "|")
print(paste0("Start shopp ",i," number = ", list_of_shop_ids[i]))
cols_to_keep <- c("week_id","shop_id","item_id" ,"product_group_description", "new_causal_name"  ,"corrected_sales_in_volume","actual_price")
data_to_plot_final <- data_to_plot[,cols_to_keep,with=F]
data_to_plot_final$corrected_sales_in_volume <- as.numeric(data_to_plot_final$corrected_sales_in_volume)
data_to_plot_final <- melt(data_to_plot_final, id=1:5, measure=6:7)
data_to_plot_final[ variable == "corrected_sales_in_volume", variable := "Sales volume"]
data_to_plot_final[ variable == "actual_price", variable := "Actual price"]
data_to_plot_final[,`:=` (
 median = median(value,na.rm = TRUE),
 average = mean(value,na.rm = TRUE),
 maxim = max(value,na.rm = TRUE)), by=c("variable","shop_id","item_id")]
 data_to_plot_final[,`:=` (
 index_median = (value/median),
 index_ave = (value/average),
 index_max = (value/maxim))]
 data_to_plot_final[ variable == "Sales volume", value_to_plot := index_ave]
 data_to_plot_final[ variable == "Actual price", value_to_plot := index_median]


print(paste0("Start plots shop ",i," out of ", length(list_of_shop_ids)))


for(j in 1:length(list_item)){ 
print(paste0("Start plots item ",j," out of ", length(list_item)))
data_to_plot_final_by_product <- data_to_plot_final[item_id == list_item[j]]
my_plot <- ggplot() + geom_point(data=data_to_plot_final_by_product, aes(x=week_id, y=value_to_plot,group = new_causal_name, colour = new_causal_name)) + 
geom_line(data = data_to_plot_final_by_product, aes(x=week_id, y= value_to_plot,  group = variable, colour = variable))
my_plot <- my_plot + scale_colour_manual(values = Palette_1)    
my_plot <- my_plot + scale_y_continuous("Index (Price and Sales volume)") 
my_plot <- my_plot + scale_x_continuous("Week")                             
my_plot <- my_plot + ggtitle (paste0("Weekly volume X price by promotion\n item ", data_to_plot_final_by_product$product_group_description[1]," shop ", data_to_plot_final_by_product$shop_id[1])) 
my_plot <- my_plot + theme(plot.title = element_text (size= as.numeric(16), face= "plain",hjust = 0.5),
                              panel.background=element_blank(), 
                              axis.line = element_line(size = 0.7, colour = "black"),
                              legend.position = "bottom",
                              legend.text = element_text(size = as.numeric(12)),
                              legend.title = element_blank(),
                              axis.text = element_text(size = as.numeric(10), colour = "black"),
                              axis.title = element_text(size = as.numeric(12), face = "plain"),
                              panel.grid.major.y = element_line(colour = "#D9D9D9", size=0.3),
                              panel.grid.minor.y = element_blank(),
                              panel.grid.major.x = element_blank(),
                              panel.grid.minor.x = element_blank(),
                              plot.background = element_blank(),
                              complete = FALSE)
my_plot <- my_plot + guides(fill=guide_legend(nrow=3,byrow=TRUE))
ggsave((paste0('/home/michela/plots/index_shop_',list_of_shop_ids[i],'_item_',list_item[j],'.pdf')), width=8.2, height=6)
print(paste0("End plots item ",j," out of ", length(list_item)," product ",i," out of ",length(list_of_shop_ids) ))}}

