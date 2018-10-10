
library(data.table)
library(RPostgreSQL)
library(reshape2)
library(bit64)



######################################################
# create a connection to the database
######################################################

# create a connection

pw <- {
  "hagen123"
}

# loads the PostgreSQL driver
drv <- dbDriver("PostgreSQL")
# creates a connection to the postgres database
# note that "con" will be used later in each connection to the database
con <- dbConnect(drv, dbname = "rtp",
                 host = "localhost", port = 5432,
                 user = "hagen", password = pw)
rm(pw) # removes the password

######################################################
# Check the connection
######################################################
dbExistsTable(con, c("nz_ambient", "dex_dwh"))
#TRUE (answer has to be True, otherwise is not connected)



######################################################
# Select only Super stores
######################################################

query_select <- paste0("select shop_id from nz_coke.causal_shop ")
#query_select <- paste0("select * from nz_ambient_food.dex_dmf WHERE characteristic_short_description LIKE 'AAC_StoreType' AND characteristic_value_long_description LIKE 'SUPER' ")

all_super_stores <- dbGetQuery(con, query_select)

dim(all_super_stores)
#[1] 57661     7

all_super_stores <- data.table(all_super_stores)

count_shop_ids <- all_super_stores[,.N, by = shop_id]

dim(count_shop_ids)
#[1] 396   2


super_shops_ids <- paste(count_shop_ids$shop_id,collapse=",")


write.table(super_shops_ids, "/home/hagen/nz_ambient_food/super_shops_ids.psv", sep = "|", row.names = FALSE, col.names = TRUE)

######################################################
# Causal info from selected stores
######################################################

schema_cat <- "nz_ambient"
causal_table <- "dex_cau"

causal_query <- paste0("select * from ",schema_cat,".",causal_table,
                       " where shop_id in (",super_shops_ids,")" )

dex_cau <- dbGetQuery(con, causal_query)

dex_cau <- data.table(dex_cau)

dex_cau[causal_name == "LOW OTHER", new_causal_name := "Minor Display"]
dex_cau[causal_name == "ADVERT/COUPON", new_causal_name := "Catalogue or Feature"]
dex_cau[causal_name == "MAJOR DISPLAY", new_causal_name := "Major Display"]
dex_cau[causal_name == "PROMO AISLE DISPLAY", new_causal_name := "Major Display"]

write.table(dex_cau, "/home/hagen/nz_ambient_food/dex_cau.psv", sep = "|", row.names = FALSE, col.names = TRUE)

#################################################################
# Count causal information by store by item of ALL SUPER stores 
#################################################################

count_causal_by_item_store <- dex_cau[, total := sum(causal_value), by = list(shop_id,item_id,new_causal_name)]
cols_to_keep <- c("shop_id","item_id","new_causal_name", "total")
count_causal_by_item_store <- count_causal_by_item_store[,cols_to_keep, with = FALSE]
count_causal_by_item_store <- unique(count_causal_by_item_store)

dim(count_causal_by_item_store)
#[1] 545072      4


major_minor_only <- count_causal_by_item_store[new_causal_name %in% c("Minor Display","Major Display")]
setkey(major_minor_only)
dim(major_minor_only)
#[1] 118387      4


setkey(major_minor_only, total)

setkey(major_minor_only)

###########################################################
# Select a random sample of items + stores in the causal table
###########################################################

# sample_of_stores_itens <- major_minor_only[sample(nrow(major_minor_only), trunc(nrow(major_minor_only)*0.003)), ]
# dim(sample_of_stores_itens)
# #[1] 355   4
# 
# ###########################################################
# # Compare distribution of sample and full population
# ###########################################################
# 
# causal_density_data <- major_minor_only[, .N, by = list(new_causal_name,total)]
# causal_density_data <- causal_density_data[,total_2 := sum(N), by = list(new_causal_name)]
# causal_density_data[,prop := (N/total_2)]
# 
# 
# causal_density_data_sample <- sample_of_stores_itens[, .N, by = list(new_causal_name,total)]
# 
# causal_density_data_sample <- causal_density_data_sample[,total_2 := sum(N), by = list(new_causal_name)]
# causal_density_data_sample[,prop := (N/total_2)]
# 
# setkeyv(causal_density_data_sample, c("total", "new_causal_name"))
# setkeyv(causal_density_data, c("total", "new_causal_name"))


######################################################
# Sum sales volume by item, min and max week, for selected stores
######################################################


the_query <- paste0("select shop_id, item_id, min(week_id), max(week_id), sum(corrected_sales_in_volume) from nz_ambient.dex_dwh where shop_id in (",super_shops_ids,") group by shop_id, item_id")

sales_by_store <- dbGetQuery(con, the_query)

dim(sales_by_store)
#[1]  330530      5

sales_by_store <- data.table(sales_by_store)
setnames(sales_by_store, names(sales_by_store)[3:5], c("min_week", "max_week", "sum_corrected_sales_in_volume"))
sales_by_store [,total_weeks := (max_week - min_week)]
sales_by_store [,ave_sales_vol_week := (sum_corrected_sales_in_volume/ total_weeks)]

write.table(sales_by_store, "/home/hagen/nz_ambient_food/sales_by_store.psv", sep = "|", row.names = FALSE, col.names = TRUE)

#########################################################################
# Select top 5 and bottom 5 and middle 5 items (sales volume) per store
#########################################################################


sales_by_store_59_weeks <- sales_by_store[ total_weeks == 59]
setkeyv(sales_by_store_59_weeks, c("shop_id","sum_corrected_sales_in_volume"))
# 
dim(sales_by_store)
# #[1] 330530      7
dim(sales_by_store_59_weeks)
# #[1] 119804      7
# 
# 119804/330530
# #[1] 0.3624603
# 
# #bottom_5 <- sales_by_store_59_weeks[, head(.SD, 5), by=shop_id]
# #top_5 <- sales_by_store_59_weeks[, tail(.SD, 5), by=shop_id]
top_25 <- sales_by_store_59_weeks[, tail(.SD, 32), by=shop_id]

# count_itens <- sales_by_store_59_weeks[,.N , by=shop_id]
# count_itens[,N3 := as.integer(trunc(N/2)),by=shop_id]
# 
# count_itens[,`:=` (
#   N1 = as.integer(N3-2),
#   N2 = as.integer(N3-1),
#   N4 = as.integer(N3+1),
#   N5 = as.integer(N3+2)), by=shop_id]
# 
# 
# count_itens <- melt(count_itens, id=1, measure=2:7)
# count_itens <- count_itens[variable != "N"]
# 


# setkeyv(sales_by_store_59_weeks, c("shop_id","sum_corrected_sales_in_volume"))
# sales_by_store_59_weeks[, value := sequence(.N), by = shop_id]
# 
# setkeyv(sales_by_store_59_weeks, c("shop_id","value"))
# setkeyv(count_itens, c("shop_id","value"))
# 
# middle_5 <- sales_by_store_59_weeks[count_itens]
# to_del <- c( "value" ,"variable")
# middle_5[,(to_del) := NULL]
# 
# bottom_5$item_selected <- "bottom" 
# middle_5$item_selected <- "middle" 
top_25$item_selected <- "top" 

# itens_shops <- rbind(bottom_5,middle_5,top_5)


##################################################################################
# Select top 5 and bottom 5 and middle 5 stores (per sales volume) sum all items
##################################################################################


# sales_by_store_59_weeks_2 <- sales_by_store_59_weeks[,sum_sales_all_itens := sum(sum_corrected_sales_in_volume), by = shop_id]
# cols_to_keep <- c("shop_id","sum_sales_all_itens")
# sales_by_store_59_weeks_2 <- sales_by_store_59_weeks_2[,cols_to_keep, with=FALSE]
# 
# sales_by_store_59_weeks_2 <- unique(sales_by_store_59_weeks_2)
# setkey(sales_by_store_59_weeks_2,sum_sales_all_itens)
# 
# #bottom_5 <- sales_by_store_59_weeks_2[, head(.SD, 5)]
# top_25 <- sales_by_store_59_weeks_2[, tail(.SD, 25)]
# 
# 
# dim(sales_by_store_59_weeks_2)
# #[1] 345   2

# the_middle <- as.integer(trunc(nrow(sales_by_store_59_weeks_2)/2))
# 
# middle_5 <- rbind(sales_by_store_59_weeks_2[the_middle], sales_by_store_59_weeks_2[the_middle+1], sales_by_store_59_weeks_2[the_middle+2], sales_by_store_59_weeks_2[the_middle-1], sales_by_store_59_weeks_2[the_middle-2])
# 
# bottom_5$shop_selected <- "bottom"
top_25$shop_selected <- " list of 70"
# middle_5$shop_selected <- "middle"
# 
# shops <-  rbind(bottom_5,middle_5,top_5)
#shops <- top_25

##################################################################################
# Select just 15 shops from itens_shops and create final list
##################################################################################

# setkey(shops, shop_id)
# setkey(itens_shops, shop_id)
# 
#final_list_shops_itens <- itens_shops[shops]
final_list_shops_itens <- top_25
write.table(final_list_shops_itens, "/home/hagen/nz_ambient_food/final_list_shops_itens.psv", sep = "|", row.names = FALSE, col.names = TRUE)


###########################################################
# Add sample from dex_cau to the final list
###########################################################

cols_to_keep <- c("shop_id" ,"item_id", "item_selected" ,"shop_selected"  )
final_list_shops_itens_small <- final_list_shops_itens[,cols_to_keep, with = FALSE]
# 
# sample_of_stores_itens$item_selected <- "random_causal"
# sample_of_stores_itens$shop_selected <- "random_causal"
# 
# sample_of_stores_itens_small <- sample_of_stores_itens[,cols_to_keep, with = FALSE]

# final_list_shops_itens_small[,shop_id:= as.integer64(shop_id)]
# final_list_shops_itens_small[,item_id:= as.integer(item_id)]
# 
# sample_of_stores_itens_small[,shop_id:= as.integer64(shop_id)]
# sample_of_stores_itens_small[,item_id:= as.integer(item_id)]
# 
# final_list_shops_itens_small <- rbind(final_list_shops_itens_small,sample_of_stores_itens_small)
# dim(final_list_shops_itens_small)
# #[1] 580   4
# setkey(final_list_shops_itens_small)
# final_list_shops_itens_small <- unique(final_list_shops_itens_small)
# dim(final_list_shops_itens_small)
# #[1] 580   4

write.table(final_list_shops_itens_small, "/home/hagen/nz_ambient_food/final_list_shops_itens_small.psv", sep = "|", row.names = FALSE, col.names = TRUE)


##################################################################################
# Create table by shop and list of itens
##################################################################################


final_list_shops_itens_small[, list_of_itens := paste(item_id, collapse=","), by = shop_id]
cols_to_keep <- c("shop_id" ,"list_of_itens" )
final_list_shops_itens_small_short <- unique(final_list_shops_itens_small[,cols_to_keep, with = FALSE])

cols_to_keep <- c("shop_id" ,"item_id" )
final_list_shops_itens_small_long <- unique(final_list_shops_itens_small[,cols_to_keep, with = FALSE])

dim(final_list_shops_itens_small_short)
#[1] 216   2
dim(final_list_shops_itens_small_long)
#[1] 580   2

write.table(final_list_shops_itens_small_short, "/home/hagen/nz_ambient_food/final_list_shops_itens_small_short.psv", sep = "|", row.names = FALSE, col.names = TRUE)
write.table(final_list_shops_itens_small_long, "/home/hagen/nz_ambient_food/final_list_shops_itens_small_long.psv", sep = "|", row.names = FALSE, col.names = TRUE)


######################################################
# Read list of shops and items
######################################################

final_list_shops_itens <- fread("/home/hagen/nz_ambient_food/final_list_shops_itens_small.psv", sep = "|")
final_list_shops_itens_small <- fread("/home/hagen/nz_ambient_food/final_list_shops_itens_small.psv", sep = "|")
final_list_shops_itens_small_short <- fread("/home/hagen/nz_ambient_food/final_list_shops_itens_small_short.psv", sep = "|")
final_list_shops_itens_small_long <- fread("/home/hagen/nz_ambient_food/final_list_shops_itens_small_long.psv", sep = "|")
dex_cau <- fread("/home/hagen/nz_ambient_food/dex_cau.psv", sep = "|")
super_shops_ids<- fread("/home/hagen/nz_ambient_food/super_shops_ids.psv", sep = "|")

# dim(final_list_shops_itens_small)
# #[1] 580   4
# 
# dim(final_list_shops_itens_small_short)
# #[1] 216   2
# 
# dim(final_list_shops_itens_small_long)
# #[1] 580   4





###########################################################
# Select causal info from list of stores only
###########################################################

setkeyv(final_list_shops_itens_small_long, c("shop_id","item_id"))
setkeyv(dex_cau, c("shop_id","item_id"))

dex_cau_selected_stores_items <- dex_cau[final_list_shops_itens_small_long]
dex_cau_selected_stores_items$total <- NULL

dim(dex_cau_selected_stores_items)
#[1] 10574     6


dex_cau_selected_stores_items <- dex_cau_selected_stores_items[!is.na(causal_name)]

dim(dex_cau_selected_stores_items)
#[1] 10431     6

############################################################################################
# Colapses causal names in case there is more than one for the same shopping, item, week
# The output is nz_ambient_food_dex_cau_simple
############################################################################################

cols_to_keep <- c("week_id","shop_id","item_id","new_causal_name")
nz_ambient_food_dex_cau_simple <- dex_cau_selected_stores_items[,cols_to_keep, with=F] 

setkey(nz_ambient_food_dex_cau_simple)
nz_ambient_food_dex_cau_simple <- unique(nz_ambient_food_dex_cau_simple)
dim(nz_ambient_food_dex_cau_simple)
#[[1] 10429     4
nz_ambient_food_dex_cau_simple <- nz_ambient_food_dex_cau_simple[,new_causal_name := paste(new_causal_name,collapse=","),by=c("week_id","shop_id","item_id")]

nz_ambient_food_dex_cau_simple <- nz_ambient_food_dex_cau_simple[!is.na(week_id)]
nz_ambient_food_dex_cau_simple <- nz_ambient_food_dex_cau_simple[!is.na(new_causal_name)]
dim(nz_ambient_food_dex_cau_simple)
#[1] 10429     4



############################################################################################
# Select data from dex_dwh
############################################################################################

nz_ambient_food_dex_dwh <- data.table()
for(i in 1:nrow(final_list_shops_itens_small_short)) {
  the_query <- paste0("select * from nz_ambient.dex_dwh where shop_id = ",final_list_shops_itens_small_short$shop_id[i], " AND item_id in (",final_list_shops_itens_small_short$list_of_itens[i]," )"  )
  print(paste0("Start ",i," out of ", nrow(final_list_shops_itens_small_short)))
  temp_1 <- dbGetQuery(con, the_query)
  print(paste0("End ",i," out of ", nrow(final_list_shops_itens_small_short)))
  nz_ambient_food_dex_dwh <- rbind(nz_ambient_food_dex_dwh, temp_1)}

dim(nz_ambient_food_dex_dwh)
#[1]32952    12
setkey(nz_ambient_food_dex_dwh)

nz_ambient_food_dex_dwh <- unique(nz_ambient_food_dex_dwh)

dim(nz_ambient_food_dex_dwh)
#[1] 72867    12

############################################################################################
# Find product description
############################################################################################ 

product_description <- data.table()
for(i in 1:nrow(final_list_shops_itens_small_short)) {
  the_query <- paste0("select * from nz_ambient.dex_dim where item_id in ( ",final_list_shops_itens_small_short$list_of_itens[i]," )"  )
  temp_1 <- dbGetQuery(con, the_query)
  product_description <- rbind(product_description, temp_1)}

dim(product_description)
# [1] 580   7

cols_to_keep <- c("item_id", "product_group_description")
product_description <- product_description[,cols_to_keep, with=F ]
setkey(product_description, item_id)
product_description <- unique(product_description)
dim(product_description)
# 363   2


##########################################
# Join data
##########################################

setkey(nz_ambient_food_dex_dwh, item_id)
setkey(product_description, item_id)

final_data_ambient_food <- product_description[nz_ambient_food_dex_dwh, allow.cartesian=TRUE]
dim(final_data_ambient_food)
#[1] 72867    13


setkeyv(final_data_ambient_food,c("item_id", "week_id","shop_id"))
setkeyv(nz_ambient_food_dex_cau_simple,c("item_id", "week_id","shop_id"))
final_data_ambient_food <- nz_ambient_food_dex_cau_simple[final_data_ambient_food]
dim(final_data_ambient_food)
#[1] 73822    14

setkeyv(final_data_ambient_food,c("item_id", "shop_id"))
setkeyv(final_list_shops_itens_small,c("item_id", "shop_id"))

final_data_ambient_food <- final_list_shops_itens_small[final_data_ambient_food]
final_data_ambient_food[is.na(new_causal_name), new_causal_name:= "No display"]

##########################################
# Add indicators of causal
##########################################

final_data_ambient_food$Major_Ind <- as.integer(grepl(pattern = "Major Display", x = final_data_ambient_food$new_causal_name))
final_data_ambient_food$Minor_Ind <- as.integer(grepl(pattern = "Minor Display", x = final_data_ambient_food$new_causal_name))
final_data_ambient_food$Catalogue_feature_Ind <- as.integer(grepl(pattern = "Catalogue or Feature", x = final_data_ambient_food$new_causal_name))
final_data_ambient_food$No_display_Ind <- as.integer(grepl(pattern = "No display", x = final_data_ambient_food$new_causal_name))



##########################################
# Read all details from selected stores
##########################################

query_2 <- paste0("select * from nz_ambient.dex_dmf WHERE characteristic_short_description = 'AAC_Retailer' AND shop_id in (",super_shops_ids,")" )

dex_dmf <- dbGetQuery(con, query_2)

##########################################
# Keep retailer name
##########################################
dex_dmf <- data.table(dex_dmf)
cols_to_keep <- c("week_id","shop_id","characteristic_value_long_description")
dex_dmf <- dex_dmf[,cols_to_keep,with=F]
setkey(dex_dmf)
dex_dmf <- unique(dex_dmf)
setnames(dex_dmf,"characteristic_value_long_description", "retailer")


##########################################
# Join retailer name in the final data
##########################################

setkeyv(dex_dmf, c("week_id","shop_id"))
setkeyv(final_data_ambient_food, c("week_id","shop_id"))

final_data_ambient_food <- dex_dmf[final_data_ambient_food]

##########################################
# Save data
##########################################
write.table(final_data_ambient_food, "/home/hagen/nz_ambient_food/final_data_ambient_70.psv", sep = "|", row.names = FALSE, col.names = TRUE)