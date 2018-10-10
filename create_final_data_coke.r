
#install.packages("data.table")
#install.packages("RPostgreSQL")
#install.packages("bit64")

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



######################################################
# Select only Super stores
######################################################


query_select <- paste0("select * from nz_coke.dex_dmf WHERE characteristic_short_description LIKE 'AAC_StoreType' AND characteristic_value_long_description LIKE 'SUPER' ")

all_super_stores <- dbGetQuery(con, query_select)


dim(all_super_stores)
#[1] 38734     7

all_super_stores <- data.table(all_super_stores)

count_shop_ids <- all_super_stores[,.N, by = shop_id]

dim(count_shop_ids)
#[1] 388   2

super_shops_ids <- paste(count_shop_ids$shop_id,collapse=",")

super_shops_ids_v2 <- c(6400001395,6400003048,6400005110,6400005111,6400005115,6400005116,
6400005117,6400005118,6400005119,6400005210,6400005309,6400005419,6400005431,6400005614,
6400005700,6400005828,6400005846,6400005847,6400005865,6400006304,6400006326,6400006340,
6400006393,6400006395,6400006524,6400006560,6400006561,6400006602,6400006603,6400006622,
6400006623,6400007101,6400007102,6400007103,6400007105,6400007109,6400007110,6400007112,
6400007114,6400007115,6400007116,6400007163,6400007164,6400007165,6400007166,6400007167,
6400007171,6400007172,6400007173,6400007174,6400007176,6400007177,6400007178,6400007182,
6400007183,6400007184,6400007185,6400007186,6400007191,6400007201,6400007202,6400007203,
6400007206,6400007207,6400007208,6400007209,6400007210,6400007211,6400007212,6400007214,
6400007217,6400007218,6400007219,6400007220,6400007223,6400007225,6400007226,6400007227,
6400007228,6400007229,6400007231,6400007233,6400007255,6400007266,6400007267,6400007268,
6400007269,6400007282,6400007283,6400007285,6400007286,6400007287,6400007289,6400007290,
6400007600,6400007602,6400007604,6400007605,6400007606,6400007607,6400007608,6400007615,
6400007617,6400007620,6400007621,6400007622,6400007623,6400007624,6400007627,6400007628,
6400007702,6400007703,6400007709,6400007810,6400007811,6400007812,6400007816,6400007818,
6400007819,6400007822,6400007824,6400007825,6400007828,6400007830,6400007832,6400007837,
6400007838,6400007839,6400007840,6400007842,6400007843,6400007904,6400007907,6400007911,
6400007914,6400007915,6400007916,6400007917,6400007919,6400007920,6400007925,6400007926,
6400008312,6400008316,6400008317,6400008318,6400008319,6400008320,6400008322,6400008325,
6400008326,6400008327,6400008331,6400008333,6400008334,6400008335,6400008336,6400008337,
6400008338,6400008339,6400008340,6400008341,6400008342,6400008343,6400008346,6400008348,
6400008349,6400008401,6400008402,6400008403,6400008504,6400008506,6400008507,6400008509,
6400008607,6400008608,6400008609,6400008610,6400008612,6400008614,6400008615,6400008616,
6400008617,6400008618,6400008619,6400008701,6400008702,6400008803,6400008804,6400008814,
6400008821,6400008822,6400008823,6400008830,6400008831,6400008832,6400008839,6400008841,
6400008842,6400008844,6400008850,6400008852,6400008853,6400008858,6400008859,6400008860,
6400008861,6400008862,6400008863,6400008867,6400008869,6400008870,6400008871,6400008872,
6400008873,6400008874,6400008875,6400008876,6400008877,6400008878,6400008881,6400008882,6400008886,6400008887,6400008889,
6400008893,6400008897,6400008898,6400008901,6400008904,6400008906,6400008907,6400008909,6400008911,6400008912,6400008913,
6400008914,6400008919,6400008921,6400008923,6400008924,6400008925,6400008928,6400008930,6400008934,6400008935,6400008936,
6400008937,6400008938,6400008940,6400008942,6400008943,6400008944,6400008947,6400008948,6400008949,6400008950,6400008951,
6400008952,6400008953,6400008955,6400008956,6400008957,6400008958,6400008959,6400008961,6400008962,6400008963,6400008964,
6400008965,6400008967,6400008968,6400008969,6400008970,6400008971,6400008972,6400008973,6400008974,6400008975,6400008976,
6400008977,6400008978,6400008979,6400008980,6400008981,6400008982,6400008983,6400008984,6400008985,6400008986,6400008987,
6400008988,6400008989,6400008990,6400008991,6400008992,6400008993,6400008994,6400008995,6400008996,6400008997,6400008998,
6400008999,6400009900,6400009901,6400009902,6400009903,6400009904,6400009905,6400009906,6400009907,6400009909,6400009910,
6400009911,6400009912,6400009913,6400009914,6400009915,6400009916,6400009919,6400009932,6400009933,6400009934,6400009935,
6400009936,6400009937,6400009938,6400009939,6400009940,6400009941,6400009942,6400009943,6400009944,6400009945,6400009946,
6400009947,6400009948,6400009949,6400009950,6400040035,6400040081,6406001603,6406001650,6406001651,6406001754,6406002541,
6406002594,6406002739,6406003055,6406003112,6406003143,6406003351,6406003525,6406004312,6406005460,6406006764,6406007052,
6406007053,6406007066,6406102010,6406102014,6406102098,6406102099,6406102111,6400008885,6406102112,6406102133,6406102190,
6406102191,6406102192,6406102209,6406102213,6406102224,6406102233,6406102234,6406102236,6406102238,6406102239,6406102355,
6406102398,6406102415,6406102424,6406102469,6406102565,6406102586,6406102603,6406102620,6406102626)


write.table(super_shops_ids, "/home/michela/super_shops_ids.psv", sep = "|", row.names = FALSE, col.names = TRUE)

######################################################
# Causal info from selected stores
######################################################

schema_cat <- "nz_coke"
causal_table <- "dex_cau"

causal_query <- paste0("select * from ",schema_cat,".",causal_table,
" where shop_id in (",super_shops_ids,")" )

dex_cau <- dbGetQuery(con, causal_query)

write.table(dex_cau, "/home/michela/dex_cau.psv", sep = "|", row.names = FALSE, col.names = TRUE)
dex_cau <- data.table(dex_cau)

dex_cau[causal_name == "LOW OTHER", new_causal_name := "Minor Display"]
dex_cau[causal_name == "ADVERT/COUPON", new_causal_name := "Catalogue or Feature"]
dex_cau[causal_name == "MAJOR DISPLAY", new_causal_name := "Major Display"]
dex_cau[causal_name == "PROMO AISLE DISPLAY", new_causal_name := "Major Display"]


#################################################################
# Count causal information by store by item of ALL SUPER stores 
#################################################################

count_causal_by_item_store <- dex_cau[, total := sum(causal_value), by = list(shop_id,item_id,new_causal_name)]
cols_to_keep <- c("shop_id","item_id","new_causal_name", "total")
count_causal_by_item_store <- count_causal_by_item_store[,cols_to_keep, with = FALSE]
count_causal_by_item_store <- unique(count_causal_by_item_store)

dim(count_causal_by_item_store)
#[1] 858215      4

major_minor_only <- count_causal_by_item_store[new_causal_name %in% c("Minor Display","Major Display")]
setkey(major_minor_only)
dim(major_minor_only)
#[1] 164862      4

setkey(major_minor_only, total)

table(major_minor_only$total,major_minor_only$new_causal_name)




###########################################################
# Select a random sample of items + stores in the causal table
###########################################################

sample_of_stores_itens <- major_minor_only[sample(nrow(major_minor_only), trunc(nrow(major_minor_only)*0.002)), ]

###########################################################
# Compare distribution of sample and full population
###########################################################

causal_density_data <- major_minor_only[, .N, by = list(new_causal_name,total)]
causal_density_data <- causal_density_data[,total_2 := sum(N), by = list(new_causal_name)]
causal_density_data[,prop := (N/total_2)]


causal_density_data_sample <- sample_of_stores_itens[, .N, by = list(new_causal_name,total)]

causal_density_data_sample <- causal_density_data_sample[,total_2 := sum(N), by = list(new_causal_name)]
causal_density_data_sample[,prop := (N/total_2)]

setkeyv(causal_density_data_sample, c("total", "new_causal_name"))
setkeyv(causal_density_data, c("total", "new_causal_name"))


######################################################
# Sum sales volume by item, min and max week, for selected stores
######################################################


the_query <- paste0("select shop_id, item_id, min(week_id), max(week_id), sum(corrected_sales_in_volume) from nz_coke.dex_dwh where shop_id in (",super_shops_ids,") group by shop_id, item_id")

sales_by_store <- dbGetQuery(con, the_query)

sales_by_store <- data.table(sales_by_store)
setnames(sales_by_store, names(sales_by_store)[3:5], c("min_week", "max_week", "sum_corrected_sales_in_volume"))
sales_by_store [,total_weeks := (max_week - min_week)]
sales_by_store [,ave_sales_vol_week := (sum_corrected_sales_in_volume/ total_weeks)]

write.table(sales_by_store, "/home/michela/sales_by_store.psv", sep = "|", row.names = FALSE, col.names = TRUE)

#########################################################################
# Select top 5 and bottom 5 and middle 5 items (sales volume) per store
#########################################################################


sales_by_store_103_weeks <- sales_by_store[ total_weeks == 103]
setkeyv(sales_by_store_103_weeks, c("shop_id","sum_corrected_sales_in_volume"))

bottom_5 <- sales_by_store_103_weeks[, head(.SD, 5), by=shop_id]
top_5 <- sales_by_store_103_weeks[, tail(.SD, 5), by=shop_id]

count_itens <- sales_by_store_103_weeks[,.N , by=shop_id]
count_itens[,N3 := as.integer(trunc(N/2)),by=shop_id]

count_itens[,`:=` (
 N1 = as.integer(N3-2),
 N2 = as.integer(N3-1),
 N4 = as.integer(N3+1),
 N5 = as.integer(N3+2)), by=shop_id]


count_itens <- melt(count_itens, id=1, measure=2:7)
count_itens <- count_itens[variable != "N"]



setkeyv(sales_by_store_103_weeks, c("shop_id","sum_corrected_sales_in_volume"))
sales_by_store_103_weeks[, value := sequence(.N), by = shop_id]

setkeyv(sales_by_store_103_weeks, c("shop_id","value"))
setkeyv(count_itens, c("shop_id","value"))

middle_5 <- sales_by_store_103_weeks[count_itens]
to_del <- c( "value" ,"variable")
middle_5[,(to_del) := NULL]

bottom_5$item_selected <- "bottom" 
middle_5$item_selected <- "middle" 
top_5$item_selected <- "top" 

itens_shops <- rbind(bottom_5,middle_5,top_5)


##################################################################################
# Select top 5 and bottom 5 and middle 5 stores (per sales volume) sum all items
##################################################################################


sales_by_store_103_weeks_2 <- sales_by_store_103_weeks[,sum_sales_all_itens := sum(sum_corrected_sales_in_volume), by = shop_id]
cols_to_keep <- c("shop_id","sum_sales_all_itens")
sales_by_store_103_weeks_2 <- sales_by_store_103_weeks_2[,cols_to_keep, with=FALSE]

sales_by_store_103_weeks_2 <- unique(sales_by_store_103_weeks_2)
setkey(sales_by_store_103_weeks_2,sum_sales_all_itens)

bottom_5 <- sales_by_store_103_weeks_2[, head(.SD, 5)]
top_5 <- sales_by_store_103_weeks_2[, tail(.SD, 5)]

dim(sales_by_store_103_weeks_2)
#[1] 353   2

the_middle <- as.integer(trunc(nrow(sales_by_store_103_weeks_2)/2))

middle_5 <- rbind(sales_by_store_103_weeks_2[the_middle], sales_by_store_103_weeks_2[the_middle+1], sales_by_store_103_weeks_2[the_middle+2], sales_by_store_103_weeks_2[the_middle-1], sales_by_store_103_weeks_2[the_middle-2])

bottom_5$shop_selected <- "bottom"
top_5$shop_selected <- "top"
middle_5$shop_selected <- "middle"

shops <-  rbind(bottom_5,middle_5,top_5)

##################################################################################
# Select just 15 shops from itens_shops and create final list
##################################################################################

setkey(shops, shop_id)
setkey(itens_shops, shop_id)

final_list_shops_itens <- itens_shops[shops]

write.table(final_list_shops_itens, "/home/michela/final_list_shops_itens.psv", sep = "|", row.names = FALSE, col.names = TRUE)


###########################################################
# Add sample from dex_cau to the final list
###########################################################

cols_to_keep <- c("shop_id" ,"item_id", "item_selected" ,"shop_selected"  )
final_list_shops_itens_small <- final_list_shops_itens[,cols_to_keep, with = FALSE]

sample_of_stores_itens$item_selected <- "random_causal"
sample_of_stores_itens$shop_selected <- "random_causal"

sample_of_stores_itens_small <- sample_of_stores_itens[,cols_to_keep, with = FALSE]

final_list_shops_itens_small[,shop_id:= as.integer64(shop_id)]
final_list_shops_itens_small[,item_id:= as.integer(item_id)]

final_list_shops_itens_small <- rbind(final_list_shops_itens_small,sample_of_stores_itens_small)
dim(final_list_shops_itens_small)
#[1] 554   4
setkey(final_list_shops_itens_small)
final_list_shops_itens_small <- unique(final_list_shops_itens_small)
dim(final_list_shops_itens_small)
#[1] 553   4

write.table(final_list_shops_itens_small, "/home/michela/final_list_shops_itens_small.psv", sep = "|", row.names = FALSE, col.names = TRUE)


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
#[1] 553   2

write.table(final_list_shops_itens_small_short, "/home/michela/final_list_shops_itens_small_short.psv", sep = "|", row.names = FALSE, col.names = TRUE)
write.table(final_list_shops_itens_small_long, "/home/michela/final_list_shops_itens_small_long.psv", sep = "|", row.names = FALSE, col.names = TRUE)


######################################################
# Read list of shops and items
######################################################


final_list_shops_itens_small <- fread("/home/michela/final_list_shops_itens_small.psv", sep = "|")
final_list_shops_itens_small_short <- fread("/home/michela/final_list_shops_itens_small_short.psv", sep = "|")
final_list_shops_itens_small_long <- fread("/home/michela/final_list_shops_itens_small_long.psv", sep = "|")


dim(final_list_shops_itens_small)
#553   4

dim(final_list_shops_itens_small_short)
#[1] 216   2

dim(final_list_shops_itens_small_long)
#[1] 553   2





###########################################################
# Select causal info from list of stores only
###########################################################

setkeyv(final_list_shops_itens_small_long, c("shop_id","item_id"))
setkeyv(dex_cau, c("shop_id","item_id"))

dex_cau_selected_stores_items <- dex_cau[final_list_shops_itens_small_long]
dex_cau_selected_stores_items$total <- NULL

dim(dex_cau_selected_stores_items)
#[1] 7880    6

dex_cau_selected_stores_items <- dex_cau_selected_stores_items[!is.na(causal_name)]

dim(dex_cau_selected_stores_items)
#[1] 7747    6

############################################################################################
# Colapses causal names in case there is more than one for the same shopping, item, week
# The output is nz_coke_dex_cau_simple
############################################################################################

cols_to_keep <- c("week_id","shop_id","item_id","new_causal_name")
nz_coke_dex_cau_simple <- dex_cau_selected_stores_items[,cols_to_keep, with=F] 

setkey(nz_coke_dex_cau_simple)
nz_coke_dex_cau_simple <- unique(nz_coke_dex_cau_simple)
dim(nz_coke_dex_cau_simple)
#[1] 7723    4

nz_coke_dex_cau_simple <- nz_coke_dex_cau_simple[,new_causal_name := paste(new_causal_name,collapse=","),by=c("week_id","shop_id","item_id")]

dim(nz_coke_dex_cau_simple)
#[1] 6479    4

nz_coke_dex_cau_simple <- nz_coke_dex_cau_simple[!is.na(week_id)]
nz_coke_dex_cau_simple <- nz_coke_dex_cau_simple[!is.na(new_causal_name)]
dim(nz_coke_dex_cau_simple)
#[1] 6479    4



############################################################################################
# Select data from dex_dwh
############################################################################################

nz_coke_dex_dwh <- data.table()
for(i in 1:nrow(final_list_shops_itens_small_short)) {
the_query <- paste0("select * from nz_coke.dex_dwh where shop_id = ",final_list_shops_itens_small_short$shop_id[i], " AND item_id in (",final_list_shops_itens_small_short$list_of_itens[i]," )"  )
print(paste0("Start ",i," out of ", nrow(final_list_shops_itens_small_short)))
temp_1 <- dbGetQuery(con, the_query)
print(paste0("End ",i," out of ", nrow(final_list_shops_itens_small_short)))
nz_coke_dex_dwh <- rbind(nz_coke_dex_dwh, temp_1)}

dim(nz_coke_dex_dwh)
#[1] 47559    12
setkey(nz_coke_dex_dwh)

nz_coke_dex_dwh <- unique(nz_coke_dex_dwh)

dim(nz_coke_dex_dwh)
#[1] 47559    12


############################################################################################
# Find product description
############################################################################################ 

product_description <- data.table()
for(i in 1:nrow(final_list_shops_itens_small_short)) {
the_query <- paste0("select * from nz_coke.dex_dim where item_id in ( ",final_list_shops_itens_small_short$list_of_itens[i]," )"  )
temp_1 <- dbGetQuery(con, the_query)
product_description <- rbind(product_description, temp_1)}

dim(product_description)
# 553   7
cols_to_keep <- c("item_id", "product_group_description")
product_description <- product_description[,cols_to_keep, with=F ]
setkey(product_description, item_id)
product_description <- unique(product_description)
dim(product_description)
# 371   2


##########################################
# Join data
##########################################

setkey(nz_coke_dex_dwh, item_id)
setkey(product_description, item_id)

final_data_coke <- product_description[nz_coke_dex_dwh, allow.cartesian=TRUE]
dim(final_data_coke)
#[1] 47559    13

setkeyv(final_data_coke,c("item_id", "week_id","shop_id"))
setkeyv(nz_coke_dex_cau_simple,c("item_id", "week_id","shop_id"))
final_data_coke <- nz_coke_dex_cau_simple[final_data_coke]
dim(final_data_coke)
#[1] 47559    14


setkeyv(final_data_coke,c("item_id", "shop_id"))
setkeyv(final_list_shops_itens_small,c("item_id", "shop_id"))

final_data_coke <- final_list_shops_itens_small[final_data_coke]
final_data_coke[is.na(new_causal_name), new_causal_name:= "No display"]

##########################################
# Add indicators of causal
##########################################

final_data_coke$Major_Ind <- as.integer(grepl(pattern = "Major Display", x = final_data_coke$new_causal_name))
final_data_coke$Minor_Ind <- as.integer(grepl(pattern = "Minor Display", x = final_data_coke$new_causal_name))
final_data_coke$Catalogue_feature_Ind <- as.integer(grepl(pattern = "Catalogue or Feature", x = final_data_coke$new_causal_name))
final_data_coke$No_display_Ind <- as.integer(grepl(pattern = "No display", x = final_data_coke$new_causal_name))


##########################################
# Save data
##########################################
write.table(final_data_coke, "/home/michela/final_data_coke.psv", sep = "|", row.names = FALSE, col.names = TRUE)


##########################################
# Read all details from selected stores
##########################################

query_2 <- paste0("select * from nz_coke.dex_dmf WHERE characteristic_short_description = 'AAC_Retailer' AND shop_id in (",super_shops_ids,")" )

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
setkeyv(final_data_coke, c("week_id","shop_id"))

final_data_coke <- dex_dmf[final_data_coke]

##########################################
# Save data
##########################################
write.table(final_data_coke, "/home/michela/final_data_coke.psv", sep = "|", row.names = FALSE, col.names = TRUE)

