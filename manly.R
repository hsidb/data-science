######################
# manly ######
# Merchant match analyses###
######################

bcc_FV_list <- Manly.Traders.Directory 

wtp_MA_list_0801 <- read.csv("merchants_manly_big.csv")
wtp_MA_list_0801_clean <- wtp_MA_list_0801[wtp_MA_list_0801$term_city %in% "MANLY",]



##### Apend all names from BCC and clean name

# setnames(bcc_FV_list, names(bcc_FV_list), c("X4", "X6","X7"))
# bcc_FV_list$ID <- seq.int(nrow(bcc_FV_list))
# 
# 
# name_1 <- bcc_FV_list[,c("name_1","ID")]
# name_1$from <- "name_1"
# setnames(name_1, 1, "name")
# 
# Business_Name <- bcc_FV_list[,c("Business_Name","ID")]
# Business_Name$from <- "Business_Name"
# setnames(Business_Name, 1, "name")
# 
# 
# Main_Trading_Name <- bcc_FV_list[,c("Main_Trading_Name","ID")]
# Main_Trading_Name$from <- "Main_Trading_Name"
# setnames(Main_Trading_Name, 1, "name")
# 
# Entity_Name <- bcc_FV_list[,c("Entity_Name","ID")]
# Entity_Name$from <- "Entity_Name"
# setnames(Entity_Name, 1, "name")

X4 <- as.data.frame(bcc_FA_list$X.4)
X6 <- as.data.frame(bcc_FA_list$X.6)
X7 <- as.data.frame(bcc_FA_list$X.7)

names(X4)[1] <- "merchant"
names(X6)[1] <- "merchant"
names(X7)[1] <- "merchant"

names_bcc_df <- rbind(X4,X6,X7)

names_bcc_df_copy <- names_bcc_df


names_bcc_df$clean_names_bcc <- toupper(names_bcc_df$merchant)
names_bcc_df$clean_names_bcc <- gsub("[[:punct:]]", "", names_bcc_df$clean_names_bcc)
names_bcc_df$clean_names_bcc <- gsub("PTY", "", names_bcc_df$clean_names_bcc)
names_bcc_df$clean_names_bcc <- gsub("LTD", "", names_bcc_df$clean_names_bcc)
names_bcc_df$clean_names_bcc <- gsub("MANLY", "", names_bcc_df$clean_names_bcc)
names_bcc_df$clean_names_bcc <- gsub(" ", "", names_bcc_df$clean_names_bcc)

names_bcc_df_new <- names_bcc_df[!apply(is.na(names_bcc_df)| names_bcc_df == "",1,all   ),]

##### Apend all names from Westpac and clean name


# wtp_FV_list <- fread("/Users/michelaguimaraes/Dropbox (Zetaris)/Clients/Westpac/Westpac_Institutional/Brisbane City Council/Fortitude Valley/merchants_fortitude_all_westpac.csv")
# setkey(wtp_FV_list)
# 
# wtp_FV_list_v2 <- unique(wtp_FV_list[,c(1,2)])
# 
# wtp_FV_list_v2$ID <- seq.int(nrow(wtp_FV_list_v2))
# 
# "termownrname" "termnamelocn"
# 
# termownrname <- wtp_FV_list_v2[,c("termownrname")]
# termownrname$from <- "termownrname"
# setnames(termownrname, 1, "name")
# setkey(termownrname)
# termownrname <- unique(termownrname)
# 
# 
# termnamelocn <- wtp_FV_list_v2[,c("termnamelocn")]
# termnamelocn$from <- "termnamelocn"
# setnames(termnamelocn, 1, "name")
# setkey(termnamelocn)
# termnamelocn <- unique(termnamelocn)
# 

names_wstp_df <- as.data.frame(wtp_MA_list_0801_clean$termownrname)
#names_wstp_df$name <- iconv(names_wstp_df$name,"WINDOWS-1252","UTF-8")


names_wstp_df$clean_names_wstp <- toupper(names_wstp_df$`wtp_MA_list_0801_clean$termownrname`)
names_wstp_df$clean_names_wstp <- gsub("[[:punct:]]", "", names_wstp_df$clean_names_wstp)
names_wstp_df$clean_names_wstp <- gsub("PTY", "", names_wstp_df$clean_names_wstp)
names_wstp_df$clean_names_wstp <- gsub("LTD", "", names_wstp_df$clean_names_wstp)
names_wstp_df$clean_names_wstp <- gsub("LT", "", names_wstp_df$clean_names_wstp)
names_wstp_df$clean_names_wstp <- gsub("MANLY", "", names_wstp_df$clean_names_wstp)
names_wstp_df$clean_names_wstp <- gsub(" ", "", names_wstp_df$clean_names_wstp)




library(RecordLinkage)
library(dplyr)

wordlist_FT <- expand.grid(names_bcc = unique(names_bcc_df_new$clean_names_bcc), names_wtc = unique(names_wstp_df$clean_names_wstp), stringsAsFactors = FALSE)

final_result_1_FT <- wordlist_FT %>% group_by(names_bcc) %>% mutate(match_score = jarowinkler(names_bcc, names_wtc)) %>% summarise(match = match_score[which.max(match_score)], matched_to = names_wtc[which.max(match_score)])

final_result_2_FT <- wordlist_FT %>% group_by(names_bcc) %>% mutate(match_score = jarowinkler(names_bcc, names_wtc)) 
final_result_2_FT <- final_result_2_FT[order(final_result_2_FT$names_bcc, -(final_result_2_FT$match_score)), ]
final_result_2_FT$count_top <- as.integer(ave(final_result_2_FT$names_bcc, final_result_2_FT$names_bcc, FUN=seq_along))


final_result_2_FT_85perc <- final_result_2_FT[final_result_2_FT$match_score > 0.85,]
final_result_2_FTtop_3 <- final_result_2_FT[final_result_2_FT$count_top <= 3,]
final_result_2_FTtop_3$count_top <- NULL

final_result_top <- rbind(final_result_2_FT_85perc,final_result_2_FTtop_3)
final_result_top <- as.data.table(final_result_top)

setkey(final_result_top)
final_result_top_copy <- final_result_top

final_result_top <- unique(final_result_top)

#final_result_top <- final_result_top[!duplicated(final_result_top$names_bcc)]

setkey(final_result_top, c("names_bcc","match_score" ))
final_result_top <- final_result_top[order(final_result_top$names_bcc, -(final_result_top$match_score)), ]

final_result_top$order_top <- as.integer(ave(final_result_top$names_bcc, final_result_top$names_bcc, FUN=seq_along))

final_result_wide <- data.table(final_result_top)
#final_result_wide$match_score <- NULL

#final_result_wide <- reshape(final_result_wide, idvar = "names_bcc", timevar = "order_top", v.names= "names_wtc", direction = "wide")

names_bcc_df <- as.data.table(names_bcc_df)

#setkey(names_bcc_df, clean_names_bcc)
#setnames(final_result_wide_clean_1, "names_bcc", "names_wtc")
#setkey(final_result_wide_clean_1, clean_names_bcc)
final_result_MA <- merge(final_result_wide_clean_2, names_bcc_df, by.x = "names_bcc", by.y = "clean_names_bcc", all.x = TRUE)
final_result_MA <- unique(final_result_MA)
#final_result_FV <- final_result_wide[names_bcc_df]

write.csv(final_result_MA, "final_result_MA.csv")
