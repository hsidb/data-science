Valley.Business.List_clean <- Valley.Business.List[Valley.Business.List$Found == '1',]

Valley.Business.List_clean_reduced <- Valley.Business.List_clean[(c(13:15))]

names_wstp_df_reduced <- names_wstp_df[c(2,4)]




match_termwonrnameclean_termownrname <- merge(names_wstp_df_reduced,Valley.Business.List_clean_reduced,
                by.x = c("clean_names_wstp"),by.y = c("Westpac.clean.name"), all.y = TRUE)
                                                     )

termownrname_list.df <- match_termwonrnameclean_termownrname[!duplicated(match_termwonrnameclean_termownrname$name),]


write.csv(match_termwonrnameclean_termownrname, file = "fortitude_merchants_michela.csv")
