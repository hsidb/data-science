# data-science
work related data science projects

Nielsen Causal Estimation model manual:

•	Two example sql scripts calculating market volume per schema (e.g. coke products) and market share per item (COKE 1.5l) in sql folder:
1.	market_share_ambient.sql 
2.	market_volume_ambient.sql

•	Getting the data into R: three files in R folder
1.	create_final_data_ambient_70.R 
2.	create_final_data_coke_70.R 
3.	create_final_data_pet_70.R

This scripts run in R on the server. The best commented is the coke script.

•	Transforming the data and feature engineering: three files R folder
1.	Model_preparation_ambient.R
2.	Model_preparation_pet.R
3.	Model_preparation_coke.R

•	Run the model
1.	Run_model.R

Important data sets are on the Nielsen server:
ssh nielsen: hagen@master-nielsen-1     /home/Rtpdata/merged_v45testandtraining.csv.zip
•	Latest model results have been achieved with this data set
       


