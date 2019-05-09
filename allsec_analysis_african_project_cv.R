water_food_av_pca<-water_food_df %>%
  dplyr::select("Long-term annual precip depth",
                "Total renewable water sources",
                "Interannual variability",
                "Seasonal variability",
                "Flood occurrence",
                "Drought frequency")
water_avail_all_sector<-water_food_av_pca[yes_list_food_energy,]
pc_water_avail_all_sec<-prcomp(water_avail_all_sector, scale= TRUE, center = TRUE)
summary(pc_water_avail_all_sec)#two pc accounts for 77% of the variance
pc_water_avail_all_sec
pc_water_avail_all_sec_scores<-pc_water_avail_all_sec$x
colnames(pc_water_avail_all_sec_scores)<-c("PC1_water","PC2_water","PC3_water","PC4_water","PC5_water","PC6_water")#two pc captured 70% of the total variance in water availability
pc_water_avail_all_sec_scores<-pc_water_avail_all_sec_scores[,1:2]

###################
###ener resources to food availability 
food_avail_all_sec_df<-ener_food_df %>%
  dplyr::select("food_availability_index",
                #energy res
                "Total_fossil_fuel_production", "Total_non_fossil_fuel_production", 
                "import_export_difference", 
                "Fossil_fuel_reserves_Total", 
                "Fossil_fuel_reserves_Natural_Gas_share", 
                "Fossil_fuel_reserves_Oil_share", 
                #general human capacity
                "GDP_per_capita", "Education_Index", 
                "PC1_GI", "PC2_GI", "Percentage_of_rural_population",
                #food res specific human capacity
                "Value_of_food_imports_over_total_merchandise_exports", 
                "Cereal_import_dependency_ratio", 
                "Rail_lines_density", 
                "Percent_of_arable_land_equipped_for_irrigation",
                #food res
                "PC1_land_res",
                "PC2_land_res"
  ) %>%cbind.data.frame(pc_water_avail_all_sec_scores)


all_sec_res_indpt_var <- colnames(food_avail_all_sec_df)
num_all_sec_ener_res_indpt_var<- ncol(food_avail_all_sec_df)-1

#CVSRA for all sectors Food utilization 
result_food_avail_ener_sec_df<-cv_step_reg(data_input =  food_avail_all_sec_df, 
                                           num_indpt_var = num_all_sec_ener_res_indpt_var, 
                                           names_var = all_sec_res_indpt_var)

model_lm_food_avail_all_sec<- lm(food_availability_index~Rail_lines_density
                                  +Percent_of_arable_land_equipped_for_irrigation
                                  , data = food_avail_all_sec_df)
summary(model_lm_food_avail_all_sec)

##Check AIC 
dfaic = food_avail_all_sec_df
dpt_var = "food_availability_index"
model.min = lm(paste(dpt_var,"~",1), data = dfaic)
model.full = lm(paste(dpt_var,"~","."), data = dfaic)
test1 = stepAIC(model.min, direction = "forward", 
                scope=list(upper=model.full,lower=model.min),
                trace = FALSE)
test1$anova


######################################################################################
nutri_bal_all_sec_df<-ener_food_df %>%
  dplyr::select("nutri_bal_index",
                #energy res
                "Total_fossil_fuel_production", "Total_non_fossil_fuel_production", 
                "import_export_difference", 
                "Fossil_fuel_reserves_Total", 
                "Fossil_fuel_reserves_Natural_Gas_share", 
                "Fossil_fuel_reserves_Oil_share", 
                #general human capacity
                "GDP_per_capita", "Education_Index", 
                "PC1_GI", "PC2_GI", "Percentage_of_rural_population",
                #food res specific human capacity
                "Value_of_food_imports_over_total_merchandise_exports", 
                "Cereal_import_dependency_ratio", 
                "Rail_lines_density", 
                "Percent_of_arable_land_equipped_for_irrigation",
                #food res
                "PC1_land_res",
                "PC2_land_res"
  ) %>%cbind.data.frame(pc_water_avail_all_sec_scores)


all_sec_res_indpt_var <- colnames(nutri_bal_all_sec_df)
num_all_sec_ener_res_indpt_var<- ncol(nutri_bal_all_sec_df)-1

##CVSRA all sec Protein Balance Index
result_nutri_bal_ener_sec_df<-cv_step_reg(data_input =  nutri_bal_all_sec_df, 
                                           num_indpt_var = num_all_sec_ener_res_indpt_var, 
                                           names_var = all_sec_res_indpt_var)
model_lm_nutri_bal_all_sec<- lm(nutri_bal_index~GDP_per_capita, data = nutri_bal_all_sec_df)
summary(model_lm_nutri_bal_all_sec)

##Check AIC 
dfaic = nutri_bal_all_sec_df
dpt_var = "nutri_bal_index"
model.min = lm(paste(dpt_var,"~",1), data = dfaic)
model.full = lm(paste(dpt_var,"~","."), data = dfaic)
test1 = stepAIC(model.min, direction = "forward", 
                scope=list(upper=model.full,lower=model.min),
                trace = FALSE)
test1$anova
AIC(model_lm_nutri_bal_all_sec)
######################################################################################
food_prod_var_all_sec_df<-ener_food_df %>%
  dplyr::select("Per_capita_food_production_variability",
                #energy res
                "Total_fossil_fuel_production", "Total_non_fossil_fuel_production", 
                "import_export_difference", 
                "Fossil_fuel_reserves_Total", 
                "Fossil_fuel_reserves_Natural_Gas_share", 
                "Fossil_fuel_reserves_Oil_share", 
                #general human capacity
                "GDP_per_capita", "Education_Index", 
                "PC1_GI", "PC2_GI", "Percentage_of_rural_population",
                #food res specific human capacity
                "Value_of_food_imports_over_total_merchandise_exports", 
                "Cereal_import_dependency_ratio", 
                "Rail_lines_density", 
                "Percent_of_arable_land_equipped_for_irrigation",
                #food res
                "PC1_land_res",
                "PC2_land_res"
  ) %>%cbind.data.frame(pc_water_avail_all_sec_scores)

all_sec_res_indpt_var <- colnames(food_prod_var_all_sec_df)
num_all_sec_res_indpt_var<- ncol(food_prod_var_all_sec_df)-1


result_food_prod_var_ener_sec_df<-cv_step_reg(data_input =  food_prod_var_all_sec_df, 
                                              num_indpt_var = num_all_sec_res_indpt_var, 
                                              names_var = all_sec_res_indpt_var)
model_lm_food_prod_var_ener_sec<- lm(Per_capita_food_production_variability~Fossil_fuel_reserves_Oil_share, 
                                     data = food_prod_var_ener_sec_df)
summary(model_lm_food_prod_var_ener_sec)

##Check AIC 
dfaic = food_prod_var_all_sec_df
dpt_var = "Per_capita_food_production_variability"
model.min = lm(paste(dpt_var,"~",1), data = dfaic)
model.full = lm(paste(dpt_var,"~","."), data = dfaic)
test1 = stepAIC(model.min, direction = "forward", 
                scope=list(upper=model.full,lower=model.min),
                trace = FALSE)
test1$anova
######################################################################################
food_spl_var_all_sec_df<-ener_food_df %>%
  dplyr::select("Per_capita_food_supply_variability",
                #energy res
                "Total_fossil_fuel_production", "Total_non_fossil_fuel_production", 
                "import_export_difference", 
                "Fossil_fuel_reserves_Total", 
                "Fossil_fuel_reserves_Natural_Gas_share", 
                "Fossil_fuel_reserves_Oil_share", 
                #general human capacity
                "GDP_per_capita", "Education_Index", 
                "PC1_GI", "PC2_GI", "Percentage_of_rural_population",
                #food res specific human capacity
                "Value_of_food_imports_over_total_merchandise_exports", 
                "Cereal_import_dependency_ratio", 
                "Rail_lines_density", 
                "Percent_of_arable_land_equipped_for_irrigation",
                #food res
                "PC1_land_res",
                "PC2_land_res"
  ) %>%cbind.data.frame(pc_water_avail_all_sec_scores)

all_sec_res_indpt_var <- colnames(food_spl_var_all_sec_df)
num_all_sec_res_indpt_var<- ncol(food_spl_var_all_sec_df)-1


result_food_spl_var_all_sec_df<-cv_step_reg(data_input =  food_spl_var_all_sec_df, 
                                              num_indpt_var = num_all_sec_res_indpt_var, 
                                              names_var = all_sec_res_indpt_var)
model_lm_food_spl_var_all_sec<- lm(Per_capita_food_supply_variability~import_export_difference, 
                                     data = food_spl_var_ener_sec_df)
summary(model_lm_food_spl_var_all_sec)

##Check AIC 
dfaic = food_spl_var_all_sec_df
dpt_var = "Per_capita_food_supply_variability"
model.min = lm(paste(dpt_var,"~",1), data = dfaic)
model.full = lm(paste(dpt_var,"~","."), data = dfaic)
test1 = stepAIC(model.min, direction = "forward", 
                scope=list(upper=model.full,lower=model.min),
                trace = FALSE)
test1$anova

######################################################################################
ac_dw<-ac_dw_df[yes_list_food_energy,c("Total population with access to safe drinking-water")]
ac_dw_all_sec_df<-cbind.data.frame(ac_dw,food_spl_var_all_sec_df[,-1])

all_sec_res_indpt_var <- colnames(ac_dw_all_sec_df)
num_all_sec_res_indpt_var<- ncol(ac_dw_all_sec_df)-1

result_ac_dw_all_sec_df<-cv_step_reg(data_input =  ac_dw_all_sec_df, 
                                            num_indpt_var = num_all_sec_res_indpt_var, 
                                            names_var = all_sec_res_indpt_var)

model_lm_ac_dw_all_sec<- lm(ac_dw~GDP_per_capita, 
                                     data = ac_dw_all_sec_df)
summary(model_lm_ac_dw_all_sec)

##Check AIC 
dfaic = ac_dw_all_sec_df
dpt_var = "ac_dw"
model.min = lm(paste(dpt_var,"~",1), data = dfaic)
model.full = lm(paste(dpt_var,"~","."), data = dfaic)
test1 = stepAIC(model.min, direction = "forward", 
                scope=list(upper=model.full,lower=model.min),
                trace = FALSE)
test1$anova

######################################################################################
ac_sani<-ac_sani_df[yes_list_food_energy,1]
ac_sani_all_sec_df<-cbind.data.frame(ac_sani,food_spl_var_all_sec_df[,-1])

all_sec_res_indpt_var <- colnames(ac_sani_all_sec_df)
num_all_sec_res_indpt_var<- ncol(ac_sani_all_sec_df)-1

result_ac_sani_all_sec_df<-cv_step_reg(data_input =  ac_sani_all_sec_df, 
                                     num_indpt_var = num_all_sec_res_indpt_var, 
                                     names_var = all_sec_res_indpt_var)

model_lm_ac_sani_all_sec<- lm(ac_sani~GDP_per_capita+Fossil_fuel_reserves_Oil_share, 
                            data = ac_sani_all_sec_df)
summary(model_lm_ac_sani_all_sec)

##Check AIC 
dfaic = ac_sani_all_sec_df
dpt_var = "ac_sani"
model.min = lm(paste(dpt_var,"~",1), data = dfaic)
model.full = lm(paste(dpt_var,"~","."), data = dfaic)
test1 = stepAIC(model.min, direction = "forward", 
                scope=list(upper=model.full,lower=model.min),
                trace = FALSE)
test1$anova

######################################################################################
ener_svs_index<-ener_svs_food_res_df[,c("ener_svs_index")]

ener_svs_all_sec_df<-cbind.data.frame(ener_svs_index,food_spl_var_all_sec_df[,-1])

all_sec_res_indpt_var <- colnames(ener_svs_all_sec_df)
num_all_sec_res_indpt_var<- ncol(ener_svs_all_sec_df)-1

result_ener_svs_all_sec_df<-cv_step_reg(data_input =  ener_svs_all_sec_df, 
                                       num_indpt_var = num_all_sec_res_indpt_var, 
                                       names_var = all_sec_res_indpt_var)

model_lm_ener_svs_all_sec<- lm(ener_svs_index~Education_Index+Rail_lines_density, 
                              data = ener_svs_all_sec_df)
summary(model_lm_ener_svs_all_sec)

##Check AIC 
dfaic = ener_svs_all_sec_df
dpt_var = "ener_svs_index"
model.min = lm(paste(dpt_var,"~",1), data = dfaic)
model.full = lm(paste(dpt_var,"~","."), data = dfaic)
test1 = stepAIC(model.min, direction = "forward", 
                scope=list(upper=model.full,lower=model.min),
                trace = FALSE)
test1$anova
AIC(model_lm_ener_svs_all_sec)

######################################################################################
Population_with_access_to_improved_sanitation<-mal_ind_water_svs_df[yes_list_food_energy,c("Population_with_access_to_improved_sanitation")]
Total_population_with_access_to_safe_drinking_water<-mal_ind_water_svs_df[yes_list_food_energy,c("Total_population_with_access_to_safe_drinking_water")]

#food_health all sector
mal_ind_all_svs_df<-cbind.data.frame(mal_ind_ener_svs_df,
                                     Total_population_with_access_to_safe_drinking_water,
                                     Population_with_access_to_improved_sanitation)
all_svs_indpt_var <- colnames(mal_ind_all_svs_df)
num_all_svs_indpt_var<- ncol(mal_ind_all_svs_df)-1

result_mal_ind_all_sec_df<-cv_step_reg(data_input =  mal_ind_all_svs_df, 
                                        num_indpt_var = num_all_svs_indpt_var, 
                                        names_var = all_svs_indpt_var)

model_lm_mal_ind_all_svs<- lm(malnutrition_index~nutri_bal_index+Per_capita_food_supply_variability, 
                               data = mal_ind_all_svs_df)
summary(model_lm_mal_ind_all_svs)

##Check AIC 
dfaic = mal_ind_all_svs_df
dpt_var = "malnutrition_index"
model.min = lm(paste(dpt_var,"~",1), data = dfaic)
model.full = lm(paste(dpt_var,"~","."), data = dfaic)
test1 = stepAIC(model.min, direction = "forward", 
                scope=list(upper=model.full,lower=model.min),
                trace = FALSE)
test1$anova
######################################################################################
Diarrhea_as_a_cause_of_death_for_children_under_5<-mortal_rate_health_df[yes_list_food_energy,c("Diarrhea_as_a_cause_of_death_for_children_under_5")]
diar_all_svs_df<-cbind.data.frame(Diarrhea_as_a_cause_of_death_for_children_under_5,
                                  mal_ind_all_svs_df[,-1])
all_svs_indpt_var <- colnames(diar_all_svs_df)
num_all_svs_indpt_var<- ncol(diar_all_svs_df)-1

result_diar_all_sec_df<-cv_step_reg(data_input =  diar_all_svs_df, 
                                        num_indpt_var = num_all_svs_indpt_var, 
                                        names_var = all_svs_indpt_var)
model_lm_diar_all_svs<- lm(Diarrhea_as_a_cause_of_death_for_children_under_5~PC1_GI, 
                              data = diar_all_svs_df)
summary(model_lm_diar_all_svs)

##Check AIC 
dfaic = diar_all_svs_df
dpt_var = "Diarrhea_as_a_cause_of_death_for_children_under_5"
model.min = lm(paste(dpt_var,"~",1), data = dfaic)
model.full = lm(paste(dpt_var,"~","."), data = dfaic)
test1 = stepAIC(model.min, direction = "forward", 
                scope=list(upper=model.full,lower=model.min),
                trace = FALSE)
test1$anova
######################################################################################
Causes_of_Death_attributable_to_Air_Pollt<-mortal_rate_health_df[yes_list_food_energy,c("Causes_of_Death_attributable_to_Air_Pollt")]
air_pol_all_svs_df<-cbind.data.frame(Causes_of_Death_attributable_to_Air_Pollt,
                                     mal_ind_all_svs_df[,-1])

all_svs_indpt_var <- colnames(air_pol_all_svs_df)
num_all_svs_indpt_var<- ncol(air_pol_all_svs_df)-1

result_air_pol_all_sec_df<-cv_step_reg(data_input =  air_pol_all_svs_df, 
                                    num_indpt_var = num_all_svs_indpt_var, 
                                    names_var = all_svs_indpt_var)
model_lm_air_pol_all_svs<- lm(Causes_of_Death_attributable_to_Air_Pollt~food_availability_index, 
                           data = air_pol_all_svs_df)
summary(model_lm_air_pol_all_svs)

##Check AIC 
dfaic = air_pol_all_svs_df
dpt_var = "Causes_of_Death_attributable_to_Air_Pollt"
model.min = lm(paste(dpt_var,"~",1), data = dfaic)
model.full = lm(paste(dpt_var,"~","."), data = dfaic)
test1 = stepAIC(model.min, direction = "forward", 
                scope=list(upper=model.full,lower=model.min),
                trace = FALSE)
test1$anova
# svs_df <- data %>%
#   dplyr::select(  #water
#                   "Total population with access to safe drinking-water",
#                   "Population with access to improved sanitation",
#                   #energy
#                   "Energy supply per capita",
#                   "Electricity consumption per capita",
#                   #protein index
#                   "Share of dietary energy supply derived from cereals, roots, and tubers",
#                   "Average protein supply",
#                   "Average supply of protein of animal origin",
#                   #Food-availability
#                   "Average dietary energy supply adequacy",
#                   "Prevalence of undernourishment",
#                   "Depth of the food deficit",
#                   "Average value of food production",
#                   
#                   "Per capita food supply variability",
#                   
#                   "Per capita food production variability")