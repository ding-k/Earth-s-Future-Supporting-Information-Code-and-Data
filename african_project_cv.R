library(ggplot2)
library(reshape2)
library(Hmisc)
library(ggrepel)
require(CCA)
library(cluster)
library(fpc)
library(YaleToolkit)
options(warn=-1)
library(GGally)
library(boot)
library(leaps)
library(httpuv)
library(shiny)
yylibrary(tidyr)
library(plotly)
library(bootstrap)
library(MASS)
library(GGally)
#remove not useful columns in raw data table
setwd("~/Desktop/R-programming/AfricanProject")
data<-read.csv("WEF_Data_Compilation_ALL_SSA.csv") 
no_use<- names(data) %in% c("X")
data<-data[!no_use]

#make the parameter column as charactor so we can search the certain name in the column
data$Parameter.<-as.character(data$Parameter.)

##`. create df with the primary response variables: service variables and health outcome
svs_all_var <- data %>%
  # dplyr::rename(Parameter = Parameter.) %>% #change column name to exclude period
  dplyr::filter(Parameter. %in% c("Total population with access to safe drinking-water","Population with access to improved sanitation","Depth of the food deficit","Energy supply per capita")) %>%
  dplyr::select(-China,-USA,-`Sri.Lanka`,-India,-Bangladesh) #emit countries not in Africa
#emit countries not in Africa
row.names(svs_all_var)<-svs_all_var$Parameter.
##. Explore missing datasets 

svs_all_var<-svs_all_var[,-c(1,2,3,4)]
summary(svs_all_var)
sum(is.na(svs_all_var["Total population with access to safe drinking-water",]))
sum(is.na(svs_all_var["Population with access to improved sanitation",]))
sum(is.na(svs_all_var["Depth of the food deficit",]))
sum(is.na(svs_all_var["Energy supply per capita",]))
svs_all_var<-t(svs_all_var)
# name<-for (i in 1:nrow(svs_all_var) ){
#   for(j in 1:ncol(svs_all_var)){ifelse(complete.cases(svs_all_var[i,j])==FALSE,print(paste(row.names(svs_all_var)[i],colnames(svs_all_var)[j], sep = ",")),print(" "))}
# }
# 
# ##. Decide which countries to eliminate from analysis due to missing data
ac_dw_df <- data %>%
  # dplyr::rename(Parameter = Parameter.) %>% #change column name to exclude period
  #ac_dw,ppt_lt,tot_water_renew,var_interannual,var_sea,flood,drought,gdp,edu_ind,voi_ac,pol_sta,gov_ef,reg_qual,ru_law,con_corru,ru_percent
  dplyr::filter(Parameter. %in% c("Total population with access to safe drinking-water",
                                  "Long-term annual precip depth",
                                  "Total renewable water sources",
                                  "Interannual variability",
                                  "Seasonal variability",
                                  "Flood occurrence",
                                  "Drought frequency",
                                  "GDP per capita",
                                  "Education Index","Voice and Accountability",
                                  "Political Stability and Absence of Violence/Terrorism",
                                  "Government Effectiveness",
                                  "Regulatory Quality",
                                  "Rule of Law",
                                  "Control of Corruption",
                                  "Rural Population",
                                  "Population")) %>%
  dplyr::select(-Burundi, -Comoros, -Eritrea, -Congo..Dem..Rep., -Equatorial.Guinea, -Seychelles, -Somalia, -South.Sudan, -Somalia,-China,-USA,-`Sri.Lanka`,-India,-Bangladesh)
row.names(ac_dw_df)<-ac_dw_df$Parameter.
ac_dw_df<-ac_dw_df[,-c(1,2,3,4)]

ac_dw_df<- ac_dw_df %>%
  dplyr::select(-Cabo.Verde,-Mauritius,-Sao.Tome.and.Principe) #emit countries not in Africa

#make dataframe numeric
for (i in 1:ncol(ac_dw_df)){
  ac_dw_df[,i]=as.numeric(as.character(ac_dw_df[,i]))
}
#transpose to make df in row of countries and col of parameters
ac_dw_df<-as.data.frame(t(ac_dw_df))

#convert the total renewable water resources to per capita base unit: m^3/year/cap
ac_dw_df[,which(colnames(ac_dw_df)=="Total renewable water sources")]<-log10(ac_dw_df$`Total renewable water sources`/ac_dw_df$Population*10^6)
#GDP is log 10 transformed
ac_dw_df[,which(colnames(ac_dw_df)=="GDP per capita")]<-log10(ac_dw_df$`GDP per capita`)

ggpairs(ac_dw_df)

#principal component analysis for water availability variability and inclement 
water_av_pca<-ac_dw_df %>%
  dplyr::select("Long-term annual precip depth",
                "Total renewable water sources",
                "Interannual variability",
                "Seasonal variability",
                "Flood occurrence",
                "Drought frequency")

pc_water<-prcomp(water_av_pca, scale= TRUE, center = TRUE)
summary(pc_water)
pc_water
ggpairs(water_av_pca)
biplot(pc_water,ylim=c(-.45,.45),xlim=c(-.45,.45))

#principal component analysis in governance variables
WBGI_pca_ac_dw<-ac_dw_df %>%
  dplyr::select("Voice and Accountability","Political Stability and Absence of Violence/Terrorism","Government Effectiveness","Regulatory Quality","Rule of Law","Control of Corruption")
pc_WBGI<-prcomp(WBGI_pca_ac_dw)
summary(pc_WBGI)
pc_WBGI
biplot(pc_WBGI, xlim = c(-0.4,0.9), ylim =c(-0.4,0.4))
pc_water_GI_scores<-cbind.data.frame(pc_water$x,pc_WBGI$x)
row.names(pc_water_GI_scores)<-row.names(ac_dw_df)
colnames(pc_water_GI_scores)<-c("PC1_water","PC2_water","PC3_water","PC4_water","PC5_water","PC6_water","PC1_GI","PC2_GI","PC3_GI","PC4_GI","PC5_GI","PC6_GI")

pca_ac_dw_df<- ac_dw_df %>%
  dplyr::select("Total population with access to safe drinking-water","GDP per capita","Education Index","Rural Population","Population")%>%
  cbind.data.frame(pc_water_GI_scores)
ru_percent<-pca_ac_dw_df$`Rural Population`/pca_ac_dw_df$Population*100

#prove that the pca is conducted approapriately

pca_ac_dw_df<-pca_ac_dw_df%>%
  dplyr::select("Total population with access to safe drinking-water",
                "GDP per capita",
                "Education Index",
                "PC1_water",
                "PC2_water",
                "PC1_GI",
                "PC2_GI"
  )%>%
  cbind.data.frame(ru_percent)

colnames(pca_ac_dw_df)[ncol(pca_ac_dw_df)]<-c("Percentage of rural population")
ggpairs(pca_ac_dw_df[,-1])

cv.error <- rep(0,(ncol(pca_ac_dw_df)-1))
#change the column names to no space format
para <- colnames(pca_ac_dw_df)
for(i in 1:ncol(pca_ac_dw_df)){
  para[i]<- gsub('([[:punct:]])|\\s+','_',para[i])
}
colnames(pca_ac_dw_df)<-para
cv_step_reg<- function(data_input, names_var, num_indpt_var, ...)
  {
  ########stepwise 1
    cv.error <- rep(0,(ncol(data_input)-1))
    formu<- rep(0,(ncol(data_input)-1))
    for (i in 1:num_indpt_var)
    {
      fml = paste(names_var[1],"~",names_var[i+1],sep="")
      glm.fit=glm(fml,data=data_input)
      cv.error[i]=cv.glm(data_input,glm.fit)$delta[1]
      formu[i]=fml
      
    }
    best_1_num<-(which((cv.error)==min(cv.error))+1)
    best_fml<-formu[best_1_num-1]
    best_var_1<-names_var[best_1_num]
    cat(noquote(c("Stepwise Regession: Single best variable",'\n')),sep = '\n')
    cat(noquote(c("The most important variable is: ", best_var_1,'\n')),sep = '\n')
    cat(noquote(c("The minimum cross validation error: ", min(cv.error),'\n')),sep = '\n')
    
    
  ########stepwise 2
    names_var2<- names_var[-best_1_num]
    cv.error2 <- rep(0,(ncol(data_input)-2))
    formu2<- rep(0,(ncol(data_input)-2))
    for (i in 1:num_indpt_var-1)
    {
      
      fml = paste(names_var2[1],"~", names_var[best_1_num] , "+", names_var2[i+1], sep="")
      glm.fit=glm(fml,data=data_input)
      cv.error2[i]=cv.glm(data_input, glm.fit)$delta[1] 
      formu2[i]=fml
    }
    best_2_num<-which((cv.error2)==min(cv.error2))+1
    best_var_2<-names_var2[best_2_num]
    best_fml2<-formu2[best_2_num-1]
    if (min(cv.error) > min(cv.error2)) 
      {
        cat(noquote(c("Stepwise Regession: Best two variables",'\n')), sep = '\n')
        cat(noquote(c("The second most important variable is: ", best_var_2,'\n')),sep = '\n')
        cat(noquote(c("The minimum cross validation error: ", min(cv.error2),'\n')),sep = '\n')
        cat(noquote(best_fml2),sep = '\n')
        print(summary(glm(best_fml2,data = data_input)))
        
    }
    
    if(min(cv.error) <= min(cv.error2)) 
    {
      
      cat(noquote(c(best_fml ,'\n')),sep = '\n')
      print(summary(glm(best_fml,data = data_input)))
      return(best_var_1)
      }
    
    
  ########stepwise 3
    names_var3<- names_var2[-best_2_num]
    cv.error3 <- rep(0,(ncol(data_input)-3))
    formu3<- rep(0,(ncol(data_input)-3))
    for (i in 1:(num_indpt_var-2))
    {
      fml = paste(names_var3[1], "~", names_var[best_1_num] , "+", names_var2[best_2_num] , "+", names_var3[i+1], sep="")
      glm.fit=glm(fml,data=data_input)
      cv.error3[i]=cv.glm(data_input,glm.fit)$delta[1] 
      formu3[i]=fml
    }
    best_3_num<-which((cv.error3)==min(cv.error3))+1
    best_var_3<-names_var3[best_3_num]
    best_fml3<-formu3[best_3_num-1]
    if (min(cv.error2) > min(cv.error3)) 
      {
        cat(noquote(c("Stepwise Regession: Best three variables", '\n')), sep = '\n')
        cat(noquote(c("The third most important variable is: ", best_var_3, '\n')), sep = '\n')
        cat(noquote(c("The minimum cross validation error: ", min(cv.error3),'\n')), sep = '\n')
        cat(noquote(best_fml3), sep = '\n')
        print(summary(glm(best_fml3,data = data_input)))
        return(best_fml3)
    }
    if (min(cv.error2) <= min(cv.error3))
    {
      
      cat(noquote(c("The mininum LOOCV error can be achieved with two variables")))
      return(best_fml2)
    }
}
##CVSRA regression results
result<-cv_step_reg(pca_ac_dw_df, para, 7)
model_ac_dw_water_res<-lm(Total_population_with_access_to_safe_drinking_water~PC1_GI+Percentage_of_rural_population,pca_ac_dw_df)
summary(model_ac_dw_water_res)

##checking anwsers use AIC Stepwise regression
model.min = lm(Total_population_with_access_to_safe_drinking_water ~ 1, data = pca_ac_dw_df)
model.full = lm(Total_population_with_access_to_safe_drinking_water ~ ., data = pca_ac_dw_df)

test1 = stepAIC(model.min, direction = "forward", scope=list(upper=model.full,lower=model.min),trace = FALSE)
test1$anova

ac_sani_df <- data %>%
  # dplyr::rename(Parameter = Parameter.) %>% #change column name to exclude period
  #ac_dw,ppt_lt,tot_water_renew,var_interannual,var_sea,flood,drought,gdp,edu_ind,voi_ac,pol_sta,gov_ef,reg_qual,ru_law,con_corru,ru_percent
  dplyr::filter(Parameter. %in% c("Population with access to improved sanitation"
  )) %>%
  dplyr::select(-Burundi, -Comoros, -Eritrea, -Congo..Dem..Rep., -Equatorial.Guinea, -Seychelles, -Somalia, -South.Sudan, -Somalia,-China,-USA,-`Sri.Lanka`,-India,-Bangladesh)
row.names(ac_sani_df)<-ac_sani_df$Parameter.
ac_sani_df<-ac_sani_df[,-c(1,2,3,4)]

ac_sani_df<- ac_sani_df %>%
  dplyr::select(-Cabo.Verde,-Mauritius,-Sao.Tome.and.Principe) #emit countries not in Africa



##. Explore missing datasets
summary(ac_sani_df) 

#make data frame numeric 
for (i in 1:ncol(ac_sani_df)){
  ac_sani_df[,i]=as.numeric(as.character(ac_sani_df[,i]))
}
ac_sani_df<-as.data.frame(t(ac_sani_df))
ac_sani_df <- cbind.data.frame(ac_sani_df,pca_ac_dw_df[,-1])
ggpairs(ac_sani_df[,-1])


model2<-lm(`Population with access to improved sanitation`~.,data = ac_sani_df)
summary(model2)




##make pc of water and GI into dataframe
pca_ac_sani_df<- ac_sani_df 
##calculating percentage of rural population
ru_percent<-pca_ac_sani_df$`Rural Population`/pca_ac_sani_df$Population*100

#change the column names to no space format
ac_sani_indpt_var <- colnames(pca_ac_sani_df)
num_indpt_var_ac_sani<-ncol(pca_ac_sani_df)-1
for(i in 1:ncol(pca_ac_sani_df)){
  ac_sani_indpt_var[i]<- gsub('([[:punct:]])|\\s+','_',ac_sani_indpt_var[i])
}
colnames(pca_ac_sani_df)<-ac_sani_indpt_var

## CVSRA regression results
result_ac_sani<-cv_step_reg(pca_ac_sani_df, ac_sani_indpt_var, num_indpt_var_ac_sani)

#deduct PC2 GI and PC1 water sequentially, cut off by large p value. 
result_lm_ac_sani<-lm(Population_with_access_to_improved_sanitation~GDP_per_capita, data = pca_ac_sani_df)
summary(result_lm_ac_sani)

##checking anwsers use AIC Stepwise regression
dfaic = pca_ac_sani_df
dpt_var = "Population_with_access_to_improved_sanitation"
model.min = lm(paste(dpt_var,"~",1), data = dfaic)
model.full = lm(paste(dpt_var,"~","."), data = dfaic)

test1 = stepAIC(model.min, direction = "forward", scope=list(upper=model.full,lower=model.min),trace = FALSE)
test1$anova

result1 = lm(Population_with_access_to_improved_sanitation ~ GDP_per_capita + PC1_GI, data = dfaic)


diarrhea_access <- data %>%
  # dplyr::rename(Parameter = Parameter.) %>% #change column name to exclude period
  #ac_dw,ppt_lt,tot_water_renew,var_interannual,var_sea,flood,drought,gdp,edu_ind,voi_ac,pol_sta,gov_ef,reg_qual,ru_law,con_corru,ru_percent
  dplyr::filter(Parameter. %in% c("Diarrhea as a cause of death for children under 5","Total population with access to safe drinking-water",
                                  "Population with access to improved sanitation",
                                  "GDP per capita",
                                  "Education Index",
                                  "Voice and Accountability",
                                  "Political Stability and Absence of Violence/Terrorism"
                                  ,"Government Effectiveness",
                                  "Regulatory Quality",
                                  "Rule of Law",
                                  "Control of Corruption",
                                  "Rural Population",
                                  "Population")) %>%
  dplyr::select(-Burundi, -Comoros, -Eritrea, -Congo..Dem..Rep., -Equatorial.Guinea, -Seychelles, -Somalia, -South.Sudan, -Somalia,-China,-USA,-`Sri.Lanka`,-India,-Bangladesh)
row.names(diarrhea_access)<-diarrhea_access$Parameter.
diarrhea_access<-diarrhea_access[,-c(1,2,3,4)]


diarrhea_access<- diarrhea_access %>%
  dplyr::select(-Cabo.Verde,-Mauritius,-Sao.Tome.and.Principe) #emit countries not in Africa Continent

##. Create your df with predictors for access to sani alone


##. Explore missing datasets
summary(diarrhea_access) 

#make data frame numeric 
for (i in 1:ncol(diarrhea_access)){
  diarrhea_access[,i]=as.numeric(as.character(diarrhea_access[,i]))
}
diarrhea_access<-as.data.frame(t(diarrhea_access))

diarrhea_access[,which(colnames(diarrhea_access)=="GDP per capita")]<-log10(diarrhea_access$`GDP per capita`)


ru_percent<-diarrhea_access$`Rural Population`/diarrhea_access$Population*100
###combine pc into the dataframe
pc_GI_scores<-pc_water_GI_scores %>%
  dplyr::select("PC1_GI","PC2_GI","PC3_GI","PC4_GI","PC5_GI","PC6_GI")
diarrhea_access_pc_GI<- diarrhea_access %>%
  dplyr::select("Diarrhea as a cause of death for children under 5",
                "Total population with access to safe drinking-water","Population with access to improved sanitation",
                "GDP per capita",
                "Education Index") %>%
  cbind.data.frame(pc_GI_scores[,1:2]) %>%
  cbind.data.frame(ru_percent)
colnames(diarrhea_access_pc_GI)[which(colnames(diarrhea_access_pc_GI)=="ru_percent")]<-c("Percentage of rural population")
ggpairs(diarrhea_access_pc_GI[,-1])

water_service<-c(
  "Total population with access to safe drinking-water",
  "Population with access to improved sanitation",
  "GDP per capita",
  "Percentage of rural population",
  "Education Index"
)



water_svs_ac<-c("Total population with access to safe drinking-water",
                "Population with access to improved sanitation")

water_svs_dev<-c(  "GDP per capita",
                   "ru_percent",
                   "Education Index")
ggpairs(diarrhea_access_pc_GI[,water_service])


water_health_indpt_var <- colnames(diarrhea_access_pc_GI)
num_water_health_indpt_var<- ncol(diarrhea_access_pc_GI)-1
for(i in 1:ncol(pca_ac_dw_df)){
  water_health_indpt_var[i]<- gsub('([[:punct:]])|\\s+','_',water_health_indpt_var[i])
}
colnames(diarrhea_access_pc_GI)<-water_health_indpt_var

## CVSRA results for water services to health
result_diar_water_svs<- cv_step_reg(diarrhea_access_pc_GI, water_health_indpt_var, num_water_health_indpt_var)
model_lm_diar_water_svs<- lm(result_diar_water_svs, data = diarrhea_access_pc_GI)
summary(model_lm_diar_water_svs)

##Check AIC 
dfaic = diarrhea_access_pc_GI
dpt_var = "Diarrhea_as_a_cause_of_death_for_children_under_5"
model.min = lm(paste(dpt_var,"~",1), data = dfaic)
model.full = lm(paste(dpt_var,"~","."), data = dfaic)

test1 = stepAIC(model.min, direction = "forward", scope=list(upper=model.full,lower=model.min),trace = FALSE)
test1$anova



####energy
ener_pp_df <- data %>%
  dplyr::filter(Parameter. %in% c("Energy supply per capita","Electricity consumption per capita","Electricity consumption","Total Primary Energy Supply (TPES)",
                                  "Causes of Death attributable to Air Pollt",
                                  "DALY attributable to Air Pollt",
                                  "Coal production","Crude oil production","Oil products production","Natural gas production","Nuclear production","Hydro production","Geothermo solar etc production","Biofuel waste production",
                                  "Net imports","energy export","Net imports ratio","energy export ratio", "import export difference",
                                  "Fossil fuel reserves Total","Fossil fuel reserves Natural Gas share","Fossil fuel reserves Oil share",
                                  "GDP per capita","Education Index","Voice and Accountability","Political Stability and Absence of Violence/Terrorism","Government Effectiveness","Regulatory Quality","Rule of Law","Control of Corruption","Rural Population","Population")) %>%
  dplyr::select(-Burundi, -Comoros, -Eritrea, -Congo..Dem..Rep., -Equatorial.Guinea, -Seychelles, -Somalia, -South.Sudan, -Somalia,-China,-USA,-`Sri.Lanka`,-India,-Bangladesh)
row.names(ener_pp_df)<-ener_pp_df$Parameter.
ener_pp_df<-ener_pp_df[,-c(1,2,3,4)]

ener_pp_df<- ener_pp_df %>%
  dplyr::select(-Cabo.Verde,-Mauritius,-Sao.Tome.and.Principe) #emit countries not in Africa Countinent

#make dataframe numeric
for (i in 1:ncol(ener_pp_df)){
  ener_pp_df[,i]=as.numeric(as.character(ener_pp_df[,i]))
}
#transpose to make df in row of countries and col of parameters
ener_pp_df<-as.data.frame(t(ener_pp_df))
ener_pp_df<-na.omit(ener_pp_df)



#principal component analysis in governance variables
WBGI_pca_ener_pp<-ener_pp_df %>%
  dplyr::select("Voice and Accountability","Political Stability and Absence of Violence/Terrorism","Government Effectiveness","Regulatory Quality","Rule of Law","Control of Corruption")
pc_WBGI_ener<-prcomp(WBGI_pca_ener_pp, scale = FALSE, center = TRUE)
summary(pc_WBGI_ener) ##two pc captured 92% of variance in governance
pc_WBGI_ener
pc_ener_GI_scores<-as.data.frame(pc_WBGI_ener$x)
row.names(pc_ener_GI_scores)<-row.names(ener_pp_df)

colnames(pc_ener_GI_scores)<-c("PC1_GI","PC2_GI","PC3_GI","PC4_GI","PC5_GI","PC6_GI")

mtoe_energy_idpt_var<-c(
  #fossil 
  "Coal production",
  "Crude oil production",
  "Oil products production",
  
  #non_fossil
  "Natural gas production",
  "Nuclear production",
  "Hydro production",
  "Geothermo solar etc production",
  "Biofuel waste production",
  #reserve
  "Fossil fuel reserves Total"
)

#Transfer the data from total mtoe to mtoe/cap
ener_pp_df[,c(mtoe_energy_idpt_var)]<-ener_pp_df[,c(mtoe_energy_idpt_var)]/(ener_pp_df$Population/1000)

ggpairs(ener_pp_df[,c(mtoe_energy_idpt_var)])

###select right "utility" proxy which is electricity consumption per capita since the supply per cap was calculated simply by sum up the production by sectors)
pca_ener_pp_df<- ener_pp_df %>%
  dplyr::select("Electricity consumption per capita",
                "Coal production","Crude oil production","Oil products production","Natural gas production",
                "Nuclear production","Hydro production","Geothermo solar etc production","Biofuel waste production",
                "Net imports","energy export","Net imports ratio","energy export ratio", "import export difference",
                "Fossil fuel reserves Total","Fossil fuel reserves Natural Gas share","Fossil fuel reserves Oil share",
                "GDP per capita","Education Index","Rural Population","Population")%>%
  cbind.data.frame(pc_ener_GI_scores)
ru_percent_ener<-pca_ener_pp_df$`Rural Population`/pca_ener_pp_df$Population*100

####check independent variables colinearity of energy raw resources
ener_res_prod<-c("Coal production","Crude oil production","Oil products production","Natural gas production",
                 "Nuclear production","Hydro production","Geothermo solar etc production","Biofuel waste production","Net imports","energy export","Net imports ratio","energy export ratio")
ener_rsv<-pca_ener_pp_df[,c("Fossil fuel reserves Total","Fossil fuel reserves Natural Gas share",
                            "Fossil fuel reserves Oil share")]


# ggpairs(pca_ener_pp_df[,c(ener_res_prod,ener_rsv)])
#total fossil fuel list
tot_fos<-pca_ener_pp_df[,c("Coal production",
                           "Crude oil production",
                           # "Oil products production",
                           # "Net imports",
                           "Natural gas production")]
# ggpairs(tot_fos)
#total non-fossil fuel list
tot_non_fos<-pca_ener_pp_df[, c("Nuclear production",
                                "Hydro production",
                                "Geothermo solar etc production",
                                "Biofuel waste production")]
ggpairs(tot_non_fos)
ggpairs(ener_rsv)
sum_tot_fos<-tot_fos$`Coal production`+tot_fos$`Crude oil production`+tot_fos$`Natural gas production`
# +tot_fos$`Net imports`
sum_tot_fos<-as.data.frame(sum_tot_fos)

sum_tot_non_fos<-tot_non_fos$`Nuclear production`+tot_non_fos$`Hydro production`+
  tot_non_fos$`Geothermo solar etc production`+tot_non_fos$`Biofuel waste production`
sum_tot_non_fos<-as.data.frame(sum_tot_non_fos)
plot(sum_tot_fos$sum_tot_fos,sum_tot_non_fos$sum_tot_non_fos,xlab = "Total Fossil Fuel Domestic Production", ylab = "Total Non-Fossil Fuel Domestic Production")
#percentage of non-fossil fuel production in total primary energy supply
per_non_fos<-sum_tot_fos/ener_pp_df$`Total Primary Energy Supply (TPES)`*100

pca_ener_pp_df<-cbind.data.frame(pca_ener_pp_df,sum_tot_fos,sum_tot_non_fos)
colnames(pca_ener_pp_df)[(ncol(pca_ener_pp_df)-1):ncol(pca_ener_pp_df)]<-c("Total fossil fuel production","Total non-fossil fuel production")

total_ener_res<- c("Fossil fuel reserves Total",
                   "Total fossil fuel production",
                   "Total non-fossil fuel production")

#transform data to log 10 base per cap
pca_ener_pp_df[,c(total_ener_res)]<-log10(pca_ener_pp_df[,c(total_ener_res)]+0.01)
pca_ener_pp_df$`import export difference`<-pca_ener_pp_df$`import export difference`/(pca_ener_pp_df$Population/1000)# average import export by population
ggpairs(pca_ener_pp_df[,c(total_ener_res)])# log10 value for per cap total energy resources

# ener_res_elec_pp<- pca_ener_pp_df%>%
#   dplyr::select("Electricity consumption per capita",
#                 # "Coal production","Crude oil production","Oil products production","Natural gas production",
#                 "Total fossil fuel production",
#                 # "Nuclear production","Hydro production","Geothermo solar etc production","Biofuel waste production",
#                 "Total non-fossil fuel production",
#                 # "Net imports","energy export","Net imports ratio","energy export ratio",
#                 "import export difference",
#                 "Fossil fuel reserves Total","Fossil fuel reserves Natural Gas share","Fossil fuel reserves Oil share",
#                 "GDP per capita","Education Index"
#                 # ,"Population"
#                 ) %>%
#   cbind.data.frame(ru_percent_ener)
# colnames(ener_res_elec_pp)[ncol(ener_res_elec_pp)]<-c("Percentage of rural population")
# 
# model_test<-regsubsets(`Electricity consumption per capita`~., data = ener_res_elec_pp)
# summary(model_test)
# 
# # energy_service<-ener_pp_df[,c("Electricity consumption",
# #                               "Total Primary Energy Supply (TPES)")]
# # ggpairs(energy_service)
# # pc_energy_service<-prcomp(energy_service, scale= TRUE, center = TRUE)
# # summary(pc_energy_service)
# # biplot(pc_energy_service)
# # ener_svs_score<-pc_energy_service_pp$x
# # colnames(ener_svs_score)<-c("PC1_energy_svs", "PC2_energy_svs")
# 

energy_service_pp<-ener_pp_df[,c("Energy supply per capita",
                                 "Electricity consumption per capita")]
ggpairs(energy_service_pp)
pc_energy_service_pp<-prcomp(energy_service_pp, scale= TRUE, center = TRUE)
summary(pc_energy_service_pp)
pc_energy_service_pp
biplot(pc_energy_service_pp)
ener_svs_pp_score<-pc_energy_service_pp$x
colnames(ener_svs_pp_score)<-c("PC1_energy_svs_pp", "PC2_energy_svs_pp")
ener_res_serv_pp<-pca_ener_pp_df%>%
  dplyr::select("Total fossil fuel production",
                # "Nuclear production","Hydro production","Geothermo solar etc production","Biofuel waste production",
                "Total non-fossil fuel production",
                # "Net imports","energy export","Net imports ratio","energy export ratio",
                "import export difference",
                "Fossil fuel reserves Total","Fossil fuel reserves Natural Gas share","Fossil fuel reserves Oil share",
                "GDP per capita","Education Index"
                # ,"Population"
  ) %>% 
  cbind.data.frame(pc_ener_GI_scores[,1:2])%>%
  cbind.data.frame(ru_percent_ener)

ener_res_serv_pp<-cbind.data.frame(ener_svs_pp_score,ener_res_serv_pp)
colnames(ener_res_serv_pp)[ncol(ener_res_serv_pp)]<-c("Percentage of rural population")
ener_res_serv_pp$`GDP per capita`<-log10(ener_res_serv_pp$`GDP per capita`)
#new data frame for energy svs per cap as depedent variable for energy resources to services
ener_res_serv_pp<-ener_res_serv_pp[,-(which(colnames(ener_res_serv_pp)=="PC2_energy_svs_pp"))]
############
ggpairs(ener_res_serv_pp[,-1])

ener_res_indpt_var <- colnames(ener_res_serv_pp)
num_ener_res_indpt_var<- ncol(ener_res_serv_pp)-1
for(i in 1:ncol(ener_res_serv_pp)){
  ener_res_indpt_var[i]<- gsub('([[:punct:]])|\\s+','_',ener_res_indpt_var[i])
}
colnames(ener_res_serv_pp)<-ener_res_indpt_var

##CVSRA results from energy resources to services
result_energy_svs_ener_res<-cv_step_reg(data_input =  ener_res_serv_pp, num_indpt_var = num_ener_res_indpt_var,names_var = ener_res_indpt_var)
model_lm_energy_svs_ener_res<- lm(PC1_energy_svs_pp~GDP_per_capita, data = ener_res_serv_pp)
summary(model_lm_energy_svs_ener_res)

##Check AIC 
dfaic = ener_res_serv_pp
dpt_var = "PC1_energy_svs_pp"
model.min = lm(paste(dpt_var,"~",1), data = dfaic)
model.full = lm(paste(dpt_var,"~","."), data = dfaic)

test1 = stepAIC(model.min, direction = "forward", scope=list(upper=model.full,lower=model.min),trace = FALSE)
test1$anova

modeltest1 = lm(PC1_energy_svs_pp~Education_Index, data = ener_res_serv_pp)
modeltest1

glm.fit_edu=glm(PC1_energy_svs_pp~Education_Index,data=ener_res_serv_pp)
cv.error_test1_edu=cv.glm(ener_res_serv_pp,glm.fit_edu)$delta[1] 
cv.error_test1_edu
AIC(glm.fit_edu, k = 2)
glm.fit_gdp=glm(PC1_energy_svs_pp~GDP_per_capita,data=ener_res_serv_pp)
cv.error_test1_gdp=cv.glm(ener_res_serv_pp,glm.fit_gdp)$delta[1] 
cv.error_test1_gdp
AIC(glm.fit_gdp, k = 2)


pred_energy_svs_pc<-predict(model_lm_energt_svs_ener_res, newdata = ener_res_serv_pp)




p4<-ggplot(ener_res_serv_pp, aes( x=`PC1_energy_svs_pp`, y=pred_energy_svs_pc))+
  geom_point(size=8, aes(color = `GDP_per_capita`))+
  # geom_point(color= "brown", aes(size = GDP_per_capita))+
  labs(x="Energy Service Index (PC1)",y= "Model Estimation (PC score)")+
  geom_abline(slope = 1,color="red",linetype=2)+
  scale_x_continuous(limits = c(-3, 6))+
  scale_y_continuous(limits = c(-3, 6))+
  theme_bw()+
  theme(axis.title.x = element_text(size=30),
        axis.title.y = element_text(size = 30),
        axis.text.x = element_text(color="#993333", size=14),
        axis.text.y = element_text(color="#993333", size=14))+
  theme(legend.title = element_text(colour="black", size=10, face="bold"),
        legend.text = element_text(size =5))+
  geom_text_repel(aes(`PC1_energy_svs_pp`, pred_energy_svs_pc, label = row.names(ener_res_serv_pp)), show.legend = FALSE)

p4 + scale_color_continuous(name="GDP per capita (log10)", #name of legend
                            breaks = with(ener_res_serv_pp, c(min(GDP_per_capita), mean(GDP_per_capita), max(GDP_per_capita))), #choose breaks of variable
                            labels = c("Low", "Medium", "High"), #label
                            low = "pink",  #color of lowest value
                            high = "red"  #color of highest value
                            
)

p4.1<-ggplot(ener_res_serv_pp, aes( y=`PC1_energy_svs_pp`, x=GDP_per_capita))+
  geom_point(size=8, aes(color = `GDP_per_capita`))+
  # geom_point(color= "brown", aes(size = GDP_per_capita))+
  labs(x="GDP per capita (log10)",y= "Energy Service Index (PC1)")+
  geom_smooth(method='lm',formula=y~x, se=FALSE)+
  # scale_x_continuous(limits = c(0, 6))+
  # scale_y_continuous(limits = c(0, 6))+
  theme_bw()+
  theme(axis.title.x = element_text(size=30),
        axis.title.y = element_text(size = 30),
        axis.text.x = element_text(color="#993333", size=14),
        axis.text.y = element_text(color="#993333", size=14))+
  theme(legend.title = element_text(colour="black", size=10, face="bold"),
        legend.text = element_text(size =5))+
  geom_text_repel(aes(GDP_per_capita,`PC1_energy_svs_pp`, label = row.names(ener_res_serv_pp)), show.legend = FALSE)

p4.1 + scale_color_continuous(name="GDP per capita (log10)", #name of legend
                            breaks = with(ener_res_serv_pp, c(min(GDP_per_capita), mean(GDP_per_capita), max(GDP_per_capita))), #choose breaks of variable
                            labels = c("Low", "Medium", "High"), #label
                            low = "pink",  #color of lowest value
                            high = "red"  #color of highest value
                            
)

pca_ener_health_df<- ener_pp_df %>%
  dplyr::select("Causes of Death attributable to Air Pollt",
                "import export difference",
                "GDP per capita","Education Index","Population") %>%
  cbind.data.frame(ener_res_serv_pp)
pca_ener_health_df<-pca_ener_health_df[,-c(2,3,4,5,7,8,10,11,12)]  


ener_svs_indpt_var <- colnames(pca_ener_health_df)
num_ener_svs_indpt_var<- ncol(pca_ener_health_df)-1
for(i in 1:ncol(pca_ener_health_df)){
  ener_svs_indpt_var[i]<- gsub('([[:punct:]])|\\s+','_',ener_svs_indpt_var[i])
}
colnames(pca_ener_health_df)<-ener_svs_indpt_var

ggpairs(pca_ener_health_df)
result_air_pol_ener_svs<-cv_step_reg(data_input =  pca_ener_health_df, num_indpt_var = num_ener_svs_indpt_var,names_var = ener_svs_indpt_var)
model_lm_air_pol_ener_svs<- lm(Causes_of_Death_attributable_to_Air_Pollt~PC1_GI, data = pca_ener_health_df)
summary(model_lm_air_pol_ener_svs)

pred_energy_svs_pc<-predict(model_lm_energt_svs_ener_res, newdata = ener_res_serv_pp)




ener_prod_air_pol<-ener_pp_df %>%
  dplyr::select("Causes of Death attributable to Air Pollt") %>%
  cbind.data.frame(ener_res_serv_pp)

ener_all_indpt_var <- colnames(ener_prod_air_pol)
num_ener_all_indpt_var<- ncol(ener_prod_air_pol)-1
for(i in 1:ncol(ener_prod_air_pol)){
  ener_all_indpt_var[i]<- gsub('([[:punct:]])|\\s+','_',ener_all_indpt_var[i])
}
colnames(ener_prod_air_pol)<-ener_all_indpt_var 

result_air_pol_ener_res<-cv_step_reg(data_input =  ener_prod_air_pol, num_indpt_var = num_ener_all_indpt_var, names_var = ener_all_indpt_var)

model_lm_air_pol_ener_svs<- lm(Causes_of_Death_attributable_to_Air_Pollt~Fossil_fuel_reserves_Oil_share, data = ener_prod_air_pol)
summary(model_lm_air_pol_ener_svs)


p4.2<-ggplot(ener_prod_air_pol, aes( y=Causes_of_Death_attributable_to_Air_Pollt, x=Fossil_fuel_reserves_Oil_share))+
  geom_point(size=8, aes(color = `Fossil_fuel_reserves_Oil_share`))+
  # geom_point(color= "brown", aes(size = GDP_per_capita))+
  labs(x="Share of Oil in Fossil Fuel Reserve(%)",y= "Causes of Death Attributable to Air Pollution (%)")+
  geom_smooth(method='lm',formula=y~x, se=FALSE)+
  # scale_x_continuous(limits = c(0, 6))+
  # scale_y_continuous(limits = c(0, 6))+
  theme_bw()+
  theme(axis.title.x = element_text(size=30),
        axis.title.y = element_text(size = 30),
        axis.text.x = element_text(color="#993333", size=14),
        axis.text.y = element_text(color="#993333", size=14))+
  theme(legend.title = element_text(colour="black", size=10, face="bold"),
        legend.text = element_text(size =5))+
  geom_text_repel(aes(Fossil_fuel_reserves_Oil_share, Causes_of_Death_attributable_to_Air_Pollt, label = row.names(ener_res_serv_pp)), show.legend = FALSE)

p4.2 + scale_color_continuous(name="Share of Oil in Fossil Fuel Reserve(%)", #name of legend
                              breaks = with(ener_prod_air_pol, c(min(Fossil_fuel_reserves_Oil_share), mean(Fossil_fuel_reserves_Oil_share), max(Fossil_fuel_reserves_Oil_share))), #choose breaks of variable
                              labels = c("Low", "Medium", "High"), #label
                              low = "pink",  #color of lowest value
                              high = "red"  #color of highest value
                              
)
hist(ener_prod_air_pol$Fossil_fuel_reserves_Oil_share,10)




######Food Section

food_res_svs_df<- data %>%
  dplyr::filter(Parameter. %in% c(
    #Land-availability-variability
    "% of total country area cultivated (%)",
    "% of Arable land",
    "% of Permanent crops",
    "% of Permanent meadows and pastures",
    "Land degradation (% territory)",
    
    #Governance
    "Voice and Accountability",
    "Political Stability and Absence of Violence/Terrorism",
    "Government Effectiveness",
    "Regulatory Quality",
    "Rule of Law",
    "Control of Corruption",
    
    #social-economic development
    "GDP per capita",
    "Education Index",
    "Rural Population",
    "Population",
    "Value of food imports over total merchandise exports",
    "Cereal import dependency ratio",
    "Rail lines density",
    "Percent of arable land equipped for irrigation",
    ############### ############### ############### ###############   
    #Nutritional-balance
    "Share of dietary energy supply derived from cereals, roots, and tubers",
    "Average protein supply",
    "Average supply of protein of animal origin",
    
    #Food-ultilization
    "Average dietary energy supply adequacy",
    "Prevalence of undernourishment",
    "Depth of the food deficit",
    "Per capita food supply variability",
    "Average value of food production",
    "Per capita food production variability",
    ############### ############### ############### ###############    
    #Young-age-malnutrition
    "Percentage of children under 5 years of age who are stunted",
    "Percentage of children under 5 years of age affected by wasting",
    "Percentage of children under 5 years of age who are underweight",
    "Percentage of children under 5 years of age who are overweight"
    
    
  ))  %>%
  dplyr::select(-Burundi, -Comoros, -Eritrea, -Congo..Dem..Rep., -Equatorial.Guinea, -Seychelles, -Somalia, -South.Sudan, -Somalia,
                -China,-USA,-`Sri.Lanka`,-India,-Bangladesh)
row.names(food_res_svs_df)<-food_res_svs_df$Parameter.
food_res_svs_df<-food_res_svs_df[,-c(1,2,3,4)]

food_res_svs_df<- food_res_svs_df %>%
  dplyr::select(-Cabo.Verde,-Mauritius,-Sao.Tome.and.Principe) #emit countries not in Africa

#make dataframe numeric
for (i in 1:ncol(food_res_svs_df)){
  food_res_svs_df[,i]=as.numeric(as.character(food_res_svs_df[,i]))
}
#transpose to make df in row of countries and col of parameters
food_res_svs_df<-as.data.frame(t(food_res_svs_df))

food_res_svs_df<-na.omit(food_res_svs_df)

ru_percent_food<-food_res_svs_df$`Rural Population`/food_res_svs_df$Population*100

#add percentage of rural population
food_res_svs_df<-cbind.data.frame(food_res_svs_df, ru_percent_food)
colnames(food_res_svs_df)[which(colnames(food_res_svs_df)=="ru_percent_food")]<-c("Percentage of rural population")

# ###rail line density missing too much values so just remove for now.
# food_defi_df<-food_defi_df%>%
#   dplyr::select(-`Rail lines density`)
# food_defi_df<-na.omit(food_defi_df)




#principal component analysis in governance variables
WBGI_pca_food_res_svs<-food_res_svs_df %>%
  dplyr::select("Voice and Accountability","Political Stability and Absence of Violence/Terrorism","Government Effectiveness","Regulatory Quality","Rule of Law","Control of Corruption")
pc_WBGI_food<-prcomp(WBGI_pca_food_res_svs, center = TRUE, scale = FALSE)
summary(pc_WBGI_food) ###two pc captured 88% of variance for the food sector
pc_WBGI_food
pc_food_GI_scores<-as.data.frame(pc_WBGI_food$x)
row.names(pc_food_GI_scores)<-row.names(food_res_svs_df)

colnames(pc_food_GI_scores)<-c("PC1_GI","PC2_GI","PC3_GI","PC4_GI","PC5_GI","PC6_GI")

food_pca_health_proxy<-food_res_svs_df %>%
  dplyr::select(
    # "Prevalence of undernourishment",
    "Percentage of children under 5 years of age who are stunted",
    "Percentage of children under 5 years of age affected by wasting",
    "Percentage of children under 5 years of age who are underweight",
    "Percentage of children under 5 years of age who are overweight"
  )
pc_food_heal<-prcomp(food_pca_health_proxy)#these value do not need to be scaled since they are all percentages
summary(pc_food_heal)
pc_food_heal
biplot(pc_food_heal, xlim= c(-0.4,0.8),ylim= c(-0.5, 0.5))
malnutrition_index<-pc_food_heal$x
colnames(malnutrition_index)<-c("PC1_U5nourish","PC2_U5nourish","PC3_U5nourish","PC4_U5nourish")
ggpairs(food_res_svs_df[,22:25])
malnutrition_index<- malnutrition_index[,1]
##we se that The U5 predictors are highly correlated. so we use 1 pc to capture 69 percent of the total variance as U5 proxy



####food subcategory data manipulation

land_avail<-c(#Land-availability-variability
  "% of total country area cultivated (%)",
  # "% of Arable land",
  "% of Permanent crops",
  "% of Permanent meadows and pastures",
  "Land degradation (% territory)")

food_human_capacity<-c(#social-economic development
  "GDP per capita",
  "Education Index",
  "Percentage of rural population",
  "Population",
  "Value of food imports over total merchandise exports",
  "Cereal import dependency ratio",
  "Rail lines density",
  "Percent of arable land equipped for irrigation")

nutri_bal<-c("Share of dietary energy supply derived from cereals, roots, and tubers",
             "Average protein supply",
             "Average supply of protein of animal origin")

food_util<-c(#Food-ultilization
  "Average dietary energy supply adequacy",
  "Prevalence of undernourishment",
  "Depth of the food deficit",
  "Average value of food production")

food_svs_var<-c("Per capita food supply variability",
                "Per capita food production variability")

ggpairs(food_res_svs_df[,land_avail])
#plot land availability pc 1 and 2
pc_land_avail<-prcomp(food_res_svs_df[,land_avail])
summary(pc_land_avail)
pc_land_avail
biplot(pc_land_avail,ylim=c(-.58,.58),xlim=c(-1,1))
pc_land_avail_scores<-pc_land_avail$x[,1:2]
colnames(pc_land_avail_scores)<-c("PC1_land_res", "PC2_land_res")

ggpairs(food_res_svs_df[,nutri_bal])
pc_nutri_bal<-prcomp(food_res_svs_df[,nutri_bal], scale = TRUE, center = TRUE)
summary(pc_nutri_bal)
pc_nutri_bal
biplot(pc_nutri_bal,
       ylim=c(-.58,.58),
       xlim=c(-1,1)
)
pc_nutri_bal_score<-pc_nutri_bal$x
nutri_bal_index<-pc_nutri_bal_score[,1]

ggpairs(food_res_svs_df[,food_util])
pc_food_util<-prcomp(food_res_svs_df[,food_util], scale = TRUE, center = TRUE)
summary(pc_food_util)
pc_food_util
biplot(pc_food_util,
       ylim=c(-.58,.58),
       xlim=c(-1,1)
)
pc_food_util_scores<-pc_food_util$x[,1:2]
colnames(pc_food_util_scores)<-c("PC1_food_availability","PC2_food_availability")

ggpairs(food_res_svs_df[,food_svs_var])
pc_food_svs_var<-prcomp(food_res_svs_df[,food_svs_var], scale = TRUE, center = TRUE)
summary(pc_food_svs_var)
pc_food_svs_var # first pc only captures 60 % of the variance, rather use the raw parameter. 


ggpairs(food_res_svs_df[,food_human_capacity])

land_res_idp_var<-cbind.data.frame(pc_land_avail_scores,
                                   pc_food_GI_scores[,1:2],
                                   food_res_svs_df[,food_human_capacity])

#first food svs: nutritional balance
land_svs_nutri_bal_df<-cbind.data.frame(nutri_bal_index,land_res_idp_var)
ggpairs(land_svs_nutri_bal_df)



###data transformation
land_svs_nutri_bal_df$`GDP per capita`<-log10(land_svs_nutri_bal_df$`GDP per capita`)
food_human_capacity_trans<-c("Value of food imports over total merchandise exports",
                             "Rail lines density" ,
                             "Percent of arable land equipped for irrigation")
land_svs_nutri_bal_df[,food_human_capacity_trans]<-log10(land_svs_nutri_bal_df[,food_human_capacity_trans]+0.01)
land_svs_nutri_bal_df<-land_svs_nutri_bal_df[,-which(names(land_svs_nutri_bal_df)=="Population")]

ggpairs(land_svs_nutri_bal_df)

land_svs_indpt_var <- colnames(land_svs_nutri_bal_df)
num_land_svs_indpt_var<- ncol(land_svs_nutri_bal_df)-1
for(i in 1:ncol(land_svs_nutri_bal_df)){
  land_svs_indpt_var[i]<- gsub('([[:punct:]])|\\s+','_',land_svs_indpt_var[i])
}
colnames(land_svs_nutri_bal_df)<-land_svs_indpt_var

#CVSRA results: protein balance
result_land_svs_nutri_bal<-cv_step_reg(data_input =  land_svs_nutri_bal_df, num_indpt_var = num_land_svs_indpt_var,names_var = land_svs_indpt_var)
model_lm_land_svs_nutri_bal<- lm(nutri_bal_index~GDP_per_capita, data = land_svs_nutri_bal_df)
summary(model_lm_land_svs_nutri_bal)

##Check AIC 
dfaic = land_svs_nutri_bal_df
dpt_var = "nutri_bal_index"
model.min = lm(paste(dpt_var,"~",1), data = dfaic)
model.full = lm(paste(dpt_var,"~","."), data = dfaic)

test1 = stepAIC(model.min, direction = "forward", scope=list(upper=model.full,lower=model.min),trace = FALSE)
test1$anova

pred_nutri_bal<-predict(model_lm_land_svs_nutri_bal, newdata = land_svs_nutri_bal_df)


p5<-ggplot(land_svs_nutri_bal_df, aes( x=nutri_bal_index, y=pred_nutri_bal))+
  geom_point(size=8, aes(color = `GDP_per_capita`))+
  # geom_point(color= "brown", aes(size = GDP_per_capita))+
  labs(x="Nutritional Balance Index (PC1)",y= "Model Estimation (PC score)")+
  geom_abline(slope = 1,color="red",linetype=2)+
  scale_x_continuous(limits = c(-3, 3))+
  scale_y_continuous(limits = c(-3, 3))+
  theme_bw()+
  theme(axis.title.x = element_text(size=30),
        axis.title.y = element_text(size = 30),
        axis.text.x = element_text(color="#993333", size=14),
        axis.text.y = element_text(color="#993333", size=14))+
  theme(legend.title = element_text(colour="black", size=10, face="bold"),
        legend.text = element_text(size =5))+
  geom_text_repel(aes(nutri_bal_index, pred_nutri_bal, label = row.names(land_svs_nutri_bal_df)), show.legend = FALSE)

p5 + scale_color_continuous(name="GDP per capita (log10)", #name of legend
                            breaks = with(land_svs_nutri_bal_df, c(min(GDP_per_capita), mean(GDP_per_capita), max(GDP_per_capita))), #choose breaks of variable
                            labels = c("Low", "Medium", "High"), #label
                            low = "pink",  #color of lowest value
                            high = "red"  #color of highest value
                            
)

p5.1<-ggplot(land_svs_nutri_bal_df, aes( y=nutri_bal_index, x=GDP_per_capita))+
  geom_point(size=8, aes(color = `GDP_per_capita`))+
  # geom_point(color= "brown", aes(size = GDP_per_capita))+
  labs(x="GDP per capita (log10)",y= "Protein Index (PC score)")+
  geom_smooth(method='lm',formula=y~x, se=FALSE)+
  # scale_x_continuous(limits = c(0, 6))+
  # scale_y_continuous(limits = c(0, 6))+
  theme_bw()+
  theme(axis.title.x = element_text(size=30),
        axis.title.y = element_text(size = 30),
        axis.text.x = element_text(color="#993333", size=14),
        axis.text.y = element_text(color="#993333", size=14))+
  theme(legend.title = element_text(colour="black", size=10, face="bold"),
        legend.text = element_text(size =5))+
  geom_text_repel(aes(GDP_per_capita,nutri_bal_index, label = row.names(land_svs_nutri_bal_df)), show.legend = FALSE)

p5.1 + scale_color_continuous(name="GDP per capita (log10)", #name of legend
                              breaks = with(land_svs_nutri_bal_df, c(min(GDP_per_capita), mean(GDP_per_capita), max(GDP_per_capita))), #choose breaks of variable
                              labels = c("Low", "Medium", "High"), #label
                              low = "pink",  #color of lowest value
                              high = "red"  #color of highest value
                              
)


###second food svs: Food Utilization
food_availability_index<-pc_food_util_scores[,1]
land_svs_food_avail_df<-cbind.data.frame(food_availability_index, land_svs_nutri_bal_df[,-1])
land_svs_indpt_var<-colnames(land_svs_food_avail_df)

## CVSRA result: Food Ultilization
result_land_svs_food_avail<-cv_step_reg(data_input =land_svs_food_avail_df, num_indpt_var = num_land_svs_indpt_var, names_var = land_svs_indpt_var)
model_lm_land_svs_food_avail<- lm(food_availability_index~PC2_GI+Percentage_of_rural_population, data = land_svs_food_avail_df)
summary(model_lm_land_svs_food_avail)

##Check AIC 
dfaic = land_svs_food_avail_df
dpt_var = "food_availability_index"
model.min = lm(paste(dpt_var,"~",1), data = dfaic)
model.full = lm(paste(dpt_var,"~","."), data = dfaic)

test1 = stepAIC(model.min, direction = "forward", scope=list(upper=model.full,lower=model.min),trace = FALSE)
test1$anova

pred_food_avail_index<-predict(model_lm_land_svs_food_avail, newdata = land_svs_food_avail_df)

p5.2<-ggplot(land_svs_food_avail_df, aes( x=food_availability_index, y=pred_food_avail_index))+
  geom_point(size=8, aes(color = `PC2_GI`))+
  labs(x="Food Availability Index (PC1)",y= "Model Estimation")+
  geom_abline(slope = 1,color="red",linetype=2)+
  scale_x_continuous(limits = c(-3.5, 3.5))+
  scale_y_continuous(limits = c(-3.5, 3.5))+
  theme_bw()+
  theme(axis.title.x = element_text(size=30),
        axis.title.y = element_text(size = 30),
        axis.text.x = element_text(color="#993333", size=14),
        axis.text.y = element_text(color="#993333", size=14))+
  theme(legend.title = element_text(colour="black", size=10, face="bold"),
        legend.text = element_text(size =5))+
  geom_text_repel(aes(food_availability_index, pred_food_avail_index, label = row.names(land_svs_food_avail_df)), show.legend = FALSE)

p5.2 + scale_color_continuous(name="Political Stability Index", #name of legend
                              breaks = with(land_svs_food_avail_df, c(min(PC2_GI), mean(PC2_GI), max(PC2_GI))), #choose breaks of variable
                              labels = c("Low", "Medium", "High"), #label
                              low = "pink",  #color of lowest value
                              high = "red"  #color of highest value
                              
)

#######3D plot
#Graph Resolution (more important for more complex shapes)
graph_reso <- 0.05

#Setup Axis
axis_x <- seq(min(land_svs_food_avail_df$PC2_GI), 
              max(land_svs_food_avail_df$PC2_GI), by = graph_reso)
axis_y <- seq(min(land_svs_food_avail_df$Percentage_of_rural_population), 
              max(land_svs_food_avail_df$Percentage_of_rural_population), by = graph_reso)

#Sample points
model5.2_surface <- expand.grid(PC2_GI = axis_x, Percentage_of_rural_population = axis_y, KEEP.OUT.ATTRS = F)
model5.2_surface$food_availability_index <- predict.lm(model_lm_land_svs_food_avail, newdata = model5.2_surface)
model5.2_surface <- acast(model5.2_surface, Percentage_of_rural_population ~ PC2_GI , value.var = "food_availability_index") #y ~ x

# hcolors=c("red","blue","green")[my_df$Species]
p5.2_3d <- plot_ly(land_svs_food_avail_df, 
                 x = ~PC2_GI, 
                 y = ~Percentage_of_rural_population, 
                 z = ~food_availability_index,
                 text = row.names(land_svs_food_avail_df),
                 type = "scatter3d"
                 ,
                 mode = "markers"
                 # ,
                 # marker = list(color = hcolors)
)%>%
  layout(
    scene = list(
      xaxis = list(title = "PC2_GI"),
      yaxis = list(title = "% RuralPop"),
      zaxis = list(title = "Food Unavailability Index")
    ))

p5.2_3d <- add_trace(p = p5.2_3d,
                   z = model5.2_surface,
                   x = axis_x,
                   y = axis_y,
                   type = "surface",
                   legend = FALSE
)
p5.2_3d

### Third food svs: food variability:
land_svs_food_spl_var_df<-cbind.data.frame(food_res_svs_df$`Per capita food supply variability`, land_svs_nutri_bal_df[,-1])
colnames(land_svs_food_spl_var_df)[1]<-c("Per_capita_food_supply_variability")

land_svs_indpt_var<-colnames(land_svs_food_spl_var_df)

result_land_svs_food_spl_var<-cv_step_reg(data_input =land_svs_food_spl_var_df, num_indpt_var = num_land_svs_indpt_var, names_var = land_svs_indpt_var)
model_lm_land_svs_food_spl_var<- lm(Per_capita_food_supply_variability~Percentage_of_rural_population, data = land_svs_food_spl_var_df)
summary(model_lm_land_svs_food_spl_var)




land_svs_food_prod_var_df<-cbind.data.frame(food_res_svs_df$`Per capita food production variability`, land_svs_nutri_bal_df[,-1])
colnames(land_svs_food_prod_var_df)[1]<-c("Per_capita_food_production_variability")

land_svs_indpt_var<-colnames(land_svs_food_prod_var_df)

## CVSRA results: Food Supply Variability
result_land_svs_food_prod_var<-cv_step_reg(data_input =land_svs_food_prod_var_df, num_indpt_var = num_land_svs_indpt_var, names_var = land_svs_indpt_var)
model_lm_land_svs_food_prod_var<- lm(Per_capita_food_production_variability~PC2_GI, data = land_svs_food_prod_var_df)
summary(model_lm_land_svs_food_prod_var)

##Check AIC 
dfaic = land_svs_food_prod_var_df
dpt_var = "Per_capita_food_production_variability"
model.min = lm(paste(dpt_var,"~",1), data = dfaic)
model.full = lm(paste(dpt_var,"~","."), data = dfaic)

test1 = stepAIC(model.min, direction = "forward", scope=list(upper=model.full,lower=model.min),trace = FALSE)
test1$anova


pred_food_prod_var<-predict(model_lm_land_svs_food_prod_var, newdata = land_svs_food_prod_var_df)


p5.3<-ggplot(land_svs_food_prod_var_df, aes( x=Per_capita_food_production_variability, y=pred_food_prod_var))+
  geom_point(size=8, aes(color = PC2_GI))+
  # geom_point(color= "brown", aes(size = GDP_per_capita))+
  labs(x="Per capita Food Production Variability (I$/person)",y= "Model Estimation (I$/person)")+
  geom_abline(slope = 1,color="red",linetype=2)+
  # scale_x_continuous(limits = c(-3, 3))+
  # scale_y_continuous(limits = c(-3, 3))+
  theme_bw()+
  theme(axis.title.x = element_text(size=30),
        axis.title.y = element_text(size = 30),
        axis.text.x = element_text(color="#993333", size=14),
        axis.text.y = element_text(color="#993333", size=14))+
  theme(legend.title = element_text(colour="black", size=10, face="bold"),
        legend.text = element_text(size =5))+
  geom_text_repel(aes(x=Per_capita_food_production_variability, y=pred_food_prod_var, label = row.names(land_svs_food_prod_var_df)), show.legend = FALSE)

p5.3 + scale_color_continuous(name="Political Stability Index", #name of legend
                            breaks = with(land_svs_food_prod_var_df, c(min(PC2_GI), mean(PC2_GI), max(PC2_GI))), #choose breaks of variable
                            labels = c("Low", "Medium", "High"), #label
                            low = "pink",  #color of lowest value
                            high = "red"  #color of highest value
                            
)

p5.4<-ggplot(land_svs_food_prod_var_df, aes( y=Per_capita_food_production_variability, x= PC2_GI))+
  geom_point(size=8, aes(color =  PC2_GI))+
  # geom_point(color= "brown", aes(size = GDP_per_capita))+
  labs(x="Political Stability Index",y= "Per capita Food Production Variability (I$/person)")+
  geom_smooth(method='lm',formula=y~x, se=TRUE)+
  # scale_x_continuous(limits = c(0, 6))+
  # scale_y_continuous(limits = c(0, 6))+
  theme_bw()+
  theme(axis.title.x = element_text(size=30),
        axis.title.y = element_text(size = 30),
        axis.text.x = element_text(color="#993333", size=14),
        axis.text.y = element_text(color="#993333", size=14))+
  theme(legend.title = element_text(colour="black", size=10, face="bold"),
        legend.text = element_text(size =5))+
  geom_text_repel(aes(y=Per_capita_food_production_variability, x= PC2_GI, label = row.names(land_svs_food_prod_var_df)), show.legend = FALSE)

p5.4 + scale_color_continuous(name="Political Stability Index", #name of legend
                              breaks = with(land_svs_food_prod_var_df, c(min( PC2_GI), mean( PC2_GI), max( PC2_GI))), #choose breaks of variable
                              labels = c("Low", "Medium", "High"), #label
                              low = "pink",  #color of lowest value
                              high = "red"  #color of highest value
                              
)


####ultilization to health
food_svs_health_df<-cbind.data.frame(malnutrition_index,
                                     nutri_bal_index,
                                     food_availability_index,
                                     food_res_svs_df$`Per capita food production variability`,
                                     food_res_svs_df$`Per capita food supply variability`,
                                     land_svs_food_avail_df[,-c(1,2,3)]
)
colnames(food_svs_health_df)[4:5]<- c("Per_capita_food_production_variability","Per_capita_food_supply_variability")


ggpairs(food_svs_health_df)

## CVSRA result for U5 malnutrition
result_malnutrition_index_food_svs<-cv_step_reg(data_input = food_svs_health_df,names_var = colnames(food_svs_health_df), num_indpt_var = ncol(food_svs_health_df)-1)
model_lm_malnutrition_index_food_svs<-lm(malnutrition_index~GDP_per_capita+Rail_lines_density, data = food_svs_health_df)
summary(model_lm_malnutrition_index_food_svs)

##Check AIC 
dfaic = food_svs_health_df
dpt_var = "malnutrition_index"
model.min = lm(paste(dpt_var,"~",1), data = dfaic)
model.full = lm(paste(dpt_var,"~","."), data = dfaic)

test1 = stepAIC(model.min, direction = "forward", scope=list(upper=model.full,lower=model.min),trace = FALSE)
test1$anova

pred_malnutrition_index<-predict(model_lm_malnutrition_index_food_svs, newdata = food_svs_health_df)

p5.5<-ggplot(food_svs_health_df, aes( x=malnutrition_index, y=pred_malnutrition_index))+
  geom_point(size=8, aes(color = GDP_per_capita))+
  labs(x="Young Age Malnutrition Index (PC1)",y= "Model Estimation")+
  geom_abline(slope = 1,color="red",linetype=2)+
  scale_x_continuous(limits = c(-30, 20))+
  scale_y_continuous(limits = c(-30, 20))+
  theme_bw()+
  theme(axis.title.x = element_text(size=30),
        axis.title.y = element_text(size = 30),
        axis.text.x = element_text(color="#993333", size=14),
        axis.text.y = element_text(color="#993333", size=14))+
  theme(legend.title = element_text(colour="black", size=10, face="bold"),
        legend.text = element_text(size =5))+
  geom_text_repel(aes(malnutrition_index, pred_malnutrition_index, label = row.names(food_svs_health_df)), show.legend = FALSE)

p5.5 + scale_color_continuous(name="GDP per capita", #name of legend
                              breaks = with(food_svs_health_df, c(min(GDP_per_capita), mean(GDP_per_capita), max(GDP_per_capita))), #choose breaks of variable
                              labels = c("Low", "Medium", "High"), #label
                              low = "pink",  #color of lowest value
                              high = "red"  #color of highest value
                              
)

#######3D plot
#Graph Resolution (more important for more complex shapes)
graph_reso <- 0.05

#Setup Axis
axis_x <- seq(min(food_svs_health_df$GDP_per_capita), 
              max(food_svs_health_df$GDP_per_capita), by = graph_reso)
axis_y <- seq(min(food_svs_health_df$Rail_lines_density), 
              max(food_svs_health_df$Rail_lines_density), by = graph_reso)

#Sample points
model5.5_surface <- expand.grid(GDP_per_capita = axis_x, Rail_lines_density = axis_y, KEEP.OUT.ATTRS = F)
model5.5_surface$malnutrition_index <- predict.lm(model_lm_malnutrition_index_food_svs, newdata = model5.5_surface)
model5.5_surface <- acast(model5.5_surface, Rail_lines_density ~ GDP_per_capita , value.var = "malnutrition_index") #y ~ x

# hcolors=c("red","blue","green")[my_df$Species]
p5.5_3d <- plot_ly(food_svs_health_df, 
                   x = ~GDP_per_capita, 
                   y = ~Rail_lines_density, 
                   z = ~malnutrition_index,
                   text = row.names(food_svs_health_df),
                   type = "scatter3d"
                   ,
                   mode = "markers"
                   # ,
                   # marker = list(color = hcolors)
)%>%
  layout(
    scene = list(
      xaxis = list(title = "GDP_per_capita"),
      yaxis = list(title = "Rail_lines_density"),
      zaxis = list(title = "Malnutrition_Index")
    ))

p5.5_3d <- add_trace(p = p5.5_3d,
                     z = model5.5_surface,
                     x = axis_x,
                     y = axis_y,
                     type = "surface",
                     legend = FALSE
)
p5.5_3d


####countries jointly in enery and food sector
yes_list<-rep(NA,nrow(food_res_svs_df))
no_list<-rep(NA,nrow(food_res_svs_df))
for (i in 1:nrow(ener_res_serv_pp)){
  ifelse(row.names(ener_res_serv_pp)[i] %in% row.names(food_res_svs_df), yes_list[i]<-row.names(food_res_svs_df)[i], no_list[i]<-row.names(food_res_svs_df)[i])
}
no_list_ener_food<-no_list[complete.cases(no_list)]
yes_list_ener_food<-yes_list[complete.cases(yes_list)]

###countries jointly in water and food sector
yes_list<-rep(NA,nrow(food_res_svs_df))
no_list<-rep(NA,nrow(food_res_svs_df))
for (i in 1:nrow(ac_dw_df)){
  ifelse(row.names(ac_dw_df)[i] %in% row.names(food_res_svs_df), yes_list[i]<-row.names(ac_dw_df)[i], no_list[i]<-row.names(ac_dw_df)[i])
}
no_list_water_food<-no_list[complete.cases(no_list)]
yes_list_water_food<-yes_list[complete.cases(yes_list)]

###countries jointly in water and energy sector
yes_list<-rep(NA,nrow(ener_res_serv_pp))
no_list<-rep(NA,nrow(ener_res_serv_pp))
for (i in 1:nrow(ac_dw_df)){
  ifelse(row.names(ac_dw_df)[i] %in% row.names(ener_res_serv_pp), yes_list[i]<-row.names(ac_dw_df)[i], no_list[i]<-row.names(ac_dw_df)[i])
}
no_list_water_energy<-no_list[complete.cases(no_list)]
yes_list_water_energy<-yes_list[complete.cases(yes_list)]

###countries jointly in food and energy sector
yes_list<-rep(NA,nrow(food_res_svs_df))
no_list<-rep(NA,nrow(food_res_svs_df))
for (i in 1:nrow(ener_res_serv_pp)){
  ifelse(row.names(ener_res_serv_pp)[i] %in% row.names(food_res_svs_df), yes_list[i]<-row.names(ener_res_serv_pp)[i], no_list[i]<-row.names(ener_res_serv_pp)[i])
}
no_list_food_energy<-no_list[complete.cases(no_list)]
yes_list_food_energy<-yes_list[complete.cases(yes_list)]



####Water_energy_link
water_energy_df<-cbind.data.frame(ac_dw_df[yes_list_water_energy,], ener_res_serv_pp[yes_list_water_energy,]) %>%
  cbind.data.frame(ac_sani_df[yes_list_water_energy,]$`Population with access to improved sanitation`)
colnames(water_energy_df)[ncol(water_energy_df)]<-c("Population with access to improved sanitation")
water_energy_df<-water_energy_df[,-(8:17)]


water_ener_av_pca<-water_energy_df %>%
  dplyr::select("Long-term annual precip depth",
                "Total renewable water sources",
                "Interannual variability",
                "Seasonal variability",
                "Flood occurrence",
                "Drought frequency")

pc_water_ener<-prcomp(water_ener_av_pca, scale= TRUE, center = TRUE)
summary(pc_water_ener)##two pc account for 73% of variance
pc_water_ener
pc_water_ener_av_scores<-pc_water_ener$x
pc_water_ener_av_scores
colnames(pc_water_ener_av_scores)<-c("PC1_water","PC2_water","PC3_water","PC4_water","PC5_water","PC6_water")#two pc captured 70% of the total variance in water availability
pc_water_ener_av_scores  <-pc_water_ener_av_scores [,c("PC1_water",
                                                        "PC2_water")]

ru_percent<-water_energy_df$`Rural Population`/water_energy_df$Population*100


########################################################################################
###control check for direct link with subset countries
###mute water sector variables
ener_svs_water_sec_df<-water_energy_df %>%
  dplyr::select("PC1_energy_svs_pp",
                "GDP_per_capita", 
                "Education_Index", "PC1_GI", "PC2_GI", 
                "Percentage_of_rural_population",
                ###energy res
                "Total_fossil_fuel_production", 
                "Total_non_fossil_fuel_production", 
                "import_export_difference", 
                "Fossil_fuel_reserves_Total", 
                "Fossil_fuel_reserves_Natural_Gas_share", 
                "Fossil_fuel_reserves_Oil_share"   
  )

# ggpairs(ener_svs_water_sec_df)
# ggpairs(ener_svs_water_sec_df)

W_to_E_water_res_indpt_var <- colnames(ener_svs_water_sec_df)
num_W_to_E_water_res_indpt_var<- ncol(ener_svs_water_sec_df)-1


result_ener_svs_water_sec<-cv_step_reg(data_input =  ener_svs_water_sec_df, 
                                       num_indpt_var = num_W_to_E_water_res_indpt_var, 
                                       names_var = W_to_E_water_res_indpt_var)
model_lm_ener_svs_water_sec<- lm(PC1_energy_svs_pp~GDP_per_capita, data = ener_svs_water_sec_df)
summary(model_lm_ener_svs_water_sec)
########################################################################################





ener_svs_water_sec_df<-water_energy_df %>%
  dplyr::select("PC1_energy_svs_pp",
                "GDP_per_capita", 
                "Education_Index", "PC1_GI", "PC2_GI", 
                "Percentage_of_rural_population",
                ###energy res
                "Total_fossil_fuel_production", 
                "Total_non_fossil_fuel_production", 
                "import_export_difference", 
                "Fossil_fuel_reserves_Total", 
                "Fossil_fuel_reserves_Natural_Gas_share", 
                "Fossil_fuel_reserves_Oil_share"   
                  ) %>%
  cbind.data.frame(pc_water_ener_av_scores)

# ggpairs(ener_svs_water_sec_df)
ggpairs(ener_svs_water_sec_df)

## CVSRA result: Energy service W-E
W_to_E_water_res_indpt_var <- colnames(ener_svs_water_sec_df)
num_W_to_E_water_res_indpt_var<- ncol(ener_svs_water_sec_df)-1


result_ener_svs_water_sec<-cv_step_reg(data_input =  ener_svs_water_sec_df, 
                                       num_indpt_var = num_W_to_E_water_res_indpt_var, 
                                       names_var = W_to_E_water_res_indpt_var)
model_lm_ener_svs_water_sec<- lm(PC1_energy_svs_pp~GDP_per_capita, data = ener_svs_water_sec_df)
summary(model_lm_ener_svs_water_sec)

##Check AIC 
dfaic = ener_svs_water_sec_df
dpt_var = "PC1_energy_svs_pp"
model.min = lm(paste(dpt_var,"~",1), data = dfaic)
model.full = lm(paste(dpt_var,"~","."), data = dfaic)

test1 = stepAIC(model.min, direction = "forward", scope=list(upper=model.full,lower=model.min),trace = FALSE)
test1$anova

########################################################################################
###control check for direct link with subset countries
###mute ener sector variables
ac_dw_ener_res_df<-water_energy_df %>%
  dplyr::select("Total population with access to safe drinking-water",
                "GDP_per_capita", 
                "Education_Index", 
                "PC1_GI", "PC2_GI", 
                "Percentage_of_rural_population"
                # #energyres
                # "Total_fossil_fuel_production", 
                # "Total_non_fossil_fuel_production", 
                # "import_export_difference", 
                # "Fossil_fuel_reserves_Total", 
                # "Fossil_fuel_reserves_Natural_Gas_share", 
                # "Fossil_fuel_reserves_Oil_share"   
  )  %>%
  cbind.data.frame(pc_water_ener_av_scores)
colnames(ac_dw_ener_res_df)[1]<-c("Total_population_with_access_to_safe_drinking_water")

E_to_W_ener_res_indpt_var <- colnames(ac_dw_ener_res_df)
num_E_to_W_ener_res_indpt_var<- ncol(ac_dw_ener_res_df)-1


result_ac_dw_ener_res<-cv_step_reg(data_input =  ac_dw_ener_res_df, 
                                   num_indpt_var = num_E_to_W_ener_res_indpt_var, 
                                   names_var = E_to_W_ener_res_indpt_var)
model_lm_ac_dw_ener_res<- lm(Population_with_access_to_improved_sanitation~Total_population_with_access_to_safe_drinking_water~Percentage_of_rural_population+PC1_GI+PC2_GI, data = ac_sani_ener_res_df)
summary(model_lm_ac_dw_ener_res)
########################################################################################

ac_dw_ener_res_df<-water_energy_df %>%
  dplyr::select("Total population with access to safe drinking-water",
                "GDP_per_capita", 
                "Education_Index", 
                "PC1_GI", "PC2_GI", 
                "Percentage_of_rural_population",
                #energyres
                "Total_fossil_fuel_production", 
                "Total_non_fossil_fuel_production", 
                "import_export_difference", 
                "Fossil_fuel_reserves_Total", 
                "Fossil_fuel_reserves_Natural_Gas_share", 
                "Fossil_fuel_reserves_Oil_share"   
  )  %>%
  cbind.data.frame(pc_water_ener_av_scores)
colnames(ac_dw_ener_res_df)[1]<-c("Total_population_with_access_to_safe_drinking_water")

E_to_W_ener_res_indpt_var <- colnames(ac_dw_ener_res_df)
num_E_to_W_ener_res_indpt_var<- ncol(ac_dw_ener_res_df)-1

## CVSRA access to safe drinking water W-E
result_ac_dw_ener_res<-cv_step_reg(data_input =  ac_dw_ener_res_df, 
                                       num_indpt_var = num_E_to_W_ener_res_indpt_var, 
                                       names_var = E_to_W_ener_res_indpt_var)
model_lm_ac_dw_ener_res<- lm(Total_population_with_access_to_safe_drinking_water~Percentage_of_rural_population+PC1_GI+PC2_GI, data = ac_dw_ener_res_df)
summary(model_lm_ac_dw_ener_res)

##Check AIC 
dfaic = ac_dw_ener_res_df
dpt_var = "Total_population_with_access_to_safe_drinking_water"
model.min = lm(paste(dpt_var,"~",1), data = dfaic)
model.full = lm(paste(dpt_var,"~","."), data = dfaic)
test1 = stepAIC(model.min, direction = "forward", 
                scope=list(upper=model.full,lower=model.min),
                trace = FALSE)
test1$anova

test1.3 = stepAIC(lm(Total_population_with_access_to_safe_drinking_water~Percentage_of_rural_population+PC1_GI+PC2_GI, data = ac_dw_ener_res_df),
                  direction = "forward", 
                  # scope=list(upper=model.full,lower=model.min),
                  trace = FALSE)
test1.3$anova

model_test2<- lm(Total_population_with_access_to_safe_drinking_water~Percentage_of_rural_population+PC1_GI+import_export_difference, data = ac_dw_ener_res_df)
summary(model_test2)
glm.fit_og=glm(Total_population_with_access_to_safe_drinking_water~Percentage_of_rural_population+PC1_GI+PC2_GI,data=ac_dw_ener_res_df)
cv.error_test1_og=cv.glm(ac_dw_ener_res_df,glm.fit_og)$delta[1] 
cv.error_test1_og
AIC(glm.fit_og, k = 2)
glm.fit_aic=glm(Total_population_with_access_to_safe_drinking_water~Percentage_of_rural_population+PC1_GI+import_export_difference,data=ac_dw_ener_res_df)
cv.error_test1_aic=cv.glm(ac_dw_ener_res_df,glm.fit_aic)$delta[1] 
cv.error_test1_aic
AIC(glm.fit_aic, k = 2)
# ggpairs(ac_dw_ener_res_df)


pred_ac_dw_ener_res<-predict(model_lm_ac_dw_ener_res, newdata =  ac_dw_ener_res_df)
# 
p6<-ggplot(ac_dw_ener_res_df, aes( x=Total_population_with_access_to_safe_drinking_water, y=pred_ac_dw_ener_res))+
  geom_point(size=8, aes(color = Percentage_of_rural_population))+
  labs(x="Total Population with Access to Safe Drinking Water (%)",y= "Model Estimation (%)")+
  geom_abline(slope = 1,color="red",linetype=2)+
  scale_x_continuous(limits = c(50, 100))+
  scale_y_continuous(limits = c(50, 100))+
  theme_bw()+
  theme(axis.title.x = element_text(size=30),
        axis.title.y = element_text(size = 30),
        axis.text.x = element_text(color="#993333", size=14),
        axis.text.y = element_text(color="#993333", size=14))+
  theme(legend.title = element_text(colour="black", size=10, face="bold"),
        legend.text = element_text(size =5))+
  geom_text_repel(aes(Total_population_with_access_to_safe_drinking_water, 
                      pred_ac_dw_ener_res, 
                      label = row.names(ac_dw_ener_res_df)), show.legend = FALSE)

p6 + scale_color_continuous(name="% Rural Population ", #name of legend
                              breaks = with(ac_dw_ener_res_df, c(min(Percentage_of_rural_population), mean(Percentage_of_rural_population), max(Percentage_of_rural_population))), #choose breaks of variable
                              labels = c("Low", "Medium", "High"), #label
                              low = "pink",  #color of lowest value
                              high = "red"  #color of highest value
                              
)

#energy resources to ac sani
Population_with_access_to_improved_sanitation<-ac_sani_df[yes_list_water_energy,1]


########################################################################################
###control check for direct link with subset countries
###mute ener sector variables
ac_sani_ener_res_df<-water_energy_df %>%
  dplyr::select(
    "GDP_per_capita", 
    "Education_Index", 
    "PC1_GI", "PC2_GI", 
    "Percentage_of_rural_population"
    #energyres
    # "Total_fossil_fuel_production", 
    # "Total_non_fossil_fuel_production", 
    # "import_export_difference", 
    # "Fossil_fuel_reserves_Total", 
    # "Fossil_fuel_reserves_Natural_Gas_share", 
    # "Fossil_fuel_reserves_Oil_share"   
  ) %>%
  cbind.data.frame(pc_water_ener_av_scores)

ac_sani_ener_res_df<-cbind.data.frame(Population_with_access_to_improved_sanitation,ac_sani_ener_res_df)

E_to_W_ener_res_indpt_var <- colnames(ac_sani_ener_res_df)
num_E_to_W_ener_res_indpt_var<- ncol(ac_sani_ener_res_df)-1


result_ac_sani_ener_res<-cv_step_reg(data_input =  ac_sani_ener_res_df, 
                                     num_indpt_var = num_E_to_W_ener_res_indpt_var, 
                                     names_var = E_to_W_ener_res_indpt_var)

model_control_lm_ac_sani_ener_res<- lm(Population_with_access_to_improved_sanitation~GDP_per_capita, 
                               data = ac_sani_ener_res_df)
summary(model_control_lm_ac_sani_ener_res)


########################################################################################



ac_sani_ener_res_df<-water_energy_df %>%
  dplyr::select(
                "GDP_per_capita", 
                "Education_Index", 
                "PC1_GI", "PC2_GI", 
                "Percentage_of_rural_population",
                #energyres
                "Total_fossil_fuel_production", 
                "Total_non_fossil_fuel_production", 
                "import_export_difference", 
                "Fossil_fuel_reserves_Total", 
                "Fossil_fuel_reserves_Natural_Gas_share", 
                "Fossil_fuel_reserves_Oil_share"   
  ) %>%
  cbind.data.frame(pc_water_ener_av_scores)
  
ac_sani_ener_res_df<-cbind.data.frame(Population_with_access_to_improved_sanitation,ac_sani_ener_res_df)

E_to_W_ener_res_indpt_var <- colnames(ac_sani_ener_res_df)
num_E_to_W_ener_res_indpt_var<- ncol(ac_sani_ener_res_df)-1

## CVSRA results access to sanitation W-E
result_ac_sani_ener_res<-cv_step_reg(data_input =  ac_sani_ener_res_df, 
                                   num_indpt_var = num_E_to_W_ener_res_indpt_var, 
                                   names_var = E_to_W_ener_res_indpt_var)

model_lm_ac_sani_ener_res<- lm(Population_with_access_to_improved_sanitation~GDP_per_capita+Fossil_fuel_reserves_Oil_share, data = ac_sani_ener_res_df)
summary(model_lm_ac_sani_ener_res)

##Check AIC 
dfaic = ac_sani_ener_res_df
dpt_var = "Population_with_access_to_improved_sanitation"
model.min = lm(paste(dpt_var,"~",1), data = dfaic)
model.full = lm(paste(dpt_var,"~","."), data = dfaic)
test1 = stepAIC(model.min, direction = "forward", 
                scope=list(upper=model.full,lower=model.min),
                trace = FALSE)
test1$anova

# ggpairs(ac_sani_ener_res_df)


pred_ac_sani_ener_res<-predict(model_lm_ac_sani_ener_res, newdata =  ac_sani_ener_res_df)
# 
p6.1<-ggplot(ac_sani_ener_res_df, aes( x=Population_with_access_to_improved_sanitation, y=pred_ac_sani_ener_res))+
  geom_point(size=8, aes(color = GDP_per_capita))+
  labs(x="Population with access to improved sanitation (%)",y= "Model Estimation (%)")+
  geom_abline(slope = 1,color="red",linetype=2)+
  # scale_x_continuous(limits = c(50, 100))+
  # scale_y_continuous(limits = c(50, 100))+
  theme_bw()+
  theme(axis.title.x = element_text(size=30),
        axis.title.y = element_text(size = 30),
        axis.text.x = element_text(color="#993333", size=14),
        axis.text.y = element_text(color="#993333", size=14))+
  theme(legend.title = element_text(colour="black", size=10, face="bold"),
        legend.text = element_text(size =5))+
  geom_text_repel(aes(Population_with_access_to_improved_sanitation, 
                      pred_ac_sani_ener_res, 
                      label = row.names(ac_sani_ener_res_df)), show.legend = FALSE)

p6.1 + scale_color_continuous(name="GDP per capita", #name of legend
                            breaks = with(ac_sani_ener_res_df, c(min(GDP_per_capita), mean(GDP_per_capita), max(GDP_per_capita))), #choose breaks of variable
                            labels = c("Low", "Medium", "High"), #label
                            low = "pink",  #color of lowest value
                            high = "red"  #color of highest value
                            
)

#######3D plot
#Graph Resolution (more important for more complex shapes)
graph_reso <- 0.05

#Setup Axis
axis_x <- seq(min(ac_sani_ener_res_df$GDP_per_capita), 
              max(ac_sani_ener_res_df$GDP_per_capita), by = graph_reso)
axis_y <- seq(min(ac_sani_ener_res_df$Fossil_fuel_reserves_Oil_share), 
              max(ac_sani_ener_res_df$Fossil_fuel_reserves_Oil_share), by = graph_reso)

#Sample points
model6.2_surface <- expand.grid(GDP_per_capita = axis_x, Fossil_fuel_reserves_Oil_share = axis_y, KEEP.OUT.ATTRS = F)
model6.2_surface$Population_with_access_to_improved_sanitation <- predict.lm(model_lm_ac_sani_ener_res, newdata = model6.2_surface)
model6.2_surface <- acast(model6.2_surface, Fossil_fuel_reserves_Oil_share ~ GDP_per_capita , value.var = "Population_with_access_to_improved_sanitation") #y ~ x

# hcolors=c("red","blue","green")[my_df$Species]
p6.2_3d <- plot_ly(ac_sani_ener_res_df, 
                   x = ~GDP_per_capita, 
                   y = ~Fossil_fuel_reserves_Oil_share, 
                   z = ~Population_with_access_to_improved_sanitation,
                   text = row.names(ac_sani_ener_res_df),
                   type = "scatter3d"
                   ,
                   mode = "markers"
                   # ,
                   # marker = list(color = hcolors)
)%>%
  layout(
    scene = list(
      xaxis = list(title = "GDP per capita"),
      yaxis = list(title = "%Oil in Fossil fuel reserves"),
      zaxis = list(title = "Access to Sanitation")
    ))

p6.2_3d <- add_trace(p = p6.2_3d,
                     z = model6.2_surface,
                     x = axis_x,
                     y = axis_y,
                     type = "surface",
                     legend = FALSE
)
p6.2_3d


####water_food link
water_food_df<-cbind.data.frame(ac_dw_df[yes_list_water_food,], 
                                land_svs_food_avail_df[yes_list_water_food,],
                                land_svs_food_prod_var_df[yes_list_water_food,1],
                                land_svs_food_spl_var_df[yes_list_water_food,1],
                                land_svs_nutri_bal_df[yes_list_water_food,1]
                                ) %>%
  cbind.data.frame(ac_sani_df[yes_list_water_food,]$`Population with access to improved sanitation`)
water_food_df<-water_food_df[,-(7:17)]#remove redundant variables
colnames(water_food_df)[ncol(water_food_df)]<-c("Population_with_access_to_improved_sanitation")
colnames(water_food_df)[ncol(water_food_df)-1]<-c("nutri_bal_index")
colnames(water_food_df)[ncol(water_food_df)-2]<-c("Per_capita_food_supply_variability")
colnames(water_food_df)[ncol(water_food_df)-3]<-c("Per_capita_food_production_variability")
water_food_df<-cbind.data.frame(water_food_df,
                                ac_dw_df[yes_list_water_food,]$`Total population with access to safe drinking-water`)
colnames(water_food_df)[ncol(water_food_df)]<-c("Total_population_with_access_to_safe_drinking_water")
water_food_av_pca<-water_food_df %>%
  dplyr::select("Long-term annual precip depth",
                "Total renewable water sources",
                "Interannual variability",
                "Seasonal variability",
                "Flood occurrence",
                "Drought frequency")

pc_water_food<-prcomp(water_food_av_pca, scale= TRUE, center = TRUE)
summary(pc_water_food)#two pc accounts for 67% of the variance
pc_water_food
pc_water_food_av_scores<-pc_water_food$x
colnames(pc_water_food_av_scores)<-c("PC1_water","PC2_water","PC3_water","PC4_water","PC5_water","PC6_water")#two pc captured 70% of the total variance in water availability
pc_water_food_av_scores<-pc_water_food_av_scores[,1:2]

###water resources to food availability 
########################################################################################
###control check for direct link with subset countries
###mute water sector variables
###ac_dw
ac_dw_water_sec_df<-water_food_df %>%
  dplyr::select("Total_population_with_access_to_safe_drinking_water", 
                "GDP_per_capita", 
                "Education_Index",  
                "PC1_GI", "PC2_GI", 
                "Percentage_of_rural_population"
                # #food res specific human capacity
                # "Value_of_food_imports_over_total_merchandise_exports", 
                # "Cereal_import_dependency_ratio", 
                # "Rail_lines_density", 
                # "Percent_of_arable_land_equipped_for_irrigation",
                # #food res
                # "PC1_land_res",
                # "PC2_land_res"
                
  ) %>%
  cbind.data.frame(pc_water_food_av_scores)

W_to_F_water_res_indpt_var <- colnames(ac_dw_water_sec_df)
num_W_to_F_water_res_indpt_var<- ncol(ac_dw_water_sec_df)-1


result_food_avail_water_sec_df<-cv_step_reg(data_input =  ac_dw_water_sec_df, 
                                            num_indpt_var = num_W_to_F_water_res_indpt_var, 
                                            names_var = W_to_F_water_res_indpt_var)
model_control_lm_ac_dw_water_sec<- lm(Total_population_with_access_to_safe_drinking_water~PC1_GI, data = ac_dw_water_sec_df)
summary(model_control_lm_ac_dw_water_sec)


###ac sani
ac_sani_water_sec_df<-water_food_df %>%
  dplyr::select("Population_with_access_to_improved_sanitation", 
                "GDP_per_capita", 
                "Education_Index",  
                "PC1_GI", "PC2_GI", 
                "Percentage_of_rural_population"
                # #food res specific human capacity
                # "Value_of_food_imports_over_total_merchandise_exports", 
                # "Cereal_import_dependency_ratio", 
                # "Rail_lines_density", 
                # "Percent_of_arable_land_equipped_for_irrigation",
                # #food res
                # "PC1_land_res",
                # "PC2_land_res"
                
  ) %>%
  cbind.data.frame(pc_water_food_av_scores)

W_to_F_water_res_indpt_var <- colnames(ac_sani_water_sec_df)
num_W_to_F_water_res_indpt_var<- ncol(ac_sani_water_sec_df)-1


result_ac_sani_water_sec_df<-cv_step_reg(data_input =  ac_sani_water_sec_df, 
                                            num_indpt_var = num_W_to_F_water_res_indpt_var, 
                                            names_var = W_to_F_water_res_indpt_var)
model_control_lm_ac_sani_water_sec<- lm(Population_with_access_to_improved_sanitation~GDP_per_capita, data = ac_sani_water_sec_df)
summary(model_control_lm_ac_sani_water_sec)
########################################################################################

food_avail_water_sec_df<-water_food_df %>%
  dplyr::select("food_availability_index", 
                "GDP_per_capita", 
                "Education_Index",  
                "PC1_GI", "PC2_GI", 
                "Percentage_of_rural_population",
                #food res specific human capacity
                "Value_of_food_imports_over_total_merchandise_exports", 
                "Cereal_import_dependency_ratio", 
                "Rail_lines_density", 
                "Percent_of_arable_land_equipped_for_irrigation",
                #food res
                "PC1_land_res",
                "PC2_land_res"
                
                ) %>%
  cbind.data.frame(pc_water_food_av_scores)


W_to_F_water_res_indpt_var <- colnames(food_avail_water_sec_df)
num_W_to_F_water_res_indpt_var<- ncol(food_avail_water_sec_df)-1

## CVSRA results for Food Utilization Index W-F
result_food_avail_water_sec_df<-cv_step_reg(data_input =  food_avail_water_sec_df, 
                                   num_indpt_var = num_W_to_F_water_res_indpt_var, 
                                   names_var = W_to_F_water_res_indpt_var)
model_lm_food_avail_water_sec<- lm(food_availability_index~PC2_GI+Percentage_of_rural_population, data = food_avail_water_sec_df)
summary(model_lm_food_avail_water_sec)

##Check AIC 
dfaic = food_avail_water_sec_df
dpt_var = "food_availability_index"
model.min = lm(paste(dpt_var,"~",1), data = dfaic)
model.full = lm(paste(dpt_var,"~","."), data = dfaic)
test1 = stepAIC(model.min, direction = "forward", 
                scope=list(upper=model.full,lower=model.min),
                trace = FALSE)
test1$anova


ggpairs(food_avail_water_sec_df) 



#water resources to Protein Index
nutri_bal_water_sec_df<-water_food_df %>%
  dplyr::select("nutri_bal_index", 
                "GDP_per_capita", 
                "Education_Index",  "PC1_GI", "PC2_GI", 
                "Percentage_of_rural_population",
                #food res specific human capacity
                "Value_of_food_imports_over_total_merchandise_exports", 
                "Cereal_import_dependency_ratio", 
                "Rail_lines_density", 
                "Percent_of_arable_land_equipped_for_irrigation",
                #food res
                "PC1_land_res",
                "PC2_land_res"
                ) %>%
  cbind.data.frame(pc_water_food_av_scores)


W_to_F_water_res_indpt_var <- colnames(nutri_bal_water_sec_df)
num_W_to_F_water_res_indpt_var<- ncol(nutri_bal_water_sec_df)-1

## CVSRA result for protein balance index W-F
result_nutri_bal_water_sec_df<-cv_step_reg(data_input =  nutri_bal_water_sec_df, 
                                            num_indpt_var = num_W_to_F_water_res_indpt_var, 
                                            names_var = W_to_F_water_res_indpt_var)
model_lm_nutri_bal_water_sec<- lm(nutri_bal_index~GDP_per_capita+PC1_land_res, data = nutri_bal_water_sec_df)
summary(model_lm_nutri_bal_water_sec)

##Check AIC 
dfaic = nutri_bal_water_sec_df
dpt_var = "nutri_bal_index"
model.min = lm(paste(dpt_var,"~",1), data = dfaic)
model.full = lm(paste(dpt_var,"~","."), data = dfaic)
test1 = stepAIC(model.min, direction = "forward", 
                scope=list(upper=model.full,lower=model.min),
                trace = FALSE)
test1$anova

ggpairs(nutri_bal_water_sec_df) 


#water resources to Food production variability
food_prod_var_water_sec_df<-water_food_df %>%
  dplyr::select("Per_capita_food_production_variability",
                "GDP_per_capita", 
                "Education_Index",  
                "PC1_GI", "PC2_GI", 
                "Percentage_of_rural_population",
                #food res specific human capacity
                "Value_of_food_imports_over_total_merchandise_exports", 
                "Cereal_import_dependency_ratio", 
                "Rail_lines_density", 
                "Percent_of_arable_land_equipped_for_irrigation",
                #food res
                "PC1_land_res",
                "PC2_land_res"
                ) %>%
  cbind.data.frame(pc_water_food_av_scores)


W_to_F_water_res_indpt_var <- colnames(food_prod_var_water_sec_df)
num_W_to_F_water_res_indpt_var<- ncol(food_prod_var_water_sec_df)-1

## CVSRA result for food production variability F-W
result_food_prod_var_water_sec<-cv_step_reg(data_input =  food_prod_var_water_sec_df, 
                                           num_indpt_var = num_W_to_F_water_res_indpt_var, 
                                           names_var = W_to_F_water_res_indpt_var)
model_lm_food_prod_var_water_sec<- lm(Per_capita_food_production_variability~PC2_GI, data = food_prod_var_water_sec_df)
summary(model_lm_food_prod_var_water_sec)

##Check AIC 
dfaic = food_prod_var_water_sec_df
dpt_var = "Per_capita_food_production_variability"
model.min = lm(paste(dpt_var,"~",1), data = dfaic)
model.full = lm(paste(dpt_var,"~","."), data = dfaic)
test1 = stepAIC(model.min, direction = "forward", 
                scope=list(upper=model.full,lower=model.min),
                trace = FALSE)
test1$anova

ggpairs(food_prod_var_water_sec_df) 


#water resources to Food production variability
food_spl_var_water_sec_df<-water_food_df %>%
  dplyr::select("Per_capita_food_supply_variability",
                "GDP_per_capita", 
                "Education_Index",  
                "PC1_GI", "PC2_GI", 
                "Percentage_of_rural_population",
                #food res specific human capacity
                "Value_of_food_imports_over_total_merchandise_exports", 
                "Cereal_import_dependency_ratio", 
                "Rail_lines_density", 
                "Percent_of_arable_land_equipped_for_irrigation",
                #food res
                "PC1_land_res",
                "PC2_land_res") %>%
  cbind.data.frame(pc_water_food_av_scores)


W_to_F_water_res_indpt_var <- colnames(food_spl_var_water_sec_df)
num_W_to_F_water_res_indpt_var<- ncol(food_spl_var_water_sec_df)-1

## CVSRA result: food supply variability W-F no relations found
result_food_spl_var_water_sec<-cv_step_reg(data_input =  food_spl_var_water_sec_df, 
                                            num_indpt_var = num_W_to_F_water_res_indpt_var, 
                                            names_var = W_to_F_water_res_indpt_var)
model_lm_food_spl_var_water_sec<- lm(Per_capita_food_supply_variability~., data = food_spl_var_water_sec_df)
summary(model_lm_food_spl_var_water_sec)

ggpairs(food_spl_var_water_sec_df) 



### Energy Food link: Energy resources to food svs variables

###create energy food sector variables data frame:
food_energy_df<-cbind.data.frame(ener_pp_df[yes_list_food_energy,], food_res_svs_df[yes_list_food_energy,])
food_energy_df<-food_energy_df[,-(1:9)]#remove redundant variables
food_energy_df<-food_energy_df[,-55]#remove redundant variables

ggpairs(food_energy_df[,land_avail])
#plot land availability pc 1 and 2

###Land Availability for Energy-Food sector
pc_land_avail<-prcomp(food_energy_df[,land_avail])
summary(pc_land_avail)
pc_land_avail
biplot(pc_land_avail,ylim=c(-.58,.58),xlim=c(-1,1))
pc_land_avail_scores<-pc_land_avail$x[,1:2]
colnames(pc_land_avail_scores)<-c("PC1_land_res", "PC2_land_res")


###Nutritional Balance or Protein Balance for Energy-Food Sector
ggpairs(food_energy_df[,nutri_bal])
pc_nutri_bal<-prcomp(food_energy_df[,nutri_bal], scale = TRUE, center = TRUE)
summary(pc_nutri_bal)
pc_nutri_bal
biplot(pc_nutri_bal,
       ylim=c(-.58,.58),
       xlim=c(-1,1)
)
pc_nutri_bal_score<-pc_nutri_bal$x
nutri_bal_index<-pc_nutri_bal_score[,1]


###Food utilization for Energy-Food sector
ggpairs(food_energy_df[,food_util])
pc_food_util<-prcomp(food_energy_df[,food_util], scale = TRUE, center = TRUE)
summary(pc_food_util)
pc_food_util
biplot(pc_food_util,
       ylim=c(-.58,.58),
       xlim=c(-1,1)
)
pc_food_util_scores<-pc_food_util$x[,1:2]
colnames(pc_food_util_scores)<-c("PC1_food_availability","PC2_food_availability")
food_availability_index<-pc_food_util_scores[,1]


###Young-age malnutrition for Energy-Food sector
food_pca_health_proxy<-food_energy_df[yes_list_food_energy,] %>%
  dplyr::select(
    # "Prevalence of undernourishment",
    "Percentage of children under 5 years of age who are stunted",
    "Percentage of children under 5 years of age affected by wasting",
    "Percentage of children under 5 years of age who are underweight",
    "Percentage of children under 5 years of age who are overweight"
  )
pc_food_heal<-prcomp(food_pca_health_proxy)#these value do not need to be scaled since they are all percentages
summary(pc_food_heal)
pc_food_heal
biplot(pc_food_heal, xlim= c(-0.4,0.8),ylim= c(-0.5, 0.5))
malnutrition_index<-pc_food_heal$x
colnames(malnutrition_index)<-c("PC1_U5nourish","PC2_U5nourish","PC3_U5nourish","PC4_U5nourish")
malnutrition_index<- malnutrition_index[,1]
##we se that The U5 predictors are highly correlated. so we use 1 pc to capture 63 percent of the total variance as U5 proxy





ener_food_df<-cbind.data.frame(ener_res_serv_pp[yes_list_food_energy,-c(1,10,11)],
                                malnutrition_index,
                                
                                food_energy_df[,c("Cereal import dependency ratio",
                                                  "Value of food imports over total merchandise exports", 
                                                  "Rail lines density", 
                                                  "Percent of arable land equipped for irrigation")],
                                food_availability_index,
                                land_svs_food_prod_var_df[yes_list_food_energy,1],
                                land_svs_food_spl_var_df[yes_list_food_energy,1],
                                nutri_bal_index)

colnames(ener_food_df)[ncol(ener_food_df)]<-c("nutri_bal_index")
colnames(ener_food_df)[ncol(ener_food_df)-1]<-c("Per_capita_food_supply_variability")
colnames(ener_food_df)[ncol(ener_food_df)-2]<-c("Per_capita_food_production_variability")
colnames(ener_food_df)[ncol(ener_food_df)-3]<-c("food_availability_index")
colnames(ener_food_df)[ncol(ener_food_df)-4]<-c("Percent_of_arable_land_equipped_for_irrigation")
colnames(ener_food_df)[ncol(ener_food_df)-5]<-c("Rail_lines_density")
colnames(ener_food_df)[ncol(ener_food_df)-6]<-c("Value_of_food_imports_over_total_merchandise_exports")
colnames(ener_food_df)[ncol(ener_food_df)-7]<-c("Cereal_import_dependency_ratio")

ener_food_df[,13:14]<-log10(ener_food_df[,13:14])

pc_WBGI_food_energy<-prcomp(food_energy_df%>%
                              dplyr::select(
                                "Voice and Accountability",
                                "Political Stability and Absence of Violence/Terrorism",
                                "Government Effectiveness",
                                "Regulatory Quality",
                                "Rule of Law",
                                "Control of Corruption"
                              )) 
summary(pc_WBGI_food_energy)###two pcs captured 94% of total variance.
pc_WBGI_food_energy
pc_GI_scores_food_ener<-pc_WBGI_food_energy$x
colnames(pc_GI_scores_food_ener)<-c("PC1_GI","PC2_GI","PC3_GI","PC4_GI","PC5_GI","PC6_GI")
pc_GI_scores_food_ener<-pc_GI_scores_food_ener[,1:2]#two pc accounts for 94% of the variance

ener_food_df<-cbind.data.frame(ener_food_df,pc_GI_scores_food_ener,pc_land_avail_scores)



###################
###ener resources to food availability 
food_avail_ener_sec_df<-ener_food_df %>%
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
                )


E_to_F_ener_res_indpt_var <- colnames(food_avail_ener_sec_df)
num_E_to_F_ener_res_indpt_var<- ncol(food_avail_ener_sec_df)-1

##CVSRA result Food Utilization （availability) Index E-F
result_food_avail_ener_sec_df<-cv_step_reg(data_input =  food_avail_ener_sec_df, 
                                            num_indpt_var = num_E_to_F_ener_res_indpt_var, 
                                            names_var = E_to_F_ener_res_indpt_var)

model_lm_food_avail_ener_sec<- lm(food_availability_index~Rail_lines_density
                                          +Percent_of_arable_land_equipped_for_irrigation
                                          , data = food_avail_ener_sec_df)
summary(model_lm_food_avail_ener_sec)

##Check AIC 
dfaic = food_avail_ener_sec_df
dpt_var = "food_availability_index"
model.min = lm(paste(dpt_var,"~",1), data = dfaic)
model.full = lm(paste(dpt_var,"~","."), data = dfaic)
test1 = stepAIC(model.min, direction = "forward", 
                scope=list(upper=model.full,lower=model.min),
                trace = FALSE)
test1$anova


# ggpairs(food_avail_ener_sec_df)
pred_food_avail_ener_sec<-predict(model_lm_food_avail_ener_sec, newdata = food_avail_ener_sec_df)

########plot
p7.5<-ggplot(food_avail_ener_sec_df, aes( x=food_availability_index, y=pred_food_avail_ener_sec))+
  geom_point(size=8, aes(color = `Rail_lines_density`))+
  # geom_point(color= "brown", aes(size = GDP_per_capita))+
  labs(x="Food Unavailability Index (PC1)",y= "Model Estimation (PC score)")+
  geom_abline(slope = 1,color="red",linetype=2)+
  # scale_x_continuous(limits = c(-3, 3))+
  # scale_y_continuous(limits = c(-3, 3))+
  theme_bw()+
  theme(axis.title.x = element_text(size=30),
        axis.title.y = element_text(size = 30),
        axis.text.x = element_text(color="#993333", size=14),
        axis.text.y = element_text(color="#993333", size=14))+
  theme(legend.title = element_text(colour="black", size=10, face="bold"),
        legend.text = element_text(size =5))+
  geom_text_repel(aes(food_availability_index, pred_food_avail_ener_sec, label = row.names(food_avail_ener_sec_df)), show.legend = FALSE)

p7.5 + scale_color_continuous(name="Rail Lines Density (log10)", #name of legend
                            breaks = with(food_avail_ener_sec_df, c(min(Rail_lines_density), mean(Rail_lines_density), max(Rail_lines_density))), #choose breaks of variable
                            labels = c("Low", "Medium", "High"), #label
                            low = "pink",  #color of lowest value
                            high = "red"  #color of highest value
                            
)


#######3D plot
#Graph Resolution (more important for more complex shapes)
graph_reso <- 0.05

#Setup Axis
axis_x <- seq(min(food_avail_ener_sec_df$Rail_lines_density), 
              max(food_avail_ener_sec_df$Rail_lines_density), by = graph_reso)
axis_y <- seq(min(food_avail_ener_sec_df$Percent_of_arable_land_equipped_for_irrigation), 
              max(food_avail_ener_sec_df$Percent_of_arable_land_equipped_for_irrigation), by = graph_reso)

#Sample points
model7.4_surface <- expand.grid(Rail_lines_density = axis_x, Percent_of_arable_land_equipped_for_irrigation = axis_y, KEEP.OUT.ATTRS = F)
model7.4_surface$food_availability_index <- predict.lm(model_lm_food_avail_ener_sec, newdata = model7.4_surface)
model7.4_surface <- acast(model7.4_surface, Percent_of_arable_land_equipped_for_irrigation ~ Rail_lines_density , value.var = "food_availability_index") #y ~ x

# hcolors=c("red","blue","green")[my_df$Species]
p7.4_3d <- plot_ly(food_avail_ener_sec_df, 
                   x = ~Rail_lines_density, 
                   y = ~Percent_of_arable_land_equipped_for_irrigation, 
                   z = ~food_availability_index,
                   text = row.names(food_avail_ener_sec_df),
                   type = "scatter3d"
                   ,
                   mode = "markers"
                   # ,
                   # marker = list(color = hcolors)
)%>%
  layout(
    scene = list(
      xaxis = list(title = "Rail_lines_density"),
      yaxis = list(title = "% Irrigation"),
      zaxis = list(title = "Food Unavailability Index")
    ))

p7.4_3d <- add_trace(p = p7.4_3d,
                     z = model7.4_surface,
                     x = axis_x,
                     y = axis_y,
                     type = "surface",
                     legend = FALSE
)
p7.4_3d


########################################################################################
###control check for direct link with subset countries
###mute energy sector variables and 
food_avail_ener_sec_df<-ener_food_df %>%
  dplyr::select("food_availability_index",
                # #energy res
                # "Total_fossil_fuel_production", "Total_non_fossil_fuel_production", 
                # "import_export_difference", 
                # "Fossil_fuel_reserves_Total", 
                # "Fossil_fuel_reserves_Natural_Gas_share", 
                # "Fossil_fuel_reserves_Oil_share", 
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
  )


E_to_F_ener_res_indpt_var <- colnames(food_avail_ener_sec_df)
num_E_to_F_ener_res_indpt_var<- ncol(food_avail_ener_sec_df)-1


result_food_avail_ener_sec_df<-cv_step_reg(data_input =  food_avail_ener_sec_df, 
                                           num_indpt_var = num_E_to_F_ener_res_indpt_var, 
                                           names_var = E_to_F_ener_res_indpt_var)

model_control_lm_food_avail_ener_sec<- lm(food_availability_index~PC2_GI
                                  +Percentage_of_rural_population
                                  , data = food_avail_ener_sec_df)
summary(model_control_lm_food_avail_ener_sec)
########################################################################################


#ener resources to Protein Index
########################################################################################
###control check for direct link with subset countries
###mute energy sector variables
nutri_bal_ener_sec_df<-ener_food_df %>%
  dplyr::select("nutri_bal_index",
                # #energy res
                # "Total_fossil_fuel_production", "Total_non_fossil_fuel_production", 
                # "import_export_difference", 
                # "Fossil_fuel_reserves_Total", 
                # "Fossil_fuel_reserves_Natural_Gas_share", 
                # "Fossil_fuel_reserves_Oil_share", 
                #general capacity
                "GDP_per_capita", 
                "Education_Index", 
                "PC1_GI", "PC2_GI",
                "Percentage_of_rural_population",
                #food res specific human capacity
                "Value_of_food_imports_over_total_merchandise_exports",
                "Cereal_import_dependency_ratio",
                "Rail_lines_density",
                "Percent_of_arable_land_equipped_for_irrigation",
                #food res
                "PC1_land_res",
                "PC2_land_res")


E_to_F_ener_res_indpt_var <- colnames(nutri_bal_ener_sec_df)
num_E_to_F_ener_res_indpt_var<- ncol(nutri_bal_ener_sec_df)-1


result_nutri_bal_ener_sec_df<-cv_step_reg(data_input =  nutri_bal_ener_sec_df, 
                                          num_indpt_var = num_E_to_F_ener_res_indpt_var, 
                                          names_var = E_to_F_ener_res_indpt_var)
########################################################################################

nutri_bal_ener_sec_df<-ener_food_df %>%
  dplyr::select("nutri_bal_index",
                #energy res
                "Total_fossil_fuel_production", "Total_non_fossil_fuel_production", 
                "import_export_difference", 
                "Fossil_fuel_reserves_Total", 
                "Fossil_fuel_reserves_Natural_Gas_share", 
                "Fossil_fuel_reserves_Oil_share", 
                #general capacity
                "GDP_per_capita", 
                "Education_Index", 
                "PC1_GI", "PC2_GI",
                "Percentage_of_rural_population",
                #food res specific human capacity
                "Value_of_food_imports_over_total_merchandise_exports",
                "Cereal_import_dependency_ratio",
                "Rail_lines_density",
                "Percent_of_arable_land_equipped_for_irrigation",
                #food res
                "PC1_land_res",
                "PC2_land_res")


E_to_F_ener_res_indpt_var <- colnames(nutri_bal_ener_sec_df)
num_E_to_F_ener_res_indpt_var<- ncol(nutri_bal_ener_sec_df)-1

## CVSRA result: Protein (nutrition) Balance Index
result_nutri_bal_ener_sec_df<-cv_step_reg(data_input =  nutri_bal_ener_sec_df, 
                                           num_indpt_var = num_E_to_F_ener_res_indpt_var, 
                                           names_var = E_to_F_ener_res_indpt_var)
model_lm_nutri_bal_ener_sec<- lm(nutri_bal_index~GDP_per_capita, data = nutri_bal_ener_sec_df)
summary(model_lm_nutri_bal_ener_sec)

##Check AIC 
dfaic = nutri_bal_ener_sec_df
dpt_var = "nutri_bal_index"
model.min = lm(paste(dpt_var,"~",1), data = dfaic)
model.full = lm(paste(dpt_var,"~","."), data = dfaic)
test1 = stepAIC(model.min, direction = "forward", 
                scope=list(upper=model.full,lower=model.min),
                trace = FALSE)
test1$anova
AIC(lm(nutri_bal_index~GDP_per_capita, data = nutri_bal_ener_sec_df))


ggpairs(nutri_bal_ener_sec_df) 

pred_nutri_bal_ener_sec<-predict(model_lm_nutri_bal_ener_sec, newdata =  nutri_bal_ener_sec_df)

p7<-ggplot(nutri_bal_ener_sec_df, aes( x=nutri_bal_index, y=pred_nutri_bal_ener_sec))+
  geom_point(size=8, aes(color = `GDP_per_capita`))+
  # geom_point(color= "brown", aes(size = GDP_per_capita))+
  labs(x="Protein Index (PC1)",y= "Model Estimation (PC score)")+
  geom_abline(slope = 1,color="red",linetype=2)+
  scale_x_continuous(limits = c(-3, 3))+
  scale_y_continuous(limits = c(-3, 3))+
  theme_bw()+
  theme(axis.title.x = element_text(size=30),
        axis.title.y = element_text(size = 30),
        axis.text.x = element_text(color="#993333", size=14),
        axis.text.y = element_text(color="#993333", size=14))+
  theme(legend.title = element_text(colour="black", size=10, face="bold"),
        legend.text = element_text(size =5))+
  geom_text_repel(aes(nutri_bal_index, pred_nutri_bal_ener_sec, label = row.names(nutri_bal_ener_sec_df)), show.legend = FALSE)

p7 + scale_color_continuous(name="GDP per capita (log10)", #name of legend
                            breaks = with(nutri_bal_ener_sec_df, c(min(GDP_per_capita), mean(GDP_per_capita), max(GDP_per_capita))), #choose breaks of variable
                            labels = c("Low", "Medium", "High"), #label
                            low = "pink",  #color of lowest value
                            high = "red"  #color of highest value
                            
)
p7.1<-ggplot(nutri_bal_ener_sec_df, aes( x=`GDP_per_capita`, y=nutri_bal_index))+
  geom_point(size=8, aes(color = `GDP_per_capita`))+
  # geom_point(color= "brown", aes(size = GDP_per_capita))+
  labs(x="Protein Index (PC1)",y= "Model Estimation (PC score)")+
  geom_smooth(method='lm',formula=y~x, se=TRUE)+
  # scale_x_continuous(limits = c(-3, 3))+
  # scale_y_continuous(limits = c(-3, 3))+
  theme_bw()+
  theme(axis.title.x = element_text(size=30),
        axis.title.y = element_text(size = 30),
        axis.text.x = element_text(color="#993333", size=14),
        axis.text.y = element_text(color="#993333", size=14))+
  theme(legend.title = element_text(colour="black", size=10, face="bold"),
        legend.text = element_text(size =5))+
  geom_text_repel(aes(`GDP_per_capita`, nutri_bal_index, label = row.names(nutri_bal_ener_sec_df)), show.legend = FALSE)

p7.1 + scale_color_continuous(name="GDP per capita (log10)", #name of legend
                            breaks = with(nutri_bal_ener_sec_df, c(min(GDP_per_capita), mean(GDP_per_capita), max(GDP_per_capita))), #choose breaks of variable
                            labels = c("Low", "Medium", "High"), #label
                            low = "pink",  #color of lowest value
                            high = "red"  #color of highest value
                            
)

#ener resources to Food production variability
########################################################################################
###control check for direct link with subset countries
###mute energy sector variables
food_prod_var_ener_sec_df<-ener_food_df %>%
  dplyr::select("Per_capita_food_production_variability",
                #energy res
                # "Total_fossil_fuel_production", "Total_non_fossil_fuel_production", 
                # "import_export_difference", 
                # "Fossil_fuel_reserves_Total", 
                # "Fossil_fuel_reserves_Natural_Gas_share", 
                # "Fossil_fuel_reserves_Oil_share", 
                #general capacity
                "GDP_per_capita", 
                "Education_Index", 
                "PC1_GI", "PC2_GI",
                "Percentage_of_rural_population",
                #food res specific human capacity
                "Value_of_food_imports_over_total_merchandise_exports",
                "Cereal_import_dependency_ratio",
                "Rail_lines_density",
                "Percent_of_arable_land_equipped_for_irrigation",
                #food res
                "PC1_land_res",
                "PC2_land_res")


E_to_F_ener_res_indpt_var <- colnames(food_prod_var_ener_sec_df)
num_E_to_F_ener_res_indpt_var<- ncol(food_prod_var_ener_sec_df)-1


result_food_prod_var_ener_sec_df<-cv_step_reg(data_input =  food_prod_var_ener_sec_df, 
                                              num_indpt_var = num_E_to_F_ener_res_indpt_var, 
                                              names_var = E_to_F_ener_res_indpt_var)
model_control_lm_food_prod_var_ener_sec<- lm(Per_capita_food_production_variability~PC2_GI, 
                                     data = food_prod_var_ener_sec_df)
summary(model_control_lm_food_prod_var_ener_sec)
########################################################################################
food_prod_var_ener_sec_df<-ener_food_df %>%
  dplyr::select("Per_capita_food_production_variability",
                #energy res
                "Total_fossil_fuel_production", "Total_non_fossil_fuel_production", 
                "import_export_difference", 
                "Fossil_fuel_reserves_Total", 
                "Fossil_fuel_reserves_Natural_Gas_share", 
                "Fossil_fuel_reserves_Oil_share", 
                #general capacity
                "GDP_per_capita", 
                "Education_Index", 
                "PC1_GI", "PC2_GI",
                "Percentage_of_rural_population",
                #food res specific human capacity
                "Value_of_food_imports_over_total_merchandise_exports",
                "Cereal_import_dependency_ratio",
                "Rail_lines_density",
                "Percent_of_arable_land_equipped_for_irrigation",
                #food res
                "PC1_land_res",
                "PC2_land_res")


E_to_F_ener_res_indpt_var <- colnames(food_prod_var_ener_sec_df)
num_E_to_F_ener_res_indpt_var<- ncol(food_prod_var_ener_sec_df)-1

## CVSRA result for food production variability E-F
result_food_prod_var_ener_sec_df<-cv_step_reg(data_input =  food_prod_var_ener_sec_df, 
                                          num_indpt_var = num_E_to_F_ener_res_indpt_var, 
                                          names_var = E_to_F_ener_res_indpt_var)
model_lm_food_prod_var_ener_sec<- lm(Per_capita_food_production_variability~Fossil_fuel_reserves_Oil_share, 
                                     data = food_prod_var_ener_sec_df)
summary(model_lm_food_prod_var_ener_sec)

##Check AIC 
dfaic = food_prod_var_ener_sec_df
dpt_var = "Per_capita_food_production_variability"
model.min = lm(paste(dpt_var,"~",1), data = dfaic)
model.full = lm(paste(dpt_var,"~","."), data = dfaic)
test1 = stepAIC(model.min, direction = "forward", 
                scope=list(upper=model.full,lower=model.min),
                trace = FALSE)
test1$anova

ggpairs(food_prod_var_ener_sec_df) 

pred_food_prod_var_ener_sec<-predict(model_lm_food_prod_var_ener_sec, newdata =  food_prod_var_ener_sec_df)

p7.2<-ggplot(food_prod_var_ener_sec_df, aes( x=Per_capita_food_production_variability, y=pred_food_prod_var_ener_sec))+
  geom_point(size=8, aes(color = Fossil_fuel_reserves_Oil_share))+
  # geom_point(color= "brown", aes(size = GDP_per_capita))+
  labs(x="Per Capita Food Production Variability (I$/person)",y= "Model Estimation (I$/person)")+
  geom_abline(slope = 1,color="red",linetype=2)+
  scale_x_continuous(limits = c(0, 20))+
  scale_y_continuous(limits = c(0, 20))+
  theme_bw()+
  theme(axis.title.x = element_text(size=30),
        axis.title.y = element_text(size = 30),
        axis.text.x = element_text(color="#993333", size=14),
        axis.text.y = element_text(color="#993333", size=14))+
  theme(legend.title = element_text(colour="black", size=10, face="bold"),
        legend.text = element_text(size =5))+
  geom_text_repel(aes(Per_capita_food_production_variability, pred_food_prod_var_ener_sec, label = row.names(food_prod_var_ener_sec_df)), show.legend = FALSE)

p7.2 + scale_color_continuous(name="Share of Oil in Total Fossil Fuel Reserve", #name of legend
                            breaks = with(food_prod_var_ener_sec_df, c(min(Fossil_fuel_reserves_Oil_share), mean(Fossil_fuel_reserves_Oil_share), max(Fossil_fuel_reserves_Oil_share))), #choose breaks of variable
                            labels = c("Low", "Medium", "High"), #label
                            low = "pink",  #color of lowest value
                            high = "red"  #color of highest value
                            
)


p7.3<-ggplot(food_prod_var_ener_sec_df, aes( x=Fossil_fuel_reserves_Oil_share, y=Per_capita_food_production_variability))+
  geom_point(size=8, aes(color = Fossil_fuel_reserves_Oil_share))+
  # geom_point(color= "brown", aes(size = GDP_per_capita))+
  labs(x="Share of Oil in Total Fossil Fuel Reserve (%)",y="Per Capita Food Production Variability (I$/person)")+
  geom_smooth(method='lm',formula=y~x, se=TRUE)+
  # scale_x_continuous(limits = c(-3, 3))+
  # scale_y_continuous(limits = c(-3, 3))+
  theme_bw()+
  theme(axis.title.x = element_text(size=30),
        axis.title.y = element_text(size = 30),
        axis.text.x = element_text(color="#993333", size=14),
        axis.text.y = element_text(color="#993333", size=14))+
  theme(legend.title = element_text(colour="black", size=10, face="bold"),
        legend.text = element_text(size =5))+
  geom_text_repel(aes(Fossil_fuel_reserves_Oil_share, Per_capita_food_production_variability, label = row.names(food_prod_var_ener_sec_df)), show.legend = FALSE)

p7.3 + scale_color_continuous(name="Share of Oil in Total Fossil Fuel Reserve", #name of legend
                              breaks = with(food_prod_var_ener_sec_df, c(min(Fossil_fuel_reserves_Oil_share), mean(Fossil_fuel_reserves_Oil_share), max(Fossil_fuel_reserves_Oil_share))), #choose breaks of variable
                              labels = c("Low", "Medium", "High"), #label
                              low = "pink",  #color of lowest value
                              high = "red"  #color of highest value
                              
)



#Energy resources to Food supply variability
########################################################################################
###control check for direct link with subset countries
###mute energy sector variables
food_spl_var_ener_sec_df<-ener_food_df %>%
  dplyr::select("Per_capita_food_supply_variability",
                # #energy res
                # "Total_fossil_fuel_production", "Total_non_fossil_fuel_production", 
                # "import_export_difference", 
                # "Fossil_fuel_reserves_Total", 
                # "Fossil_fuel_reserves_Natural_Gas_share", 
                # "Fossil_fuel_reserves_Oil_share", 
                #general capacity
                "GDP_per_capita", 
                "Education_Index", 
                "PC1_GI", "PC2_GI",
                "Percentage_of_rural_population",
                #food res specific human capacity
                "Value_of_food_imports_over_total_merchandise_exports",
                "Cereal_import_dependency_ratio",
                "Rail_lines_density",
                "Percent_of_arable_land_equipped_for_irrigation",
                #food res
                "PC1_land_res",
                "PC2_land_res")


E_to_F_ener_res_indpt_var <- colnames(food_spl_var_ener_sec_df)
num_E_to_F_ener_res_indpt_var<- ncol(food_spl_var_ener_sec_df)-1


result_food_spl_var_ener_sec_df<-cv_step_reg(data_input =  food_spl_var_ener_sec_df, 
                                             num_indpt_var = num_E_to_F_ener_res_indpt_var, 
                                             names_var = E_to_F_ener_res_indpt_var)
########################################################################################


food_spl_var_ener_sec_df<-ener_food_df %>%
  dplyr::select("Per_capita_food_supply_variability",
                #energy res
                "Total_fossil_fuel_production", "Total_non_fossil_fuel_production", 
                "import_export_difference", 
                "Fossil_fuel_reserves_Total", 
                "Fossil_fuel_reserves_Natural_Gas_share", 
                "Fossil_fuel_reserves_Oil_share", 
                #general capacity
                "GDP_per_capita", 
                "Education_Index", 
                "PC1_GI", "PC2_GI",
                "Percentage_of_rural_population",
                #food res specific human capacity
                "Value_of_food_imports_over_total_merchandise_exports",
                "Cereal_import_dependency_ratio",
                "Rail_lines_density",
                "Percent_of_arable_land_equipped_for_irrigation",
                #food res
                "PC1_land_res",
                "PC2_land_res")


E_to_F_ener_res_indpt_var <- colnames(food_spl_var_ener_sec_df)
num_E_to_F_ener_res_indpt_var<- ncol(food_spl_var_ener_sec_df)-1

## CVSRA result: food supply variability no clear relationship
result_food_spl_var_ener_sec_df<-cv_step_reg(data_input =  food_spl_var_ener_sec_df, 
                                              num_indpt_var = num_E_to_F_ener_res_indpt_var, 
                                              names_var = E_to_F_ener_res_indpt_var)
model_lm_food_spl_var_ener_sec<- lm(Per_capita_food_supply_variability~import_export_difference, 
                                     data = food_spl_var_ener_sec_df)
summary(model_lm_food_spl_var_ener_sec)

##Check AIC 
dfaic = food_spl_var_ener_sec_df
dpt_var = "Per_capita_food_supply_variability"
model.min = lm(paste(dpt_var,"~",1), data = dfaic)
model.full = lm(paste(dpt_var,"~","."), data = dfaic)
test1 = stepAIC(model.min, direction = "forward", 
                scope=list(upper=model.full,lower=model.min),
                trace = FALSE)
test1$anova

ggpairs(food_spl_var_ener_sec_df) 

#####################

####Food resources to Energy Service Index:
energy_service_pp<-food_energy_df[yes_list_food_energy,c("Energy supply per capita",
                                 "Electricity consumption per capita")]
# ggpairs(energy_service_pp)
pc_energy_service_pp<-prcomp(energy_service_pp, scale= TRUE, center = TRUE)
summary(pc_energy_service_pp)
pc_energy_service_pp
biplot(pc_energy_service_pp)
ener_svs_pp_score<-pc_energy_service_pp$x
colnames(ener_svs_pp_score)<-c("PC1_energy_svs_pp", "PC2_energy_svs_pp")
ener_svs_index<-ener_svs_pp_score[,1]

ener_svs_food_res<- food_energy_df %>%
  dplyr::select( "GDP per capita",
                "Education Index", 
                "Percentage of rural population",
                "Cereal import dependency ratio",
                "Value of food imports over total merchandise exports", 
                "Rail lines density", 
                "Percent of arable land equipped for irrigation")
# ener_svs_food_res[,c("Cereal import dependency ratio")]<-ener_svs_food_res[,c("Cereal import dependency ratio")] + abs(ener_svs_food_res[c("Zambia"),c("Cereal import dependency ratio")])
###The GDP per capita is been log10 transformed. 
ener_svs_food_res[,c("GDP per capita")]<-log10(ener_svs_food_res[,c("GDP per capita")])
#The original distribution of some food related human capacity variables are skewed to the left. 
# These variables: "Value of food imports over total merchandise exports", "Rail lines density", and "Percent of arable land equipped for irrigation" are added by 0.01 and then been log10 transformed.
ener_svs_food_res[,(ncol(ener_svs_food_res)-2):ncol(ener_svs_food_res)]<-
  log10(ener_svs_food_res[,(ncol(ener_svs_food_res)-2):ncol(ener_svs_food_res)]+0.01)

GI_ener_food<-ener_food_df[,c("PC1_GI", "PC2_GI")]

ener_svs_food_res_df<-cbind.data.frame(ener_svs_index,
                                       GI_ener_food,
                                       pc_land_avail_scores,
                                       ener_svs_food_res)
# ggpairs(ener_svs_food_res_df)



for(i in 1:ncol(ener_svs_food_res_df)){
  land_res_indpt_var[i]<- gsub('([[:punct:]])|\\s+','_',land_res_indpt_var[i])
}
colnames(ener_svs_food_res_df)<-land_res_indpt_var

##bind energy res data into this as well 2:7 is the energy variables
ener_svs_food_res_df<-cbind.data.frame(ener_svs_food_res_df,food_spl_var_ener_sec_df[,2:7])
########################################################################################
###control check for direct link with subset countries
###mute food sector variables
ener_svs_food_res_df<-ener_svs_food_res_df %>%
  dplyr::select(-Value_of_food_imports_over_total_merchandise_exports,
                -Cereal_import_dependency_ratio,
                -Rail_lines_density,
                -Percent_of_arable_land_equipped_for_irrigation,
                
                -PC1_land_res,
                -PC2_land_res)
land_res_indpt_var <- colnames(ener_svs_food_res_df)
num_land_res_indpt_var<- ncol(ener_svs_food_res_df)-1

result_ener_svs_food_sec<-cv_step_reg(data_input =  ener_svs_food_res_df, 
                                      num_indpt_var = num_land_res_indpt_var,
                                      names_var = land_res_indpt_var)
model_control_lm_energy_svs_food_res<- lm(ener_svs_index~
                                            Education_Index,
                                            # GDP_per_capita, 
                                  data = ener_svs_food_res_df)
summary(model_control_lm_energy_svs_food_res)
########################################################################################


land_res_indpt_var <- colnames(ener_svs_food_res_df)
num_land_res_indpt_var<- ncol(ener_svs_food_res_df)-1

for(i in 1:ncol(ener_svs_food_res_df)){
  land_res_indpt_var[i]<- gsub('([[:punct:]])|\\s+','_',land_res_indpt_var[i])
}
colnames(ener_svs_food_res_df)<-land_res_indpt_var

## CVSRA result: Energy service E-F
result_ener_svs_food_sec<-cv_step_reg(data_input =  ener_svs_food_res_df, 
                                      num_indpt_var = num_land_res_indpt_var,
                                      names_var = land_res_indpt_var)
model_lm_energy_svs_food_res<- lm(ener_svs_index~Education_Index+Rail_lines_density, 
                                  data = ener_svs_food_res_df)
summary(model_lm_energy_svs_food_res)

##Check AIC 
dfaic = ener_svs_food_res_df
dpt_var = "ener_svs_index"
model.min = lm(paste(dpt_var,"~",1), data = dfaic)
model.full = lm(paste(dpt_var,"~","."), data = dfaic)
test1 = stepAIC(model.min, direction = "forward", 
                scope=list(upper=model.full,lower=model.min),
                trace = FALSE)
test1$anova

# glm.fit_og=glm(ener_svs_index~Education_Index+Rail_lines_density,
#                data=food_spl_var_ener_sec_df)
# cv.error_test1_og=cv.glm(food_spl_var_ener_sec_df,glm.fit_og)$delta[1] 
# cv.error_test1_og
# AIC(glm.fit_og, k = 2)
# glm.fit_aic=glm(Per_capita_food_supply_variability ~ Fossil_fuel_reserves_Oil_share + 
#                   Percent_of_arable_land_equipped_for_irrigation,
#                 data=food_spl_var_ener_sec_df)
# cv.error_test1_aic=cv.glm(food_spl_var_ener_sec_df,glm.fit_aic)$delta[1] 
# cv.error_test1_aic
# AIC(glm.fit_aic, k = 2)


pred_ener_svs_food_res<-predict(model_lm_energy_svs_food_res, newdata = ener_svs_food_res_df)

p8<-ggplot(ener_svs_food_res_df, aes( x=ener_svs_index, y=pred_ener_svs_food_res))+
  geom_point(size=8, aes(color = Education_Index))+
  # geom_point(color= "brown", aes(size = GDP_per_capita))+
  labs(x="Energy Service Index (PC1)",y= "Model Estimation (PC1)")+
  geom_abline(slope = 1,color="red",linetype=2)+
  scale_x_continuous(limits = c(-1.5, 5))+
  scale_y_continuous(limits = c(-1.5, 5))+
  theme_bw()+
  theme(axis.title.x = element_text(size=30),
        axis.title.y = element_text(size = 30),
        axis.text.x = element_text(color="#993333", size=14),
        axis.text.y = element_text(color="#993333", size=14))+
  theme(legend.title = element_text(colour="black", size=10, face="bold"),
        legend.text = element_text(size =5))+
  geom_text_repel(aes(ener_svs_index, pred_ener_svs_food_res, 
                      label = row.names(ener_svs_food_res_df)), show.legend = FALSE)

p8 + scale_color_continuous(name="Education Index", #name of legend
                              breaks = with(ener_svs_food_res_df, c(min(Education_Index), mean(Education_Index), max(Education_Index))), #choose breaks of variable
                              labels = c("Low", "Medium", "High"), #label
                              low = "pink",  #color of lowest value
                              high = "red"  #color of highest value
                              
)

#######3D plot
#Graph Resolution (more important for more complex shapes)
graph_reso <- 0.05

#Setup Axis
axis_x <- seq(min(ener_svs_food_res_df$Education_Index), 
              max(ener_svs_food_res_df$Education_Index), by = graph_reso)
axis_y <- seq(min(ener_svs_food_res_df$Rail_lines_density), 
              max(ener_svs_food_res_df$Rail_lines_density), by = graph_reso)

#Sample points
model8.1_surface <- expand.grid(Education_Index = axis_x, Rail_lines_density = axis_y, KEEP.OUT.ATTRS = F)
model8.1_surface$ener_svs_index <- predict.lm(model_lm_energy_svs_food_res, newdata = model8.1_surface)
model8.1_surface <- acast(model8.1_surface, Rail_lines_density ~ Education_Index , value.var = "ener_svs_index") #y ~ x

# hcolors=c("red","blue","green")[my_df$Species]
p8.1_3d <- plot_ly(ener_svs_food_res_df, 
                   x = ~Education_Index, 
                   y = ~Rail_lines_density, 
                   z = ~ener_svs_index,
                   text = row.names(ener_svs_food_res_df),
                   type = "scatter3d"
                   ,
                   mode = "markers"
                   # ,
                   # marker = list(color = hcolors)
)%>%
  layout(
    scene = list(
      xaxis = list(title = "Education Index"),
      yaxis = list(title = "Rail Lines Density"),
      zaxis = list(title = "Energy Service Index")
    ))

p8.1_3d <- add_trace(p = p8.1_3d,
                     z = model8.1_surface,
                     x = axis_x,
                     y = axis_y,
                     type = "surface",
                     legend = FALSE
)
p8.1_3d


######water and food services to food related health
food_pca_health_proxy<-food_res_svs_df[yes_list_water_food,] %>%
  dplyr::select(
    # "Prevalence of undernourishment",
    "Percentage of children under 5 years of age who are stunted",
    "Percentage of children under 5 years of age affected by wasting",
    "Percentage of children under 5 years of age who are underweight",
    "Percentage of children under 5 years of age who are overweight"
  )
pc_food_heal<-prcomp(food_pca_health_proxy)#these value do not need to be scaled since they are all percentages
summary(pc_food_heal)
pc_food_heal
biplot(pc_food_heal, xlim= c(-0.4,0.8),ylim= c(-0.5, 0.5))
malnutrition_index<-pc_food_heal$x
colnames(malnutrition_index)<-c("PC1_U5nourish","PC2_U5nourish","PC3_U5nourish","PC4_U5nourish")

malnutrition_index<- malnutrition_index[,1]
mal_ind_water_svs_df<- cbind.data.frame(
    food_svs_health_df,
    pca_ac_dw_df[yes_list_water_food,1],
    pca_ac_sani_df[yes_list_water_food,1]
)

# colnames(mal_ind_water_svs_df)[ncol(mal_ind_water_svs_df)-4]<-c(names(nutri_bal_water_sec_df)[1])
# colnames(mal_ind_water_svs_df)[ncol(mal_ind_water_svs_df)-3]<-c(names(food_spl_var_water_sec_df)[1])
# colnames(mal_ind_water_svs_df)[ncol(mal_ind_water_svs_df)-2]<-c(names(food_prod_var_water_sec_df)[1])
colnames(mal_ind_water_svs_df)[ncol(mal_ind_water_svs_df)-1]<-c("Total_population_with_access_to_safe_drinking_water")
colnames(mal_ind_water_svs_df)[ncol(mal_ind_water_svs_df)]<-c("Population_with_access_to_improved_sanitation")

water_svs_indpt_var <- colnames(mal_ind_water_svs_df)
num_water_svs_indpt_var<- ncol(mal_ind_water_svs_df)-1

## CVSRA result: Malnutrition W-F service to health
result_ener_svs_food_sec<-cv_step_reg(data_input =  mal_ind_water_svs_df, 
                                      num_indpt_var = num_water_svs_indpt_var,
                                      names_var = water_svs_indpt_var)
model_lm_water_svs_food_health<- lm(malnutrition_index~GDP_per_capita+Rail_lines_density, 
                                  data = mal_ind_water_svs_df)
summary(model_lm_water_svs_food_health)

##Check AIC 
dfaic = mal_ind_water_svs_df
dpt_var = "malnutrition_index"
model.min = lm(paste(dpt_var,"~",1), data = dfaic)
model.full = lm(paste(dpt_var,"~","."), data = dfaic)
test1 = stepAIC(model.min, direction = "forward", 
                scope=list(upper=model.full,lower=model.min),
                trace = FALSE)
test1$anova

# ggpairs(mal_ind_water_svs_df)


########################################################################################
###control check for direct link with subset countries
###mute food sector variables
#this is using food svs variables only for food health
mal_ind_water_svs_df<- cbind.data.frame(
  food_svs_health_df
)

# colnames(mal_ind_water_svs_df)[ncol(mal_ind_water_svs_df)-4]<-c(names(nutri_bal_water_sec_df)[1])
# colnames(mal_ind_water_svs_df)[ncol(mal_ind_water_svs_df)-3]<-c(names(food_spl_var_water_sec_df)[1])
# colnames(mal_ind_water_svs_df)[ncol(mal_ind_water_svs_df)-2]<-c(names(food_prod_var_water_sec_df)[1])
colnames(mal_ind_water_svs_df)[ncol(mal_ind_water_svs_df)-1]<-c("Total_population_with_access_to_safe_drinking_water")
colnames(mal_ind_water_svs_df)[ncol(mal_ind_water_svs_df)]<-c("Population_with_access_to_improved_sanitation")

water_svs_indpt_var <- colnames(mal_ind_water_svs_df)
num_water_svs_indpt_var<- ncol(mal_ind_water_svs_df)-1


result_ener_svs_food_sec<-cv_step_reg(data_input =  mal_ind_water_svs_df, 
                                      num_indpt_var = num_water_svs_indpt_var,
                                      names_var = water_svs_indpt_var)
model_lm_water_svs_food_health<- lm(malnutrition_index~GDP_per_capita, 
                                    data = mal_ind_water_svs_df)
summary(model_lm_water_svs_food_health)

# ggpairs(mal_ind_water_svs_df)

########################################################################################
###control check for direct link with subset countries
###mute food sector variables
#this is using water svs variables only for water health
diarrhea_water_svs_df<- mal_ind_water_svs_df %>%
  dplyr::select(
                # "malnutrition_index", 
                "GDP_per_capita", 
                "Education_Index", 
                "PC1_GI", "PC2_GI", 
                "Percentage_of_rural_population", 
                "Total_population_with_access_to_safe_drinking_water", 
                "Population_with_access_to_improved_sanitation"
  )

Diarrhea_as_a_cause_of_death_for_children_under_5<-diarrhea_access_pc_GI[yes_list_water_food,1]
diarrhea_water_svs_df<-cbind.data.frame(Diarrhea_as_a_cause_of_death_for_children_under_5,diarrhea_water_svs_df)
# colnames(mal_ind_water_svs_df)[ncol(mal_ind_water_svs_df)-4]<-c(names(nutri_bal_water_sec_df)[1])
# colnames(mal_ind_water_svs_df)[ncol(mal_ind_water_svs_df)-3]<-c(names(food_spl_var_water_sec_df)[1])
# colnames(mal_ind_water_svs_df)[ncol(mal_ind_water_svs_df)-2]<-c(names(food_prod_var_water_sec_df)[1])
# colnames(mal_ind_water_svs_df)[ncol(mal_ind_water_svs_df)-1]<-c("Total_population_with_access_to_safe_drinking_water")
# colnames(mal_ind_water_svs_df)[ncol(mal_ind_water_svs_df)]<-c("Population_with_access_to_improved_sanitation")

water_svs_indpt_var <- colnames(diarrhea_water_svs_df)
num_water_svs_indpt_var<- ncol(diarrhea_water_svs_df)-1


result_diarrhea_water_svs<-cv_step_reg(data_input =  diarrhea_water_svs_df, 
                                      num_indpt_var = num_water_svs_indpt_var,
                                      names_var = water_svs_indpt_var)
model_control_lm_water_svs_food_health<- lm(Diarrhea_as_a_cause_of_death_for_children_under_5~PC1_GI, 
                                    data = diarrhea_water_svs_df)
summary(model_control_lm_water_svs_food_health)



########################################################################################



pred_malnutrition_index<-predict(model_lm_water_svs_food_health, newdata = mal_ind_water_svs_df)

p9<-ggplot(mal_ind_water_svs_df, aes( x=malnutrition_index, y=pred_malnutrition_index))+
  geom_point(size=8, aes(color = GDP_per_capita))+
  # geom_point(color= "brown", aes(size = GDP_per_capita))+
  labs(x="Young Age Malnutrition Index (PC1)",y= "Model Estimation (PC1)")+
  geom_abline(slope = 1,color="red",linetype=2)+
  scale_x_continuous(limits = c(-30, 20))+
  scale_y_continuous(limits = c(-30, 20))+
  theme_bw()+
  theme(axis.title.x = element_text(size=30),
        axis.title.y = element_text(size = 30),
        axis.text.x = element_text(color="#993333", size=14),
        axis.text.y = element_text(color="#993333", size=14))+
  theme(legend.title = element_text(colour="black", size=10, face="bold"),
        legend.text = element_text(size =5))+
  geom_text_repel(aes(malnutrition_index, pred_malnutrition_index, label = row.names(mal_ind_water_svs_df)), show.legend = FALSE)

p9 + scale_color_continuous(name="GDP_per_capita", #name of legend
                              breaks = with(mal_ind_water_svs_df, c(min(GDP_per_capita), mean(GDP_per_capita), max(GDP_per_capita))), #choose breaks of variable
                              labels = c("Low", "Medium", "High"), #label
                              low = "pink",  #color of lowest value
                              high = "red"  #color of highest value
                              
)


p9.1<-ggplot(mal_ind_water_svs_df, aes( x=GDP_per_capita, y=malnutrition_index))+
  geom_point(size=8, aes(color = GDP_per_capita))+
  # geom_point(color= "brown", aes(size = GDP_per_capita))+
  labs(x="GDP per capita (log 10)",y="Young Age Malnutrition Index (PC1)")+
  geom_smooth(method='lm',formula=y~x, se=TRUE)+
  # scale_x_continuous(limits = c(-3, 3))+
  # scale_y_continuous(limits = c(-3, 3))+
  theme_bw()+
  theme(axis.title.x = element_text(size=30),
        axis.title.y = element_text(size = 30),
        axis.text.x = element_text(color="#993333", size=14),
        axis.text.y = element_text(color="#993333", size=14))+
  theme(legend.title = element_text(colour="black", size=10, face="bold"),
        legend.text = element_text(size =5))+
  geom_text_repel(aes(GDP_per_capita, malnutrition_index, label = row.names(mal_ind_water_svs_df)), show.legend = FALSE)

p9.1 + scale_color_continuous(name="GDP_per_capita", #name of legend
                              breaks = with(mal_ind_water_svs_df, c(min(GDP_per_capita), mean(GDP_per_capita), max(GDP_per_capita))), #choose breaks of variable
                              labels = c("Low", "Medium", "High"), #label
                              low = "pink",  #color of lowest value
                              high = "red"  #color of highest value
                              
)

####
#Food SVS to water health: diarrhea

food_svs_diar_df<-cbind.data.frame(
                                   diarrhea_access_pc_GI[yes_list_water_food,1],
                                   food_svs_health_df[,-1],
                                   mal_ind_water_svs_df[,c("Total_population_with_access_to_safe_drinking_water", 
                                                            "Population_with_access_to_improved_sanitation")]
                                   )
colnames(food_svs_diar_df)[1]<-c("Diarrhea_as_a_cause_of_death_for_children_under_5")
food_svs_indpt_var <- colnames(food_svs_diar_df)
num_food_svs_indpt_var<- ncol(food_svs_diar_df)-1


result_diar_food_svs<-cv_step_reg(data_input =  food_svs_diar_df, 
                                      num_indpt_var = num_food_svs_indpt_var,
                                      names_var = food_svs_indpt_var)
model_lm_water_svs_food_health<- lm(Diarrhea_as_a_cause_of_death_for_children_under_5~PC1_GI, 
                                    data = food_svs_diar_df)
summary(model_lm_water_svs_food_health)

##Check AIC 
dfaic = food_svs_diar_df
dpt_var = "Diarrhea_as_a_cause_of_death_for_children_under_5"
model.min = lm(paste(dpt_var,"~",1), data = dfaic)
model.full = lm(paste(dpt_var,"~","."), data = dfaic)
test1 = stepAIC(model.min, direction = "forward", 
                scope=list(upper=model.full,lower=model.min),
                trace = FALSE)
test1$anova

ggpairs(food_svs_diar_df)


#### 
#Energy services to food related health: malnutrition index
food_pca_health_proxy<-food_energy_df[yes_list_food_energy,] %>%
  dplyr::select(
    # "Prevalence of undernourishment",
    "Percentage of children under 5 years of age who are stunted",
    "Percentage of children under 5 years of age affected by wasting",
    "Percentage of children under 5 years of age who are underweight",
    "Percentage of children under 5 years of age who are overweight"
  )
pc_food_heal<-prcomp(food_pca_health_proxy)#these value do not need to be scaled since they are all percentages
summary(pc_food_heal)
pc_food_heal
biplot(pc_food_heal, xlim= c(-0.4,0.8),ylim= c(-0.5, 0.5))
malnutrition_index<-pc_food_heal$x
colnames(malnutrition_index)<-c("PC1_U5nourish","PC2_U5nourish","PC3_U5nourish","PC4_U5nourish")
malnutrition_index<- malnutrition_index[,1]#first PC captures 63% of the variations


########################################################################################
###control check for direct link with subset countries
#this is using food svs variables only for food health
food_svs_health_df_sub<-mal_ind_ener_svs_df %>%
  dplyr::select("malnutrition_index", "food_availability_index", "GDP_per_capita", 
                "Education_Index", "PC1_GI", "PC2_GI", "Percentage_of_rural_population", 
                "Value_of_food_imports_over_total_merchandise_exports", "Cereal_import_dependency_ratio", 
                "Rail_lines_density", "Percent_of_arable_land_equipped_for_irrigation", 
                "PC1_land_res", "PC2_land_res",  
                "Per_capita_food_supply_variability", "nutri_bal_index", "Per_capita_food_production_variability")
# ggpairs(food_svs_health_df_sub)

result_malnutrition_index_food_svs<-cv_step_reg(data_input = food_svs_health_df_sub,names_var = colnames(food_svs_health_df_sub), num_indpt_var = ncol(food_svs_health_df_sub)-1)
model_control_lm_malnutrition_index_food_svs<-lm(malnutrition_index~nutri_bal_index + Per_capita_food_supply_variability, data = food_svs_health_df_sub)
summary(model_control_lm_malnutrition_index_food_svs)


########################################################################################
###control check for direct link with subset countries
#this is using ener svs variables only for ener health
Causes_of_Death_attributable_to_Air_Pollt<-ener_prod_air_pol[yes_list_food_energy,1]
ener_svs_air_pol_sub<-cbind.data.frame(Causes_of_Death_attributable_to_Air_Pollt,
                                        ener_svs_index,
                                        mal_ind_ener_svs_df[,c( "GDP_per_capita", 
                                                                "Education_Index", 
                                                                "PC1_GI", "PC2_GI", 
                                                                "Percentage_of_rural_population", 
                                                                "import_export_difference"
                                                                    )]
                                        )


result_air_pol_ener_res<-cv_step_reg(data_input =  ener_svs_air_pol_sub, num_indpt_var = ncol(ener_svs_air_pol_sub)-1, names_var = colnames(ener_svs_air_pol_sub))
model_control_lm_energy_svs_ener_res<- lm(Causes_of_Death_attributable_to_Air_Pollt~Percentage_of_rural_population, data = ener_svs_air_pol_sub)
summary(model_control_lm_energy_svs_ener_res)
########################################################################################

mal_ind_ener_svs_df<-cbind.data.frame(malnutrition_index,
                                      food_avail_ener_sec_df,
                                      ener_svs_index,
                                      food_spl_var_ener_sec_df[,c("import_export_difference","Per_capita_food_supply_variability")],
                                      nutri_bal_ener_sec_df[,1],
                                      food_prod_var_ener_sec_df[,1]
                                        )

colnames(mal_ind_ener_svs_df)[(ncol(mal_ind_ener_svs_df)-1):ncol(mal_ind_ener_svs_df)]<-c("nutri_bal_index","Per_capita_food_production_variability")

ener_svs_indpt_var <- colnames(mal_ind_ener_svs_df)
num_ener_svs_indpt_var<- ncol(mal_ind_ener_svs_df)-1

## CVSRA result: U5 Malnutrition  E-F
result_ener_svs_food_health<-cv_step_reg(data_input =  mal_ind_ener_svs_df, 
                                  num_indpt_var = num_ener_svs_indpt_var,
                                  names_var = ener_svs_indpt_var)
model_lm_ener_svs_food_health<- lm(malnutrition_index~nutri_bal_index+Per_capita_food_supply_variability, 
                                    data = mal_ind_ener_svs_df)
summary(model_lm_ener_svs_food_health)

##Check AIC 
dfaic = mal_ind_ener_svs_df
dpt_var = "malnutrition_index"
model.min = lm(paste(dpt_var,"~",1), data = dfaic)
model.full = lm(paste(dpt_var,"~","."), data = dfaic)
test1 = stepAIC(model.min, direction = "forward", 
                scope=list(upper=model.full,lower=model.min),
                trace = FALSE)
test1$anova

ggpairs(mal_ind_ener_svs_df)

### air pollution Energy-Food service to health
air_pol_food_svs_df<-cbind.data.frame(Causes_of_Death_attributable_to_Air_Pollt,
                                      mal_ind_ener_svs_df[,-1])

## CVSRA result: air pollution E-F
result_air_pol_food_res<-cv_step_reg(data_input =  air_pol_food_svs_df, num_indpt_var = ncol(air_pol_food_svs_df)-1, names_var = colnames(air_pol_food_svs_df))
model_control_lm_energy_svs_food_res<- lm(Causes_of_Death_attributable_to_Air_Pollt~food_availability_index, data = air_pol_food_svs_df)
summary(model_control_lm_energy_svs_food_res)

##Check AIC 
dfaic = air_pol_food_svs_df
dpt_var = "Causes_of_Death_attributable_to_Air_Pollt"
model.min = lm(paste(dpt_var,"~",1), data = dfaic)
model.full = lm(paste(dpt_var,"~","."), data = dfaic)
test1 = stepAIC(model.min, direction = "forward", 
                scope=list(upper=model.full,lower=model.min),
                trace = FALSE)
test1$anova

AIC(lm(formula = Causes_of_Death_attributable_to_Air_Pollt ~ Percentage_of_rural_population, 
       data = ener_svs_air_pol_sub))

####
#Energy services to water related health: diarhea 

ener_svs_df_sub<-ener_svs_water_sec_df %>% dplyr:: select(
  "PC1_energy_svs_pp", "GDP_per_capita", "Education_Index", "PC1_GI", 
  "PC2_GI", "Percentage_of_rural_population",  "import_export_difference"
)
diar_ener_svs_df<-cbind.data.frame(diarrhea_access_pc_GI[yes_list_water_energy,1:3],
                                      ener_svs_df_sub)
colnames(diar_ener_svs_df)[1]<-c("Diarrhea_as_a_cause_of_death_for_children_under_5")

ener_svs_indpt_var <- colnames(diar_ener_svs_df)
num_ener_svs_indpt_var<- ncol(diar_ener_svs_df)-1


result_ener_svs_food_health<-cv_step_reg(data_input =  diar_ener_svs_df, 
                                         num_indpt_var = num_ener_svs_indpt_var,
                                         names_var = ener_svs_indpt_var)
model_lm_ener_svs_water_health<- lm(Diarrhea_as_a_cause_of_death_for_children_under_5~PC1_GI, 
                                   data = diar_ener_svs_df)
summary(model_lm_ener_svs_water_health)
##Check AIC 
dfaic = diar_ener_svs_df
dpt_var = "Diarrhea_as_a_cause_of_death_for_children_under_5"
model.min = lm(paste(dpt_var,"~",1), data = dfaic)
model.full = lm(paste(dpt_var,"~","."), data = dfaic)
test1 = stepAIC(model.min, direction = "forward", 
                scope=list(upper=model.full,lower=model.min),
                trace = FALSE)
test1$anova

summary(model_lm_ener_svs_water_health)
########################################################################################
###control check for direct link with subset countries
#this is using ener svs variables only for ener health
diar_water_svs_df_sub<-diar_ener_svs_df %>%
  dplyr::select("Diarrhea_as_a_cause_of_death_for_children_under_5", "Total_population_with_access_to_safe_drinking_water", 
                "Population_with_access_to_improved_sanitation",  
                "GDP_per_capita", "Education_Index", "PC1_GI", "PC2_GI", "Percentage_of_rural_population" 
                )

water_svs_indpt_var <- colnames(diar_water_svs_df_sub)
num_water_svs_indpt_var<- ncol(diar_water_svs_df_sub)-1


result_diar_water_svs_sub<-cv_step_reg(data_input =  diar_water_svs_df_sub, 
                                         num_indpt_var = num_water_svs_indpt_var,
                                         names_var = water_svs_indpt_var)
model_control_lm_ener_svs_water_health<- lm(Diarrhea_as_a_cause_of_death_for_children_under_5~PC1_GI, 
                                    data = diar_water_svs_df_sub)
summary(model_control_lm_ener_svs_water_health)
########################################################################################


ggpairs(diar_ener_svs_df)


####health to mortality
health_morta_df <- data %>%
  # dplyr::rename(Parameter = Parameter.) %>% #change column name to exclude period
  #ac_dw,ppt_lt,tot_water_renew,var_interannual,var_sea,flood,drought,gdp,edu_ind,voi_ac,pol_sta,gov_ef,reg_qual,ru_law,con_corru,ru_percent
  dplyr::filter(Parameter. %in% c("Mortality Rate",
                                  "Diarrhea as a cause of death for children under 5",
                                  "Causes of Death attributable to Air Pollt",
                                  "Percentage of children under 5 years of age who are stunted",
                                  "Percentage of children under 5 years of age affected by wasting",
                                  "Percentage of children under 5 years of age who are underweight",
                                  "Percentage of children under 5 years of age who are overweight",
                                  "GDP per capita",
                                  "Education Index",
                                  "Voice and Accountability",
                                  "Political Stability and Absence of Violence/Terrorism",
                                  "Government Effectiveness",
                                  "Regulatory Quality",
                                  "Rule of Law",
                                  "Control of Corruption",
                                  "Rural Population",
                                  "Population")) %>%
  dplyr::select(-Burundi, -Comoros, -Eritrea, -Congo..Dem..Rep., -Equatorial.Guinea, -Seychelles, -Somalia, -South.Sudan, -Somalia,-China,-USA,-`Sri.Lanka`,-India,-Bangladesh)
row.names(health_morta_df)<-health_morta_df$Parameter.
health_morta_df<-health_morta_df[,-c(1,2,3,4)]

health_morta_df<- health_morta_df %>%
  dplyr::select(-Cabo.Verde,-Mauritius,-Cote.Dlvoire,-Sao.Tome.and.Principe) #emit countries not in Africa

#make dataframe numeric
for (i in 1:ncol(health_morta_df)){
  health_morta_df[,i]=as.numeric(as.character(health_morta_df[,i]))
}
#transpose to make df in row of countries and col of parameters
health_morta_df<-as.data.frame(t(health_morta_df))

malnutrition_var<-c("Percentage of children under 5 years of age who are stunted",
                    "Percentage of children under 5 years of age affected by wasting",
                    "Percentage of children under 5 years of age who are underweight",
                    "Percentage of children under 5 years of age who are overweight")

malnutrition_index<-prcomp(health_morta_df[,malnutrition_var],scale=FALSE, center = TRUE)
summary(malnutrition_index)
malnutrition_index<-malnutrition_index$x
malnutrition_index<-malnutrition_index[,1]
gov_var<-c(                                  "Voice and Accountability",
                                             "Political Stability and Absence of Violence/Terrorism",
                                             "Government Effectiveness",
                                             "Regulatory Quality",
                                             "Rule of Law",
                                             "Control of Corruption")
gov_index<-prcomp(health_morta_df[,gov_var],scale=FALSE, center = TRUE)
summary(gov_index)
gov_index<-gov_index$x
governance_index<-gov_index[,1]
political_stability_index<-gov_index[,2]
percent_rural<-health_morta_df$`Rural Population`/health_morta_df$Population*100

mortal_rate_health_df<-health_morta_df %>%
                                  dplyr:: select("Mortality Rate",
                                  "Diarrhea as a cause of death for children under 5",
                                  "Causes of Death attributable to Air Pollt",
                                  "GDP per capita",
                                  "Education Index"
                                ) %>%
  cbind.data.frame(malnutrition_index,governance_index,political_stability_index)

mortal_rate_health_df$`GDP per capita`<-log10(mortal_rate_health_df$`GDP per capita`)

ggpairs(mortal_rate_health_df)

health_indpt_var <- colnames(mortal_rate_health_df)
num_health_indpt_var<- ncol(mortal_rate_health_df)-1
for(i in 1:ncol(mortal_rate_health_df)){
  health_indpt_var[i]<- gsub('([[:punct:]])|\\s+','_',health_indpt_var[i])
}
colnames(mortal_rate_health_df)<-health_indpt_var

result_morta_rate_health<-cv_step_reg(data_input =  mortal_rate_health_df, 
                                      num_indpt_var = num_health_indpt_var,
                                      names_var = health_indpt_var)


lm_mort_rate_health<-lm(Mortality_Rate~Causes_of_Death_attributable_to_Air_Pollt+GDP_per_capita, 
                     data = mortal_rate_health_df)
summary(lm_mort_rate_health)



pred_mortal_rate<-predict(lm_mort_rate_health, newdata = mortal_rate_health_df)

p10<-ggplot(mortal_rate_health_df, aes( x=Mortality_Rate, y=pred_mortal_rate))+
  geom_point(size=8, aes(color = Causes_of_Death_attributable_to_Air_Pollt))+
  # geom_point(color= "brown", aes(size = GDP_per_capita))+
  labs(x="Crude Death Rate (Deaths per 1000 population)",y= "Model Estimation (Deaths per 1000 population)")+
  geom_abline(slope = 1,color="red",linetype=2)+
  scale_x_continuous(limits = c(5, 20))+
  scale_y_continuous(limits = c(5, 20))+
  theme_bw()+
  theme(axis.title.x = element_text(size=30),
        axis.title.y = element_text(size = 30),
        axis.text.x = element_text(color="#993333", size=14),
        axis.text.y = element_text(color="#993333", size=14))+
  theme(legend.title = element_text(colour="black", size=10, face="bold"),
        legend.text = element_text(size =5))+
  geom_text_repel(aes(Mortality_Rate, pred_mortal_rate, label = row.names(mortal_rate_health_df)), show.legend = FALSE)

p10 + scale_color_continuous(name="Death: due to Air Pol.", #name of legend
                            breaks = with(mortal_rate_health_df, c(min(Causes_of_Death_attributable_to_Air_Pollt), 
                                                                   mean(Causes_of_Death_attributable_to_Air_Pollt), 
                                                                   max(Causes_of_Death_attributable_to_Air_Pollt))), #choose breaks of variable
                            labels = c("Low", "Medium", "High"), #label
                            low = "pink",  #color of lowest value
                            high = "red"  #color of highest value
                            
)


#######3D plot
#Graph Resolution (more important for more complex shapes)
graph_reso <- 0.05

#Setup Axis
axis_x <- seq(min(mortal_rate_health_df$Causes_of_Death_attributable_to_Air_Pollt), 
              max(mortal_rate_health_df$Causes_of_Death_attributable_to_Air_Pollt), by = graph_reso)
axis_y <- seq(min(mortal_rate_health_df$GDP_per_capita), 
              max(mortal_rate_health_df$GDP_per_capita), by = graph_reso)

#Sample points
model10.1_surface <- expand.grid(Causes_of_Death_attributable_to_Air_Pollt = axis_x, GDP_per_capita = axis_y, 
                                 KEEP.OUT.ATTRS = F)
model10.1_surface$mortal_rate <- predict.lm(lm_mort_rate_health, newdata = model10.1_surface)
model10.1_surface <- acast(model10.1_surface, GDP_per_capita ~ Causes_of_Death_attributable_to_Air_Pollt , 
                           value.var = "mortal_rate") #y ~ x

# hcolors=c("red","blue","green")[my_df$Species]
p10.1_3d <- plot_ly(mortal_rate_health_df, 
                   x = ~Causes_of_Death_attributable_to_Air_Pollt, 
                   y = ~GDP_per_capita, 
                   z = ~Mortality_Rate,
                   text = row.names(mortal_rate_health_df),
                   type = "scatter3d"
                   ,
                   mode = "markers"
                   # ,
                   # marker = list(color = hcolors)
)%>%
  layout(
    scene = list(
      xaxis = list(title = "Death: Air Pollution"),
      yaxis = list(title = "GDP per cap"),
      zaxis = list(title = "Crude death Rate")
    ))

p10.1_3d <- add_trace(p = p10.1_3d,
                     z = model10.1_surface,
                     x = axis_x,
                     y = axis_y,
                     type = "surface",
                     legend = FALSE
)
p10.1_3d
