##ISFM model ##

####Farm Level Economic Benefit ####

####Install and Load packages####
install.packages("tidyverse")
install.packages("decisionSupport")
install.packages("dplyr")

library(tidyverse)
library(decisionSupport)
library(dplyr)

#Load and read data 
table <- read.csv("./ISFM_inputs.csv")


#Make variable to test the model
make_variables <- function(est, n= 1) {
  x <- random(rho = est,n = n)
  for(i in colnames(x)) 
    assign(i, as.numeric(x[1, i]),envir = .GlobalEnv)
}

make_variables <- make_variables (as.estimate(table))

#### ISFM components ####

#Status Quo (sq) #Traditional varieties and no soil amendment
#comp1. Improved Germplasm (IG)
#comp2. IG + Inorganic fertilizer (IG + IF)
#comp3. IG + Organic fertilizer (IG + 0F) #Organic fertilizer is manure and crop residue
#comp4. IG + OF + IF
#comp5. IG + OF + IF + Minimum tillage (MT) #Specific for Northern Ghana soil


####Model function####

system_benefits <- function(x, varnames){
  
  
####Risks types under ISFM ####

###Production risks (weather and agronomic risk)####
  
##All the risks associated with maize in every cropping season ##
maize_risks <- chance_event(maize_risks_prob, value_if = percent_maize_risk_damage , #multiplier when there is market risks
                            value_if_not = 1,
                            n= years)
                

###price and market risks###

#Probability of no market for produce or high competition and of Price fluctuation or discounting produce
market_risks <- chance_event(market_risks_prob, value_if = percent_market_risk_damage, #multiplier when there is market risks
                             value_if_not = 1,
                             n= years)

###institutional and policy risks###
#Probability of land litigation limiting investment for long term innovation
#This is coupled with norms which may limit the use of some practices 

institutional_risks <- chance_event(institution_risks_prob, value_if = percent_institution_risk, 
                                    value_if_not = 1,
                                    n= years)
  

###Human and personal risks### 
#Probability of human made or personal risks

farmers_risks <- chance_event(farmers_risks_prob, value_if = percent_farmer_risk_damage, 
                              value_if_not = 1,
                              n= years)


#### Costs of ISFM ####

###Generic costs of ISFM###
inputs_costs <- vv (var_mean = improved_maize_seed + improved_soybean_seed #Improved seed and pesticide
                    + pesticide, 
                    var_CV = var_cv,
                    n= years) 


labor <- vv(var_mean= hired_labor, #Hired labor for all field related activities other than household and neighbor force
          var_CV = var_cv,
             n= years) 
            

#farmers work a lot so they sometimes have pain and may need to buy painkiller
#This cost applies to all component of ISFM but could increase as component number increase
#Probably the chance of pain comes with high number of ISFM component

medicine = vv(medicine_cost * pain_days,  #Number of days farmers might experience pain in a cropping season 
              var_CV = var_cv,
              n= years)
             

no_medicine = vv(no_pain, 
              var_CV = var_cv,
              n=years) #Zero cost if no pain or denial for feeling pain or resistance to take medicine


sickness <- chance_event(sickness_probability,
                         value_if = medicine,
                         value_if_not = no_medicine,
                         n= years)


                 
other_costs_raw <- (tools #equipment here are cutlass, hoe, wood and tiles for fencing etc 
                 + transport # transport of inputs and harvest produce to and from market
                 + fuel  # small token paid to extension officers when they visit
                 + training # Farmers need to make time for training but sometimes also pay for it in terms of transport 
                 + soil_testing  # Not very common but sometimes farmers have to do it
                + land # Cost of land acquisition and registration
                + tractor_services) 

other_costs1 <- vv (var_mean = other_costs_raw, 
                    var_CV = var_cv,
                    n= years)

#Adding the cost of medicine if farmers get sick 

other_costs <- other_costs1 + sickness #If buying medicine           

##Here the above costs are generic in all ISFM components and status quo
component_sq_inputs <-vv (var_mean = (trad_seed + pesticide), #traditional seed cost and pesticide
                   var_CV = var_cv,
                   n= years)
                   

total_cost_sq <- (component_sq_inputs 
                  + labor + other_costs)

##From here specific inputs for each ISFM components are added

##Component 1 (Improved Germplasm) #1st entry point in ISFM##

component1_inputs <- inputs_costs # Improved seed and pesticide only

total_cost_1 <- (component1_inputs + labor + other_costs)

##Component 2 (Improved Germplasm + Inorganic fertilizer)##

component2_inputs <- vv (var_mean = fertilizer_price + fertilizer_application, 
                    var_CV = var_cv,
                    n= years)
                   

total_cost_2 <- (component2_inputs + labor + other_costs + inputs_costs)

####Introduction of organic amendment####

#From component 3 onwards, Organic fertilizer is included in the system, 
#Organic fertilizer is manure and crop residue combined 
#Some/most farmers do not incorporate their crop residues in the field and some others do not own livestock to have manure 
#In such cases when using organic amendment they have to buy in the market or outsource from neighbors at a small fee

own_residue= vv(farmer_residue, #Zero cost to buy residue in this case because farmers left some for farming
              var_CV = var_cv,
              n= years)
              
                     
no_residue = vv(residue_price, #When farmers have to outsource so they could make organic fertilizer
                    var_CV = var_cv,
                    n= years)
                    
# Need for residue for farming and how much
crop_residue <- chance_event(if_crop_residue,
                                value_if = own_residue,
                                value_if_not = no_residue,
                                n= years)


#Manure 

livestock= vv(farmer_manure, #Zero cost to buy manure in this case
      var_CV = var_cv,
      n=years)
      

no_livestock <- vv(manure_price, #When farmers have to outsource manure because they don't own livestock
               var_CV = var_cv,
               n= years)
               
#Probability of paying for manure and how much
manure <- chance_event(if_livestock,
                              value_if = livestock,
                              value_if_not = no_livestock,
                              n= years)


##Component 3 (Improved germplasm and organic fertilizer)

component3_inputs<- vv(compost_preparation, #Cost of preparing organic fertilizer mixing residue and manure before application
   var_CV = var_cv,
   n= years)
   

total_cost_3<- (component3_inputs+ manure + crop_residue 
                    + labor + other_costs+ inputs_costs)


##component 4 (IG + OF + IF)
component4_inputs <- (component2_inputs + component3_inputs)

total_cost_4 <- (component4_inputs+ inputs_costs + manure+
                   crop_residue + labor + other_costs)


##component 5 (IG + OF+ IF + M/ZT)
#Less need to till land hence no need for tractor services 
#discounting the other cost by removing the tractor services cost 

other_costs_raw5 <- (tools #equipment here are cutlass, hoe, wood and tiles for fencing etc 
                    + transport # transport of inputs and harvest produce to and from market
                    + fuel  # small token paid to extension officers when they visit
                    + training # Farmers need to make time for training but sometimes also pay for it in terms of transport 
                    + soil_testing  # Not very common but sometimes farmers have to do it
                    + land) # Cost of land acquisition and registration
                    

other_costs5 <- vv (var_mean = other_costs_raw5, 
                    var_CV = var_cv,
                    n= years)


component5_inputs <- (component4_inputs + other_costs5)

total_cost_5 <- (component5_inputs + inputs_costs + labor+ manure + crop_residue) 


#### Systems benefits of Component 1,2,3,4,5 of ISFM #### 
## We will have 4 layers of benefits: farm profit, environmental benefits, household benefits and total system benefit 

####Farm revenue from maize and soybean ####

#The yield is affected by production risks and farmers risks 
#while the income or farm revenue is affected by price and market risks


##Status Quo farm profit ##

##Maize monoculture and no soil amendment
#Traditional maize varieties are used hence we assume they are susceptible to pest/disease and weather conditions

maize_profit_sq <- vv (maize_yield_sq * maize_price, 
                    var_CV = var_cv, 
                    n= years) * maize_risks   #yield (kg/ha) profit


maize_residue_profit_sq <- vv (maize_residue_sq * residue_price,
                     var_CV = var_cv, 
                     n= years)  #profit on biomass (kg/ha)


maize_income_sq <- (maize_profit_sq + maize_residue_profit_sq) * market_risks


#From component 1 onwards improved resistance varieties are advised and they are not affected or severely affected by production risks

#Component 1 farm profit #

#maize component 1#

maize_profit_1 <- vv (maize_yield_1 * maize_price, 
                    var_CV = var_cv, 
                    n= years)  #yield (kg/ha) profit


maize_residue_profit1 <- vv (maize_residue_1 * residue_price,
                     var_CV = var_cv, 
                     n= years)  #profit on biomass (kg/ha)


maize_income_1 <- (maize_profit_1 + maize_residue_profit1) * market_risks


##soybean component 1##

soybean_profit_1 <- vv (soybean_yield_1 * soybean_price, 
                      var_CV = var_cv, 
                      n= years)  #yield in t/ha


soybean_residue_profit1 <- vv (soybean_residue_1 * residue_price,
                       var_CV = var_cv, 
                       n= years)  #biomass in t/ha

soybean_income_1 <- (soybean_profit_1 + soybean_residue_profit1)* market_risks 

 
#Component 2 farm profit #

#maize component 2#

maize_profit_2 <- vv (maize_yield_2 * maize_price, 
                      var_CV = var_cv, 
                      n= years)  #yield (t/ha) profit


maize_residue_profit2 <- vv (maize_residue_2 * residue_price,
                       var_CV = var_cv, 
                       n= years)  #profit on biomass (t/ha)


maize_income_2 <- (maize_profit_2 + maize_residue_profit2) * market_risks


##soybean component 2 ##

soybean_profit_2 <- vv (soybean_yield_2 * soybean_price, 
                        var_CV = var_cv, 
                        n= years)  #yield in t/ha


soybean_residue_profit2 <- vv (soybean_residue_2 * residue_price,
                         var_CV = var_cv, 
                         n= years)  #biomass in t/ha

soybean_income_2 <- (soybean_profit_2 + soybean_residue_profit2)* market_risks

#Component 3 farm profit #

#maize component 3#

maize_profit_3 <- vv (maize_yield_3 * maize_price, 
                      var_CV = var_cv, 
                      n= years)  #yield (t/ha) profit


maize_residue_profit3 <- vv (maize_residue_3 * residue_price,
                       var_CV = var_cv, 
                       n= years)  #profit on biomass (t/ha)


maize_income_3 <- (maize_profit_3 + maize_residue_profit3) * market_risks


##soybean component 3 ##

soybean_profit_3 <- vv (soybean_yield_3 * soybean_price, 
                        var_CV = var_cv, 
                        n= years)  #yield in t/ha


soybean_residue_profit3 <- vv (soybean_residue_3 * residue_price,
                         var_CV = var_cv, 
                         n= years)  #biomass in t/ha

soybean_income_3 <- (soybean_profit_3 + soybean_residue_profit3)* market_risks


#Component 4 farm profit #

#maize component 4#

maize_profit_4 <- vv (maize_yield_4 * maize_price, 
                      var_CV = var_cv, 
                      n= years)  #yield (t/ha) profit


maize_residue_profit4 <- vv (maize_residue_4 * residue_price,
                       var_CV = var_cv, 
                       n= years)  #profit on biomass (t/ha)


maize_income_4 <- (maize_profit_4 + maize_residue_profit4) * market_risks


##soybean component 4 ##

soybean_profit_4 <- vv (soybean_yield_4 * soybean_price, 
                        var_CV = var_cv, 
                        n= years)  #yield in t/ha


soybean_residue_profit4 <- vv (soybean_residue_4 * residue_price,
                         var_CV = var_cv, 
                         n= years)  #biomass in t/ha

soybean_income_4 <- (soybean_profit_4 + soybean_residue_profit4)* market_risks


#Component 5 farm profit #

#maize component 5#

maize_profit_5 <- vv (maize_yield_5 * maize_price, 
                      var_CV = var_cv, 
                      n= years)  #yield (t/ha) profit


maize_residue_profit5 <- vv (maize_residue_5 * residue_price,
                       var_CV = var_cv, 
                       n= years)  #profit on biomass (t/ha)


maize_income_5 <- (maize_profit_5 + maize_residue_profit5) * market_risks


##soybean component 5 ##

soybean_profit_5 <- vv (soybean_yield_5 * soybean_price, 
                        var_CV = var_cv, 
                        n= years)  #yield in t/ha


soybean_residue_profit5 <- vv (soybean_residue_5 * residue_price,
                         var_CV = var_cv, 
                         n= years)  #biomass in t/ha

soybean_income_5 <- (soybean_profit_5 + soybean_residue_profit5)* market_risks

  
#### Environmental benefits of ISFM components linked to soil health ####

##Nutrients returned to soil (Nutrient partial balance in kg) counted by the value of NPK fertilizer ##
#This will be affected by the availability and price of inorganic fertilizer in the market 
#Also affected by farmers application method, right dose at right place and right time 

nutrient_replenished <- vv (nutrient_partial_balance, 
                                   var_CV = var_cv, 
                                   n= years)* market_risks* farmers_risks 

## Soil loss prevention as Organic fertilization improves soil structure ##
#This will be affected by land rights because it takes some time for his benefit to be visible 

erosion_control <- vv (reduced_soil_loss * price_saved_soil,
                       var_CV = var_cv,
                       n= years)* institutional_risks 


## Due to organic fertilizer application there will be high moisture and less need for irrigation ##
saved_water <- vv (percent_moisture * water_price,
                var_CV = var_cv, 
                n= years)


##Biological Nitrogen fixation (BNF) N/ha from the soybean##
#This will be affected by soybean variety linked to its BNF potential as well as it's growth rate
#Soybean risks which will affect the Nitrogen fixation potential 

soybean_risks <- chance_event(soybean_risks_prob, value_if = percent_soybean_risk_damage, 
                              value_if_not = 1,
                              n= years)  


fixed_Nitrogen <- vv (total_Nitrogen * Nitrogen_price, 
           var_CV = var_cv, 
           n= years) * soybean_risks 


##Active Carbon (mg/kg of soil) which takes long time to accumulate in the soil and might be affected by land tenure problems ##

SOC <- vv (soil_organic_carbon * carbon_payment, 
           var_CV = var_cv, 
           n= years) * institutional_risks

saved_carbon <- vv (active_carbon * carbon_payment, 
                    var_CV = var_cv, 
                    n= years) * institutional_risks 

##Microbes diversity which is estimated by a sac of inoculant used per ha
#Affected by ownership of land

biodiversity <- vv (var_mean = inoculant,
                var_CV = var_cv, 
                n= years) * institutional_risks 


##If there is high infiltration rate the inputs will not be washed away. 
#This rate increases with less stress on the land created by use of tractor (farmers lease to use a tractor)

infiltration <- vv (infiltration_rate * tractor_services,
                    var_CV = var_cv,
                    n= years)


## Minimum tillage will reduce incidence of weed hence the need for pesticide##

weed_suppression <-vv (percent_weed * weed_management_price,
                       var_CV = var_cv,
                       n= years) 


#### Social benefits ####

### Health ###

##Household Dietary diversity (HDD) 
##This might be affected if soils are poor due to no fertilization- crops do not get enough nutrients for healthy diets (hidden hunger)
#This is linked to vitality and infant survival#

nutrition <- vv (household_diet* calory_price,
                 var_CV = var_cv, 
                 n= years)* market_risks 


## Months of food security * food availability Or food sufficiency ##
##If the return on investment assures farmers not to lack food at any point in a month year round ##

food_availability <- vv(food_availability_index * meal_price,
                        var_CV = var_cv,
                        n= years) * market_risks 


##Reduced expenditure on health matters because there is less contamination due to leaching ##
#clean drinking water and no respiratory diseases or hand burners from touching chemicals 

contamination <- vv(percent_contamination_reduction * health_expenditure,
                    var_CV = var_cv,
                    n= years)* farmers_risks 

##Stable mental health leads to happiness and healthy life

#Women mental health will be affected by Gender based violence 
if_GBV <- chance_event(GBV_probability, 
                       value_if = percentage_GBV, 
                       value_if_not = 1,
                       n= years)
#for men 
mental_health_men <- vv(mental_health_incidence* health_expenditure,
                    var_CV = var_cv,
                    n= years)
#for women
mental_health_women <- vv(mental_health_incidence* health_expenditure,
                    var_CV = var_cv,
                    n= years) * if_GBV


##Less mineral fertilizer will reduce GHG emission ##
##Reduced Greenhouse gases * Nitrous oxide emissions from N fertilizer
# And CO2 and methane emission reduced per ha

GHG <- (vv (nitrous_oxide,
            var_CV = var_cv,
            n= years) +
          vv (methane,
              var_CV = var_cv,
              n= years) +
          vv(carbon_dioxide,
             var_CV = var_cv,
             n= years))

#Since farmers don't get paid for reduction of GHG, we consider them benefit to their health: less air pollution etc  

reduced_GHG <- (GHG * health_expenditure)

####Health Benefits per gender####
#Different gender will not benefit equally from health benefits especially women who take care of their families wellbeing and are affected by GBV

women_health <- (nutrition + food_availability + reduced_GHG 
                 + contamination + mental_health_women)

men_health <- (nutrition + food_availability + reduced_GHG
               + contamination + mental_health_men)


##Social benefits for Men ##

#proper land tenure systems and good agricultural practices gives healthy lands which increases its value 
healthly_land <- vv(land,
                    var_CV = var_cv,
                    n= years)

#More assets which will lead to high social status which might also increase opportunities, access

social_status <- chance_event(profit_probability, 
                              value_if = assets_value,
                              value_if_not = 1,
                              n= years)


### The worth of knowledge could be associated to the cost for paying labor if the farmer has to leave some other activities to listen to the extension officers and get someone else to farm for him###
##OR Because they don't pay for training as extension officers are supposed to train for free, but this is usually not the case as farmers have to give a small token (fuel money)##
#So training price here could be cost of hired labor or fuel for extension agents 

agric_knowledge <- vv(var_mean = training, 
                var_CV = var_cv, 
                n= years)


#less need for insurance
#if no production risks there won't be need for farmers to pay for insurance

schock_resilience <- vv(insurance_price,
                        var_CV = var_cv,
                        n= years,
                        relative_trend = inflation)* (1-maize_risks) * (1-soybean_risks)
  
men_social_benefits <- (men_health + healthly_land + social_status 
                + agric_knowledge + schock_resilience)   

  
##Women

##women do not own land, they might get access from their brother or husband and these might take it away at any point 
#They do not benefit equally as other household members, they cannot inherit land
land_conflict <- chance_event(land_conflict_probability,
                              value_if = percentage_land_conflict,
                              value_if_not = 1,
                              n= years)

#There is competing interest for residue use and women use it for cooking, if they are used for the farms the women feel threatened
#This conflict might lead to some kind of violence because the women will resist the husband decision of crop residue use
crop_residue_conflict <- chance_event(crop_residue_conflict,
                                      value_if = percentage_cropresidue_conflict,
                                      value_if_not = 1,
                                      n= years)

##Women are rarely targeted in maize systems,and have a small network hence their access to information and resources is not certain
#Due to this men in the household is always the main decision maker on all aspects except they decide to give some to the women
#women get to benefit if their husbands provide access to resources, profit, information and so on 

if_access <- chance_event(probability_women_access,
                          value_if = percentage_women_access,
                          value_if_not = 1,
                          n= years)


#Gender based violence (GBV) might occur as a result of land conflict, crop residue interest or access to information and opportunities

GBV <- (land_conflict * crop_residue_conflict * if_access)

#Provided access to information and resources  
#More ISFM components come with increase labor for women as they still have to do household chores
#this is a cost to the women

additional_labor <- vv (var_mean = women_labor,
                         var_CV = var_cv,
                         n= years)

#If provided access to the benefits, Women agency might lead to some income, here new women get to enroll to be part of a community where they will probably have information, access or support
agency <- vv (var_mean = enrolment_cost,
              var_CV = var_cv,
              n= years)


#Social network: If more farmers are included and do the same practice, they can create more activities together that will save them some money. 
#For example helping each other in farm activities, and group savings especially for the women can allow them to start a side business that will generate off-farm income
#This can be linked to economic empowerment and perhaps some form of decision making power
network <- vv (var_mean = saved_labor + off_farm_income,
               var_CV = var_cv,
               n= years)

#Knowledge from new practices

agric_knowledge <- vv(var_mean = training, 
                      var_CV = var_cv, 
                      n= years) 


###All women benefits put together and discounted in case of Gender Based Violeonce 
women_social_benefits_1 <- (women_health + agency 
              + network + agric_knowledge + schock_resilience)
              
women_social_benefits<- (women_social_benefits_1 - additional_labor) * 
                  (1-GBV)


###Social benefits for the system 
#Average of men and women social benefits 

social_benefits_raw <- (men_social_benefits + women_social_benefits)/2


#### TOTAL ECONOMIC BENEFITS of ISFM ####
#### 4 layers of benefits  ####
#profit + environmental benefits + social benefits and all together to get the systems benefits


####Profit####
#Profit is farm revenue minus total costs, where farm revenue is only from the harvested crops and the residue
#These benefits are compared to the status quo
#Assuming the area ratio of maize and soybean are equally distributed
#All raw values are in Ghana cedis before conversion to US Dollars

statusquo_profit <- (maize_income_sq- total_cost_sq)/exchange_rate #maize is on all 100 % of farmer area
mean_status <- range(statusquo_profit)  #to know the range profit of the statusquo 

component1_profit_raw <- ((maize_income_1 + soybean_income_1)- total_cost_1)/exchange_rate
component1_profit <- component1_profit_raw - statusquo_profit

component2_profit_raw <- ((maize_income_2 + soybean_income_2)- total_cost_2)/exchange_rate
component2_profit <- component2_profit_raw - statusquo_profit

component3_profit_raw <- ((maize_income_3 + soybean_income_3)- total_cost_3)/exchange_rate
component3_profit <- component3_profit_raw - statusquo_profit

component4_profit_raw <- ((maize_income_4 + soybean_income_4)- total_cost_4)/exchange_rate
component4_profit <- component4_profit_raw - statusquo_profit

component5_profit_raw <- ((maize_income_5 + soybean_income_5)- total_cost_5)/exchange_rate
component5_profit <- component5_profit_raw - statusquo_profit

#### Environmental benefits####
statusquo_env <- (fixed_Nitrogen)/exchange_rate

component1_env_raw <- (fixed_Nitrogen)/exchange_rate
component1_env <- component1_env_raw - statusquo_env
                  
component2_env_raw <- (fixed_Nitrogen + nutrient_replenished)/exchange_rate
component2_env <- component2_env_raw - statusquo_env


component3_env_raw <- (fixed_Nitrogen
                    + saved_carbon + saved_water + SOC + reduced_GHG + biodiversity
                    + contamination + erosion_control)/exchange_rate

component3_env <- component3_env_raw - statusquo_env


component4_env_raw <- (fixed_Nitrogen
                  + saved_carbon + saved_water + reduced_GHG + SOC + biodiversity+ contamination
                  + erosion_control + nutrient_replenished)/exchange_rate

component4_env <- component4_env_raw - statusquo_env


component5_env_raw <- (fixed_Nitrogen + saved_carbon + SOC +
                    + saved_water + reduced_GHG + biodiversity+ contamination 
                    + erosion_control + nutrient_replenished
                    + infiltration + weed_suppression)/exchange_rate

component5_env <- component5_env_raw - statusquo_env


#####Farm soil benefits after exchange rate ####
social_benefits <- social_benefits_raw/exchange_rate

####Total benefits####
#ISFM components benefits

total_benefit_sq <- (statusquo_profit)
statusquo_range <- range(total_benefit_sq) #to know the range of the status quo NPV

total_benefit_1 <- (component1_profit + component1_env + social_benefits)

total_benefit_2 <- (component2_profit + component2_env + social_benefits)

total_benefit_3 <- (component3_profit + component3_env + social_benefits)

total_benefit_4 <- (component4_profit + component4_env + social_benefits)

total_benefit_5 <- (component5_profit + component5_env + social_benefits)


##### Farm level benefits of complete ISFM disagragated by gender ####

#Women benefits are affected by access to resources and information 

women_access_probability <- chance_event(probability_women_access, 
                                 value_if = percentage_women_access, #multiplier when there women don't have access to ressources and information
                                 value_if_not = 1,
                                 n= years)
  
women_benefits_total <- (women_social_benefits+ component5_profit + component5_env ) * women_access_probability

men_benefits_total <- (men_social_benefits + component5_profit + component5_env)


####ANALYSIS####

#Net Present Value (NPV)
#Cashflow analysis: projected trend of monetary return based on the profit
#Discount rate is time value for money

#Social benefits are equally distributed across all components of ISFM 

NPV_social_benefits <- discount(social_benefits, discount_rate = discount_rate, 
                                calculate_NPV = TRUE)

##Status quo ##

statusquo_profit <- discount(statusquo_profit, discount_rate = discount_rate, 
                             calculate_NPV = TRUE)

statusquo_env <- discount(statusquo_env, discount_rate = discount_rate, 
                           calculate_NPV = TRUE)

NPV_sq <- discount(total_benefit_sq, discount_rate = discount_rate, 
                      calculate_NPV = TRUE)


cashflow_sq <- discount (total_benefit_sq, discount_rate = discount_rate,
                        calculate_NPV = FALSE)

cumulative_cashflow_sq <- cumsum(cashflow_sq)

  
##Component 1

component1_profit <- discount(component1_profit, discount_rate = discount_rate, 
                              calculate_NPV = TRUE)

component1_env <- discount(component1_env, discount_rate = discount_rate, 
                           calculate_NPV = TRUE)


NPV_comp1 <- discount(total_benefit_1, discount_rate = discount_rate, 
                       calculate_NPV = TRUE)


cashflow_1 <- discount (total_benefit_1, discount_rate = discount_rate,
                      calculate_NPV = FALSE)

cumulative_cashflow_1 <- cumsum(cashflow_1)


##Component 2 

component2_profit <- discount(component2_profit, discount_rate = discount_rate, 
                              calculate_NPV = TRUE)

component2_env <- discount(component2_env, discount_rate = discount_rate, 
                           calculate_NPV = TRUE)


NPV_comp2 <- discount(total_benefit_2, discount_rate = discount_rate, 
                      calculate_NPV = TRUE)


cashflow_2 <- discount (total_benefit_2, discount_rate = discount_rate,
                        calculate_NPV = FALSE)

cumulative_cashflow_2 <- cumsum(cashflow_2)


##component 3

component3_profit <- discount(component3_profit, discount_rate = discount_rate, 
                              calculate_NPV = TRUE)

component3_env <- discount(component3_env, discount_rate = discount_rate, 
                           calculate_NPV = TRUE)


NPV_comp3 <- discount(total_benefit_3, discount_rate = discount_rate, 
                       calculate_NPV = TRUE)


cashflow_3 <- discount (total_benefit_3, discount_rate = discount_rate,
                      calculate_NPV = FALSE)

cumulative_cashflow_3 <- cumsum(cashflow_3)


##component 4

component4_profit <- discount(component4_profit, discount_rate = discount_rate, 
                              calculate_NPV = TRUE)

component4_env <- discount(component4_env, discount_rate = discount_rate, 
                            calculate_NPV = TRUE)

NPV_comp4 <- discount(total_benefit_4, discount_rate = discount_rate, 
                      calculate_NPV = TRUE)

cashflow_4 <- discount (total_benefit_4, discount_rate = discount_rate,
                        calculate_NPV = FALSE)

cumulative_cashflow_4 <- cumsum(cashflow_4)


##component 5

component5_profit <- discount(component5_profit, discount_rate = discount_rate, 
                              calculate_NPV = TRUE)

component5_env <- discount (component5_env, discount_rate = discount_rate, 
                            calculate_NPV = TRUE)

NPV_comp5 <- discount(total_benefit_5, discount_rate = discount_rate, 
                      calculate_NPV = TRUE)

cashflow_5 <- discount (total_benefit_5, discount_rate = discount_rate,
                        calculate_NPV = FALSE)

cumulative_cashflow_5 <- cumsum(cashflow_5)


####Household benefits from only complete ISFM 5 components 

#women cashflow
women_benefits <- discount (women_benefits_total, discount_rate = discount_rate, 
                            calculate_NPV = TRUE)

cashflow_women <- discount (women_benefits_total, discount_rate = discount_rate,
                        calculate_NPV = FALSE)

cumulative_cashflow_women <- cumsum(cashflow_women)

#men cashflow

men_benefits <- discount (men_benefits_total, discount_rate = discount_rate, 
                          calculate_NPV = TRUE)


cashflow_men <- discount (men_benefits_total, discount_rate = discount_rate,
                        calculate_NPV = FALSE)

cumulative_cashflow_men <- cumsum(cashflow_men)


###Calling anything I need to plot##
##profit, environmental, household and total of all benefits ##

return(list(Profit_statusquo = statusquo_profit,
            Profit_component1 = component1_profit,
            Profit_component2 = component2_profit,
            Profit_component3 = component3_profit,
            Profit_component4 = component4_profit,
            Profit_component5 = component5_profit,
            Environmental_benefit_sq= statusquo_env,
            Environmental_benefit1 = component1_env,
            Environmental_benefit2 = component2_env,
            Environmental_benefit3 = component3_env,
            Environmental_benefit4 = component4_env,
            Environmental_benefit5 = component5_env,
            Social_benefits = NPV_social_benefits,
            Women_benefits = women_benefits,
            Men_benefits = men_benefits,
            NPV_statusquo = NPV_sq,
            NPV_component1 = NPV_comp1,
            NPV_component2 = NPV_comp2,
            NPV_component3 = NPV_comp3,
            NPV_component4 = NPV_comp4,
            NPV_component5 = NPV_comp5,
            Cashflow_statusquo = cumulative_cashflow_sq,
            Cashflow_comp1= cumulative_cashflow_1,
            Cashflow_comp2= cumulative_cashflow_2,
            Cashflow_comp3= cumulative_cashflow_3,
            Cashflow_comp4= cumulative_cashflow_4,
            Cashflow_comp5= cumulative_cashflow_5, 
            Men_cashflow= cumulative_cashflow_men,
            Women_cashflow= cumulative_cashflow_women))
}


####Monte Carlo simulation

ISFM_mc_simulation <- mcSimulation(as.estimate(table), 
                              model_function = system_benefits,
                              numberOfModelRuns = 10000,
                              functionSyntax = "plainNames")

write.csv(ISFM_mc_simulation, "./ISFM_mc_simulation_results.csv")

####subsetting data ####

mc_result_ISFM <-read.csv("ISFM_mc_simulation_results.csv",header= TRUE, sep=",")


####PLOTTING####


install.packages("gridExtra")
install.packages("cowplot")
install.packages("gganimate")
install.packages("ggpubr")

library(cowplot) #Build plots together
library(dplyr)
library(gganimate)
library(ggplot2)
library(ggpubr)
library(ggthemes)
library(gridExtra) #Build plots together
library(tidyverse)
library(patchwork) #to build plots together
                         
###Doing box plot and smooth plot to choose from later###  


##Profit visualization#### 

profit_table<-mc_result_ISFM [,c(3:7)] 

profit_data_frame <- data.frame(profit_table)


ISFM_profit <- profit_data_frame %>% 
  pivot_longer(cols = y.Profit_component1:y.Profit_component5, names_to = "ISFM_Components",
               values_to = 'Profit')


#### Plotting  profit distribution ####

### Box plot to see how they overlap ###

profit_plot_box = ggplot(ISFM_profit, aes(x = Profit, fill = ISFM_Components, color= ISFM_Components)) +                       
  geom_boxplot() +
  scale_fill_colorblind() +
  ylab("Probability density") +
  xlab("Farm Profit ($)") +
  theme(
    axis.title.x = element_text(size = 16),    # Increase x-axis title font size
    axis.title.y = element_text(size = 16),    # Increase y-axis title font size
    axis.text.x = element_text(size = 16),     # Increase x-axis text font size
    axis.text.y = element_text(size = 16),     # Increase y-axis text font size
    legend.title = element_text(size = 16),    # Increase legend title font size
    legend.text = element_text(size = 16)    # Increase legend text font size
  )


profit_plot_box

#Save profit boxplot

ggsave("profit_boxplot.png", plot = profit_plot_box, width = 10, height = 8, dpi = 300)

### Smooth Plot ###

profit_plot_smooth= ggplot(ISFM_profit, aes(x = Profit, fill = ISFM_Components, color= ISFM_Components)) +                       
  geom_density(alpha = 0.05)+  
  scale_fill_colorblind()+
  ylab("Probability density")+
  xlab("Farm Profit ($)") 


profit_plot_smooth <- profit_plot_smooth + 
  theme_bw() +
  theme(
    legend.position = c(.7, .8),
    legend.title = element_blank(),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  # Increase title font size
    axis.title.x = element_text(size = 16),    # Increase x-axis title font size
    axis.title.y = element_text(size = 16),    # Increase y-axis title font size
    axis.text.x = element_text(size = 16),     # Increase x-axis text font size
    axis.text.y = element_text(size = 16),     # Increase y-axis text font size
    legend.text = element_text(size = 16)      # Increase legend text font size
  )


profit_plot_smooth

#Save profit smooth plot 
ggsave("profit_smoothplot.png", plot = profit_plot_smooth, width = 10, height = 8, dpi = 300)


#### Environmental benefits visualization#### 

environmental_table<-mc_result_ISFM [,c(9:13)] 

env_data_frame <- data.frame(environmental_table)

ISFM_env <- env_data_frame %>% 
  pivot_longer(cols = y.Environmental_benefit1:y.Environmental_benefit5, names_to = "ISFM_Components",
               values_to = 'Environmental_benefits')

#### Plotting  environmental benefits ####

#Box plot environmental benefits
env_plot_box= ggplot(ISFM_env, aes(x = Environmental_benefits, fill = ISFM_Components, color= ISFM_Components)) +                       
  geom_boxplot()+
  scale_fill_colorblind()+
  ylab("Probability density")+
  xlab("Environmental benefits ($)") + 
  theme(
    axis.title.x = element_text(size = 16),    # Increase x-axis title font size
    axis.title.y = element_text(size = 16),    # Increase y-axis title font size
    axis.text.x = element_text(size = 16),     # Increase x-axis text font size
    axis.text.y = element_text(size = 16),     # Increase y-axis text font size
    legend.title = element_text(size = 16),    # Increase legend title font size
    legend.text = element_text(size = 16)    # Increase legend text font size
  )

env_plot_box

#save environmental plot benefits
ggsave("environmental_boxplot.png", plot = env_plot_box, width = 10, height = 8, dpi = 300)


## Economic Net Present Value with all the benefits (profit, environmental and social) put together##

npv_table<-mc_result_ISFM [,c(18:22)] 


npv_data_frame <- data.frame(npv_table)


###Changing arrangement of NPV data###

ISFM_npv <- npv_data_frame %>% 
  pivot_longer(cols = y.NPV_component1:y.NPV_component5, names_to = "ISFM_Components",
               values_to = 'NPV')
                             

#### Plotting  NPV distribution ####

### Box plot ###

npvplot_box= ggplot(ISFM_npv, aes(x = NPV, fill = ISFM_Components, color= ISFM_Components)) +                       
  geom_boxplot()+
  scale_fill_colorblind()+
  ylab("Probability density")+
  xlab("Net Present Value ($)") +
  theme(
    axis.title.x = element_text(size = 16),    # Increase x-axis title font size
    axis.title.y = element_text(size = 16),    # Increase y-axis title font size
    axis.text.x = element_text(size = 16),     # Increase x-axis text font size
    axis.text.y = element_text(size = 16),     # Increase y-axis text font size
    legend.title = element_text(size = 16),    # Increase legend title font size
    legend.text = element_text(size = 16)    # Increase legend text font size
  )

npvplot_box


#save NPV boxplot 

ggsave("NPV_boxplot.png", plot = npvplot_box, width = 10, height = 8, dpi = 300)

## Smooth density 

npvplot_smooth= ggplot(ISFM_npv, aes(x = NPV, fill = ISFM_Components, color= ISFM_Components)) +                       
  geom_density(alpha = 0.05)+  
  scale_fill_colorblind()+
  ylab("Probability density")+
  xlab("Net Present Value ($)") + 
  theme(
    legend.position = c(.8, .9),
    legend.title = element_blank(),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  # Increase title font size
    axis.title.x = element_text(size = 16),    # Increase x-axis title font size
    axis.title.y = element_text(size = 16),    # Increase y-axis title font size
    axis.text.x = element_text(size = 16),     # Increase x-axis text font size
    axis.text.y = element_text(size = 16),     # Increase y-axis text font size
    legend.text = element_text(size = 16)      # Increase legend text font size
  )


npvplot_smooth

#save NPV smooth 
ggsave("NPV_smooth.png", plot = npvplot_smooth, width = 10, height = 8, dpi = 300)


##NPV visualization disagrageted by gender for only full ISFM

gendered_benefits_table<-mc_result_ISFM[,c(15:16)] 

gendered_data_frame <- data.frame(gendered_benefits_table)

###Changing arrangement of social benefits data###

gendered_benefits_ISFM <- gendered_data_frame %>% 
  pivot_longer(cols = y.Women_benefits:y.Men_benefits, names_to = "gender",
               values_to = 'gendered_benefits')


#### Plotting Gendered NPV distribution ####

### Box plot ###

gendered_plot_box= ggplot(gendered_benefits_ISFM, aes(x = gendered_benefits, fill = gender, color= gender)) +                       
  geom_boxplot()+
  scale_fill_colorblind()+
  ylab("Probability density")+
  xlab("NPV ($)") + 
  theme(
    legend.position = c(.8, .9),
    legend.title = element_blank(),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  # Increase title font size
    axis.title.x = element_text(size = 16),    # Increase x-axis title font size
    axis.title.y = element_text(size = 16),    # Increase y-axis title font size
    axis.text.x = element_text(size = 16),     # Increase x-axis text font size
    axis.text.y = element_text(size = 16),     # Increase y-axis text font size
    legend.text = element_text(size = 16),    # Increase legend text font size
    )

gendered_plot_box

#save household benefits boxplot 
ggsave("gendered_boxplot.png", plot = gendered_box, width = 10, height = 8, dpi = 300)



## Smooth density 

gendered_smooth= ggplot(gendered_benefits_ISFM, aes(x = gendered_benefits, fill = gender, color= gender)) +                       
  geom_density(alpha = 0.05)+  
  scale_fill_colorblind()+
  ylab("Probability density")+
  xlab("Gendered NPV ($)") +
  theme(
  legend.position = c(.8, .9),
  legend.title = element_blank(),
  plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  # Increase title font size
  axis.title.x = element_text(size = 16),    # Increase x-axis title font size
  axis.title.y = element_text(size = 16),    # Increase y-axis title font size
  axis.text.x = element_text(size = 16),     # Increase x-axis text font size
  axis.text.y = element_text(size = 16),     # Increase y-axis text font size
  legend.text = element_text(size = 16),    # Increase legend text font size
)

gendered_smooth

#saving household benefits smooth plot
ggsave("gendered_smooth.png", plot = gendered_smooth, width = 10, height = 8, dpi = 300)



#### SENSITIVITY ANALYSIS #### 

#### Projection to Latent structure (PLS) regression ####

pls_ISFM <-  plsr.mcSimulation(object = ISFM_mc_simulation,
                            resultName = names(ISFM_mc_simulation$y)[1], ncomp= 1)

plot_pls_ISFM <- plot_pls(pls_ISFM, threshold = 0.8,
                          base_size = 10,
                          pos_color = "skyblue", neg_color = "red")+
labs(title = "Projection to Latent structure (PLS) regression of ISFM", size= 8) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  # Increase title font size
    axis.title.x = element_text(size = 16),    # Increase x-axis title font size
    axis.title.y = element_text(size = 16),    # Increase y-axis title font size
    axis.text.x = element_text(size = 16),     # Increase x-axis text font size
    axis.text.y = element_text(size = 16),     # Increase y-axis text font size
    legend.text = element_text(size = 16),     # Increase legend text font size
    legend.title = element_text(size = 16)     # Increase legend title font size
  )


plot_pls_ISFM

#save pls plot 
ggsave("pls.png", plot = plot_pls_ISFM, width = 10, height = 8, dpi = 300)


#### Expected Value of Perfect information (EVPI) Voi analysis ####

ISFM_voi <- data.frame (ISFM_mc_simulation$x, ISFM_mc_simulation$y[1:21])

evpi_ISFM <- multi_EVPI(mc= ISFM_voi, first_out_var = "Profit_statusquo")


##Plotting EVPI for ISFM NPV only since the NPVs includes all the other layers of benefits

plot_evpi1 <- plot_evpi(evpi_ISFM, decision_vars = "NPV_component1")

plot_evpi2 <- plot_evpi(evpi_ISFM, decision_vars = "NPV_component2")

plot_evpi3 <- plot_evpi(evpi_ISFM, decision_vars = "NPV_component3")

plot_evpi4 <- plot_evpi(evpi_ISFM, decision_vars = "NPV_component4")

plot_evpi5 <- plot_evpi(evpi_ISFM, decision_vars = "NPV_component5")


##Compound figures for EVPI
compound_evpi <- (plot_evpi1|plot_evpi2|plot_evpi3|
                    plot_evpi4 |plot_evpi5)

compound_evpi


#### CASHFLOW ANALYSIS ####


#Cashflow status quo

cashflowsq <- plot_cashflow(mcSimulation_object = ISFM_mc_simulation,
                           cashflow_var_name = "Cashflow_statusquo",
                           x_axis_name = "Years of intervention",
                           y_axis_name = "Cashflow in US Dollars",
                           color_25_75 = "grey",
                           color_5_95 = "yellow",
                           color_median= "red",
                           base= 16) +
  theme(
    axis.title.x = element_text(size = 14),    # Increase x-axis title font size
    axis.title.y = element_text(size = 14),    # Increase y-axis title font size
    axis.text.x = element_text(size = 14),     # Increase x-axis text font size
    axis.text.y = element_text(size = 14),     # Increase y-axis text font size
    legend.text = element_text(size = 14),     # Increase legend text font size
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)  # Increase title font size
  )

cashflowsq

#Cashflow for component 1 

cashflow1 <- plot_cashflow(mcSimulation_object = ISFM_mc_simulation,
                           cashflow_var_name = "Cashflow_comp1",
                           x_axis_name = "Years of intervention",
                           y_axis_name = "Cashflow in US Dollars",
                           color_25_75 = "grey",
                           color_5_95 = "yellow",
                           color_median= "red",
                           base= 16)+
  theme(
    axis.title.x = element_text(size = 14),    # Increase x-axis title font size
    axis.title.y = element_text(size = 14),    # Increase y-axis title font size
    axis.text.x = element_text(size = 14),     # Increase x-axis text font size
    axis.text.y = element_text(size = 14),     # Increase y-axis text font size
    legend.text = element_text(size = 14),     # Increase legend text font size
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)  # Increase title font size
  )


#Cashflow for component 2 

cashflow2 <- plot_cashflow(mcSimulation_object = ISFM_mc_simulation,
                           cashflow_var_name = "Cashflow_comp2",
                           x_axis_name = "Years of intervention",
                           y_axis_name = "Cashflow in US Dollars",
                           color_25_75 = "grey",
                           color_5_95 = "yellow",
                           color_median= "red",
                           base= 16)+
  theme(
    axis.title.x = element_text(size = 14),    # Increase x-axis title font size
    axis.title.y = element_text(size = 14),    # Increase y-axis title font size
    axis.text.x = element_text(size = 14),     # Increase x-axis text font size
    axis.text.y = element_text(size = 14),     # Increase y-axis text font size
    legend.text = element_text(size = 14),     # Increase legend text font size
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)  # Increase title font size
  )


#Cashflow for component 3 

cashflow3 <- plot_cashflow(mcSimulation_object = ISFM_mc_simulation,
                           cashflow_var_name = "Cashflow_comp3",
                           x_axis_name = "Years of intervention",
                           y_axis_name = "Cashflow in US Dollars",
                           color_25_75 = "grey",
                           color_5_95 = "yellow",
                           color_median= "red",
                           base= 16) +
  theme(
    axis.title.x = element_text(size = 14),    # Increase x-axis title font size
    axis.title.y = element_text(size = 14),    # Increase y-axis title font size
    axis.text.x = element_text(size = 14),     # Increase x-axis text font size
    axis.text.y = element_text(size = 14),     # Increase y-axis text font size
    legend.text = element_text(size = 14),     # Increase legend text font size
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)  # Increase title font size
  )


#Cashflow for component 4 

cashflow4 <- plot_cashflow(mcSimulation_object = ISFM_mc_simulation,
                           cashflow_var_name = "Cashflow_comp4",
                           x_axis_name = "Years of intervention",
                           y_axis_name = "Cashflow in US Dollars",
                           color_25_75 = "grey",
                           color_5_95 = "yellow",
                           color_median= "red",
                           base= 16) +
  theme(
    axis.title.x = element_text(size = 14),    # Increase x-axis title font size
    axis.title.y = element_text(size = 14),    # Increase y-axis title font size
    axis.text.x = element_text(size = 14),     # Increase x-axis text font size
    axis.text.y = element_text(size = 14),     # Increase y-axis text font size
    legend.text = element_text(size = 14),     # Increase legend text font size
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)  # Increase title font size
  )


#Cashflow for component 5

cashflow5 <- plot_cashflow(mcSimulation_object = ISFM_mc_simulation,
                           cashflow_var_name = "Cashflow_comp5",
                           x_axis_name = "Years of intervention",
                           y_axis_name = "Cashflow in US Dollars",
                           color_25_75 = "grey",
                           color_5_95 = "yellow",
                           color_median= "red",
                           base= 16) +
  theme(
    axis.title.x = element_text(size = 14),    # Increase x-axis title font size
    axis.title.y = element_text(size = 14),    # Increase y-axis title font size
    axis.text.x = element_text(size = 14),     # Increase x-axis text font size
    axis.text.y = element_text(size = 14),     # Increase y-axis text font size
    legend.text = element_text(size = 14),     # Increase legend text font size
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)  # Increase title font size
  )


###Putting the ISFM Cashflows in one frame ###

library(patchwork)

ISFM_cashflow_all <- (cashflowsq + cashflow1 + cashflow2 + cashflow3
+ cashflow4 + cashflow5)

ISFM_cashflow_all


#save cashflow plot 
ggsave("ISFM_cashflow.png", plot = ISFM_cashflow_all, width = 15, height = 10, dpi = 300)


#Gender disagregated cashflows #####START HERE

#Women cashflow

women_cashflow <- plot_cashflow(mcSimulation_object = ISFM_mc_simulation,
                              cashflow_var_name = "Women_cashflow",
                              x_axis_name = "Years of intervention",
                              y_axis_name = "Cashflow in US Dollars",
                              color_25_75 = "grey",
                              color_5_95 = "yellow",
                              color_median= "red",
                              base= 16)+
  theme(
    axis.title.x = element_text(size = 14),    # Increase x-axis title font size
    axis.title.y = element_text(size = 14),    # Increase y-axis title font size
    axis.text.x = element_text(size = 14),     # Increase x-axis text font size
    axis.text.y = element_text(size = 14),     # Increase y-axis text font size
    legend.text = element_text(size = 14),     # Increase legend text font size
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)  # Increase title font size
  )

women_cashflow


#Men cashflow 

men_cashflow <- plot_cashflow(mcSimulation_object = ISFM_mc_simulation,
                           cashflow_var_name = "Men_cashflow",
                           x_axis_name = "Years of intervention",
                           y_axis_name = "Cashflow in US Dollars",
                           color_25_75 = "grey",
                           color_5_95 = "yellow",
                           color_median= "red",
                           base= 16)+
  theme(
    axis.title.x = element_text(size = 14),    # Increase x-axis title font size
    axis.title.y = element_text(size = 14),    # Increase y-axis title font size
    axis.text.x = element_text(size = 14),     # Increase x-axis text font size
    axis.text.y = element_text(size = 14),     # Increase y-axis text font size
    legend.text = element_text(size = 14),     # Increase legend text font size
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)  # Increase title font size
  )

men_cashflow

# Putting the gendered cashflow together 

gendered_cashflow <- (men_cashflow + women_cashflow)

gendered_cashflow


#save gendered cashflow plot 

ggsave("gendered_cashflow.png", plot = gendered_cashflow, width = 10, height = 8, dpi = 300)

