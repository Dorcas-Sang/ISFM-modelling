## ISFM model ##

##Farm Level Economic Benefit ##

##Install and Load packages##
# install.packages("tidyverse")
# install.packages("decisionSupport")
# install.packages("dplyr")

library(tidyverse)
library(decisionSupport)
library(dplyr)

## Load and read data ##

table <- read.csv("./ISFM_inputs.csv")


## Make variable to test the model ##
make_variables <- function(est, n= 1) {
  x <- random(rho = est,n = n)
  for(i in colnames(x)) 
    assign(i, as.numeric(x[1, i]),envir = .GlobalEnv)
}

make_variables <- make_variables (as.estimate(table))

#### ISFM components ####

## Status Quo (sq) #Traditional varieties and no soil amendment
## comp1. Improved Germplasm (IG)
## comp2. IG + Inorganic fertilizer (IG + IF)
## comp3. IG + Organic fertilizer (IG + 0F) 
  #Organic fertilizer is manure and crop residue
## comp4. IG + OF + IF
## comp5. IG + OF + IF + Minimum tillage (MT) 
  #Minimum tillage is specific for Northern Ghana soil


## Model function ##

system_benefits <- function(x, varnames){
  
  
####Risks types under ISFM ####

## Production risks (weather and agronomic risk) ##
  
# All the risks associated with maize in every cropping season #
  
maize_risks <- chance_event(maize_risks_probability, 
                            value_if = (1-percentage_maize_risk_damage) ,
                            #multiplier when there is maize risks
                            value_if_not = 1,
                            n= years)
                
# Soybean risk which will affect the soybean biological Nitrogen Fixation Potential

soybean_risks <- chance_event(soybean_risks_probability, 
                              value_if = 1-percentage_soybean_risk_damage, 
                              #multiplier for soybean risk
                              value_if_not = 1,
                              n= years) 

## Price and market risks ##
#Probability of no market for produce or high competition 
#and of Price fluctuation or discounting produce

market_risks <- chance_event(market_risks_probability,
                             value_if = 1-percentage_market_risk_damage, 
                             #multiplier when there is market risks
                             value_if_not = 1,
                             n= years)

## Institutional and policy risks##
#Probability of land litigation due to land tenure insecurity
#This limit investment for long term innovation
#This is coupled with norms which may limit some groups to own land

institutional_risks <- chance_event(institutional_risks_probability, 
                                    value_if = 1-percentage_institutional_risk, 
                                    value_if_not = 1,
                                    n= years)
  

## Human and personal risks ## 
#Probability of human made or personal risks 

farmers_risks <- chance_event(farmers_risks_probability, 
                              value_if = 1-percentage_farmer_risk_damage, 
                              value_if_not = 1,
                              n= years)

#### Costs ####

#Cost of agricultural inputs in statusquo
#Traditional varieties of maize and pesticide alone
statusquo_inputs_cost <-vv (var_mean = (traditional_seed_price 
                                        + pesticide_price), 
                                   var_CV = var_cv,
                                          n= years)

## Generic costs of ISFM ##
  #These costs are included in all ISFM components and some in the statusquo #

agricultural_inputs_cost <- vv (var_mean = price_improved_maize_seed 
                    + price_improved_soybean_seed 
                    + pesticide_price, 
                    var_CV = var_cv,
                    n= years) 

#Hired labor for all field related activities other than household force
labor_cost_per_season <- vv(var_mean= hired_labor, 
                            #hired labor per cropping season
                           var_CV = var_cv,
                            n= years) 
            

#farmers work a lot so they sometimes have pain and may need to buy painkiller
#This cost applies to all component of ISFM

medical_bill <- chance_event(sickness_probability,
               value_if = daily_medicine_cost * pain_days_per_cropping_season,
                value_if_not = 0,
#Zero cost if no pain or denial for feeling pain or resistance to take medicine
                 n= years)

                 
running_cost <- (fuel  
              # small token paid to extension officers when they visit
                 + training_cost 
 # Farmers need to make time for training and also pay transport to attend
                 + soil_testing  
              # Not very common but it is recommended that farmers to do it
                + tractor_services_cost) 

running_cost_vv <- vv (var_mean = running_cost, 
                    var_CV = var_cv,
                    n= years)

infrastructure_cost <- (land_acquisition_price 
              # Cost of land acquisition and registration
                  + tools 
            #equipment here are cutlass, hoe, wood and tiles for fencing etc 
                  + transport)
              # transport of inputs and harvest produce to and from market

infrastructure_cost_vv <- vv (var_mean = infrastructure_cost, 
                       var_CV = var_cv,
                       n= years)

## Overall cost for the status quo ##
 # In statusquo farmers don't attend any training, don't test soil 
 # Hence they don;t pay for running cost 

total_cost_statusquo <- (statusquo_inputs_cost +
                        infrastructure_cost_vv + medical_bill 
                          + labor_cost_per_season)


## Overall cost for ISFM components ##
 #In ISFM components farmers have to account for running cost 

generic_costs_ISFM <-  (agricultural_inputs_cost + running_cost_vv 
                      + infrastructure_cost_vv + medical_bill 
                       +  labor_cost_per_season)


# From here specific inputs for each ISFM components are added #

## Component 1 (Improved varieties) #1st entry point in ISFM ##

total_cost_component1 <- generic_costs_ISFM

##Component 2 (Improved Germplasm + Inorganic fertilizer)##

component2_inputs <- vv (var_mean = fertilizer_price_per_hectare 
                         + fertilizer_application_price_per_hectare, 
                    var_CV = var_cv,
                    n= years)
                   

total_cost_component2 <- (component2_inputs + generic_costs_ISFM)

## Introduction of organic amendment ##

#From component 3 onwards, Organic fertilizer is included in the system, 
#Organic fertilizer is manure and crop residue combined 
#Some/most farmers do not incorporate their crop residues in the field and 
#some other farmers do not own livestock to have manure 
#In such cases when using organic amendment they have to buy in the market 
#or outsource from neighbors at a small fee


## Probability of paying for crop residue and how much  

crop_residue_cost <- chance_event(probability_crop_residue_availability,
                                value_if = 0,
  #Zero cost to buy residue in this case because farmers left some for farming
          value_if_not = crop_residue_price_per_kg * crop_residue_quantity_needed,
  # When farmers have to outsource so they could apply organic fertilizer
  #In most cases outsourcing is not possible as each farmer use their own
                                n= years)

## Probability of paying for manure and how much
manure_cost <- chance_event(probability_of_owning_livestock,
                              value_if = 0,
    #Zero cost to buy manure in this case
                              value_if_not = manure_price,
     #When farmers have to outsource manure because they don't own livestock
                              n= years)

## Organic fertilizer to be introduced from component 3 

organic_fertilizer_cost <- crop_residue_cost + manure_cost

## Component 3 (Improved germplasm and organic fertilizer)

component3_inputs<- vv(compost_preparation_cost, 
#Cost of preparing organic fertilizer mixing residue and manure before application
                    var_CV = var_cv,
                    n= years)
   

total_cost_component3<- (component3_inputs+ organic_fertilizer_cost
                         + generic_costs_ISFM)


## component 4 (IG + OF + IF) ##
component4_inputs <- (component2_inputs + component3_inputs)

total_cost_component4 <- (component4_inputs+ organic_fertilizer_cost
                 + generic_costs_ISFM)
                   

##component 5 (IG + OF+ IF + M/ZT) ## 
#Introduction of minimum tillage hence Less need to till land 
# This means no need for tractor services 
# This discounts the infrastructure by removing the tractor services cost 

component5_inputs <- vv (var_mean = running_cost - tractor_services_cost, 
                         var_CV = var_cv,
                         n= years)

total_cost_component5 <- (component5_inputs  + organic_fertilizer_cost
                          + generic_costs_ISFM)


##  Systems benefits of Component 1,2,3,4,5 of ISFM ## 
## We will have 4 layers of benefits: 
#farm profit, environmental benefits, social benefits and total system benefit 

#### Farm profit from maize and soybean ####

#The yield is affected by production risks and farmers risks 
#while the income or farm revenue is affected by price and market risks


##Status Quo farm profit ##

##Maize monoculture and no soil amendment
#Traditional maize varieties are used hence we assume 
#they are susceptible to pest/disease and weather conditions

maize_profit_statusquo <- vv (maize_yield_statusquo * maize_price_per_kg, 
                    var_CV = var_cv, 
                    n= years) * maize_risks   #profit on yield (kg/acre) 


maize_residue_profit_statusquo <- vv (maize_residue_statusquo 
                                      *crop_residue_price_per_kg,
                                    var_CV = var_cv, 
                                     n= years)  #profit on biomass (kg/acre)

#The assumption is that Maize monoculture is on all field where there was to be soybean 
# i.e 2 acres 
maize_income_statusquo_2_acres <- (maize_profit_statusquo + maize_residue_profit_statusquo)*2

  
maize_income_statusquo <- maize_income_statusquo_2_acres* market_risks



#From component 1 onwards improved resistance varieties are advised 
#and they are not affected or severely affected by production risks
#From component 1 crop rotation/intercropping of maize and soybean is introduced

##Component 1 farm profit ##

#maize component 1#

maize_profit_1 <- vv (maize_yield_component1 * maize_price_per_kg, 
                    var_CV = var_cv, 
                    n= years)  #Profit on yield (kg/acre)


maize_residue_profit1 <- vv (maize_residue_component1 * crop_residue_price_per_kg,
                     var_CV = var_cv, 
                     n= years)  #profit on biomass (kg/acre)


maize_income_1 <- (maize_profit_1 + maize_residue_profit1) * market_risks


##soybean component 1##

soybean_profit_1 <- vv (soybean_yield_component1 * soybean_price_per_kg, 
                      var_CV = var_cv, 
                      n= years)  #Profit on yield (kg/acre)


soybean_residue_profit1 <- vv (soybean_residue_component1 
                               * crop_residue_price_per_kg,
                              var_CV = var_cv, 
                             n= years)  #profit on biomass (kg/acre)

soybean_income_1 <- (soybean_profit_1 + soybean_residue_profit1)* market_risks 

 
##Component 2 farm profit ##

#maize component 2#

maize_profit_2 <- vv (maize_yield_component2 * maize_price_per_kg, 
                      var_CV = var_cv, 
                      n= years)  #profit yield (kg/acre)


maize_residue_profit2 <- vv (maize_residue_component2 
                             * crop_residue_price_per_kg,
                       var_CV = var_cv, 
                       n= years)  #profit on biomass (kg/acre)


maize_income_2 <- (maize_profit_2 + maize_residue_profit2) * market_risks


##soybean component 2 ##

soybean_profit_2 <- vv (soybean_yield_component2 * soybean_price_per_kg, 
                        var_CV = var_cv, 
                        n= years)  #Profit on yield (kg/acre)


soybean_residue_profit2 <- vv (soybean_residue_component2 
                               * crop_residue_price_per_kg,
                         var_CV = var_cv, 
                         n= years)  #Profit on biomass (kg/acre)

soybean_income_2 <- (soybean_profit_2 + soybean_residue_profit2)* market_risks

##Component 3 farm profit ##

#maize component 3#

maize_profit_3 <- vv (maize_yield_component3 * maize_price_per_kg, 
                      var_CV = var_cv, 
                      n= years)  # Profit on yield (kg/acre)


maize_residue_profit3 <- vv (maize_residue_component3 
                             * crop_residue_price_per_kg,
                       var_CV = var_cv, 
                       n= years)  #profit on biomass (kg/acre)


maize_income_3 <- (maize_profit_3 + maize_residue_profit3) * market_risks


##soybean component 3 ##

soybean_profit_3 <- vv (soybean_yield_component3 * soybean_price_per_kg, 
                        var_CV = var_cv, 
                        n= years)  #Profit on yield (kg/acre)


soybean_residue_profit3 <- vv (soybean_residue_component3
                            * crop_residue_price_per_kg,
                         var_CV = var_cv, 
                         n= years)  #Profit on biomass (kg/acre)

soybean_income_3 <- (soybean_profit_3 + soybean_residue_profit3)* market_risks


##Component 4 farm profit ##

#maize component 4#

maize_profit_4 <- vv (maize_yield_component4 * maize_price_per_kg, 
                      var_CV = var_cv, 
                      n= years)  #Profit on yield (kg/acre)


maize_residue_profit4 <- vv (maize_residue_component4 
                             * crop_residue_price_per_kg,
                       var_CV = var_cv, 
                       n= years)  #profit on biomass (kg/acre)


maize_income_4 <- (maize_profit_4 + maize_residue_profit4) * market_risks


##soybean component 4 ##

soybean_profit_4 <- vv (soybean_yield_component4 * soybean_price_per_kg, 
                        var_CV = var_cv, 
                        n= years)  #Profit on yield (kg/acre)


soybean_residue_profit4 <- vv (soybean_residue_component4 
                               * crop_residue_price_per_kg,
                         var_CV = var_cv, 
                         n= years)  #Profit on biomass (kg/acre)

soybean_income_4 <- (soybean_profit_4 + soybean_residue_profit4)* market_risks


##Component 5 farm profit ##

#maize component 5#

maize_profit_5 <- vv (maize_yield_component5 * maize_price_per_kg, 
                      var_CV = var_cv, 
                      n= years)  #Profit on yield (kg/acre)


maize_residue_profit5 <- vv (maize_residue_component5 
                             * crop_residue_price_per_kg,
                       var_CV = var_cv, 
                       n= years)  #profit on biomass (kg/acre)


maize_income_5 <- (maize_profit_5 + maize_residue_profit5) * market_risks


##soybean component 5 ##

soybean_profit_5 <- vv (soybean_yield_component5 * soybean_price_per_kg, 
                        var_CV = var_cv, 
                        n= years)  #Profit on yield (kg/acre)


soybean_residue_profit5 <- vv (soybean_residue_component5 
                               * crop_residue_price_per_kg,
                         var_CV = var_cv, 
                         n= years)  #Profit on biomass (kg/acre)

soybean_income_5 <- (soybean_profit_5 + soybean_residue_profit5)* market_risks


## Profit ##
#Profit is farm revenue minus total costs,
#where farm revenue is only from the harvested crops and the residue
#These benefits are compared to the status quo
# We assume the area ratio of maize and soybean are equally distributed 
#1 acre for each and rotated every year
# values are in Ghana cedis and are converted to EURO

statusquo_profit <- (maize_income_statusquo- total_cost_statusquo)/exchange_rate 
#maize is on all 100 % of farmer area i.e 2 acres

range_statusquo <- range(statusquo_profit)  
#to know the range profit of the statusquo 

#Discounting ISFM component with statusquo means
#Expected additional gains if a farmer chooses to ISFM components and not the statusquo

component1_profit <- 
     ((maize_income_1 + soybean_income_1)- total_cost_component1)/exchange_rate

component1_profit_EURO <- component1_profit - statusquo_profit

component2_profit <- 
  ((maize_income_2 + soybean_income_2)- total_cost_component2)/exchange_rate

component2_profit_EURO <- component2_profit - statusquo_profit

component3_profit <- ((maize_income_3 + soybean_income_3)-
                      total_cost_component3)/exchange_rate

component3_profit_EURO <- component3_profit - statusquo_profit

component4_profit <- ((maize_income_4 + soybean_income_4)-
                        total_cost_component4)/exchange_rate

component4_profit_EURO <- component4_profit - statusquo_profit

component5_profit <- ((maize_income_5 + soybean_income_5)-
                      total_cost_component5)/exchange_rate

component5_profit_EURO <- component5_profit - statusquo_profit


####Environmental benefits of ISFM linked to soil health and land value ####

##Nutrients returned to soil (Nutrient partial balance in kg) counted 
#by the value of NPK fertilizer ##
#This will be affected by the availability and price of fertilizer in the market 
#Also affected by farmers application method, right dose at right place and right time 

soil_nutrient_replenished <- vv (nutrient_partial_balance_value, 
                                   var_CV = var_cv, 
                                   n= years)* market_risks* farmers_risks 

## Soil loss prevention as Organic fertilization improves soil structure ##

reduced_soil_loss <- vv (saved_soil * price_saved_soil, 
                       #amount (kg) of saved soil times the price of saved soil per kg
                       var_CV = var_cv,
                       n= years)


## Due to organic fertilizer application there will be high moisture 
#This leads to less need for irrigation #

soil_moisture <- vv (liters_water_saved * water_price_per_liter, 
                     #liters of water saved times price of water
                var_CV = var_cv, 
                n= years)


##Biological Nitrogen fixation (BNF) N/ha from the soybean##
#This will be affected by soybean variety linked to its BNF potential 
#as well as it's growth rate
#Soybean risks will affect the Nitrogen fixation potential benefit

fixed_nitrogen <- vv (total_nitrogen_fixed * nitrogen_price, 
           var_CV = var_cv, 
           n= years) * soybean_risks 


## Soil organic Carbon 
#It takes long time to accumulate in the soil 
#Their accumulation in the soil might be affected by land tenure problems 
#the land can be taken away before the SOC is replenished ##

soil_organic_carbon_replenished <- vv (soil_organic_carbon * carbon_payment, 
           var_CV = var_cv, 
           n= years) * institutional_risks


##Soil microbes diversity
microbial_population <- vv (var_mean = inoculant_price,
                var_CV = var_cv, 
                n= years)


##If there is high infiltration rate the inputs will not be washed away. 
#This rate increases with less stress on the land created by use of tractor 
#If farmers don't use tractors they increase infiltration rate 
#They also save money of tractor services

infiltration <- vv (tractor_services_cost,
                    var_CV = var_cv,
                    n= years)


## Minimum tillage will reduce incidence of weed hence the need for pesticide##

weed_suppression <-vv (percentage_weed_reduced * weed_management_price,
                       var_CV = var_cv,
                       n= years) 


#### Environmental benefits####
#The status quo doesn't provide any environmental benefit

environmental_benefit_1 <- (fixed_nitrogen)/exchange_rate

environmental_benefit_2 <- (fixed_nitrogen 
                            + soil_nutrient_replenished)/exchange_rate


environmental_benefit_3 <- (fixed_nitrogen + soil_moisture 
                   + soil_organic_carbon_replenished
                   + microbial_population 
                   + reduced_soil_loss + reduced_soil_loss)/exchange_rate


environmental_benefit_4 <- (fixed_nitrogen + soil_nutrient_replenished 
                            +soil_moisture + soil_organic_carbon_replenished
                    + microbial_population 
                   + reduced_soil_loss)/exchange_rate


environmental_benefit_5 <- (fixed_nitrogen + soil_nutrient_replenished 
                            +soil_moisture + soil_organic_carbon_replenished
                             + microbial_population 
                            + reduced_soil_loss
                            + infiltration + weed_suppression)/exchange_rate
  


#### Social benefits ####

### Health ###

##Nutrition
##This might be affected if soils are poor due to no fertilization
##crops need to get enough nutrients for healthy diets and food availability


nutrition <- vv(days_of_food_availability * daily_meal_price,
                       var_CV = var_cv,
                        n= years) * market_risks 


##Reduced leaching due to Less use of tractor and mineral fertilizer
#This leads to reduced expenditure on health matters 
#because there is less contamination of drinking water and 
#less exposure to respiratory diseases or physical damage from using chemicals 

contamination <- vv(percent_contamination_reduction * yearly_health_expenditure,
                    var_CV = var_cv,
                    n= years)* farmers_risks 


####Health Benefits ####
health <- nutrition + contamination


### The worth of knowledge could be associated to the cost for paying
#for training which should not be the case 
#as extension officers are supposed to train for free
#but this is usually not the case as farmers have to give a small token (fuel money)
#So training price here could be cost of fuel for extension agents 
#or transport to get the training place

value_of_knowledge_gained <- vv(var_mean = training_cost, 
                var_CV = var_cv, 
                n= years)


#less need for insurance 
#because farmers will need to use improved varieties and good agronomic practices
#Their resilience will be affected by production risks

schock_resilience <- vv(insurance_price,
                        var_CV = var_cv,
                        n= years)* (maize_risks) * (soybean_risks)

#Social network: If more farmers are included and do the same practice, 
#they can create more activities together that will save them some money. 
#For example helping each other in farm activities saving on labor, 
#and group savings especially for the women can allow them to start a side business 
#that will generate off-farm income and agency
#This can be linked to economic empowerment and perhaps some form of decision making power

network <- vv (var_mean = hired_labor + off_farm_income,
               var_CV = var_cv,
               n= years)

  
men_social_benefits <- (health + network
            + value_of_knowledge_gained + schock_resilience)/exchange_rate  

##Women have household chores that they must do other than farming 
#This may limit the time they dedicate to farming and affect their productivity
#The chance of this burden is reduced if the household chores are shared

women_additional_labor <- chance_event(women_additional_labor_probability,
               value_if = 1-multiplier_women_labor,
               value_if_not = 0,
               n= years)

  
##Women and factors that affect their ability to do good agricultural practices

##women do not own land, they might get access from their brother or husband 
#and these might take it away at any point

land_security <- chance_event(land_grabbing_probability,
                              value_if = 1-multiplier_effect_for_land_grabbing,
                              value_if_not = 0,
                              n= years)

##Women are rarely targeted in maize systems,
#They have a small network hence their access to information and resources
#is not certain

women_access <- chance_event(probability_women_access,
                             value_if = 1-multiplier_effect_for_women_access,
                             value_if_not = 0,
                             n= years)

#There is competing interest for residue use and some women use it for cooking, 
#if they are used for the farms the women feel threatened
#This conflict might lead to some kind of violence because the women will resist 
#the husband decision of crop residue use
#Gender based violence (GBV) might occur as a result of all these factors

gender_based_violence_occurence <- 
  chance_event(gender_based_violence_probability,
               value_if = 1-percentage_gender_based_violence,
               value_if_not = 0,
               n= years)


###All women benefits put together and discounted in case of the above risks 
women_risks <- min(gender_based_violence_occurence,
                   women_additional_labor,
                  land_security,
                  women_access)
  
women_social_benefits_raw <- (health + network 
     + value_of_knowledge_gained + schock_resilience) * women_risks
 

women_social_benefits <- women_social_benefits_raw/exchange_rate



#### TOTAL ECONOMIC BENEFITS of ISFM ####
#### 4 layers of benefits  ####
#profit + environmental benefits + social benefits and all together to get the systems benefits


###ISFM Social benefits for the system in US dollars

#Average of men and women social benefits to get social benefit of a farmer

social_benefits <- (men_social_benefits + women_social_benefits)/2


####Total system benefits####
#ISFM components benefits

total_benefit_sq <- (statusquo_profit)
statusquo_range <- range(total_benefit_sq) #to know the range of the status quo 

total_benefit_1 <- (component1_profit_EURO
                    + environmental_benefit_1 + social_benefits)

total_benefit_2 <- (component2_profit_EURO 
                    + environmental_benefit_2 + social_benefits)

total_benefit_3 <- (component3_profit_EURO 
                    + environmental_benefit_3 + social_benefits)

total_benefit_4 <- (component4_profit_EURO 
                    + environmental_benefit_4 + social_benefits)

total_benefit_5 <- (component5_profit_EURO 
                    + environmental_benefit_5 + social_benefits)


#### ANALYSIS ####
#Here we first generate the benefit of each ISFM component 
#to know the results of additional components 

#Net Present Value (NPV)


## NPV Profit ISFM components ##
statusquo_profit <- discount(statusquo_profit, discount_rate = discount_rate, 
                             #Discount rate is time value for money
                             calculate_NPV = TRUE)

component1_profit <- discount(component1_profit_EURO, discount_rate = discount_rate, 
                              calculate_NPV = TRUE)

component2_profit <- discount(component2_profit_EURO, discount_rate = discount_rate, 
                              calculate_NPV = TRUE)

component3_profit <- discount(component3_profit_EURO, discount_rate = discount_rate, 
                              calculate_NPV = TRUE)

component4_profit <- discount(component4_profit_EURO, discount_rate = discount_rate, 
                              calculate_NPV = TRUE)

component5_profit <- discount(component5_profit_EURO, discount_rate = discount_rate, 
                              calculate_NPV = TRUE)


## Total benefit of the system ##

NPV_sq <- discount(total_benefit_sq, discount_rate = discount_rate, 
                      calculate_NPV = TRUE)

NPV_comp1 <- discount(total_benefit_1, discount_rate = discount_rate, 
                      calculate_NPV = TRUE)

NPV_comp2 <- discount(total_benefit_2, discount_rate = discount_rate, 
                      calculate_NPV = TRUE)

NPV_comp3 <- discount(total_benefit_3, discount_rate = discount_rate, 
                      calculate_NPV = TRUE)

NPV_comp4 <- discount(total_benefit_4, discount_rate = discount_rate, 
                      calculate_NPV = TRUE)

NPV_comp5 <- discount(total_benefit_5, discount_rate = discount_rate, 
                      calculate_NPV = TRUE)

##  Gender disagregated benefits of ISFM component being promoted by SARI
#SARI is promoting component 5 as ISFM # 

women_total_benefit <- (component5_profit_EURO + environmental_benefit_5 
                     + women_social_benefits)

women_benefit_by_adoption = discount(women_total_benefit, discount_rate = discount_rate, 
                                     calculate_NPV = TRUE)

men_total_benefit <- (component5_profit_EURO + environmental_benefit_5 
                   + men_social_benefits)

men_benefit_by_adoption <- discount(men_total_benefit, discount_rate = discount_rate, 
                                    calculate_NPV = TRUE)


###Calling anything I need to plot##
##profit, environmental, social and total of all benefits ##

return(list(Profit_statusquo = statusquo_profit,
            Profit_improved_seed = component1_profit,
            Profit_inorganic_fertilizer = component2_profit,
            Profit_organic_fertilizer = component3_profit,
            Profit_fertilizer_combination = component4_profit,
            Profit_minimum_tillage = component5_profit,
            NPV_statusquo = NPV_sq,
            NPV_improved_seed = NPV_comp1,
            NPV_inorganic_fertilizer = NPV_comp2,
            NPV_organic_fertilizer = NPV_comp3,
            NPV_fertilizer_combination = NPV_comp4,
            NPV_minimum_tillage = NPV_comp5,
            Women_benefit = women_benefit_by_adoption,
            Men_benefit= men_benefit_by_adoption ))
}


####Monte Carlo simulation

ISFM_mc_simulation <- mcSimulation(as.estimate(table), 
                              model_function = system_benefits,
                              numberOfModelRuns = 1000,
                              functionSyntax = "plainNames")

write.csv(ISFM_mc_simulation, "./ISFM_mc_simulation_results.csv")


####PLOTTING####


# install.packages("gridExtra")
# install.packages("cowplot")
# install.packages("gganimate")
# install.packages("ggpubr")

library(cowplot) #Build plots together
library(dplyr)
library(gganimate)
library(ggplot2)
library(ggpubr)
library(ggthemes)
library(gridExtra) #Build plots together
library(tidyverse)
library(patchwork) #to build plots together
library(RColorBrewer)   #color palette

##Profit visualization##

palette_colors <- brewer.pal(5, "Set2") # for boxplot colors

profit_box_plot <- plot_distributions(
  mcSimulation_object = ISFM_mc_simulation,
  vars = c("Profit_improved_seed", "Profit_inorganic_fertilizer",
           "Profit_organic_fertilizer", "Profit_fertilizer_combination",
           "Profit_minimum_tillage"),
  method = 'boxplot',
  colors = palette_colors,
  base_size = 20
) +
  labs(x = "Farm Profit (Euro)" ) 


profit_box_plot

#Save profit boxplot

ggsave("profit_boxplot.png", plot = profit_box_plot, 
       width = 10, height = 8, dpi = 300)



## Economic Net Present Value with all the benefits 
#(profit, environmental and social) put together#

#### Plotting  NPV distribution ####

### Box plot ###
npv_box_plot <- plot_distributions(mcSimulation_object = ISFM_mc_simulation,
                vars = c("NPV_improved_seed", "NPV_inorganic_fertilizer",
                      "NPV_organic_fertilizer", "NPV_fertilizer_combination",
                          "NPV_minimum_tillage"),
                                   method = 'boxplot',
          colors = c("#0000FF","#F0E442","#667","#F56", "#067"),
                                   base_size = 20) +
  labs(x = "Net present value (Euro)")


npv_box_plot


#save NPV boxplot 

ggsave("NPV_boxplot.png", plot = npv_box_plot, width = 10, height = 8, dpi = 300)

## Smooth plot for NPV
npv_smooth_plot <- plot_distributions(mcSimulation_object = ISFM_mc_simulation,
                 vars = c("NPV_improved_seed", "NPV_inorganic_fertilizer",
                         "NPV_organic_fertilizer", "NPV_fertilizer_combination",
                            "NPV_minimum_tillage"),
                                   method = 'smooth_simple_overlay',
                       colors = c("#0000FF","#F0E442","#667","#F56", "#067"),
                                   base_size = 20) +
  labs(x = "Net present value (Euro)") 

npv_smooth_plot

#save NPV smooth 
ggsave("NPV_smooth.png", plot = npv_smooth_plot, width = 10, height = 8, dpi = 300)


##NPV visualization disagrageted by gender for ISFM components based on adoption



#### Plotting gendered NPV distribution ####

### Box plot ###

gendered_box_plot <- plot_distributions(mcSimulation_object = ISFM_mc_simulation,
                                   vars = c("Women_benefit", 
                                            "Men_benefit"),
                                   method = 'boxplot',
                                   colors = c("#089","#F0E442"),
                                   base_size = 20) +
  labs(
    x = "Net present value (Euro)") 

gendered_box_plot


#save gender benefits boxplot 
ggsave("gendered_boxplot.png", plot = gendered_box_plot, width = 10, height = 8, dpi = 300)


###Smooth plot 

gendered_smooth_plot <- plot_distributions(mcSimulation_object = ISFM_mc_simulation,
                                        vars = c("Women_benefit", 
                                                 "Men_benefit"),
                                        method = 'smooth_simple_overlay',
                                        colors = c("#089","#F0E442"),
                                        base_size = 20) +
  labs(
    x = "Net present value (Euro)") 

gendered_smooth_plot



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

ISFM_voi <- data.frame (ISFM_mc_simulation$x, ISFM_mc_simulation$y[12:14]) 
 # component5, men5, women 5

evpi_ISFM <- multi_EVPI(mc= ISFM_voi, first_out_var = "NPV_component5")


##Plotting EVPI for ISFM NPV only since the NPVs includes all the other layers of benefits

plot_evpi5 <- plot_evpi(evpi_ISFM, decision_vars = "NPV_component5")

plot_evpiwomen <- plot_evpi(evpi_ISFM, decision_vars = "Women_benefit")

plot_evpi5 <- plot_evpi(evpi_ISFM, decision_vars = "Men_benefit")





