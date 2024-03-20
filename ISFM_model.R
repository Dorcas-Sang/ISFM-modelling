
##ISFM model ##

####Economic outcome ####

####Load packages####
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

make_variables <- (as.estimate(table))

#### ISFM components ####

#comp1. Improved Germplasm (IG)
#comp2. IG + Organic fertilizer (IG + 0F)
#comp3. IG + Inorganic fertilizer (IG + IF)
#comp4. IG + OF + IF
#comp5. IG + OF + IF + Minimum/zero tillage (M/Z T)


####Model function####

system_benefits <- function(x, varnames){
  
  
####Risks of ISFM ####

#All the risks associated with maize in every cropping season
  
#maize_risks <- min(if_failed_germination,
                     #if_elnino,
                     #if_drought,
                     #if_theft)

##OR##

maize_risks <- chance_event(maize_risk,
                            value_if = 1,
                            n= years)

#All the risks associated with soybean in every cropping season  
#soybean_risks <- min(if_failed_germination,
                   #if_elnino,
                   #if_drought,
                   #if_theft)

##OR##

soybean_risks <- chance_event(soybean_risk, 
                              value_if = 1,
                              n= years)

#Chance of land litigation happening
land_risks <- chance_event(land_risk, 
                                value_if = 1,
                                n= years)


#Probability of unavailability of fertilizer in the market as well as 
#Chance of Price fluctuation * inflation
market_risks <- chance_event(availability_risk, 
                                    value_if = 1,
                                    n= years)

#Probability that there will be competition for crop residue use 
competing_interest <- chance_event(crop_residue_risk, 
                                   value_if = 1,
                                   n= years) 

#Chances that farmers will not trust extension officers and 
#have the patience  to wait to see the long term benefits 

#farmers_risks <- min(if_no_trust,
                     #if_no_patience,
                     #if_no_knowledge,
                     #if_violence,
                     #if_no_acceptance)

## OR ###

farmers_risks <- chance_event(farmers_risk, 
                      value_if = 1,
                      n= years)


#Chances that livestock/humans might walk into the fields and compact the soil
soil_compaction <- chance_event(compaction_risk, 
                                value_if = 1,
                                n= years)

#### Systems benefits of Component 1,2,3,4,5 ####  

#Maize 

maize_yield <- vv (var_mean = maize_yield, 
                    var_CV = var_cv, 
                    n= years) * maize_risks #yield in t/ha


maize_residue <- vv (var_mean = maize_residue,
                     var_CV = var_cv, 
                     n= years) * competing_interest  #biomass in t/ha

maize_income <- (maize_yield * maize_price) + (maize_residue* residue_price)
/ exhange_rate


#soybean

soybean_yield <- vv (var_mean = soybean_yield, 
                     var_CV = var_cv, 
                     n= years) * soybean_risks #yield in t/ha


soybean_residue <- vv (var_mean = soybean_residue,
                       var_CV = var_cv, 
                       n= years) * competing_interest #biomass in t/ha

soybean_income <- (soybean_yield * soybean_price) + (soybean_residue * residue_price)
/ exchange_rate

  
## Other benefits that increase ISFM NPV ##

saved_Nitrogen <- vv (var_mean = total_Nitrogen, 
           var_CV = var_cv, 
           n= years) *soybean_risks ###Biological Nitrogen fixation (BNF) N/ha

saved_Nitrogen <- (saved_Nitrogen * Nitrogen_price)/ exchange_rate


saved_carbon <- vv (var_mean = active_carbon, 
                     var_CV = var_cv, 
                     n= years) *land_risks ###Active Carbon in mg/kg of soil

saved_carbon<- (saved_carbon * carbon_payment)/ exchange_rate


biodiversty <- vv (var_mean = biodiversity_index,
                var_CV = var_cv, 
                n= years) * farmers_risks * land_risks ### % microbes below and above ground

biodiversity <- (biodiversity * biodiversity_index_value)/ exhange_rate



nutrition <- vv (var_mean = HDD,
           var_CV = var_cv, 
           n= years)* market_risks ##Household Dietary diversity (HDD) OR Household Food Insecurity Access Score (HFIAS)

nutrition <- (nutrition * calory_price)/ exchange_rate



food_availability <- vv(var_mean = food,
                        var_CV = var_cv,
                        n= years)* land_risks * farmers_risks ##Months of food security * food availability 
# Or food sufficiency 

food_availability <- (food_availability * meal_price)/ exchange_rate
  


reduced_leaching <- vv(var_mean = percent_contamination_reduction,
             var_CV = var_cv,
             n= years)* farmers_risks ##Reduced expenditure on health matters because there is less contamination/leaching


reduced_leaching <- (reduced_leaching * health_expenditure)/ exchange_rate 


soil_nutrient_replenishment <- vv (var_mean = nutrient, 
           var_CV = var_cv, 
           n= years)* market_risks* farmers_risks ###Nutrients returned to soil #Nutrient partial balance

nutrient_replenished <- (soil_nutrient_replenishment * fertilizer_price)/ exchange_rate


stress_resistance <- vv (var_mean = reduced_pesticide,
                         var_CV = var_cv,
                         n= years) * soybean_risks * maize_risks

stress_resistance <- (stress_resistance * pesticide_price)/ exchange_rate
  

moisture <- vv (var_mean = percent_moisture,
                var_CV = var_cv, 
                n= years)* competing_interest ###Less need for irrigation 

saved_water <- (moisture * water_price)/exhange_rate


erosion_control <- vv (reduced_soil_loss,
                       var_CV = var_cv,
                       n= years)* land_risks * competing_interest

erosion_control <- (erosion_control * price_saved_soil)/ exchange_rate


GHG <- (vv (nitrous_oxide,
           var_CV = var_cv,
           n= years) +
          vv (methane,
               var_CV = var_cv,
               n= years) +
             vv(CO2,
                  var_CV = var_cv,
                  n= years))
          
          ##Reduced Greenhouse gases * Nitrous oxide emissions from N fertilizer
                         # CO2 and methane emission reduced per ha

reduced_GHG <- (GHG * payment_GHG)/exchange_rate


infiltration <- vv (infiltration_rate,
                    var_CV = var_cv,
                    n= years)* soil_compaction

infiltration_rate <- (infiltration * money_saved_for_tractor)/ exchange_rate

#If there is high infiltration rate the inputs will not be washed away. 
#This rate increases with less stress on the land created by use of tractor


weed_suppression <-vv (percentage_weed,
                       var_CV = var_cv,
                       n= years)* soil_compaction

weed_suppression<- (weed_suppression * weed_management_price)/ exchange_rate
  
  

social_inclusion <- vv (var_mean = percentage_farmers_bonding,
                        var_CV = var_cv,
                        n= years)* farmers_risks * land_risks

social_inclusion <- (social_inclusion * saved_labor_cost 
                     *saved_money_group_savings)/ exchange_rate

#If more farmers are included and do the same practice, they can create more activities together that will save them some money. 
#For example helping each other in farm activities, and group savings can allow farmers to start a side business that will generate off-farm income

  
knowledge <- vv(var_mean = knowledge_gained,
                 var_CV = var_cv, 
                 n= years) * farmers_risks

knowledge <- (knowledge * training_price)/ exchange_rate



#### TOTAL ECONOMIC BENEFITS of ISFM ####

total_benefit_1 <- maize_income + soybean_income + saved_Nitrogen 
                    + food_availability + knowledge + social_inclusion + nutrition
                  
  
total_benefit_2 <- maize_income + soybean_income + saved_Nitrogen 
                  + food_availability + knowledge + social_inclusion + nutrition
                  + saved_carbon + saved_water + reduced_GHG + biodiversity
                  + reduced_leaching + erosion_control 


total_benefit_3 <- maize_income + soybean_income + saved_Nitrogen 
                  + food_availability + knowledge + social_inclusion
                  + nutrient_replenished + nutrition + stress_resistance


total_benefit_4 <- maize_income + soybean_income + saved_carbon
                  + saved_water + reduced_GHG + biodiversity+ reduced_leaching 
                  + erosion_control + nutrient_replenished + nutrition + stress_resistance

  
total_benefit_5 <- maize_income + soybean_income + saved_carbon
                  + saved_water + reduced_GHG + biodiversity+ reduced_leaching 
                  + erosion_control + nutrient_replenished + nutrition 
                  + stress_resistance + infiltration + weed_suppression 


#### Costs of ISFM ####

#Component 1 

##Purchased inputs 
inputs_1 <- (seed + pesticide + equipements)

inputs_1 <- vv (var_mean = purchased_inputs, 
              var_CV = var_cv,
              n= years, 
              relative_trend = inflation)/ exchange_rate #percentage of increase each year

##Land operations and labor costs
land_costs_1 <- (preparation + planting + maintainance + 
                 harvest + clearing) 

land_costs_1 <- vv (var_mean = land_costs,
                  var_CV = var_cv,
                  n= years, 
                  relative_trend = inflation)/ exchange_rate #percentage of increase each year

#Other costs 
other_costs_1 <- (transport + learning_time + fuel 
               + medical_cost)

other_costs_1 <- vv (var_mean = other_costs,
                   var_CV = var_cv,
                   n= years, 
                   relative_trend = inflation)/ exchange_rate #percentage of increase each year

total_cost_1 <- inputs_1 + land_costs_1 + other_costs_1



#Component 2
inputs_costs2 <- (seed + manure + crop_residue + pesticide + equipements)

#For farmers who manage their crop residue and own livestock 

inputs_costs2 <- if (quantity_organic_fertilizer > reduced_organic_cost) {
  
inputs_costs2 = inputs_costs2 * cost_reduced_own_organic
} else {
  inputs_costs2 = inputs_costs2 
}


inputs_2 <- vv (var_mean = inputs_costs2, 
                var_CV = var_cv,
                n= years, 
                relative_trend = inflation)/ exchange_rate #percentage of increase each year

##Land operations and labor costs
land_costs_2 <- (preparation + planting + maintainance + 
                   harvest + clearing) 

land_costs_2 <- vv (var_mean = land_costs,
                    var_CV = var_cv,
                    n= years, 
                    relative_trend = inflation)/ exchange_rate #percentage of increase each year

#Other costs 
other_costs_2 <- (transport + OF_preparation + learning_time + fuel 
                  + medical_cost)

other_costs_2 <- vv (var_mean = other_costs,
                     var_CV = var_cv,
                     n= years, 
                     relative_trend = inflation)/ exchange_rate #percentage of increase each year

total_cost_2 <- inputs_2 + land_costs_2 + other_costs_2


#component 3

inputs_3 <- (seed + mineral_fertilizer + pesticide + equipements)

inputs_3 <- vv (var_mean = purchased_inputs, 
                var_CV = var_cv,
                n= years, 
                relative_trend = inflation)/ exchange_rate #percentage of increase each year

##Land operations and labor costs
land_costs_3 <- (preparation + planting + maintainance + 
                   harvest + clearing) 

land_costs_3 <- vv (var_mean = land_costs,
                    var_CV = var_cv,
                    n= years, 
                    relative_trend = inflation)/ exchange_rate #percentage of increase each year
#Other costs 
other_costs_3 <- (transport + fertilizer_application + learning_time + fuel 
                  + medical_cost)

other_costs_3 <- vv (var_mean = other_costs,
                     var_CV = var_cv,
                     n= years, 
                     relative_trend = inflation)/ exchange_rate #percentage of increase each year

total_cost_3 <- inputs_3 + land_costs_3 + other_costs_3



#component 4
##Purchased inputs 
inputs_4 <- (seed + manure + crop_residue +
                         mineral_fertilizer + pesticide + equipements)

#For farmers who manage their crop residue and own livestock 

inputs_costs4 <- if (quantity_organic_fertilizer > reduced_organic_cost) {
  
  inputs_costs4 = inputs_costs4 * cost_reduced_own_organic
} else {
  inputs_costs4 = inputs_costs4
}

inputs_4 <- vv (var_mean = inputs_costs4, 
                var_CV = var_cv,
                n= years, 
                relative_trend = inflation)/ exchange_rate #percentage of increase each year
 
##Land operations and labor costs
land_costs_4 <- (preparation + planting + maintainance + 
                  harvest + clearing) 

land_costs_4 <- vv (var_mean = land_costs,
                     var_CV = var_cv,
                     n= years, 
                   relative_trend = inflation)/ exchange_rate #percentage of increase each year
#Other costs 
other_costs_4 <- (transport + learning_time + fuel 
                 + OF_preparation + fertilizer_application+ medical_cost)
 
other_costs_4 <- vv (var_mean = other_costs,
                    var_CV = var_cv,
                    n= years, 
                    relative_trend = inflation)/ exchange_rate #percentage of increase each year

total_cost_4 <- inputs_4 + land_costs_4 + other_costs_4


#component 5 #

##Purchased inputs 
inputs_5 <- (seed + manure + crop_residue +
                         mineral_fertilizer + pesticide + equipements)

#For farmers who manage their crop residue and own livestock 

inputs_costs5 <- if (quantity_organic_fertilizer > reduced_organic_cost) {
  
  inputs_costs5 = inputs_costs5 * cost_reduced_own_organic
} else {
  inputs_costs5 = inputs_costs5 
}


inputs_5 <- vv (var_mean = inputs_costs5, 
                var_CV = var_cv,
                n= years, 
                relative_trend = inflation)/ exchange_rate #percentage of increase each year


##Land operations and labor costs
land_costs_5 <- (preparation + planting + maintainance + 
                   harvest + clearing) 

land_costs_5 <- vv (var_mean = land_costs,
                    var_CV = var_cv,
                    n= years, 
                    relative_trend = inflation)/ exchange_rate #percentage of increase each year
#Other costs 
other_costs_5 <- (transport + learning_time + fuel 
                  + OF_preparation + fertilizer_application+ medical_cost)

other_costs_5 <- vv (var_mean = other_costs,
                     var_CV = var_cv,
                     n= years, 
                     relative_trend = inflation)/ exchange_rate #percentage of increase each year

total_cost_5 <- inputs_5 + land_costs_5 + other_costs_5


##Total costs 

total_costs_1 <- inputs_1 + land_costs_1 + other_costs_1
  
total_costs_2 <- inputs_2 + land_costs_2 + other_costs_2
  
total_costs_3 <- inputs_3 + land_costs_3 + other_costs_3
  
total_costs_4 <- inputs_4 + land_costs_4 + other_costs_4
  
total_costs_5 <- inputs_5 + land_costs_5 + other_costs_5 


####ANALYSIS####

#Cost-benefit analysis (NPV)
#Benefit-cost ratio: the discounted value of the benefits divided by the discounted value of the costs
#Cashflow analysis: projected trend of monetary return

#### Component 1 ####

bottomline_benefit_1 <- total_benefit_1 - total_costs_1
  
#Net Present value 1

NPV_comp1 <- discount(bottomline_benefit_1, discount_rate = discount_rate, 
                       calculate_NPV = TRUE)

discount_total_benefit_1 <- discount(total_benefit_1,discount_rate, 
                                   calculate_NPV = TRUE)

discount_total_cost_1 <- discount(total_cost_1, discount_rate, 
                                calculate_NPV = TRUE)

ratio1 <- discount_total_benefit_1/discount_total_cost_1


cashflow_1 <- discount (bottomline_benefit_1, discount_rate = discount_rate,
                      calculate_NPV = FALSE)

cumulative_cashflow_1 <- cumsum(cashflow_1)


##### Component 2 ####

bottomline_benefit_2 <- total_benefit_2 - total_costs_2

#Net Present value 2

NPV_comp2 <- discount(bottomline_benefit_2, discount_rate = discount_rate, 
                      calculate_NPV = TRUE)

discount_total_benefit_2 <- discount(total_benefit_2,discount_rate, 
                                     calculate_NPV = TRUE)

discount_total_cost_2 <- discount(total_cost_2, discount_rate, 
                                  calculate_NPV = TRUE)

ratio2 <- discount_total_benefit_2/discount_total_cost_2


cashflow_2 <- discount (bottomline_benefit_2, discount_rate = discount_rate,
                        calculate_NPV = FALSE)

cumulative_cashflow_2 <- cumsum(cashflow_2)


####component 3####

bottomline_benefit_3 <- total_benefit_3 - total_costs_3
  
#Net Present value 3

NPV_comp3 <- discount(bottomline_benefit_3, discount_rate = discount_rate, 
                       calculate_NPV = TRUE)

discount_total_benefit_3 <- discount(total_benefit_3,discount_rate, 
                                   calculate_NPV = TRUE)

discount_total_cost_3 <- discount(total_cost_3, discount_rate, 
                                calculate_NPV = TRUE)

ratio3 <- discount_total_benefit_3/discount_total_cost_3


cashflow_3 <- discount (bottomline_benefit_3, discount_rate = discount_rate,
                      calculate_NPV = FALSE)

cumulative_cashflow_3 <- cumsum(cashflow_3)


####component 4 ####

bottomline_benefit_4 <- total_benefit_4 - total_costs_4

#Net Present value 4

NPV_comp4 <- discount(bottomline_benefit_4, discount_rate = discount_rate, 
                      calculate_NPV = TRUE)

discount_total_benefit_4 <- discount(total_benefit_4,discount_rate, 
                                     calculate_NPV = TRUE)

discount_total_cost_4 <- discount(total_cost_4, discount_rate, 
                                  calculate_NPV = TRUE)

ratio4 <- discount_total_benefit_4/discount_total_cost_4


cashflow_4 <- discount (bottomline_benefit_4, discount_rate = discount_rate,
                        calculate_NPV = FALSE)

cumulative_cashflow_4 <- cumsum(cashflow_4)


####component 5####

bottomline_benefit_5 <- total_benefit_5 - total_costs_5

#Net Present value 5

NPV_comp5 <- discount(bottomline_benefit_5, discount_rate = discount_rate, 
                      calculate_NPV = TRUE)

discount_total_benefit_5 <- discount(total_benefit_5,discount_rate, 
                                     calculate_NPV = TRUE)

discount_total_cost_5 <- discount(total_cost_5, discount_rate, 
                                  calculate_NPV = TRUE)

ratio5 <- discount_total_benefit_5/discount_total_cost_5


cashflow_5 <- discount (bottomline_benefit_5, discount_rate = discount_rate,
                        calculate_NPV = FALSE)

cumulative_cashflow_5 <- cumsum(cashflow_5)


return(list(NPV_Comp1 = NPV_comp1,
            NPV_comp2 = NPV_comp2,
            NPV_comp3 = NPV_comp3,
            NPV_comp4 = NPV_comp4,
            NPV_comp5 = NPV_comp5,
            cashflow_comp1= cumulative_cashflow_1,
            cashflow_comp2= cumulative_cashflow_2,
            cashflow_comp3= cumulative_cashflow_3,
            cashflow_comp4= cumulative_cashflow_4,
            cashflow_comp5= cumulative_cashflow_5,
            benefit_cost_ratio_1 = ratio1,
            benefit_cost_ratio_2 = ratio2,
            benefit_cost_ratio_3 = ratio3,
            benefit_cost_ratio_4 = ratio4,
            benefit_cost_ratio_5 = ratio5
            ))
  
}


####Monte Carlo simulation 
mc_simulation <- mcSimulation(as.estimate(table), 
                              model_function = system_benefits,
                              numberOfModelRuns = 1000,
                              functionSyntax = "plainNames")

write.csv(mc_simulation, "./mc_simulation_results.csv")



####PLOTTING####

#MERGE THE CODE IN THE OTHER SCRIPT

