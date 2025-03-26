#ISFM base function##

#This script contains the general function that will estimate the costs, the 
#benefits, the risks and the outcomes of the statusquo and the different ISFM components
#This function will be applied later to each of the options with their specific values

library(tidyverse)
library(decisionSupport)
library(dplyr)

## Load and read the data ##
ISFM_table <- read.csv("data./ISFM_inputs.csv")

## Make variable to test the model bit by bit ##
make_variables <- function(est, n= 1) {
  x <- random(rho = est,n = n)
  for(i in colnames(x)) 
    assign(i, as.numeric(x[1, i]),envir = .GlobalEnv)
}

make_variables <- make_variables (as.estimate(ISFM_table))

#### Defining the decision options####
#We are testing the outcomes of the business as usual compared to ISFM practices

     ## Status Quo (sq) #Traditional varieties and no soil amendment

### ISFM components (from 1 to 5 is partial adoption while 6 is complete ISFM) ##

    ## component 1. Improved Germplasm (IG)
    ## component 2. IG + Inorganic fertilizer (IG + IF)
    ## component 3. IG + Organic fertilizer (IG + 0F) 
       #Organic fertilizer is manure and crop residue
    ## component 4. IG + OF + IF (organic and inorganic fertilizer combination)
    ## component 5. IG + minimum tillage (MT)
         #Minimum tillage is specific for northern Ghana soil
    ## component 6. IG + OF + IF + MT (full ISFM)

#### Risks ####
#Crop production risks
production_risks <- chance_event(production_risks_probability, 
                              value_if = 1-percentage_production_risk_damage,
                              #damage when there is production risk
                              value_if_not = 1,
                              n= years)
  
## Price and market risks ##
#probability of Price fluctuation or discounting produce 
#when production and demand do not meet
market_risks <- chance_event(market_risks_probability,
                               value_if = 1-percentage_market_risk_damage, 
                               #damage when there is market risk
                               value_if_not = 1,
                               n= years)
  
#### Costs ####
#Cost of agricultural inputs in statusquo and ISFM
#Traditional varieties of maize in monoculture, pesticide and tractor are used 

statusquo_inputs_cost <- vv (var_mean = (traditional_maize_seed_price * maize_seed_quantity)
                            + (pesticide_quantity * pesticide_price),
                            var_CV = var_cv,
                            n= years)

#With ISFM there is introduction of cereal-legume in a rotation system 
#Farmers are advised to plant both crops in a season and then rotate field area the following season
#Given that land partitioning is never into halves we will account for this into the costs of seed and the outputs
#Maize is mostly given priority since it is a major staple crop in northern Ghana

land_proportion_maize <- field_area_proportion  # Maize land proportion
land_proportion_soybean <- 1 - land_proportion_maize # Remaining portion for Soybean

maize_seed <- (maize_seed_quantity*land_proportion_maize)*price_improved_maize_seed
soybean_seed <- (soybean_seed_quantity *land_proportion_soybean)*price_improved_soybean_seed

isfm_inputs_cost_vv <- vv (var_mean = (maize_seed + soybean_seed)
  #here plant density is assumed the same as in the statusquo since about 85% of farmers don't follow spacing
                           + (pesticide_quantity *pesticide_price),
                           var_CV = var_cv,
                           n= years)

#Hired labor for all field related activities other than household force
labor_cost_per_season <- vv(var_mean= hired_labor, 
                            #hired labor per cropping season
                            var_CV = var_cv,
                            n= years) 

#farmers work a lot so they sometimes have pain and may need to buy painkiller
medical_bill <- chance_event(sickness_probability,
                             value_if = daily_medicine_cost * pain_days_per_cropping_season,
                             value_if_not = 0,
 #Zero cost if no pain or denial for sickness leading to resistance to take medicine
                             n= years)

isfm_additional_cost_vv <- vv(var_mean = soil_testing,  
                          # highly recommended for resource use efficiency
                              var_CV = var_cv, 
                              n= years)

#Land, tools, training are only paid once in our simulation period 
one_off_payment_isfm <- land_acquisition_price + tools + training_cost
#ISFM includes training but not the statusquo
one_off_payment_statusquo <- land_acquisition_price + tools

one_payment_isfm <- rep(0, years)
one_payment_statusquo <- rep(0, years)

# Setting it to be paid only in the first year
one_payment_isfm [1] <- one_off_payment_isfm
one_payment_statusquo[1] <- one_off_payment_statusquo

establishment_cost_statusquo <- one_payment_statusquo
establishment_cost_isfm <- one_payment_isfm


infrastructure_cost <- vv (var_mean= (transport
    # transport of inputs from market. here inputs only because most farmers produce end 
   # at farm gate with the aggregators/tradors who come to the farmers 
                    + tractor_services_cost), 
                  # for maintenance of the land
                           var_CV = var_cv,
                           n= years)

infrastructure_cost_status_quo <- infrastructure_cost + establishment_cost_statusquo

infrastructure_cost_isfm <- infrastructure_cost + establishment_cost_isfm

## Overall cost for the status quo ##
total_cost_statusquo <- (statusquo_inputs_cost +
                           infrastructure_cost_status_quo + medical_bill 
                         + labor_cost_per_season)

## Overall cost for ISFM components ##
standard_costs_ISFM <-  (isfm_inputs_cost_vv + isfm_additional_cost_vv 
                         + infrastructure_cost_isfm + medical_bill 
                         +  labor_cost_per_season) 

# From here specific inputs for each ISFM components are added #

## Component 1 (Improved varieties) #1st entry point into ISFM with maize and soybean ##
#In our context it is a rotation system with each of the 2 crops planted every season 
#and then rotation of planted area the following cropping season
total_cost_component1 <- standard_costs_ISFM

##Component 2 (Improved Germplasm + Inorganic fertilizer)##
component2_inputs <- vv (var_mean = (fertilizer_quantity_per_acre *
                                       fertilizer_price_per_bag)
                         + fertilizer_application_price_per_acre, 
                         var_CV = var_cv,
                         n= years)

total_cost_component2 <- (component2_inputs + standard_costs_ISFM)

## Introduction of organic amendment ##

#From component 3, organic fertilizer is included in the system, 
#Organic fertilizer is manure and crop residue combined 
#Farmers never have enough organic amendment to meet 
# the recommended application rates (3-5 t/acres) for optimal soil fertility and crop yields
#this is due to limited availability of manure and competing uses for crop residues.
# We will discount the maximum yield at 100% organic fertilizer based on the amount farmers applied 
# Since farmers may have 10-20 % organic fertilizer available, we define this fraction 

organic_fertilizer_availability_factor <- runif(n= years, min = 0.01, max = 0.2)

organic_fertilizer_applied <- organic_amendment_needed * organic_fertilizer_availability_factor
#This will later discount the benefits in organic fertilizer component

## Component 3 (Improved germplasm and organic fertilizer)
#Since farmers do not buy organic fertilizer, they use from their own farms or livestock,
#We will not have a cost attached to it but the cost of its application only

component3_inputs <- vv(compost_preparation_cost  + fertilizer_application_price_per_acre, 
         #Cost of preparing organic fertilizer mixing residue and manure before application
                        var_CV = var_cv,
                        n= years)

total_cost_component3<- (component3_inputs + standard_costs_ISFM)


## component 4 (IG + OF + IF) fertilizer combination ##
component4_inputs <- (component2_inputs + component3_inputs)/2 
#Each fertilizer is added in halves compared to when it is applied alone

total_cost_component4 <- (component4_inputs + standard_costs_ISFM)


## Component 5 (IG + Minimum tillage)
#Introduction of minimum tillage on improved seed
#This means no need for tractor services to tilt the land
#hence it discounts the infrastructure costs by removing the tractor services cost
#Although tillage comes with high labor costs

tillage_cost <- vv(var_mean = tillage_labor_cost,
                   var_CV = var_cv,
                   n= years)

component5_inputs <- (isfm_inputs_cost_vv + isfm_additional_cost_vv 
  + medical_bill +  labor_cost_per_season+ standard_costs_ISFM - tractor_services_cost)
#removing tractor services from ISFM standard costs

total_cost_component5 <- component5_inputs + tillage_cost

## Component 6 (IG + OF + IF + M/ZT) ## 
#Introduction of minimum tillage on improved seed and fertilizer combination
#This is complete ISFM  

total_cost_component6 <- (total_cost_component5 + component4_inputs)

#### Benefits ####

### Environmental benefits of ISFM linked to soil health and land value ###

##Nutrients returned to the soil (Nutrient partial balance)
#This will be affected by the availability and application of fertilizer

nutrient_balance_organic_fertilizer <-
  vv ((nutrient_partial_balance_value_organic_fertilizer * fertilizer_price_per_bag) , 
      var_CV = var_cv, 
      n= years)

nutrient_balance_mineral_fertilizer <-
  vv ((nutrient_partial_balance_value_mineral_fertilizer * fertilizer_price_per_bag) , 
      var_CV = var_cv, 
      n= years)* market_risks #For mineral fertilizer

nutrient_balance_fertilizer_combination <-
  vv ((nutrient_partial_balance_value_fertilizer_combination * fertilizer_price_per_bag) , 
      var_CV = var_cv, 
      n= years)*market_risks   #Fertilizer combination

## Soil loss prevention as organic fertilization and minimum tillage improve soil structure ##
reduced_soil_loss <- vv (saved_soil * price_saved_soil, 
              #amount (kg) of saved soil times the price of saved soil per kg
                         var_CV = var_cv,
                         n= years)

## Due to organic fertilizer application there will be high moisture 
soil_moisture <- vv (liters_water_saved * water_price_per_liter, 
                     var_CV = var_cv, 
                     n= years)

##Biological Nitrogen fixation (BNF) from the soybean##
#This will be affected by soybean variety linked to its BNF potential as well as it's growth rate
fixed_nitrogen <- vv (total_nitrogen_fixed * nitrogen_price, 
                      var_CV = var_cv, 
                      n= years) * production_risks 

## Soil Organic Carbon replenished with the application of organic fertilizer ##
soil_organic_carbon_replenished <- vv (soil_organic_carbon * carbon_payment, 
                                       var_CV = var_cv, 
                                       n= years)

##Soil microbes diversity
microbial_population <- vv (var_mean = inoculant_price,
                            var_CV = var_cv, 
                            n= years)

##If there is high infiltration rate the inputs will not be washed away. 
#This rate increases with less stress on the land created by use of tractor 
#If farmers don't use tractors they increase infiltration rate 
#They also save money that they would have otherwise used for tractor services
infiltration <- vv (tractor_services_cost,
                    var_CV = var_cv,
                    n= years)

## Minimum tillage will reduce incidence of weed hence the need for pesticide##
weed_suppression <-vv (percentage_weed_reduced * weed_management_price,
                       var_CV = var_cv,
                       n= years) 

### Social benefits ###

### The worth of knowledge could be associated to the cost for paying
#for training or transport to get the training place
knowledge <- vv(var_mean = training_cost, 
                                var_CV = var_cv, 
                                n= years)
#less need for insurance 
#because farmers will use improved seed, good agronomic practices and rotation
#They will suffer less schock 
schock_resilience <- vv(insurance_price,
                        var_CV = var_cv,
                        n= years)

#Social network: If more farmers are trained and do the same practice, 
#they can create more activities together that will save them some money. 
#For example helping each other in farm activities saving on labor cost 
network <- vv (var_mean = hired_labor,
               var_CV = var_cv,
               n= years)

#When ISFM is mastered, some NGOs empower farmers to teach other farmers
# this comes with some privileges such as being paid to be the instructor 
agency <- vv(var_mean = training_cost, 
             var_CV = var_cv, 
             n= years)

### Human benefits###
## The human domain is linked to health and nutrition ##
##Nutrition will be discount by the revenue farmers generate
nutrition <- vv(days_of_food_availability * daily_meal_price,
                var_CV = var_cv,
                n= years)

##Reduced leaching due to Less use of mineral fertilizer
#This leads to reduced expenditure on health matters 
#because there is less contamination of drinking water and 
#less exposure to respiratory diseases or physical damage from using chemicals 
reduced_contamination <- vv(percent_contamination_reduction * yearly_health_expenditure,
                            var_CV = var_cv,
                            n= years)  

#### Main function to calculate the 5 outcomes of ISFM ####
#This function will be used for each of the decision options# 

## There will be five different outcomes following 
#the sustainable intensification assessment framework (SIAF) with its 5 indicators
#Productivity, income, environmental, social and human domains

outcomes <- function(total_costs,
                    maize_yield,
                    soybean_yield,
                    production_risks,
                    maize_price,
                    soybean_price,
                    market_risks,
                    nutrient_balance_organic_fertilizer,
                    nutrient_balance_mineral_fertilizer,
                    nutrient_balance_fertilizer_combination,
                    reduced_soil_loss,
                    soil_moisture,
                    fixed_nitrogen,
                    soil_organic_carbon,
                    microbial_population,
                    infiltration,
                    weed_suppression,
                    knowledge,
                    schock_resilience,
                    network,
                    agency,
                    nutrition,
                    reduced_contamination) {

#Because the yield in ISFM is reported for both maize and soybean each per acre,
#making them 2 acres, while the baseline is monoculture 1 acre of maize
#we need to account for land proportion 
#also because farmers do not always divide their land by halves
#Maize is mostly given priority since it is a major staple crop in northern Ghana
  
land_proportion_maize <- field_area_proportion  # Maize land proportion
land_proportion_soybean <- 1 - land_proportion_maize # Remaining portion for Soybean
  
maize_yield_proportion <- (maize_yield * land_proportion_maize) *production_risks
soybean_yield_proportion <- (soybean_yield *land_proportion_soybean)*production_risks 


grain_yield <- (maize_yield_proportion + soybean_yield_proportion)

revenue <- ((maize_yield_proportion * maize_price)
        + (soybean_yield_proportion * soybean_price))*market_risks

income <- (revenue- total_costs)
  
soil_health  <- (nutrient_balance_organic_fertilizer +
                nutrient_balance_mineral_fertilizer +
                nutrient_balance_fertilizer_combination +
                reduced_soil_loss + soil_moisture + fixed_nitrogen
                + soil_organic_carbon + microbial_population +
                infiltration + weed_suppression) 
  
social_benefits <- knowledge + schock_resilience + network + agency

# nutrition or food availability will depend on income farmers generate from 
# each practice which determines the purchasing power of farmers to buy food
# So we define the discount function to adjust nutrition based on income

discount_nutrition_by_income <- function(nutrition, income) {
discount_factor <- income/1000 #convert income into a proportional discount factor 
discounted_nutrition <- nutrition * discount_factor
  return(discounted_nutrition)
}
nutrition_discounted <- discount_nutrition_by_income(nutrition, income)

human_benefits <- (nutrition_discounted
                   + reduced_contamination)
  
# define the outputs of the general function based on the 5 SIAF outcomes
  return(list(productivity = grain_yield,
              economic_benefits = income, 
              environmental_benefits = soil_health,  
              social_benefits = social_benefits, 
              human_benefits = human_benefits
  ))
}
  
  