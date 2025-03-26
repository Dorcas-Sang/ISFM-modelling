library(tidyverse)
library(decisionSupport)
library(dplyr)

## Load and read data ##

ISFM_table <- read.csv("./ISFM_inputs.csv")


## Make variable to test the model ##
make_variables <- function(est, n= 1) {
  x <- random(rho = est,n = n)
  for(i in colnames(x)) 
    assign(i, as.numeric(x[1, i]),envir = .GlobalEnv)
}

make_variables <- make_variables (as.estimate(ISFM_table))

#### ISFM components ####

## Status Quo (sq) #Traditional varieties and no soil amendment
## component 1. Improved Germplasm (IG)
## component 2. IG + Inorganic fertilizer (IG + IF)
## component 3. IG + Organic fertilizer (IG + 0F) 
#Organic fertilizer is manure and crop residue
## component 4. IG + OF + IF (organic and inorganic fertilizer combination)
## component 5. IG + minimum tillage (MT)
#Minimum tillage is specific for Northern Ghana soil
## component 6. IG + OF + IF + MT


## Model function to calculate the outcomes of ISFM practices ##

ISFM_system_benefits <- function(x, varnames){
  
  
  ####Risks under ISFM ####
  
  ## Production risks (weather and agronomic risk) ##
  
  #All the risks associated with maize in every cropping season #
  source("functions/function.R")  
  
  maize_risks <- chance_event(maize_risks_probability, 
                              value_if = 1-percentage_maize_risk_damage,
                              #damage when there is maize risk
                              value_if_not = 1,
                              n= years)
  
  #Soybean risk which will affect the soybean biological Nitrogen Fixation Potential
  #and yield
  
  soybean_risks <- chance_event(soybean_risks_probability, 
                                value_if = 1-percentage_soybean_risk_damage, 
                                #damage when there is soybean risk
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
  
  ## Unavailability of organic amendment as risks to maximum yield 
  #Crop residue and manure contribute to reviving the land, 
  #and since environemntal benefits are linked to the land 
  # this unavailability could affect the yield and environmental benefits
  #in components where organic amendment are to be used
  
  organic_amendement_risks <- chance_event(organic_amendment_availability,
                                           value_if = 1-percentage_damage_organic_fertilizer_unavailabilty,
                                           value_if_not = 1,
                                           n= years)
  
  #### Costs ####
  
  #Cost of agricultural inputs in statusquo
  #Traditional varieties of maize and pesticide alone and 
  #use of tractor to tilt the land
  
  statusquo_inputs_cost <-vv (var_mean = (traditional_maize_seed_price * maize_seed_quantity)
                              + (pesticide_quantity * pesticide_price),
                              var_CV = var_cv,
                              n= years)
  
  ## Standard costs in ISFM ##
  #These costs are included in all ISFM components and some in the statusquo #
  
  isfm_inputs_cost_vv <- vv (var_mean = (price_improved_maize_seed * maize_seed_quantity)
                             #plant density is assumed the same as in the statusquo since about 85% of farmers don't follow spacing
                             + (price_improved_soybean_seed * soybean_seed_quantity)
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
  
  isfm_additional_cost_vv <- vv(var_mean = soil_testing,  
                                # highly recommended for resource use efficiency
                                var_CV = var_cv, 
                                n= years)
  
  #Land, tools, training are only paid once in our simulation period
  
  one_off_payment_isfm <- land_acquisition_price + tools + training_cost
  one_off_payment_statusquo <- land_acquisition_price + tools
  
  one_payment_isfm <- rep(0, years)
  one_payment_statusquo <- rep(0, years)
  
  # Setting it to be paid only in the first year
  one_payment_isfm [1] <- one_off_payment_isfm
  one_payment_statusquo[1] <- one_off_payment_statusquo
  
  establishment_cost_statusquo <- one_payment_statusquo
  establishment_cost_isfm <- one_payment_isfm
  
  
  infrastructure_cost <- vv (var_mean= (transport
                                        # transport of inputs from market,inputs only because most farmers produce end 
                                        # at farm gate with the aggregators who come to the farmers 
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
  
  # Since farmers have 1-10 % organic fertilizer available, we define this fraction 
  
  organic_fertilizer_availability_factor <- runif(n= years, min = 0.01, max = 0.1)
  
  organic_fertilizer_applied <- organic_amendment_needed * organic_fertilizer_availability_factor
  
  
  ## Component 3 (Improved germplasm and organic fertilizer)
  #Since farmers do not buy organic fertilizer, they use from their own farms or livestock,
  #We will not have a cost attached to it but the cost of its application
  
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
  # hence it discounts the infrastructure costs by removing the tractor services cost
  #Although tillage comes with some labor costs
  
  tillage_cost <- vv(var_mean = tillage_labor_cost,
                     var_CV = var_cv,
                     n= years)
  
  component5_inputs <- (isfm_inputs_cost_vv + isfm_additional_cost_vv 
                        + medical_bill +  labor_cost_per_season) 
  + (standard_costs_ISFM - tractor_services_cost)
  #removing tractor services from ISFM standard costs
  
  
  total_cost_component5 <- component5_inputs + tillage_cost
  
  
  ## Component 6 (IG + OF + IF + M/ZT) ## 
  #Introduction of minimum tillage on improved seed and fertilizer combination
  #This is full ISFM  
  
  total_cost_component6 <- (total_cost_component5 + component4_inputs
  )
  
  ### Environmental benefits of ISFM linked to soil health and land value ####
  
  ##Nutrients returned to soil (Nutrient partial balance in kg)
  #This will be affected by the availability and price of fertilizer in the market 
  #Also affected by farmers application method, right dose at right place and right time 
  
  soil_nutrient_replenished_organic_fertilizer <-
    vv ((nutrient_partial_balance_value_organic_fertilizer * fertilizer_price_per_bag) , 
        var_CV = var_cv, 
        n= years)* organic_amendement_risks #for organic fertilizer
  
  soil_nutrient_replenished_mineral_fertilizer <-
    vv ((nutrient_partial_balance_value_mineral_fertilizer * fertilizer_price_per_bag) , 
        var_CV = var_cv, 
        n= years)* market_risks #For mineral fertilizer
  
  
  #For fertilizer combination we account for the one with high damage
  risks_fertilizer_combination <- pmax(market_risks, organic_amendment_availability)
  
  soil_nutrient_replenished_fertilizer_combination <-
    vv ((nutrient_partial_balance_value_fertilizer_combination * fertilizer_price_per_bag) , 
        var_CV = var_cv, 
        n= years)*risks_fertilizer_combination   #Fertilizer combination
  
  ## Soil loss prevention as organic fertilization and minimum tillage improve soil structure ##
  
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
  
  
  ## Soil organic Carbon replenished with the application of organic fertilizer ##
  
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
  #They also save money of tractor services
  
  infiltration <- vv (tractor_services_cost,
                      var_CV = var_cv,
                      n= years)
  
  ## Minimum tillage will reduce incidence of weed hence the need for pesticide##
  
  weed_suppression <-vv (percentage_weed_reduced * weed_management_price,
                         var_CV = var_cv,
                         n= years) 
  
  
  #### Social benefits ####
  
  ### The worth of knowledge could be associated to the cost for paying
  #for training or transport to get the training place
  
  value_of_knowledge_gained <- vv(var_mean = training_cost, 
                                  var_CV = var_cv, 
                                  n= years)
  #less need for insurance 
  #because farmers will use improved seed, good agronomic practices and rotation 
  
  schock_resilience <- vv(insurance_price,
                          var_CV = var_cv,
                          n= years)* (pmin(maize_risks,soybean_risks))
  
  #Social network: If more farmers are included and do the same practice, 
  #they can create more activities together that will save them some money. 
  #For example helping each other in farm activities saving on labor cost 
  
  network <- vv (var_mean = hired_labor,
                 var_CV = var_cv,
                 n= years)
  
  #### Human outcome ####
  ## The human domain is linked to health and nutrition ##
  ##This might be affected if soils are poor due to no fertilization and lower yields
  ##crops need to get enough nutrients for healthy diets and food availability
  ##Nutrition is affected by the availability of inputs in the market as well as
  #Farmers bargaining power and prices when selling their produce
  
  nutrition <- vv(days_of_food_availability * daily_meal_price,
                  var_CV = var_cv,
                  n= years) * market_risks 
  
  
  ##Reduced leaching due to Less use of mineral fertilizer
  #This leads to reduced expenditure on health matters 
  #because there is less contamination of drinking water and 
  #less exposure to respiratory diseases or physical damage from using chemicals 
  
  reduced_contamination <- vv(percent_contamination_reduction * yearly_health_expenditure,
                              var_CV = var_cv,
                              n= years)  
  
  ####  ISFM OUTCOMES ####
  
  ## There will be five different outcomes following 
  #the sustainable intensification assessment framework (SIAF) with its 5 indicators
  #Productivity, income, environmental, social and human domains
  
  #### Statusquo outcomes ####
  
  #Given the context in the statusquo with maize monoculture and no soil amendment
  #we use the exponential decay function on maize yield to account for yield decrease over time
  
  maize_exponential_decay <- function(initial_maize_yield, decay_rate, year) {
    return(initial_maize_yield * exp(-decay_rate * year))
  }
  
  # Status quo productivity accounting for the decay function
  statusquo_productivity_vv <- sapply(0:(years-1), function(t) {
    maize_exponential_decay(maize_yield_statusquo, decay_rate, t)
  })
  
  statusquo_productivity <- discount(statusquo_productivity_vv, 
                                     discount_rate = discount_rate, 
                                     calculate_NPV = TRUE)
  
  statusquo_revenue <- vv(statusquo_productivity_vv * maize_price_per_kg,
                          var_CV = var_cv,
                          n= years)* market_risks
  
  statusquo_income_vv <- statusquo_revenue - total_cost_statusquo
  
  NPV_statusquo_income <- discount(statusquo_income_vv, 
                                   discount_rate = discount_rate, 
                                   calculate_NPV = TRUE)
  
  #THERE ARE NO ENVIRONMENTAL AND SOCIAL BENEFITS LINKED TO THE STATUSQUO hence 
  #they are all a small value to allow calculation for change with ISFM 
  
  statusquo_environmental_benefit_vv <- vv(0.1, #to avoid 0
                                           var_CV = var_cv,
                                           n= years)
  
  statusquo_environmental_benefit <- discount(statusquo_environmental_benefit_vv, 
                                              discount_rate = discount_rate, 
                                              calculate_NPV = TRUE)
  
  statusquo_social_benefit_vv <- vv(0,
                                    var_CV = var_cv,
                                    n= years)
  
  statusquo_social_benefit <- discount(statusquo_social_benefit_vv, 
                                       discount_rate = discount_rate, 
                                       calculate_NPV = TRUE)
  
  #In the statusquo of maize monoculture there is no contamination from fertilizer
  #But there is a tradeoff on nutrition since farmers don't harvest enough to feed themselves and sale, 
  
  statusquo_human_benefit_vv <- reduced_contamination
  statusquo_human_benefit <- discount(reduced_contamination, 
                                      discount_rate = discount_rate, 
                                      calculate_NPV = TRUE)
  
  #From ISFM component 1 onwards improved resistant varieties of maize and soybean on a rotation system are advised 
  ## All the outcomes will be discounted by those of the statusquo to find the relative change 
  
  #Because the yield in ISFM is reported for both maize and soybean each per acre,
  #making them 2 acres, while the baseline is monoculture 1 acre of maize
  #we need to account for land proportion 
  #also because farmers do not always divide their land by halves
  #Maize is mostly given priority since it is a major staple crop in northern Ghana
  
  land_proportion_maize <- field_area_proportion  # Maize land proportion
  land_proportion_soybean <- 1 - land_proportion_maize # Remaining portion for Soybean
  
  #ISFM outcomes when vital resources are available (all things being equal)
  
  ##ISFM1 
  
  # ISFM1 productivity adjusted based on land proportion
  maize_yield_component1_adjusted <- land_proportion_maize * maize_yield_component1 
  soybean_yield_component1_adjusted <- land_proportion_soybean * soybean_yield_component1 
  
  #yield gains from the 2 crops in isfm1
  isfm1_productivity_vv <- vv (maize_yield_component1_adjusted 
                               + soybean_yield_component1_adjusted, 
                               var_CV = var_cv, 
                               n= years) #yield (kg/acre) 
  
  #Finding absolute change on productivity compared to the statusquo
  isfm1_productivity_change <- isfm1_productivity_vv - statusquo_productivity_vv
  
  isfm1_productivity <- discount(isfm1_productivity_change,
                                 discount_rate = discount_rate, 
                                 calculate_NPV = TRUE)
  
  # ISFM1 income
  isfm1_revenue <- vv((maize_yield_component1_adjusted * maize_price_per_kg) 
                      + (soybean_yield_component1_adjusted * soybean_price_per_kg),
                      var_CV = var_cv,
                      n= years)
  
  isfm1_income_vv <- (isfm1_revenue- total_cost_component1)* market_risks
  
  isfm1_income_change <- isfm1_income_vv - statusquo_income_vv
  
  isfm1_income <- discount(isfm1_income_change,
                           discount_rate = discount_rate, 
                           calculate_NPV = TRUE)
  
  cashflow_isfm1 <- cumsum(isfm1_income_vv)
  
  #ISFM1 environmental benefits
  
  isfm1_environmental_change <- fixed_nitrogen - statusquo_environmental_benefit_vv
  
  isfm1_environmental_benefit <- discount(isfm1_environmental_change, 
                                          discount_rate = discount_rate, 
                                          calculate_NPV = TRUE)
  
  #ISFM1 social benefits
  isfm1_social_benefit_change <- schock_resilience - statusquo_social_benefit_vv
  
  isfm1_social_benefit <- discount(isfm1_social_benefit_change,
                                   discount_rate = discount_rate, 
                                   calculate_NPV = TRUE)
  
  #ISFM1 human benefits
  isfm1_human_benefit_change <- (nutrition + reduced_contamination)- statusquo_human_benefit_vv
  
  isfm1_human_benefit <- discount(isfm1_human_benefit_change,
                                  discount_rate = discount_rate, 
                                  calculate_NPV = TRUE)
  
  ## ISFM2
  
  maize_yield_component2_adjusted <- land_proportion_maize * maize_yield_component2
  soybean_yield_component2_adjusted <- land_proportion_soybean * soybean_yield_component2   
  
  isfm2_productivity_vv <- vv (maize_yield_component2_adjusted 
                               + soybean_yield_component2_adjusted, 
                               var_CV = var_cv, 
                               n= years) #yield (kg/acre)
  
  isfm2_productivity_change <- isfm2_productivity_vv - statusquo_productivity_vv
  
  isfm2_productivity <- discount(isfm2_productivity_change, 
                                 discount_rate = discount_rate, 
                                 calculate_NPV = TRUE)
  
  isfm2_revenue_vv <- vv ((maize_yield_component2_adjusted*maize_price_per_kg) 
                          +(soybean_yield_component2_adjusted*soybean_price_per_kg),
                          var_CV = var_cv,
                          n= years)
  
  isfm2_income_vv <- (isfm2_revenue_vv - total_cost_component2)*market_risks
  
  isfm2_income_change <- isfm2_income_vv - statusquo_income_vv
  
  isfm2_income <- discount(isfm2_income_change, 
                           discount_rate = discount_rate, 
                           calculate_NPV = TRUE)
  
  cashflow_isfm2 <- cumsum(isfm2_income_vv)
  
  isfm2_environmental_benefit_vv <- (fixed_nitrogen 
                                     + soil_nutrient_replenished_mineral_fertilizer)
  
  isfm2_environmental_change <- isfm2_environmental_benefit_vv - statusquo_environmental_benefit_vv
  
  isfm2_environmental_benefit <- discount(isfm2_environmental_change, 
                                          discount_rate = discount_rate, 
                                          calculate_NPV = TRUE)
  
  isfm2_social_benefit_vv <- schock_resilience
  
  isfm2_social_benefit_change <- isfm2_social_benefit_vv - statusquo_social_benefit_vv
  
  isfm2_social_benefit <- discount(isfm2_social_benefit_change, 
                                   discount_rate = discount_rate, 
                                   calculate_NPV = TRUE)
  
  isfm2_human_benefit_change <- nutrition - statusquo_human_benefit_vv
  
  isfm2_human_benefit <- discount(isfm2_human_benefit_change,
                                  discount_rate = discount_rate, 
                                  calculate_NPV = TRUE)
  
  ### ISFM 3
  # Discount some benefits based on the fraction of organic amendment available
  
  maize_yield_component3_adjusted <- vv (land_proportion_maize * maize_yield_component3, 
                                         var_CV = var_cv, 
                                         n= years)* (organic_fertilizer_applied / organic_amendment_needed)
  
  soybean_yield_component3_adjusted <- vv(land_proportion_soybean * soybean_yield_component3,
                                          var_CV = var_cv,
                                          n= years) * (organic_fertilizer_applied / organic_amendment_needed)
  
  isfm3_productivity_vv <- (maize_yield_component3_adjusted 
                            + soybean_yield_component3_adjusted) #yield (kg/acre) 
  
  isfm3_productivity_change <- isfm3_productivity_vv - statusquo_productivity_vv
  
  isfm3_productivity <- discount(isfm3_productivity_change, 
                                 discount_rate = discount_rate, 
                                 calculate_NPV = TRUE)
  
  isfm3_revenue_vv <- vv (((maize_yield_component3_adjusted * maize_price_per_kg) 
                           + (soybean_yield_component3_adjusted * soybean_price_per_kg)),
                          var_CV = var_cv,
                          n= years)
  
  isfm3_income_vv <- (isfm3_revenue_vv - total_cost_component3)*market_risks
  
  isfm3_income_change <- isfm3_income_vv - statusquo_income_vv
  
  isfm3_income <- discount(isfm3_income_change, 
                           discount_rate = discount_rate, 
                           calculate_NPV = TRUE)
  
  cashflow_isfm3 <- cumsum(isfm3_income_vv)
  
  isfm3_environmental_benefit_vv <- (fixed_nitrogen + soil_moisture 
                                     + soil_organic_carbon_replenished
                                     + microbial_population 
                                     + nutrient_partial_balance_value_organic_fertilizer             
                                     + reduced_soil_loss) * (organic_fertilizer_applied / organic_amendment_needed)
  
  
  isfm3_environmental_change <- isfm3_environmental_benefit_vv - statusquo_environmental_benefit_vv
  
  isfm3_environmental_benefit <- discount(isfm3_environmental_change, 
                                          discount_rate = discount_rate, 
                                          calculate_NPV = TRUE)
  
  isfm3_social_benefit_vv <- (value_of_knowledge_gained
                              + schock_resilience)
  
  isfm3_social_benefit_change <- isfm3_social_benefit_vv - statusquo_social_benefit_vv
  
  isfm3_social_benefit <- discount(isfm3_social_benefit_change, 
                                   discount_rate = discount_rate, 
                                   calculate_NPV = TRUE)
  
  isfm3_human_benefit_change <- (reduced_contamination + nutrition) - statusquo_human_benefit_vv
  
  isfm3_human_benefit <- discount(isfm3_human_benefit_change, 
                                  discount_rate = discount_rate, 
                                  calculate_NPV = TRUE)
  ##ISFM4
  
  maize_yield_component4_adjusted <- land_proportion_maize * maize_yield_component4
  soybean_yield_component4_adjusted <- land_proportion_soybean * soybean_yield_component4   
  
  isfm4_productivity_vv <- vv (maize_yield_component4_adjusted 
                               + soybean_yield_component4_adjusted, 
                               var_CV = var_cv, 
                               n= years) #yield (kg/acre)
  
  isfm4_productivity_change <- isfm4_productivity_vv - statusquo_productivity_vv
  
  isfm4_productivity <- discount(isfm4_productivity_change, 
                                 discount_rate = discount_rate, 
                                 calculate_NPV = TRUE)
  
  isfm4_revenue_vv <- vv ((maize_yield_component4_adjusted * maize_price_per_kg) 
                          + (soybean_yield_component4_adjusted * soybean_price_per_kg),
                          var_CV = var_cv,
                          n= years) 
  
  isfm4_income_vv <- (isfm4_revenue_vv - total_cost_component4) * market_risks                 
  
  isfm4_income_change <- isfm4_income_vv - statusquo_income_vv
  
  isfm4_income <- discount(isfm4_income_change, 
                           discount_rate = discount_rate, 
                           calculate_NPV = TRUE)
  
  cashflow_isfm4 <- cumsum(isfm4_income_vv)
  
  
  isfm4_environmental_benefit_vv <- (fixed_nitrogen 
                                     + nutrient_partial_balance_value_fertilizer_combination 
                                     +soil_moisture + soil_organic_carbon_replenished
                                     + microbial_population 
                                     + reduced_soil_loss)* (organic_amendment_availability/organic_amendment_needed)
  
  isfm4_environmental_change <- isfm4_environmental_benefit_vv - statusquo_environmental_benefit_vv
  
  isfm4_environmental_benefit <- discount(isfm4_environmental_change, 
                                          discount_rate = discount_rate, 
                                          calculate_NPV = TRUE)
  
  isfm4_social_benefit_vv <- (network + value_of_knowledge_gained
                              + schock_resilience)
  
  isfm4_social_benefit_change <- isfm4_social_benefit_vv - statusquo_social_benefit_vv
  
  isfm4_social_benefit<- discount(isfm4_social_benefit_change, 
                                  discount_rate = discount_rate, 
                                  calculate_NPV = TRUE)
  
  isfm4_human_benefit_change <- (reduced_contamination + nutrition) - statusquo_human_benefit_vv
  
  isfm4_human_benefit <- discount(isfm4_human_benefit_change, 
                                  discount_rate = discount_rate, 
                                  calculate_NPV = TRUE)
  # IFSM 5 outcomes
  
  maize_yield_component5_adjusted <- land_proportion_maize * maize_yield_component5 
  soybean_yield_component5_adjusted <- land_proportion_soybean * soybean_yield_component5  
  
  isfm5_productivity_vv <- vv (maize_yield_component5_adjusted 
                               + soybean_yield_component5_adjusted, 
                               var_CV = var_cv, 
                               n= years) #yield (kg/acre)
  
  isfm5_productivity_change <- isfm5_productivity_vv - statusquo_productivity_vv
  
  isfm5_productivity <- discount(isfm5_productivity_change, 
                                 discount_rate = discount_rate, 
                                 calculate_NPV = TRUE)
  
  isfm5_revenue_vv <- vv ((maize_yield_component5_adjusted * maize_price_per_kg) 
                          + (soybean_yield_component5_adjusted * soybean_price_per_kg),
                          var_CV = var_cv,
                          n= years) 
  
  isfm5_income_vv <- (isfm5_revenue_vv - total_cost_component5) * market_risks
  
  isfm5_income_change <- isfm5_income_vv - statusquo_income_vv
  
  isfm5_income <- discount(isfm5_income_change, 
                           discount_rate = discount_rate, 
                           calculate_NPV = TRUE)
  
  cashflow_isfm5 <- cumsum(isfm5_income_vv)
  
  
  isfm5_environmental_benefit_vv <- (fixed_nitrogen 
                                     + reduced_soil_loss + infiltration
                                     + weed_suppression)
  
  isfm5_environmental_change <- isfm5_environmental_benefit_vv - statusquo_environmental_benefit_vv
  
  isfm5_environmental_benefit <- discount(isfm5_environmental_change, 
                                          discount_rate = discount_rate, 
                                          calculate_NPV = TRUE)
  
  isfm5_social_benefit_change <- isfm4_social_benefit_change # already discounted 
  isfm5_social_benefit <- isfm4_social_benefit
  
  isfm5_human_benefit_change <- isfm3_human_benefit_change # already discounted
  isfm5_human_benefit <- isfm3_human_benefit
  
  
  ## ISFM 6 outcomes##
  
  maize_yield_component6_adjusted <- land_proportion_maize * maize_yield_component6
  soybean_yield_component6_adjusted <- land_proportion_soybean * soybean_yield_component6  
  
  isfm6_productivity_vv <- vv (maize_yield_component6_adjusted 
                               + soybean_yield_component6_adjusted, 
                               var_CV = var_cv, 
                               n= years) #yield (kg/acre)
  
  isfm6_productivity_change <- isfm6_productivity_vv - statusquo_productivity_vv
  
  isfm6_productivity <- discount(isfm6_productivity_change, 
                                 discount_rate = discount_rate, 
                                 calculate_NPV = TRUE)
  
  isfm6_revenue_vv <- vv ((maize_yield_component6_adjusted * maize_price_per_kg) 
                          + (soybean_yield_component6_adjusted * soybean_price_per_kg),
                          var_CV = var_cv,
                          n= years)
  
  isfm6_income_vv <- (isfm6_revenue_vv- total_cost_component6)* market_risks 
  
  isfm6_income_change<- isfm6_income_vv - statusquo_income_vv 
  
  isfm6_income <- discount(isfm6_income_change, 
                           discount_rate = discount_rate, 
                           calculate_NPV = TRUE)
  
  cashflow_isfm6 <- cumsum(isfm6_income_vv)
  
  isfm6_environmental_benefit_vv <- (fixed_nitrogen 
                                     + nutrient_partial_balance_value_fertilizer_combination
                                     +soil_moisture + soil_organic_carbon_replenished
                                     + microbial_population 
                                     + reduced_soil_loss + infiltration
                                     + weed_suppression) * (organic_amendment_availability/organic_amendment_needed)
  
  isfm6_environmental_change <- isfm6_environmental_benefit_vv - statusquo_environmental_benefit_vv
  
  isfm6_environmental_benefit <- discount(isfm6_environmental_change, 
                                          discount_rate = discount_rate, 
                                          calculate_NPV = TRUE)
  
  isfm6_social_benefit_change <- isfm4_social_benefit_change # same as fertilizer combination
  isfm6_social_benefit <- isfm4_social_benefit
  
  isfm6_human_benefit_change <- isfm4_human_benefit_change # same as fertilizer combination
  isfm6_human_benefit <- isfm4_human_benefit
  
  ## To use ISFM a farmer must have some vital resources 
  #These ressources were identify leading to different farmers typology or persona# 
  #Land, finance, knowledge and labor were considered very importance ##
  #But in this study, the effect of only the first 2 ressources will be considered
  
  #### Outcomes based on land ####
  
  ## Not every farm own land in northern Ghana due to certain norms
  # There is also a high probability of land litigation due to land tenure insecurity
  #land-less farmers will have to rent land, or pay with some part of their produce to the land owner every season
  #This makes land a very volatile asset as its costs will vary highly affecting the outcomes
  
  # ISFM1 based on land
  
  # Probability of an eviction event starting from the 2 year
  eviction_probability <- c(FALSE, runif(years - 1) < land_grabbing_probability)
  
  # If eviction happens, land access is zero for the rest of the years discounting farmers profit to zero
  land_access <- cumsum(eviction_probability) == 0
  
  isfm1_productivity_land <- isfm1_productivity_change*land_access
  isfm1_productivity_land_based <- discount(isfm1_productivity_land, 
                                            discount_rate = discount_rate, 
                                            calculate_NPV = TRUE)
  
  isfm1_income_land <- (isfm1_income_vv-renting_land_cost)* land_access
  isfm1_income_land_based <- discount(isfm1_income_land, 
                                      discount_rate = discount_rate, 
                                      calculate_NPV = TRUE)
  
  isfm1_environment_land <- isfm1_environmental_change* land_access
  isfm1_environmental_benefit_land_based <- discount(isfm1_environment_land, 
                                                     discount_rate = discount_rate, 
                                                     calculate_NPV = TRUE)
  
  isfm1_social_benefit_land_based <- discount(isfm1_social_benefit_change,
                                              discount_rate = discount_rate, 
                                              calculate_NPV = TRUE)
  
  isfm1_human_benefit_land_based <- discount(isfm1_human_benefit_change,
                                             discount_rate = discount_rate, 
                                             calculate_NPV = TRUE)
  
  cashflow_isfm1_land_based <- cumsum(cashflow_isfm1- renting_land_cost)*land_access
  
  # ISFM2 based on land
  
  isfm2_productivity_land <- isfm2_productivity_change*land_access
  isfm2_productivity_land_based <- discount(isfm2_productivity_land, 
                                            discount_rate = discount_rate, 
                                            calculate_NPV = TRUE)
  
  isfm2_income_land <- (isfm2_income_vv-renting_land_cost)* land_access
  isfm2_income_land_based <- discount(isfm2_income_land, 
                                      discount_rate = discount_rate, 
                                      calculate_NPV = TRUE)
  
  isfm2_environment_land <- isfm2_environmental_change* land_access
  isfm2_environmental_benefit_land_based <- discount(isfm2_environment_land, 
                                                     discount_rate = discount_rate, 
                                                     calculate_NPV = TRUE)
  
  isfm2_social_benefit_land_based <- discount(isfm2_social_benefit_change,
                                              discount_rate = discount_rate, 
                                              calculate_NPV = TRUE)
  
  isfm2_human_benefit_land_based <- discount(isfm2_human_benefit_change,
                                             discount_rate = discount_rate, 
                                             calculate_NPV = TRUE)
  
  cashflow_isfm2_land_based <- cumsum(cashflow_isfm2- renting_land_cost)*land_access
  
  ## ISFM3 based on land 
  isfm3_productivity_land <- isfm3_productivity_change*land_access
  isfm3_productivity_land_based <- discount(isfm3_productivity_land, 
                                            discount_rate = discount_rate, 
                                            calculate_NPV = TRUE)
  
  isfm3_income_land <- (isfm3_income_vv-renting_land_cost)* land_access
  isfm3_income_land_based <- discount(isfm3_income_land, 
                                      discount_rate = discount_rate, 
                                      calculate_NPV = TRUE)
  
  isfm3_environment_land <- isfm3_environmental_change* land_access
  isfm3_environmental_benefit_land_based <- discount(isfm3_environment_land, 
                                                     discount_rate = discount_rate, 
                                                     calculate_NPV = TRUE)
  
  isfm3_social_benefit_land_based <- discount(isfm3_social_benefit_change,
                                              discount_rate = discount_rate, 
                                              calculate_NPV = TRUE)
  
  isfm3_human_benefit_land_based <- discount(isfm3_human_benefit_change,
                                             discount_rate = discount_rate, 
                                             calculate_NPV = TRUE)
  
  cashflow_isfm3_land_based <- cumsum(cashflow_isfm3- renting_land_cost)*land_access
  
  ## ISFM4 based on land 
  isfm4_productivity_land <- isfm4_productivity_change*land_access
  isfm4_productivity_land_based <- discount(isfm4_productivity_land, 
                                            discount_rate = discount_rate, 
                                            calculate_NPV = TRUE)
  
  isfm4_income_land <- (isfm4_income_vv-renting_land_cost)* land_access
  isfm4_income_land_based <- discount(isfm4_income_land, 
                                      discount_rate = discount_rate, 
                                      calculate_NPV = TRUE)
  
  isfm4_environment_land <- isfm4_environmental_change* land_access
  isfm4_environmental_benefit_land_based <- discount(isfm4_environment_land, 
                                                     discount_rate = discount_rate, 
                                                     calculate_NPV = TRUE)
  
  isfm4_social_benefit_land_based <- discount(isfm4_social_benefit_change,
                                              discount_rate = discount_rate, 
                                              calculate_NPV = TRUE)
  
  isfm4_human_benefit_land_based <- discount(isfm4_human_benefit_change,
                                             discount_rate = discount_rate, 
                                             calculate_NPV = TRUE)
  
  cashflow_isfm4_land_based <- cumsum(cashflow_isfm4- renting_land_cost)*land_access
  
  ## ISFM5 based on land 
  
  isfm5_productivity_land <- isfm5_productivity_change*land_access
  isfm5_productivity_land_based <- discount(isfm5_productivity_land, 
                                            discount_rate = discount_rate, 
                                            calculate_NPV = TRUE)
  
  isfm5_income_land <- (isfm5_income_vv-renting_land_cost)* land_access
  isfm5_income_land_based <- discount(isfm5_income_land, 
                                      discount_rate = discount_rate, 
                                      calculate_NPV = TRUE)
  
  isfm5_environment_land <- isfm5_environmental_change* land_access
  isfm5_environmental_benefit_land_based <- discount(isfm5_environment_land, 
                                                     discount_rate = discount_rate, 
                                                     calculate_NPV = TRUE)
  
  isfm5_social_benefit_land_based <- discount(isfm5_social_benefit_change,
                                              discount_rate = discount_rate, 
                                              calculate_NPV = TRUE)
  
  isfm5_human_benefit_land_based <- discount(isfm5_human_benefit_change,
                                             discount_rate = discount_rate, 
                                             calculate_NPV = TRUE)
  
  cashflow_isfm5_land_based <- cumsum(cashflow_isfm5- renting_land_cost)*land_access
  
  ## ISFM 6
  isfm6_productivity_land <- isfm6_productivity_change*land_access
  isfm6_productivity_land_based <- discount(isfm6_productivity_land, 
                                            discount_rate = discount_rate, 
                                            calculate_NPV = TRUE)
  
  isfm6_income_land <- (isfm6_income_vv-renting_land_cost)* land_access
  isfm6_income_land_based <- discount(isfm6_income_land, 
                                      discount_rate = discount_rate, 
                                      calculate_NPV = TRUE)
  
  isfm6_environment_land <- isfm6_environmental_change* land_access
  isfm6_environmental_benefit_land_based <- discount(isfm6_environment_land, 
                                                     discount_rate = discount_rate, 
                                                     calculate_NPV = TRUE)
  
  isfm6_social_benefit_land_based <- discount(isfm6_social_benefit_change,
                                              discount_rate = discount_rate, 
                                              calculate_NPV = TRUE)
  
  isfm6_human_benefit_land_based <- discount(isfm6_human_benefit_change,
                                             discount_rate = discount_rate, 
                                             calculate_NPV = TRUE)
  
  cashflow_isfm6_land_based <- cumsum(cashflow_isfm6- renting_land_cost)*land_access
  
  
  ### ISFM outcomes based on agricultural inputs availability ####
  
  #availability of agricultural inputs which is linked to finance or/and availability 
  # Only the wealthy farmers will be able to afford the inputs all the time 
  
  # Simulate the financial constraint effect
  financial_constraint_prob <- 0.70  # 10% chance of financial constraints in a season
  financial_constraint_factor <- 0.1  # Only 10% of required inputs available if constraint occurs
  
  
  financial_constraint_event <- runif(years) < financial_constraint_prob  # Random financial constraints each year
  
  # Adjust input availability for each year (based on the financial constraint event)
  wealth_probability <- ifelse(financial_constraint_event, financial_constraint_factor, 1)
  
  ## ISFM1 based on agricultural inputs availability
  
  isfm1_productivity_inputs <- isfm1_productivity_change*wealth_probability
  isfm1_productivity_inputs_based <- discount(isfm1_productivity_inputs, 
                                              discount_rate = discount_rate, 
                                              calculate_NPV = TRUE)
  
  isfm1_income_inputs <- isfm1_income_change*wealth_probability
  isfm1_income_inputs_based <- discount(isfm1_income_inputs, 
                                        discount_rate = discount_rate, 
                                        calculate_NPV = TRUE)
  
  isfm1_environment_inputs <- isfm1_environmental_change * wealth_probability
  isfm1_environmental_benefit_inputs_based <- discount(isfm1_environment_inputs, 
                                                       discount_rate = discount_rate, 
                                                       calculate_NPV = TRUE)
  
  isfm1_social_benefit_inputs_based <- discount(isfm1_social_benefit_change,
                                                discount_rate = discount_rate, 
                                                calculate_NPV = TRUE)
  
  isfm1_human_benefit_inputs <- isfm1_human_benefit_change* wealth_probability
  isfm1_human_benefit_inputs_based <- discount(isfm1_human_benefit_inputs,
                                               discount_rate = discount_rate, 
                                               calculate_NPV = TRUE)
  
  cashflow_isfm1_inputs <- cumsum(cashflow_isfm1*wealth_probability)
  
  ## ISFM 2 based on agricultural inputs
  
  isfm2_productivity_inputs <- isfm2_productivity_change*wealth_probability
  isfm2_productivity_inputs_based <- discount(isfm2_productivity_inputs, 
                                              discount_rate = discount_rate, 
                                              calculate_NPV = TRUE)
  
  isfm2_income_inputs <- isfm2_income_change*wealth_probability
  isfm2_income_inputs_based <- discount(isfm2_income_inputs, 
                                        discount_rate = discount_rate, 
                                        calculate_NPV = TRUE)
  
  isfm2_environment_inputs <- isfm2_environmental_change * wealth_probability
  isfm2_environmental_benefit_inputs_based <- discount(isfm2_environment_inputs, 
                                                       discount_rate = discount_rate, 
                                                       calculate_NPV = TRUE)
  
  isfm2_social_benefit_inputs_based <- discount(isfm2_social_benefit_change,
                                                discount_rate = discount_rate, 
                                                calculate_NPV = TRUE)
  
  isfm2_human_benefit_inputs <- isfm2_human_benefit_change* wealth_probability
  isfm2_human_benefit_inputs_based <- discount(isfm2_human_benefit_inputs,
                                               discount_rate = discount_rate, 
                                               calculate_NPV = TRUE)
  
  cashflow_isfm2_inputs <- cumsum(cashflow_isfm2*wealth_probability)
  
  ## ISFM 3 based on agricultural inputs
  
  isfm3_productivity_inputs <- isfm3_productivity_change*wealth_probability
  isfm3_productivity_inputs_based <- discount(isfm3_productivity_inputs, 
                                              discount_rate = discount_rate, 
                                              calculate_NPV = TRUE)
  
  isfm3_income_inputs <- isfm3_income_change*wealth_probability
  isfm3_income_inputs_based <- discount(isfm3_income_inputs, 
                                        discount_rate = discount_rate, 
                                        calculate_NPV = TRUE)
  
  isfm3_environment_inputs <- isfm3_environmental_change * wealth_probability
  isfm3_environmental_benefit_inputs_based <- discount(isfm3_environment_inputs, 
                                                       discount_rate = discount_rate, 
                                                       calculate_NPV = TRUE)
  
  isfm3_social_benefit_inputs_based <- discount(isfm3_social_benefit_change,
                                                discount_rate = discount_rate, 
                                                calculate_NPV = TRUE)
  
  isfm3_human_benefit_inputs <- isfm3_human_benefit_change* wealth_probability
  isfm3_human_benefit_inputs_based <- discount(isfm3_human_benefit_inputs,
                                               discount_rate = discount_rate, 
                                               calculate_NPV = TRUE)
  
  cashflow_isfm3_inputs <- cumsum(cashflow_isfm3*wealth_probability)
  
  ## ISFM 4 input based
  isfm4_productivity_inputs <- isfm4_productivity_change*wealth_probability
  isfm4_productivity_inputs_based <- discount(isfm4_productivity_inputs, 
                                              discount_rate = discount_rate, 
                                              calculate_NPV = TRUE)
  
  isfm4_income_inputs <- isfm4_income_change*wealth_probability
  isfm4_income_inputs_based <- discount(isfm4_income_inputs, 
                                        discount_rate = discount_rate, 
                                        calculate_NPV = TRUE)
  
  isfm4_environment_inputs <- isfm4_environmental_change * wealth_probability
  isfm4_environmental_benefit_inputs_based <- discount(isfm4_environment_inputs, 
                                                       discount_rate = discount_rate, 
                                                       calculate_NPV = TRUE)
  
  isfm4_social_benefit_inputs_based <- discount(isfm4_social_benefit_change,
                                                discount_rate = discount_rate, 
                                                calculate_NPV = TRUE)
  
  isfm4_human_benefit_inputs <- isfm4_human_benefit_change* wealth_probability
  isfm4_human_benefit_inputs_based <- discount(isfm4_human_benefit_inputs,
                                               discount_rate = discount_rate, 
                                               calculate_NPV = TRUE)
  
  cashflow_isfm4_inputs <- cumsum(cashflow_isfm4*wealth_probability)
  
  ## ISFM5 based on inputs 
  isfm5_productivity_inputs <- isfm5_productivity_change*wealth_probability
  isfm5_productivity_inputs_based <- discount(isfm5_productivity_inputs, 
                                              discount_rate = discount_rate, 
                                              calculate_NPV = TRUE)
  
  isfm5_income_inputs <- isfm5_income_change*wealth_probability
  isfm5_income_inputs_based <- discount(isfm5_income_inputs, 
                                        discount_rate = discount_rate, 
                                        calculate_NPV = TRUE)
  
  isfm5_environment_inputs <- isfm5_environmental_change * wealth_probability
  isfm5_environmental_benefit_inputs_based <- discount(isfm5_environment_inputs, 
                                                       discount_rate = discount_rate, 
                                                       calculate_NPV = TRUE)
  
  isfm5_social_benefit_inputs_based <- discount(isfm5_social_benefit_change,
                                                discount_rate = discount_rate, 
                                                calculate_NPV = TRUE)
  
  isfm5_human_benefit_inputs <- isfm5_human_benefit_change* wealth_probability
  isfm5_human_benefit_inputs_based <- discount(isfm5_human_benefit_inputs,
                                               discount_rate = discount_rate, 
                                               calculate_NPV = TRUE)
  
  cashflow_isfm5_inputs <- cumsum(cashflow_isfm5*wealth_probability)
  
  ## ISFM 6 input based
  
  isfm6_productivity_inputs <- isfm6_productivity_change*wealth_probability
  isfm6_productivity_inputs_based <- discount(isfm6_productivity_inputs, 
                                              discount_rate = discount_rate, 
                                              calculate_NPV = TRUE)
  
  isfm6_income_inputs <- isfm6_income_change*wealth_probability
  isfm6_income_inputs_based <- discount(isfm6_income_inputs, 
                                        discount_rate = discount_rate, 
                                        calculate_NPV = TRUE)
  
  isfm6_environment_inputs <- isfm6_environmental_change * wealth_probability
  isfm6_environmental_benefit_inputs_based <- discount(isfm6_environment_inputs, 
                                                       discount_rate = discount_rate, 
                                                       calculate_NPV = TRUE)
  
  isfm6_social_benefit_inputs_based <- discount(isfm6_social_benefit_change,
                                                discount_rate = discount_rate, 
                                                calculate_NPV = TRUE)
  
  isfm6_human_benefit_inputs <- isfm6_human_benefit_change* wealth_probability
  isfm6_human_benefit_inputs_based <- discount(isfm6_human_benefit_inputs,
                                               discount_rate = discount_rate, 
                                               calculate_NPV = TRUE)
  
  cashflow_isfm6_inputs <- cumsum(cashflow_isfm6*wealth_probability)
  
  ###Calling everything I need in the simulation and posthoc analysis##
  
  return(list(  
    #all things being equal (avalability of vital ressources)
    
    isfm1_productivity = isfm1_productivity ,
    isfm1_income = isfm1_income,
    isfm1_environmental_benefit = isfm1_environmental_benefit ,
    isfm1_social_benefit = isfm1_social_benefit ,
    isfm1_human_benefit = isfm1_human_benefit ,
    
    isfm2_productivity = isfm2_productivity ,
    isfm2_income = isfm2_income ,
    isfm2_environmental_benefit = isfm2_environmental_benefit ,
    isfm2_social_benefit = isfm2_social_benefit ,
    isfm2_human_benefit = isfm2_human_benefit,
    
    isfm3_productivity = isfm3_productivity ,
    isfm3_income = isfm3_income ,
    isfm3_environmental_benefit = isfm3_environmental_benefit ,
    isfm3_social_benefit = isfm3_social_benefit ,
    isfm3_human_benefit = isfm3_human_benefit ,
    
    isfm4_productivity = isfm4_productivity ,
    isfm4_income = isfm4_income,
    isfm4_environmental_benefit = isfm4_environmental_benefit ,
    isfm4_social_benefit = isfm4_social_benefit,
    isfm4_human_benefit = isfm4_human_benefit,
    
    isfm5_productivity = isfm5_productivity,
    isfm5_income = isfm5_income,
    isfm5_environmental_benefit = isfm5_environmental_benefit,
    isfm5_social_benefit = isfm5_social_benefit ,
    isfm5_human_benefit = isfm5_human_benefit,
    
    isfm6_productivity = isfm6_productivity ,
    isfm6_income = isfm6_income ,
    isfm6_environmental_benefit = isfm6_environmental_benefit ,
    isfm6_social_benefit = isfm6_social_benefit ,
    isfm6_human_benefit = isfm6_human_benefit,
    
    #Based on land
    
    isfm1_productivity_land_based = isfm1_productivity_land_based,
    isfm1_income_land_based = isfm1_income_land_based,
    isfm1_environmental_benefit_land_based = isfm1_environmental_benefit_land_based,
    isfm1_social_benefit_land_based = isfm1_social_benefit_land_based,
    isfm1_human_benefit_land_based = isfm1_human_benefit_land_based,
    
    isfm2_productivity_land_based = isfm2_productivity_land_based,
    isfm2_income_land_based = isfm2_income_land_based,
    isfm2_environmental_benefit_land_based = isfm2_environmental_benefit_land_based,
    isfm2_social_benefit_land_based = isfm2_social_benefit_land_based,
    isfm2_human_benefit_land_based = isfm2_human_benefit_land_based,
    
    isfm3_productivity_land_based = isfm3_productivity_land_based,
    isfm3_income_land_based = isfm3_income_land_based,
    isfm3_environmental_benefit_land_based = isfm3_environmental_benefit_land_based,
    isfm3_social_benefit_land_based = isfm3_social_benefit_land_based,
    isfm3_human_benefit_land_based = isfm3_human_benefit_land_based,
    
    isfm4_productivity_land_based = isfm4_productivity_land_based,
    isfm4_income_land_based = isfm4_income_land_based,
    isfm4_environmental_benefit_land_based = isfm4_environmental_benefit_land_based,
    isfm4_social_benefit_land_based = isfm4_social_benefit_land_based,
    isfm4_human_benefit_land_based = isfm4_human_benefit_land_based,
    
    isfm5_productivity_land_based = isfm5_productivity_land_based,
    isfm5_income_land_based = isfm5_income_land_based,
    isfm5_environmental_benefit_land_based = isfm5_environmental_benefit_land_based,
    isfm5_social_benefit_land_based = isfm5_social_benefit_land_based,
    isfm5_human_benefit_land_based = isfm5_human_benefit_land_based,
    
    isfm6_productivity_land_based = isfm6_productivity_land_based,
    isfm6_income_land_based = isfm6_income_land_based,
    isfm6_environmental_benefit_land_based = isfm6_environmental_benefit_land_based,
    isfm6_social_benefit_land_based = isfm6_social_benefit_land_based,
    isfm6_human_benefit_land_based = isfm6_human_benefit_land_based,
    
    #Based on agricultural inputs (wealth and availability)
    
    isfm1_productivity_inputs_based = isfm1_productivity_inputs_based,
    isfm1_income_inputs_based = isfm1_income_inputs_based,
    isfm1_environmental_benefit_inputs_based = isfm1_environmental_benefit_inputs_based,
    isfm1_social_benefit_inputs_based = isfm1_social_benefit_inputs_based,
    isfm1_human_benefit_inputs_based = isfm1_human_benefit_inputs_based,
    
    isfm2_productivity_inputs_based = isfm2_productivity_inputs_based,
    isfm2_income_inputs_based = isfm2_income_inputs_based,
    isfm2_environmental_benefit_inputs_based = isfm2_environmental_benefit_inputs_based,
    isfm2_social_benefit_inputs_based = isfm2_social_benefit_inputs_based,
    isfm2_human_benefit_inputs_based = isfm2_human_benefit_inputs_based,
    
    isfm3_productivity_inputs_based = isfm3_productivity_inputs_based,
    isfm3_income_inputs_based = isfm3_income_inputs_based,
    isfm3_environmental_benefit_inputs_based = isfm3_environmental_benefit_inputs_based,
    isfm3_social_benefit_inputs_based = isfm3_social_benefit_inputs_based,
    isfm3_human_benefit_inputs_based = isfm3_human_benefit_inputs_based,
    
    isfm4_productivity_inputs_based = isfm4_productivity_inputs_based,
    isfm4_income_inputs_based = isfm4_income_inputs_based,
    isfm4_environmental_benefit_inputs_based = isfm4_environmental_benefit_inputs_based,
    isfm4_social_benefit_inputs_based = isfm4_social_benefit_inputs_based,
    isfm4_human_benefit_inputs_based = isfm4_human_benefit_inputs_based,
    
    isfm5_productivity_inputs_based = isfm5_productivity_inputs_based,
    isfm5_income_inputs_based = isfm5_income_inputs_based,
    isfm5_environmental_benefit_inputs_based = isfm5_environmental_benefit_inputs_based,
    isfm5_social_benefit_inputs_based = isfm5_social_benefit_inputs_based,
    isfm5_human_benefit_inputs_based = isfm5_human_benefit_inputs_based,
    
    isfm6_productivity_inputs_based = isfm6_productivity_inputs_based,
    isfm6_income_inputs_based = isfm6_income_inputs_based,
    isfm6_environmental_benefit_inputs_based = isfm6_environmental_benefit_inputs_based,
    isfm6_social_benefit_inputs_based = isfm6_social_benefit_inputs_based,
    isfm6_human_benefit_inputs_based = isfm6_human_benefit_inputs_based,
    
    #cashflow
    
    isfm1_cashflow = cashflow_isfm1,
    isfm2_cashflow = cashflow_isfm2,
    isfm3_cashflow = cashflow_isfm3,
    isfm4_cashflow = cashflow_isfm4,
    isfm5_cashflow = cashflow_isfm5,
    isfm6_cashflow = cashflow_isfm6,
    
    isfm1_cashflow_land_based = cashflow_isfm1_land_based,
    isfm2_cashflow_land_based = cashflow_isfm2_land_based,
    isfm3_cashflow_land_based = cashflow_isfm3_land_based,
    isfm4_cashflow_land_based = cashflow_isfm4_land_based,
    isfm5_cashflow_land_based = cashflow_isfm5_land_based,
    isfm6_cashflow_land_based = cashflow_isfm6_land_based,
    
    isfm1_cashflow_inputs_based = cashflow_isfm1_inputs,
    isfm2_cashflow_inputs_based = cashflow_isfm2_inputs,
    isfm3_cashflow_inputs_based = cashflow_isfm3_inputs,
    isfm4_cashflow_inputs_based = cashflow_isfm4_inputs,
    isfm5_cashflow_inputs_based = cashflow_isfm5_inputs,
    isfm6_cashflow_inputs_based = cashflow_isfm6_inputs
    
  ))
}