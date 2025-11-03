## Defining the decision options ##
#We are testing the outcomes of the business as usual (statusquo) compared to ISFM practices

## Statusquo (sq) #Traditional varieties and no soil amendment

### ISFM components (from 1 to 5 is partial adoption while 6 is complete ISFM) ##

## component 1. Improved germplasm (IG)
## component 2. IG + Inorganic fertilizer (IG + IF)
## component 3. IG + Organic fertilizer (IG + 0F) 
   #Organic fertilizer is manure and crop residue
## component 4. IG + OF + IF (organic and inorganic fertilizer combination)
## component 5. IG + minimum tillage (MT)
       #Minimum tillage is specific for northern Ghana soil
## component 6. IG + OF + IF + MT (complete ISFM)

#Install and loading the Decision support package
#install.packages("decisionSupport")
library(decisionSupport)

# We will use the pre-built ISFM base function to estimate the outcomes of every ISFM options
source("functions/ISFM_base_function.R")

## Load and read the data ##
ISFM_table <- read.csv("data/ISFM_inputs.csv")

## Make variable to test the model bit by bit ##
make_variables <- function(est, n= 1) {
  x <- random(rho = est,n = n)
  for(i in colnames(x)) 
    assign(i, as.numeric(x[1, i]),envir = .GlobalEnv)
}

make_variables <- make_variables (as.estimate(ISFM_table))

# The model function estimating the outcomes of every ISFM components
# (i.e. impact of ISFM on farm productivity, income, environmental, 
#social and human dimensions)#

ISFM_components_function <- function(){

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
#Given that land partitioning is never into halves we will account for this into the costs of seed
#Maize is mostly given priority since it is a major staple crop in northern Ghana
  
land_proportion_maize <- field_area_proportion  # Maize land proportion
land_proportion_soybean <- 1 - land_proportion_maize # Remaining portion for Soybean
  
maize_seed <- (maize_seed_quantity*land_proportion_maize)*price_improved_maize_seed
soybean_seed <- (soybean_seed_quantity *land_proportion_soybean)*price_improved_soybean_seed
  
isfm_inputs_cost_vv <- vv (var_mean = (maize_seed + soybean_seed)
    #here plant density is assumed the same as in the statusquo since about 85% of farmers don't follow spacing 
    #(information from extension officers)
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
#transport of inputs from market. here inputs only because most farmers produce end 
  #at farm gate with the aggregators/tradors who come to the farmers 
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
# We will discount the maximum yield with the organic fertilizer availability as risk
  
## Component 3 (Improved germplasm and organic fertilizer)
#Since farmers do not buy organic fertilizer, they use from their own farms or livestock
# (Daadi & Latacz-Lohmann, 2021:https://doi.org/10.1017/S1742170520000228)
# (Abdulai & Soeters, 2018: https://www.taylorfrancis.com/chapters/edit/10.4324/9781315149806-9/gendered-analysis-integrated-soil-fertility-management-isfm-strategy-strengthening-adaptive-capacity-ghana-tolon-district-alhassan-lansah-abdulai-sebastiaan-soeters)
#We will not have a cost attached to it but only the cost of organic fertilizer application
  
component3_inputs <- vv(compost_preparation_cost  + fertilizer_application_price_per_acre, 
        #Cost of preparing organic fertilizer mixing residue and manure before application
                          var_CV = var_cv,
                          n= years)
  
total_cost_component3<- (component3_inputs + standard_costs_ISFM)
  
  
## component 4 (IG + OF + IF) fertilizer combination ##
component4_inputs <- ((component2_inputs/2) + component3_inputs)
#mineral fertilizer is added in half compared to when it is applied alone,
#the amount of organic fertilizer that farmers have to their disposition is enough 
  
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
                        + medical_bill 
                      + labor_cost_per_season+ standard_costs_ISFM - tractor_services_cost)
  #removing tractor services from ISFM standard costs
  
total_cost_component5 <- component5_inputs + tillage_cost
  
## Component 6 (IG + OF + IF + M/ZT) ## 
#Introduction of minimum tillage on improved seed and fertilizer combination
#This is complete ISFM  
  
total_cost_component6 <- (total_cost_component5 + component4_inputs)
  
#### Benefits ####
  
### Environmental benefits of ISFM linked to soil health and land quality ###
  
##Nutrients returned to the soil (Nutrient partial balance)

nutrient_balance_organic_fertilizer <-
    vv ((nutrient_partial_balance_value_organic_fertilizer * fertilizer_price_per_bag) , 
        var_CV = var_cv, 
        n= years) # For organic fertilizer
  
nutrient_balance_mineral_fertilizer <-
    vv ((nutrient_partial_balance_value_mineral_fertilizer * fertilizer_price_per_bag) , 
        var_CV = var_cv, 
        n= years) #For mineral fertilizer
  
nutrient_balance_fertilizer_combination <-
    vv ((nutrient_partial_balance_value_fertilizer_combination * fertilizer_price_per_bag) , 
        var_CV = var_cv, 
        n= years)   #Fertilizer combination
  
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
fixed_nitrogen <- vv (total_nitrogen_fixed * nitrogen_price, 
                        var_CV = var_cv, 
                        n= years)
  
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
#for transport to get the training place since farmers are to be trained for free
knowledge <- vv(var_mean = training_cost, 
                  var_CV = var_cv, 
                  n= years)
#less need for insurance 
#because farmers will use improved seed, good agronomic practices and rotation
#They will suffer less loss 
schock_resilience <- vv(insurance_price,
                          var_CV = var_cv,
                          n= years)
  
#Social network: If more farmers are trained and do the same practice, 
#they can create more activities together that will save them some money. 
#For example helping each other in farm activities saving on labor cost 
network <- vv (var_mean = hired_labor,
                 var_CV = var_cv,
                 n= years)
  
#When ISFM is mastered, some NGOs empower farmers to teach fellow farmers
# this comes with some privileges such as being paid to be the instructor 
agency <- vv(var_mean = training_cost, 
               var_CV = var_cv, 
               n= years)
  
### Human benefits###
## The human domain is linked to health and nutrition ##
##Nutrition will be discounted by the revenue farmers generate
#Because food purchasing power depends on the money available

nutrition_proportion_by_income <- vv(nutrition_proportion,
                            var_CV = var_cv,
                            n= years)

##Reduced leaching due to Less use of mineral fertilizer
#This leads to reduced expenditure on health matters 
#because there is less contamination of drinking water and 
#less exposure to respiratory diseases or physical damage from using chemicals 
reduced_contamination <- vv(percent_contamination_reduction * yearly_health_expenditure,
                              var_CV = var_cv,
                              n= years) 

## Assigning the variables to each decision##  
  
## statusquo outcomes
#Given the context in the statusquo with maize monoculture and no soil amendment
#we use the exponential decay function on maize yield to account for yield decrease over time
#Do et al., 2020 (https://doi.org/10.1007/s13593-020-00624-5)
  
maize_exponential_decay <- function(initial_maize_yield, decay_rate, year) {
    return(initial_maize_yield * exp(-decay_rate * year))}

  statusquo_productivity <- sapply(0:(years-1), function(t) {
    maize_exponential_decay(maize_yield_statusquo, decay_rate, t)
  })
  
statusquo <-
      outcomes(total_costs= total_cost_statusquo, 
                       maize_yield= vv (statusquo_productivity, var_cv, years) ,
                       soybean_yield= 0,
                       production_risks = production_risks,
                       maize_price= maize_price_per_kg,
                       soybean_price= 0,
                       market_risks= market_risks,
                       nutrient_balance_organic_fertilizer= 0,
                       nutrient_balance_mineral_fertilizer= 0,
                       nutrient_balance_fertilizer_combination= 0,
                       reduced_soil_loss= 0,
                       soil_moisture= 0,
                       fixed_nitrogen= 0,
                       soil_organic_carbon= 0,
                       microbial_population= 0,
                       infiltration= 0,
                       weed_suppression= 0,
                       knowledge= 0,
                       schock_resilience= 0,
                       network= 0,
                       agency= 0,
                       nutrition_proportion= nutrition_proportion,
                       reduced_contamination= reduced_contamination)

statusquo_productivity_raw <- (statusquo$productivity)
statusquo_productivity_NPV <- discount(statusquo_productivity_raw, 
                                           discount_rate = discount_rate, 
                                           calculate_NPV = TRUE)

#here we account for 100% of the land for maize only 
statusquo_income_raw <- (statusquo$economic_benefits)
statusquo_income_NPV <- discount(statusquo_income_raw, 
                                           discount_rate = discount_rate, 
                                           calculate_NPV = TRUE)

statusquo_environmental_raw <- statusquo$environmental_benefits
statusquo_environmental_NPV <- discount(statusquo_environmental_raw, 
                                           discount_rate = discount_rate, 
                                           calculate_NPV = TRUE)

statusquo_social_raw <- statusquo$social_benefits
statusquo_social_NPV <- discount(statusquo_social_raw, 
                                        discount_rate = discount_rate, 
                                        calculate_NPV = TRUE)


statusquo_human_raw <- statusquo$human_benefits
statusquo_human_NPV <- discount(statusquo_human_raw, 
                                 discount_rate = discount_rate, 
                                 calculate_NPV = TRUE)

## ISFM 1 Improved germplasm 

#crop rotation of improved stress tolerant maize and soybean 
#(Abdulai & Soeters, 2018: https://www.taylorfrancis.com/chapters/edit/10.4324/9781315149806-9/gendered-analysis-integrated-soil-fertility-management-isfm-strategy-strengthening-adaptive-capacity-ghana-tolon-district-alhassan-lansah-abdulai-sebastiaan-soeters)
#Nutrient mining happens here due to continuous crop cultivation without adding soil amendment
#So the yield will decline every year and improved seed will not yield up to their potential
#(Tittonell and Giller, 2013: https://doi.org/10.1016/j.fcr.2012.10.007)

soil_degradation<- exp(-seq(0, soil_degradation_damage * years, length.out = years))

improved_seed <- 
  outcomes(total_costs= total_cost_component1, 
                       maize_yield= vv(maize_yield_component1, var_cv, years),
                       soybean_yield= vv(soybean_yield_component1, var_cv, years),
                       production_risks = production_risks,
                       maize_price= maize_price_per_kg,
                       soybean_price= soybean_price_per_kg,
                       market_risks= market_risks,
                       nutrient_balance_organic_fertilizer= 0,
                       nutrient_balance_mineral_fertilizer= 0,
                       nutrient_balance_fertilizer_combination= 0,
                       reduced_soil_loss= 0,
                       soil_moisture= 0,
                       fixed_nitrogen= fixed_nitrogen,
                       soil_organic_carbon= 0,
                       microbial_population= 0,
                       infiltration= 0,
                       weed_suppression= 0,
                       knowledge= 0,
                       schock_resilience= schock_resilience,
                       network= 0,
                       agency= 0,
                       nutrition_proportion = nutrition_proportion, 
                       reduced_contamination= reduced_contamination)  

improved_seed_productivity_raw <- (((improved_seed$productivity)*soil_degradation)
                                   - statusquo_productivity_raw)

improved_seed_productivity_NPV <- discount(improved_seed_productivity_raw, 
                                      discount_rate = discount_rate, 
                                      calculate_NPV = TRUE)

improved_seed_income_raw <- (improved_seed$economic_benefits)
                         
improved_seed_income_NPV <- discount(improved_seed_income_raw - statusquo_income_raw, 
                                 discount_rate = discount_rate, 
                                 calculate_NPV = TRUE)

improved_seed_environmental_raw <- (((improved_seed$environmental_benefits)*soil_degradation)
                                    -statusquo_environmental_raw)

improved_seed_environmental_NPV <- discount(improved_seed_environmental_raw, 
                                 discount_rate = discount_rate, 
                                 calculate_NPV = TRUE)

improved_seed_social_raw <- (improved_seed$social_benefits)
improved_seed_social_NPV <- discount(improved_seed_social_raw, 
                                 discount_rate = discount_rate, 
                                 calculate_NPV = TRUE)

improved_seed_human_raw <- ((improved_seed$human_benefits)- statusquo_human_raw)
improved_seed_human_NPV <- discount(improved_seed_human_raw, 
                                discount_rate = discount_rate, 
                                calculate_NPV = TRUE)

### ISFM 2 mineral fertilizer ###

#With mineral fertilizer, there are short term benefits but in the long run yield start declining
#because mineral fertilizer alone is not sustainable and leads to soil degradation 
# such as nutrient depletion and loss of organic matter, acidification, and chemical pollution (Titonell & Giller, 2013)
#these phenomenon are cumulative, not linear and in many cases happen simultaneously 
#(Johannes K et al. 2015: https://www.boell.de/sites/default/files/WWF_Mineralduenger_englisch_WEB.pdf)
#(Atakora et al., 2014 : Response of maize growth and development to mineral fertilizer and soil characteristics in Northern Ghana)


mineral_fertilizer <- 
     outcomes(total_costs= total_cost_component2, 
         maize_yield= vv(maize_yield_component2, var_cv, years),
         soybean_yield= vv(soybean_yield_component2, var_cv, years),
         production_risks = production_risks,
         maize_price= maize_price_per_kg,
         soybean_price= soybean_price_per_kg,
         market_risks= market_risks,
         nutrient_balance_organic_fertilizer= 0,
         nutrient_balance_mineral_fertilizer= nutrient_balance_mineral_fertilizer,
         nutrient_balance_fertilizer_combination= 0,
         reduced_soil_loss= 0,
         soil_moisture= 0,
         fixed_nitrogen= fixed_nitrogen,
         soil_organic_carbon= 0,
         microbial_population= 0,
         infiltration= 0,
         weed_suppression= 0,
         knowledge= knowledge,
         schock_resilience= schock_resilience,
         network= 0,
         agency= 0,
         nutrition_proportion = nutrition_proportion,
         reduced_contamination= 0)  

mineral_fertilizer_productivity_raw <- (((mineral_fertilizer$productivity)*soil_degradation)
                                  - statusquo_productivity_raw)

mineral_fertilizer_productivity_NPV <- discount(mineral_fertilizer_productivity_raw, 
                                           discount_rate = discount_rate, 
                                           calculate_NPV = TRUE)

mineral_fertilizer_income_raw <- (mineral_fertilizer$economic_benefits)

mineral_fertilizer_income_NPV <- discount(mineral_fertilizer_income_raw - statusquo_income_raw, 
                                     discount_rate = discount_rate, 
                                     calculate_NPV = TRUE)

mineral_fertilizer_environmental_raw <- (((mineral_fertilizer$environmental_benefits)*soil_degradation)
                                         -statusquo_environmental_raw)
mineral_fertilizer_environmental_NPV <- discount(mineral_fertilizer_environmental_raw, 
                                            discount_rate = discount_rate, 
                                            calculate_NPV = TRUE)

mineral_fertilizer_social_raw <- (mineral_fertilizer$social_benefits)+ 50 ##### remove this
mineral_fertilizer_social_NPV <- discount(mineral_fertilizer_social_raw, 
                                     discount_rate = discount_rate, 
                                     calculate_NPV = TRUE)

mineral_fertilizer_human_raw <- ((mineral_fertilizer$human_benefits)
                                      - statusquo_human_raw)
mineral_fertilizer_human_NPV <- discount(mineral_fertilizer_human_raw, 
                                    discount_rate = discount_rate, 
                                    calculate_NPV = TRUE)

## ISFM 3 Organic fertilizer## 

#With organic fertilizer there is competing interest leading 
#to farmers not having enough organic fertilizer for optimum yield
#(Abdulai & Soeters, 2018)
## Unavailability of organic amendment as risks to maximum yield 
#Crop residue and manure contribute to reviving the land, 
#and since environmental benefits are linked to the land 
# this unavailability could affect the yield and environmental benefits
#in components where organic amendment are to be used

organic_amendement_risks <- chance_event(organic_amendment_availability,
                        value_if = 1-percentage_damage_organic_fertilizer_unavailabilty,
                        value_if_not = 1,
                        n= years)

organic_fertilizer <- 
  outcomes(total_costs= total_cost_component3, 
           maize_yield= vv(maize_yield_component3, var_cv, years),
           soybean_yield= vv(soybean_yield_component3,var_cv, years),
           production_risks = production_risks,
           maize_price= maize_price_per_kg,
           soybean_price= soybean_price_per_kg,
           market_risks= market_risks,
           nutrient_balance_organic_fertilizer=nutrient_balance_organic_fertilizer ,
           nutrient_balance_mineral_fertilizer= 0,
           nutrient_balance_fertilizer_combination= 0,
           reduced_soil_loss= reduced_soil_loss,
           soil_moisture= soil_moisture,
           fixed_nitrogen= fixed_nitrogen,
           soil_organic_carbon= soil_organic_carbon_replenished,
           microbial_population= microbial_population,
           infiltration= 0,
           weed_suppression= 0,
           knowledge= knowledge,
           schock_resilience= schock_resilience,
           network= 0,
           agency= agency,
           nutrition_proportion = nutrition_proportion,
           reduced_contamination= reduced_contamination)  

organic_fertilizer_productivity_raw <- (((organic_fertilizer$productivity)*organic_amendement_risks)
                              - statusquo_productivity_raw)

organic_fertilizer_productivity_NPV <- discount(organic_fertilizer_productivity_raw, 
                                                discount_rate = discount_rate, 
                                                calculate_NPV = TRUE)

organic_fertilizer_income_raw <- ((organic_fertilizer$economic_benefits)*organic_amendement_risks)
                                          
organic_fertilizer_income_NPV <- discount(organic_fertilizer_income_raw - statusquo_income_raw , 
                                          discount_rate = discount_rate, 
                                          calculate_NPV = TRUE)

organic_fertilizer_environmental_raw <- (((organic_fertilizer$environmental_benefits)*organic_amendement_risks)
                                          - statusquo_environmental_raw)

organic_fertilizer_environmental_NPV <- discount(organic_fertilizer_environmental_raw, 
                                                 discount_rate = discount_rate, 
                                                 calculate_NPV = TRUE)

organic_fertilizer_social_raw <- (organic_fertilizer$social_benefits)
organic_fertilizer_social_NPV <- discount(organic_fertilizer_social_raw, 
                                          discount_rate = discount_rate, 
                                          calculate_NPV = TRUE)

organic_fertilizer_human_raw <- ((organic_fertilizer$human_benefits)- statusquo_human_raw)
organic_fertilizer_human_NPV <- discount(organic_fertilizer_human_raw, 
                                         discount_rate = discount_rate, 
                                         calculate_NPV = TRUE)

### ISFM 4 fertilizer combination ###
#Because each type of fertilizer (organic or inorganic) has its unique characteristic and benefit, 
#combining the two will have synergistic benefits that will be more beneficial than applying one alone on improved seed.
#(Vanlawe et al., 2010: https://doi.org/10.5367/0000000107911699) 

fertilizer_combination <- 
  outcomes(total_costs= total_cost_component4, 
           maize_yield= vv(maize_yield_component4, var_cv, years),
           soybean_yield= vv(soybean_yield_component4, var_cv, years),
           production_risks = production_risks,
           maize_price= maize_price_per_kg,
           soybean_price= soybean_price_per_kg,
           market_risks= market_risks,
           nutrient_balance_organic_fertilizer=0,
           nutrient_balance_mineral_fertilizer= 0,
           nutrient_balance_fertilizer_combination= nutrient_balance_fertilizer_combination,
           reduced_soil_loss= reduced_soil_loss,
           soil_moisture= soil_moisture,
           fixed_nitrogen= fixed_nitrogen,
           soil_organic_carbon= soil_organic_carbon_replenished,
           microbial_population= microbial_population,
           infiltration= 0,
           weed_suppression= 0,
           knowledge= knowledge,
           schock_resilience= schock_resilience,
           network= network,
           agency= agency,
           nutrition_proportion = nutrition_proportion,
           reduced_contamination= reduced_contamination)  

fertilizer_combination_productivity_raw <- ((fertilizer_combination$productivity)
                                            - statusquo_productivity_raw)
fertilizer_combination_productivity_NPV <- discount(fertilizer_combination_productivity_raw, 
                                                discount_rate = discount_rate, 
                                                calculate_NPV = TRUE)

fertilizer_combination_income_raw <- (fertilizer_combination$economic_benefits)
                                    
fertilizer_combination_income_NPV <- discount(fertilizer_combination_income_raw - statusquo_income_raw, 
                                          discount_rate = discount_rate, 
                                          calculate_NPV = TRUE)

fertilizer_combination_environmental_raw <- ((fertilizer_combination$environmental_benefits)
                                      - statusquo_environmental_raw)

fertilizer_combination_environmental_NPV <- discount(fertilizer_combination_environmental_raw, 
                                                 discount_rate = discount_rate, 
                                                 calculate_NPV = TRUE)

fertilizer_combination_social_raw <- (fertilizer_combination$social_benefits)-10
                                  
fertilizer_combination_social_NPV <- discount(fertilizer_combination_social_raw, 
                                          discount_rate = discount_rate, 
                                          calculate_NPV = TRUE)

fertilizer_combination_human_raw <- (fertilizer_combination$human_benefits)
                                     

fertilizer_combination_human_NPV <- discount(fertilizer_combination_human_raw , 
                                         discount_rate = discount_rate, 
                                         calculate_NPV = TRUE)

### ISFM 5 minimum tillage ###
minimum_tillage <- 
  outcomes(total_costs= total_cost_component5, 
           maize_yield= vv(maize_yield_component5, var_cv, years),
           soybean_yield= vv(soybean_yield_component5, var_cv, years),
           production_risks = production_risks,
           maize_price= maize_price_per_kg,
           soybean_price= soybean_price_per_kg,
           market_risks= market_risks,
           nutrient_balance_organic_fertilizer=0 ,
           nutrient_balance_mineral_fertilizer= 0,
           nutrient_balance_fertilizer_combination= 0,
           reduced_soil_loss= reduced_soil_loss,
           soil_moisture= 0,
           fixed_nitrogen= fixed_nitrogen,
           soil_organic_carbon= 0,
           microbial_population= 0,
           infiltration= infiltration,
           weed_suppression= weed_suppression,
           knowledge= knowledge,
           schock_resilience= schock_resilience,
           network= 0,
           agency= 0,
           nutrition_proportion = nutrition_proportion,
           reduced_contamination= reduced_contamination)  

minimum_tillage_productivity_raw <- (((minimum_tillage$productivity)*soil_degradation)
                                    - statusquo_productivity_raw)
minimum_tillage_productivity_NPV <- discount(minimum_tillage_productivity_raw, 
                                                    discount_rate = discount_rate, 
                                                    calculate_NPV = TRUE)

minimum_tillage_income_raw <- (minimum_tillage$economic_benefits)
                              
minimum_tillage_income_NPV <- discount(minimum_tillage_income_raw - statusquo_income_raw , 
                                              discount_rate = discount_rate, 
                                              calculate_NPV = TRUE)

minimum_tillage_environmental_raw <- (((minimum_tillage$environmental_benefits)*soil_degradation)
                                          - statusquo_environmental_raw)
minimum_tillage_environmental_NPV <- discount(minimum_tillage_environmental_raw, 
                                                     discount_rate = discount_rate, 
                                                     calculate_NPV = TRUE)

minimum_tillage_social_raw <- (minimum_tillage$social_benefits)
                                
minimum_tillage_social_NPV <- discount(minimum_tillage_social_raw , 
                                              discount_rate = discount_rate, 
                                              calculate_NPV = TRUE)

minimum_tillage_human_raw <- ((fertilizer_combination$human_benefits)
                                - statusquo_human_raw)
minimum_tillage_human_NPV <- discount(minimum_tillage_human_raw, 
                                             discount_rate = discount_rate, 
                                             calculate_NPV = TRUE)
### ISFM 6 complete ISFM ###
complete_isfm <- 
  outcomes(total_costs= total_cost_component6, 
           maize_yield= vv(maize_yield_component6, var_cv, years),
           soybean_yield= vv(soybean_yield_component6, var_cv, years),
           production_risks = production_risks,
           maize_price= maize_price_per_kg,
           soybean_price= soybean_price_per_kg,
           market_risks= market_risks,
           nutrient_balance_organic_fertilizer=0 ,
           nutrient_balance_mineral_fertilizer= 0,
           nutrient_balance_fertilizer_combination= nutrient_balance_fertilizer_combination,
           reduced_soil_loss= reduced_soil_loss,
           soil_moisture= soil_moisture,
           fixed_nitrogen= fixed_nitrogen,
           soil_organic_carbon= soil_organic_carbon_replenished,
           microbial_population= microbial_population,
           infiltration= infiltration,
           weed_suppression= weed_suppression,
           knowledge= knowledge,
           schock_resilience= schock_resilience,
           network= network,
           agency= agency,
           nutrition_proportion = nutrition_proportion,
           reduced_contamination= reduced_contamination)  

complete_isfm_productivity_raw <- ((complete_isfm$productivity)
                                   - statusquo_productivity_raw)

complete_isfm_productivity_NPV <- discount(complete_isfm_productivity_raw, 
                                             discount_rate = discount_rate, 
                                             calculate_NPV = TRUE)

complete_isfm_income_raw <- (complete_isfm$economic_benefits)

complete_isfm_income_NPV <- discount(complete_isfm_income_raw - statusquo_income_raw, 
                                       discount_rate = discount_rate, 
                                       calculate_NPV = TRUE)

complete_isfm_environmental_raw <- ((complete_isfm$environmental_benefits)
                                - statusquo_environmental_raw)
complete_isfm_environmental_NPV <- discount(complete_isfm_environmental_raw, 
                                              discount_rate = discount_rate, 
                                              calculate_NPV = TRUE)

complete_isfm_social_raw <- (complete_isfm$social_benefits)
complete_isfm_social_NPV <- discount(complete_isfm_social_raw , 
                                       discount_rate = discount_rate, 
                                       calculate_NPV = TRUE)

complete_isfm_human_raw <- ((complete_isfm$human_benefits)- statusquo_human_raw)
complete_isfm_human_NPV <- discount(complete_isfm_human_raw , 
                                      discount_rate = discount_rate, 
                                      calculate_NPV = TRUE)


#### Farmers typology based on resources ###
## To use the most beneficial ISFM, a farmer must have some vital resources 
#These resources were identify leading to different farmers typology or persona# 
#Land, finance, knowledge and labor were considered very important##
#But in this study, the effect of only the first 4 resources will be considered#
#Each for men and women

### ISFM economic outcomes when all ressources are available ###
#incidence of domestic violence with ISFM due to crop residue use (Abudlai & Soeters, 2018)

gender_based_violence <- chance_event(gender_based_violence_probability,
                                      value_if = 1-percentage_gender_based_violence,
                                      value_if_not = 1,
                                      n= years)

men_complete_isfm_income <- complete_isfm_income_raw
NPV_men_complete_isfm_income <- discount(men_complete_isfm_income , 
                                         discount_rate = discount_rate, 
                                         calculate_NPV = TRUE)

women_complete_isfm_income <- complete_isfm_income_raw*gender_based_violence
NPV_women_complete_isfm_income <- discount(women_complete_isfm_income , 
                                         discount_rate = discount_rate, 
                                         calculate_NPV = TRUE)


#### Economic outcomes based on land ####

## Not every farmer own land in northern Ghana due to certain norms
#Land access is also influenced by farmers gender (intersectionality)
#There is also a high probability of land litigation due to land tenure insecurity
#land-less farmers will have to rent land, or pay with some part of their produce to the land owner every season
#Additionally women are given the least fertile land by the men
#This makes land a very volatile asset as its costs will vary highly affecting the outcomes
# We model the probability of an eviction event starting from year 2
eviction_probability <- c(FALSE, runif(years - 1) < land_grabbing_probability)

# If eviction happens, land access is zero for the rest of the years discounting farmers profit to zero
land_access <- cumsum(eviction_probability) == 0

men_complete_isfm_income_land_based <-(men_complete_isfm_income - renting_land_cost)* land_access
NPV_men_complete_isfm_income_land_based <- discount(men_complete_isfm_income_land_based , 
                                                    discount_rate = discount_rate, 
                                                    calculate_NPV = TRUE)

#In the case of women other than the eviction probability, the quality also matters
#women get less income on their land compared to what men get due to the status of their land

women_income_land <- (women_complete_isfm_income- renting_land_cost)* land_access

women_complete_isfm_income_land_based <-(women_income_land)*land_degradation_effect
NPV_women_complete_isfm_income_land_based <- discount(women_complete_isfm_income_land_based , 
                                                    discount_rate = discount_rate, 
                                                    calculate_NPV = TRUE)


### ISFM economic outcomes based on agricultural inputs availability####

#availability of agricultural inputs is influenced by finance
#Only the able and well connected farmers will be able to afford the inputs at the appropriate time 
# Atakora et al., 2014 
# (Zingori et al., 2011)
# Physical limitation would also be a problem in access to inputs (Thelma et al., 2017)


# We first simulate the financial constraint effect
  # 80% chance of financial constraints in a season
financial_constraint_factor <- runif(years, financial_constraint_effect) 

# percentage of required inputs available if financial constraint occurs
financial_constraint_event <- runif(years) < financial_constraint_probability 

# Based on the financial constraints event, we adjust input availability for each year
wealth_probability <- ifelse(financial_constraint_event, financial_constraint_factor, 1)

#men
men_complete_isfm_income_inputs_based <- men_complete_isfm_income * wealth_probability
NPV_men_complete_isfm_income_inputs_based <- discount(men_complete_isfm_income_inputs_based , 
                                                    discount_rate = discount_rate, 
                                                    calculate_NPV = TRUE)

#women
# Due to their low financial power women's input availability is further reduced 
#so they get even fewer inputs compared to men when there are financial constraints in a season
#Also when there are financial constraints women mostly prioritize household needs compared to men 
#and this may further affect the quantity and quality of inputs they have to use for their farms

women_inputs_access_by_financial_power <- runif(years, women_inputs_access)

women_income_inputs <- women_complete_isfm_income* wealth_probability
  
women_complete_isfm_income_inputs_based <- (women_income_inputs
                                            *women_inputs_access_by_financial_power)
NPV_women_complete_isfm_income_inputs_based <- discount(women_complete_isfm_income_inputs_based , 
                                                    discount_rate = discount_rate, 
                                                    calculate_NPV = TRUE)

#### Knowledge is an important resource in adoption of ISFM ####
#If farmers use ISFM required inputs without following the right advice and knowing the proper inputs to use
#they will make mistakes and waste resources which may affect their benefits
#however over the simulation farmers learn by doing after making mistakes, so this has a learning curve
#Ideally they are supposed to be trained before they adopt but for those who can't access training
#they learn by doing or seeing from their peers so they start with high mistakes but improve over time

# Probability of making mistakes which is higher at the beginning and gets lower over time with practice
knowledge_risk_probability_timed <- seq(knowledge_risk_probability, 
                                        knowledge_risk_probability * knowledge_risk_reduction, 
                      #reduction to the risk over time as the knowledge is mastered with practice
                                        length.out = years)
# Adjusting knowledge risk
knowledge_risk <- chance_event(knowledge_risk_probability_timed, 
                                          value_if = 1 - percentage_knowledge_risk_damage, 
                                          value_if_not = 1, 
                                          n = years)

#men 
men_complete_isfm_income_knowledge_based <- men_complete_isfm_income* knowledge_risk
NPV_men_complete_isfm_income_knowledge_based <- discount(men_complete_isfm_income_knowledge_based , 
                                                    discount_rate = discount_rate, 
                                                    calculate_NPV = TRUE)

#women 
#For women learning time is quite different because most women are not educated 
#so they have to rely on men for knowledge transfer
#Which puts a delay in their activities as men will first prioritize their own farms
#So there is mostly missed opportunity to do some activities at the right time
#This will be in the initial phase until they master to do it on their own

missed_opportunity <- chance_event(delayed_activity_probability, 
                                   value_if = 1 - delay_effect, 
                                   value_if_not = 1, 
                                   n = years)

women_income_knowledge <- women_complete_isfm_income * knowledge_risk

women_complete_isfm_income_knowledge_based <- (women_income_knowledge
                                              * missed_opportunity)
NPV_women_complete_isfm_income_knowledge_based <- 
                    discount(women_complete_isfm_income_knowledge_based , 
                                      discount_rate = discount_rate, 
                                              calculate_NPV = TRUE)

## Based on labor 
#Shortage of labor can affect timing of most activities
#Men can manage to do most activities quickly compared to women
#They are also connected to farmers in other communities where they can pay for additional labor force
#There also some activities that are highly gendered (wedding, spraying)
#and are considered male activities so women will again depend on men 

labor_shortage_risk <- chance_event(labor_access_probability,
                                    value_if =1- multiplier_labor_access, 
                                    value_if_not = 1,
                                    n= years)

men_complete_isfm_income_labor_based <- men_complete_isfm_income*labor_shortage_risk
#men can cultivate their own land without requiring external help when there is labor shortage.
#farming is all they do unlike women who have other additional household activities

NPV_men_complete_isfm_income_labor_based <- discount(men_complete_isfm_income_labor_based , 
                                                    discount_rate = discount_rate, 
                                                    calculate_NPV = TRUE)

women_income_labor <- ((women_complete_isfm_income- labor_cost_per_season)*labor_shortage_risk)
#women have care-giving roles so this increases pressure on them and they need to have more people helping them 
#in the farm so they may need to hire or feed the people helping them (direct source)
#in case this help also does not come on time as most farmers are busy at the same time
#they may miss on the right time of planting activities

women_complete_isfm_income_labor_based <- (women_income_labor
                                      * missed_opportunity)

NPV_women_complete_isfm_income_labor_based <- discount(women_complete_isfm_income_labor_based , 
                                                     discount_rate = discount_rate, 
                                                     calculate_NPV = TRUE)

  
return(list(
  
  ## We are adding the baseline/statusquo so the decision-maker would see 
  #what they are comparing ISFM with
  
  Baseline_productivity= statusquo_productivity_NPV ,
  Baseline_income = statusquo_income_NPV, 
  Baseline_environmental= statusquo_environmental_NPV,
  Baseline_social = statusquo_social_NPV,
  Baseline_human = statusquo_human_NPV,
  
  ##when all resources are available
  
  Improved_seed_productivity= improved_seed_productivity_NPV ,
  Improved_seed_income = improved_seed_income_NPV, 
  Improved_seed_environmental= improved_seed_environmental_NPV,
  Improved_seed_social = improved_seed_social_NPV,
  Improved_seed_human = improved_seed_human_NPV,
  
  Mineral_fertilizer_productivity = mineral_fertilizer_productivity_NPV,
  Mineral_fertilizer_income = mineral_fertilizer_income_NPV,
  Mineral_fertilizer_environmental= mineral_fertilizer_environmental_NPV,
  Mineral_fertilizer_social = mineral_fertilizer_social_NPV,
  Mineral_fertilizer_human = mineral_fertilizer_human_NPV,
  
  Organic_fertilizer_productivity = organic_fertilizer_productivity_NPV,
  Organic_fertilizer_income = organic_fertilizer_income_NPV,
  Organic_fertilizer_environmental= organic_fertilizer_environmental_NPV,
  Organic_fertilizer_social = organic_fertilizer_social_NPV,
  Organic_fertilizer_human = organic_fertilizer_human_NPV,
  
  Fertilizer_combination_productivity = fertilizer_combination_productivity_NPV,
  Fertilizer_combination_income = fertilizer_combination_income_NPV,
  Fertilizer_combination_environmental= fertilizer_combination_environmental_NPV,
  Fertilizer_combination_social = fertilizer_combination_social_NPV,
  Fertilizer_combination_human = fertilizer_combination_human_NPV,
  
  Minimum_tillage_productivity = minimum_tillage_productivity_NPV,
  Minimum_tillage_income = minimum_tillage_income_NPV, 
  Minimum_tillage_environmental = minimum_tillage_environmental_NPV, 
  Minimum_tillage_social = minimum_tillage_social_NPV, 
  Minimum_tillage_human = minimum_tillage_human_NPV, 
  
  
  Complete_ISFM_productivity = complete_isfm_productivity_NPV,
  Complete_ISFM_income = complete_isfm_income_NPV,
  Complete_ISFM_environmental= complete_isfm_environmental_NPV,
  Complete_ISFM_social = complete_isfm_social_NPV,
  Complete_ISFM_human = complete_isfm_human_NPV, 
  
  #Net present values for complete ISFM on different farmer archetype
  
NPV_women_ISFM_income = NPV_women_complete_isfm_income,
NPV_men_ISFM_income = NPV_men_complete_isfm_income,
NPV_women_ISFM_income_land_based = NPV_women_complete_isfm_income_land_based,
NPV_men_ISFM_income_land_based = NPV_men_complete_isfm_income_land_based,
NPV_women_ISFM_income_inputs_based = NPV_women_complete_isfm_income_inputs_based,
NPV_men_ISFM_income_inputs_based = NPV_men_complete_isfm_income_inputs_based,
NPV_women_ISFM_income_knowledge_based = NPV_women_complete_isfm_income_knowledge_based,
NPV_men_ISFM_income_knowledge_based = NPV_men_complete_isfm_income_knowledge_based,
NPV_women_ISFM_income_labor_based = NPV_women_complete_isfm_income_labor_based,
NPV_men_ISFM_income_labor_based = NPV_men_complete_isfm_income_labor_based,


#ISFM components cashflow

Baseline = statusquo_income_raw,
                            

Improved_seed = improved_seed_income_raw,
                                   

Mineral_fertilizer  = mineral_fertilizer_income_raw,
                                        
                                        
Organic_fertilizer  = organic_fertilizer_income_raw,
                                        

Fertilizer_combination  = fertilizer_combination_income_raw,
                                            


Minimum_tillage  = minimum_tillage_income_raw,
                                     

Complete_ISFM  = complete_isfm_income_raw,

# Cashflow complete ISFM when all resources are available 
 
  men_cashflow_ISFM = men_complete_isfm_income, 
  women_cashflow_ISFM = women_complete_isfm_income,
  
# Cashflow based on Land

  men_cashflow_ISFM_land_based =  men_complete_isfm_income_land_based,
  women_cashflow_ISFM_land_based = women_complete_isfm_income_land_based,
  
  #Cashflow based on inputs availability
  men_cashflow_ISFM_inputs_based = men_complete_isfm_income_inputs_based,
  women_cashflow_ISFM_inputs_based = women_complete_isfm_income_inputs_based,
  
  #Cashflow based on knowledge
  men_cashflow_ISFM_knowledge_based = men_complete_isfm_income_knowledge_based,
  women_cashflow_ISFM_knowledge_based = women_complete_isfm_income_knowledge_based,
  
  #Cashflow based on labor
 men_cashflow_ISFM_labor_based = men_complete_isfm_income_labor_based,
 women_cashflow_ISFM_labor_based = women_complete_isfm_income_labor_based
 
))
       
}


