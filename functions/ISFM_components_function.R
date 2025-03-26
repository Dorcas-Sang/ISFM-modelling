# We will use the ISFM base function to estimate the outcomes of every ISFM options
source("functions/ISFM_base_function.R")

# The model function estimating the outcomes of every ISFM components
# (i.e. impact of ISFM on farm productivity, income, environmental, 
#social and human dimensions)#

ISFM_components_function <- function(){
  
## statusquo outcomes
#Given the context in the statusquo with maize monoculture and no soil amendment
#we use the exponential decay function on maize yield to account for yield decrease over time
  
maize_exponential_decay <- function(initial_maize_yield, decay_rate, year) {
    return(initial_maize_yield * exp(-decay_rate * year))}

  statusquo_productivity <- sapply(0:(years-1), function(t) {
    maize_exponential_decay(maize_yield_statusquo, decay_rate, t)
  })
  
statusquo <-
      outcomes(total_costs= total_cost_statusquo, 
                       maize_yield= statusquo_productivity,
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
                       nutrition= nutrition,
                       reduced_contamination= reduced_contamination)

statusquo_productivity_raw <- ((statusquo$productivity)/land_proportion_soybean+land_proportion_maize)
#here we account for 100% of the land for maize only 
statusquo_income_raw <- statusquo$economic_benefits
statusquo_environmental_raw <- statusquo$environmental_benefits
statusquo_social_raw <- statusquo$social_benefits
statusquo_human_raw <- statusquo$human_benefits

## ISFM 1 Improved germplasm 

improved_seed <- 
  outcomes(total_costs= total_cost_component1, 
                       maize_yield= maize_yield_component1,
                       soybean_yield= soybean_yield_component1,
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
                       nutrition= nutrition,
                       reduced_contamination= reduced_contamination)  

improved_seed_productivity_raw <- ((improved_seed$productivity)- statusquo_productivity_raw)
improved_seed_productivity_NPV <- discount(improved_seed_productivity_raw, 
                                      discount_rate = discount_rate, 
                                      calculate_NPV = TRUE)

improved_seed_income_raw <- ((improved_seed$economic_benefits)- statusquo_income_raw)

improved_seed_income_NPV <- discount(improved_seed_income_raw, 
                                 discount_rate = discount_rate, 
                                 calculate_NPV = TRUE)

improved_seed_environmental_raw <- ((improved_seed$environmental_benefits)-statusquo_environmental_raw)
improved_seed_environmental_NPV <- discount(improved_seed_environmental_raw, 
                                 discount_rate = discount_rate, 
                                 calculate_NPV = TRUE)

improved_seed_social_raw <- ((improved_seed$social_benefits)-statusquo_social_raw)
improved_seed_social_NPV <- discount(improved_seed_social_raw, 
                                 discount_rate = discount_rate, 
                                 calculate_NPV = TRUE)

improved_seed_human_raw <- ((improved_seed$human_benefits)- statusquo_human_raw)
improved_seed_human_NPV <- discount(improved_seed_human_raw, 
                                discount_rate = discount_rate, 
                                calculate_NPV = TRUE)

### ISFM 2 mineral fertilizer ###

#With mineral fertilizer, there are short term benefits but in the long run yield start declining
#because mineral fertilizer alone is not sustainable
#farmers have to increase the amount of mineral fertilizer to be able to maintain the same yields (fertilizer treadmill)
#these factors are cumulative and not linear

mineral_fertilizer_treadmill<- exp(-seq(0, damage_soil_acidification * years, length.out = years))
#due to soil acidification

mineral_fertilizer <- 
     outcomes(total_costs= total_cost_component2, 
         maize_yield= maize_yield_component2,
         soybean_yield= soybean_yield_component2,
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
         nutrition= nutrition,
         reduced_contamination= 0)  

mineral_fertilizer_productivity_raw <- (((mineral_fertilizer$productivity)*mineral_fertilizer_treadmill)
                                  - statusquo_productivity_raw)
mineral_fertilizer_productivity_NPV <- discount(mineral_fertilizer_productivity_raw, 
                                           discount_rate = discount_rate, 
                                           calculate_NPV = TRUE)

mineral_fertilizer_income_raw <- (((mineral_fertilizer$economic_benefits)*mineral_fertilizer_treadmill)- statusquo_income_raw)
mineral_fertilizer_income_NPV <- discount(mineral_fertilizer_income_raw, 
                                     discount_rate = discount_rate, 
                                     calculate_NPV = TRUE)

mineral_fertilizer_environmental_raw <- ((mineral_fertilizer$environmental_benefits)-statusquo_environmental_raw)
mineral_fertilizer_environmental_NPV <- discount(mineral_fertilizer_environmental_raw, 
                                            discount_rate = discount_rate, 
                                            calculate_NPV = TRUE)

mineral_fertilizer_social_raw <- ((mineral_fertilizer$social_benefits)- statusquo_social_raw)
mineral_fertilizer_social_NPV <- discount(mineral_fertilizer_social_raw, 
                                     discount_rate = discount_rate, 
                                     calculate_NPV = TRUE)

mineral_fertilizer_human_raw <- ((mineral_fertilizer$human_benefits)*mineral_fertilizer_treadmill)
                                      - statusquo_human_raw
mineral_fertilizer_human_NPV <- discount(mineral_fertilizer_human_raw, 
                                    discount_rate = discount_rate, 
                                    calculate_NPV = TRUE)

## ISFM 3 Organic fertilizer## 

#With organic fertilizer there is competing interest leading 
#to farmers not having enough organic fertilizer for optimum yield
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
           maize_yield= maize_yield_component3,
           soybean_yield= soybean_yield_component3,
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
           network= network,
           agency= 0,
           nutrition= nutrition,
           reduced_contamination= reduced_contamination)  

organic_fertilizer_productivity_raw <- (((organic_fertilizer$productivity)*organic_amendement_risks)
                              - statusquo_productivity_raw)

organic_fertilizer_productivity_NPV <- discount(organic_fertilizer_productivity_raw, 
                                                discount_rate = discount_rate, 
                                                calculate_NPV = TRUE)

organic_fertilizer_income_raw <- (((organic_fertilizer$economic_benefits)*organic_amendement_risks) 
                                          - statusquo_income_raw)

organic_fertilizer_income_NPV <- discount(organic_fertilizer_income_raw , 
                                          discount_rate = discount_rate, 
                                          calculate_NPV = TRUE)

organic_fertilizer_environmental_raw <- (((organic_fertilizer$environmental_benefits)*organic_amendement_risks)
                                          - statusquo_environmental_raw)

organic_fertilizer_environmental_NPV <- discount(organic_fertilizer_environmental_raw, 
                                                 discount_rate = discount_rate, 
                                                 calculate_NPV = TRUE)

organic_fertilizer_social_raw <- ((organic_fertilizer$social_benefits)- statusquo_social_raw)
organic_fertilizer_social_NPV <- discount(organic_fertilizer_social_raw, 
                                          discount_rate = discount_rate, 
                                          calculate_NPV = TRUE)

organic_fertilizer_human_raw <- ((organic_fertilizer$human_benefits)- statusquo_human_raw)
organic_fertilizer_human_NPV <- discount(organic_fertilizer_human_raw, 
                                         discount_rate = discount_rate, 
                                         calculate_NPV = TRUE)

### ISFM 4 fertilizer combination ###
fertilizer_combination <- 
  outcomes(total_costs= total_cost_component4, 
           maize_yield= maize_yield_component4,
           soybean_yield= soybean_yield_component4,
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
           infiltration= 0,
           weed_suppression= 0,
           knowledge= knowledge,
           schock_resilience= schock_resilience,
           network= network,
           agency= agency,
           nutrition= nutrition,
           reduced_contamination= reduced_contamination)  

fertilizer_combination_productivity_raw <- ((fertilizer_combination$productivity)
                                            - statusquo_productivity_raw)
fertilizer_combination_productivity_NPV <- discount(fertilizer_combination_productivity_raw, 
                                                discount_rate = discount_rate, 
                                                calculate_NPV = TRUE)

fertilizer_combination_income_raw <- ((fertilizer_combination$economic_benefits)
                                    - statusquo_income_raw)
fertilizer_combination_income_NPV <- discount(fertilizer_combination_income_raw, 
                                          discount_rate = discount_rate, 
                                          calculate_NPV = TRUE)

fertilizer_combination_environmental_raw <- ((fertilizer_combination$environmental_benefits)
                                      - statusquo_environmental_raw)
fertilizer_combination_environmental_NPV <- discount(fertilizer_combination_environmental_raw, 
                                                 discount_rate = discount_rate, 
                                                 calculate_NPV = TRUE)

fertilizer_combination_social_raw <- ((fertilizer_combination$social_benefits)
                                  - statusquo_social_raw)
fertilizer_combination_social_NPV <- discount(fertilizer_combination_social_raw, 
                                          discount_rate = discount_rate, 
                                          calculate_NPV = TRUE)

fertilizer_combination_human_raw <- ((fertilizer_combination$human_benefits)
                                    - statusquo_human_raw)
fertilizer_combination_human_NPV <- discount(fertilizer_combination_human_raw , 
                                         discount_rate = discount_rate, 
                                         calculate_NPV = TRUE)

### ISFM 5 minimum tillage ###
minimum_tillage <- 
  outcomes(total_costs= total_cost_component5, 
           maize_yield= maize_yield_component5,
           soybean_yield= soybean_yield_component5,
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
           microbial_population= microbial_population,
           infiltration= infiltration,
           weed_suppression= weed_suppression,
           knowledge= knowledge,
           schock_resilience= schock_resilience,
           network= network,
           agency= 0,
           nutrition= nutrition,
           reduced_contamination= reduced_contamination)  

minimum_tillage_productivity_raw <- ((minimum_tillage$productivity)
                                    - statusquo_productivity_raw)
minimum_tillage_productivity_NPV <- discount(minimum_tillage_productivity_raw, 
                                                    discount_rate = discount_rate, 
                                                    calculate_NPV = TRUE)

minimum_tillage_income_raw <- ((minimum_tillage$economic_benefits)
                              - statusquo_income_raw)
minimum_tillage_income_NPV <- discount(minimum_tillage_income_raw , 
                                              discount_rate = discount_rate, 
                                              calculate_NPV = TRUE)

minimum_tillage_environmental_raw <- ((minimum_tillage$environmental_benefits)
                                          - statusquo_environmental_raw)
minimum_tillage_environmental_NPV <- discount(minimum_tillage_environmental_raw, 
                                                     discount_rate = discount_rate, 
                                                     calculate_NPV = TRUE)

minimum_tillage_social_raw <- ((minimum_tillage$social_benefits)
                                - statusquo_social_raw)
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
           maize_yield= maize_yield_component6,
           soybean_yield= soybean_yield_component6,
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
           nutrition= nutrition,
           reduced_contamination= reduced_contamination)  

complete_isfm_productivity_raw <- ((complete_isfm$productivity)
                                   - statusquo_productivity_raw)
complete_isfm_productivity_NPV <- discount(complete_isfm_productivity_raw, 
                                             discount_rate = discount_rate, 
                                             calculate_NPV = TRUE)

complete_isfm_income_raw <- ((complete_isfm$economic_benefits)- statusquo_income_raw)
complete_isfm_income_NPV <- discount(complete_isfm_income_raw , 
                                       discount_rate = discount_rate, 
                                       calculate_NPV = TRUE)

complete_isfm_environmental_raw <- ((complete_isfm$environmental_benefits)
                                - statusquo_environmental_raw)
complete_isfm_environmental_NPV <- discount(complete_isfm_environmental_raw, 
                                              discount_rate = discount_rate, 
                                              calculate_NPV = TRUE)

complete_isfm_social_raw <- ((complete_isfm$social_benefits)- statusquo_social_raw)
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
#But in this study, the effect of only the first 3 resources will be considered#

#### Economic outcomes based on land ####

## Not every farm own land in northern Ghana due to certain norms
# There is also a high probability of land litigation due to land tenure insecurity
#land-less farmers will have to rent land, or pay with some part of their produce to the land owner every season
#This makes land a very volatile asset as its costs will vary highly affecting the outcomes
# We model the probability of an eviction event starting from year 2
eviction_probability <- c(FALSE, runif(years - 1) < land_grabbing_probability)

# If eviction happens, land access is zero for the rest of the years discounting farmers profit to zero
land_access <- cumsum(eviction_probability) == 0

improved_seed_income_land_based_raw <- (improved_seed_income_raw - renting_land_cost)* land_access
improved_seed_income_land_based <- discount(improved_seed_income_land_based_raw,
                                    discount_rate = discount_rate, 
                                  calculate_NPV = TRUE)

mineral_fertilizer_income_land_based_raw <- (mineral_fertilizer_income_raw - renting_land_cost)* land_access
mineral_fertilizer_income_land_based <-
                 discount(mineral_fertilizer_income_land_based_raw,
                 discount_rate = discount_rate, 
                  calculate_NPV = TRUE)

organic_fertilizer_income_land_based_raw <-(organic_fertilizer_income_raw - renting_land_cost)* land_access
organic_fertilizer_income_land_based <-
        discount(organic_fertilizer_income_land_based_raw,
         discount_rate = discount_rate, 
         calculate_NPV = TRUE)

fertilizer_combination_income_land_based_raw <- (fertilizer_combination_income_raw - renting_land_cost)* land_access
fertilizer_combination_income_land_based <- discount(fertilizer_combination_income_land_based_raw,
                                  discount_rate = discount_rate, 
                                  calculate_NPV = TRUE)   

minimum_tillage_income_land_based_raw <- (minimum_tillage_income_raw - renting_land_cost)* land_access
minimum_tillage_income_land_based <- discount(minimum_tillage_income_land_based_raw,
                                      discount_rate = discount_rate, 
                                       calculate_NPV = TRUE)

complete_isfm_income_land_based_raw <-(complete_isfm_income_raw - renting_land_cost)* land_access
complete_isfm_income_land_based <- discount(complete_isfm_income_land_based_raw,
                                    discount_rate = discount_rate, 
                                    calculate_NPV = TRUE)


### ISFM economic outcomes based on agricultural inputs availability####

#availability of agricultural inputs is influenced by finance
#Only the able and well connected farmers will be able to afford the inputs all the time 

# We first simulate the financial constraint effect
  # 80% chance of financial constraints in a season
financial_constraint_factor <- runif(years, financial_constraint_effect) 

# percentage of required inputs available if financial constraint occurs
financial_constraint_event <- runif(years) < financial_constraint_probability 

# Based on the financial constraints event, we adjust input availability for each year
wealth_probability <- ifelse(financial_constraint_event, financial_constraint_factor, 1)

improved_seed_income_inputs_based_raw <- improved_seed_income_raw* wealth_probability
improved_seed_income_inputs_based <-
           discount(improved_seed_income_inputs_based_raw,
           discount_rate = discount_rate, 
           calculate_NPV = TRUE)

mineral_fertilizer_income_inputs_based_raw <- mineral_fertilizer_income_raw* wealth_probability
mineral_fertilizer_income_inputs_based <- 
          discount(mineral_fertilizer_income_inputs_based_raw,
           discount_rate = discount_rate, 
           calculate_NPV = TRUE)

organic_fertilizer_income_inputs_based_raw <- organic_fertilizer_income_raw*wealth_probability
organic_fertilizer_income_inputs_based <- 
            discount(organic_fertilizer_income_inputs_based_raw,
           discount_rate = discount_rate, 
           calculate_NPV = TRUE)

fertilizer_combination_income_inputs_based_raw <- fertilizer_combination_income_raw*wealth_probability
fertilizer_combination_income_inputs_based <- 
          discount(fertilizer_combination_income_inputs_based_raw,
           discount_rate = discount_rate, 
           calculate_NPV = TRUE)

minimum_tillage_income_inputs_based_raw <- minimum_tillage_income_raw*wealth_probability
minimum_tillage_income_inputs_based <- 
            discount(minimum_tillage_income_inputs_based_raw,
           discount_rate = discount_rate, 
           calculate_NPV = TRUE)

complete_isfm_income_inputs_based_raw <- complete_isfm_income_raw* wealth_probability
complete_isfm_income_inputs_based <-
          discount(complete_isfm_income_inputs_based_raw,
           discount_rate = discount_rate, 
           calculate_NPV = TRUE)

#### Knowledge is an important resource in adoption of ISFM ####
#If farmers use ISFM required inputs without following the right advice and knowing the proper inputs to use
#they will make mistakes and waste resources which may affect their benefits
#however over the simulation farmers learn by doing after making mistakes, so this has a learning curve
#Ideally they are supposed to be trained before they adopt but for those who can't access training
#they learn by doing or seeing from their peers so they start with high mistakes but improve over time 

isfm_learning_curve <- seq(0.3, 0.9, length.out = years)  # ISFM learning curve 

# Probability of making mistakes which is higher at the beginning and gets lower over time with practice
knowledge_risk_probability_timed <- seq(knowledge_risk_probability, 
                                        knowledge_risk_probability * 0.5, #reduction to the risk over time
                                        length.out = years)
# Adjusting knowledge risk
knowledge_risk_occurrence <- chance_event(knowledge_risk_probability_timed, 
                                          value_if = 1 - percentage_knowledge_risk_damage, 
                                          value_if_not = 1, 
                                          n = years)

# We use the learning curve so that mistakes are less damaging over time as farmers learn by doing
knowledge_risk <- knowledge_risk_occurrence * isfm_learning_curve

  
improved_seed_income_knowledge_based_raw <- improved_seed_income_raw* knowledge_risk
improved_seed_income_knowledge_based <-
  discount(improved_seed_income_knowledge_based_raw,
           discount_rate = discount_rate, 
           calculate_NPV = TRUE)

mineral_fertilizer_income_knowledge_based_raw <- mineral_fertilizer_income_raw* knowledge_risk
mineral_fertilizer_income_knowledge_based <- 
  discount(mineral_fertilizer_income_knowledge_based_raw,
           discount_rate = discount_rate, 
           calculate_NPV = TRUE)

organic_fertilizer_income_knowledge_based_raw <- organic_fertilizer_income_raw* knowledge_risk
organic_fertilizer_income_knowledge_based <- 
  discount(organic_fertilizer_income_knowledge_based_raw,
           discount_rate = discount_rate, 
           calculate_NPV = TRUE)

fertilizer_combination_income_knowledge_based_raw <- fertilizer_combination_income_raw* knowledge_risk
fertilizer_combination_income_knowledge_based <- 
  discount(fertilizer_combination_income_knowledge_based_raw,
           discount_rate = discount_rate, 
           calculate_NPV = TRUE)

minimum_tillage_income_knowledge_based_raw <- minimum_tillage_income_raw* knowledge_risk
minimum_tillage_income_knowledge_based <- 
  discount(minimum_tillage_income_knowledge_based_raw,
           discount_rate = discount_rate, 
           calculate_NPV = TRUE)

complete_isfm_income_knowledge_based_raw <- complete_isfm_income_raw* knowledge_risk
complete_isfm_income_knowledge_based <-
  discount(complete_isfm_income_knowledge_based_raw,
           discount_rate = discount_rate, 
           calculate_NPV = TRUE)
  
return(list(
  ##when all resources are available
  improved_seed_productivity= improved_seed_productivity_NPV ,
  improved_seed_income = improved_seed_income_NPV, 
  improved_seed_environmental= improved_seed_environmental_NPV,
  improved_seed_social = improved_seed_social_NPV,
  improved_seed_human = improved_seed_human_NPV,
  
  mineral_fertilizer_productivity = mineral_fertilizer_productivity_NPV,
  mineral_fertilizer_income = mineral_fertilizer_income_NPV,
  mineral_fertilizer_environmental= mineral_fertilizer_environmental_NPV,
  mineral_fertilizer_social = mineral_fertilizer_social_NPV,
  mineral_fertilizer_human = mineral_fertilizer_human_NPV,
  
  organic_fertilizer_productivity = organic_fertilizer_productivity_NPV,
  organic_fertilizer_income = organic_fertilizer_income_NPV,
  organic_fertilizer_environmental= organic_fertilizer_environmental_NPV,
  organic_fertilizer_social = organic_fertilizer_social_NPV,
  organic_fertilizer_human = organic_fertilizer_human_NPV,
  
  fertilizer_combination_productivity = fertilizer_combination_productivity_NPV,
  fertilizer_combination_income = fertilizer_combination_income_NPV,
  fertilizer_combination_environmental= fertilizer_combination_environmental_NPV,
  fertilizer_combination_social = fertilizer_combination_social_NPV,
  fertilizer_combination_human = fertilizer_combination_human_NPV,
  
  tillage_productivity = minimum_tillage_productivity_NPV,
  tillage_income = minimum_tillage_income_NPV,
  tillage_environmental= minimum_tillage_environmental_NPV,
  tillage_social = minimum_tillage_social_NPV,
  tillage_human = minimum_tillage_human_NPV,
  
  complete_ISFM_productivity = complete_isfm_productivity_NPV,
  complete_ISFM_income = complete_isfm_income_NPV,
  complete_ISFM_environmental= complete_isfm_environmental_NPV,
  complete_ISFM_social = complete_isfm_social_NPV,
  complete_ISFM_human = complete_isfm_human_NPV, 
  
  ## Economic return based on land 
  
  improved_seed_income_land_based = improved_seed_income_land_based,
  mineral_fertilizer_income_land_based = mineral_fertilizer_income_land_based,
  organic_fertilizer_income_land_based = organic_fertilizer_income_land_based,
  fertilizer_combination_income_land_based = fertilizer_combination_income_land_based,
  tillage_income_land_based = minimum_tillage_income_land_based,
  complete_ISFM_income_land_based = complete_isfm_income_land_based,
 
  ## Economic benefits based on inputs
  
  improved_seed_income_inputs_based = improved_seed_income_inputs_based,
  mineral_fertilizer_income_inputs_based = mineral_fertilizer_income_inputs_based,
  organic_fertilizer_income_inputs_based = organic_fertilizer_income_inputs_based,
  fertilizer_combination_income_inputs_based = fertilizer_combination_income_inputs_based,
  tillage_income_inputs_based = minimum_tillage_income_inputs_based,
  complete_isfm_income_inputs_based = complete_isfm_income_inputs_based,
  
  #Economic return based on knowledge
  
  improved_seed_income_knowledge_based = improved_seed_income_knowledge_based,
  mineral_fertilizer_income_knowledge_based = mineral_fertilizer_income_knowledge_based,
  organic_fertilizer_income_knowledge_based = organic_fertilizer_income_knowledge_based,
  fertilizer_combination_income_knowledge_based = fertilizer_combination_income_knowledge_based,
  tillage_income_knowledge_based = minimum_tillage_income_knowledge_based,
  complete_isfm_income_knowledge_based = complete_isfm_income_knowledge_based,
  
  #cashflow
  
  improved_seed = improved_seed_income_raw,
  mineral_fertilizer= mineral_fertilizer_income_raw,
  organic_fertilizer = (organic_fertilizer_income_raw),
  fertilizer_combination = (fertilizer_combination_income_raw),
  tillage =  (minimum_tillage_income_raw),
  complete_ISFM = (complete_isfm_income_raw), 
  
  improved_seed_land_based = (improved_seed_income_land_based_raw),
  mineral_fertilizer_land_based=  (mineral_fertilizer_income_land_based_raw),
  organic_fertilizer_land_based =  (organic_fertilizer_income_land_based_raw),
  fertilizer_combination_land_based =  (fertilizer_combination_income_land_based_raw),
  tillage_land_based = (minimum_tillage_income_land_based_raw),
  complete_ISFM_land_based =  (complete_isfm_income_land_based_raw),
  
  improved_seed_inputs_based = (improved_seed_income_inputs_based_raw),
  mineral_fertilizer_inputs_based= (mineral_fertilizer_income_inputs_based_raw),
  organic_fertilizer_inputs_based = (organic_fertilizer_income_inputs_based_raw),
  fertilizer_combination_inputs_based = (fertilizer_combination_income_inputs_based_raw),
  tillage_inputs_based = (minimum_tillage_income_inputs_based_raw),
  complete_ISFM_inputs_based = (complete_isfm_income_inputs_based_raw),
  
  improved_seed_knowledge_based = (improved_seed_income_knowledge_based_raw),
  mineral_fertilizer_knowledge_based= (mineral_fertilizer_income_knowledge_based_raw),
  organic_fertilizer_knowledge_based = (organic_fertilizer_income_knowledge_based_raw),
  fertilizer_combination_knowledge_based = (fertilizer_combination_income_knowledge_based_raw),
  tillage_knowledge_based = (minimum_tillage_income_knowledge_based_raw),
  complete_ISFM_knowledge_based = (complete_isfm_income_knowledge_based_raw)
  
))
       
}

