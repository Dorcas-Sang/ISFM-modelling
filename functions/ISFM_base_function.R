#ISFM base function##

#This script contains the general function that will estimate the costs, the 
#benefits, the risks and the outcomes of the statusquo and the different ISFM components
#This function has arguments that will be applied later to each of the options with their specific values


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
                    nutrition_proportion= nutrition_proportion, 
                    reduced_contamination) {

grain_yield <- (maize_yield)* 2.47105 #converted from acre to hectares

# We only compare how much maize yield increase there is compared to the statusquo

#However the revenue is calculated for both maize and the intercrop (soybean)
#Because the yield in ISFM is reported for both maize and soybean each per acre,
#making them 2 acres, while the baseline is monoculture 1 acre of maize
#we need to account for land proportion 
#also because farmers do not always divide their land by halves
#Maize is mostly given priority since it is a major staple crop in Ghana for human consumption
# (Wongnaa et al., 2019: https://doi.org/10.1016/j.sciaf.2019.e00206) & (Abdulai & Soeters, 2018)
#and for livestock feed (Atakora et al., 2014; Wongnaa et al., 2019)
#while soybean is a cash crop (Abdulai & Soeters, 2018) 

land_proportion_maize <- field_area_proportion  # Maize land proportion
land_proportion_soybean <- 1 - land_proportion_maize # Remaining portion for Soybean

#Convertion of yield from kg/acres to kg/hectares  
maize_yield_proportion <- ((maize_yield * land_proportion_maize)*production_risks)* 2.47105

soybean_yield_proportion <- ((soybean_yield *land_proportion_soybean)*production_risks)* 2.47105


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

nutrition_by_income <- income * nutrition_proportion  
             #percentage of income assumed spent on nutrition
  
human_benefits <- (nutrition_by_income
                   + reduced_contamination)
  
# define the outputs of the general function based on the 5 SIAF outcomes
  return(list(productivity = grain_yield,
              economic_benefits = income, 
              environmental_benefits = soil_health,  
              social_benefits = social_benefits, 
              human_benefits = human_benefits
  ))
}
  
  