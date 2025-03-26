
#OLD STUFF 

after mc we will transform the data for visualization using z-score transformation

z_score <- function(x) {
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}


isfm1_normalized<- z_score(isfm1_productivity_change)




## Labor is also another vital ressource as more component will demand increase in labor force
# Farmers with less household member will be most likely affected,especially the one with many other off farms activity
#This may limit the time they dedicate to farming and affect their outcomes

#labor_risks <- chance_event(additional_labor_probability,
#value_if = additional_labor_cost,
#value_if_not = 1)




#There is competing interest for residue use 
#Sustainable practices are also know to increase decision making power of farmers 
#Gender based violence (GBV) might occur as a result of all these factors
#affecting women economic outcome as well

gender_based_violence <- 
  chance_event(gender_based_violence_probability,
               value_if = 1-percentage_gender_based_violence,
               value_if_not = 1,
               n= years)


## Probability and cost of outsourcing crop residue 
crop_residue_cost <- chance_event(probability_crop_residue_availability,
                                  value_if = 0,
                                  #Zero cost to buy residue in this case because farmers left some for farming
                                  value_if_not = crop_residue_price_per_kg * crop_residue_quantity_needed,
                                  # When farmers have to outsource so they could apply organic fertilizer
                                  n= years)

## Probability of paying for manure and the cost
manure_cost <- chance_event(probability_of_owning_livestock,
                            value_if = 0,
                            #Zero cost to buy manure in this case
                            value_if_not = manure_price,
                            #When farmers have to outsource manure because they don't own livestock
                            n= years)




#### Component 1 outcomes ####
isfm1_productivity_vv <- vv (maize_yield_component1 + soybean_yield_component1, 
                             var_CV = var_cv, 
                             n= years)* field_area_proportion #yield (kg/acre)

#to find percentage change on productivity compared to the statusquo
isfm1_productivity_percentage <- ((isfm1_productivity_vv - statusquo_productivity_vv)
                                  /statusquo_productivity_vv)*100

isfm1_productivity_land <- discount(isfm1_productivity_percentage, 
                                    discount_rate = discount_rate, 
                                    calculate_NPV = TRUE)* land_risks

isfm1_productivity_wealth <- discount(isfm1_productivity_percentage, 
                                      discount_rate = discount_rate, 
                                      calculate_NPV = TRUE)* wealth_risks

isfm1_productivity_knowledge <- discount(isfm1_productivity_percentage, 
                                         discount_rate = discount_rate, 
                                         calculate_NPV = TRUE)* knowledge_risks

isfm1_productivity_labor <- discount(isfm1_productivity_percentage, 
                                     discount_rate = discount_rate, 
                                     calculate_NPV = TRUE)* labor_risks


isfm1_income_vv <- vv (((maize_yield_component1 * maize_price_per_kg) 
                        + (soybean_yield_component1 * soybean_price_per_kg))* field_area_proportion
                       - total_cost_component1,
                       var_CV = var_cv,
                       n= years) * market_risks

#To find the percentage change on income compared to the statusquo
isfm1_income_percentage <- ((isfm1_income_vv - statusquo_income_vv)/statusquo_income_vv)*100

isfm1_income_land <- discount(isfm1_income_percentage, 
                              discount_rate = discount_rate, 
                              calculate_NPV = TRUE)*land_risks

isfm1_income_wealth <- discount(isfm1_income_percentage, 
                                discount_rate = discount_rate, 
                                calculate_NPV = TRUE)*wealth_risks

isfm1_income_knowledge <- discount(isfm1_income_percentage, 
                                   discount_rate = discount_rate, 
                                   calculate_NPV = TRUE)*knowledge_risks

isfm1_income_labor <- discount(isfm1_income_percentage, 
                               discount_rate = discount_rate, 
                               calculate_NPV = TRUE)*labor_risks

#cashflow isfm1 based on resources
cashflow_isfm1_based_on_land <- discount (isfm1_income_vv,
                                          discount_rate = discount_rate, 
                                          calculate_NPV = FALSE)*land_risk

cashflow_isfm1_based_on_wealth <- discount (isfm1_income_vv,
                                            discount_rate = discount_rate, 
                                            calculate_NPV = FALSE)* wealth_risk

cashflow_isfm1_based_on_knowledge <- discount (isfm1_income_vv,
                                               discount_rate = discount_rate, 
                                               calculate_NPV = FALSE)* knowledge_risk

cashflow_isfm1_based_on_labor <- discount (isfm1_income_vv,
                                           discount_rate = discount_rate, 
                                           calculate_NPV = FALSE)* labor_risk


cumulative_cashflow_isfm1_land <- cumsum(cashflow_isfm1_based_on_land)
cumulative_cashflow_isfm1_wealth <- cumsum(cashflow_isfm1_based_on_wealth)
cumulative_cashflow_isfm1_knowledge <- cumsum(cashflow_isfm1_based_on_knowledge)
cumulative_cashflow_isfm1_wealth <- cumsum(cashflow_isfm1_based_on_labor)


isfm1_environnmental_percentage <- ((fixed_nitrogen - statusquo_environmental_benefit)
                                    /statusquo_environmental_benefit)*100

isfm1_environmental_benefit <- discount(isfm1_environnmental_percentage, 
                                        discount_rate = discount_rate, 
                                        calculate_NPV = TRUE)

isfm1_social_benefit_percentage <- ((schock_resilience - statusquo_social_benefit)
                                    /statusquo_social_benefit)*100

isfm1_social_benefit <- discount(isfm1_social_benefit_percentage,
                                 discount_rate = discount_rate, 
                                 calculate_NPV = TRUE)

isfm1_human_benefit_percentage <- (((nutrition + reduced_contamination)
                                    - statusquo_human_benefit_vv)/statusquo_human_benefit_vv)* 100

isfm1_human_benefit <- discount(isfm1_human_benefit_percentage,
                                discount_rate = discount_rate, 
                                calculate_NPV = TRUE)


#### Component 2 outcomes ####

isfm2_productivity_vv <- vv (maize_yield_component2 + soybean_yield_component2, 
                             var_CV = var_cv, 
                             n= years) #yield (kg/acre)

isfm2_productivity_percentage <- ((isfm2_productivity_vv - statusquo_productivity_vv)
                                  /statusquo_productivity_vv) * 100

isfm2_productivity <- discount(isfm2_productivity_percentage, 
                               discount_rate = discount_rate, 
                               calculate_NPV = TRUE)

isfm2_income_vv <- vv ((maize_yield_component2 * maize_price_per_kg) 
                       + (soybean_yield_component2 * soybean_price_per_kg)-
                         total_cost_component2,
                       var_CV = var_cv,
                       n= years)* market_risks

isfm2_income_percentage <- ((isfm2_income_vv - statusquo_income_vv)/statusquo_income_vv)*100

isfm2_income <- discount(isfm2_income_percentage, discount_rate = discount_rate, 
                         calculate_NPV = TRUE)

cashflow_isfm2_based_on_land <- discount (isfm2_income_vv,
                                          discount_rate = discount_rate, 
                                          calculate_NPV = FALSE)*land_risk

cashflow_isfm2_based_on_wealth <- discount (isfm2_income_vv,
                                            discount_rate = discount_rate, 
                                            calculate_NPV = FALSE)* wealth_risk

cashflow_isfm2_based_on_knowledge <- discount (isfm2_income_vv,
                                               discount_rate = discount_rate, 
                                               calculate_NPV = FALSE)* knowledge_risk

cashflow_isfm2_based_on_labor <- discount (isfm2_income_vv,
                                           discount_rate = discount_rate, 
                                           calculate_NPV = FALSE)* labor_risk


cumulative_cashflow_isfm2_land <- cumsum(cashflow_isfm2_based_on_land)
cumulative_cashflow_isfm2_wealth <- cumsum(cashflow_isfm2_based_on_wealth)
cumulative_cashflow_isfm2_knowledge <- cumsum(cashflow_isfm2_based_on_knowledge)
cumulative_cashflow_isfm2_wealth <- cumsum(cashflow_isfm2_based_on_labor)


isfm2_environmental_benefit_vv <- (fixed_nitrogen 
                                   + soil_nutrient_replenished_mineral_fertilizer)

isfm2_environmental_benefit_percentage <- ((isfm2_environmental_benefit_vv - statusquo_environmental_benefit)
                                           /statusquo_environmental_benefit)*100  


isfm2_environmental_benefit <- discount(isfm2_environmental_benefit_percentage, 
                                        discount_rate = discount_rate, 
                                        calculate_NPV = TRUE)

isfm2_social_benefit_vv <- (value_of_knowledge_gained
                            + schock_resilience)

isfm2_social_benefit_percentage <- ((isfm2_social_benefit_vv - statusquo_social_benefit)
                                    / statusquo_social_benefit) *100      

isfm2_social_benefit <- discount(isfm2_social_benefit_percentage, 
                                 discount_rate = discount_rate, 
                                 calculate_NPV = TRUE)

#nutrition as the only human benefit because application 
#of mineral fertilizer leads to contamination of ground water and health

isfm2_human_benefit_percentage <- ((nutrition - statusquo_human_benefit)
                                   /statusquo_human_benefit)* 100

isfm2_human_benefit <- discount(isfm2_human_benefit_percentage,
                                discount_rate = discount_rate, 
                                calculate_NPV = TRUE)

#### Component 3 ####

isfm3_productivity_vv <- vv (maize_yield_component3 + soybean_yield_component3, 
                             var_CV = var_cv, 
                             n= years)  #yield (kg/acre)

isfm3_productivity_percentage <- ((isfm3_productivity_vv - statusquo_productivity_vv)
                                  /statusquo_productivity_vv)* 100

isfm3_productivity <- discount(isfm3_productivity_percentage, 
                               discount_rate = discount_rate, 
                               calculate_NPV = TRUE)

isfm3_income_vv <- vv ((maize_yield_component3 * maize_price_per_kg) 
                       + (soybean_yield_component3 * soybean_price_per_kg)-
                         total_cost_component3,
                       var_CV = var_cv,
                       n= years)* market_risks

isfm3_income_percentage <- ((isfm3_income_vv - statusquo_income_vv)/
                              statusquo_income_vv)*100

isfm3_income <- discount(isfm3_income_percentage, 
                         discount_rate = discount_rate, 
                         calculate_NPV = TRUE)

cashflow_isfm3_based_on_land <- discount (isfm3_income_vv,
                                          discount_rate = discount_rate, 
                                          calculate_NPV = FALSE)*land_risk

cashflow_isfm3_based_on_wealth <- discount (isfm3_income_vv,
                                            discount_rate = discount_rate, 
                                            calculate_NPV = FALSE)* wealth_risk

cashflow_isfm3_based_on_knowledge <- discount (isfm3_income_vv,
                                               discount_rate = discount_rate, 
                                               calculate_NPV = FALSE)* knowledge_risk

cashflow_isfm3_based_on_labor <- discount (isfm3_income_vv,
                                           discount_rate = discount_rate, 
                                           calculate_NPV = FALSE)* labor_risk


cumulative_cashflow_isfm3_land <- cumsum(cashflow_isfm3_based_on_land)
cumulative_cashflow_isfm3_wealth <- cumsum(cashflow_isfm3_based_on_wealth)
cumulative_cashflow_isfm3_knowledge <- cumsum(cashflow_isfm3_based_on_knowledge)
cumulative_cashflow_isfm3_wealth <- cumsum(cashflow_isfm3_based_on_labor)

isfm3_environmental_benefit_vv <- (fixed_nitrogen + soil_moisture 
                                   + soil_organic_carbon_replenished
                                   + microbial_population 
                                   + nutrient_partial_balance_value_organic_fertilizer             
                                   + reduced_soil_loss) * crop_residue_risks


isfm3_environmental_benefit_percentage <- ((isfm3_environmental_benefit_vv - statusquo_environmental_benefit)
                                           /statusquo_environmental_benefit)* 100


isfm3_environmental_benefit <- discount(isfm3_environmental_benefit_percentage, 
                                        discount_rate = discount_rate, 
                                        calculate_NPV = TRUE)


isfm3_social_benefit_vv <- (value_of_knowledge_gained
                            + schock_resilience)

isfm3_social_benefit_percentage <- ((isfm3_social_benefit_vv - statusquo_social_benefit)
                                    / statusquo_social_benefit) *100

isfm3_social_benefit <- discount(isfm3_social_benefit_percentage, 
                                 discount_rate = discount_rate, 
                                 calculate_NPV = TRUE)

isfm3_human_benefit_percentage <- ((reduced_contamination - statusquo_human_benefit)
                                   #less contamination because of the introduction of organic fertilizer which reduces leaching
                                   #and high exposure to mineral fertilizer
                                   /statusquo_human_benefit) * 100

isfm3_human_benefit <- discount(isfm3_human_benefit_percentage, 
                                discount_rate = discount_rate, 
                                calculate_NPV = TRUE)


#### Component 4 outcomes ####

isfm4_productivity_vv <- vv (maize_yield_component4 + soybean_yield_component4, 
                             var_CV = var_cv, 
                             n= years)  #yield (kg/acre)

isfm4_productivity_percentage <- ((isfm4_productivity_vv - statusquo_productivity_vv)
                                  / statusquo_productivity_vv) *100

isfm4_productivity <- discount(isfm4_productivity_percentage, 
                               discount_rate = discount_rate, 
                               calculate_NPV = TRUE)
isfm4_income_vv <- vv ((maize_yield_component4 * maize_price_per_kg) 
                       + (soybean_yield_component4 * soybean_price_per_kg)-
                         total_cost_component4,
                       var_CV = var_cv,
                       n= years)* market_risks

isfm4_income_percentage <- ((isfm4_income_vv - statusquo_income_vv)
                            /statusquo_income_vv) * 100

isfm4_income <- discount(isfm4_income_percentage, 
                         discount_rate = discount_rate, 
                         calculate_NPV = TRUE)

cashflow_isfm4_based_on_land <- discount (isfm4_income_vv,
                                          discount_rate = discount_rate, 
                                          calculate_NPV = FALSE)*land_risk

cashflow_isfm4_based_on_wealth <- discount (isfm4_income_vv,
                                            discount_rate = discount_rate, 
                                            calculate_NPV = FALSE)* wealth_risk

cashflow_isfm4_based_on_knowledge <- discount (isfm4_income_vv,
                                               discount_rate = discount_rate, 
                                               calculate_NPV = FALSE)* knowledge_risk

cashflow_isfm4_based_on_labor <- discount (isfm4_income_vv,
                                           discount_rate = discount_rate, 
                                           calculate_NPV = FALSE)* labor_risk


cumulative_cashflow_isfm4_land <- cumsum(cashflow_isfm4_based_on_land)
cumulative_cashflow_isfm4_wealth <- cumsum(cashflow_isfm4_based_on_wealth)
cumulative_cashflow_isfm4_knowledge <- cumsum(cashflow_isfm4_based_on_knowledge)
cumulative_cashflow_isfm4_wealth <- cumsum(cashflow_isfm4_based_on_labor)

isfm4_environmental_benefit_vv <- (fixed_nitrogen 
                                   + nutrient_partial_balance_value_fertilizer_combination 
                                   +soil_moisture + soil_organic_carbon_replenished
                                   + microbial_population 
                                   + reduced_soil_loss) * crop_residue_risks

isfm4_environmental_benefit_percentage <- ((isfm4_environmental_benefit_vv - statusquo_environmental_benefit)
                                           / statusquo_environmental_benefit) * 100                                          


isfm4_environmental_benefit <- discount(isfm4_environmental_benefit_percentage, 
                                        discount_rate = discount_rate, 
                                        calculate_NPV = TRUE)

isfm4_social_benefit_vv <- (network + value_of_knowledge_gained
                            + schock_resilience)

isfm4_social_benefit_percentage <- ((isfm4_social_benefit_vv - statusquo_social_benefit)
                                    / statusquo_social_benefit) * 100

isfm4_social_benefit <- discount(isfm4_social_benefit_percentage, 
                                 discount_rate = discount_rate, 
                                 calculate_NPV = TRUE)

isfm4_human_benefit_percentage <- (((reduced_contamination + nutrition) - statusquo_human_benefit)
                                   #increase food availability, less contamination because of the synergy of the 2 fertilizer type
                                   /statusquo_human_benefit) * 100

isfm4_human_benefit <- discount(isfm4_human_benefit_percentage, 
                                discount_rate = discount_rate, 
                                calculate_NPV = TRUE)


return_list
land_based_isfm1_cashflow = cashflow_isfm1_based_on_land,
wealth_based_isfm1_cashflow = cashflow_isfm1_based_on_wealth,
knowledge_based_isfm1_cashflow = cashflow_isfm1_based_on_knowledge,
labor_based_isfm1_cashflow = cashflow_isfm1_based_on_labor,

land_based_isfm2_cashflow = cashflow_isfm2_based_on_land,
wealth_based_isfm2_cashflow = cashflow_isfm2_based_on_wealth,
knowledge_based_isfm2_cashflow = cashflow_isfm2_based_on_knowledge,
labor_based_isfm2_cashflow = cashflow_isfm2_based_on_labor,

land_based_isfm3_cashflow = cashflow_isfm3_based_on_land,
wealth_based_isfm3_cashflow = cashflow_isfm3_based_on_wealth,
knowledge_based_isfm3_cashflow = cashflow_isfm3_based_on_knowledge,
labor_based_isfm3_cashflow = cashflow_isfm3_based_on_labor,

land_based_isfm4_cashflow = cashflow_isfm4_based_on_land,
wealth_based_isfm4_cashflow = cashflow_isfm4_based_on_wealth,
knowledge_based_isfm4_cashflow = cashflow_isfm4_based_on_knowledge,
labor_based_isfm4_cashflow = cashflow_isfm4_based_on_labor,

land_based_isfm5_cashflow = cashflow_isfm5_based_on_land,
wealth_based_isfm5_cashflow = cashflow_isfm5_based_on_wealth,
knowledge_based_isfm5_cashflow = cashflow_isfm5_based_on_knowledge,
labor_based_isfm5_cashflow = cashflow_isfm5_based_on_labor,

land_based_isfm6_cashflow = cashflow_isfm6_based_on_land,
wealth_based_isfm6_cashflow = cashflow_isfm6_based_on_wealth,
knowledge_based_isfm6_cashflow = cashflow_isfm6_based_on_knowledge,
labor_based_isfm6_cashflow = cashflow_isfm6_based_on_labor,

###calling the other outcomes for comparison

isfm1_productivity = isfm1_productivity,
isfm1_income = isfm1_income,
isfm1_environmental_benefit = isfm1_environmental_benefit,
isfm1_social_benefit = isfm1_social_benefit,
isfm1_human_benefit = isfm1_human_benefit,

isfm2_productivity = isfm2_productivity,
isfm2_income = isfm2_income,
isfm2_environmental_benefit = isfm2_environmental_benefit,
isfm2_social_benefit = isfm2_social_benefit,
isfm2_human_benefit = isfm2_human_benefit,

isfm3_productivity = isfm3_productivity,
isfm3_income= isfm3_income,
isfm3_environmental_benefit = isfm3_environmental_benefit,
isfm3_social_benefit = isfm3_social_benefit,
isfm3_human_benefit = isfm3_human_benefit,

isfm4_productivity = isfm4_productivity,
isfm4_income = isfm4_income,
isfm4_environmental_benefit = isfm4_environmental_benefit,
isfm4_social_benefit = isfm4_social_benefit,
isfm4_human_benefit = isfm4_human_benefit,

isfm5_productivity = isfm5_productivity,
isfm5_income= isfm5_income,
isfm5_environmental_benefit = isfm5_environmental_benefit,
isfm5_social_benefit = isfm5_social_benefit,
isfm5_human_benefit = isfm5_human_benefit,       

isfm6_productivity = isfm6_productivity,
isfm6_income= isfm6_income,
isfm6_environmental_benefit = isfm6_environmental_benefit,
isfm6_social_benefit = isfm6_social_benefit,
isfm6_human_benefit = isfm6_human_benefit, 




















#POST HOC 

#### Monte Carlo simulation ####

ISFM_mc_simulation <- mcSimulation(as.estimate(ISFM_table), 
                                   model_function = ISFM_system_benefits,
                                   numberOfModelRuns = 1000,
                                   functionSyntax = "plainNames"
)

write.csv(ISFM_mc_simulation, "./ISFM_mc_simulation_results.csv")


## Cumulative cashflow for the 5 ISFM

#install.packages("gridExtra")
library(gridExtra)

cashflow <- plot_cashflow(mcSimulation_object = ISFM_mc_simulation,
                          cashflow_var_name = c("cashflow_statusquo_men",
                                                "cashflow_statusquo_women",
                                                "cashflow_1_men", "cashflow_1_women",
                                                "cashflow_2_men", "cashflow_2_women",
                                                "cashflow_3_men", "cashflow_3_women",
                                                "cashflow_4_men","cashflow_4_women",
                                                "cashflow_5_men","cashflow_5_women" ),
                          x_axis_name = "Years of intervention",
                          y_axis_name = "Cashflow in Ghana Cedis",
                          color_25_75 = "grey",
                          color_5_95 = "yellow",
                          color_median= "red",
                          base= 10 )



cashflow

#save Cashflow plot

####### ggsave("Cashflowlot.png", plot = npv_box_plot, width = 10, height = 8, dpi = 300)

# Heat map of ISFM components and outcomes

# 6 columns and 5 rows corresponding to 5 indicators and 5 ISFM practice

#Subletting the data of outcomes only

ISFM_impact_data <- ISFM_mc_simulation$y[1:34]

#percenbtage increase to represnt the bubbles
#nv-ov/ov*100 to find the percentage

ISFM_impact_percentage_data <- df %>%
  mutate(
    isfm1_productity_change = ((isfm1_productivity - statusquo_productivity) / statusquo_productivity) * 100,
    
  )

library(tidyr)

#Changing the date set arrangement into a long list #

ISFM_pivoted_outcome_data <- tidyr::pivot_longer(ISFM_impact_data,
                                                 cols = names(ISFM_impact_data), 
                                                 names_to = "Outcome with ISFM", values_to = "Relative difference")

library(ggplot2)

#Pivot the data and split names #
library(stringr)

ISFM_pivoted_outcome_data["Outcome"] <- 
  str_extract(ISFM_pivoted_outcome_data$`Outcome with ISFM`, 
              "productivity|men_income|women_income|environmental_benefit|social_benefit|human_benefit")



ISFM_pivoted_outcome_data["Practices"] <-  str_remove(ISFM_pivoted_outcome_data$`Outcome with ISFM`, 
                                                      "_productivity|_men_income|_women_income|_environmental_benefit|_social_benefit|_human_benefit")                                                     



# create labels for 'high', 'medium' and 'low' impact ####

# make some data with mean differences ####

summarized_pivoted_outcome_data <- ISFM_pivoted_outcome_data %>% 
  group_by(Practices, Outcome) %>% 
  dplyr::summarize(median = median(`Relative difference`), 
                   mean = mean(`Relative difference`))  


# with color for direction (positive, negative)

summarized_pivoted_outcome_data$effect_direction <- case_when(
  summarized_pivoted_outcome_data$median > 0 ~ "positive",  # Positive effect
  summarized_pivoted_outcome_data$median < 0 ~ "negative",  # Negative effect
  TRUE ~ "no_effect"  # Zero effect
)


##Finding the threshold to assess outcomes effect strenght 

threshold_high <- sd(summarized_pivoted_outcome_data$median, na.rm = TRUE) * 1
threshold_medium <- sd(summarized_pivoted_outcome_data$median, na.rm = TRUE) * 0.5
threshold_low<- sd(summarized_pivoted_outcome_data$median, na.rm = TRUE) * 0
threshold_negative <- sd(summarized_pivoted_outcome_data$median, na.rm = TRUE) * -1

summarized_pivoted_outcome_data$effect_strength <- case_when(
  summarized_pivoted_outcome_data$median > 21207.85 ~ "H",  # High impact (> 21,207.85)
  summarized_pivoted_outcome_data$median >= 10603 & summarized_pivoted_outcome_data$median <= 21207.85 ~ "M",  # Medium (10,603 - 21,207.85)
  summarized_pivoted_outcome_data$median >= 0 & summarized_pivoted_outcome_data$median < 10603 ~ "L",  # Low (0 - 10,603)
  summarized_pivoted_outcome_data$median < 0 ~ "Negative"  # Risky (negative effect)
)


#Rename columns with variables ####

summarized_pivoted_outcome_data$Outcome <- factor(summarized_pivoted_outcome_data$Outcome, 
                                                  levels = c("productivity", "men_income", 
                                                             "women_income","environmental_benefit", 
                                                             "social_benefit", "human_benefit"), 
                                                  labels = c("Productivity\nkg\nper\nacre", 
                                                             "Men\nincome",
                                                             "Women\nincome", 
                                                             "Environmental\nbenefit",
                                                             "Social\nbenefit",
                                                             "Human\nbenefit"))

summarized_pivoted_outcome_data$effect_direction <- ifelse(summarized_pivoted_outcome_data$median > 0, "positive", 
                                                           ifelse(summarized_pivoted_outcome_data$median < 0, "negative", 
                                                                  "no_effect"))


summarized_pivoted_outcome_data$Practices <- factor(summarized_pivoted_outcome_data$Practices, 
                                                    levels = c("isfm1", 
                                                               "isfm2", 
                                                               "isfm3", 
                                                               "isfm4", 
                                                               "isfm5"),
                                                    labels = c("Improved\ngermplasm",
                                                               "Mineral\nfertilizer",
                                                               "Organic\nfertilizer",
                                                               "Fertilizer\ncombination", 
                                                               "Minimum\ntillage"))

## Plotting with bubble plot

library(ggplot2)
library(dplyr)

ISFM_bubble_plot <- ggplot(summarized_pivoted_outcome_data, aes(x = Outcome, y =Practices )) +
  # Bubble plot: Size based on effect strength, color based on effect direction
  geom_point(aes(size = effect_strength, color = effect_direction), alpha = 0.7) +  
  
  # Scale settings for size and color
  scale_size_manual(values = c("H" = 10, "M" = 7, "L" = 4, "Negative" = 6)) +  # Size based on effect strength
  scale_color_manual(values = c("positive" = "green", "negative" = "red", "no_effect" = "gray")) +  # Color based on effect direction
  
  # Labels and theme
  labs(x = "Outcomes", y ="ISFM Practices" , size = "Effect Strength", color = "Effect Direction") +
  theme_minimal() +
  theme(legend.position = "right", axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x labels for clarity

ISFM_bubble_plot


#percentage chance of new isfm to the statusquo
if it is equal than 1 is better
if less than 1 so its not better than the statusquo_inputs_cost
