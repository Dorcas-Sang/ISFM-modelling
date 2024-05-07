
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
#comp2. IG + Inorganic fertilizer (IG + IF)
#comp3. IG + Organic fertilizer (IG + 0F) #Organic fertilizer is manure and crop residue
#comp4. IG + OF + IF
#comp5. IG + OF + IF + Minimum tillage (MT) #Specific for Northern Ghana soil


####Model function####

system_benefits <- function(x, varnames){
  
  
####Risks types- ISFM ####

###Production risks###
  
##All the risks associated with maize in every cropping season ##
  
maize_risks <- max(germination, #if failed germination
                  elnino,
                  drought,
                  theft,
                  seed,  #poor seed and susceptible to pest and diseases
                  compaction) #Chances that livestock/humans might walk into the fields and compact the soil


##All the risks associated with soybean in every cropping season ##  

soybean_risks <- max(germination,
                    elnino,
                    drought,
                    theft,
                    seed, #poor seed susceptible to pest and diseases
                    N_fixation, #Varieties with poor Biological Nitrogen fixation potential
                    compaction)



###price and market risks###

#Probability of unavailability of fertilizer in the market as well as 
#Chance of Price fluctuation due to inflation

market_risks <- max(inputs_unavailability, 
                    if_inflation)

###institutional and policy risks###

institutional_risks <- max(land_litigation, #Chance of land litigation happening
                           norms, #norms limiting the chance of doing an activity
                           infrastructure) # Bad road limiting transport of product from and to market
                           
                        

###financial risks###

financial_risks <- max(capital, #if no capital there is a high probability of taking credit/debt
                       currency_change) #Leads to change in interest rate hence farmers have to pay high or maybe low
                       
              

  
###Human and personal risks### 

farmers_risks <- max(competition, #Probability that there will be competition for crop residue
                     trust, #Chances that farmers will not trust extension officers 
                     patience, #No patience for long term benefits 
                     knowledge, # more training time or high probability of mistake
                     violence, #Gender-based violence
                     conflict, #No social cohesion
                     acceptance,#Resistance/reluctance to change
                     pain) #chance that farmers will fall sick or have pain
  

#### Systems benefits of Component 1,2,3,4,5 of ISFM ####  

## Benefits from Maize ##

maize_yield <- vv (var_mean = maize_yield, 
                    var_CV = var_cv, 
                    n= years) * maize_risks  #yield in t/ha


maize_residue <- vv (var_mean = maize_residue,
                     var_CV = var_cv, 
                     n= years)  #biomass in t/ha

maize_income <- (maize_yield * maize_price) + (maize_residue * residue_price)/exchange_rate


## Benefits from soybean ##

soybean_yield <- vv (var_mean = soybean_yield, 
                     var_CV = var_cv, 
                     n= years) * soybean_risks #yield in t/ha


soybean_residue <- vv (var_mean = soybean_residue,
                       var_CV = var_cv, 
                       n= years)  #biomass in t/ha

soybean_income <- (soybean_yield * soybean_price) + (soybean_residue * residue_price)/exchange_rate

  
### Other benefits of ISFM ###

##Biological Nitrogen fixation (BNF) N/ha from the soybean##
saved_Nitrogen <- vv (var_mean = total_Nitrogen, 
           var_CV = var_cv, 
           n= years) *soybean_risks 

saved_Nitrogen <- (saved_Nitrogen * Nitrogen_price)/ exchange_rate


##Active Carbon (mg/kg of soil) which takes long time to accumulate in the soil## 
saved_carbon <- vv (var_mean = active_carbon, 
                     var_CV = var_cv, 
                     n= years) *farmers_risks * institutional_risks 

saved_carbon<- (saved_carbon * carbon_payment)/ exchange_rate


##Species below and above ground estimated by the shannon diversity index##

biodiversity <- vv (var_mean = biodiversity_index,
                var_CV = var_cv, 
                n= years) * farmers_risks * institutional_risks 

biodiversity <- (biodiversity * biodiversity_index_value)/ exchange_rate


##Household Dietary diversity (HDD) OR Household Food Insecurity Access Score (HFIAS) 
##This might be affected if soils are poor due to no fertilization- crops do not get enough nutrients for healthy diets

nutrition <- vv (var_mean = HDD,
           var_CV = var_cv, 
           n= years)* market_risks 

nutrition <- (nutrition * calory_price)/ exchange_rate


## Months of food security * food availability Or food sufficiency ##
##If the return on investment assures farmers enough food ##

food_availability <- vv(var_mean = food,
                        var_CV = var_cv,
                        n= years)* institutional_risks * farmers_risks * market_risks 

food_availability <- (food_availability * meal_price)/ exchange_rate
  

##Reduced expenditure on health matters because there is less contamination/leaching ##

reduced_leaching <- vv(var_mean = percent_contamination_reduction,
             var_CV = var_cv,
             n= years)* farmers_risks 

reduced_leaching <- (reduced_leaching * health_expenditure)/ exchange_rate 


##Nutrients returned to soil (Nutrient partial balance) ##

soil_nutrient_replenishment <- vv (var_mean = nutrient, 
           var_CV = var_cv, 
           n= years)* market_risks* farmers_risks 

nutrient_replenished <- (soil_nutrient_replenishment * fertilizer_price)/ exchange_rate


## Good seed and agricultural practices will reduce weed, pest and diseases, hence the need for pesticide ##
stress_resistance <- vv (var_mean = reduced_pesticide,
                         var_CV = var_cv,
                         n= years) * soybean_risks * maize_risks

stress_resistance <- (stress_resistance * pesticide_price)/ exchange_rate

  
## Due to organic fertilizer application there will be high moisture and less need for irrigation ##
moisture <- vv (var_mean = percent_moisture,
                var_CV = var_cv, 
                n= years)* farmers_risks 

saved_water <- (moisture * water_price)/exchange_rate


## Organic fertilization improves soil structure ##
erosion_control <- vv (reduced_soil_loss,
                       var_CV = var_cv,
                       n= years)* institutional_risks * farmers_risks

erosion_control <- (erosion_control * price_saved_soil)/ exchange_rate

##Less mineral fertilizer will reduce GHG emission ##
##Reduced Greenhouse gases * Nitrous oxide emissions from N fertilizer
# CO2 and methane emission reduced per ha

GHG <- (vv (nitrous_oxide,
           var_CV = var_cv,
           n= years) +
          vv (methane,
               var_CV = var_cv,
               n= years) +
             vv(carbon_dioxide,
                  var_CV = var_cv,
                  n= years)) * institutional_risks * market_risks 
          

reduced_GHG <- (GHG * payment_GHG)/exchange_rate

##If there is high infiltration rate the inputs will not be washed away. 
#This rate increases with less stress on the land created by use of tractor

infiltration <- vv (infiltration_rate,
                    var_CV = var_cv,
                    n= years)* institutional_risks

infiltration <- (infiltration * tractor_service)/ exchange_rate


## Minimum tillage will reduce incidence of weed ##

weed_suppression <-vv (percentage_weed,
                       var_CV = var_cv,
                       n= years) * farmers_risks

weed_suppression<- (weed_suppression * weed_management_price)/ exchange_rate
  

#Social inclusion: If more farmers are included and do the same practice, they can create more activities together that will save them some money. 
#For example helping each other in farm activities, and group savings can allow farmers to start a side business that will generate off-farm income

social_inclusion <- vv (var_mean = percentage_farmers_bonding,
                        var_CV = var_cv,
                        n= years)* farmers_risks * institutional_risks

social_inclusion <- (social_inclusion * saved_labor_cost 
                     *money_group_savings)/ exchange_rate

##On Social cohesion We could also add domestic conflict (GBV) as farmers risk


##Each component of ISFM comes with new technique and knowledge ##

knowledge <- vv(var_mean = time, 
                 var_CV = var_cv, 
                 n= years) * farmers_risks

knowledge <- (knowledge * training_price)/ exchange_rate

### The worth of knowledge could be associated to the cost for paying labor if the farmer has to leave some other activities and get someone else to do it###
##Because they don't pay for training as extension officers are supposed to train for free, but this is usually not the case as farmers have to give a small token (fuel money)##
#So training price here should be replaced with cost of hired labor


#### TOTAL ECONOMIC BENEFITS of ISFM ####

total_benefit_1 <- (maize_income + soybean_income + saved_Nitrogen 
                    + food_availability + knowledge + social_inclusion + nutrition)
                  

total_benefit_2 <- (maize_income + soybean_income + saved_Nitrogen 
                  + food_availability + knowledge + social_inclusion
                  + nutrient_replenished + nutrition + stress_resistance)

  
total_benefit_3 <- (maize_income + soybean_income + saved_Nitrogen 
                  + food_availability + knowledge + social_inclusion + nutrition
                  + saved_carbon + saved_water + reduced_GHG + biodiversity
                  + reduced_leaching + erosion_control) 


total_benefit_4 <- (maize_income + soybean_income + saved_Nitrogen +
                  + saved_carbon + food_availability + knowledge + social_inclusion
                  + nutrition + saved_water + reduced_GHG + biodiversity+ reduced_leaching 
                  + erosion_control + nutrient_replenished + nutrition + stress_resistance)

  
total_benefit_5 <- (maize_income + soybean_income + saved_Nitrogen +
                  + saved_carbon + food_availability + knowledge + social_inclusion
                  + saved_water + reduced_GHG + biodiversity+ reduced_leaching 
                  + erosion_control + nutrient_replenished + nutrition 
                  + stress_resistance + infiltration + weed_suppression)



#### Costs of ISFM ####

###Fixed costs ###

#farmers work a lot so they sometimes have pain and may need to buy painkiller
#This cost applies to all component of ISFM but would increase as component number increase
#Probably the chance of pain comes with high number of ISFM component

pain_yes_no <- chance_event(pain, 
                            value_if = 1,
                            value_if_not = 0)

pain_killer <- if(pain_yes_no ==1){
  pain_killer = tablet_cost * pain_days #Number of days farmers might experience pain in a cropping season 
}else{
  pain_killer = 0 #Zero cost if no pain or denial for feeling pain or resistance to medecine
}



fixed_cost <- (seed_price
                + pesticide_price 
                + equipment #equipment here are cutlass, hoe, wood and tiles for fencing etc 
                + preparation # labor for land preparation
                + planting   # labor for planting
                + maintainance  # labor for maintenance
                + harvest # labor for harvest
                + clearing #labor to clear field, sort crop residue after harvest
                + transport # transport of inputs and harvest produce to and from market
                + fuel  # small token paid to extension officers when they visit
                + time # Time is money. Farmers need to make time for training but also more time to make sure they follow the correct planting practice
                + soil_testing  # Not very common but sometimes farmers have to do it if working with research institutes
                + pain_killer)

          

###Fixed costs and Variable costs ###

#Component 1 (IG)

total_cost_1 <- fixed_cost
  
total_cost_1 <-  vv (var_mean = total_cost_1, 
                    var_CV = var_cv,
                    n= years, 
                    relative_trend = inflation)/ exchange_rate #inflation: percentage of increase each year which is quite high in Ghana


#Component 2 (Improved Germplasm + Inorganic fertilizer)

variable_cost_2 <- (mineral_fertilizer + fertilizer_application)

total_cost_2 <- fixed_cost +  variable_cost_2

total_cost_2 <- vv (var_mean = total_cost_2, 
                var_CV = var_cv,
                n= years, 
                relative_trend = inflation)/ exchange_rate



######### MODEL IS BREAKING FROM HERE GIVING ERROR ######


#From component 3 onwards, Organic fertilizer is included in the system, 
#Organic fertilizer is manure and crop residue combined 

#Some/most farmers do not manage their crop residues and some others do not own livestock to have manure 
#In such cases they have to buy in the market or outsource from neighbors at a small fee


own_residue <- vv(cost_own_residue,
                 var_CV = var_cv,
                 n= years,
                 relative_trend = inflation) / exchange_rate

residue_managers_no <- chance_event(if_no_crop_residue)

    if(residue_managers_no == 1) {
   crop_residue <- residue_price * residue_quantity
  } else { 
  crop_residue = own_residue #Zero cost to buy residue in this case
  }



own_manure <- vv(cost_own_manure,
             var_CV = var_cv,
             n= years,
             relative_trend = inflation) / exchange_rate

own_livestock_no <- chance_event(if_no_livestock)
                                 

if(own_livestock_no == 1) {
  manure <- manure_price * manure_quantity
} else { 
  manure= own_manure #Zero cost to buy manure in this case
}



#Component 3 (Improved germplasm and organic fertilizer)
variable_cost_3 <- (manure + crop_residue 
                    + OF_preparation) #Cost of preparing organic fertilizer mixing residue and manure before application
                   +(time * training_price) #Time it takes to learn the technique for preparation


total_cost_3 <- fixed_cost + variable_cost_3

total_cost_3 <- vv (var_mean = total_cost_3,
                     var_CV = var_cv,
                     n= years, 
                     relative_trend = inflation)/ exchange_rate


#component 4 (IG + OF + IF)

total_cost_4 <- fixed_cost + variable_cost_2 + variable_cost_3

total_cost_4 <- vv (var_mean = total_cost_4, 
                var_CV = var_cv,
                n= years, 
                relative_trend = inflation)/ exchange_rate
 

#component 5 (IG + OF+ IF + M/ZT)

#Inputs

total_cost_5 <- fixed_cost + variable_cost_2 + variable_cost_3 

total_cost_5 <- vv (var_mean = total_cost_5, 
                var_CV = var_cv,
                n= years, 
                relative_trend = inflation)/ exchange_rate




####ANALYSIS####

#Cost-benefit analysis (NPV)
#Benefit-cost ratio: the discounted value of the benefits divided by the discounted value of the costs
#Cashflow analysis: projected trend of monetary return

#### Component 1 ####

bottomline_benefit_1 <- total_benefit_1 - total_cost_1
  
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

bottomline_benefit_2 <- total_benefit_2 - total_cost_2

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

bottomline_benefit_3 <- total_benefit_3 - total_cost_3
  
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

bottomline_benefit_4 <- total_benefit_4 - total_cost_4

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

bottomline_benefit_5 <- total_benefit_5 - total_cost_5

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


return(list(NPV_component1 = NPV_comp1,
            NPV_component2 = NPV_comp2,
            NPV_component3 = NPV_comp3,
            NPV_component4 = NPV_comp4,
            NPV_component5 = NPV_comp5,
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

write.csv(mc_simulation, "./ISFM_mc_simulation_results.csv")



####PLOTTING####

### PLOTTING ISFM ###

install.packages("gridExtra")
install.packages("cowplot")
install.packages("gganimate")
install.packages("ggpubr")

library(cowplot)
library(decisionSupport)
library(dplyr)
library(gganimate)
library(ggplot2)
library(ggpubr)
library(ggthemes)
library(gridExtra)
library(tidyverse)


## Economic Net Present Value ##

#subset NPV data 

mc_result<-read.csv("ISFM_mc_simulation_results.csv",header = TRUE,sep=",")

npv_table<-mc_result[,c(1:6)] 


npv_data_frame <- data.frame(npv_table)

###Changing arrangement of NPV data###

ISFM <- npv_data_frame %>% 
  pivot_longer(cols = y.NPV_component1:y.NPV_component5, names_to = "treatment",
               values_to = 'NPV')
                             

#### Plotting  NPV distribution ####

## Smooth density 

npvplot= ggplot(ISFM, aes(x = NPV, fill = treatment, color= treatment)) +                       
  geom_density(alpha = 0.05)+  
  scale_fill_colorblind()+
  ylab("Probability density")+
  xlab("Net Present Value (in USD)") 


### Box plot to see how they overlap ###

npvplot= ggplot(ISFM, aes(x = NPV, fill = treatment, color= treatment)) +                       
  geom_boxplot()+
  scale_fill_colorblind()+
  ylab("Probability density")+
  xlab("Net Present Value (in USD)")



#### Visualization #### 

npv<-npvplot+ theme_bw() +theme(legend.position = c(.7, .8))+
  theme(legend.title = element_blank())+
  ggtitle("Integrated Soil Fertility Management")+
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))


npv


#### SENSITIVITY ANALYSIS #### 

#Codes to be done later when i figure out how to put them together#
#Merge with codes on the other plotting script


##Projection to Latent structure (PLS) regression ###
vip 



## Expected Value of Perfect information ##
EVPI 



#### CASHFLOW ANALYSIS ####

#Cashflow for component 1 

cashflow1 <- plot_cashflow(mcSimulation_object = mc_simulation,
                           cashflow_var_name = "cashflow_comp1",
                           x_axis_name = "Years of intervention",
                           y_axis_name = "Cashflow in US Dollars",
                           color_25_75 = "grey",
                           color_5_95 = "yellow",
                           color_median= "red",
                           base= 10)

cashflow1


#Cashflow for component 2 

cashflow2 <- plot_cashflow(mcSimulation_object = mc_simulation,
                           cashflow_var_name = "cashflow_comp2",
                           x_axis_name = "Years of intervention",
                           y_axis_name = "Cashflow in US Dollars",
                           color_25_75 = "grey",
                           color_5_95 = "yellow",
                           color_median= "red",
                           base= 10)

cashflow2



#Cashflow for component 3 

cashflow3 <- plot_cashflow(mcSimulation_object = mc_simulation,
                           cashflow_var_name = "cashflow_comp3",
                           x_axis_name = "Years of intervention",
                           y_axis_name = "Cashflow in US Dollars",
                           color_25_75 = "grey",
                           color_5_95 = "yellow",
                           color_median= "red",
                           base= 10)

cashflow3



#Cashflow for component 4 

cashflow4 <- plot_cashflow(mcSimulation_object = mc_simulation,
                           cashflow_var_name = "cashflow_comp4",
                           x_axis_name = "Years of intervention",
                           y_axis_name = "Cashflow in US Dollars",
                           color_25_75 = "grey",
                           color_5_95 = "yellow",
                           color_median= "red",
                           base= 10)

cashflow4



#Cashflow for component 5

cashflow5 <- plot_cashflow(mcSimulation_object = mc_simulation,
                           cashflow_var_name = "cashflow_comp5",
                           x_axis_name = "Years of intervention",
                           y_axis_name = "Cashflow in US Dollars",
                           color_25_75 = "grey",
                           color_5_95 = "yellow",
                           color_median= "red",
                           base= 10)

cashflow5


###Find how to put the 5 Cashflows in one frame ###
## Change Cashflow names on function return list 










