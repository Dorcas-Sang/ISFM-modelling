##ISFM model ##

####Farm Level Economic Benefit ####

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

make_variables <- make_variables (as.estimate(table))

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
  
maize_risks <- chance_event(maize_risk, value_if = 1, value_if_not = 0)
                 

##All the risks associated with soybean in every cropping season ##  

soybean_risks <- chance_event(soy_risk, value_if = 1, value_if_not = 0)
                

###price and market risks###

#Probability of no market for produce or high competition and of Price fluctuation or discounting produce
market_risks <- chance_event(market_marketrisks, value_if = 1, value_if_not = 0)

###institutional and policy risks###
#Probability of land litigation limiting investment for long term innovation

institutional_risks <- chance_event(institution_risks, value_if = 1, value_if_not = 0)
  

###Human and personal risks### 
#Probability of human made or personal risks

farmers_risks <- chance_event(farmers-risks, value_if = 1, value_if_not = 0)


#### Costs of ISFM ####

###status quo costs###
inputs_costs <- (seed + pesticide) 


#farmers work a lot so they sometimes have pain and may need to buy painkiller
#This cost applies to all component of ISFM but would increase as component number increase
#Probably the chance of pain comes with high number of ISFM component

pain_yes_no <- chance_event(pain, 
                            value_if = 1,
                            value_if_not = 0)

medicine <- if(pain_yes_no ==1){
  medicine = tablet_cost * pain_days #Number of days farmers might experience pain in a cropping season 
}else{
  medicine = 0 #Zero cost if no pain or denial for feeling pain or resistance to medicine
}


labor <- (preparation # labor for land preparation
          + planting   # labor for planting
          + maintainance  # labor for maintenance
          + harvest # labor for harvest
          + clearing) #labor to clear field, sort crop residue after harvest
                 
other_costs <- (tools #equipment here are cutlass, hoe, wood and tiles for fencing etc 
                 + transport # transport of inputs and harvest produce to and from market
                 + fuel  # small token paid to extension officers when they visit
                 + training # Farmers need to make time for training but sometimes also pay for it
                 + soil_testing  # Not very common but sometimes farmers have to do it
                 + pain_killer
                + land)# Cost of land acquisition and registration

                          

##Component 1 (Improved Germplasm) #STATUS QUO##
component1_inputs <- inputs_costs
total_cost_1 <- component1_inputs + labor + other_costs

total_cost_1 <-  vv (var_mean = total_cost_1, 
                     var_CV = var_cv,
                     n= years, 
                     relative_trend = inflation)/ exchange_rate #inflation: percentage of increase each year which is quite high in Ghana


##Component 2 (Improved Germplasm + Inorganic fertilizer)##

component2_inputs <- (mineral_fertilizer + fertilizer_application)

total_cost_2 <- component2_inputs + labor+ other_costs+ inputs_costs

total_cost_2 <- vv (var_mean = total_cost_2, 
                    var_CV = var_cv,
                    n= years, 
                    relative_trend = inflation)/ exchange_rate

####Introduction of organic amendment####

#From component 3 onwards, Organic fertilizer is included in the system, 
#Organic fertilizer is manure and crop residue combined 
#Some/most farmers do not incorporate their crop residues in the field and some others do not own livestock to have manure 
#In such cases when using organic amendment they have to buy in the market or outsource from neighbors at a small fee

own_residue <- vv(cost_own_residue,
                  var_CV = var_cv,
                  n= years,
                  relative_trend = inflation) / exchange_rate #value of own farm residue

residue_managers_no <- chance_event(if_no_crop_residue)

if(residue_managers_no == 1) {
  crop_residue <- residue_price * residue_quantity
} else { 
  crop_residue = own_residue #Zero cost to buy residue in this case
}



own_manure <- vv(cost_own_manure,
                 var_CV = var_cv,
                 n= years,
                 relative_trend = inflation) / exchange_rate #value of manure from own livestock

own_livestock_no <- chance_event(if_no_livestock)

if(own_livestock_no == 1) {
  manure <- manure_price * manure_quantity
} else { 
  manure= own_manure #Zero cost to buy manure in this case
}


#Component 3 (Improved germplasm and organic fertilizer)
component3_inputs<- manure + crop_residue 
+training_price # To learn the technique for compost preparation
+ compost_preparation #Cost of preparing organic fertilizer mixing residue and manure before application
+ compost_application_cost

total_cost_3 <- component3_inputs + labor + other_costs+ inputs_costs

total_cost_3 <- vv (var_mean = total_cost_3,
                    var_CV = var_cv,
                    n= years, 
                    relative_trend = inflation)/ exchange_rate


#component 4 (IG + OF + IF)
component4_inputs <- component2_inputs + component3_inputs

total_cost_4 <- component4_inputs+ inputs_costs + labor + other_costs

total_cost_4 <- vv (var_mean = total_cost_4, 
                    var_CV = var_cv,
                    n= years, 
                    relative_trend = inflation)/ exchange_rate


#component 5 (IG + OF+ IF + M/ZT)

component5_inputs <- component4_inputs

total_cost_5 <- component5_inputs+ inputs_costs + labor + other_costs

total_cost_5 <- vv (var_mean = total_cost_5, 
                    var_CV = var_cv,
                    n= years, 
                    relative_trend = inflation)/ exchange_rate


#### Systems benefits of Component 1,2,3,4,5 of ISFM ####  

##Farm revenue from maize and soybean ##
#The yield is affected by production risks and farmers risks 
#while the income or farm revenue is affected by price and market risks

##Benefits from Maize ##

maize_yield <- vv (var_mean = maize_yield, 
                    var_CV = var_cv, 
                    n= years) * maize_risks * farmers_risks  #yield in t/ha


maize_residue <- vv (var_mean = maize_residue,
                     var_CV = var_cv, 
                     n= years)  #biomass in t/ha

maize_income <- (maize_yield * maize_price) + (maize_residue * residue_price)
                  * market_risks /exchange_rate


##Benefits from soybean ##

soybean_yield <- vv (var_mean = soybean_yield, 
                     var_CV = var_cv, 
                     n= years) * soybean_risks * farmers_risks #yield in t/ha


soybean_residue <- vv (var_mean = soybean_residue,
                       var_CV = var_cv, 
                       n= years)  #biomass in t/ha

soybean_income <- (soybean_yield * soybean_price) + (soybean_residue * residue_price)
                    * market_risks /exchange_rate

####Profit####
#Profit is farm revenue minus total costs

component1_profit <- (maize_income+ soybean_income)- total_cost_1
component2_profit <- (maize_income+ soybean_income)- total_cost_2
component3_profit <- (maize_income+ soybean_income)- total_cost_3
component4_profit <- (maize_income+ soybean_income)- total_cost_4
component5_profit <- (maize_income+ soybean_income)- total_cost_5

  
#### Environmental benefits of ISFM linked to soil health####

##Nutrients returned to soil (Nutrient partial balance) ##
#This will be affected by the availability and price of inorganic fertilizer in the market 
#Also affected by farmers application method, right dose at right place and right time

soil_nutrient_replenishment <- vv (var_mean = nutrient, 
                                   var_CV = var_cv, 
                                   n= years)* market_risks* farmers_risks 

nutrient_replenished <- (soil_nutrient_replenishment * fertilizer_price)/ exchange_rate


## Soil loss prevention as Organic fertilization improves soil structure ##
#This will be affected by land rights and farmers choice of applying organic fertilizer
erosion_control <- vv (reduced_soil_loss,
                       var_CV = var_cv,
                       n= years)* institutional_risks * farmers_risks

erosion_control <- (erosion_control * price_saved_soil)/ exchange_rate


## Due to organic fertilizer application there will be high moisture and less need for irrigation ##
#This will be affected by land rights and farmers choice of applying organic fertilizer
moisture <- vv (var_mean = percent_moisture,
                var_CV = var_cv, 
                n= years)* farmers_risks 

saved_water <- (moisture * water_price)/exchange_rate


##Biological Nitrogen fixation (BNF) N/ha from the soybean##
#This will be affected by soybean variety as well as it's growth potential
fixed_Nitrogen <- vv (var_mean = total_Nitrogen, 
           var_CV = var_cv, 
           n= years) * soybean_risks 

fixed_Nitrogen <- (fixed_Nitrogen * Nitrogen_price)/ exchange_rate


##Active Carbon (mg/kg of soil) which takes long time to accumulate in the soil and might be affected by land tenure problems## 
saved_carbon <- vv (var_mean = active_carbon, 
                     var_CV = var_cv, 
                     n= years) *farmers_risks * institutional_risks 

saved_carbon<- (saved_carbon * carbon_payment)/ exchange_rate


##Species below and above ground estimated by the shannon diversity index##

biodiversity <- vv (var_mean = biodiversity_index,
                var_CV = var_cv, 
                n= years) * farmers_risks * institutional_risks 

biodiversity <- (biodiversity * biodiversity_index_value)/ exchange_rate


##If there is high infiltration rate the inputs will not be washed away. 
#This rate increases with less stress on the land created by use of tractor

infiltration <- vv (infiltration_rate,
                    var_CV = var_cv,
                    n= years)* institutional_risks

infiltration <- (infiltration * tractor_service)/ exchange_rate


## Minimum tillage will reduce incidence of weed hence the need for pesticide##

weed_suppression <-vv (percentage_weed,
                       var_CV = var_cv,
                       n= years) * farmers_risks

weed_suppression<- (weed_suppression * weed_management_price)/ exchange_rate


####Household benefits ####

###Household health###

##Household Dietary diversity (HDD) OR Household Food Insecurity Access Score (HFIAS) 
##This might be affected if soils are poor due to no fertilization- crops do not get enough nutrients for healthy diets
#This is linked to vitality and infant survival#

nutrition <- vv (var_mean = HDD,
                 var_CV = var_cv, 
                 n= years)* market_risks 

nutrition <- (nutrition * calory_price)/ exchange_rate

## Months of food security * food availability Or food sufficiency ##
##If the return on investment assures farmers not to lack food at any point in a month year round ##

food_availability <- vv(var_mean = food,
                        var_CV = var_cv,
                        n= years)* institutional_risks * farmers_risks * market_risks 

food_availability <- (food_availability * meal_price)/ exchange_rate
  

##Reduced expenditure on health matters because there is less contamination due to leaching ##
contamination <- vv(var_mean = percent_contamination_reduction,
                    var_CV = var_cv,
                    n= years)* farmers_risks 

contamination <- (contamination * health_expenditure)/ exchange_rate 

##Stable mental health leads to happiness and healthy life
mental_health <- vv(var_mean = mental_health_incidence,
                    var_CV = var_cv,
                    n= years)* farmers_risks 

mental_health <- (mental_health * health_expenditure)/ exchange_rate 

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
             n= years)) * farmers_risks 


reduced_GHG <- (GHG * payment_GHG)/exchange_rate


##All the above put together to get the generic household health benefit for all members##

household_health <- nutrition + food_availability +contamination
                    + mental_health + reduced_GHG

####Benefits per member of the household####

##Children##
#Children only benefits education, nutrition and inherit fertile land, they don't get the profit 
#If there is profit, children get to go to school 

school_allocation <- vv(children_school,
                  var_CV = var_cv,
                  n= years,
                  relative_trend = inflation) / exchange_rate 

if_school_fees <- chance_event(school_fees)

if(if_school_fees == 0) {
  education <- school_time * school_price
} else { 
  education = school_allocation
}

#Children get to learn from their parents while helping them in the field
#They would not need to pay extension officers for this service

knowledge <- vv(var_mean = time, 
                var_CV = var_cv, 
                n= years) * farmers_risks

agric_knowledge <- (knowledge * training_price)/ exchange_rate


#if proper land tenure systems and good agricultural practices, children inherit healthy lands  
land_inheritance <- vv(land_price,
                          var_CV = var_cv,
                          n= years,
                          relative_trend = inflation) / exchange_rate 


children_benefits <-household_health + agric_knowledge
                    + if_school_fees + land_inheritance
  

##Men ##

#high social status might increase opportunities, access and more assets 
social_status <- chance_event(if_profit)

social_status <- vv(social_status, 
                    var_CV = var_cv,
                    n=years, 
                    relative_trend = inflation)/exchange_rate


### The worth of knowledge could be associated to the cost for paying labor if the farmer has to leave some other activities to listen to the extension officers and get someone else to farm for him###
##OR Because they don't pay for training as extension officers are supposed to train for free, but this is usually not the case as farmers have to give a small token (fuel money)##
#So training price here could be cost of hired labor or fuel for extension agents 

knowledge <- vv(var_mean = time, 
                var_CV = var_cv, 
                n= years) * farmers_risks

agric_knowledge <- (knowledge * training_price)/ exchange_rate



#less need for insurance
schock_resilience <- vv(insurance_price,
                        var_CV = var_cv,
                        n= years,
                        relative_trend = inflation)/exchange_rate
  
men_benefits <- household_health+social_status 
                + agric_knowledge + schock_resilience   
  
#Women
##women do not own land, they might get access from their brother or husband and these might take it away at any point 

land_conflict <- chance_event(land_conflict)

#There is competing interest for residue use and women use it for cooking, if they are used for the farms the women feel threatened
#In most cases they do not also own livestock which in addition to the crop residue conflict may limit their ISFM benefit because they will not use organic fertilizer
#This conflict might lead to some kind of violence because the women will resist the husband decision
crop_residue_conflict <- chance_event(crop_residue_conflict)

##Women are rarely targeted in maize systems,and have a small network hence their access to information and resources is not certain
if_access <- chance_event(women_access)


#Gender based violence (GBV) might occur as a result of land conflict, crop residue interest or access to information and opportunities

GBV <- max(land_conflict,
           crop_residue_conflict,
           if_access)

#Provided access to information and resources  
#More ISFM components come with increase labor for women as they still have to do household chores
#this is a cost to the women

additional_labor <- vv (var_mean = women_labor,
                         var_CV = var_cv,
                         n= years)* (1-if_access) 

#Women agency might lead to some more income, here tnew women get to enroll to be part of a community where they will probably have information, access or support
agency <- vv (var_mean = enrolment_cost,
              var_CV = var_cv,
              n= years)* (1-if_access)


#Social network: If more farmers are included and do the same practice, they can create more activities together that will save them some money. 
#For example helping each other in farm activities, and group savings especially for the women can allow them to start a side business that will generate off-farm income
#This can be linked to economic empowerment and perhaphs some form of decision making power
network <- vv (var_mean = percentage_farmers_bonding,
               var_CV = var_cv,
               n= years)* (1-if_access)

network <- (network * saved_labor_cost 
                     *money_group_savings)/ exchange_rate


#Knowledge from new practices

agric_knowledge <- vv(var_mean = time, 
                      var_CV = var_cv, 
                      n= years) * (1- if_access)

agric_knowledge <- (agric_knowledge * training_price)/ exchange_rate


#less need for insurance
schock_resilience <- vv(insurance_price,
                        var_CV = var_cv,
                        n= years,
                        relative_trend = inflation)/exchange_rate

###All women benefits put together and discounted in case of Gender Based Violeonce 
women_benefits <- (household_health + agency 
              + network + agric_knowledge + schock_resilience) - additional_labor

women_benefits <- women_benefits * 1-GBV

#### TOTAL ECONOMIC BENEFITS of ISFM ####
#profit + soil benefits + household benefits


#### 3 layers of benefits  ####

####Profit####
#Profit is farm revenue minus total costs, where farm revenue is only from the harvested crops and it's residue

component1_profit <- (maize_income+ soybean_income)- total_cost_1
component2_profit <- (maize_income+ soybean_income)- total_cost_2
component3_profit <- (maize_income+ soybean_income)- total_cost_3
component4_profit <- (maize_income+ soybean_income)- total_cost_4
component5_profit <- (maize_income+ soybean_income)- total_cost_5

#### Environmental benefits####

component1_env<- fixed_Nitrogen 
                  

component2_env <- fixed_Nitrogen 
                    + nutrient_replenished + weed_suppression


component3_env <- fixed_Nitrogen
                    + saved_carbon + saved_water + reduced_GHG + biodiversity
                    + reduced_leaching + erosion_control


component4_env<- fixed_Nitrogen
                  + saved_carbon + saved_water + reduced_GHG + biodiversity+ reduced_leaching 
                  + erosion_control + nutrient_replenished + weed_suppression


component5_env <- fixed_Nitrogen + saved_carbon
                    + saved_water + reduced_GHG + biodiversity+ reduced_leaching 
                    + erosion_control + nutrient_replenished
                    + infiltration + weed_suppression



#####Farm level Household benefits ####
household_benefits <- children_benefits + women_benefits + men_benefits


####Total benefits####
total_benefit_1 <- component1_profit + component1_env + household_benefits
total_benefit_2 <- component2_profit + component2_env + household_benefits
total_benefit_3 <- component3_profit + component3_env + household_benefits
total_benefit_4 <- component4_profit + component4_env + household_benefits
total_benefit_5 <- component5_profit + component5_env + household_benefits


####ANALYSIS####

#Net Present Value (NPV)
#Cashflow analysis: projected trend of monetary return

  
##Component 1

NPV_comp1 <- discount(total_benefit_1, discount_rate = discount_rate, 
                       calculate_NPV = TRUE)


cashflow_1 <- discount (total_benefit_1, discount_rate = discount_rate,
                      calculate_NPV = FALSE)

cumulative_cashflow_1 <- cumsum(cashflow_1)


##Component 2 

NPV_comp2 <- discount(total_benefit_2, discount_rate = discount_rate, 
                      calculate_NPV = TRUE)

cashflow_2 <- discount (total_benefit_2, discount_rate = discount_rate,
                        calculate_NPV = FALSE)

cumulative_cashflow_2 <- cumsum(cashflow_2)


##component 3

NPV_comp3 <- discount(total_benefit_3, discount_rate = discount_rate, 
                       calculate_NPV = TRUE)

cashflow_3 <- discount (total_benefit_3, discount_rate = discount_rate,
                      calculate_NPV = FALSE)

cumulative_cashflow_3 <- cumsum(cashflow_3)


##component 4

NPV_comp4 <- discount(total_benefit_4, discount_rate = discount_rate, 
                      calculate_NPV = TRUE)

cashflow_4 <- discount (total_benefit_4, discount_rate = discount_rate,
                        calculate_NPV = FALSE)

cumulative_cashflow_4 <- cumsum(cashflow_4)


##component 5

NPV_comp5 <- discount(total_benefit_5, discount_rate = discount_rate, 
                      calculate_NPV = TRUE)


cashflow_5 <- discount (total_benefit_5, discount_rate = discount_rate,
                        calculate_NPV = FALSE)

cumulative_cashflow_5 <- cumsum(cashflow_5)

###Calling anything I need to plot##
##profit, environmental, household benefits and total of all benefits ##

return(list(Profit_component1= component1_profit,
            Profit_component2= component2_profit,
            Profit_component3= component3_profit,
            Profit_component4= component4_profit,
            Profit_component5= component5_profit,
            Environmental_component1= component1_env,
            Environmental_component2= component2_env,
            Environmental_component3= component3_env,
            Environmental_component4= component4_env,
            Environmental_component5= component5_env,
            Children_benefits = children_benefits,
            Women_benefits = women_benefits,
            Men_benefits = men_benefits,
            Household_benefits = household_benefits,
            NPV_component1 = NPV_comp1,
            NPV_component2 = NPV_comp2,
            NPV_component3 = NPV_comp3,
            NPV_component4 = NPV_comp4,
            NPV_component5 = NPV_comp5,
            cashflow_comp1= cumulative_cashflow_1,
            cashflow_comp2= cumulative_cashflow_2,
            cashflow_comp3= cumulative_cashflow_3,
            cashflow_comp4= cumulative_cashflow_4,
            cashflow_comp5= cumulative_cashflow_5,
            ))
  
}


####Monte Carlo simulation
mc_simulation <- mcSimulation(as.estimate(table), 
                              model_function = system_benefits,
                              numberOfModelRuns = 10000,
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


##Profit visualization



##Environmental benefits visualization 



##Household benefits visualization



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


###Put the 5 Cashflows in one frame ###
## Change Cashflow names on function return list 










