#Load and read data 
table <- read.csv("./ISFM_inputs.csv")


#Make variable
make_variables <- function(est, n= 1) {
  x <- random(rho = est,n = n)
  for(i in colnames(x)) 
    assign(i, as.numeric(x[1, i]),envir = .GlobalEnv)
}

make_variables <- (as.estimate(table))

####Model function####

system_benefits <- function(x, varnames){
  
###Maize yield
  
maize_yield <-  vv (var_mean = maize_yield, 
                      var_CV = var_cv, 
                      n= years)

maize_income <- maize_yield * maize_price
  
  
###Soybean yield
  
soybean_yield <- vv (var_mean = soybean_yield, 
                       var_CV = var_cv, 
                       n= years)
  
soybean_income <- soybean_yield * soybean_price
  
  
total_income <- (maize_income + soybean_income)
  
  
#Costs of ISFM 
  
##Purchased inputs 
inputs <- (fertilizer + seed +pesticide + equipements)
  
inputs <- vv (var_mean = inputs, 
                var_CV = var_cv,
                n= years)
  
##Other costs
other_costs <- (land + planting_harvest + transport)
  
other_costs <- vv (var_mean = other_costs,
                     var_CV = var_cv,
                     n= years)
  
##Total costs
total_costs <- (inputs + other_costs)
  
#Cost-benefit analysis
  
revenue <- total_income - total_costs
  
cashflow <- revenue
  
#NPV
NPV_ISFM <- discount(revenue, discount_rate = discount_rate, 
                       calculate_NPV = TRUE)
  
  return(list(cashflow= cashflow, NPV_ISFM = NPV_ISFM))
  
}


####Monte Carlo simulation 

mc_simulation <- mcSimulation(as.estimate(table), 
                              model_function = system_benefits,
                              numberOfModelRuns = 200,
                              functionSyntax = "plainNames")

write.csv(mc_simulation, "./mc_simulation_results.csv")


####Plotting the NPV

plot_distributions(mcSimulation_object = mc_simulation,
                   vars= "NPV_ISFM",
                   method= 'smooth_simple_overlay',
                   colors= c("#0000FF"),
                   base_size= 10)


plot_distributions(mcSimulation_object = mc_simulation,
                   vars = "NPV_ISFM",
                   method = 'boxplot',
                   colors = c("#F0E442"),
                   base_size = 20)



####Cashflow analysis

cashflow <- plot_cashflow(mcSimulation_object = mc_simulation,
                          cashflow_var_name = "cashflow",
                          x_axis_name = "Years of intervention",
                          y_axis_name = "Cashflow in Ghana Cedis",
                          color_25_75 = "grey",
                          color_5_95 = "yellow",
                          color_median= "red",
                          base= 10)

cashflow


######Simple and quick decision outcome
compound_figure(model = system_benefits,
                input_table = table,
                decision_var_name = "NPV_ISFM",
                cashflow_var_name = "cashflow",
                model_runs = 100,
                distribution_method = 'smooth_simple_overlay')



