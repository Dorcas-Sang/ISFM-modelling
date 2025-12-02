##Value of information ##

#Expected Value of Perfect Information (EVPI) 
#This analysis is crucial since it informs the decision-maker how much he/she should be willing to pay if given perfect information, to eliminate uncertainties and avoid opportunity loss.

# 1. Value of information for the 6 ISFM components

cols <- c(7, 12, 17, 22, 27, 32)

evpi_mcresults_ISFM_levels <- data.frame(
  ISFM_mc_simulation$x,
  ISFM_mc_simulation$y[, cols, drop = FALSE]
)

evpi<- multi_EVPI(mc= evpi_mcresults_ISFM_levels,
                  first_out_var = "Improved_seed_income", write_table = FALSE,
                  outfolder = ./results/evpi_ISFM_levels)

# Plot all EVPI for the decision variables related to ISFM components 
improved_seed_evpi <- plot_evpi(evpi,
                                decision_vars = "Improved_seed_income",  
                                unit = "Euro",
                                bar_color = "yellow4",
                                base_size = 10)


mineral_fertilizer_evpi <- plot_evpi(evpi,
                                     decision_vars = "Mineral_fertilizer_income",  
                                     unit = "Euro",
                                     bar_color = "yellow4",
                                     base_size = 10)

organic_fertilizer_evpi <- plot_evpi(evpi,
                                     decision_vars = "Organic_fertilizer_income",  
                                     unit = "Euro",
                                     bar_color = "yellow4",
                                     base_size = 10)

fertilizer_combination_evpi <- plot_evpi(evpi,
                                         decision_vars = "Fertilizer_combination_income",  
                                         unit = "Euro",
                                         bar_color = "yellow4",
                                         base_size = 10)

minimum_tillage_evpi <- plot_evpi(evpi,
                                  decision_vars = "Minimum_tillage_income",  
                                  unit = "Euro",
                                  bar_color = "yellow4",
                                  base_size = 10)

complete_ISFM_evpi <- plot_evpi(evpi,
                                decision_vars = "Complete_ISFM_income",  
                                unit = "Euro",
                                bar_color = "yellow4",
                                base_size = 10)


improved_seed_evpi
mineral_fertilizer_evpi
organic_fertilizer_evpi
fertilizer_combination_evpi
minimum_tillage_evpi
complete_ISFM_evpi



# 2. Value of information for intersectionality related analysis

evpi_mcresults_table <- data.frame(ISFM_mc_simulation$x,
                                   ISFM_mc_simulation$y[36:45])

evpi<- multi_EVPI(mc= evpi_mcresults_table,
                  first_out_var = "NPV_women_ISFM_income", write_table = FALSE, outfolder = ./results/evpi_results)


# Plot all EVPI for the decision variables 
women_evpi <- plot_evpi(evpi,
                        decision_vars = "NPV_women_ISFM_income",  
                        unit = "Euro",
                        bar_color = "yellow4",
                        base_size = 10)

men_evpi <- plot_evpi(evpi,
                      decision_vars = "NPV_men_ISFM_income",  
                      unit = "Euro",
                      bar_color = "yellow4",
                      base_size = 10)

women_evpi_land <- plot_evpi(evpi,
                             decision_vars = "NPV_women_ISFM_income_land_based",  
                             unit = "Euro",
                             bar_color = "yellow4",
                             base_size = 10)

men_evpi_land <- plot_evpi(evpi,
                           decision_vars = "NPV_men_ISFM_income_land_based",  
                           unit = "Euro",
                           bar_color = "yellow4",
                           base_size = 10)

women_evpi_inputs <- plot_evpi(evpi,
                               decision_vars = "NPV_women_ISFM_income_inputs_based",  
                               unit = "Euro",
                               bar_color = "yellow4",
                               base_size = 10)

men_evpi_inputs <- plot_evpi(evpi,
                             decision_vars = "NPV_men_ISFM_income_inputs_based",  
                             unit = "Euro",
                             bar_color = "yellow4",
                             base_size = 10)

women_evpi_knowledge <- plot_evpi(evpi,
                                  decision_vars = "NPV_women_ISFM_income_knowledge_based",  
                                  unit = "Euro",
                                  bar_color = "yellow4",
                                  base_size = 10)

men_evpi_knowledge <- plot_evpi(evpi,
                                decision_vars = "NPV_men_ISFM_income_knowledge_based",  
                                unit = "Euro",
                                bar_color = "yellow4",
                                base_size = 10)

women_evpi_labor <- plot_evpi(evpi,
                              decision_vars = "NPV_women_ISFM_income_labor_based",  
                              unit = "Euro",
                              bar_color = "yellow4",
                              base_size = 10)

men_evpi_labor <- plot_evpi(evpi,
                            decision_vars = "NPV_men_ISFM_income_labor_based",  
                            unit = "Euro",
                            bar_color = "yellow4",
                            base_size = 10)

women_evpi
men_evpi
women_evpi_land
men_evpi_land
women_evpi_inputs
men_evpi_inputs
women_evpi_knowledge
men_evpi_knowledge
women_evpi_labor
men_evpi_labor

