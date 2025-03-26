##### PLOTTING ISFM ####


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


####Monte Carlo simulation
mc_simulation <- mcSimulation(as.estimate(table), 
                              model_function = system_benefits,
                              numberOfModelRuns = 1000,
                              functionSyntax = "plainNames")

write.csv(mc_simulation, "./mc_simulation_results.csv")


## NPV ##

#subset NPV data

mc_result<-read.csv("MCResults/mcSimulationResults.csv",header = TRUE,sep=",")
npv_table<-mc_result[,c(144:147)] ####REVISE write own number 


# Convert data into million unit
npv_million<-npv_table/1000000


#Create data frame for NPV plot

npv_data_frame <- data.frame(values = c(npv_million$NPV_comp1,                    
                                        npv_million$NPV_comp2,
                                        npv_million$NPV_comp3,
                                        npv_million$NPV_comp4,
                                        npv_million$NPV_comp5),
                             group = c(rep("1) Improved_Germplasm", 10000),
                                       rep("2) IG-Organic fertilizer", 10000),
                                       rep("3) IG-Inorganic fertilizer", 10000),
                                       rep("4) IG-OF-IF", 10000),
                                       rep("5) IG-OF-IF-minimum tillage", 10000)))


# Plot smoothed density 

npvplot= ggplot(npv_data_frame, aes(x = values, fill = group, color=group)) +                       
  geom_density(alpha = 0.05)+
  scale_fill_colorblind()+
  ylab("Probability density")+
  xlab("Net Present Value (in million $)")

# Remove background color
# https://felixfan.github.io/ggplot2-remove-grid-background-margin/
# Remove legend
# https://www.datanovia.com/en/blog/ggplot-legend-title-position-and-labels/

npv<-npvplot+ theme_bw() +theme(legend.position = c(.7, .8))+
  theme(legend.title = element_blank())+
  ggtitle("a)")+
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))
npv


###### START FROM HERE ######

#1.2. Benefit-Cost ratio plot without long x values####

bcrx<-mc_result[,c(150:153)] ###adjust 

bcrx_data_frame <- data.frame(benefitcostratio= c(bcrx$Benefit_Cost_Ratio_comp1,                    
                                                  bcrx$Benefit_Cost_Ratio_comp2,
                                                  bcrx$Benefit_Cost_Ratio_comp3,
                                                  bcrx$Benefit_Cost_Ratio_comp4,
                                                  bcrx$Benefit_Cost_Ratio_comp5),
                              Component=c(rep("1)",10000),rep("2)",10000),
                                          rep("3)",10000),rep("4)",10000), rep("5)", 10000)))

bcrxplot <- ggplot(data=bcrx_data_frame, aes(x=Component, y=benefitcostratio, fill=Component)) + geom_boxplot()

bcrxplotsave<-bcrxplot + scale_fill_discrete(name="",
                                             breaks=c("1)", "2)", "3)", "4)", "5)"),
                                             labels=c("1) Improved Germplasm", "2) IG-Organic fertilizer", 
                                                      "3) IG-Inorganic fertilizer","4) IG-OF-IF",
                                                      "5) IG-OF-IF-Zero/minimum tillage"))+
  theme_bw() +theme(legend.position = c(.3, .8))+
  ylab("Benefit cost ratio")+
  ggtitle("b)")+
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))



bcrx
#ggsave("bcrxplotsave.jpeg", dpi = 500, width = 7, height = 7) 


#1.3 Plot for both npv and bcr####
combinenpvbcrx<- plot_grid(npv,bcrxplotsave)

ggsave("Figures/FIG5.TIF.Combine-npv-bcrx.jpeg", dpi = 1000, width = 9,
       height = 5)



#### EVPI AND VIP ####

# Calculate EVPI####
#Call the Monte Carlo simulation results from the input table
mc_acis<-mc_result[,c(2:143,144:149)] ####REVISE

#Running the EVPI can take few hours

#results_all <- multi_EVPI(mc_acis,"NPV_Intervention1",write_table = TRUE)

#plot(results_all, "output_1")
#plot(results_all, "output_2")
#plot(results_all, "output_3")
#plot(results_all, "output_4")
#plot(results_all, "output_5")


####START FROM HERE ####


# Create data frame for VIP and EVPI

#2.1. PLOTTING VIP####

#2.1.1. PLOTTING VIP1####

# Reading the VIP scores for component 1
vip1<-read.csv("MCResults/NPV_comp1_pls_results.csv",header = TRUE,sep=",")

# Reading EVPI score for component 1
evpi1<-read.csv("EVPI_table_NPV_comp1.csv")
evpi1_omit<-na.omit(evpi1)

# Merge data of vip and evpi
vip_evpi1 <-merge(vip1, evpi1_omit)

# Creating Category for variables based on value of coefficient  
vip_evpi1$Category[vip_evpi1$Coefficient> 0] = "cadetblue"
vip_evpi1$Category[vip_evpi1$Coefficient< 0] = "firebrick"
vip_evpi1_threshold <- filter(vip_evpi1, VIP>=1)

# Creating plot with grid and border

vip1plot<-ggplot(vip_evpi1_threshold,aes(x=VIP,y=reorder(Label,VIP)))+
  geom_bar(aes(fill=Category),stat ="identity")+ 
  xlim(0,6)+
  xlab("VIP: Improved Germplasm")+
  ylab(NULL)+
  scale_fill_manual(values = c("cadetblue","firebrick","grey"))+
  theme(axis.title.x =element_text(color="black", size=8), 
        axis.ticks.x = element_blank())+
  geom_vline(xintercept = 1.0, size=0.2)+
  theme_bw()+
  theme(legend.position = "none")+
  theme(plot.title = element_text(hjust = 0.5))

vip1plot


#2.1.2. PLOTTING VIP2####
# Reading the VIP scores for Intervention 2
vip2<-read.csv("MCResults/NPV_comp2_pls_results.csv",header = TRUE,sep=",")

# Reading EVPI score for Intervention 2
evpi2<-read.csv("EVPI_table_NPV_comp2.csv")
# Remove NA value
evpi2_omit<-na.omit(evpi2)

# Merge data of vip and evpi
vip_evpi2 <-merge(vip2, evpi2_omit)

# Creating Category for variables based on value of coefficient  
vip_evpi2$Category[vip_evpi2$Coefficient> 0] = "cadetblue"
vip_evpi2$Category[vip_evpi2$Coefficient< 0] = "firebrick"
vip_evpi2_threshold <- filter(vip_evpi2, VIP>=1.0)
# Creating plot with grid and border

vip2plot<-ggplot(vip_evpi2_threshold,aes(x=VIP,y=reorder(Label,VIP)))+
  geom_bar(aes(fill=Category),stat ="identity")+ 
  xlab("VIP: IG- Organic fertilizer")+
  xlim(0,6)+
  ylab(NULL)+
  scale_fill_manual(values = c("cadetblue","firebrick","grey"))+
  theme(axis.title.x =element_text(color="black", size=10), 
        axis.ticks.x = element_blank())+
  geom_vline(xintercept = 1.0, size=0.2)+
  theme_bw()+
  theme(legend.position = "none")+
  theme(plot.title = element_text(hjust = 0.5))
vip2plot

#2.1.3. PLOTTING VIP3####
# Reading the VIP scores for component 3
vip3<-read.csv("MCResults/NPV_comp3_pls_results.csv",header = TRUE,sep=",")

# Reading EVPI score for Intervention 3
evpi3<-read.csv("EVPI_table_NPV_comp3.csv")
# Remove NA value
evpi3_omit<-na.omit(evpi3)

# Merge data of vip and evpi
vip_evpi3 <-merge(vip3, evpi3_omit)

# Creating Category for variables based on value of coefficient  
vip_evpi3$Category[vip_evpi3$Coefficient> 0] = "cadetblue"
vip_evpi3$Category[vip_evpi3$Coefficient< 0] = "firebrick"
vip_evpi3_threshold <- filter(vip_evpi3, VIP>=1.0)
# Creating plot with grid and border

vip3plot<-ggplot(vip_evpi3_threshold,aes(x=VIP,y=reorder(Label,VIP)))+
  geom_bar(aes(fill=Category),stat ="identity")+ 
  xlab("VIP: IG- Inorganic fertilizer")+
  xlim(0,6)+
  ylab(NULL)+
  scale_fill_manual(values = c("cadetblue","firebrick","grey"))+
  theme(axis.title.x =element_text(color="black", size=10), 
        axis.ticks.x = element_blank())+
  geom_vline(xintercept = 1.0, size=0.2)+
  theme_bw()+
  theme(legend.position = "none")+
  theme(plot.title = element_text(hjust = 0.5))
vip3plot

#2.1.4. PLOTTING VIP4####
# Reading the VIP scores for component 4
vip4<-read.csv("MCResults/NPV_comp4_pls_results.csv",header = TRUE,sep=",")

# Reading EVPI score for component 4
evpi4<-read.csv("EVPI_table_NPV_comp4.csv")
# Remove NA value
evpi4_omit<-na.omit(evpi4)

# Merge data of vip and evpi
vip_evpi4 <-merge(vip4, evpi4_omit)

# Creating Category for variables based on value of coefficient  
vip_evpi4$Category[vip_evpi4$Coefficient> 0] = "cadetblue"
vip_evpi4$Category[vip_evpi4$Coefficient< 0] = "firebrick"
vip_evpi4_threshold <- filter(vip_evpi4, VIP>=1.0)
# Creating plot with grid and border

vip4plot<-ggplot(vip_evpi4_threshold,aes(x=VIP,y=reorder(Label,VIP)))+
  geom_bar(aes(fill=Category),stat ="identity")+ 
  xlab("VIP: IG - OF- IF ")+
  xlim(0,6)+
  ylab(NULL)+
  scale_fill_manual(values = c("cadetblue","firebrick","grey"))+
  theme(axis.text.x =element_text(color="black", size=12), 
        axis.ticks.x = element_blank())+
  geom_vline(xintercept = 1.0, size=0.2)+
  theme_bw()+
  theme(legend.position = "none")+
  theme(plot.title = element_text(hjust = 0.5))
vip4plot 


#2.1.5. PLOTTING VIP5####
# Reading the VIP scores for component 5
vip5<-read.csv("MCResults/NPV_comp5_pls_results.csv",header = TRUE,sep=",")

# Reading EVPI score for component 4
evpi5<-read.csv("EVPI_table_NPV_comp5.csv")
# Remove NA value
evpi5_omit<-na.omit(evpi5)

# Merge data of vip and evpi
vip_evpi5 <-merge(vip5, evpi5_omit)

# Creating Category for variables based on value of coefficient  
vip_evpi5$Category[vip_evpi5$Coefficient> 0] = "cadetblue"
vip_evpi5$Category[vip_evpi5$Coefficient< 0] = "firebrick"
vip_evpi5_threshold <- filter(vip_evpi5, VIP>=1.0)

# Creating plot with grid and border

vip5plot<-ggplot(vip_evpi5_threshold,aes(x=VIP,y=reorder(Label,VIP)))+
  geom_bar(aes(fill=Category),stat ="identity")+ 
  xlab("VIP: IG - OF- IF - Zero/Minimum tillage ")+
  xlim(0,6)+
  ylab(NULL)+
  scale_fill_manual(values = c("cadetblue","firebrick","grey"))+
  theme(axis.text.x =element_text(color="black", size=12), 
        axis.ticks.x = element_blank())+
  geom_vline(xintercept = 1.0, size=0.2)+
  theme_bw()+
  theme(legend.position = "none")+
  theme(plot.title = element_text(hjust = 0.5))

vip5plot 


# Combine VIP plots####

combinevip <- plot_grid(vip1plot, vip2plot, vip3plot, vip4plot, vip5plot,
                        labels = c('a)', 'b)', 'c)', 'd)','e)'),
                        scale=0.95,
                        hjust=-0.2, vjust = 1) 

ggsave("Figures/FIG6.TIF.Combine-vip.jpeg", dpi = 1000, 
       width = 9,
       height = 8)

min(vip_evpi1_threshold$VIP)
max(vip_evpi1_threshold$VIP)
min(vip_evpi2_threshold$VIP)
max(vip_evpi2_threshold$VIP)
min(vip_evpi3_threshold$VIP)
max(vip_evpi3_threshold$VIP)
min(vip_evpi4_threshold$VIP)
max(vip_evpi4_threshold$VIP)
min(vip_evpi5_threshold$VIP)
max(vip_evpi5_threshold$VIP)



# 2.2 Plotting EVPI####

# Creating plot with grid and border

evpiplot1<-ggplot(vip_evpi1_threshold,aes(x=EVPI_do,y=reorder(Label,VIP)))+
  geom_bar(aes(fill=Category),stat ="identity")+ 
  xlab("1) Improved Germplasm")+
  ylab(NULL)+
  scale_fill_manual(values = c("cadetblue","firebrick","grey"))+
  theme(axis.title.x =element_text(color="black", size=12), 
        axis.ticks.x = element_blank())+
  geom_vline(xintercept = 0, size=0.2)+
  theme_bw()+
  theme(legend.position = "none")+
  theme(plot.title = element_text(hjust = 0.5)) 
evpiplot1

# 2.2.2 EVPI 2#### 

# Creating plot with grid and border

evpiplot2<- ggplot(vip_evpi2_threshold,aes(x=EVPI_do,y=reorder(Label,VIP)))+
  geom_bar(aes(fill=Category),stat ="identity")+ 
  xlab("2) IG- Organic fertilizer")+
  ylab(NULL)+
  scale_fill_manual(values = c("cadetblue","firebrick","grey"))+
  theme(axis.title.x =element_text(color="black", size=12), 
        axis.ticks.x = element_blank())+
  geom_vline(xintercept = 0, size=0.2)+
  theme_bw()+
  theme(legend.position = "none")+
  theme(plot.title = element_text(hjust = 0.5))     
evpiplot2

# 2.2.3 EVPI 3#### 

# Creating plot with grid and border
evpiplot3<- ggplot(vip_evpi3_threshold,aes(x=EVPI_do,y=reorder(Label,VIP)))+
  geom_bar(aes(fill=Category),stat ="identity")+ 
  xlab("3) IG- Inorganic fertilizer")+
  ylab(NULL)+
  scale_fill_manual(values = c("cadetblue","firebrick","grey"))+
  theme(axis.title.x =element_text(color="black", size=12), 
        axis.ticks.x = element_blank())+
  geom_vline(xintercept = 0, size=0.2)+
  theme_bw()+
  theme(legend.position = "none")+
  theme(plot.title = element_text(hjust = 0.5)) 
evpiplot3

# 2.2.4 EVPI 4#### 

# Creating plot with grid and border

evpiplot4<-ggplot(vip_evpi4_threshold,aes(x=EVPI_do,y=reorder(Label,VIP)))+
  geom_bar(aes(fill=Category),stat ="identity")+ 
  xlab("4) IG- OF- IF")+
  ylab(NULL)+
  scale_fill_manual(values = c("cadetblue","firebrick","grey"))+
  theme(axis.title.x =element_text(color="red", size=12), 
        axis.ticks.x = element_blank())+
  geom_vline(xintercept = 0, size=0.2)+
  theme_bw()+
  theme(legend.position = "none")+
  theme(plot.title = element_text(hjust = 0.5)) 
evpiplot4


# 2.2.5 EVPI 5#### 

# Creating plot with grid and border

evpiplot5<-ggplot(vip_evpi5_threshold,aes(x=EVPI_do,y=reorder(Label,VIP)))+
  geom_bar(aes(fill=Category),stat ="identity")+ 
  xlab("4) IG- OF- IF- Zero/Minimum tillage")+
  ylab(NULL)+
  scale_fill_manual(values = c("cadetblue","firebrick","grey"))+
  theme(axis.title.x =element_text(color="red", size=12), 
        axis.ticks.x = element_blank())+
  geom_vline(xintercept = 0, size=0.2)+
  theme_bw()+
  theme(legend.position = "none")+
  theme(plot.title = element_text(hjust = 0.5)) 
evpiplot5


#### CASHFLOW ANALYSIS ####

#Cashflow for component 1 

cashflow1 <- plot_cashflow(mcSimulation_object = mc_simulation,
                           cashflow_var_name = "cashflow Improved Germplasm",
                           x_axis_name = "Years of intervention",
                           y_axis_name = "Cashflow in US Dollars",
                           color_25_75 = "grey",
                           color_5_95 = "yellow",
                           color_median= "red",
                           base= 10)

cashflow1


#Cashflow for component 2 

cashflow2 <- plot_cashflow(mcSimulation_object = mc_simulation,
                           cashflow_var_name = "cashflow IG- Organic fertilizer",
                           x_axis_name = "Years of intervention",
                           y_axis_name = "Cashflow in US Dollars",
                           color_25_75 = "grey",
                           color_5_95 = "yellow",
                           color_median= "red",
                           base= 10)

cashflow2



#Cashflow for component 3 

cashflow3 <- plot_cashflow(mcSimulation_object = mc_simulation,
                           cashflow_var_name = "cashflow IG- Inorganic fertilizer",
                           x_axis_name = "Years of intervention",
                           y_axis_name = "Cashflow in US Dollars",
                           color_25_75 = "grey",
                           color_5_95 = "yellow",
                           color_median= "red",
                           base= 10)

cashflow3



#Cashflow for component 4 

cashflow4 <- plot_cashflow(mcSimulation_object = mc_simulation,
                           cashflow_var_name = "cashflow IG- OF- IF",
                           x_axis_name = "Years of intervention",
                           y_axis_name = "Cashflow in US Dollars",
                           color_25_75 = "grey",
                           color_5_95 = "yellow",
                           color_median= "red",
                           base= 10)

cashflow4



#Cashflow for component 5

cashflow5 <- plot_cashflow(mcSimulation_object = mc_simulation,
                           cashflow_var_name = "cashflow IG- OF- IF- Zero/Minimum tillage",
                           x_axis_name = "Years of intervention",
                           y_axis_name = "Cashflow in US Dollars",
                           color_25_75 = "grey",
                           color_5_95 = "yellow",
                           color_median= "red",
                           base= 10)

cashflow5




#### END ####













#### OLD PLOT CODE ####


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


#####Simple and quick decision outcome
compound_figure(model = system_benefits,
                input_table = table,
                decision_var_name = "NPV_ISFM",
                cashflow_var_name = "cashflow",
                model_runs = 100,
                distribution_method = 'smooth_simple_overlay')



