##########################################################
# script for YF and forest fragmentation analysis

rm(list=ls())
library(tidyverse)
library(sjmisc)
library(sjPlot)
library(fixest)
library(survival)

#####################################
## read in final dataset:
#####################################

dat <- read.csv("YF_final_data.csv", header=T)
head(dat)

#####################################
# Models:
#####################################
# feols on binary, linear probability model

### main model specification:
YF_feols_FEs_LPM_main <- feols(
  Yellow_Fever_YN ~
    log.Forest_Area_ha
  + log.area_mn_forest
  + log.forest_edge_density
  + log.Forest_Urban
  + NDVI_std
  + LST_Day_std
  + Precip_std
  + Population_Density_std
  |
    Year^Country +
    Region_Code
  ,
  data=dat,
  cluster = c("Region_Code")
)
summary(YF_feols_FEs_LPM_main)
collinearity(YF_feols_FEs_LPM_main)


# model patch area:
YF_feols_FEs_LPM_area_mn <- feols(
  Yellow_Fever_YN ~
    log.Forest_Area_ha
  + log.area_mn_forest
  #+ log.forest_edge_density
  #+ log.Forest_Urban
  + NDVI_std
  + LST_Day_std
  + Precip_std
  + Population_Density_std
  |
    Year^Country +
    Region_Code
  ,
  data=dat,
  cluster = c("Region_Code")
)
summary(YF_feols_FEs_LPM_area_mn)
collinearity(YF_feols_FEs_LPM_area_mn)


# model edge:
YF_feols_FEs_LPM_edge <- feols(
  Yellow_Fever_YN ~
    log.Forest_Area_ha
  #+ log.area_mn_forest
  + log.forest_edge_density
  #+ log.Forest_Urban
  + NDVI_std
  + LST_Day_std
  + Precip_std
  + Population_Density_std
  |
    Year^Country +
    Region_Code
  ,
  data=dat,
  cluster = c("Region_Code")
)
summary(YF_feols_FEs_LPM_edge)
collinearity(YF_feols_FEs_LPM_edge)


# model forest-urban:
YF_feols_FEs_LPM_forest_urban <- feols(
  Yellow_Fever_YN ~
    log.Forest_Area_ha
  #+ log.area_mn_forest
  #+ log.forest_edge_density
  + log.Forest_Urban
  + NDVI_std
  + LST_Day_std
  + Precip_std
  + Population_Density_std
  |
    Year^Country +
    Region_Code
  ,
  data=dat,
  cluster = c("Region_Code")
)
summary(YF_feols_FEs_LPM_forest_urban)
collinearity(YF_feols_FEs_LPM_forest_urban)


##########
#Table S2: 
##########
# YF LPM models:
tab_model(YF_feols_FEs_LPM_area_mn, YF_feols_FEs_LPM_edge, YF_feols_FEs_LPM_forest_urban, YF_feols_FEs_LPM_main,
          file = "YF_LPM_models_final_tables.doc", pred.labels = c(
            "Forest Area",  "Mean Forest Patch Size", "NDVI", "Temperature", "Precipitation", "Population Density",
            "Forest Edge Density", "Forest-Urban Adjacency"
          ), show.r2 = TRUE, 
          dv.labels = c("Yellow Fever Spillover"))


###########
# Figure 2
###########
## plot average marginal effects from stata output:
#create dataframe of coefs and 95% CIs: 
LPM_area_mn_forest_area_AME <- 0.00814
LPM_area_mn_forest_area_se <- 0.003
LPM_area_mn_area_mn_AME <- -0.0196
LPM_area_mn_area_mn_se <- 0.008
LPM_area_mn_edge_density_AME <- NA
LPM_area_mn_edge_density_se <- NA
LPM_area_mn_forest_urban_AME <- NA
LPM_area_mn_forest_urban_se <- NA

LPM_edge_forest_area_AME <- -0.000126
LPM_edge_forest_area_se <- 0.004
LPM_edge_area_mn_AME <- NA
LPM_edge_area_mn_se <- NA
LPM_edge_edge_density_AME <- 0.0186
LPM_edge_edge_density_se <- 0.009
LPM_edge_forest_urban_AME <- NA
LPM_edge_forest_urban_se <- NA

LPM_forest_urban_forest_area_AME <- -0.00253
LPM_forest_urban_forest_area_se <- 0.002
LPM_forest_urban_area_mn_AME <- NA
LPM_forest_urban_area_mn_se <- NA
LPM_forest_urban_edge_density_AME <- NA
LPM_forest_urban_edge_density_se <- NA
LPM_forest_urban_forest_urban_AME <- 0.00926
LPM_forest_urban_forest_urban_se <- 0.003

LPM_main_forest_area_AME <- 0.00354
LPM_main_forest_area_se <- 0.005
LPM_main_area_mn_AME <- -0.0197
LPM_main_area_mn_se <- 0.011
LPM_main_edge_density_AME <- 0.000165
LPM_main_edge_density_se <- 0.011
LPM_main_forest_urban_AME <- 0.00921
LPM_main_forest_urban_se <- 0.003

AMEs <- rbind(LPM_area_mn_forest_area_AME, LPM_area_mn_area_mn_AME, LPM_area_mn_edge_density_AME, LPM_area_mn_forest_urban_AME,
              LPM_edge_forest_area_AME, LPM_edge_area_mn_AME, LPM_edge_edge_density_AME, LPM_edge_forest_urban_AME,
              LPM_forest_urban_forest_area_AME, LPM_forest_urban_area_mn_AME, LPM_forest_urban_edge_density_AME, LPM_forest_urban_forest_urban_AME,
              LPM_main_forest_area_AME, LPM_main_area_mn_AME, LPM_main_edge_density_AME, LPM_main_forest_urban_AME)
SEs <- rbind(LPM_area_mn_forest_area_se, LPM_area_mn_area_mn_se, LPM_area_mn_edge_density_se, LPM_area_mn_forest_urban_se,
             LPM_edge_forest_area_se, LPM_edge_area_mn_se, LPM_edge_edge_density_se, LPM_edge_forest_urban_se,
             LPM_forest_urban_forest_area_se, LPM_forest_urban_area_mn_se, LPM_forest_urban_edge_density_se, LPM_forest_urban_forest_urban_se,
             LPM_main_forest_area_se, LPM_main_area_mn_se, LPM_main_edge_density_se, LPM_main_forest_urban_se)
AME_SE <- cbind(AMEs, SEs)
AME_SE
AME_SE <- as.data.frame(AME_SE)
colnames(AME_SE)[1] <- "AMEs"
colnames(AME_SE)[2] <- "SEs"
AME_SE
vars <- c("Forest area", "Forest patch size", "Forest edge density", "Forest-urban",
          "Forest area", "Forest patch size", "Forest edge density", "Forest-urban",
          "Forest area", "Forest patch size", "Forest edge density", "Forest-urban",
          "Forest area", "Forest patch size", "Forest edge density", "Forest-urban")
AME_SE_dataframe <- cbind(AME_SE, vars)
AME_SE_dataframe
AME_SE_dataframe <- AME_SE_dataframe[,c(3, 1, 2)]
AME_SE_dataframe <- as.data.frame(AME_SE_dataframe)
AME_SE_dataframe$upper <- AME_SE_dataframe$AMEs + (1.96*AME_SE_dataframe$SEs)
AME_SE_dataframe$lower <- AME_SE_dataframe$AMEs - (1.96*AME_SE_dataframe$SEs)
AME_SE_dataframe
nrow(AME_SE_dataframe)
AME_SE_dataframe$Model <- c("Patch size", "Patch size", "Patch size", "Patch size",
                            "Edge density", "Edge density", "Edge density", "Edge density",
                            "Forest-urban", "Forest-urban", "Forest-urban", "Forest-urban",
                            "Main model", "Main model", "Main model", "Main model")
AME_SE_dataframe

#Figure 2:
AME_plot <- ggplot(AME_SE_dataframe, aes(x=vars, y=AMEs, color=Model)) +
  geom_point(size=3, position=position_dodge(width=0.5)) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.5, position=position_dodge(width=0.5)) + 
  geom_hline(yintercept = 0, color='black') +
  scale_y_continuous(limits = c(-0.05, 0.05)) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(), 
        axis.text = element_text(size = 10),
        axis.title=element_text(size=12,face="bold"), 
        axis.text.x = element_text(angle = 45),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12),
        plot.title = element_text(size = 14,face="bold")) +
  labs(y= "Average marginal effect (95% CI)", x=NULL) +
  ggtitle("Effect of forest fragmentation on Yellow Fever spillover") +
  scale_color_manual(values = c("Patch size" = "#440154FF", "Edge density" = "#2A788EFF", "Forest-urban" = "#7AD151FF", "Main model" = "#FDE725FF")) +
  annotate("rect", xmin = 3.5, xmax = 4.5, ymin = -0.02, ymax = 0.035, alpha = 0, color= "darkgrey")
AME_plot


############################
# Yellow fever logistic:
############################
######## logistic:
YF_logistic_FEs_nonzero_main <- feglm(
  Yellow_Fever_YN ~
    log.Forest_Area_ha
  + log.area_mn_forest
  + log.forest_edge_density
  + log.Forest_Urban
  + NDVI_std
  + LST_Day_std
  + Precip_std
  + Population_Density_std
  |
    Year^Country +
    Region_Code
  ,
  data=dat,
  family=binomial(link="logit")
  ,
  cluster = c("Region_Code")
)
summary(YF_logistic_FEs_nonzero_main)
collinearity(YF_logistic_FEs_nonzero_main)


######### conditional logistic regression (for use in final table model comparison): 
YF_cond_logistic_FEs_nonzero_main <- clogit(
  Yellow_Fever_YN ~
    log.Forest_Area_ha
  + log.area_mn_forest
  + log.forest_edge_density
  + log.Forest_Urban
  + NDVI_std
  + LST_Day_std
  + Precip_std
  + Population_Density_std
  + strata(Year)*strata(Country)
  + strata(Region_Code)
  ,
  data=dat,
  method="approximate",
  cluster=Region_Code,
  na.action=na.exclude
)
summary(YF_cond_logistic_FEs_nonzero_main)

#########
#Table S1: 
#########
# YF LPM and conditional logistic models:
tab_model(YF_feols_FEs_LPM_main, YF_cond_logistic_FEs_nonzero_main,
          file = "YF_LPM_and_clogit_models_final_tables.doc", pred.labels = c(
            "Forest Area",  "Mean Forest Patch Size", "Forest Edge Density", "Forest-Urban Adjacency",
            "NDVI", "Temperature", "Precipitation", "Population Density"
          ), show.r2 = TRUE, 
          dv.labels = c("Yellow Fever Spillover"))

