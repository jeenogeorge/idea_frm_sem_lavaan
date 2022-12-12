library(lavaan)
library(semTools)
library(caret)
library(dplyr)
library(tidyverse)
library(mice)

#confirmatory factor analysis (CFA)
sem_data <- read.csv("sem/SEM_Input_Consolidated - cum_5y.csv")
nrow(sem_data)
#replace na with 0 as NA are only in the tenders data
sem_data <- sem_data %>% replace(is.na(.), 0)
# view(sem_data)
#normalise data with min-max scaling
process <- preProcess(sem_data_with_rev_circle, method = c("range"))
norm_scale <- predict(process, sem_data_with_rev_circle)
#model syntax
sem_model <- '

#latent variable definitions
fld_proneness =~ Inundation + assam_dist_from_major_rivers_updated_3857 + sum + GCN250_ARCIII_average + strm_filled_slope_degrees + ndvi + srtm_filled_dem + assam_soil_silt + assam_soil_loamysand + assam_lith_neogene + assam_lith_paleogene + assam_lith_paleozoic + assam_lith_quaternary + assam_lith_tertiary + assam_lith_undevelopedprecambrian + landuse_rangeland + landuse_vegetation
demo_vul =~ ind_ppp_UNadj + aged + young + percaay + deprived + nophone + noSanitation + nodrinkingWater + totLivestock
infr_access =~ ndbi + proximity_hosptial_rd + proximity_embankment_rd + proximity_rail_rd + proximity_local_rd + proximity_arterial_rd
impact =~ population_affected + human_lives_lost + roads_dam + bridges_dam + embankment_total
prep =~ Count_Total + Count_SDRF + Count_SOPD + Count_RIDF + Sum_Total + Sum_RIDF + Sum_Erosion

# regressions
impact ~ p1*fld_proneness + p2*demo_vul + p3*infr_access
prep ~ p4*fld_proneness + p7*impact + p6*infr_access + p5*demo_vul
# effects
total := p4 + p1*p7 + p2*p7 + p3*p7 + p6 + p5
indirect := p1*p7 + p2*p7 + p3*p7
ind1 := p4+p5+p6
'
#fit model 
estimator <- "DWLS" # change to "ULS"

fit <- sem(sem_model, data = norm_scale, estimator = estimator)
#summary of the fitted model
summary(fit)
#visualisation
fitMeasures(fit)
write.csv(fitMeasures(fit), sprintf("%s_fitmeasures.csv",estimator))

residual_matrix <- lavInspect(fit, what = "sampstat")$cov - 
  lavInspect(fit, what = "implied")$cov

#estimates
parameterEstimates(fit)
# coef(fit)
write.csv(parameterEstimates(fit), sprintf("%s_estimates.csv",estimator))
# fitted(fit)
#to predict the latent variables
latent_predict <- as.data.frame(lavPredict(fit))
latent_predict$objectid <- sem_data$object_id

write.csv(latent_predict, sprintf("%s_predict_lantent_variable.csv",estimator))
