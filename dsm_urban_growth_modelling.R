# ==========================================================
# GIS–RS–LR-CA Urban Growth Model: DSM  Prediction  2 0 5 0
# ==========================================================

# 0. Load Packages
required_packages <- c("terra","pROC","ggplot2","dplyr","patchwork")
installed <- rownames(installed.packages())
for(pkg in required_packages){
  if(!(pkg %in% installed)){
    install.packages(pkg, dependencies=TRUE)
  }
}
lapply(required_packages, library, character.only=TRUE)

# 1. Set working Directory
setwd("Your working directory")

# 2. Load Rasters
Slope      <- rast("Slope.tif")
Road       <- rast("Road.tif")
River      <- rast("River.tif")
BNLC2010  <- rast("BNLC2010.tif")
BNLC2020  <- rast("BNLC2020.tif")
Constraint <- rast("Constraint.tif")

# 3. Align Rasters to 2020 reference
ref <- BNLC2020
Slope      <- resample(Slope, ref, method="bilinear")
Road       <- resample(Road, ref, method="bilinear")
River      <- resample(River, ref, method="bilinear")
BNLC2010  <- resample(BNLC2010, ref, method="near")
Constraint <- resample(Constraint, ref, method="near")

# 4. Binary Urban Maps
urban2010 <- ifel(BNLC2010 == 2, 1, 0)
urban2020 <- ifel(BNLC2020 == 2, 1, 0)

# Transition 2010 -> 2020
transition <- ifel((urban2010 == 0) & (urban2020 == 1), 1, 0)

# 5. Logistic Regression for Transition Probability
drivers <- c(Road,River,Slope)
names(drivers) <- c("Road","River","Slope")
stack_all <- c(transition, drivers)
names(stack_all)[1] <- "transition"

sample_data <- spatSample(stack_all, size=5000, method="random", na.rm=TRUE)
df <- as.data.frame(sample_data)

logit_model <- glm(transition ~ Road + River + Slope,
                   data=df, family=binomial)

prob_map <- predict(drivers, logit_model, type="response")

# 6. Cellular Automata Simulation Function
simulate_CA <- function(iterations, urban_start, prob_map, Constraint,
                        suitability_threshold=0.3, transition_probability=0.5, noise_factor=0.05){
  current_urban <- urban_start
  w <- matrix(1,3,3)
  
  for(i in 1:iterations){
    neigh_count <- focal(current_urban, w=w, fun=sum, na.rm=TRUE)
    neigh_rule <- neigh_count >= 1
    
    suitability_rule <- prob_map >= suitability_threshold
    constraint_rule <- Constraint == 0
    
    rand_prob <- runif(ncell(current_urban))
    prob_rule <- setValues(current_urban, rand_prob) <= transition_probability
    
    rand_noise <- runif(ncell(current_urban))
    stochastic_rule <- setValues(current_urban, rand_noise) <= noise_factor
    
    new_urban <- (current_urban == 1) |
      ((current_urban == 0) &
         neigh_rule & suitability_rule & constraint_rule & prob_rule) |
      (stochastic_rule & constraint_rule)
    
    values(current_urban) <- as.numeric(values(new_urban))
  }
  
  return(current_urban)
}

# 7. Simulate 2030, 2040, 2050
urban2030 <- simulate_CA(2, urban2020, prob_map, Constraint)
urban2040 <- simulate_CA(4, urban2020, prob_map, Constraint)
urban2050 <- simulate_CA(6, urban2020, prob_map, Constraint)

# Fill NA/non-data pixels within 2020 DSM extent as urban
dsm_mask <- !is.na(BNLC2020)
values(urban2030)[is.na(values(urban2030)) & values(dsm_mask) == 1] <- 1
values(urban2040)[is.na(values(urban2040)) & values(dsm_mask) == 1] <- 1
values(urban2050)[is.na(values(urban2050)) & values(dsm_mask) == 1] <- 1

# 8. Model Validation
roc_obj <- roc(df$transition, fitted(logit_model))
auc_value <- auc(roc_obj)
print(paste("Model AUC =", round(auc_value,3)))

# 9. Save Outputs
writeRaster(prob_map, "Transition_Potential.tif", overwrite=TRUE)
writeRaster(urban2010, "Urban_2010_Binary.tif", overwrite=TRUE)
writeRaster(urban2020, "Urban_2020_Binary.tif", overwrite=TRUE)
writeRaster(transition, "Urban_Transition_2010_2020.tif", overwrite=TRUE)
writeRaster(urban2030, "Urban_2030_Binary.tif", overwrite=TRUE)
writeRaster(urban2040, "Urban_2040_Binary.tif", overwrite=TRUE)
writeRaster(urban2050, "Urban_2050_Binary.tif", overwrite=TRUE)

# ==========================================================
# 10. Maps Visualization
# ==========================================================
raster_to_df <- function(r){
  df <- as.data.frame(r, xy=TRUE)
  colnames(df)[3] <- "value"
  if(all(unique(na.omit(df$value)) %in% c(0,1))){
    df$value <- factor(df$value, levels=c(0,1))
  }
  return(df)
}

plot_urban <- function(r, title){
  df <- raster_to_df(r)
  urban_colors <- scale_fill_manual(
    values=c("0"="green","1"="yellow"),
    labels=c("Non-Urban","Urban"),
    name="Class"
  )
  common_theme <- theme_minimal() +
    theme(legend.position="bottom",
          axis.title=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          plot.title=element_text(hjust=0.5))
  
  ggplot(df) +
    geom_tile(aes(x=x, y=y, fill=value)) + 
    urban_colors + coord_equal() +
    ggtitle(title) + common_theme
}

# 11. Plot Maps
plot_urban(urban2010, "DSM Urban 2010")
plot_urban(urban2020, "DSM Urban 2020")
plot_urban(transition, "Urban Transition 2010-2020")

# Transition Probability Map
df_prob <- as.data.frame(prob_map, xy=TRUE)
colnames(df_prob)[3] <- "value"
ggplot(df_prob) +
  geom_tile(aes(x=x, y=y, fill=value)) +
  scale_fill_viridis_c(name="Transition Probability") + coord_equal() +
  ggtitle("Transition Potential Map") +
  theme_minimal() +
  theme(legend.position="bottom",
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        plot.title=element_text(hjust=0.5))
plot_urban(urban2030, "Predicted DSM Urban 2030")
plot_urban(urban2040, "Predicted DSM Urban 2040")
plot_urban(urban2050, "Predicted DSM Urban 2050")

# ==========================================================
# End of Script
# ==========================================================