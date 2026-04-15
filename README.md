GIS–RS–LR-CA Urban Growth Model (DSM Prediction 2050)

Overview - This project models and predicts urban growth in Dar es Salaam
using Remote Sensing (RS), Logistic Regression, and Cellular Automata
(CA). It analyzes land cover change from 2010 to 2020 and simulates
future urban expansion up to 2050.

Objectives - Analyze urban growth (2010–2020) - Model transition
probability using logistic regression - Simulate future urban expansion
(2030, 2040, 2050)

Data - Slope.tif – Terrain slope - Road.tif – Distance to roads -
River.tif – Distance to rivers - BNLC2010.tif – Land cover 2010 -
BNLC2020.tif – Land cover 2020 - Constraint.tif – Restricted areas

Methodology 1. Preprocess and align raster datasets 2. Generate binary
urban maps 3. Model urban transition using logistic regression 4.
Simulate urban growth using Cellular Automata 5. Validate model using
ROC and AUC

Outputs - Transition probability map - Urban maps: 2010, 2020, 2030,
2040, 2050 - Visualization of urban growth and transition potential

Tools - R - terra, pROC, ggplot2, dplyr, patchwork

How to Run git clone https://github.com/RugaiyaMemes/dsm_urban_growth_modelling.git

setwd("G:/My Drive/My Projects/Papers/Dataset/RStudio/dsm_urban_growth_modelling") source(“dsm_urban_growth_modelling.R”)

Notes - Ensure raster files are correctly placed before running - Large
datasets may require high memory

Authors - Rugaiya Justin Henriko, Frank Joseph Wambura, and Tumpale Sakijege

License For academic and research use

Future Work - Add more drivers (e.g., population, socio-economic data) -
Apply machine learning models - Improve spatial resolution
