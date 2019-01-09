## FIXED PARAMETERS 
#- percent of biomass expendable before death (0.5) 
#- minimum and maximum percent for asymmetric biomass redistribution 
# after division (0.7 / 1.3) 

#their only death source is predation
#Everyone has the same amount of nutrients

#WORKING WITH TEN SPECIES, FIXED NETWORK
#NUMBER OF TROPHIC LEVELS, NUMBER OF INDIVIDUALS PER SPECIES,
#AND SIZE DIFFERENCES ARE INTO initialization.R

#Numerical instabilities
 # put max over growth for everyone

input0 <- list(mu = 1, k_n = 0.001, d_prod = 0.0, d_cons = 0.0,
              bas = 1e-9, n10 = 0.1, n20 = 0.1, tma = 10,
              min_asym_bio = 0.7, level_num = 0:3, species_number = 1,
              troph_size_class = secon_subpop$individual_carbon,
              predati_effici   = 1,
              product_rate     = secon_subpop$µ,
              respiration      = 0.001,
              assimilatio      = 1 - ifelse(is.na(secon_subpop$ε),
                                            0, secon_subpop$ε)
              )
