# Initial population
sp.set <-  trunc(secon_subpop$green_number / 1e9)
# perfect division in_bio_fac  <- 1
in_bio_fac  <- runif(sum(sp.set), min = min_asym_bio,
                                     max = (2 - min_asym_bio)
                    )
species_long <- rep(factor(secon_subpop$fn), sp.set)
# a in_bio_spsp <- rep(troph_size_class,      sp.set)
pred_effici_long <- rep(predati_effici,   sum(sp.set))
prod_rate_long <- rep(product_rate,   sp.set)
respiration_long <- rep(respiration,      sum(sp.set))
assimeff_long <- rep(assimilatio,      sp.set)
in_base <- rep(secon_subpop$individual_carbon_ng, sp.set)
in_bio  <- in_bio_fac * in_base
inds    <- make.inds(id = seq_along(species_long), species = species_long,
                     age = 0,
                     biomass = in_bio, basemass = in_base, parents = 0,
                     enc_suca = pred_effici_long, resp_rate = respiration_long,
                     prd_rate = prod_rate_long,
                     ass_eff = assimeff_long)

# Object for storing results (population size in time)
N <- array(NaN, dim = c(tmax, length(Species)))
colnames(N) <- Species
N[1, ] <- summary(inds$species)

# Objects for storing total biomass 
totb <- cbind(t = 0, sum(inds$biomass))

# Object for storing total biomass PER SPECIES
sp.totb <- array(NaN, dim = c(tmax, length(Species)))
sp.totb[1, ] <- by(inds$biomass, inds$species, sum, na.rm = T)

# Object for storing average biomass PER SPECIES
sp.aveb <- array(NaN, dim = c(tmax, length(Species)))
sp.aveb[1, ] <- by(inds$biomass, inds$species, mean, na.rm = T)

# Object for storing Nutrient concentration
Nu.dyn <- array(NA, dim = c(tmax, length(Nu)))
Nu.dyn[1, ] <- Nu
indo <- inds

# Object for storing species 1 trajectories
# ctrl Spuno_pre  <- cbind(Time = 1, subset(inds, species == 1))
# ctrl Spuno_post <- cbind(Time = 1, subset(inds, species == 1))
 
# Temporary object for storing count of individuals with negative biomass
#a sp.nega <- array(NaN, dim = c(tmax, length(Species)))
#a sp.nega[1, ] <- by(inds$biomass, inds$species, function(x) {
#a     sum(x < 0, na.rm = T)
#a })
