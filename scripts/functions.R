make.inds <- function(id = NaN, species = NaN, age = NaN, biomass = NaN,
                      basemass = NaN, parents = NaN, enc_suca = NaN,
                      resp_rate = NaN, prd_rate = NaN,
                      ass_eff = NaN) {
  inds <- data.table(id = id, species = species, age = age, biomass = biomass,
                     basemass = basemass, parents = parents,
                     enc_suca = enc_suca, resp_rate = resp_rate,
                     prd_rate = prod_rate_long,
                     ass_eff = ass_eff)
  return(inds)
}

producer.growth <- function(inds, Nu, parms, producers) {
  prody <- inds$species %in% producers
  lepre <- length(prody)
  if (sum(prody) == 0){
    val <- 0
  } else {
    avail_n <- vector("double", length(Nu))
    avail_n <- Nu / sum(prody > 0)
    biomass_production <- vector("double", lepre)
    biomass_production <- inds$prd_rate * inds[, 4] * 1 / 16 *
                          min(avail_n / (avail_n + parms[[2]]))
    refval <- 2 * inds$basemass - inds$biomass
    biomass_production <- unlist(biomass_production, use.names = F)
    val <- ifelse(biomass_production < refval, biomass_production, refval)
  }
  inds[, 4] <- inds[, 4] + val * prody - inds[, 8] * inds[, 4]
  return(inds)
}

kill.inds <- function(inds, M, producers) {
  kill <- which( (inds[, 4] <= 0.5 * inds[, 5]))
  if (length(kill) != 0){
    inds <- inds[-kill]
  }
  return(inds)
}

reproduce.inds <- function(inds) {
  ready_to_split <- which(inds[, 4] >= 2 * inds[, 5])
  n_ready_to_split <- length(ready_to_split)
  masses <- vector("double", n_ready_to_split)
  masses <- runif(n_ready_to_split,
                  min = min_asym_bio * inds$basemass[ready_to_split],
                 max = (2 - min_asym_bio) * inds$basemass[ready_to_split])
  if (n_ready_to_split > 0){
    starting <- make.inds(id       = seq(max(inds[, 1]) + 1,
                                       length.out = n_ready_to_split),
                          species  = inds$species[ready_to_split],
                          age      = 0,
                          biomass  = masses,
                          basemass = inds$basemass[ready_to_split],
                          parents  = inds$id[ready_to_split],
                          enc_suca = inds$enc_suca[ready_to_split],
                          resp_rate = inds$resp_rate[ready_to_split],
                          prd_rate = inds$prd_rate[ready_to_split],
                          ass_eff = inds$ass_eff[ready_to_split]
                          )
   inds$biomass[ready_to_split] <- inds$biomass[ready_to_split] - masses
   inds <- data.table::rbindlist(list(inds, starting), use.names = T, fill = F)
  }
  return(inds)
}

Nu.dynamics <- function(inds, Nu, parms, producers){
  producers_biomass <- subset(inds, inds$species %in% producers)
  sum_producers_biomass_growth <- sum(producers_biomass$prd_rate
                                      * producers_biomass[, 4])
  l <- length(sum_producers_biomass_growth)
  if (l > 0){
  avail_n <- Nu / l
  consu_n <- vector("double", length(Nu))
  consu_n <- sum_producers_biomass_growth *
             16 / 106 *
             (parms[[1]] * avail_n / (avail_n + parms[[2]]))
  Nu <- Nu - consu_n
  }
  return(Nu)
}

Encounter3 <- function(inds, giug){
  lele <- nrow(inds)
  indices <- vector("integer", lele)
  indices <- seq(lele - lele %% 2) #This is to avoid R complains if lele is odd
  encounter_split_len <- lele %/% 2 #This is to avoid R complains if lele is odd
  scrambled_idxs <- sample(indices)
  ran_first_group_idxs <- vector("integer", encounter_split_len)
  ran_first_group_idxs <- head(scrambled_idxs, encounter_split_len)
  ran_secon_group_idxs <- vector("integer", encounter_split_len)
  ran_secon_group_idxs <- tail(scrambled_idxs, encounter_split_len)
  a <- array(NA, dim = c(encounter_split_len, 2))
  #GARBAGE PRODUCED HERE BECAUSE OF SUBSETTING
  a[, 1] <- inds[ran_first_group_idxs, ]$species
  a[, 2] <- inds[ran_secon_group_idxs, ]$species
  check_interactions <- vector("integer", encounter_split_len)
  check_interactions <- giug[a]
  first_wz_a <- data.table(a = ran_first_group_idxs,
                         b = ran_secon_group_idxs,
                         ci = check_interactions)
  first_wz <- first_wz_a[ci != 0]
  lel <- nrow(first_wz)
  first <- first_wz
  second <- data.table::data.table(a = first$b, b = first$a, ci = -first$ci)
  first[ci > 0] <- second[ci < 0]
  success.vector <- which(runif(lel) < inds[first$a]$enc_suca)
  first <- first[success.vector]
  if (nrow(first) > 0){
    a_ <- double(length(first$a))
    a_ <- first$a
    val <-  inds[a_]$ass_eff * inds[first$b]$biomass
    refval <- 2 * inds[a_]$basemass - inds[a_]$biomass
    val <- ifelse(val < refval, val, refval)
    inds[a_]$biomass <- inds[a_]$biomass + inds[a_]$prd_rate * val
    kill <- first$b
    inds <- inds[-kill]
  } else {
    return(inds)
  }
}

Encounter4 <- function(inds,giug){
  # calculate the total biomass of preys
  # calculate the number of predators
  # divide the biomass times the number of predators
  # each predator's biomass increase of eff * catched_prey * H2
  # a number of preys

}
