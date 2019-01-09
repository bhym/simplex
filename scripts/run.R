for (ti in 2:tmax) {
  producers <- Species[Species %in%
                       subset(secon_subpop, trophic_status == "A")$fn]
  a <- array(NA, nrow(inds))
  a <- inds[, 3] + 1
  inds[, 3] <- a
  Nu <- Nu.dynamics(inds, Nu, parms, producers)
  Nu <- ifelse(Nu > 0, Nu, 0)#+ Nu_in
  if (nrow(inds) > 0) {
    inds <- producer.growth(inds, Nu, parms, producers)
  }
  if (nrow(inds) > 0) {
    inds <- kill.inds(inds, M, producers)
  }
  if (nrow(inds) > 0) {
   inds <- Encounter3(inds, giug)
  }
  # ctrl Spuno_pre <- rbind(Spuno_pre, cbind(Time = ti, subset(inds, species == 1)))
  if (nrow(inds) > 0) {
    inds <- reproduce.inds(inds)
  }
  # ctrl Spuno_post <- rbind(Spuno_post, cbind(Time = ti, subset(inds, species == 1)))

  # store results
  N[ti, ] <- summary(na.omit(inds$species))
  sp.aveb[ti, ] <- by(inds$biomass, inds$species, mean, na.rm = T)
  sp.totb[ti, ] <- by(inds$biomass, inds$species, sum, na.rm = T)
  Nu.dyn[ti, ]  <- Nu
  totb          <- rbind(totb, cbind(t = ti, sum(inds$biomass, na.rm = T)))
  print(paste("time =", ti, "; tot = ", nrow(inds), "; super = ",
              list(table(inds[which(inds$biomass >= 2 * inds$basemass), ][, 2])
                   )))
}
