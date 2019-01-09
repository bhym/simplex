#Parameters
make_parms <- function(input, secon_subpop) {
  parms       <- list(mu  = input$mu,  k_n = input$k_n)
  M           <- list(prod = input$d_prod, cons = input$d_cons)
  bas         <- input$bas
  Species     <- levels(factor(secon_subpop$fn))

  Nu          <- c(Nu1 = input$n10, Nu2 = input$n20)
  Nu_in       <- 0

  tmax        <- input$tma
  weight      <- input$weight
  enc_succ    <- input$enc_succ
  min_asym_bio <- input$min_asym_bio
  level_num        <- input$level_num
  species_number   <- input$species_number
  troph_size_class <- input$troph_size_class
  predati_effici   <- input$predati_effici
  product_rate   <- input$product_rate
  respiration      <- input$respiration
  assimilatio      <- input$assimilatio

  temp <- list(parms = parms, M = M, bas = bas, Species = Species,
               Nu = Nu, Nu_in = Nu_in, tmax = tmax, weight = weight,
               enc_succ = enc_succ, min_asym_bio = min_asym_bio,
               level_num = level_num, species_number = species_number,
               troph_size_class = troph_size_class,
               predati_effici = predati_effici, respiration = respiration,
               product_rate = product_rate, assimilatio = assimilatio)

  i <- names(temp)
  invisible(lapply(seq_along(temp), function(x) {
    assign(i[x], temp[[x]], envir = .GlobalEnv)
                            }
            )
    )
  return()
}


make_troph_net <- function(input, Species){
  giug <- matrix(0, nrow = length(Species), ncol = length(Species))
  giug[upper.tri(giug)] <- sample(c(1, 0, -1), sum(upper.tri(giug)),
                                      replace = T)
  produs <- which(Species %in% subset(secon_subpop,
                                      secon_subpop$trophic_status == "A")$fn)
  giug[produs, ] <- ifelse(giug[produs, ] < 0, 0, 1)
  giug[produs, produs] <- 0
  giug[lower.tri(giug)] <- -t(giug)[lower.tri(giug)]
  invisible(assign("giug", giug, envir = .GlobalEnv))
}
