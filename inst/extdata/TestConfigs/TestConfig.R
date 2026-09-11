#------------------------#
#### General settings ####
#------------------------#
comb_vector <- list(seed = 28015, dispersal = 240, t_evo = 0.03) #

random_seed <- comb_vector$seed #28015
duration <- list(
  from = NA,
  to = NA,
  by = NA,
  unit = "Myr"
)
max_number_of_species <- 20000
max_number_of_coexisting_species <- 20000
initial_abundance <- 10

# ecological local equilibria variable J*
get_J <- function(a_ff, a_fh, K_f) {
  J <- sum((a_ff * K_f) / (a_ff - a_fh), na.rm = T) /
    (1 + sum((a_fh / (a_ff - a_fh)), na.rm = T)) # new
  return(J)
}

# ecological environmental function
fg <- function(x, a, b, c, ns = 1) {
  a <- a / c
  v <- a * exp(-((x - b)^2 / (2 * c^2)))
  return(v)
}

apply_trs_tradeoff <- function(traits) {
  #balance out traits in case they are above the traitoff surface
  cd <- traits[, c("dispersal", "competition")]
  # dispersal values on trade-off linear function. e.g. lines(x=c(0,1), y=c(0.9,1))
  dsurf <- -10 * cd[, "competition"] + 10
  csurf <- (10 - cd[, "dispersal"]) / 10
  Dy <- cd[, "dispersal"] - dsurf
  # Dx <- cd[,"competition"]-csurf
  # get traits above and to the right of this linear function
  above_surface_mask <- Dy > 0
  if (any(above_surface_mask)) {
    # then push them back to surface
    # print("traits were above the trait tradeoff surface... bringing them back to traide-off surface")
    # sample if either surface should be corresponding to d or c.
    c_or_d <- sample(
      list(c(T, F), c(F, T)),
      size = sum(above_surface_mask),
      replace = TRUE
    )
    c_or_d <- do.call(rbind, c_or_d)
    cd[above_surface_mask, ][c_or_d] <- cbind(
      dsurf[above_surface_mask],
      csurf[above_surface_mask]
    )[c_or_d]
    new_traits <- traits
    new_traits[, colnames(cd)] <- cd

    return(new_traits)
  } else {
    #do nothing

    return(traits)
  }
}

# a list of traits to include with each species, traits with ss_eff_ are hack traits to extract in site processes
trait_names <- c("dispersal", "t_opt", "t_range", "competition")

# ranges to scale the input environemts with:
# not listed variable:         no scaling takes place
# listed, set to NA:           the environmental variable will be scaled from [min, max] to [0, 1]
# lsited with a given range r: the environmental variable will be scaled from [r1, r2] to [0, 1]
# environmental_ranges = list("mean_temp"=c(9,26), "min_temp"=c(9,26),  "max_temp"=c(9,26))

#-------------------------#
#### Observer Function ####
#-------------------------#

end_of_timestep_observer <- function(data, vars, config) {
  plot_richness(data$all_species, data$space)
  plot_ranges(data$all_species, data$space)
  save_species()
  save_abundance()
  save_divergence()
  save_occupancy()
  save_phylogeny()
  save_traits()
}


#----------------------#
#### Initialization ####
#----------------------#

create_ancestor_species <- function(space, config) {
  # make yls latitudinal lines for the 5 species: 2 at extremes, 2 inbetween and 1 at the equator
  co <- space$coordinates
  co <- co[rownames(co) %in% rownames(space$environment), ]
  # polar line y coordinate
  plyc <- min(abs(range(co[, "y"])))
  # temp line
  tlyc <- co[, "y"][which.min(abs(co[, "y"] - plyc / 2))] # get closest avaiable y to the pl/2
  # equator line
  elyc <- co[, "y"][which.min(abs(co[, "y"] - 0))] # get closest avaiable y to the 0
  yls <- c(
    "polN" = as.numeric(plyc),
    "tempN" = as.numeric(tlyc),
    "Eq" = as.numeric(elyc),
    "tempS" = as.numeric(-tlyc),
    "polS" = as.numeric(-plyc)
  )
  new_species <- list()
  for (i in 1:length(yls)) {
    # i <- 3
    initial_sites <- rownames(co[abs(co[, "y"] - yls[i]) < 30, ]) #tolerance of 5 degrees
    # initial_sites <- sample(initial_sites, 1)
    print(paste("i", i, "yls", yls[i], "n_sites", length(initial_sites)))
    new_species[[i]] <- create_species(initial_sites, config)
    #set local adaptation to max optimal temp equals local temp
    new_species[[i]]$traits[, "dispersal"] <- 1
    new_species[[i]]$traits[, "competition"] <- 0.92
    new_species[[i]]$traits[, "t_opt"] <- space$environment[
      initial_sites,
      "temperature"
    ]
    new_species[[i]]$traits[, "t_range"] <- 0.4
    #plot_species_presence(new_species[[i]], space)
  }
  return(new_species)
}


#-----------------#
#### Dispersal ####
#-----------------#

# returns n dispersal values (proba distrib function)
get_dispersal_values <- function(n, species, space, config) {
  # mean_abd <- mean(species$abundance)
  # weight_abd <- species$abundance/mean_abd
  # # if shape =1 then it is an exponential distribution
  # values <- rweibull(n, shape = 1, scale = 100+(comb_vector$dispersal*(mean(species$traits[,"dispersal"]*weight_abd))))  #from 1 to 50
  values <- rep(9999, n)
  return(values)
}


#------------------#
#### Speciation ####
#------------------#

# threshold for genetic distance after which a speciation event takes place
divergence_threshold <- 1 # between 10 and 50 ? as 0.1 to 0.5 Myrs or 100 - 500 kyrs

# adds a value of 1 to each geographic population cluster
get_divergence_factor <- function(species, cluster_indices, space, config) {
  return(1)
}

#-----------------------#
#### Trait Evolution ####
#-----------------------#

apply_trait_evolution <- function(species, cluster_indices, space, config) {
  trait_evolutionary_power <- comb_vector$t_evo
  pw_tr_hom <- 0.5 # percentage of movement of local trait towards weighted trait mean within each population cluster
  # pw_tr_hom = ZERO means no change while a value of ONE means that traits are equal within each populations cluster)
  traits <- species[["traits"]]
  # site names
  sites <- rownames(traits)
  #homogenize trait based on abundance
  # Homogenize all traits by weighted abundance, attention, exclude here any trait that should be more neutral
  # Bring traits closer to 50% of it's distance to the abundance weighted mean of the demographically connected
  # population cluster
  trn <- config$gen3sis$general$trait_names
  for (cluster_index in unique(cluster_indices)) {
    # cluster_index <- 1
    sites_cluster <- sites[which(cluster_indices == cluster_index)]
    B_t <- sum(species$abundance[sites_cluster]) # total biomass
    B_r <- species$abundance[sites_cluster] / B_t # relative biomass
    for (ti in trn) {
      # par(mfrow=c(1,2))
      # hist(traits[sites_cluster, ti], main="before")
      traits_ti <- traits[sites_cluster, ti]
      mean_trait <- sum(traits_ti * B_r)
      tr_hom <- mean_trait - traits_ti
      traits[sites_cluster, ti] <- traits_ti + (tr_hom * pw_tr_hom)
      # hist(traits[sites_cluster, ti], main="after")
    }
  }

  #mutate all traits except dispersal and competitive ability M0 and summary effective traits, which where excluded previously
  for (ti in c("t_opt", "t_range", "competition", "dispersal")) {
    #trn[!trn%in%"dispersal"]){ # do not evolve dispersal
    if (ti == "competition") {
      # check if competition and scale proportionally 0.1 variance /10 = 1, while 1 variance /1 = 1
      tep_sd <- trait_evolutionary_power / 10
    } else {
      tep_sd <- trait_evolutionary_power
    }
    mutation_deltas <- rnorm(length(traits[, ti]), mean = 0, sd = tep_sd)
    traits[, ti] <- traits[, ti] + mutation_deltas
  }
  # set bounds so that the species cant evolve a niche beyond that present in the data
  # note that all temperature values are scaled between 0 and 1
  if (any(traits[, "t_range"] > 1)) {
    traits[which(traits[, "t_range"] > 1), "t_range"] <- 1
  }
  if (any(traits[, "t_range"] <= 0)) {
    traits[which(traits[, "t_range"] <= 0), "t_range"] <- 0.001
  } #limit of zero to avoid zero divisions on specialist/generalist trade-off
  if (any(traits[, "competition"] > 1)) {
    traits[which(traits[, "competition"] > 1), "competition"] <- 1
  }
  if (any(traits[, "competition"] < 0.9)) {
    traits[which(traits[, "competition"] < 0.9), "competition"] <- 0.9
  }
  if (any(traits[, "dispersal"] > 1)) {
    traits[which(traits[, "dispersal"] > 1), "dispersal"] <- 1
  }
  if (any(traits[, "dispersal"] < 0)) {
    traits[which(traits[, "dispersal"] < 0), "dispersal"] <- 0
  }

  return(traits)
}

#-------------------------------------------------#
#### Environmental and Ecological Interactions ####
#-------------------------------------------------#
apply_ecology <- function(
  abundance,
  traits,
  space,
  config,
  abundance_scale = 10,
  abundance_threshold = 8
) {
  return(abundance)
}

#------------------------------#
####      Modify space      ####
#------------------------------#
get_modifiers <- function(space, config, all_species) {
  modifiers <- NULL
  return(modifiers)
}

apply_modifiers <- function(space, config, modifiers) {
  return(space$environment)
}
