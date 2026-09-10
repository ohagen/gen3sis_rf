#------------------------------------------------------------
# Identity test: within-cluster speciation ON but no-op
#------------------------------------------------------------

### config based on Sympatric_4.R

random_seed <- 28015
# step_time <- list(x = 1, unit = "timestep")
# start_time <- NA
# end_time <- 50

duration <- list(
  from = NA,
  to = 55,
  by = -1,
  unit = "Myr"
)

max_number_of_species <- 100000
max_number_of_coexisting_species <- 100000

trait_names = c("temp",  "dispersal")

environmental_ranges <- list()

initial_abundance <- 1
ecological_state_names <- "frequency_dependence"
initial_ecological_state <- c("frequency_dependence" = 1)

#-------------------------#
#### Observer function ####
#-------------------------#

end_of_timestep_observer <- function(data, vars, config) {
  # save_species()
  # save_abundance()
  # save_divergence()
  # save_occupancy()
  save_phylogeny()
  save_traits()
  plot_richness(data$all_species, data$space)
}

#----------------------#
#### Initialization ####
#----------------------#

create_ancestor_species <- function(space, config) {
  
  range <- c(-95, -24, -68, 13)
  co <- space$coordinates
  selection <- co[, "x"] >= range[1] &
    co[, "x"] <= range[2] &
    co[, "y"] >= range[3] &
    co[, "y"] <= range[4]
  
  new_species <- list()
  for(i in 1){
    initial_cells <- rownames(co)[selection]
    initial_cells <- sample(initial_cells, 1)
    new_species[[i]] <- create_species(initial_cells, config)
    #set local adaptation to max optimal temp equals local temp
    new_species[[i]]$traits[ , "temp"] <- space$environment[initial_cells,"temp"]
    new_species[[i]]$traits[ , "dispersal"] <- 1
    new_species[[i]]$ecological_states[, ecological_state_names] <- 
      initial_ecological_state[ecological_state_names]
  }
  
  return(new_species)
}

#-----------------#
#### Dispersal ####
#-----------------#

max_dispersal <- Inf

get_dispersal_values <- function(n, species, space, config) {
  # values <- rweibull(n, shape = 1.5, scale = 133)
  values <- rweibull(n, shape = 1.5, scale = 500)
  return(values)}

#------------------#
#### Speciation ####
#------------------#

divergence_threshold <- 1

get_divergence_factor <- function(species, cluster_indices, space, config) {
  0
}

get_within_cluster_divergence_factor <- function(
    species,
    cells,
    divergence,
    space,
    config
) {
  0
}

daughter_fraction <- 0.5
point_speciation_rate <- 5e-4

get_within_site_divergence_factor <- function(
    species, cell, divergence, space, config
    ){
  divergence_update <- ifelse(runif(n = 1, min = 0, max = 1) <= point_speciation_rate, 
         config$gen3sis$speciation$divergence_threshold + 0.01, 
         0
         )
  return(divergence_update)
}

# does this even need "config"?
# simple proportional split & traits are inherited faithfully
apply_within_site_speciation <- function(
    abundance, traits, config
) {
  
  return(
    list(
      parent = c(abundance = abundance * (1 - config$user$daughter_fraction), traits),
      daughter = c(abundance = abundance * config$user$daughter_fraction, traits)
    )
  )

}

#-----------------------#
#### Trait evolution ####
#-----------------------#

apply_trait_evolution <- function(species, cluster_indices, space, config) {
  return(species$traits)
}

#-------------------------------------------------#
#### Environmental and ecological interactions ####
#-------------------------------------------------#

# this doesn't do anything in this config, but it could potentially feedback into 
# the within-site speciation divergence update
apply_ecology <- function(
    abundance,
    traits,
    ecological_states,
    local_environment,
    config
) {

  ecological_states["frequency_dependence", ] <-
    runif(n = length(abundance), min = 0, max = 1)
  
  return(
    rbind(
      abundance = abundance,
      ecological_states
    )
  )
}
#----------------------#
#### Space modifier ####
#----------------------#

get_modifiers <- function(space, config, all_species) {
  NULL
}

apply_modifiers <- function(space, config, modifiers) {
  return(space$environment)
}