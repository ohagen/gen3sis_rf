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
    species_presence,
    cluster_indices,
    divergence,
    space,
    config
) {
  0
}

daughter_fraction <- 0.5
point_speciation_rate <- 5e-4

apply_within_site_speciation <- function(
    species,
    space,
    config
) {
  # select the populations that are undergoing point-speciation
  event_cells <- names(species[["divergence"]][["index"]])
  event_cells <- event_cells[species[["ecological_states"]][
    event_cells,"frequency_dependence"] <= config$user$point_speciation_rate
    ]
  
  # return a list of speciation events
  events <- vector("list", length(event_cells))
  
  for (i in seq_along(event_cells)) {
    site <- event_cells[i]
    
    daughter_abundance <-
      species[["abundance"]][site] *
      config$user$daughter_fraction
    
    species[["abundance"]][site] <-
      species[["abundance"]][site] *
      (1 - config$user$daughter_fraction)
    
    events[[i]] <- list(
      site = site,
      daughter_abundance = daughter_abundance,
      daughter_traits = species[["traits"]][site, ]
    )
  }
  
  return(list(species = species, events = events))
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