random_seed <- 28015

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

divergence_threshold <- 2

get_divergence_factor <- function(species, cluster_indices, space, config) {
  0
}

#-------------------------------------#
#### Within-cluster speciation     ####
#-------------------------------------#
# the (maximum) rate of within-cluster divergence per time step
within_cluster_divergence_rate <- 1
# population traits mimic a Gaussian distribution to determine overlap in niche
# the higher the overlap, the higher the degree of gene flow in this config
sigma_trait_gene_flow <- 0.25

# new ecological implementation: divergence increases based on ecological distance
get_within_cluster_divergence_factor <- function(
    species,
    cells,
    divergence,
    space,
    config
) {
  # temperature optima trait per-population within a species
  temp <- species$traits[cells, "temp"]
  
  # distance between in trait value between the pops
  trait_distance <- abs(outer(temp, temp, "-"))
  # calculate overlap between the traits assuming a Gaussian 
  gene_flow <- exp(
    -trait_distance^2 /
      (2 * config$user$sigma_trait_gene_flow^2)
  )
  # Provide an update to the divergence with the pre-specified rate and calculated gene flow
  divergence_update <-
    config$user$within_cluster_divergence_rate *
    (1 - gene_flow)
  
  return(divergence_update)
}

#-----------------------#
#### Trait evolution ####
#-----------------------#

apply_trait_evolution <- function(species, cluster_indices, space, config) {
  traits <- species[["traits"]]
  cells <- rownames(traits)
  # selection (towards the environmental optima)
  selection_rate <- 0.05
  traits[, "temp"] <- traits[, "temp"] + selection_rate * (space$environment[cells, "temp"] - traits[, "temp"])
  # drift (negligible here but may be useful for extensions)
  drift_strength <- 0.001
  mutation_deltas <- rnorm(length(traits[, "temp"]), mean=0, sd=drift_strength)
  traits[, "temp"] <- traits[, "temp"] + mutation_deltas
  
  return(traits)
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