# Copyright (c) 2020, ETH Zurich

# DO NOT USE ' IN THIS CONFIG

# empty skeleton config
# internal helper: returns a single compiled string used by write_config_skeleton()
#' create a skeleton config string from scratch
#' @details create a skeleton config string from scratch
#'
#' @return a string containing the skeleton config
#' @noRd
skeleton_config <- function(){
 paste0(c("
#--------------------------------------#
####            METADATA            ####
#--------------------------------------#
# gen3sis configuration
#
# Version: 1.0
#
# Author:
# ", paste0("# Date: ", format(Sys.Date(), format="%d.%m.%Y")),                          
'#
# space:
#
# Publications:
#
# Description: 
#
#--------------------------------------#


#------------------------#
#### General settings ####
#------------------------#

# set the random seed for the simulation.
random_seed = NA

# Set config duration
# Currently available units are:
# "yr": year (1 year)
# "kyr": kilo year (1,000 years)
# "Myr": mega year (1,000,000 years)
# "Gyr": giga year (1,000,000,000 years)
# "timestep": bypass the entire time-conversion and assumes the config in the same unit as the space
duration <- list(
  from = NA, # set the starting time (in the same unit as duration$unit) or leave NA to use the earliest/highest time-step.
  to = NA, # set the end time (in the same unit as duration$unit) or leave as NA to use the latest/lowest time-step (0).
  by = NA, # set the amount of time each config time-step comprises (in duration$unit).
  unit = "timestep" # set the unit in which time is measured in config.
)

# maximum total number of species in the simulation before it is aborted.
max_number_of_species = 25000

# maximum number of species within one site before the simulation is aborted.
max_number_of_coexisting_species = 2500

# a list of traits to include with each species
# a "dispersal" trait is implicitly added in any case
trait_names = c("dispersal")

# names of population-level ecological states returned by apply_ecology.
# use NA if no ecological states are required.
ecological_state_names <- NA

# ranges to scale the input environments with:
# not listed variable:         no scaling takes place
# listed, set to NA:           the environmental variable will be scaled from [min, max] to [0, 1]
# listed with a given range r: the environmental variable will be scaled from [r1, r2] to [0, 1]
environmental_ranges = list( )


#-------------------------#
#### Observer Function ####
#-------------------------#

# a place to inspect the internal state of the simulation and collect additional information if desired.
end_of_timestep_observer = function(data, vars, config){
  # the list of all species can be found in data$all_species
  # the current space can be found in data$space
  
  # saving functions example:
    # save_space()
    # save_species()
  
  # plotting functions example:
    # plot environmental conditions
    # plot_space(data$space)
    # plot richness
    # plot_richness(data$all_species, data$space)
    # plot a specific environmental condition
    # plot_raster_single(data$space$environment[,"temp"], data$space, "temp", NA)
    # plot species 1 range
    # plot_species_presence(data$all_species[[1]], data$space)
    # plot(0,type="n",axes=FALSE,ann=FALSE)

}


#----------------------#
#### Initialization ####
#----------------------#

# the initial abundance of a newly colonized site, both during setup and later when 
# colonizing a site during the dispersal.
initial_abundance = 1

# place species in the space:
create_ancestor_species <- function(space, config) {
 stop("create the initial species here")
}

# initial values for the user-specified ecological state(s). Relevant during setup
# and later during dispersal.
initial_ecological_state <- NA

#-----------------#
#### Dispersal ####
#-----------------#

# the maximum range to consider when calculating the distances from local distance inputs.
max_dispersal <- Inf

# returns n dispersal values.
get_dispersal_values <- function(n, species, space, config) {
  stop("calculate dispersal values here")
}


#------------------#
#### Speciation ####
#------------------#

# threshold for genetic distance after which a speciation event takes place.
divergence_threshold = NULL

# factor by which the divergence is increased between geographically isolated population.
# can also be a matrix between the different population clusters.
get_divergence_factor <- function(species, cluster_indices, space, config) {
  stop("calculate divergence factor here")
}

# factor by which the divergence within geographically isolated clusters is either increased or decreased.
# can return a scalar (e.g. -1 as the default) or a matrix.
get_within_cluster_divergence_factor <- function(species, species_presence, cluster_indices, divergence, space, config){
  stop("calculate within-cluster divergence here")
}

#-----------------------#
#### Trait Evolution ####
#-----------------------#

# mutate the traits of populations of each species and return the new traits matrix
apply_trait_evolution <- function(species, cluster_indices, space, config) {
  stop("mutate species traits here")
}


#------------------------------------------------#
#### Ecology: Biotic and Abiotic Interactions ####
#------------------------------------------------#

# called for every site with all occurring species, this function calculates abundances 
# and/or who survives for each sites.
# returns a vector of abundances.
# set the abundance to 0 for every species supposed to die.
apply_ecology <- function(abundance, traits, ecological_states, local_environment, config) {
  stop("calculate species abundances and ecological states here")
}


#--------------------------------------------#
##  Biotic Modification of the Environment  ##
#--------------------------------------------#

# Accounts for Biospheric Feedbacks to the space
# 1. "get_modifiers" 
## Is called at the end of each time-step.
## Users can set any rules to get environmental modifiers.
## Can return any object.
## To deactivate environmental modifiers, the function must simply return NULL.
# 2. "apply_modifiers"
## Is called at the start of each time-step
## Its used to apply the modifiers computed in the previous time-step.
## Will receive the space and the modifiers object.
## Must return the "space$environment" object.
## Only runs if modifiers are not NULL.

get_modifiers <- function(space, config, all_species){
  modifiers <- NULL
  return(modifiers)
}

apply_modifiers <- function(space, config, modifiers){
  return(space$environment)
}

'
  )) # DO NOT REMOVE THIS ->'<-. IT IS IMPORTANT
}
