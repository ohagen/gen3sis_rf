
#--------------------------------------#
####            METADATA            ####
#--------------------------------------#
# gen3sis configuration
#
# Version: 1.0
#
# Author: Admir Cesar de Oliveira Junior
#
# Date: 14.08.2025
#
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
random_seed = 13

# set the starting time step or leave NA to use the earliest/highest time-step.
start_time = NA

# set the end time step or leave as NA to use the latest/lowest time-step (0).
end_time = NA

# maximum total number of species in the simulation before it is aborted.
max_number_of_species = 25000

# maximum number of species within one site before the simulation is aborted.
max_number_of_coexisting_species = 2500

# a list of traits to include with each species
# a "dispersal" trait is implicitly added in any case
trait_names = c("dispersal","albedo","optimal_temperature","thermal_tolerance")

# ranges to scale the input environments with:
# not listed variable:         no scaling takes place
# listed, set to NA:           the environmental variable will be scaled from [min, max] to [0, 1]
# listed with a given range r: the environmental variable will be scaled from [r1, r2] to [0, 1]
#environmental_ranges = list( )


#-------------------------#
#### Observer Function ####
#-------------------------#

# a place to inspect the internal state of the simulation and collect additional information if desired.
end_of_timestep_observer = function(data, vars, config){
  p<-plot_ranges(data$all_species, data$space)
  plot(p)
  global_mean_temperature <- mean(data$space$environment[,"temperature"])
  modifiers <- sapply(data$space_modifiers,function(mod){mod})

  # write_file
  # lines_to_write <- paste0(c(global_mean_temperature, modifiers), collapse = ",")
  # if(file.exists("/home/yogh/Documentos/projects/testing_gen3sis/daisy_world/DaisyWorld/global_temperature.txt")){
  #   con <- file("/home/yogh/Documentos/projects/testing_gen3sis/daisy_world/DaisyWorld/global_temperature.txt", open = "a")
  #   writeLines(lines_to_write, con)
  #   close(con)
  # }


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
  # write_file
  # file.create("/home/yogh/Documentos/projects/testing_gen3sis/daisy_world/DaisyWorld/global_temperature.txt")
  # con <- file("/home/yogh/Documentos/projects/testing_gen3sis/daisy_world/DaisyWorld/global_temperature.txt", open = "a")
  # writeLines("mean_temperature,mean_albedo,black_prevalence,white_prevalence", con)
  # mean_temp <- mean(space$environment[,"temperature"])
  # writeLines(stringr::str_glue("{mean_temp},0.5,0.09,0.09"), con)
  # close(con)
  
  # getting cells 
  initial_sites <- sample(1:nrow(space$coordinates), 20)
 
  # Black daisy
  # black_daisy <- create_species(initial_cells = initial_sites[1:10], config = config)
  black_daisy <- create_species(initial_cells = initial_sites[1:10], config = config)
  black_daisy$traits[,"dispersal"] <- 1
  black_daisy$traits[,"albedo"] <- 0.2
  black_daisy$traits[,"optimal_temperature"] <- 25
  black_daisy$traits[,"thermal_tolerance"] <- 10

  # White daisy
  # white_daisy <- create_species(initial_cells = initial_sites[11:20], config = config)
  white_daisy <- create_species(initial_cells = initial_sites[11:20], config = config)
  white_daisy$traits[,"dispersal"] <- 1
  white_daisy$traits[,"albedo"] <- 0.8
  white_daisy$traits[,"optimal_temperature"] <- 15
  white_daisy$traits[,"thermal_tolerance"] <- 10

  
  daisies <- list(
    black_daisy,
    white_daisy
  )

  p<-plot_ranges(daisies, space)
  plot(p)
  return(daisies)
}


#-----------------#
#### Dispersal ####
#-----------------#

# the maximum range to consider when calculating the distances from local distance inputs.
max_dispersal <- 99999

# returns n dispersal values.
get_dispersal_values <- function(n, species, space, config) {
  #stop("calculate dispersal values here")
  # n: depende dos site disponíveis
  # species: especie atual
  # values: vetor com a dispersão
  #values <- rnorm(n, mean = 1, sd = 0.2)
  
  values <- rep(3000, n)
  return(values)
}


#------------------#
#### Speciation ####
#------------------#

# threshold for genetic distance after which a speciation event takes place.
divergence_threshold = 1

# factor by which the divergence is increased between geographically isolated population.
# can also be a matrix between the different population clusters.
get_divergence_factor <- function(species, cluster_indices, space, config) {
  return(0)
}


#-----------------------#
#### Trait Evolution ####
#-----------------------#

# mutate the traits of populations of each species and return the new traits matrix
apply_evolution <- function(species, cluster_indices, space, config) {
  return(species[["traits"]])
}

#----------------------------------------------------------#
#### Ecology: Environmental and Ecological Interactions ####
#----------------------------------------------------------#

# called for every site with all occurring species, this function calculates abundances and/or 
# who survives for each sites.
# returns a vector of abundances.
# set the abundance to 0 for every species supposed to die.
apply_ecology <- function(abundance, traits, environment, config) {
  if (length(abundance) > 1){
    spp_survival_change <- sapply(names(abundance), function(sp){
      optimal_temperature <- traits[sp,"optimal_temperature"]
      current_temperature <- environment[,"temperature"]
      temp_distance <- abs(optimal_temperature - current_temperature)
      survival_baseline <- 0.1 # change of survival on the temperature limite
      survival_change <- survival_baseline ^ (temp_distance / traits[sp,"thermal_tolerance"]) 
      survival_change
    })
    black_survival <- spp_survival_change[[1]]
    black_death <- 1-black_survival

    white_survival <- spp_survival_change[[2]]
    white_death <- 1-white_survival

    both_survival <- white_survival*black_survival
    none_survival <- white_death*black_death
    extreme_deathly_death_from_hell <- none_survival+both_survival

    only_black <- black_survival*white_death
    only_white <- black_death*white_survival

    must_survive <- sample(c(1,2,"none"), 1, prob=c(only_black, only_white, extreme_deathly_death_from_hell))

    abundance <- sapply(names(abundance),function(sp){0})
    if(must_survive != "none"){
     abundance[must_survive] <- 1
    }
  } else {
    optimal_temperature <- traits[,"optimal_temperature"]
    current_temperature <- environment[,"temperature"]
    temp_distance <- abs(optimal_temperature - current_temperature)

    survival_baseline <- 0.25 # change of survival on the temperature limite
    survival_change <- survival_baseline ^ (temp_distance / traits[,"thermal_tolerance"]) 

    abundance[1] <- sample(c(1,0), 1,prob = c(survival_change, 1-survival_change))
  }
  # temperatura ótima define sobrevivência das espécies
  # só uma espécie pode sobreviver por site, morte probabilistica baseada na temperatura ótima
  # sample(c(1,2), 1,prob=c(0.5, 0.3))

  return(abundance)
}

#------------------------------#
#### Environmental dynamics ####
#------------------------------#

modify_space <- list(
  get_modifiers = function(space, all_species){
    base_albedo <- 0.5
    
    # densidade de daisies influencia a temperatura a nivel global
    black_daisy <- all_species[[1]]
    black_daisy_albedo <- black_daisy$traits[,"albedo"] |> unique()
    
    white_daisy <- all_species[[2]]
    white_daisy_albedo <- white_daisy$traits[,"albedo"] |> unique()

    black_prevalence <- sum(black_daisy$abundance) / nrow(space$environment)
    white_prevalence <- sum(white_daisy$abundance) / nrow(space$environment)
    empty_prevalence <- 1 - (black_prevalence + white_prevalence)

    mean_albedo <- weighted.mean(
      c(black_daisy_albedo, white_daisy_albedo, base_albedo),
      c(black_prevalence, white_prevalence, empty_prevalence)
    )

    cat("-- Black prevalence: ", black_prevalence, "\n")
    cat("-- White prevalence: ", white_prevalence, "\n")

    return_list <- list(
      mean_albedo = mean_albedo,
      black_prevalence = black_prevalence,
      white_prevalence = white_prevalence
    )
    return(return_list)
  },
  apply_modifiers = function(space, modifiers){
    base_albedo <- 0.5
    modifiers <- modifiers[["mean_albedo"]]

    mean_irradiation <- mean(space$environment[,"irradiation"])
    trapped_energy <- mean_irradiation * (1-modifiers)
    regular_energy <- mean_irradiation * base_albedo

    energy_delta <- trapped_energy - regular_energy

    cat("-- Delta: ",energy_delta,"\n")
    cat("-- Mean albedo: ",modifiers,"\n")

    cat("-- Mean temperature before: ",mean(space$environment[,"temperature"]),"\n")
    space$environment[,"temperature"] <- space$environment[,"temperature"] + (energy_delta * 1)
    cat("-- Mean temperature after: ",mean(space$environment[,"temperature"]),"\n")
    cat("-------------\n")
    return(space$environment)
  }
)
