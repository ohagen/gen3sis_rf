# Copyright (c) 2020, ETH Zurich

#---------------------------------------#
######## START OF INITIALIZATION ########
#---------------------------------------#

#' Initializes the space inputs
#'
#' @param config the current config
#' @param data the current data
#' @param vars the current vars
#'
#' @return the general vals(config, data, vars) list
#' @noRd
setup_inputs <- function(config, data, vars) {
  data[["inputs"]] <- list()
  spaces_rds <- readRDS(file.path(config$directories$input, "spaces.rds"))
  spaces <- spaces_rds$env # TODO correct cascade of names... attention to s i.e. space is contained in spaces
  space_names <- names(spaces)

  # environmental matrices
  environments <- list()
  for (name in space_names) {
    tmp <- as.matrix(spaces[[name]][, -c(1:2)])
    # scaling
    if (
      name %in%
        names(config[["gen3sis"]][["general"]][["environmental_ranges"]])
    ) {
      range <- config[["gen3sis"]][["general"]][["environmental_ranges"]][[
        name
      ]]
      if (any(is.na(range))) {
        r_min <- min(tmp, na.rm = TRUE)
        r_max <- max(tmp, na.rm = TRUE)
        range <- c(r_min, r_max)
      } else {
        range <- range
      }
      tmp <- (tmp - range[1]) / (range[2] - range[1])
    } else {
      # not in list, do not scale
    }
    environments[[name]] <- as.matrix(tmp)
  }
  # environments
  data[["inputs"]][["environments"]] <- environments
  # time-steps
  data[["inputs"]][["timesteps"]] <- colnames(environments[[1]])
  # coordinates
  data[["inputs"]][["coordinates"]] <- as.matrix(spaces[[1]][, 1:2])
  # type
  data[["inputs"]][["type"]] <- spaces_rds$meta$type
  # type_spec_res
  data[["inputs"]][["type_spec_res"]] <- spaces_rds$meta$type_spec$res
  # extent
  data[["inputs"]][["extent"]] <- spaces_rds$meta$area$extent
  # geodynamc
  data[["inputs"]][["geodynamic"]] <- spaces_rds$meta$geodynamic
  # duration
  data[["inputs"]][["duration"]] <- spaces_rds$meta$duration

  return(list(config = config, data = data, vars = vars))
}

#' Initializes the time-step-variables and flags
#'
#' @param config the current config
#' @param data the current data
#' @param vars the current vars
#'
#' @return the general vals(config, data, vars) list
#' @noRd
setup_variables <- function(config, data, vars) {
  # time-steps
  # As time-steps are 0-based, for any spaces with n time-steps, internally they will be indexed
  # in a sequence {n-1, n-2, n-3, ..., 0}, with n elements. For example, if the spaces has 5 time-steps,
  # they will be indexed in a sequence {4, 3, 2, 1, 0}. That way the latest time-step will be always 0,
  # and the earliest time-step will always n-1, what ensures compatibility across all possible situations.
  # TL;DR: as time-steps are 0-based, i.e., latest must be 0 and earliest must be the length-1

  # --- TODO --- 
  # all error conditions must be caught in the config check. Invalid values that pass the check must abort the simulation. 
  # Values that are min/max clamped should be printed, possibly as warnings (e.g. start time outside spaces time window).
  # Should we change these conditionals below?

  # TODO
  # Não atualizar as informações da config
  # Armazenar o start_timestep e o end_timestep inside other place 
  
  # Replace NA's and assume spaces' time information
  if (is.na(config$gen3sis$general$duration$from)) {
    config$gen3sis$general$duration$from <- data$inputs$duration$from
  } 

  if (is.na(config$gen3sis$general$duration$to)) {
    config$gen3sis$general$duration$to <- data$inputs$duration$to
  }

  if (is.na(config$gen3sis$general$duration$by)) {
    config$gen3sis$general$duration$by <- data$inputs$duration$by
  }

  if (is.na(config$gen3sis$general$duration$unit)) {
    config$gen3sis$general$duration$unit <- data$inputs$duration$unit
  }

  # Check if the timeframe match and updates the config duration to harmonize with spaces' config
  converted_config_duration <- check_time_match(config$gen3sis$general$duration, data$inputs$duration)

  # extract the implicit timesteps defined in the config
  implicit_config_ts <- paste0(
    seq(
      converted_config_duration$from,
      converted_config_duration$to,
      converted_config_duration$by
    ),
    converted_config_duration$unit
  )

  if(!all(implicit_config_ts %in% data$inputs$timesteps)){
    stop("Config's duration implies in non-existent time-steps.")
  }

  included_ts <- data$inputs$timesteps[which(data$inputs$timesteps %in% implicit_config_ts)] # which timesteps from the config are present in spaces 

  if(length(included_ts) == 0){
    stop("Impossible to subset spaces' time-steps. Check your config's duration.")
  }

  zero_base_ts <- (length(data$inputs$timesteps) - 1):0 # actual timesteps ordered zero-base for the entire spaces
  zero_base_ts_included <- zero_base_ts[match(included_ts, data$inputs$timesteps)] # filtered given the config's duration
 
  # Update config's duration otherwise things break
  vars$start_timestep <- zero_base_ts_included[[1]]
  vars$end_timestep <- zero_base_ts_included[[length(zero_base_ts_included)]]

  text_message <- paste0(
    "Simulation will start at time-step ",
    vars$start_timestep, " (", # tell the timestep index
    data$inputs$timesteps[match(vars$start_timestep, zero_base_ts)], ")", # tell the timestep name with the unit
    " and end at time-step ",
    vars$end_timestep, " (", # tell the timestep index
    data$inputs$timesteps[match(vars$end_timestep, zero_base_ts)], ")\n" # tell the timestep name with the unit
  )
  
  message(text_message)

  # put in start time for create_space
  vars$ti <- vars$start_timestep

  # flag
  vars$flag <- "OK"
  vars$steps <- zero_base_ts_included

  return(list(config = config, data = data, vars = vars))
}


#' Calls the creation for the initial species and prepares further data storage
#'
#' @param config the current config
#' @param data the current data
#' @param vars the current vars
#'
#' @return the general vals(config, data, vars) list
#'
#' @importFrom grDevices pdf dev.off
#' @importFrom graphics par
#' @noRd
init_attribute_ancestor_distribution <- function(config, data, vars) {
  #oldpar <- par(no.readonly = TRUE)
  #on.exit(par(oldpar))
  all_species <- config$gen3sis$initialization$create_ancestor_species(
    data$space,
    config
  )
  for (i in 1:length(all_species)) {
    force(i)
    all_species[[i]][["id"]] <- as.character(i)
  }

  data$all_species <- all_species

  # #plot starting_richness
  # grDevices::pdf(file=file.path(config$directories$output, "starting_richness.pdf"), width=10, height=6)
  # par(mfrow=c(1,1))
  # plot_richness(all_species, data$space)
  # grDevices::dev.off()
  #
  # #plot starting_ranges
  # grDevices::pdf(file=file.path(config$directories$output, "starting_ranges.pdf"), width=10, height=6)
  # par(mfrow=c(1,1))
  # plot_ranges(all_species, data$space)
  # grDevices::dev.off()

  # par(mfrow=c(1,2))
  # plot_richness(all_species, data$space)
  # plot_ranges(all_species, data$space)

  # n_sp <- ncol(geo_sp_ti)
  n_sp <- length(data$all_species)
  vars$n_sp <- n_sp
  vars$n_sp_alive <- n_sp

  data$phy <- data.frame(
    "Ancestor" = rep(1, n_sp), # c(1:n_sp),
    "Descendent" = c(1:n_sp),
    "Speciation.Time" = vars$start_timestep,
    "Extinction.Time" = rep(vars$start_timestep, n_sp),
    "Speciation.Type" = c("ROOT", rep("GENETIC", n_sp - 1)) # "ROOT"
  )
  
  return(list(config = config, data = data, vars = vars))
}


#' Prepares summary statistics to be accumulated in the simulation (e.g. geo_richness object)
#'
#' @param config the current config
#' @param data the current data
#' @param vars the current vars
#'
#' @return the general vals(config, data, vars) list
#' @noRd
init_simulation <- function(config, data, vars) {
  # internal variables
  steps <- vars$steps

  # create matrix for turnover
  data$turnover <- matrix(
    NA,
    ncol = 3,
    nrow = length(steps),
    dimnames = list(steps, c("n_new_sp_ti", "n_ext_sp_ti", "n_sp_alive"))
  )

  #..... add the first turnover at t0 (t_start)....
  data$turnover[1, ] <- c(vars$n_sp, 0, vars$n_sp)

  # create matrix for geographic total species richness geo_richness
  geo_richness <- matrix(
    NA,
    nrow = nrow(data[["inputs"]][["coordinates"]]),
    ncol = length(steps) + 2
  )
  geo_richness[, 1:2] <- data[["inputs"]][["coordinates"]]
  colnames(geo_richness) <- c(
    colnames(data[["inputs"]][["coordinates"]]),
    steps
  )
  rownames(geo_richness) <- rownames(data[["inputs"]][["coordinates"]])
  #..... add the first species distribution at t0 (t_start)....
  geo_richness[
    rownames(data[["space"]][["coordinates"]]),
    #as.character(config$gen3sis$general$duration$from)
    as.character(vars$start_timestep)
  ] <- get_geo_richness(data$all_species, data[["space"]])

  data$geo_richness <- geo_richness

  return(list(config = config, data = data, vars = vars))
}

#---------------------------------------#
######## END OF INITIALIZATION ########
#---------------------------------------#

#----------------------------------#
######## START OF LOOP BODY ########
#----------------------------------#

#-------------------------#
######## -> LOOP SETUP #######
#-------------------------#

#' Prepares the space for the current time step
#'
#' @param config the current config
#' @param data the current data
#' @param vars the current vars
#'
#' @return the general vals(config, data, vars) list
#' @noRd
setup_space <- function(config, data, vars) {
  n_total_steps <- length(data[["inputs"]][["timesteps"]])
  total_steps_zerobase <- (n_total_steps - 1):0
  index <- which(total_steps_zerobase == vars$ti) #gen3sis v1 : vars$ti + 1
  habitable_cells <- data$inputs$environments[[1]][, index, drop = FALSE]
  habitable_cells <- habitable_cells[
    which(!is.na(habitable_cells)),
    ,
    drop = FALSE
  ]
  habitable_cells <- rownames(habitable_cells)
  envir <- do.call(
    cbind,
    lapply(data$inputs$environments, "[", habitable_cells, index, drop = FALSE)
  )
  colnames(envir) <- names(data[["inputs"]][["environments"]])
  space <- create_space(
    id = vars$ti,
    environment = envir,
    coordinates = data[["inputs"]][["coordinates"]][habitable_cells, ],
    timestep = data[["inputs"]][["timesteps"]][index],
    extent = data[["inputs"]][["extent"]],
    duration = data[["inputs"]][["duration"]],
    geodynamic = data[["inputs"]][["geodynamic"]],
    type = data[["inputs"]][["type"]],
    type_spec_res = data[["inputs"]][["type_spec_res"]]
  )

  data[["space"]] <- space

  return(list(config = config, data = data, vars = vars))
}


#' Iterates over all species and calls
#' limit_species_to_cells to restrict them to the currently habitable cells
#'
#' @param config the current config
#' @param data the current data
#' @param vars the current vars
#'
#' @return the general vals(config, data, vars) list
#' @noRd
restrict_species <- function(config, data, vars) {
  points_coordinates <- data[["space"]][["coordinates"]]
  if (is.vector(points_coordinates)) {
    points_coordinates <- matrix(points_coordinates, nrow = 1)
    colnames(points_coordinates) <- names(data[["space"]][["coordinates"]])
    rownames(points_coordinates) <- "1"
  }
  data$all_species <- lapply(
    data$all_species,
    limit_species_to_cells,
    rownames(points_coordinates)
  )
  return(list(config = config, data = data, vars = vars))
}


#' Loads and prepares the distance matrix for the current time step
#'
#' @param config the current config
#' @param data the current data
#' @param vars the current vars
#'
#' @return the general vals(config, data, vars) list
#' @noRd
loop_setup_geo_dist_m_ti <- function(config, data, vars) {
  # loading geo_dist_m_ti that is the internal distance matrices
  #load(paste0(config$directories$input,"/geo_dist_m/geo_dist_m_ti/geo_dist_m_ti_t_", vars$ti, ".RData"))
  geo_dist_m_ti <- readRDS(
    file = file.path(
      config$directories$input,
      "distance_matrices",
      paste0("geo_dist_m_ti_t_", vars$ti, ".rds")
    )
  )

  cell_names <- rownames(data$space[["coordinates"]])
  rownames(geo_dist_m_ti) <- cell_names
  colnames(geo_dist_m_ti) <- cell_names

  data$geo_dist_m_ti <- geo_dist_m_ti
  return(list(config = config, data = data, vars = vars))
}


#' Load or calculates the distance matrix used for clustering and dispersal
#'
#' @details If a full matrix is found in input/distance_matrices it will be used. Otherwise the local distances
#' are loaded from the distance_neighbours directory are loaded and a distance_matrix is internally calculated
#'
#' @param config the current config
#' @param data the current data
#' @param vars the current vars
#'
#' @return the general vals(config, data, vars) list
#' @noRd
setup_distance_matrix <- function(config, data, vars) {
  if (data$space$geodynamic) {
    tiis <- vars$ti
  } else {
    # in case of static spaces, the distance matrix is always the same
    tiis <- 0
  }
  matrix_file <- file.path(
    config$directories$input,
    "distances_full",
    paste0("distances_full_", tiis, ".rds")
  )
  if (base::file.exists(matrix_file)) {
    distance_matrix <- readRDS(file = matrix_file)
  } else {
    neighbour_file <- file.path(
      config$directories$input,
      "distances_local",
      paste0("distances_local_", tiis, ".rds")
    )
    distance_neighbours <- readRDS(neighbour_file)

    habitable_cells <- as.integer(rownames(data$space$coordinates))
    num_cells <- nrow(distance_neighbours)
    distance_matrix <- get_distance_matrix(
      habitable_cells,
      num_cells,
      distance_neighbours@p,
      distance_neighbours@i,
      distance_neighbours@x,
      config$gen3sis$dispersal$max_dispersal
    )
  }

  data$distance_matrix <- distance_matrix

  return(list(config = config, data = data, vars = vars))
}

#-------------------------------------------#
######## -> UPDATE Extinction Times  #######
#-------------------------------------------#
#' Updates the extinction times
#'
#' @param config the current config
#' @param data the current data
#' @param vars the current vars
#'
#' @return the general vals(config, data, vars) list
#' @noRd
update_extinction_times <- function(config, data, vars) {
  for (sp in data$all_species) {
    if (length(sp[["abundance"]])) {
      data$phy$"Extinction.Time"[as.integer(sp[["id"]])] <- vars$ti - 1
    }
  }
  return(list(config = config, data = data, vars = vars))
  # update turnover
  #data$turnover[toString(vars$ti),] <- c(vars$n_new_sp_ti, sum(data$phy$"Extinction.Time"==(vars$ti+1)), vars$n_sp_alive)

  # update geo_richness
  #data$geo_richness[rownames(data[["space"]][["coordinates"]]), as.character(vars$ti)] <- get_geo_richness(data$all_species, data[["space"]])
  # data$geo_richness <- update.geo.richness(geo_sp_ti=data$geo_sp_ti, ti=vars$ti, geo_richness = data$geo_richness )
  #data[["eco_by_sp"]] <- get_eco_by_sp(data$all_species)
}

#--------------------------------#
######## END OF LOOP BODY ########
#--------------------------------#

#' Updates the phylogeny with the survival and extinctions from the current time step
#'
#' @param config the current config
#' @param data the current data
#' @param vars the current vars
#'
#' @return the general vals(config, data, vars) list
#' @noRd
update.phylo <- function(config, data, vars) {
  # update  phylo with the survive info
  # add MRCA to the levels of Speciation.Type
  levels(data$phy$Speciation.Type) <- c(levels(data$phy$Speciation.Type))
  # bind extinction information
  #data$phy <- cbind(data$phy[,c(1,2,3)], Extinction.Time=vars$ext, Speciation.Type=data$phy[,4])
  data$phy$Ancestor <- as.integer(data$phy$Ancestor)
  data$phy$Descendent <- as.integer(data$phy$Descendent)
  data$phy$Speciation.Time <- as.integer(data$phy$Speciation.Time)
  # end update phy
  return(list(config = config, data = data, vars = vars))
}
