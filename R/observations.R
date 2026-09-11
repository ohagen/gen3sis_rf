# Copyright (c) 2020, ETH Zurich

#' Prepare the internal data structures to be called with the observe_xx functions and call the user provide observer
#' Unless a problem shows up the save_xx functions call getDyn() to get access to the internal state, no prep here.
#'
#' @param data the current data object
#' @param vars the current vars object
#' @param config the current config
#'
#' @noRd
call_main_observer <- function(data, vars, config) {
  end_of_timestep_seed <- .GlobalEnv$.Random.seed
  config$gen3sis$general$end_of_timestep_observer(data, vars, config)
  .GlobalEnv$.Random.seed <- end_of_timestep_seed
}


# save_ functions ----

#' This function can be called within the observer function to save the current occupancy pattern
#' @return no return value, called for side effects
#' @keywords support
#' @example inst/examples/save_occupancy_help.R
#' @export
save_occupancy <- function() {
  config <- dynGet("config")
  data <- dynGet("data")
  vars <- dynGet("vars")
  save_space()
  dir.create(
    file.path(config$directories$output, "occupancy"),
    showWarnings = FALSE,
    recursive = TRUE
  )
  tmp <- get_geo_richness(data$all_species, data$space)
  tmp <- tmp > 0
  saveRDS(
    object = tmp,
    file = file.path(
      config$directories$output,
      "occupancy",
      paste0("occupancy_t_", vars$ti, ".rds")
    )
  )
}


#' This function can be called within the observer function to save the current richness pattern
#' @return no return value, called for side effects
#' @keywords support
#' @seealso \code{\link{save_species}}
#' @example inst/examples/save_richness_help.R
#' @export
save_richness <- function() {
  config <- dynGet("config")
  data <- dynGet("data")
  vars <- dynGet("vars")
  save_space()
  dir.create(
    file.path(config$directories$output, "richness"),
    showWarnings = FALSE,
    recursive = TRUE
  )
  richness <- get_geo_richness(data$all_species, data$space)
  saveRDS(
    object = richness,
    file = file.path(
      config$directories$output,
      "richness",
      paste0("richness_t_", vars$ti, ".rds")
    )
  )
}


#' This function can be called within the observer function to save the current phylogeny.
#' @return no return value, called for side effects
#' @keywords support
#' @example inst/examples/save_phylogeny_help.R
#' @export
save_phylogeny <- function() {
  config <- dynGet("config")
  data <- dynGet("data")
  vars <- dynGet("vars")

  directory <- file.path(config$directories$output, "phylogeny")
  dir.create(directory, showWarnings = FALSE, recursive = TRUE)

  file <- file.path(directory, paste0("phylogeny_t_", vars$ti, ".nex"))
  write_nex(phy = data$phy, label = "species", output_file = file)
}


#' This function can be called within the observer function to save the full species list.
#' @return no return value, called for side effects
#' @keywords support
#' @seealso \code{\link{save_space}}
#' @example inst/examples/save_species_help.R
#' @export
save_species <- function() {
  config <- dynGet("config")
  data <- dynGet("data")
  vars <- dynGet("vars")
  save_space()
  dir.create(
    file.path(config$directories$output, "species"),
    showWarnings = FALSE,
    recursive = TRUE
  )
  species <- data$all_species
  saveRDS(
    object = species,
    file = file.path(
      config$directories$output,
      "species",
      paste0("species_t_", vars$ti, ".rds")
    )
  )
}


#' This function can be called within the observer function to save
#' the current space, can be called independently by the user and is called by
#' other observer functions relying on the space to be present (e.g. save_species)
#' @return no return value, called for side effects
#' @keywords support
#' @seealso \code{\link{save_species}}
#' @example inst/examples/save_space_help.R
#' @export
save_space <- function() {
  config <- dynGet("config")
  data <- dynGet("data")
  vars <- dynGet("vars")
  space_file <- file.path(
    config$directories$output,
    "spaces",
    paste0("space_t_", vars$ti, ".rds")
  )
  if (!base::file.exists(space_file)) {
    dir.create(
      file.path(config$directories$output, "spaces"),
      showWarnings = FALSE,
      recursive = TRUE
    )
    space <- data$space
    saveRDS(object = space, file = space_file)
  }
}


#' This function can be called within the observer function to save the species abundances.
#' @return no return value, called for side effects
#' @keywords support
#' @seealso \code{\link{save_species}}
#' @example inst/examples/save_abundance_help.R
#' @export
save_abundance <- function() {
  save_extract("abundance")
}


#' This function can be called within the observer function to save the species traits.
#' @return no return value, called for side effects
#' @keywords support
#' @seealso \code{\link{save_species}}
#' @example inst/examples/save_traits_help.R
#' @export
save_traits <- function() {
  save_extract("traits")
}


#' This function can be called within the observer function to save the compressed species divergence.
#' @return no return value, called for side effects
#' @keywords support
#' @seealso \code{\link{save_species}}
#' @example inst/examples/save_divergence_help.R
#' @export
save_divergence <- function() {
  save_extract("divergence")
}


#' Save a named element from all species.
#' @param element Name of element to save, e.g. "abundance" or "traits"
#' @noRd
save_extract <- function(element) {
  config <- dynGet("config")
  data <- dynGet("data")
  vars <- dynGet("vars")
  save_space()
  dir.create(
    file.path(config$directories$output, element),
    showWarnings = FALSE,
    recursive = TRUE
  )
  tmp <- lapply(data$all_species, function(x) {
    return(x[[element]])
  })
  names(tmp) <- sapply(data$all_species, function(x) {
    x$id
  })
  saveRDS(
    object = tmp,
    file = file.path(
      config$directories$output,
      element,
      paste0(element, "_t_", vars$ti, ".rds")
    )
  )
}


# get_ functions ----

#' construct_community_matrices
#'
#' @param species_list a list of species to include in the calculations.
#' @param space the space to calculate over.
#' @param xy if TRUE, site coordinates are returned as matrix columns. Default is FALSE.
#' @param empty_sites if TRUE, sites with no species will be included in the matrix. Default is FALSE.
#' @param mode "abundane" or "presence"
#'
#' @returns For internal use only. Returns a community matrix with abundance or presence.
#' @noRd
construct_community_matrices <- function(
  species_list,
  space,
  xy,
  empty_sites,
  mode
) {
  species_id <- sapply(species_list, function(sp) {
    sp$id
  })

  sites_occupied <- sapply(species_list, function(sp) {
    names(sp$abundance)
  }) |>
    unlist() |>
    c() |>
    unique()

  community_matrix <- matrix(
    nrow = length(sites_occupied),
    ncol = length(species_list)
  )

  row.names(community_matrix) <- sites_occupied
  colnames(community_matrix) <- species_id

  for (sp in species_list) {
    community_matrix[names(sp$abundance), sp$id] <- sp$abundance
  }

  if (empty_sites) {
    empty_sites <- setdiff(row.names(space$coordinates), sites_occupied)
    empty_mtx <- matrix(nrow = length(empty_sites), ncol = length(species_list))
    row.names(empty_mtx) <- empty_sites
    colnames(empty_mtx) <- species_id

    community_matrix <- rbind(community_matrix, empty_mtx)
  }

  community_matrix[is.na(community_matrix)] <- 0

  if (mode == "presence") {
    community_matrix[community_matrix >= 1] <- 1
    community_matrix[community_matrix < 1] <- 0
  }

  if (xy) {
    coords_mtx <- space$coordinates[row.names(community_matrix), ]
    community_matrix <- cbind(coords_mtx, community_matrix)
  }

  return(community_matrix)
}

#' Get abundance matrix
#'
#' This function constructs an abundance matrix based on the space sites.
#'
#' @param species_list a list of species to include in the calculations.
#' @param space the space to calculate over.
#' @param xy if TRUE, site coordinates are returned as matrix columns. Default is FALSE.
#' @param empty_sites if TRUE, sites with no species will be included in the matrix. Default is FALSE.
#'
#' @returns an abundance matrix with sites as rows and species as columns.
#' @keywords support
#' @export
#' @seealso \code{vignette("h-support-functions", package = "gen3sis2")}
#' @example inst/examples/support_functions/get_abundance_matrix_help.R
get_abundance_matrix <- function(
  species_list,
  space = NULL,
  xy = FALSE,
  empty_sites = FALSE
) {
  abundance_matrix <- construct_community_matrices(
    species_list = species_list,
    space = space,
    xy = xy,
    empty_sites = empty_sites,
    mode = "abundance"
  )
  return(abundance_matrix)
}

#' Get presence-absence matrix
#'
#' This function constructs a presence-absence matrix based on the space sites.
#'
#' @param species_list a list of species to include in the calculations.
#' @param space the space to calculate over.
#' @param xy if TRUE, site coordinates are returned as matrix columns. Default is FALSE.
#' @param empty_sites if TRUE, sites with no species will be included in the matrix. Default is FALSE.
#'
#' @returns a presence matrix with sites as rows and species as columns.
#' @keywords support
#' @export
#' @seealso \code{vignette("h-support-functions", package = "gen3sis2")}
#' @example inst/examples/support_functions/get_presence_matrix_help.R
get_presence_matrix <- function(
  species_list,
  space = NULL,
  xy = FALSE,
  empty_sites = FALSE
) {
  presence_matrix <- construct_community_matrices(
    species_list = species_list,
    space = space,
    xy = xy,
    empty_sites = empty_sites,
    mode = "presence"
  )
  return(presence_matrix)
}

#' Get global mean richness
#'
#' This is a simple function that computes the mean richness of a given space.
#'
#' @param species_list a list of species to include in the calculations.
#' @param space the space to calculate over.
#'
#' @returns a numeric value of the mean richness.
#' @keywords support
#' @export
#' @seealso \code{vignette("h-support-functions", package = "gen3sis2")}
#' @example inst/examples/support_functions/get_mean_richness_help.R
get_mean_richness <- function(species_list, space) {
  return(get_geo_richness(species_list, space) |> mean())
}

#' Get traits matrix
#'
#' This function constructs a matrix of traits present in occupied sites in the space
#'
#' @param species_list a list of species to include in the calculations.
#' @param summarize_fun a function or a named vector of functions to summarize trait values. Accept custom functions.
#'
#' @returns a trait matrix with sites as rows and traits as columns.
#' @keywords support
#' @export
#' @seealso \code{vignette("h-support-functions", package = "gen3sis2")}
#' @example inst/examples/support_functions/get_traits_matrix_help.R
get_traits_matrix <- function(species_list, summarize_fun = NULL) {
  if (!is.null(summarize_fun)) {
    mtx_list <- lapply(species_list, function(sp) {
      sp_mtx <- sp$traits

      t_names <- colnames(sp_mtx)
      t_list <- lapply(t_names, function(tn) {
        traits_summary <- sapply(c(summarize_fun), function(f) {
          f(sp_mtx[, tn])
        }) |>
          as.matrix() |>
          t()

        if (is.null(colnames(traits_summary))) {
          colnames(traits_summary) <- "summary"
        }

        colnames(traits_summary) <- paste0(tn, "_", colnames(traits_summary))

        traits_summary
      })

      t_mtx <- do.call(cbind, t_list)
      row.names(t_mtx) <- sp$id

      t_mtx
    })

    trait_mtx <- do.call(rbind, mtx_list)
  } else {
    mtx_list <- lapply(species_list, function(sp) {
      trait_mtx <- sp$traits
      site_mtx <- matrix(row.names(trait_mtx))
      colnames(site_mtx) <- "site"
      trait_mtx <- cbind(site_mtx, trait_mtx)
      row.names(trait_mtx) <- rep(sp$id, nrow(trait_mtx))
      trait_mtx
    })

    trait_mtx <- do.call(rbind, mtx_list)
  }

  return(trait_mtx)
}

#' Get abundance and traits matrix
#'
#' This function constructs a data.frame with abundance and trait information for each species and site
#'
#' @param species_list a list of species to include in the calculations.
#'
#' @returns a data.frame with traits values, abundance, and site and species indexes.
#' @keywords support
#' @export
#' @seealso \code{vignette("h-support-functions", package = "gen3sis2")}
#' @example inst/examples/support_functions/get_trait_abundance_help.R
get_trait_abundance <- function(species_list) {
  df_list <- lapply(species_list, function(sp) {
    traits <- sp$traits |> as.data.frame()
    traits$site <- row.names(traits)

    abundance <- data.frame(abundance = sp$abundance)
    abundance$site <- row.names(abundance)

    sp_df <- merge(traits, abundance, by = "site")
    sp_df$species <- sp$id
    sp_df
  })

  abd_trait_df <- do.call(rbind, df_list)

  return(abd_trait_df)
}

#' Get trait diversity
#'
#' This function calculates trait diversity per cell (sensu Leps et al 2006)
#'
#' @param species_list a list of species to include in the calculations.
#' @param traits a vector with trait names.
#' @references De Bello, F., Lepš, J. and Sebastià, M.-T. (2006), Variations in species and functional plant diversity along climatic and grazing gradients. Ecography, 29: 801-810. https://doi.org/10.1111/j.2006.0906-7590.04683.x
#' @returns a matrix with site as rows and trait diversity as columns.
#' @keywords support
#' @export
#' @seealso \code{vignette("h-support-functions", package = "gen3sis2")}
#' @example inst/examples/support_functions/get_trait_diversity_help.R
get_trait_diversity <- function(species_list, traits = NULL) {
  if (is.null(traits)) {
    traits <- species_list[[1]]$traits |> colnames()
  }

  trait_df <- get_trait_abundance(species_list) # return trait values

  sites <- unique(trait_df$site)
  trait_diversity <- c()

  div_sites <- lapply(sites, function(st) {
    site_values <- trait_df[trait_df$site == st, ] # filter out target cell
    trait_diversity <- sapply(traits, function(tt) {
      trait_mean <- mean(site_values[, tt]) # find mean trait value
      trait_dist <- (site_values[, tt] - trait_mean)^2 # find distance from mean
      trait_abd <- trait_dist * site_values$abundance # multiply by abundance
      trait_abd <- trait_abd / sum(site_values$abundance) # divide by total abundance
      diversity <- sum(trait_abd) # sum values
    })

    trait_diversity
  })

  div_sites <- do.call(rbind, div_sites)
  row.names(div_sites) <- sites

  return(div_sites)
}

#' Get trait evenness
#'
#' This function calculates trait evenness per cell (functional evenness, sensu Mouillot et al. (2005))
#'
#' @param species_list a list of species to include in the calculations.
#' @param traits a vector with trait names.
#' @references Mouillot D, Mason WH, Dumay O, Wilson JB. Functional regularity: a neglected aspect of functional diversity. Oecologia. 2005 Jan;142(3):353-9. doi: 10.1007/s00442-004-1744-7. Epub 2004 Nov 20. PMID: 15655690.
#' @returns a matrix with site as rows and trait eveness as columns.
#' @keywords support
#' @export
#'
#' @seealso \code{vignette("h-support-functions", package = "gen3sis2")}
#'
#' @example inst/examples/support_functions/get_trait_evenness.R
get_trait_evenness <- function(species_list, traits = NULL) {
  if (is.null(traits)) {
    traits <- species_list[[1]]$traits |> colnames()
  }
  trait_values <- get_trait_abundance(species_list)

  sites <- unique(trait_values$site) # vector of cell IDs
  sites <- sites[1:10]

  evenness <- lapply(sites, function(site) {
    site_data <- trait_values[site == site, ] # test values

    tt <- traits[[1]]

    traits_sums <- sapply(traits, function(tt) {
      site_data <- site_data[order(site_data[, tt]), ] # arrange by trait values

      # find sum of all trait and abundance values (sum EW)
      EW <- c()
      for (i in 1:dim(site_data)[1] - 1) {
        x <- (site_data[, tt][i + 1] - site_data[, tt][i]) /
          (site_data$abundance[i + 1] + site_data[, tt][i])
        EW <- c(EW, x)
      }
      sum_EW <- sum(EW) # the bottom of the equation

      # find species_value
      species_values <- c()
      for (i in 1:dim(site_data)[1] - 1) {
        top <- (site_data[, tt][i + 1] - site_data[, tt][i]) /
          (site_data$abundance[i + 1] + site_data[, tt][i])
        PEW <- top / sum_EW

        default <- 1 / (dim(site_data)[1] - 1) # find default (1 / Sc - 1)

        species_values <- c(species_values, min(PEW, default))
      }

      # find evenness
      sum(species_values)
    })

    traits_sums
  })

  trait_evenness <- do.call(rbind, evenness)
  row.names(trait_evenness) <- sites

  return(trait_evenness)
}

#' Get species prevalence
#'
#' This function calculates the prevalence of each species in a given space
#'
#' @param species_list a list of species to include in the calculations.
#' @param space the space to calculate over.
#'
#' @returns a vector containing species prevalence in decimal percentages.
#' @keywords support
#' @export
#' @seealso \code{vignette("h-support-functions", package = "gen3sis2")}
#' @example inst/examples/support_functions/get_species_prevalence_help.R
get_species_prevalence <- function(species_list, space) {
  prevalence <- sapply(species_list, function(sp) {
    sum(sp$abundance > 0) / nrow(na.omit(space$environment))
  })

  names(prevalence) <- sapply(species_list, function(sp) {
    sp$id
  })

  return(prevalence)
}

#' Gets extant species
#'
#' This function simply identify which species has abundance > 0 in at least one site
#'
#' @param species_list a list of species to include in the calculations.
#'
#' @returns a vector with extant species IDs.
#' @keywords support
#' @export
#' @seealso \code{vignette("h-support-functions", package = "gen3sis2")}
#' @example inst/examples/support_functions/get_extant_species.R
get_extant_species <- function(species_list) {
  extant <- sapply(species_list, \(sp) {
    any(sp$abundance > 0)
  })
  species_id <- sapply(species_list, \(sp) {
    sp$id
  })

  return(species_id[extant])
}


#' Gets site abundance
#'
#' This function calculates the total abundance per site in a given space
#'
#' @param species_list a list of species to include in the calculations.
#' @param space the space to calculate over.
#' @param xy if TRUE, site coordinates are returned as matrix columns. Default is FALSE.
#' @param empty_sites if TRUE, sites with no species will be included in the matrix. Default is FALSE.
#'
#' @returns a matrix with sites as rows and abundance as column.
#' @keywords support
#' @export
#'
#' @seealso
#' \code{vignette("h-support-functions", package = "gen3sis2")}
#' [Based on Thomas Keggin's implementation for gen3sis](https://gitlab.ethz.ch/ele-public/gen3sis_wiki/-/blob/master/tools/keggin/traitDiversity.R)
#'
#'
#' @example inst/examples/support_functions/get_site_abundance_help.R
get_site_abundance <- function(species_list, space, xy = F, empty_sites = F) {
  abundance_vector <- sapply(species_list, function(sp) {
    sp$abundance
  }) |>
    unlist()

  abundance_vector <- tapply(abundance_vector, names(abundance_vector), sum)
  abundance_matrix <- as.matrix(abundance_vector)
  colnames(abundance_matrix) <- "abundance"

  if (empty_sites) {
    empty_sites <- setdiff(
      row.names(space$coordinates),
      row.names(abundance_matrix)
    )
    empty_mtx <- matrix(0, nrow = length(empty_sites))
    row.names(empty_mtx) <- empty_sites
    colnames(empty_mtx) <- "abundance"

    abundance_matrix <- rbind(abundance_matrix, empty_mtx)
  }

  if (xy) {
    coords_mtx <- space$coordinates[row.names(abundance_matrix), ]
    abundance_matrix <- cbind(coords_mtx, abundance_matrix)
  }

  return(abundance_matrix)
}

#' Gets species range
#'
#' This function counts the number of sites each species is present in
#'
#' @param species_list a list of species to include in the calculations.
#' @param space the space to calculate over.
#'
#' @returns a named vector containing species (names) and its range (values).
#' @keywords support
#' @export
#' @seealso \code{vignette("h-support-functions", package = "gen3sis2")}
#' @example inst/examples/support_functions/get_species_range_help.R
get_species_range <- function(species_list, space) {
  pa_mtx <- get_presence_matrix(species_list, space)
  return(colSums(pa_mtx))
}

#' Gets weighted endemism
#'
#' This function calculates the weighted endemism for each site in a given space
#'
#' @param species_list a list of species to include in the calculations.
#' @param space the space to calculate over.
#'
#' @returns a named vector with the weighted endemism (values) for each site (names).
#' @keywords support
#' @export
#' @seealso \code{vignette("h-support-functions", package = "gen3sis2")}
#' @example inst/examples/support_functions/get_weighted_endemism_help.R
get_weighted_endemism <- function(species_list, space) {
  richness <- get_geo_richness(species_list, space)
  species_range <- get_species_range(species_list, space)

  pa_mtx <- get_presence_matrix(species_list, space, empty_sites = T)

  species_present <- apply(pa_mtx, 1, function(site) {
    names(site)[site == 1]
  })

  total_range <- sapply(species_present, function(site) {
    sum(species_range[site])
  })

  richness <- richness[names(total_range)]

  weighted_endemism <- richness / total_range
  weighted_endemism[is.na(weighted_endemism)] <- 0

  return(weighted_endemism)
}

#' Gets species subset
#'
#' This function subsets the species list with species occurring in the specified sites
#'
#' @param species_list a list of species to include in the calculations.
#' @param site_vector a vector with site indexes.
#' @param trim_sites if TRUE, species traits will be trimmed to the specified sites. Default is FALSE.
#'
#' @returns a list with selected species.
#' @keywords support
#' @export
#' @seealso \code{vignette("h-support-functions", package = "gen3sis2")}
#' @example inst/examples/support_functions/get_species_subset_help.R
get_species_subset <- function(species_list, site_vector, trim_sites = FALSE) {
  pa_mtx <- get_presence_matrix(species_list)
  pa_mtx <- pa_mtx[site_vector, , drop = FALSE]

  sp_subset <- colnames(pa_mtx)[colSums(pa_mtx) > 0]
  species_subset <- Filter(\(sp) sp$id %in% sp_subset, species_list)

  if (trim_sites) {
    # the old code didn't subset the divergence matrix even though there's a function
    # that can directly be used to subset the species object to certain cells
    species_subset <- lapply(species_subset, function(sp) {
      retained_sites <- intersect(names(sp[["abundance"]]), site_vector)
      limit_species_to_cells(sp, retained_sites)
    })
  }

  return(species_subset)
}

#' Gets space subset
#'
#' This function subsets the space based on specified sites
#'
#' @param space the space to calculate over.
#' @param site_vector a vector with site indexes.
#'
#' @returns a gen3sis_space_type object with the specified sites.
#' @keywords support
#' @export
#' @seealso [Based on Thomas Keggin's implementation for gen3sis](https://gitlab.ethz.ch/ele-public/gen3sis_wiki/-/blob/master/tools/keggin/subsetLandscape.R)
#' @example inst/examples/support_functions/get_space_subset_help.R
get_space_subset <- function(space, site_vector) {
  if (length(site_vector) < 2) {
    stop("Must subset at least 2 sites.")
  }

  space_subset <- space

  # environment
  space_subset$environment <- subset(
    space$environment,
    rownames(space$environment) %in% site_vector
  )

  # coordinates
  space_subset$coordinates <- subset(
    space$coordinates,
    rownames(space$coordinates) %in% site_vector
  )

  # extent
  #subset_landscape$extent <- extent(rasterFromXYZ(subset_landscape$coordinates))

  extent <- c(
    xmin = min(space_subset$coordinates[, "x"]),
    xmax = max(space_subset$coordinates[, "x"]),
    ymin = min(space_subset$coordinates[, "y"]),
    ymax = max(space_subset$coordinates[, "y"])
  )

  return(space_subset)
}


# miscellaneous tools ----

#' Diversification summary
#' 
#' This function constructs a matrix with speciation, extinction and diversification rate over timesteps
#'
#' @param gen3sis_output a simulation output object.
#'
#' @returns a matrix with timesteps as rows and speciation, extinction and diversification rates as columns.
#' @keywords support
#' @export
#' @seealso \code{vignette("h-support-functions", package = "gen3sis2")}
#' @example inst/examples/support_functions/diversification_summary_help.R
diversification_summary <- function(gen3sis_output) {
  extant_lineages <- gen3sis_output$summary$phylo_summary[
    1:nrow(gen3sis_output$summary$phylo_summary) - 1,
    2
  ]
  speciation <- gen3sis_output$summary$phylo_summary[
    2:nrow(gen3sis_output$summary$phylo_summary),
    3
  ]
  extinction <- gen3sis_output$summary$phylo_summary[
    2:nrow(gen3sis_output$summary$phylo_summary),
    4
  ]

  speciation_rate <- speciation / extant_lineages
  extinction_rate <- extinction / extant_lineages
  diversification_rate <- speciation_rate - extinction_rate

  diverse_df <- data.frame(
    timestep = names(diversification_rate),
    DR = diversification_rate,
    SR = speciation_rate,
    ER = extinction_rate
  )

  return(diverse_df)
}

#' Subset distance matrix
#'
#' This function subsets a distance matrix based on specified sites
#'
#' @param distance_matrix a local or full distance matrix. Can be either the object or a file path.
#' @param site_vector a vector with site indexes.
#'
#' @returns a distance matrix with only the specified sites.
#' @keywords support
#' @export
#' @seealso [Based on Thomas Keggin's implementation for gen3sis](https://gitlab.ethz.ch/ele-public/gen3sis_wiki/-/blob/master/tools/keggin/distanceSubset.R)
#'
#' @example inst/examples/support_functions/distance_subset_help.R
distance_subset <- function(distance_matrix, site_vector) {
  # load the file if distance_matrix is a path
  if (is.character(distance_matrix) && file.exists(distance_matrix)) {
    distance_matrix <- readRDS(distance_matrix)
  }

  # convert site ids to character to match matrix names
  site_vector <- as.character(site_vector)

  # subset the matrix
  distance_subset <- distance_matrix[site_vector, site_vector]

  return(distance_subset)
}
