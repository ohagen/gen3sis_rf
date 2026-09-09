# Copyright (c) 2020, ETH Zurich

#' Allows the user to define the rate at which geographic clusters accumulate differentiation
#' with each other.
#'
#' @details This function determines the increase in divergence between separated clusters of a species. This function
#' should return either (i) a single value if there is an homogeneous divergence, or (ii) a matrix indicating the divergence that
#' should be accumulated between specific pairwise geographic clusters.
#'
#' The function can either return a single value or a full cluster by cluster matrix. If only one value is returned it will be used
#' to increment divergence between any given distinct cluster pairs. If a matrix is returned it has to be in the dimension of
#' cluster x cluster, in which case the divergence values will be increased according to the cluster membership of any cell pairs.
#'
#' For every time step, the divergence between geographic clusters can increase by a defined number. The divergence values can be
#' scaled optionally using the species or space information. For instance, the divergence between clusters could be higher under
#' warmer temperature, or difference in ecological traits could promote faster divergence between clusters.
#'
#' Oppositely, for every time-step, if cluster are merged their divergence is reduced by one (1).
#'
#' @param species the species of the current time step
#' @param cluster_indices an index vector indicating the cluster every occupied site is part of
#' @param space the space of the current time step
#' @param config the config of the simulation
#'
#' @return a single value or a matrix of divergences between all clusters occurring in clusters_indices
#' @keywords user
#' @export
get_divergence_factor <- function(species, cluster_indices, space, config) {
  stop(
    "this function documents the user function interface only, do not use it!"
  )
}

#' User-specified function determining the rules for within-cluster divergence of populations. 
#'
#' @param species the species of the current time step
#' @param cells cells occupied by the species part of the meta-population
#' @param divergence site by site divergence matrix for the meta-population
#' @param space the space of the current time step
#' @param config the config of the simulation
#'
#' @return a site by site matrix of potential divergence
#' @keywords user
#' @export
get_within_cluster_divergence_factor <- function(species, cells, divergence, space, config){
  stop("this function documents the user function interface only, do not use it!")
}

#' Orchestrates the speciation of any species alive in the simulation
#'
#' @param config the current config object
#' @param data the current data object
#' @param vars the current vars object
#'
#' @return an expanded species list including all newly created species
#' @noRd
loop_speciation <- function(config, data, vars) {
  if (config$gen3sis$general$verbose >= 3){
    cat(paste("entering speciation module \n"))
  }
  for (spi in 1:vars$n_sp) {
    # loop over existing species
    # get compressed genetic distance for spi
    species <- data$all_species[[spi]]

    if (!length(species[["abundance"]])) {
      next()
    }
    # define occupied cells by species
    species_presence <- names(species[["abundance"]])

    ##calling RCPP function to define physical clusters
    if (length(species_presence) == 1) {
      #check if only one cell is occupied
      clu_geo_spi_ti <- 1
    } else {
      distances <- config$gen3sis$dispersal$get_dispersal_values(
        length(species_presence),
        species,
        data$space,
        config
      )

      permutation <- sample(
        1:length(species_presence),
        length(species_presence)
      )
      clu_geo_spi_ti <- Tdbscan_variable(
        data$distance_matrix[
          species_presence[permutation],
          species_presence[permutation],
          drop = FALSE
        ],
        distances,
        1
      )
      clu_geo_spi_ti <- clu_geo_spi_ti[order(permutation)]
    }

    gen_dist_spi <- decompress_divergence(species[["divergence"]])
    # update genetic distances
    ifactor <- config$gen3sis$speciation$get_divergence_factor(species, clu_geo_spi_ti, data[["space"]], config)

    # update the between cluster divergence
    gen_dist_spi <- update_divergence(
      gen_dist_spi, 
      clu_geo_spi_ti, 
      ifactor = ifactor
    )
    
    # update the within cluster divergence (or homogenisation)
    if (length(species_presence) > 1) {
      
      gen_dist_spi <- update_within_cluster_divergence(
        divergence = gen_dist_spi,
        species = species,
        species_presence = species_presence,
        cluster_indices = clu_geo_spi_ti,
        space = data[["space"]],
        config = config
      )
    }

    gen_dist_spi <- compress_divergence(gen_dist_spi)
    # Add the within_site divergence back, which was removed in the decompression step
    # Probably do not have to subset, as the order and cells hasn't changed?
    gen_dist_spi[["within_site"]] <- species[["divergence"]][["within_site"]][
      names(gen_dist_spi[["index"]])]
    
    species[["divergence"]] <- gen_dist_spi
    # scan if any cluster exceeds the threshold
    clu_gen_spi_ti_c <- Tdbscan(
      gen_dist_spi$compressed_matrix,
      config$gen3sis$speciation$divergence_threshold,
      1
    )
    clu_gen_spi_ti <- clu_gen_spi_ti_c[gen_dist_spi$index]
    n_new_sp <- max(clu_gen_spi_ti) - 1

    # update count of new species at this time-step
    vars$n_new_sp_ti <- vars$n_new_sp_ti + n_new_sp

    if (n_new_sp > 0) {
      #if a speciation event occurred
      if (config$gen3sis$general$verbose >= 3) {
        cat(paste("[!]   Wellcome Strange  Thing   [!] \n"))
        cat(paste(n_new_sp, "speciation event(s) happened \n"))
      }

      #attributing the final names of the species in a vector
      desc_unique <- unique(clu_gen_spi_ti)[-1] +
        vars$n_sp +
        vars$n_sp_added_ti -
        1

      #udpate phy
      data$phy <- rbind(
        data$phy,
        data.frame(
          "Ancestor" = rep(spi, n_new_sp),
          "Descendent" = desc_unique,
          "Speciation.Time" = rep(vars$ti, n_new_sp),
          "Extinction.Time" = rep(vars$ti, n_new_sp),
          "Speciation.Type" = rep("Genetic", n_new_sp)
        )
      )

      #required for proper initialization of new species
      full_gen_dist <- gen_dist_spi
      gen_dist_spi$index <- gen_dist_spi$index[clu_gen_spi_ti == 1]
      ue <- unique(gen_dist_spi$index)
      gen_dist_spi$compressed_matrix <- gen_dist_spi$compressed_matrix[
        ue,
        ue,
        drop = FALSE
      ]
      #update names
      if (length(ue) > 0) {
        fullrange <- 1:length(ue)
        dimnames(gen_dist_spi$compressed_matrix) <- list(fullrange, fullrange)
        for (i in 1:length(gen_dist_spi$index)) {
          gen_dist_spi$index[i] <- fullrange[ue == gen_dist_spi$index[i]]
        }
      }

      for (desci in desc_unique) {
        #get the value of current gen_clu
        tep_clu_gen_desci_index <- which(desc_unique == desci) + 1
        new_species <- create_species_from_existing(
          species,
          desci,
          names(species[["abundance"]][
            clu_gen_spi_ti == tep_clu_gen_desci_index
          ]),
          config
        )
        data$all_species <- append(data$all_species, list(new_species))
      } # end loop over descendants

      species <- limit_species_to_cells(
        species = species,
        cells = names(species[["abundance"]][clu_gen_spi_ti == 1])
      )

      #update number of species added
      vars$n_sp_added_ti <- vars$n_sp_added_ti + n_new_sp

      # taking the physical clusters, but removing the already speciated species from spi (i.e. the "mother species")
      clu_geo_spi_ti <- clu_geo_spi_ti[clu_gen_spi_ti == 1]
    } # end of creating new species

    data$all_species[[spi]] <- species
  } # end loop over existing species
  if (config$gen3sis$general$verbose >= 3) {
    cat(paste("exiting speciation module \n"))
  }
  if (config$gen3sis$general$verbose >= 3 && vars$n_sp_added_ti > 0) {
    cat(paste(vars$n_sp_added_ti, "new species created \n"))
  }
  return(list(config = config, data = data, vars = vars))
}


#' Updates a given divergence matrix
#'
#' @param divergence a divergence matrix
#' @param cluster_indices a cluster index
#' @param ifactor the divergence factor by which the clusters distances are to be increased
#' @param dfactor the homogenisation factor by which the within cluster divergence distances are to be decreased
#'
#' @return an updated divergence matrix
#' @noRd
update_divergence <- function(divergence, cluster_indices, ifactor) {
  # update genetic distance
  if( length(ifactor) == 1 ) {
    # scalar ifactor
    # first determine if the sites/clusters belong to the same clusters
    between_clusters <- outer(cluster_indices, cluster_indices, "!=")
    # only add the ifactor to divergences from different clusters
    divergence[between_clusters] <-
      divergence[between_clusters] + ifactor
  } else {
    # matrix ifactor
    divergence <- divergence + ifactor[cluster_indices, cluster_indices]
    # it is assumed that the user already modifies the diagonal of the ifactor matrix
    # this means that the default is likely 0 meaning no change in divergence within clusters
  }

  # Genetic differences can not be negative
  divergence[divergence < 0] <- 0
  # end updating genetic distance##
  return(divergence)
}

#' Updates a given divergence matrix according to within cluster divergence
#'
#' @param divergence a divergence matrix
#' @param species the species of the current time step
#' @param species_presence character vector of the current occupied sites by the species
#' @param cluster_indices an index vector indicating the cluster every occupied site is part of
#' @param space the space of the current time step
#' @param config the current config object
#'
#' @return an updated divergence matrix
#' @noRd
update_within_cluster_divergence <- function(
    divergence,
    species,
    species_presence,
    cluster_indices,
    space,
    config
) {
  
  for(cluster in unique(cluster_indices)){
    
    cluster_cells <- species_presence[cluster_indices == cluster]
    # no within meta-population divergence if there's only one population
    if(length(cluster_cells) < 2){
      next
    }
    
    cluster_divergence <-
      divergence[
        cluster_cells,
        cluster_cells,
        drop = FALSE
      ]
    
    divergence_update <-
      config$gen3sis$speciation$
      get_within_cluster_divergence_factor(
        species = species,
        cells = cluster_cells,
        divergence = cluster_divergence,
        space = space,
        config = config
      )
    
    # regardless if the update is a scalar or a matrix addition works
    cluster_divergence <- cluster_divergence + divergence_update
    
    cluster_divergence[cluster_divergence < 0] <- 0
    diag(cluster_divergence) <- 0
    
    divergence[
      cluster_cells,
      cluster_cells
    ] <- cluster_divergence
  }
  
  return(divergence)
}

#' Orchestrates within-site speciation
#'
#' Only species that existed at the start of the current time step are
#' evaluated. Species created by spatial or within-site speciation during
#' this time step are not evaluated again.
#'
#' @param config current config object
#' @param data current data object
#' @param vars current vars object
#'
#' @return updated config, data and vars
#' @noRd
loop_within_site_speciation <- function(config, data, vars) {
  if (!is.function(config$gen3sis$speciation$apply_within_site_speciation)) {
    return(list(config = config, data = data, vars = vars))
  }
  
  if (config$gen3sis$general$verbose >= 3) {
    cat("entering within-site speciation module\n")
  }
  
  # vars$n_sp has not yet been updated with species created during this
  # timestep. It therefore represents the species present at its start.
  # this means that within site speciation doesn't depend on loop order and avoids 
  # multiple lineages splitting of at the same time (which would cause zero-duration 
  # phylogenetic branches. To note is that species get evaluated in a post spatial
  # cluster split state, which does establish a certain priority of processes.
  n_species_at_timestep_start <- vars$n_sp
  
  for (spi in seq_len(n_species_at_timestep_start)) {
    parent_before <- data$all_species[[spi]]
    
    if (!length(parent_before[["abundance"]])) {
      next
    }
    
    result <-
      config$gen3sis$speciation$apply_within_site_speciation(
        species = parent_before,
        space = data[["space"]],
        config = config
      )
    
    # updating the parent, e.g. within-site and abundance, is the responsibility 
    # of the user. Because we might otherwise limit user-freedom.
    parent_after <- result[["species"]]
    events <- result[["events"]]
    
    # changes in the divergence within a site may need to be updated regardless
    # of successful speciation.
    data$all_species[[spi]] <- parent_after
    
    # if no events have occurred, skip to the next species
    if (is.null(events) | !length(events)) {
      next
    }
    
    for (event in events) {
      site <- as.character(event[["site"]])
      daughter_abundance <- event[["daughter_abundance"]]
      daughter_traits <- event[["daughter_traits"]]
      
      if (
        is.null(names(daughter_traits)) ||
        !all(
          colnames(parent_after[["traits"]]) %in%
          names(daughter_traits)
        )
      ) {
        stop(
          "daughter_traits must be a named vector containing every ",
          "trait present in the parent species."
        )
      }
      
      new_id <-
        vars$n_sp +
        vars$n_sp_added_ti +
        1L
      
      daughter <- create_species_within_site(
        parent_species = parent_after,
        new_id = new_id,
        site = site,
        daughter_abundance = daughter_abundance,
        daughter_traits = daughter_traits,
        config = config
      )
      
      data$all_species <- append(
        data$all_species,
        list(daughter)
      )
      
      data$phy <- rbind(
        data$phy,
        data.frame(
          Ancestor = spi,
          Descendent = new_id,
          Speciation.Time = vars$ti,
          Extinction.Time = vars$ti,
          Speciation.Type = "Sympatric"
        )
      )
      
      vars$n_new_sp_ti <-
        vars$n_new_sp_ti + 1L
      
      vars$n_sp_added_ti <-
        vars$n_sp_added_ti + 1L
    }
  }
  
  if (config$gen3sis$general$verbose >= 3) {
    cat("exiting within-site speciation module\n")
  }
  
  return(list(
    config = config,
    data = data,
    vars = vars
  ))
}

#' Updates the total number of species
#'
#' @param config the current config object
#' @param data the current data list
#' @param vars the current vars list
#'
#' @return the updated vals list
#' @noRd
update1.n_sp.all_geo_sp_ti <- function(config, data, vars) {
  # update number of species
  vars$n_sp <- vars$n_sp + vars$n_sp_added_ti
  return(list(config = config, data = data, vars = vars))
}


#' Updates the total number of species alive
#'
#' @param config the current config object
#' @param data the current data list
#' @param vars the current vars list
#'
#' @return the updated vals list
#' @noRd
update2.n_sp_alive.geo_sp_ti <- function(config, data, vars) {
  # update number of species alive
  vars$n_sp_alive <- sum(sapply(data$all_species, function(sp) {
    ifelse(length(sp[["abundance"]]), 1, 0)
  }))
  return(list(config = config, data = data, vars = vars))
}
