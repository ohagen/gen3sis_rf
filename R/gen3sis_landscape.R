# Copyright (c) 2020, ETH Zurich

#' Creates an object representing the current landscape for a given time-step
#'
#' @details This function generates the internal representation of a given landscape. It 
#' consists of an identifier for the input time-step, an environmental matrix containing the environmental 
#' conditions for each suitable site, and a coordinates matrix containing the site cell center coordinates of 
#' all suitable sites
#'
#' @param id the identifier of the input time-step
#' @param timestep the name of the input time-step 
#' @param environment a matrix containing the environmental conditions
#' @param coordinates a matrix containing the coordinates of the cells centers
#' @param extent a vector of 4 containing the extent of the landscape
#' @param geodynamic a boolean indicating if the landscape is geodynamic
#' @param type a character either "raster", "h3", or "points". Final objects have
#' @param type_spec_res a numeric necessary in case type is "h3"
#' class "gen3sis_space_raster", "gen3sis_space_h3", or "gen3sis_space_points" respectively.
#'
#' @return returns a landscape of class "gen3sis_space"
#' @noRd
create_landscape <- function(id, timestep, environment, coordinates, extent=NA, geodynamic=NA, type = NA, type_spec_res=NA) {
  # landscape <- list()
  # landscape[["id"]] <- id
  # landscape[["timestep"]] <- timestep
  # landscape[["environment"]] <- environment
  # landscape[["coordinates"]] <- coordinates
  # landscape[["type"]] <- type
  #landscape[["extent"]] <- extent
  #landscape[["resolution"]] <- resolution
  landscape <- structure(list(id=id, 
                              timestep=timestep, 
                              environment=environment, 
                              coordinates=coordinates, 
                              extent=extent, 
                              geodynamic=geodynamic,
                              type=type, 
                              type_spec_res=type_spec_res),
            class = c(paste("gen3sis_space", type, sep = "_"), "list"))
  return(invisible(landscape))
}

