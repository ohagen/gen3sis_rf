# Copyright (c) 2020, ETH Zurich

#---------------------------------------------------#
##########        Internal functions        #########
#---------------------------------------------------#

#' selects the habitable cells from the input
#' @details a leftover from when the habitable cells were intended to be changed in the fly
#'
#' @param what the input to be subset
#'
#' @return a boolean vector indicating which inputs are habitable
#' @noRd
select_habitable_hab <- function(what) {
  # what should be a string! NA`s are considered unsuitable
  # selected <- eval(parse(text = paste(what, condition, "& !is.na(", what, ")")))
  selected <- !is.na(what)
  return(selected)
}


#' calculate the richness of a list of species over a given space
#'
#' @param species_list a list of species to include in the richness calculations
#' @param space the space to calculate the richness over
#'
#' @return a vector with the richness for every cell in the input space
#' @keywords support
#' @example inst/examples/get_geo_richness_help.R
#' @seealso \code{\link{plot_richness}}
#' @export
get_geo_richness <- function(species_list, space) {
  cell_names <- rownames(space[["coordinates"]])
  presences <- sapply(
    species_list,
    function(sp, cell_names) {
      cell_names %in% names(sp[["abundance"]])
    },
    cell_names
  )
  richness <- rowSums(presences)
  names(richness) <- cell_names
  return(richness)
}


#' calculate the individual average traits for all given species
#' currently deprecated
# @param species_list a list
#'
# @return a matrix filled with average traits vs all species
# @noRd
#get_eco_by_sp <- function(species_list) {
#  averages <- t(sapply(species_list, function(sp) {colMeans(sp[["traits"]])} ))
#  return(invisible(averages))
#}

#' saves the current phylogeny in nex format(?)
#'
#' @param phy the phylogeny up to this point
#' @param label a lable
#' @param output_file the file path and name to store the result
#'
#' @importFrom stringr str_replace str_extract
#' @importFrom utils write.table
#' @noRd
write_nex <- function(phy, label = "sp", output_file) {
  #    phy <- sgen3sis$phy  phy <- duplo  phy <- simples
  val <- dynGet("val")

  #check if we start with more than one ancestor, i.e. more than one root.
  rootphy <- phy$Speciation.Type == "ROOT"

  if (sum(rootphy) > 1) {
    phy$Ancestor[rootphy] <- 0
    phy$Speciation.Type <- as.character(phy$Speciation.Type)
    phy$Speciation.Type[rootphy] <- "COMB"
    addroot <- c(
      "0",
      "0",
      phy$Speciation.Time[1],
      val$config$gen3sis$general$duration$to - 1,
      "ROOT"
    )
    names(addroot) <- colnames(phy)
    phy <- rbind(addroot, phy)
    phy$Ancestor <- as.integer(phy$Ancestor)
    phy$Descendent <- as.integer(phy$Descendent)
    phy[, c("Ancestor", "Descendent")] <- phy[, c("Ancestor", "Descendent")] + 1
    phy$Ancestor <- as.integer(phy$Ancestor)
    phy$Descendent <- as.integer(phy$Descendent)
    phy$Speciation.Time <- as.integer(phy$Speciation.Time)
    phy$Extinction.Time <- as.numeric(phy$Extinction.Time)
    phy$Speciation.Type <- as.factor(phy$Speciation.Type)
  }

  #remove root
  phy_no_root <- phy[-1, , drop = FALSE]

  if (nrow(phy_no_root) == 0) {
    #following TreeSimGM and TreeSim convertion
    if (phy[1, "Extinction.Time"] == val$config$gen3sis$general$duration$to - 1) {
      String_final <- "1" # tree with only root
    } else {
      String_final <- "0" # tree with only root that got extinct
    }
  } else {
    Ancestral <- phy_no_root[, "Ancestor"]
    Derived <- phy_no_root[, "Descendent"]

    #Time calibrated tree
    root <- phy[phy$Speciation.Type == "ROOT", "Speciation.Time"]

    String <- paste0(label, "1:", root)
    Age <- c(root, phy_no_root[, "Speciation.Time"])
    #Age_new<-Age
    #unique(phy$Ancestor)

    Ancestral_age <- phy[unique(phy$Ancestor), "Speciation.Time"]
    names(Ancestral_age) <- phy[!duplicated(phy$Ancestor), "Ancestor"]

    Extinction <- c(0, phy_no_root[, "Extinction.Time"])

    for (i in 1:(max(phy_no_root[, "Descendent"]))) {
      String <- str_replace(
        String,
        paste(
          label,
          Ancestral[i],
          ":",
          Ancestral_age[toString(Ancestral[i])],
          sep = ""
        ),
        paste(
          "(",
          label,
          Ancestral[i],
          ":",
          Age[i + 1],
          ",",
          label,
          Derived[i],
          ":",
          Age[i + 1],
          "):",
          Ancestral_age[toString(Ancestral[i])] - Age[i + 1],
          sep = ""
        )
      )

      Ancestral_age[toString(Ancestral[i])] <- Age[i + 1]
      # print("-------")
      # print(String)
    }
    #adding ext times!
    extsps <- phy[phy$Extinction.Time > 0, c("Descendent", "Extinction.Time")]
    if (nrow(extsps) > 0) {
      for (i in 1:nrow(extsps)) {
        spi <- paste0(label, extsps$Descendent[i], ":")
        # print("-------")
        # print(spi)
        splited <- strsplit(String, spi)[[1]]
        oldnumb <- str_extract(splited[2], "\\-*\\d+\\.*\\d*")
        newnumb <- as.numeric(oldnumb) - extsps$Extinction.Time[i]
        #newnumb <- extsps$Extinction.Time[i]
        splited[2] <- sub(oldnumb, newnumb, splited[2])
        String <- paste0(splited[1], spi, splited[2])
      }
    }

    String_final <- paste("Tree tree = ", String, ";", sep = "")
    String_final <- (paste(
      "#NEXUS",
      "begin trees;",
      String_final,
      "end;",
      "[!Tree generated with the gen3sis R package, following convention of TreeSim and TreeSimGM r-packages]",
      sep = "\n"
    ))
  } #end test if there is a tree

  write.table(
    String_final,
    output_file,
    quote = FALSE,
    row.names = FALSE,
    col.names = FALSE
  )

  #read phylo
  # t <- read.nexus(file.path(output_location, "phy.nex"))
  # plot(t)
}

#' Convert time units
#'
#' @param x numeric. The numerical value in "from" unit
#' @param from character. The unit to convert from
#' @param to character. The unit to convert to
#'
#' @returns The numerical values in "to" unit
#' @noRd
conv_unit <- function(x, from, to) {
  if (from == "timestep" || to == "timestep") {
    return(x)
  }
  exponents <- c(yr = 0, kyr = 3, Myr = 6, Gyr = 9)
  factor <- 10^(exponents[from] - exponents[to])
  return(x * factor[[1]])
}

#' Check if time matches between space and config
#'
#' @param config_duration numeric. The numerical value in "from" unit
#' @param space_duration character. The unit to convert from
#'
#' @returns NULL
#' @noRd
check_time_match <- function(config_duration, space_duration){
  # preliminary tests
  is_numerical <- sapply(config_duration[c("from","to","by")], function(x){
    is.numeric(x)
  }) |> all()
  
  if(!is_numerical){
    stop("Config's duration$from, duration$to, and duration$by must be numerical.")
  }

  if(config_duration$by <= 0) {
    stop("Config's duration$by must be positive.")
  } 
  
  # harmonize time units
  if(xor(config_duration$unit == "timestep", space_duration$unit == "timestep")){
    stop("Cannot interpretate timeframe. Both config's and spaces' duration$unit must be 'timestep'.")
  } 

  if (config_duration$unit != space_duration$unit){
    warning("Config's and spaces' duration$by are different. Config's duration by will be converted.")
    config_duration[c("from","to","by")] <- lapply(config_duration[c("from","to","by")], function(x){
      conv_unit(x, config_duration$unit, space_duration$unit)
    })
    config_duration$unit <- space_duration$unit
  }

  # check temporal bonds
  if(config_duration$by > (config_duration$to-config_duration$from)){
    stop("Config's duration$by cannot be greater than total config's time range.")
  }

  if(any(
    config_duration$from < space_duration$from,
    config_duration$to > space_duration$to,
    config_duration$from > config_duration$to
  )){
    stop("Config's temporal resolution is out of bounds. Check config's duration.")
  }

  if(config_duration$by < space_duration$by){
    stop("Cannot subset spaces' timesteps. Config's duration$by must be equal or greater than spaces' duration$by.")
  }
  
  # computes time proportion
  time_proportion <- config_duration$by/space_duration$by

  if(time_proportion != 1){
    text_warn <- paste0(
        "--- TIME-STEP MISMATCH ---\n  ",
        "Config's duration$by is ",
        time_proportion, " ",
        "times the spaces' duration$by.\n  ",
        "Spaces' time-steps will be subsetted.\n  ",
        "Read more about time-scaling in the respective vignette."
      )
    message(text_warn)
    warning(text_warn)
  }

  return(config_duration)
}