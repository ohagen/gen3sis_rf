# Creates either an empty configuration or a pre-filled configuration object from a config file

Creates either an empty configuration or a pre-filled configuration
object from a config file

## Usage

``` r
create_input_config(config_file = NA, config_name = NULL)
```

## Arguments

- config_file:

  the path to a valid configuration file. if NA it creates an empty
  config

- config_name:

  the name of the configuration. if NULL it will be set to a random
  name, if a empty config is created, or use the file name

## Value

list of configuration elements, similar generated from reading a
config_file.R. The internal elements of this list are: "general",
"initialization", "dispersal", "speciation", "mutation" and "ecology"

## Examples

``` r
# create empty config object
config_empty <- create_input_config(config_file = NA)

# create a config object from config_file
# get path to example config
datapath <- system.file(file.path("extdata/TestSpaces/geodynamic_spaces"), package="gen3sis2")
path_config <- system.file(file.path("extdata/TestConfigs/TestConfig.R"), package="gen3sis2")
config_object <- create_input_config(config_file = path_config, config_name = "your_config")

# change seed of config_worldcenter config object
config_object$gen3sis$general$random_seed <- 2025

# run the model for config_object
# \donttest{
  sim <- run_simulation(config = config_object, 
                        space = file.path(datapath, "raster"), 
                        output_directory = tempdir())
#> Config found: using config object 
#> space found: /home/runner/work/_temp/Library/gen3sis2/extdata/TestSpaces/geodynamic_spaces/raster 
#> Output directory is: /tmp/Rtmp4LCx7U/your_config 
#> 
#> Using config: your_config 
#> Warning:   Config time unit is set to 'timestep'.
#>   The simulation will fully assume spaces duration.
#>   Read more about time-scaling in the respective vignette.
#>   Config time unit is set to 'timestep'.
#>   The simulation will fully assume spaces duration.
#>   Read more about time-scaling in the respective vignette.
#> --- Initializing --- 
#> [1] "i 1 yls 2 n_sites 5"
#> [1] "i 2 yls 2 n_sites 5"
#> [1] "i 3 yls 2 n_sites 5"
#> [1] "i 4 yls -2 n_sites 5"
#> [1] "i 5 yls -2 n_sites 5"
#> --- Running simulation --- 
#> step = 4 , species alive = 5 , species total = 5 

#> step = 3 , species alive = 5 , species total = 5 


#> step = 2 , species alive = 5 , species total = 5 


#> step = 1 , species alive = 5 , species total = 5 


#> step = 0 , species alive = 5 , species total = 5 



#> Simulation finished. All OK 
#> Simulation runtime:  0.002301656  hours
# }
```
