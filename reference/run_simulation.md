# Run a simulation in gen3sis and return a summary object possibly saving outputs and plots to the output folder

Run a simulation in gen3sis and return a summary object possibly saving
outputs and plots to the output folder

## Usage

``` r
run_simulation(
  config = NA,
  space = NA,
  output_directory = NA,
  timestep_restart = NA,
  save_state = NA,
  call_observer = "all",
  enable_gc = FALSE,
  verbose = 1
)
```

## Arguments

- config:

  configuration file for the simulation or configuration object derived
  from a config file

- space:

  directory where the all_geo_hab and distance_matrices reside

- output_directory:

  directory for the simulation output

- timestep_restart:

  set the start time time-step. If timestep_restart=NA (default), start
  at the oldest available space. If timestep_restart="ti", start from
  the last available time-step. If a number "x", start at time-step x
  (e.g. timestep_restartstart=6)

- save_state:

  save the internal state of the simulation for restarts. If
  save_state=NA (default), do not save any internal state of the
  simulation. If save_state="all", save all time-step. If
  save_state="last", saves only last time-step. If a vector, saves the
  desired time-steps (e.g. save_state=c(1,3,5))

- call_observer:

  call observer functions. If call_observer="all" (default), call all
  time-steps. If call_observer=NA, calls the start and end times. If a
  number "X", call call_observer at x time-steps equally spaced between
  start and end steps. For example, on a simulation with start time of 1
  and end time of 20, call_observer=1 calls the observer function at
  time-steps 1, 11 and 20.

- enable_gc:

  enable gc in case of memory shortages

- verbose:

  integer value (i.e. 0, 1 ,2 or 3). If verbose=0, no printed statement.
  If verbose=1 (default), print time-step progress. If verbose=2, enable
  additional progress outputs regarding current time-step. If verbose=3,
  enable additional information from within modules

## Value

a summary object containing a minimal summary on simulation and dynamics
progress (alive, speciations, extinctions) as well as useful simulation
data

## Details

This function runs a simulation with defined space and config objects.
Possibly plot and save specified outputs as defined in the
end_of_timestep_observer function inside the config object

## See also

[`plot_summary`](plot_summary.md)
[`create_input_config`](create_input_config.md)
[`create_spaces_raster`](create_spaces_raster.md)

## Examples

``` r
# \donttest{
# get path or correct input objects
spaces <- system.file(file.path("extdata", "TestSpaces","geostatic_spaces","raster"), package="gen3sis2")
config_file <- system.file(file.path("extdata", "TestConfigs","TestConfig.R"), package="gen3sis2")
# run simulation and store summary obejct to sim
sim <- run_simulation(
  config = config_file, 
  space = spaces,
  output_directory = tempdir()
)
#> config found:  /home/runner/work/_temp/Library/gen3sis2/extdata/TestConfigs/TestConfig.Rspace found: /home/runner/work/_temp/Library/gen3sis2/extdata/TestSpaces/geostatic_spaces/raster 
#> Output directory is: /tmp/Rtmp4LCx7U/TestConfig 
#> 
#> Using config: TestConfig 
#> Warning:   Config time unit is set to 'timestep'.
#>   The simulation will fully assume spaces duration.
#>   Read more about time-scaling in the respective vignette.
#>   Config time unit is set to 'timestep'.
#>   The simulation will fully assume spaces duration.
#>   Read more about time-scaling in the respective vignette.
#> --- Initializing --- 
#> [1] "i 1 yls 2 n_sites 10"
#> [1] "i 2 yls 2 n_sites 10"
#> [1] "i 3 yls 2 n_sites 10"
#> [1] "i 4 yls -2 n_sites 10"
#> [1] "i 5 yls -2 n_sites 10"
#> --- Running simulation --- 
#> step = 4 , species alive = 5 , species total = 5 

#> step = 3 , species alive = 5 , species total = 5 


#> step = 2 , species alive = 5 , species total = 5 


#> step = 1 , species alive = 5 , species total = 5 


#> step = 0 , species alive = 5 , species total = 5 



#> Simulation finished. All OK 
#> Simulation runtime:  0.002141461  hours

# plot summary object
plot_summary(sim)

# }
```
