#######################
####   Analysis    ####
#######################

#----    Load    -----

source("R/Settings.R")            # Load packages
source("R/Functions.R")           # Load functions
source("R/Auxiliary_functions.R") # Load auxiliary functions
source("R/Plan.R")                # Load analysis plan

#----    Check   -----

# Configure the analysis plan
config <- drake::drake_config(plan)

# Plot the analysis plan
vis_drake_graph(config, font_size = 16, targets_only = T)

#----    Make    ----

# Delate the analysis cache
# drake::clean(destroy = TRUE)

# Run the analysis
make(plan)

# Plot the analysis plan
vis_drake_graph(config, font_size = 16, targets_only = T)

#----    Load elements    ----
# With drake you load the results with the function:
# - loadd() to load the object in the environment
# - readd() print the object
