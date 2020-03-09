
################################
####    Project settings    ####
################################

#----     R packages   ----

# List of packages used in the analysis.

library("MASS")
library("tictoc")
library("tidyverse")

# packages_list <- c("")
#
# # load packages
# lapply(packages_list,require, character.only = TRUE)
#

#----    Procedure to remove packages   -----
# ip <- as.data.frame(installed.packages())
# ip <- subset(ip, !grepl("MRO", ip$LibPath))
# ip <- ip[!(ip[,"Priority"] %in% c("base", "recommended")),]
# path.lib <- unique(ip$LibPath)
# pkgs.to.remove <- ip[,1]
#
# sapply(pkgs.to.remove, renv::remove, lib = path.lib)

#----    renv comands    ----
# For renv function and options see: https://rstudio.github.io/renv/

# renv::settings$snapshot.type("packrat")   # Set snapshot options
# renv::dependencies()                      # Find dependencies
# renv::install("")                         # Install Packages
# renv::hydrate("formatR")                  # Upload packages from user lybrary
# sapply(packages_list, renv::hydrate)
# renv::remove()                            # Delate packages from project lybrary
# renv::purge()                             # Delate packages from cache (delate for all projects!!)
# renv::snapshot()                          # Create snapshot packages used

#---- function conflicts   ----
# For conflicts function and options see: https://www.tidyverse.org/blog/2018/06/conflicted/

# conflicted::conflict_scout()    # See conflicts

#conflicted::conflict_prefer()

#-----    ggplot2 settings    ----
theme_set(theme_bw())
