#' title: Download RAP data
#' date: 7 May, 2025
#' original author: Julia Stuart
#' current author: Matthew Van Scoyoc
#' 
#' This script downlaods RAP biomass and cover data and saves them to a project
#'     directory. Change the variables in the Project Variables section and run 
#'     the rest of the script.
#' -----------------------------------------------------------------------------


# Project Variables ----

# Forest name
unit_name <- "Bridger-Teton NF"

# Path to project directory
proj_dir <- file.path("T:/FS/NFS/PSO/MPSG/Workspace/MVanScoyoc_GISLead/BT_RAP")
fig_dir <- file.path(proj_dir, "figures")
# create figure directory if it doesn't exist
if (!dir.exists(fig_dir)) dir.create(fig_dir)

# Path to geodatabase
gdb_path <- file.path(proj_dir, "BT_RAP.gdb")

# Area of Interest feature class name
aoi_fc <- "btnf_admin_bndry"

# Raster directory path
raster_dir <- file.path(proj_dir, "rap_data")
# create raster directory if it doesn't exist
if (!dir.exists(raster_dir)) dir.create(raster_dir)

# Years of RAP data you want to download
rap_yrs <- c(1986:2024)

# NOTE: You shouldn't have to change anything else in this script.


# Setup ----
## Packages ----
#-- List of packages used in this script
pkgs <- c(
  "rapr",     # Download RAP data
  "remotes",  # Download packages from GitHub
  "sf",       # Spatial vector data tools
  "terra",    # Spatial raster data tools
  "tmap"      # Map tools
  )

#-- install packages
# List packages in your library
inst_pkgs <- pkgs %in% rownames(installed.packages())
# Install the rapr package from GitHub if it's not in your library
if(!"rapr" %in% inst_pkgs) remotes::install_github("brownag/rapr")
# Install other packages from CRAN
if (any(inst_pkgs == FALSE)) {
  install.packages(pkgs[!inst_pkgs], 
                   lib =  .libPaths()[1], 
                   repos = "https://cloud.r-project.org",
                   type = 'source', 
                   dependencies = TRUE, 
                   quiet = TRUE)
}
#-- Load packages
invisible(lapply(pkgs, library, character.only = TRUE))


# Area of Interest ----
#-- Project coordinate reference system (crs)
target_crs <- "EPSG:4326" # crs of raster data

#-- Read AoI vector data
aoi_bndry <- sf::read_sf(dsn = gdb_path, layer = aoi_fc) |> 
  sf::st_transform(target_crs)

# Plot quick map
tmap::tm_shape(aoi_bndry) + tmap::tm_borders(col = "#4EB265", lwd = 2) +
  tmap::tm_add_legend(type = "polygons", labels = c("Area of Interest"), 
                      fill = "white", col = "#4EB265", lwd = 2, 
                      title = unit_name)


# Downlaod RAP Data ----

## Biomass Data ----
# This function will download the RAP data and load them into R. 
biomass <- rapr::get_rap(x = aoi_bndry, 
                         years = rap_yrs, 
                         product = "vegetation-biomass", 
                         version = "v3")
#-- Save raster 
terra::writeRaster(
  biomass,
  filename = file.path(
    raster_dir,
    paste0("RAP_biomass_", min(rap_yrs), "_", max(rap_yrs), ".tif")
  ), 
  overwrite = TRUE
)


## Percent Cover Data ----
# This function will download the RAP data and load them into R.
cover <- get_rap(x = aoi_bndry, 
                 years = rap_yrs, 
                 product = "vegetation-cover",
                 version = "v3")
#-- Save raster
terra::writeRaster(
  cover,
  filename = file.path(
    raster_dir,
    paste0("RAP_cover_", min(rap_yrs), "_", max(rap_yrs), ".tif")
  ), 
  overwrite = TRUE
)
