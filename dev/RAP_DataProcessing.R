#' title: Process RAP data
#' date: 13 May, 2025
#' original author: Julia Stuart
#' current author: Matthew Van Scoyoc
#' 
#' This script has function and code to subset RAP data, calculate new rasters 
#'     (total biomass or change), and save them. There is code from Julia 
#'     Stuart at the bottom that can be adapted for additional raster 
#'     calculations or to create a multi-panel figure with a map and a histogram.
#' 
#' RAP data processing will vary by Forest Service (FS) unit. Modify this script to 
#'     get the data you need. Additional functions or processing steps will 
#'     included here as we use the RAP data on additional FS units.
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
# Ecosystem feature class name
ecos_diss_fc <- "btnf_nf_ecos_dissolve"

# Raster directory path
raster_dir <- file.path(proj_dir, "rap_data")
# create raster directory if it doesn't exist
if (!dir.exists(raster_dir)) dir.create(raster_dir)

# Years of RAP data
rap_yrs <- c(1986:2024)


# Setup ----
## Packages ----
#-- List of packages used in this script
pkgs <- c(
  "ggplot2",  # Making figures (plots)
  "remotes",  # Install packages from GitHub
  "sf",       # Spatial vector data tools
  "stringr",  # String (character) data tools
  "terra"     # Spatial raster data tools
)

#-- Install packages
# List packages if they are in your library
inst_pkgs <- pkgs %in% rownames(installed.packages())
# Install packages not in your library
if (any(inst_pkgs == FALSE)) {
  install.packages(pkgs[!inst_pkgs], 
                   lib =  .libPaths()[1], 
                   repos = "https://cloud.r-project.org",
                   type = 'source', 
                   dependencies = TRUE, 
                   quiet = TRUE)
}
#-- Load packages
invisible(lapply(pkgs, library, character.only = TRUE)) |> suppressMessages()


# Functions ----

#' Calculate change for each variable in a RAP raster
#'
#' @param raster SpatRaster of RAP data
#' @param t1 A vector of years for time point one, the reference time period. 
#'               e.g., 1990:1994
#' @param t2 A vector of years for time point two, the comparison time period. 
#'               e.g., 2020:2024
#'
#' @return SpatRaster
change_by_variable <- function(raster, t1, t2){
  # raster = bm_sage; t1 = 1990:1994; t2=2020:2024
  data_type = stringr::str_split_i(names(raster), "_", 1) |> unique()
  data_sets = tibble::tibble(raster_name = names(raster)) |>
    dplyr::mutate(var = stringr::str_split_i(raster_name, "_", 4),
                  var = ifelse(var == "annual", "annual_forb_grass", var), 
                  var = ifelse(var == "perennial", "perennial_forb_grass", var), 
                  year = as.numeric(stringr::str_split_i(raster_name, "_", 3)))
  change_ras = lapply(unique(data_sets$var), function(my_var){
    # my_var = unique(data_sets$var)[1]
    print(my_var)
    # Subset t1 data
    t1_lyrs = data_sets |> 
      dplyr::filter(var == my_var & year %in% t1) |> 
      dplyr::pull(raster_name)
    t1_ras = terra::app(terra::subset(raster, t1_lyrs), mean)
    # Subset t2 data
    t2_lyrs = data_sets |> 
      dplyr::filter(var == my_var & year %in% t2) |> 
      dplyr::pull(raster_name)
    t2_ras = terra::app(terra::subset(raster, t2_lyrs), mean)
    # Calculate Change
    change = t2_ras - t1_ras
  })
  names(change_ras) = lapply(unique(data_sets$var), function(my_var){
    paste(
      paste0(data_type, "-change"), my_var, paste(min(t1), max(t1), sep = "-"), 
      paste(min(t2), max(t2), sep = "-"), sep = "_"
    )
  }) |> unlist()
  return(change_ras)
}


#' Calculate percent annual cover
#' 
#' This function calculates percent annual cover from RAP raster data.
#'
#' @param rasters SpatRaster RAP data 
#'
#' @return a SpatRaster
percent_annual <- function(cover_rast) {
  ac = names(cover_rast)[grepl("^annual", names(cover_rast))]
  pc = names(cover_rast)[grepl("^perennial", names(cover_rast))]
  results_list <- list()
  
  if (length(ac) == length(pc)){
    for (i in 1:length(ac)) {
      results_list[[i]] <- cover_rast[[ac[i]]] / (cover_rast[[ac[i]]] + 
                                                    cover_rast[[pc[i]]])
    }
    results <- rast(results_list)
    names(results) <-  paste0("pct_annual_cover_", rap_yrs)
    return(results)
  } else(message("Annual and perennial raster data do not match."))
}


#' Calculate mean raster value for each raster layer
#'
#' @param raster SpatRaster of RAP data
#' @param label Ecosystem name
#'
#' @return A [tibble::tibble()].
mean_raster_ts <- function(raster, label){
  terra::global(raster, fun = 'mean', na.rm = TRUE) |>
    rownames_to_column(var = "raster_name") |>
    dplyr::mutate(ecosystem = label,
                  type = str_to_title(str_split_i(raster_name, "_", 4)),
                  year = as.numeric(str_split_i(raster_name, "_", 3))) |>
    tibble::tibble()
}


#' Calculate total biomass
#' 
#' This function calculates total biomass from RAP raster data.
#'
#' @param biomass_rast SpatRaster RAP data 
#'
#' @return a SpatRaster
total_biomass <- function(biomass_rast) {
  # biomass_rast = biomass
  ab = names(biomass_rast)[stringr::str_detect(names(biomass_rast), "annual")]
  pb = names(biomass_rast)[stringr::str_detect(names(biomass_rast), "perennial")]
  results_list <- list()
  if (length(ab) == length(pb)){
    for (i in 1:length(ab)) {
      results_list[[i]] <- biomass_rast[[ab[i]]] + biomass_rast[[pb[i]]]
    }
    results <- terra::rast(results_list)
    names(results) <- paste0("total_biomass_", rap_yrs)
    return(results)
  } else(message("Annual and perennial raster data do not match."))
}


# Area of Interest ----
#-- Project coordinate reference system (crs)
target_crs <- "EPSG:4326" # crs of raster data

#-- Read AoI vector data
aoi_bndry <- sf::read_sf(dsn = gdb_path, layer = aoi_fc) |> 
  sf::st_transform(target_crs)
ecosystems <- sf::read_sf(dsn = gdb_path, layer = ecos_diss_fc) |> 
  sf::st_transform(target_crs)

# Subset ecosystems
alpine <- ecosystems[ecosystems$BT_Type == "Alpine", ]
sage <- ecosystems[ecosystems$BT_Type == "Sagebrush", ]

# plot quick map
ggplot() +
  ggspatial::annotation_map_tile(type = "osm", zoom = 8, alpha = 0.3) +
  geom_sf(data = ecosystems, aes(fill = BT_Type), color = "transparent") +
  scale_fill_manual(values = c("#1965B0", "#DC050C")) +
  geom_sf(data = aoi_bndry, fill = "transparent", color = "black") +
  theme_minimal() +
  labs(fill = "Ecosystems")


# Biomass Data ----
biomass <- terra::rast(
  file.path(raster_dir,
            paste0("RAP_biomass_", min(rap_yrs), "_", max(rap_yrs), ".tif"))
  )

## Subset by ecosystem ----
#-- Alpine
bm_alpine <- terra::mask(biomass, alpine)
# Save the raster 
terra::writeRaster(
  terra::rast(bm_alpine),
  filename = file.path(
    raster_dir,
    paste0("RAP_biomass_Alpine_", min(rap_yrs), "_", max(rap_yrs), ".tif")
  ), 
  overwrite = TRUE
)

#-- Sage
bm_sage <- terra::mask(biomass, sage)
# Save the raster 
terra::writeRaster(
  terra::rast(bm_sage),
  filename = file.path(
    raster_dir,
    paste0("RAP_biomass_Sagebrush_", min(rap_yrs), "_", max(rap_yrs), ".tif")
  ), 
  overwrite = TRUE
)

## Summaries ----

### Mean raster ----
# Calculate the mean for the entire stack. Returns a raster with 1 layer.
# mean_biomass <- terra::app(biomass, mean)

# Calculate the mean of each raster. Returns a vector.
mean_bm_alpine <- mean_raster_ts(bm_alpine)
mean_bm_sage <- mean_raster_ts(bm_sage)
mean_cov_ts <- dplyr::bind_rows(mean_bm_alpine, mean_bm_sage)

### Total biomass ----
tot_bm <- total_biomass(biomass)
# Save the raster 
terra::writeRaster(
  terra::rast(tot_bm),
  filename = file.path(
    raster_dir,
    paste0("RAP_total_biomass_", min(rap_yrs), "_", max(rap_yrs), ".tif")
  ), 
  overwrite = TRUE
)

### Change ----
#-- Alpine
bm_alp_chng <- change_by_variable(bm_alpine, 1990:1994, 2020:2024)
# Save the raster 
terra::writeRaster(
  terra::rast(bm_alp_chng),
  filename = file.path(
    raster_dir,
    paste0("RAP_biomass_change_alpine", min(rap_yrs), "_", max(rap_yrs), ".tif")
  ), 
  overwrite = TRUE
)

#-- Sage
bm_sag_chng <- change_by_variable(bm_sage, 1990:1994, 2020:2024)
# Save the raster 
terra::writeRaster(
  terra::rast(bm_sag_chng),
  filename = file.path(
    raster_dir,
    paste0("RAP_biomass_change_sage", min(rap_yrs), "_", max(rap_yrs), ".tif")
  ), 
  overwrite = TRUE
)

# Percent Cover Data ----
cover <- terra::rast(
  file.path(raster_dir,
            paste0("RAP_cover_", min(rap_yrs), "_", max(rap_yrs), ".tif"))
  )

## Subset by ecosystem ----
#-- Alpine
cvr_alpine <- terra::mask(cover, alpine)
# Save the raster 
terra::writeRaster(
  terra::rast(cvr_alpine),
  filename = file.path(
    raster_dir,
    paste0("RAP_cover_Alpine_", min(rap_yrs), "_", max(rap_yrs), ".tif")
  ), 
  overwrite = TRUE
)

# Sage
cvr_sage <- terra::mask(cover, sage)
# Save the raster 
terra::writeRaster(
  terra::rast(cvr_sage),
  filename = file.path(
    raster_dir,
    paste0("RAP_cover_Sagebrush_", min(rap_yrs), "_", max(rap_yrs), ".tif")
  ), 
  overwrite = TRUE
)


## Summaries ----

### Mean raster ----
# Calculates mean of each raster and returns a vector.
mean_cov_alpine <- mean_raster_ts(cvr_alpine)
mean_cov_sage <- mean_raster_ts(cvr_sage)
mean_cov_ts <- dplyr::bind_rows(mean_cov_alpine, mean_cov_sage) |> 
  dplyr::mutate(
    type = ifelse(type == "Annual", "Annual\nForb/Grass", type),
    type = ifelse(type == "Perennial", "Perennial\nForb/Grass", type)
  )

### Change ----
#-- Change in Alpine
cvr_alp_chng <- change_by_variable(cvr_alpine, 1990:1994, 2020:2024)
# Save the raster 
terra::writeRaster(
  terra::rast(cvr_alp_chng),
  filename = file.path(
    raster_dir,
    paste0("RAP_cover_change_alpine", min(rap_yrs), "_", max(rap_yrs), ".tif")
  ), 
  overwrite = TRUE
)

#-- Change in Sage
cvr_sag_chng <- change_by_variable(cvr_sage, 1990:1994, 2020:2024)
# Save the raster 
terra::writeRaster(
  terra::rast(cvr_sag_chng),
  filename = file.path(
    raster_dir,
    paste0("RAP_cover_change_sage", min(rap_yrs), "_", max(rap_yrs), ".tif")
  ), 
  overwrite = TRUE
)

# Save ----
save(aoi_bndry, ecosystems, ecosystems_df, mean_bm_ts, mean_cov_ts,
     file = file.path(proj_dir, "RAP_summary_data.RData"))






#-----------------------------
##### Julia's Code Below #####
#-----------------------------


## Subset specific layers ----
#-- Subset tree cover for 2023:
tree_2023 <- cover[[names(cover)[stringr::str_detect(names(cover), "2023_tree")]]]

#-- Exclude areas of high tree cover
tree_below40 <- terra::clamp(tree_2023, upper = 40, value = FALSE)
low_trees_aoi <- terra::mask(tree_below40, aoi_bndry)

#-- Subset herbaceous cover (annuals and perennials)
# Subset raster data
# herb_cov <- cover[[names(cover)[grepl("^annual|^perennial", names(cover))]]] 

# Calculate percent annual herbaceous cover
# herb_annual <- percent_annual(herb_cov)


#-- Plot change in biomass

# Calculate averages
early <- terra::subset(tot_biomass,
                       names(tot_biomass) == 'total_biomass_1990'|
                         names(tot_biomass) == 'total_biomass_1991'|
                         names(tot_biomass) == 'total_biomass_1992'|
                         names(tot_biomass) == 'total_biomass_1993') |> 
  terra::mean()

late <- terra::subset(tot_biomass,
                      names(tot_biomass) == 'total_biomass_2020'|
                        names(tot_biomass) == 'total_biomass_2021'|
                        names(tot_biomass) == 'total_biomass_2022'|
                        names(tot_biomass) == 'total_biomass_2023') |> 
  terra::mean()

# Calculate change
biomass_change <- late - early 

# Mask to AoI boundary
biomass_change_aoi <- terra::mask(x = biomass_change, mask = aoi_bndry)

# Mask areas with high tree cover
biomass_change_low_trees_aoi <- terra::mask(x = biomass_change_aoi,
                                            mask = low_trees_aoi)

# Calculate carbon in Mg/ha from lbs/acre
dif_MgCha <- (biomass_change_low_trees_aoi * 0.475) * 0.00112 
dif_MgCha_df <- as.data.frame(dif_MgCha)

# Calculate 5 and 95th percentile
perc5_forgraph <- quantile(dif_MgCha_df$mean, probs = 0.05, na.rm = T)
perc95_forgraph <- quantile(dif_MgCha_df$mean, probs = 0.95, na.rm = T)
lowbound <- perc5_forgraph[[1]]
upbound <- perc95_forgraph[[1]]

#-- Map
map <- ggplot()+
    ggspatial::annotation_map_tile(type = "osm", zoom = 8, alpha = 0.3) +
    geom_spatraster(data = dif_MgCha) +
    scale_fill_gradient2(
      low = "#1965B0", mid = "#FFFFFF", high = "#DC050C", na.value = "transparent",
      midpoint = 0, limits = c(lowbound, upbound), oob = scales::squish,
      name = expression("Change (Mg C ha"^-1*")")
    ) +
    geom_spatvector(data = aoi_bndry, fill = "transparent", color = "black") +
    # coord_sf(xlim = c(ext(aoi_bndry)[1], ext(aoi_bndry)[2]),
    #          ylim = c(ext(aoi_bndry)[3], ext(aoi_bndry)[4])) +
    theme_minimal() +
    annotation_scale(location = "br", width_hint = 0.5,
                     pad_x = unit(0.1, "cm"), pad_y = unit(0.1, "cm")) +
    annotation_north_arrow(location = "tr", style = north_arrow_minimal(),
                           height = unit(0.7, "cm"), width = unit(0.7, "cm"),
                           pad_x = unit(0.1, "cm"), pad_y = unit(0.1, "cm"))+
    theme(legend.justification.right = "top")+
    ggtitle(paste0("Aboveground Carbon Change on the ", unit_name),
            subtitle = "2020-2023 average minus 1990-1993 average") +
    xlab("Longitude") +
    ylab("Latitude")

#-- Histogram
freq_dif_MgCha <- terra::freq(dif_MgCha, digits=1)
histogram <- ggplot(filter(freq_dif_MgCha, count>200)) +
    geom_col(aes(x = value, y = count, fill = value), color = "black",
             linewidth = 0.2) +
    scale_fill_gradient2(low = "#1965B0", mid = "#FFFFFF", high = "#DC050C",
                         midpoint = 0, limits = c(lowbound, upbound),
                         oob=scales::squish) +
    #xlab("Change ") +
    theme_void() +
    theme(legend.position = "none")

#-- Final figure
combined <- ggdraw() +
  draw_plot(map, x = 0.01, y = 0.01, width = 0.9, height = 1) +
  draw_plot(histogram, x = 0.65, y = 0.1, width = 0.35, height = 0.4) +
  draw_label("Count \n(pixels)", x = 0.7, y = 0.35, size = 10)

ggplot2::ggsave(filename = "AbovegroundCarbonChange.jpg", device = "jpg", 
                path = fig_dir, width = 6, height = 4, units = "in", 
                dpi = 300)
