# devtools::install_github("fs-scoyoc/psoGIStools")
library(dplyr)
library(dataRetrieval)
library(here)
library(psoGIStools)
library(openxlsx)
library(sf)


nfs_names <- psoGIStools::read_edw_lyr("EDW_ForestSystemBoundaries_01", 0) |> 
  sf::st_drop_geometry() |> 
  dplyr::select(region, forestname) |> 
  dplyr::distinct() |> 
  dplyr::arrange(region, forestname)

pull_stream_gauges <- function(forest_name, gdb_path = FALSE, xlsx = FALSE){
  # forest_name = "Lolo National Forest"
  
  # Get Forest Boundary
  nfs_bdy = psoGIStools::read_edw_lyr("EDW_ForestSystemBoundaries_01", 0) |> 
    dplyr::filter(forestname == forest_name) |> 
    sf::st_make_valid()
  aoa = sf::st_buffer(nfs_bdy, dist = units::as_units(3, "mi")) |> sf::st_bbox()
  # Pull stream gauges and clip to forest boundary
  gauges = dataRetrieval::read_waterdata_monitoring_location(bbox = aoa)
  
  
}


# read boundary from Forest Service EDW REST services
nfs_bdy <- read_edw_lyr(map_name = "EDW_ForestSystemBoundaries_01",
                        layer = 0) |> 
  dplyr::filter(forestname == "Lolo National Forest")

# pull all USGS stream gauge locations within bounding box of NFS boundary
stream_gauges <- read_waterdata_monitoring_location(bbox = sf::st_bbox(nfs_bdy))

# clip stream gauges to NFS boundary
stream_gauges_nfs <- clip_sf(stream_gauges, nfs_bdy) |> 
  dplyr::select(monitoring_location_id, monitoring_location_number, 
                monitoring_location_name, state_name, site_type, 
                contributing_drainage_area, construction_date, agency_code, 
                hydrologic_unit_code, basin_code, aquifer_code)

# write to Excel
f_name = paste0(gsub("-", "", Sys.Date()), "_", 
                gsub(" ", "", nfs_bdy$forestname), 
                "_StreamGauges.xlsx")
writexl::write_xlsx(
  list("StreamGauges" = sf::st_drop_geometry(stream_gauges_nfs)),
  here::here(f_name)
  )

