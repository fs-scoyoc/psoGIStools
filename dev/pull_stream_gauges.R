# devtools::install_github("fs-scoyoc/psoGIStools")
library(dplyr)
library(dataRetrieval)
library(here)
library(psoGIStools)
library(openxlsx)
library(sf)



#' Pull Stream Gauge Locations for a National Forest
#' 
#' This function uses the `dataRetrieval` package to acquire stream gauge 
#'     locations on a specified National Forest or Grassland. The returning data 
#'     set is an `sf` object. There are options to save the results to a 
#'     geodatabase and/or and Excel workbook.
#'
#' @param forest_name Character. Name of National Forest or Grassland of 
#'     interest.
#' @param buffer Logical (TRUE/FALSE). Use a 3-mile buffer to query stream 
#'     gauges. Default is TRUE. If FALSE, results will be clipped to the 
#'     administrative boundary.
#' @param gdb_path Character. Path to geodatabase. Default is FALSE. If FALSE, 
#'     data are not written to a geodatabase.
#' @param xlsx_path Character. Path to folder where you want an Excel workbook
#'     saved. Default if FALSE. if FALSE, an Excel workbook is not written
#'
#' @returns An `sf` object.
#' @export
#'
#' @examples
#' \dontrun{
#' library(psoGIStools)
#' 
#' pull_stream_gauges("Lolo National Forest")
#' }
pull_stream_gauges <- function(forest_name, buffer = TRUE, gdb_path = FALSE, 
                               xlsx_path = FALSE){
  # forest_name = "Lolo National Forest"
  
  # Get Forest Boundary
  nfs_bdy = psoGIStools::read_edw_lyr("EDW_ForestSystemBoundaries_01") |> 
    dplyr::filter(forestname == forest_name)
  fs_land = psoGIStools::read_edw_lyr("EDW_BasicOwnershipPADUS_01", service = "arcn") |> 
    psoGIStools::clip_sf(nfs_bdy) |> 
    dplyr::filter()
  # Buffer FS Boundary
  aoa = sf::st_buffer(nfs_bdy, dist = units::as_units(3, "mi")) |> sf::st_bbox()
  # Pull stream gauges and clip to forest boundary
  gauges = if (buffer){
    dataRetrieval::read_waterdata_monitoring_location(bbox = aoa)
    } else {
      dataRetrieval::read_waterdata_monitoring_location(bbox = aoa) |>
        psoGIStools::clip_sf(nfs_bdy)
    }
  # Clip gauges to FS land
  fs_gauges = psoGIStools::clip_sf(gauges, fs_land)
  # Make final data set
  gauges = gauges |> 
    dplyr::select(monitoring_location_id, monitoring_location_number, 
                  monitoring_location_name, state_name, site_type, 
                  contributing_drainage_area, construction_date, agency_code, 
                  hydrologic_unit_code, basin_code, aquifer_code) |> 
    dplyr::mutate(
      locataion = ifelse(gauges$monitoring_location_id %in% 
                           gauges$monitoring_location_id,
                         "FS", "Non-FS")
      )
  
  # Save
  if (!isFALSE(gdb_path)){
    sf::write_sf(gauges, 
                 layer = paste0("stream_gauges_", gsub("-", "", Sys.Date())), 
                 dsn = gdb_path)
  }
  if (!isFALSE(xlsx_path)){
    writexl::write_xlsx(
      list('Stream Gauges' = sf::st_drop_geometry(gauges)),
      file.path(xlsx_path, 
                paste0("stream_gauges_", gsub("-", "", Sys.Date()), ".xlsx"))
      )
  }
}


# nfs_names <- psoGIStools::read_edw_lyr("EDW_ForestSystemBoundaries_01") |> 
#   sf::st_drop_geometry() |> 
#   dplyr::select(region, forestname) |> 
#   dplyr::distinct() |> 
#   dplyr::arrange(region, forestname)

# read boundary from Forest Service EDW REST services
nfs_bdy <- read_edw_lyr(map_name = "EDW_ForestSystemBoundaries_01") |> 
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

