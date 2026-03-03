#' Read USGS National Hydrology Data into R
#' 
#' WARNING. This function is under active development. This function read US 
#'     Fish and Wildlife Service National Wetlands Inventory (NWI) data from 
#'     the USGS Wetlands ArcGIS REST Service, 
#'     https://fwspublicservices.wim.usgs.gov/wetlandsmapservice/rest/, into R 
#'     and clips them to an area of interest.
#'
#' @param aoi_poly `sf` polygon object of area of interest.
#' @param gdb_path Optional. Character. Directory path to geodatabase. Default 
#'     is FALSE. IF FALSE, data will not be saved to a geodatabase.
#' @param suffix Optional. Character. A suffix to add to the layer name when 
#'     writing to a geodatabase(e.g. "PlanArea").
#'
#' @returns A list of [sf] objects: *riparian* and *wetlands*.
#' @export
#'
#' @examples
#' \donturn{
#' library("psoGIStools")
#' library("dplyr")
#' 
#' # Read in forest boundary
#' sjf <- read_edw_lyr("EDW_ForestSystemBoundaries_01") |> 
#'   filter(forestname == "San Juan National Forest")
#' 
#' # Pull NWI data
#' sjf_nhd <- pull_nhd_data(sjf)
#' }
pull_nhd_data <- function(aoi_poly, gdb_path, suffix = FALSE){
  # aoi_poly = read_edw_lyr("EDW_ForestSystemBoundaries_01") |>
  filter(forestname == "San Juan National Forest")
  # gdb_path = file.path("data", "MBF_spp_eval.gdb")

  nhd_path = "https://hydro.nationalmap.gov/arcgis/rest/services/nhd/MapServer"
dat = arcgislayers::arc_read(
  glue::glue("{nhd_path}/7")
) |>
  janitor::clean_names() |>
  sf::st_make_valid() |>
  psoGIStools::clip_fc(aoi_poly)

}