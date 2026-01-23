#' Read spatial data from Forest Service ArcGIS REST Services
#'
#' read_edw_lyr reads features from the Forest Service ArcGIS REST Services
#'     Directory, https://apps.fs.usda.gov/arcx/rest/services/EDW, using the
#'     `arcgislayers` package.
#'
#'
#' @param map_name Character. Name of map layer.
#' @param layer Integer. Number of layer to read. Default is  zero (0).
#' @param crs Coordinate reference system (crs). Default is EPSG:4326 (WGS 84).
#'
#' @return An [sf] object or [terra::SpatRaster-class].
#' @seealso [arcgislayers::arc_read()], [sf::st_transform()]
#' @export
#'
#' @examples
#' library("GIStools")
#'
#' # Administrative Boundary for the Dixie National Forest
#' admin_bndry <- read_edw_lyr("EDW_ForestSystemBoundaries_01", layer = 1) |>
#'   dplyr::filter(forestname == "Dixie National Forest")
read_edw_lyr <- function(map_name, layer = 0, crs = "EPSG:4326"){
  # map_name = "EDW_ForestSystemBoundaries_01"
  # layer = 1
  edw_rest <- "https://apps.fs.usda.gov/arcx/rest/services/EDW/"
  lyr = arcgislayers::arc_read(
    glue::glue(edw_rest, "{map_name}/MapServer/{layer}")
  ) |>
    janitor::clean_names() |>
    sf::st_make_valid() |>
    sf::st_transform(crs)
  return(lyr)
}
