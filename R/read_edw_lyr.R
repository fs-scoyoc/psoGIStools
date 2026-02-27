#' Read spatial data from Forest Service ArcGIS REST Services
#' 
#' This function reads spatial features from the Forest Service ArcGIS REST 
#'     Services. Either the public ArcGIS REST Service, 
#'     https://apps.fs.usda.gov/arcx/rest/services/EDW, or the internal ArcGIS 
#'     REST Service, https://apps.fs.usda.gov/arcn/rest/services/EDW, using the 
#'     `arcgislayers` package. You must be on a Forest Service network to access 
#'     data from the interal ArcGIS REST Service.
#'
#' @param map_name Character. Name of map layer.
#' @param layer Integer. Number of layer to read. Default is  zero (0).
#' @param service Character. The public ("arcx") or internal ("arcn") ArcGIS 
#'     REST Service code. Default is "arcx". You must be on a Forest Service 
#'     network for "arcn" to work.
#' @param crs Coordinate reference system (crs). Default is EPSG:4326 
#'      (WGS 84).
#'
#' @return An [sf] object or [terra::SpatRaster-class].
#' @seealso [arcgislayers::arc_read()], [sf::st_transform()]
#' @export
#' 
#' @examples
#' \dontrun{
#' ## Not run:
#' library("psoGIStools")
#' library("dplyr")
#' 
#' # Administrative Boundary for the Medicine Bow National Forest
#' mbf <- read_edw_lyr("EDW_ForestSystemBoundaries_01") |> 
#'   filter(forestname == "Medicine Bow National Forest")
#' 
#' inv_plant <- read_edw_lyr("EDW_BioInvasivePlant_01", layer = 1, service = "arcn") |> 
#'   clip_sf(mbf)
#' ## Run:
#' }
read_edw_lyr <- function(map_name, layer = 0, service = "arcx", 
                         crs = "EPSG:4326"){
  
  # map_name = "EDW_BioTESP_01"; layer = 1; service = "arcn"
  # map_name = "EDW_ForestSystemBoundaries_01"; layer = 0; service = "arcx"
  
  edw_rest = glue::glue("https://apps.fs.usda.gov/{service}/rest/services/EDW/")
  lyr = arcgislayers::arc_read(
    glue::glue("{edw_rest}/{map_name}/MapServer/{layer}")
    ) |>
    janitor::clean_names() |> 
    sf::st_make_valid() |> 
    sf::st_transform(crs = crs)
  return(lyr)
}