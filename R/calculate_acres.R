#' Calculate acres for a simple feature (sf) polygon object.
#' 
#' This function evaluates if the units of the input polygon are in meters 
#'     before calculating acres. If the units are are not in meters, acres are
#'     not calculated. Then the function will evaluate if geodesic or planar 
#'     acres can be calculated. Geodesic acres are calculated if the input 
#'     polygon is has a geographic coordinate reference system (CRS; e.g., 
#'     WGS84, EPSG:4326). If the input polygon does not have a geographic CRS, 
#'     planar acres will be calculated. 
#'
#' @param sf_poly `sf` polygon object. 
#' @param geodesic Optional. TRUE/FALSE. Calculate geodesic acres if input 
#'     polygon has a geographic CRS. Default is TRUE.
#'
#' @returns An `sf` polygon object.
#' 
#' @export
#' 
#' @seealso [sf::st_area()], [sf::sf_use_s2()]
#' 
#' @examples
#' \dontrun{
#' library("psoGIStools")
#' 
#' dat_sf <- read_fc(lyr_name = "feature_name",
#'                   dsn = file.path("T:/path/to/geodatabase"),
#'                   crs = "EPSG:4326") |> 
#'           calculate_acres()
#' 
#' }
calculate_acres <- function(sf_poly, geodesic = TRUE){
  if(tolower(sf::st_crs(sf_poly)$units_gdal) == "metre"){
    if(sf::st_is_longlat(sf_poly) & geodesic){
      sf::sf_use_s2(TRUE)
      sf_poly$acres = as.numeric(st_area(sf_poly)) / 4046.8564224
      message("Geodesic acres were calcualted")
    } else{
      sf_poly$acres = as.numeric(st_area(sf_poly)) / 4046.8564224
      message(
        "Input object does not have a geographic CRS. Planar acres were calculated."
      )
    }
    return(sf_poly)
  } else{
    message("Input object CRS is not in meters. Acres were not calculated.")
  }
}
