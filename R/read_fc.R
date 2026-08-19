# This script contains two functions:
#  -   read_fc()
#  -   view_layers() 


#' Read feature class into R.
#'
#' This function uses the `sf` package to read a feature class into R from a
#'   geodatabase (*.gdb) using the `sf::read_sf()` function. It then checks that
#'   the feature class is in the target coordinate reference system (CRS) and
#'   will transform the feature to the target CRS if it is not.
#'
#' @param lyr_name Feature class name.
#' @param dsn Path to geodatabase that holds `lyr_name`.
#' @param crs Target coordinate reference system (CRS). Either and
#'   `sf::st_crs()` object or accepted input string for `sf::st_crs()` (e.g.
#'   "WGS84" or "NAD83"). See [sf::st_crs()] for more details. Default is NULL.
#'   If NULL, resulting [sf] object will not be transformed.
#'
#' @return `sf` object
#' @seealso [sf::read_sf()], [sf::st_transform()]
#' @export
#'
#' @examples
#' \dontrun{
#' library("psoGIStools")
#'
#' dat_sf <- read_fc(lyr_name = "feature_name",
#'                   dsn = file.path("T:/path/to/geodatabase"),
#'                   crs = "NAD83")
#' }
read_fc <- function(lyr_name, dsn, crs = NULL){
  fc = sf::read_sf(layer = lyr_name, dsn = dsn) |> 
    sf::st_make_valid() |> 
    janitor::clean_names()
  if(!is.null(crs)){fc = sf::st_transform(fc, crs = crs)}
  return(fc)
}


#' View layers in a geodatabase
#' 
#' This function retuns a vecor of layer or feature class names in a geodatabase.
#' 
#' @param dsn Path to geodatabase.
#' 
#' @return A vector of layer or feature class names.
#' 
#' @export 
#' 
#' @examples
#' \dontrun{
#' gdb <- file.path("data", "MBF_SppEvals.gdb")
#' view_layers(gdb)
#' }
view_layers <- function(dsn) {
  sf::st_layers(dsn) |> dplyr::pull(name) |> sort() |> suppressWarnings()
}
