#' Dissolve a simple feature
#' 
#' This function performs a dissolve, just like ArcGIS, by using 
#'     `dplyr::summarise()` and grouping the summary by the variable names 
#'     provided by the use. 
#' 
#' @param input_feature Input spatial feature (`sf` object).
#' @param var_names Vector of variables names to dissolve data by. 
#' 
#' @return An [sf] object.
#' 
#' @export 
#' 
#' @examples
#' \dontrun{
#' library(psoGIStools)
#' 
#' # Read data from Forest Service ArcGIS REST Service
#' fs_lands <- read_edw_lyr("EDW_SurfaceOwnership_01") |>
#'   dplyr::filter(ownerclassification == 'USDA FOREST SERVICE')
#' 
#' # Use `dissolve_sf()` to dissolve by region and unit name. 
#' fs_units <- dissolve_sf(input_feature = fs_lands, 
#'                         var_names = c("region", "nfslandunitname"))
#' }
dissolve_sf <- function(input_feature, var_names){
  dat_sf = input_feature[1:1000, ] |> 
    # dplyr::group_by(dplyr::all_of(var_names)) |> 
    dplyr::summarise(geometry = sf::st_union(geometry), 
                     .by = dplyr::all_of(var_names)) |> 
    sf::st_as_sf()
  return(dat_sf)
}
