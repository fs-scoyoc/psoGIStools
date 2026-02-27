#' Clip feature class to polygon
#'
#' This function clips a `sf` object using `sf::st_intersection()`. First, this
#'   function checks that the coordinate reference system (CRS) of the input
#'   object is the same as the clipping object. If it is not, this function
#'   transforms the clipping object to CRS of the input object using
#'   `sf::st_transform()`before clipping. The output CRS is not changed.
#'
#' @param input_feature Input spatial feature (`sf` object) to be clipped.
#' @param clip_feature Polygon feature (`sf`) used to clip `input_feature`.
#'
#' @return An [sf] object.
#' @seealso [read_fc()], [sf::st_intersection()], [sf::st_transform()]
#' @export
#'
#' @examples
#' \dontrun{
#' ## Not run:
#' library("psoGIStools")
#' library("dplyr")
#'
#' #-- Read spatial data into R
#' # Fishlake National Forest Adminstrative Boundary
#' fif <- read_edw_lyr("EDW_ForestSystemBoundaries_01") |>
#'   filter(forestname == "Fishlake National Forest")
#' # Recreation Sites
#' rec_sites <- read_edw_lyr("EDW_InfraRecreationSites_01")
#' 
#' #-- Clip rec sites to forest boundary
#' fif_rec_sties <- clip_sf(rec_sites, fif)
#' ## Run:
#' }
clip_sf <- function(input_feature, clip_feature){

  # Transform clipping layer
  if(sf::st_crs(input_feature) != sf::st_crs(clip_feature)){
    clip_feature = sf::st_transform(clip_feature,
                                    crs = sf::st_crs(input_feature))
  }

  # Clip input layer
  sf_lyr = sf::st_intersection(input_feature, clip_feature) |>
    dplyr::select(-tidyselect::any_of(colnames(clip_feature)))

  return(sf_lyr)
}



