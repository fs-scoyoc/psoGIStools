#' Clip feature class to polygon
#'
#' This function clips a `sf` object using `sf::st_intersection()`. First, this
#'   function checks that the coordinate reference system (CRS) of the input
#'   object is the same as the clipping object. If it is not, this function
#'   transforms the clipping object to CRS of the input object using
#'   `sf::st_transform()`before clipping. The output CRS is not changed.
#'
#' @param input_feature Spatial feature (`sf` object) to be clipped.
#' @param clip_feature Polygon feature (`sf`) used to clip `input_feature`.
#'
#' @return An [sf] object
#' @seealso [mpsgGIStools::read_fc()], [sf::st_intersection()],
#'     [sf::st_transform()]
#' @export
#'
#' @examples
#' library("GIStools")
#'
#' # Read spatial data into R
#' t_path <- file.path("T:/path/to/project/directory")
#' gdb_path <- file.path(t_path, "GIS_Data.gdb")
#' plan_area <- read_fc(lyr_name = "PlanArea", dsn = gdb_path, crs = "NAD83")
#' roads <- read_fc(lyr_name = "AreaRoads", dsn = gdb_path, crs = "NAD83")
#'
#' # Clip to extents
#' plan_area_roads <- clip_fc(roads, roads)
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



