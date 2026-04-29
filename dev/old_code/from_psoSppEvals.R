# Deprecated Functions ----

#' **Deprecated**. Clip feature class to polygon
#' 
#' This function is not maintained, but remains here for now. Use the 
#'     `psoGIStools` package. This function clips a `sf` object using
#'     `sf::st_intersection()`. First, this function checks that the coordinate
#'     reference system (CRS) of the input object is the same as the clipping
#'     object. If it is not, this function transforms the clipping object to CRS
#'     of the input object using `sf::st_transform()`before clipping. The output
#'     CRS is not changed.
#'
#' @param sf_lyr  Spatial (`sf`) object to be clipped.
#' @param sf_clip Polygon (`sf`) object used to clip.
#' @param locale  Optional. Short description of clipped layer, usually the 
#'                    location (e.g., forest acronym or "Buffer").
#'
#' @return An [sf] object
#' @seealso [sf::st_intersection()], [sf::st_transform()]
#' @export
#' 
#' @examples
#' \dontrun{
#' library("psoSppEvals")
#' 
#' # Read spatial data into R
#' t_path <- file.path("T:/path/to/project/directory")
#' gdb_path <- file.path(t_path, "GIS_Data.gdb")
#' sf_plan_area <- read_fc(lyr = "PlanArea", dsn = gdb_path, crs = "NAD83")
#' 
#' # Pull data from existing GBIF query
#' gbif_dat <- get_gbif(gbif_key = '9999999-999999999999999', 
#'                      t_path = file.path(t_path, "data"))
#' 
#' # Convert to spatial object
#' gbif_sf <- gbif_spatial(gbif_dat, "NAD83")
#' 
#' # Clip to extents
#' unit_gbif <- clip_fc(gbif_sf, sf_plan_area)
#' }
clip_fc <- function(sf_lyr, sf_clip, locale = NULL){
  
  # Transform clipping layer
  if(sf::st_crs(sf_lyr) != sf::st_crs(sf_clip)){
    sf_clip = sf::st_transform(sf_clip, crs = sf::st_crs(sf_lyr))
  }
  
  # Clip input layer
  sf_lyr = sf::st_intersection(sf_lyr, sf_clip) |> 
    dplyr::select(-tidyselect::any_of(colnames(sf_clip)))
  
  # Add locale
  if(!is.null(locale)){
    sf_lyr = dplyr::mutate(sf_lyr, locale = locale)
  }
  
  return(sf_lyr)
}


#' **Deprecated**. Read spatial data from Forest Service ArcGIS REST Services
#' 
#' This function is not maintained, but remains here for now. Use the 
#'     `psoGIStools` package. This function reads spatial features from the 
#'     Forest Service ArcGIS REST Services. Either the public ArcGIS REST 
#'     Service, https://apps.fs.usda.gov/arcx/rest/services/EDW, or the internal 
#'     ArcGIS REST Service, https://apps.fs.usda.gov/arcn/rest/services/EDW, 
#'     using the `arcgislayers` package. You must be on a Forest Service network 
#'     to access data from the internal ArcGIS REST Service.
#'
#' @param map_name Character. Name of map layer.
#' @param layer Integer. Number of layer to read. Default is  zero (0).
#' @param service Character. The public ("arcx") or internal ("arcn") ArcGIS 
#'     REST Service code. Default is "arcx". You must be on a Forest Service 
#'     network for "arcn" to work.
#' @param target_crs Coordinate reference system (crs). Default is EPSG:4326 
#'      (WGS 84).
#'
#' @return An [sf] object or [terra::SpatRaster-class].
#' @seealso [arcgislayers::arc_read()], [sf::st_transform()]
#' @export
#' 
#' @examples
#' library(psoSppEvals)
#' 
#' # Administrative Boundary for the Dixie National Forest
#' admin_bndry <- read_edw_lyr("EDW_ForestSystemBoundaries_01", layer = 1) |> 
#'   dplyr::filter(forestname == "Dixie National Forest")
read_edw_lyr <- function(map_name, layer = 0, service = "arcx", 
                         target_crs = "EPSG:4326"){
  
  # map_name = "EDW_BioTESP_01"; layer = 1; service = "arcn"
  # map_name = "EDW_ForestSystemBoundaries_01"; layer = 0; service = "arcx"
  
  edw_rest <- glue::glue("https://apps.fs.usda.gov/{service}/rest/services/EDW/")
  lyr = arcgislayers::arc_read(
    glue::glue("{edw_rest}/{map_name}/MapServer/{layer}")
  ) |>
    janitor::clean_names() |> 
    sf::st_make_valid() |> 
    sf::st_transform(crs = target_crs)
  return(lyr)
}


#' **Deprecated**. Read feature class into R.
#' 
#' This function is not maintained, but remains here for now. Use the 
#'     `psoGIStools` package. This function uses the `sf` package to read a 
#'     feature class into R from a geodatabase (*.gdb) using the `sf::read_sf()` 
#'     function. It then checks that the feature class is in the target 
#'     coordinate reference system (CRS) and will transform the feature to the 
#'     target CRS if it is not.
#'
#' @param lyr Feature class name.
#' @param dsn Path to geodatabase that holds `lyr`.
#' @param target_crs Target coordinate reference system (CRS). Either and 
#'   `sf::st_crs()` object or accepted input string for `sf::st_crs()` (e.g. 
#'   "WGS84" or "NAD83"). See [sf::st_crs()] for more details. Default is NULL. 
#'   If NULL, resulting [sf] object will not be transformed.
#'
#' @return sf object
#' @seealso [sf::read_sf()], [sf::st_crs()]
#' @export
#' 
#' @examples
#' \dontrun{
#' library("psoSppEvals")
#' 
#' read_fc(lyr = "feature_name", dsn = file.path("T:/path/to/geodatabase"), 
#'         crs = "NAD83")
#' }
read_fc <- function(lyr, dsn, target_crs = NULL){
  fc = sf::read_sf(layer = lyr, dsn = dsn) |> sf::st_make_valid()
  if(!is.null(target_crs)){fc = sf::st_transform(fc, crs = target_crs)}
  return(fc)
}

