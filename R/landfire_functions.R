#' Download LANDFIRE Existing Vegetation Type (250EVT)
#' 
#' This function downloads and saves LANDFIRE raster data using the `rlandfire` 
#'     package and the raster data masked to an area of interest polygon. NOTE: 
#'     This function has not been tested for more than one LANDFIRE product.
#'
#' @param aoi_polygon Area of interest `sf` polygon object.
#' @param lf_dir Directory path to save raster to.
#' @param email Email address. Passed on to [rlandfire::landfireAPIv2()].
#' @param lf_products Vector of LANDFIRE products. Defult is `250EVT`. See 
#'     <https://lfps.usgs.gov/products> for available products. 
#' @param res Raster resolution. Default is 30.
#'
#' @returns A list containing 1) the masked raster and 2) the attribute table 
#'     for the masked raster.
#' @export
#'
#' @examples
#' ## Not run:
#' library("psoGIStools")
#' library("dplyr")
#'
#' #-- Read spatial data into R
#' # Fishlake National Forest Adminstrative Boundary
#' dif <- read_edw_lyr("EDW_ForestSystemBoundaries_01") |>
#'   filter(forestname == "Dixie National Forest")
#' 
#' # Get LANDFIRE EVT data
#' evt_dif <- get_landfire(dif, lf_dir = file.path("data", "landfire"), 
#'                         email = "your.name@usda.gov")
#' ## Run:
get_landfire <- function(aoi_polygon, lf_dir, email, lf_products = "250EVT", 
                         res = 30){
  # aoi_polygon = targets::tar_read(plan_area)
  # lf_dir = file.path("data", "LANDFIRE")
  # email = Sys.getenv("GBIF_EMAIL")
  
  # Create directory if it does not exist
  if(!dir.exists(lf_dir)) dir.create(lf_dir)
  # Transform AoA to WGS 84
  aoa_sf = sf::st_buffer(aoi_polygon, 1000) |> 
    sf::st_transform(crs = "epsg:4326")
  # Generate AoA wkt string
  lf_aoi = rlandfire::getAOI(aoa_sf)
  # Pull EVT data from LANDFIRE API
  resp = rlandfire::landfireAPIv2(products = lf_products, aoi = lf_aoi,
                                  email, resolution = res,
                                  path = tempfile(fileext = ".zip"),
                                  method = 'auto', verbose = FALSE)
  # Unzip raster and save to lf_dir
  utils::unzip(resp$path, exdir = lf_dir)
  # Read raster into R
  lf = terra::rast(
    list.files(lf_dir, pattern = ".tif$", full.names = TRUE, recursive = TRUE)
  )
  # Transform AoA to raster CRS
  plan_area_proj = terra::vect(aoi_polygon) |> terra::project(terra::crs(lf))
  plan_area_proj$area_m2 = terra::expanse(plan_area_proj, unit = "m")
  plan_area_proj$acres = plan_area_proj$area_m2 / 4046.86
  
  # Mask raster to AoA
  lf_aoa = terra::mask(lf, plan_area_proj)
  # Save raster
  # terra::writeRaster(
  #   x = lf_aoa,
  #   filename = file.path(lf_dir, "LANDFIRE_250EVT_AdminBndry.tif"),
  #   overwrite = TRUE
  # )
  # Read attribute table into R
  attr_table = foreign::read.dbf(
    list.files(lf_dir, pattern = ".dbf$", full.names = TRUE, recursive = TRUE)
  ) |> 
    janitor::clean_names() |> 
    dplyr::select(-count, -lfrdb)
  # Return summarized data
  return(list("lf_raster" = lf_aoa, "attribute_table" = attr_table))
}


