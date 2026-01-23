#' Download LANDFIRE Existing Vegetation Type (250EVT)
#' 
#' This function downloads and saves LANDFIRE Existing Vegetation Type (250EVT) 
#'     raster data using the `rlandfire` package and summarizes the data by EVT 
#'     Name and EVT Physiognomy for the area of interest.
#'
#' @param aoi_polygon Area of interest `sf` polygon object.
#' @param lf_dir Directory path to save raster to.
#' @param email_address Email address. Passesd on to [rlandfire::landfireAPIv2()].
#' @param res Raster resolution. Default is 30.
#'
#' @returns A list of two data frames summarizing area of EVT.
#' @export
#'
#' @examples
#' library("GIStools")
#'
#' # Read spatial data into R
#' t_path <- file.path("T:/path/to/project/directory")
#' gdb_path <- file.path(t_path, "GIS_Data.gdb")
#' plan_area <- read_fc(lyr_name = "PlanArea", dsn = gdb_path)
#' 
#' # Get LANDFIRE EVT data
#' lf_plan_area <- get_landfire(plan_area, file.path("data", "landfire"), 
#'                              "your.name@usda.gov")
get_landfire_evt <- function(aoi_polygon, lf_dir, email_address, res = 30){
  # aoi_polygon = targets::tar_read(plan_area)
  # lf_dir = file.path("data", "LANDFIRE")
  # email_address = Sys.getenv("GBIF_EMAIL")

  # Create directory if it does not exist
  if(!dir.exists(lf_dir)) dir.create(lf_dir)
  # Transform AoA to WGS 84
  aoa_sf = sf::st_buffer(aoi_polygon, 1000) |> 
    sf::st_transform(crs = "epsg:4326")
  # Generate AoA wkt string
  lf_aoi = rlandfire::getAOI(aoa_sf)
  # Pull EVT data from LANDFIRE API
  resp = rlandfire::landfireAPIv2(products = "250EVT", aoi = lf_aoi,
                                  email_address, resolution = res,
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
  # Summarize masked raster and join attribut table
  evt_data = terra::extract(lf_aoa, aoa_sf) |> 
    janitor::clean_names() |> 
    dplyr::group_by(evt_name) |> 
    dplyr::summarise(count = dplyr::n()) |> 
    dplyr::left_join(attr_table, by = "evt_name") |> 
    dplyr::mutate(area_m2 = count * prod(terra::res(lf_aoa)), 
                  acres = area_m2 / 4046.86, 
                  pct_area = (area_m2 / sum(plan_area_proj$area_m2) * 100), 
                  .groups = 'drop')
  phys_data = evt_data |> 
    dplyr::group_by(evt_phys, evt_gp_n, evt_sbcls, evt_name) |> 
    dplyr::summarise(count = sum(count), 
                     area_m2 = sum(area_m2), 
                     acres = sum(acres), 
                     pct_area = (area_m2 / sum(plan_area_proj$area_m2) * 100), 
                     .groups = 'drop')
  # Return summarized data
  return(list("EVT" = evt_data, "PHYS" = phys_data))
}


