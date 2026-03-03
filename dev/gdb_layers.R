#' ---
#' title: "Manifest of layers in a geodatabase"
#' author:
#'   - name: "Matthew Van Scoyoc" 
#'     affiliation: |
#'       | Mountain Planning Service Group, Regions 1-4
#'       | Forest Service, USDA
#' date: 13 November, 2024
#' 
#' This script creates an Excel workbook that is a manifest of data housed in a 
#'     geodatabase. The `read_layers` function lists the layers in a geodatabase
#'     and returns a data frame that includes the feature name, geometry type, 
#'     the number of features, the number of fields in the attribute table, and 
#'     the coordinate reference system. This data frame is then written to an 
#'     Excel workbook in the project directory (`proj_dir`).
#' -----------------------------------------------------------------------------


# Project Variables ----
# Note: Change the "proj_dir" and "gdg" variables and run the rest of the script.
# Hint: After changing "proj_dir" and "gdb" variables, put your cursor on the 
#     line 1 and then hit Ctrl+E to run the script.

#-- Project directory path
# Change forward slashes (\) to back slashes (/) if you copy/paste paths from 
#     Windows Explorer.
# Example:
# proj_dir <- file.path("T:/FS/NFS/PSO/MPSG/2025_BitterrootNF/1_PreAssessment/Data")
proj_dir <- file.path("T:/FS/NFS/PSO/MPSG/path/to/folder")

#-- Geodatabase name
# Example:
# gdb <- "BRF_PreAssessmentData.gdb"
gdb <- "gdb_name.gdb"

# The rest of the script should run and write an Excel workbook to the project
#     directory.

# Packages ----
# R packages used in this script
pkgs <- c("sf",       # Spatial vector data tools
          "dplyr",    # Data management tools
          "tibble",   # Data management tools 
          "openxlsx") # Writes Excel files
# Install packages if they aren't in your library
inst_pkgs <- pkgs %in% rownames(installed.packages())
if (any(inst_pkgs == FALSE)) {
  install.packages(pkgs[!inst_pkgs], 
                   lib =  .libPaths()[1], 
                   repos = "https://cloud.r-project.org",
                   type = 'source', 
                   dependencies = TRUE, 
                   quiet = TRUE)
}
# Load packages
invisible(lapply(pkgs, library, character.only = TRUE))

# Functions ----
#' Read layers from a geodatabase.
#' This function reads the simple feature layers in a geodatabase and returns a 
#'     data frame with the feature name, geometry type, the number of features, 
#'     the number of fields in the attribute table, and the coordinate reference
#'     system.
#'
#' @param gdb Character. The name of a geodatabase.
#'
#' @return data frame
read_layers <- function(gdb_path){
  lyrs = sf::st_layers(gdb_path) |> 
    dplyr::select(-driver) |> 
    tibble::tibble()
  crs = unlist(lyrs$crs)
  crs = crs[names(crs) == "input"]
  lyrs$crs = crs
  return(lyrs)
}


# Create and save Excel workbook ----
my_wb <- openxlsx::createWorkbook()
sheet <- gsub(".gdb", "", gdb)
openxlsx::addWorksheet(wb = my_wb, sheetName = sheet)
openxlsx::writeData(wb = my_wb, sheet = sheet, 
                    x = read_layers(file.path(proj_dir, gdb)),
                    colNames = TRUE, rowNames = FALSE, keepNA = TRUE)
excel_file <- paste0(gsub("-", "", Sys.Date()), "_", sheet, "_Manifest.xlsx")
openxlsx::saveWorkbook(my_wb, file = file.path(proj_dir, excel_file),
                       overwrite = TRUE)

