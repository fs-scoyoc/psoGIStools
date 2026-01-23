# GIStools R Package

## Overview

Welcome to the Mountain Planning Service Group (MPSG; USDA Forest Service) 
GIS Tools R package.
This package has been developed by MPSG staff to standardize common GIS tasks 
used by MPSG GIS staff.

**Author:** [Matt Van Scoyoc](https://github.com/fs-scoyoc)

**Maintainer:** [Matt Van Scoyoc](https://github.com/fs-scoyoc)

**Version:** 0.0.1

**License:** Mit + file [LICENSE](https://github.com/fs-scoyoc/gis_tools/blob/main/LICENSE.md)

**Depends:** R (\>= 4.1.0)

**Imports:** arcgislayers, dplyr, glue, janitor, sf, tidyselect
    
**Issues:** This package is under active development and changes often with out 
warning. Functions may experience breaking changes at any time. If you find a 
bug or have an idea for a feature, please submit an Issue at 
<https://github.com/fs-scoyoc/gis_tools/issues>.

**Documentation:** Just the man pages for now.

## Installation

``` r
devtools::install_github("fs-scoyoc/gis_tools")
```

## List of Functions

-   `clip_sf()`: transform a `sf` object and clip it using `sf::st_intersection()`.

-   `read_edw_lyr`: read an ArcGIS Enterprise Data Warehouse layer into R as an `sf` object.

-   `read_fc`: read a file geodatabase feature class into R as an `sf` object.

