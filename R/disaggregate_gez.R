
#' Dissaggregate woody cover by Global Ecological ZOne
#'
#' Disaggregate the woody cover data of a given raster by the Global Ecological Zones for the same extent.
#'
#' @param rast a raster of the woody cover in the area of interest.
#' @param poly_gez a simple features (sf) polygon of the GEZ. This should be projected to the relevant CRS of the raster.
#' @param scaling a vector by which the raster data will be scaled. If the raster has the woody cover as a percentage (0-100), then scaling = 1 (default). If the raster has decimals (0-1), then scaling = 100.
#'
#' @return a data frame of the total woody area per GEZ.
#'
#' @importFrom magrittr `%>%`
#' @importFrom dplyr filter
#' @import raster
#' @import sf
#'

disaggregate_gez = function(rast, poly_gez, scaling = 1) {

  aoi_zones = poly_gez$gez_name
  # aoi_zones = aoi_zones[aoi_zones != "Water"]

  aoi_cell_size = res(rast) %>% prod()

  aoi_output = data.frame(gez_name = aoi_zones,
                      Cover_m2 = NA,
                      Area_m2 = NA,
                      Total_m2 = NA)

  for(i in 1:length(aoi_zones)) {
    aoi_zone_single = poly_gez %>%
      dplyr::filter(gez_name == aoi_zones[i])

    # Cover
    aoi_cover_p = raster::mask(rast, aoi_zone_single) %>%
      getValues()

    aoi_output[i,2] = sum((aoi_cover_p[!is.na(aoi_cover_p)]*scaling)*aoi_cell_size, na.rm = T)

    # Area
    aoi_area_each = aoi_cover_p

    aoi_output[i,3] = length(aoi_area_each[!is.na(aoi_area_each)])*aoi_cell_size

    # Total area
    aoi_output[i,4] = st_area(aoi_zone_single) %>%
      as.numeric()
  }

  aoi_output
  return(aoi_output)
}
