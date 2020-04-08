
#' Process woody cover change data.
#'
#' Mask, reclassify, aggregate and write woody cover change data from Hansen et al.
#'
#' @param change_raster a raster of woody gain/loss for the aoi.
#' @param aoi_shape a (sf) polygon of the area of interest
#' @param aggregation a vector by which the raster data will be aggregated. Default = 8.
#' @param aoi_prefix a character string of the prefix supplied to write the resulting raster. The default is "aoi_".
#' @param var_suffix a character string of the suffix supplied to write the resulting raster.. The default is "_coverchange".
#' @param destination a character string of the directory to save the results to. The default is the working directory.
#'
#' @return a raster image (.tif) written to the destination provided.
#'
#' @importFrom magrittr `%>%`
#' @import raster
#' @import sf
#'

process_coverchange = function(change_raster, aoi_shape, aggregation = 8,
                               aoi_prefix = "aoi_", var_suffix = "_coverchange", destination = getwd()){

  # reclass_0NA = matrix(c(0, NA), ncol = 2, byrow = T)
  reclass_binary = matrix(c(1, Inf, 1), ncol = 3, byrow = T)

  aoi_coverchange_total = change_raster %>%
    raster::mask(aoi_shape) %>%
    reclassify(reclass_binary) %>%
    # reclassify(reclass_0NA) %>%
    aggregate(fact = aggregation)

  writeRaster(aoi_coverchange_total, paste0(destination, aoi_prefix, "agg and reclass", var_suffix, "_total.tif"), overwrite = T)
  # writeRaster(aoi_coverchange_access, paste0(destination, aoi_prefix, "agg and reclass", var_suffix, "_accessible.tif"))

}

