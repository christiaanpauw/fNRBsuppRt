
#' Process woody cover change data.
#'
#' Mask, reclassify, aggregate and write woody cover change data from Hansen et al.
#'
#' @param cover_raster a raster of 2000 woody cover for the aoi.
#' @param loss_raster a raster of 2000-2018 woody loss for the aoi. (this should be the output of process_coverchange)
#' @param gain_raster a raster of 2000-2012 woody gain for the aoi. (this should be the output of process_coverchange)
#' @param remote_shape a (sf) polygon of the remote areas. (this should be the output of disaggregate_access)
#' @param aoi_shape a (sf) polygon of the area of interest
#' @param aggregation a vector by which the raster data will be aggregated. Default = 8.
#' @param aoi_prefix a character string of the prefix supplied to write the resulting raster. The default is "aoi_".
#' @param destination a character string of the directory to save the results to. The default is the working directory.
#'
#' @return three raster images (.tif) written to the destination provided.These consist of the 2000 total, 2018 total and 2018 accessible woody cover data.
#'
#' @importFrom magrittr `%>%`
#' @import raster
#' @import sf
#'

process_cover = function(cover_raster, loss_raster, gain_raster, remote_shape, aoi_shape, aggregation = 8,
                         aoi_prefix = "aoi_", destination = getwd()){

  # Aggregate and clean up 2000 woody cover
  aoi_cover_total = cover_raster %>%
    raster::mask(aoi_shape) %>%
    aggregate(fact = aggregation)

  # Calculate the 2018 cover by multiplying the area lost by zero and then adding the cover gain.

  inverse_loss_raster = 1 - loss_raster

  aoi_2018cover_total =  inverse_loss_raster %>%
    prod(aoi_cover_total, na.rm = T) %>%
    overlay(gain_raster*100, fun = function(r1, r2){return(r1+r2)}) %>%
    raster::mask(aoi_shape) %>%
    reclassify(matrix(c(100, Inf, 100), ncol = 3, byrow = T)) # If cover gain resulted in >100% cover (due to aggregations), then this should be adjusted.

  aoi_2018cover_access = raster::mask(aoi_2018cover_total, remote_shape, inverse = T)

  writeRaster(aoi_cover_total, paste0(destination, aoi_prefix, "agg and reclass_2000cover_total.tif"), overwrite = T)
  writeRaster(aoi_2018cover_total, paste0(destination, aoi_prefix, "agg and reclass_2018cover_total.tif"), overwrite = T)
  writeRaster(aoi_2018cover_access, paste0(destination, aoi_prefix, "agg and reclass_2018cover_access.tif"), overwrite = T)

}

