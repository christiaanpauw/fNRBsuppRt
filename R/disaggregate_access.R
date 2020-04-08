

#' Dissaggregate accessible - and remote or protected - areas
#'
#' Disaggregate a given area of interest (aoi), such as a national or regional project area polygon, into accessible and remote areas, according to the defined buffer distance from the roads and the protected areas. The results are saved to the directory provided, rather than loading it into the workspace.
#'
#' @param aoi_border a simple features (sf) polygon of the area of interest border or boundary. This should be projected to the relevant CRS of the aoi.
#' @param roads_projected a simple features (sf) lines of the roads in the area of interest. This should be projected to the relevant CRS of the aoi.
#' @param PAs_union a simple features (sf) polygon of the protected areas in the area of interest, not as a multipolygon, but as a single, unified shape. This should be projected to the relevant CRS of the aoi.
#' @param buffer_dist a vector of the buffer distance from the road that should be considered accessible in the projection units. The default is 2.5 km from the road (in m).
#' @param aoi_prefix a character string of the prefix supplied to write the resulting sf objects. The default is "aoi_".
#' @param destination a character string of the directory to save the results to. The default is the working directory.
#'
#' @return two shapefiles written to the destination provided.
#'
#' @importFrom magrittr `%>%`
#' @import raster
#' @import sf
#'

disaggregate_access = function(aoi_border, roads_projected, PAs_union,
                               buffer_dist = 2500, aoi_prefix = "aoi_", destination = getwd()) {

  lonlat_crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

  ## Create buffer
  aoi_road_buff = roads_projected %>%
    st_union() %>%
    st_buffer(dist = buffer_dist)

  ## Define remote and accessible areas
  aoi_remote = st_difference(aoi_border, aoi_road_buff) %>%
    st_union(PAs_union)
  aoi_accessible = st_difference(aoi_border, aoi_remote)
  aoi_accessible_trim = st_intersection(aoi_border, aoi_accessible)

  aoi_remote_ll = st_transform(aoi_remote, lonlat_crs)
  aoi_access_ll = st_transform(aoi_accessible_trim, lonlat_crs)

  ##
  st_write(aoi_remote_ll, paste0(destination, aoi_prefix, "remote.shp"), delete_layer = T)
  st_write(aoi_access_ll, paste0(destination, aoi_prefix, "accessible.shp"), delete_layer = T)

}

