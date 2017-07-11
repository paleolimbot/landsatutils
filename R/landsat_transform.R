
#' Propogate Landsat attributes through transformations
#'
#'
#'
#' @param scene
#' @param fun
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
landsat_transform <- function(scene, fun, ...) {
  # this is a base method that applies a function to a landsat_scene and propogates
  # the landsat_attrs

  # make sure input is a landsat scene
  if(!is.landsat_scene(scene)) stop("Use landsat_scene() to create a landsat scene")

  # apply method
  transformed <- fun(scene, ...)

  # propogate attributes
  attr(transformed, "landsat_attrs") <- attr(scene, "landsat_attrs")

  # return transformed version
  transformed
}

landsat_crop <- function(scene, boundary) {
  # make sure boundary is in the same crs as the scene object
  boundary_proj <- sp::spTransform(boundary, scene@crs)

  # use landsat_transform to crop and propogate attributres
  landsat_transform(scene, raster::crop, boundary_proj)
}

landsat_mask <- function(scene, boundary) {
  # make sure boundary is in the same crs as the scene object
  boundary_proj <- sp::spTransform(boundary, scene@crs)

  # use landsat_transform to mask and propogate attributres
  landsat_transform(scene, raster::mask, boundary_proj)
}

landsat_project <- function(scene, crs_obj) {
  # use landsat_transform to call projectRaster and propogate attributes
  landsat_transform(scene, raster::projectRaster, crs_obj)
}

# the reason for propogating the attributes is this function, which allows
# overlay functions to be written in terms of individual bands instead of x and y
landsat_overlay <- function(scene, fun, ...) {
  # ensure scene is a landsat_scene
  if(!is.landsat_scene(scene)) stop("Cannot use landsat_overlay without a landsat_scene")

  # ensure fun is a function
  fun <- match.fun(fun)

  # extract arguments of the function
  fun_args <- names(formals(fun))

  # extract bands available from landsat_attrs
  band_names <- attr(scene, "landsat_attrs")$.band_names

  # check that all band_names are in fun_ags
  missing_args <- fun_args[!(fun_args %in% band_names)]
  if(any(missing_args)) stop("The following bands are missing in scene that are required by fun: ",
                             paste(missing_args, collapse = ", "))

  # modify function arguments to include all the band names so that raster::calc can be used
  formals(fun) <- stats::setNames(rep(list(rlang::missing_arg()), length(band_names)), band_names)

  # return result of raster::overlay
  raster::overlay(scene, fun = fun, ...)
}
