
#' Create a RasterStack with attached Band Name Information
#'
#' This function creates a RasterStack (see \link[raster]{stack}) with band name
#' information attached. This information will be ignored by most functions in the
#' \code{raster} package, but is propogated by \link{landsat_crop}, \link{landsat_project},
#' \link{landsat_mask}, and \link{landsat_overlay}. The primary reason to use keep
#' the band name information with the RasterStack/RasterBrick object is for use with
#' \link{landsat_overlay}, which allows functions to be written in terms of band
#' numbers. This function does not load any raster files into memory.
#'
#' @param B1,B2,B3,B4,B5,B6,B7,B8,B9,B10,B11,BQA \code{RasterLayer} or
#'   filenames of raster image files to be passed to \link[raster]{raster}.
#' @param x A RasterBrick or RasterStack object
#' @param band_names The band names associated with the RasterBrick or
#'   Raster Stack object.
#' @param attrs A \code{list} that contains arbitrary meta information to keep
#'   with the \code{landsat_scene} object.
#'
#' @seealso \link{landsat_load_scenes}
#'
#' @return A \code{RasterStack} object with band names attached as attributes.
#' @export
#'
landsat_scene <- function(B1 = NULL, B2 = NULL, B3 = NULL, B4 = NULL, B5 = NULL,
                          B6 = NULL, B7 = NULL, B8 = NULL, B9 = NULL, B10 = NULL,
                          B11 = NULL, BQA = NULL, attrs = list()) {
  # create list of bands
  bands <- list(B1 = B1, B2 = B2, B3 = B3, B4 = B4,
                B5 = B5, B6 = B6, B7 = B7, B8 = B8, B9 = B9, B10 = B10, B11 = B11,
                BQA = BQA)

  # remove null or NA bands
  bands <- bands[!vapply(bands, function(x) is.null(x) || identical(x, NA), logical(1))]

  # throw error if there are no bands
  if(length(bands) == 0) stop("Zero bands were loaded in call to landsat_scene()")

  # make raster layers
  layers <- lapply(bands, function(x) {
    if(methods::is(x, "RasterLayer")) x else raster::raster(x)
  })

  # compare extents
  layer_indicies_by_extent <- landsat_compare_extents(layers)

  # if there is only one extent, ignore and continue, else:
  if(length(layer_indicies_by_extent) > 1) {
    # find the majority vote of extents
    layers_by_extent_counts <- vapply(layer_indicies_by_extent, length, integer(1))
    max_lengths <- which(layers_by_extent_counts == max(layers_by_extent_counts))

    # if there is no concensus, throw error
    if(length(max_lengths) > 1) stop("Bands do not have the same extents, and no concensus could ",
                                     "be established.")

    # if there is, set layers to the concensus and throw a warning
    keep_indicies <- layer_indicies_by_extent[[max_lengths]]
    layers <- layers[keep_indicies]

    drop_indicies <- unlist(layer_indicies_by_extent[-max_lengths])
    warning("The following bands were ignored due to non-matching extents: ",
            paste(names(bands)[drop_indicies]), collapse = ", ")

  }

  # make raster stack
  rstack <- do.call(raster::stack, stats::setNames(layers, NULL))

  # keep band names
  attrs$.band_names <- names(bands)

  # set attributes
  attr(rstack, "landsat_attrs") <- attrs

  # return stack
  rstack
}

#' @rdname landsat_scene
#' @export
as.landsat_scene <- function(x, band_names, attrs = list()) {
  # check x input
  if(!methods::is(x, "RasterStack") && !methods::is(x, "RasterBrick")) {
    stop("x must be a RasterStack or RasterBrick object")
  }

  # check band_names object for valid band names
  valid_band_names <- c(paste0("B", 1:11), "BQA")
  if(!is.atomic(x) || !all(band_names %in% valid_band_names)) {
    stop("Band names must be in (", paste(valid_band_names, collapse = ", "), ")")
  }

  # check length of band names
  if(raster::nbands(x) != length(band_names)) {
    stop("Length of band names not equal to nbands(x)")
  }

  # set the band names
  attrs$.band_names <- band_names

  # set the landsat_scene attribute
  attr(x, "landsat_attrs") <- attrs

  # return the stack/brick object
  x
}

#' @rdname landsat_scene
#' @export
is.landsat_scene <- function(x) {
  !is.null(attr(x, "landsat_attrs"))
}

#' Load multiple scenes from a vector of image filenames
#'
#' This function applies \link{landsat_scene} to a vector of image filenames, such
#' as that listed by \link{landsat_list_images}.
#'
#' @param path A vector of landsat image filenames
#' @param include_bands A vector of band codes to include in the output,
#'   or NULL to use all of them.
#'
#' @export
#' @return A data.frame with the same columns as \link{landsat_parse_filename_image}
#'   plus one named \code{scene}, containing the landsat scenes as loaded by
#'   \link{landsat_scene}.
#'
landsat_load_scenes <- function(path, include_bands = NULL) {
  # deal with zero-length input
  if(length(path) == 0) return(tibble::tibble())

  # parse filename
  info <- landsat_parse_filename_image(path)

  # filter bands, if included as an argument
  if(!is.null(include_bands)) {
    # make sure it is of the correct type
    if(!is.atomic(include_bands)) stop("`include_bands` must be a character vector")

    # filter info to only include rows with that band number
    info <- info[info$band_code %in% include_bands,]

    # if there are no rows, return an empty tibble with a warning
    if(nrow(info) == 0) {
      warning("Zero scenes were found with bands ", paste(include_bands, collapse = ", "))
      return(tibble::tibble())
    }
  }

  # sort by photo date, band number
  info <- info[order(info$acquisition_date, info$band_code), ]

  # get list of band numbers
  band_codes <- sort(unique(info$band_code))

  # use tidyr:: to cast the filenames into columns per band
  info <- tidyr::spread(info, key = "band_code", value = "path")

  # load scenes into a list() using landsat_scene
  info$scene <- plyr::mlply(info[band_codes], landsat_scene)

  # give the scene list a custom class
  class(info$scene) <- c("landsat_scene_list", "list")

  # remove filename columns
  info[band_codes] <- NULL

  # return info with a custom class
  class(info) <- c("landsat_scene_df", class(info))

  # return info
  info
}

# this function compares extents of various landsat bands (Band 8 is a frequent offender
# having a different extent than the rest of the bands)
landsat_compare_extents <- function(layer_list) {
  extents <- lapply(layer_list, function(layer) {
    ext <- raster::extent(layer)
    c(xmin = ext@xmin, ymin = ext@ymin, xmax = ext@xmax, ymax = ext@ymax)
  })

  unique_extents <- unique(extents)
  sorted_layer_indicies <- lapply(unique_extents, function(ext) {
    matches <- vapply(extents, identical, ext, FUN.VALUE = logical(1))
    which(matches)
  })

  sorted_layer_indicies
}
