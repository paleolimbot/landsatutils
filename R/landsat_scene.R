
# define function to create a scene raster stack
# a scene raster stack is just a raster stack (or brick) with band names
# b1 - b11 + BQA, and a lansat_attrs that stores landsat-specific info with the raster
# (just the band names contained in the object, for now)
# arguments can be filenames or RasterLayer objects
landsat_scene <- function(B1 = NULL, B2 = NULL, B3 = NULL, B4 = NULL, B5 = NULL,
                          B6 = NULL, B7 = NULL, B8 = NULL, B9 = NULL, B10 = NULL,
                          B11 = NULL, BQA = NULL, attrs = list()) {
  # create list of bands
  bands <- list(B1 = B1, B2 = B2, B3 = B3, B4 = B4,
                B5 = B5, B6 = B6, B7 = B7, B8 = B8, B9 = B9, B10 = B10, B11 = B11,
                BQA = BQA)

  # remove null or NA bands
  bands <- bands[!vapply(bands, function(x) is.null(x) || is.na(x), logical(1))]

  # throw error if there are no bands
  if(length(bands) == 0) stop("Zero bands were loaded in call to landsat_scene()")

  # make raster layers
  layers <- lapply(bands, raster::raster)

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
  rstack <- do.call(raster::stack, setNames(layers, NULL))

  # keep band names
  attrs$.band_names <- names(bands)

  # set attributes
  attr(rstack, "landsat_attrs") <- attrs

  # return stack
  rstack
}

# turn an existing RasterStack or RasterBrick into a landsat_scene
as.landsat_scene <- function(x, band_names, attrs = list()) {
  # check x input
  if(!methods::is(x, "RasterStack") && !methods::is(x, "RasterBrick")) {
    stop("x must be a RasterStack or RasterBrick object")
  }

  # check band_names object for valid band names
  valid_band_names <- c(paste0("B", 1:11), "BQA")
  if(!is.atomix(x) || !all(band_names %in% valid_band_names)) {
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

# test if object was created with landsat_scene
is.landsat_scene <- function(x) {
  !is.null(attr(x, "landsat_attrs"))
}

# load a list of landsat scenes from a vector of filenames
landsat_load_scenes <- function(path, include_bands = NULL) {
  # parse filename
  info <- landsat_parse_filename(path)

  # remove the band number
  info$band_number <- NULL

  # filter bands, if included as an argument
  if(!is.null(include_bands)) {
    # make sure it is of the correct type
    if(!is.atomic(include_bands)) stop("`include_bands` must be a character vector")

    # filter info to only include rows with that band number
    info <- info[info$band_code %in% include_bands,]

    # if there are no rows, stop
    if(nrow(info) == 0) stop("Zero scenes were found with bands ",
                             paste(include_bands, collapse = ", "))
  }

  # sort by photo date, band number
  info <- info[order(info$acquisition_date, info$band_code), ]

  # get list of band numbers
  band_codes <- sort(unique(info$band_code))

  # use reshape2::dcast to cast the filenames into columns per band
  info <- reshape2::dcast(info, ... ~ band_code, value.var = "path")

  # load scenes into a list() using landsat_scene
  info$scene <- plyr::mlply(info[band_codes], landsat_scene)

  # remove filename columns
  info[band_codes] <- NULL

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
