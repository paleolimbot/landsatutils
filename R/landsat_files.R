

# define a regex that matches landsat filenames like this:
# LT05_L1TP_063013_19960711_20170104_01_T1_B2.TIF
# and this:
# LT05_L1TP_063013_19960711_20170104_01_T1.tar.gz
landsat_product_id_regex <- "(L)([COTEM])([0-9]{2})_(L1TP|L1GT|L1GS)_([0-9]{3})([0-9]{3})_([0-9]{8})_([0-9]{8})_([0-9]{2})_(RT|T1|T2)"
landsat_image_regex <- paste0(landsat_product_id_regex, "_(B[0-9QA]+).TIF$")
landsat_archive_regex <- paste0(landsat_product_id_regex, ".tar.gz$")

#' List Landsat Files
#'
#' List files in \code{path} that are valid landsat GeoTiff filenames, which
#' are something like "LT05_L1TP_063013_19960711_20170104_01_T1_B2.TIF".
#'
#' @param path The directory where files are located
#' @param full.names Use full names in relation to the working directory
#' @param recursive Look in all subdirectories.
#'
#' @return A character vector of file locations.
#' @export
#'
#' @examples
#' landsat_list_images()
#' landsat_list_archives()
#'
landsat_list_images <- function(path = ".", full.names = TRUE, recursive = FALSE) {
  list.files(path, pattern = landsat_image_regex, full.names = full.names, recursive = recursive,
             ignore.case = TRUE)
}

#' @rdname landsat_list_images
#' @export
landsat_list_archives <- function(path = ".", full.names = TRUE, recursive = FALSE) {
  list.files(path, pattern = landsat_archive_regex, full.names = full.names, recursive = recursive,
             ignore.case = TRUE)
}


#' Extract Information from Landsat Filenames
#'
#' Uses Landsat filenames (that perhaps were listed using \link{landsat_list_images}
#' or \link{landsat_list_archives})
#' to fill in data that are present in the filename.
#'
#' @param path Filenames or file paths that are valid Landsat GeoTIFF files.
#'
#' @return A data.frame with the same number of rows as the length of \code{path}.
#' @export
#'
#' @references
#' \url{https://landsat.usgs.gov/what-are-naming-conventions-landsat-scene-identifiers}
#'
#' @examples
#' landsat_parse_product_id("LT05_L1TP_063013_19960711_20170104_01_T1")
#' landsat_parse_filename_archive("LT05_L1TP_063013_19960711_20170104_01_T1.tar.gz")
#' landsat_parse_filename_image("LT05_L1TP_063013_19960711_20170104_01_T1_B1.TIF")
#'
#' landsat_images <- landsat_list_images()
#' landsat_parse_filename_image(landsat_images)
#'
#' landsat_archives <- landsat_list_archives()
#' landsat_parse_filename_archive(landsat_archives)
#'
landsat_parse_filename_image <- function(path) {
  # Example LT05_L1TP_063013_19960711_20170104_01_T1_B2.TIF
  # this is vectorized, so a vector of file paths can be passed to "path"

  # handle zero-length output
  if(length(path) == 0) return(tibble::tibble())

  # remove the first part of the filename and the extension, make upper case
  fname <- toupper(basename(path))

  # check that fnames match the regex
  valid_name <- grepl(landsat_image_regex, fname, ignore.case = TRUE)
  if(!all(valid_name)) stop("The following filenames are not valid Landsat image filenames: ",
                            paste(path[!valid_name], collapse = ", "))

  # extract product ID information
  product_id_info <- landsat_parse_product_id(path)

  # extract image filename components from the regex
  filename_components <- stringr::str_match(fname, landsat_image_regex)
  product_id_info$band_code <- filename_components[, 12, drop = TRUE]

  # return the product_id info
  product_id_info
}

#' @rdname landsat_parse_filename_image
#' @export
landsat_parse_filename_archive <- function(path) {
  # Example LT05_L1TP_063013_19960711_20170104_01_T1.tar.gz
  # this is vectorized, so a vector of file paths can be passed to "path"

  # handle zero-length output
  if(length(path) == 0) return(tibble::tibble())

  # check that fnames match the regex
  valid_name <- grepl(landsat_archive_regex, path, ignore.case = TRUE)
  if(!all(valid_name)) stop("The following filenames are not valid Landsat archive filenames: ",
                            paste(path[!valid_name], collapse = ", "))

  # return product ID information
  landsat_parse_product_id(path)
}

#' @rdname landsat_parse_filename_image
#' @export
landsat_parse_product_id <- function(path) {
  # Example LT05_L1TP_063013_19960711_20170104_01_T1
  # some help from https://landsat.usgs.gov/what-are-naming-conventions-landsat-scene-identifiers
  # this is vectorized, so a vector of file paths can be passed to "path"

  # handle zero-length output
  if(length(path) == 0) return(tibble::tibble())

  products <- c("L" = "Landsat")
  # "T" is listed twice on the website, which doesn't make sense
  sensors <- c("C" = "OLI/TIRS Combined", "O" = "OLI-only", "T" = "TIRS-only", "E" = "ETM+",
               "T" = "TM", "M" = "MSS")
  correction_levels <- c("L1TP" = "Precision Terrain", "L1GT" = "Systematic Terrain",
                         "L1GS" = "Systematic")
  collection_categories <- c("RT" = "Real-Time", "T1" = "Tier 1", "T2" = "Tier 2")

  # remove the first part of the filename and the extension, make upper case
  fname <- toupper(basename(path))

  # check that fnames match the regex
  valid_name <- grepl(landsat_product_id_regex, fname, ignore.case = TRUE)
  if(!all(valid_name)) stop("The following filenames do not contain valid Landsat product IDs: ",
                            paste(path[!valid_name], collapse = ", "))

  # extract filename components from the regex
  filename_components <- stringr::str_match(fname, landsat_product_id_regex)

  # extract the codes
  product_id <- filename_components[, 1, drop = TRUE]
  product_code <- filename_components[, 2, drop = TRUE]
  sensor_code <- filename_components[, 3, drop = TRUE]
  satellite_code <- filename_components[, 4, drop = TRUE]
  correction_level_code <- filename_components[, 5, drop = TRUE]
  wrs_path <- as.integer(filename_components[, 6, drop = TRUE])
  wrs_row <- as.integer(filename_components[, 7, drop = TRUE])
  acquisition_date <- lubridate::ymd(filename_components[, 8, drop = TRUE])
  processing_date <- lubridate::ymd(filename_components[, 9, drop = TRUE])
  collection_number <- as.integer(filename_components[, 10, drop = TRUE])
  collection_category_code <- filename_components[, 11, drop = TRUE]

  # return a data.frame with the info
  tibble::tibble(
    path = path,
    product_id = product_id,
    product_code = product_code,
    product = products[product_code],
    sensor_code = sensor_code,
    sensor = sensors[sensor_code],
    satellite_code = satellite_code,
    correction_level_code = correction_level_code,
    correction_level = correction_levels[correction_level_code],
    wrs_path = wrs_path,
    wrs_row = wrs_row,
    acquisition_date = acquisition_date,
    processing_date = processing_date
  )
}

