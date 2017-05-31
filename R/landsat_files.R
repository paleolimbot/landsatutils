

# define a regex that matches landsat filenames like this:
# LT05_L1TP_063013_19960711_20170104_01_T1_B2.TIF
landsat_regex <- "(L)([COTEM])([0-9]{2})_(L1TP|L1GT|L1GS)_([0-9]{3})([0-9]{3})_([0-9]{8})_([0-9]{8})_([0-9]{2})_(RT|T1|T2)_(B[0-9QA]+).TIF$"

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
#' landsat_list_files()
#'
landsat_list_files <- function(path = ".", full.names = TRUE, recursive = FALSE) {
  list.files(path, pattern = landsat_regex, full.names = full.names, recursive = recursive,
             ignore.case = TRUE)
}


#' Extract Information from Landsat Filenames
#'
#' Uses Landsat filenames (that perhaps were listed using \link{landsat_list_files})
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
#' landsat_files <- landsat_list_files()
#' landsat_parse_filename(landsat_files)
#'
landsat_parse_filename <- function(path) {
  # Example LT05_L1TP_063013_19960711_20170104_01_T1_B2.TIF
  # some help from https://landsat.usgs.gov/what-are-naming-conventions-landsat-scene-identifiers
  # this is vectorized, so a vector of file paths can be passed to "path"

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
  valid_name <- grepl(landsat_regex, fname, ignore.case = TRUE)
  if(!all(valid_name)) stop("The following filenames are not valid Landsat product filenames: ",
                            paste(fname[!valid_name], collapse = ", "))

  # extract filename components from the regex
  filename_components <- stringr::str_match(fname, landsat_regex)

  # extract the codes
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
  band_code <- filename_components[, 12, drop = TRUE]

  # product id is the match minus the band code/extension
  product_id <- stringr::str_replace(filename_components[, 1, drop = TRUE],
                                     paste0("_", band_code, ".TIF"), "")

  # return a data.frame with the info
  data.frame(
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
    processing_date = processing_date,
    band_code = band_code,
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}

