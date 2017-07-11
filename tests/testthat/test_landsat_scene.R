
context("landsat_scene 'objects'")

generate_band <- function(bbox, epsg, width = 100, height = 100) {
  layer <- raster::raster(raster::extent(bbox), nrows = height, ncols = width,
                          crs = sp::CRS(paste0("+init=epsg:", epsg)))
  raster::values(layer) <- runif(width * height)
  layer
}

# generate a real lat/lon extent, transform to UTM zone 20
ext <- matrix(c(-67.14096, -64.19541, 43.95664, 45.24014),
              ncol = 2, nrow = 2, byrow = TRUE)
ext <- sp::bbox(sp::spTransform(sp::SpatialPoints(sp::coordinates(t(ext)),
                                                  proj4string = sp::CRS("+init=epsg:4326")),
                                CRSobj = sp::CRS("+init=epsg:26920")))

# generate bands
bands <- sapply(c(paste0("B", 1:11), "BQA"), function(x) generate_band(ext, epsg = 26920))
# mess with band 8 extent
raster::extent(bands$B8) <- raster::extent(sp::bbox(bands$B8) - 0.1)
# write bands to temp files
temp_dir <- tempfile()[1]
dir.create(temp_dir)

band_fnames <- sapply(names(bands), function(band_name) {
  fname <- file.path(temp_dir, sprintf("LC08_L1TP_009029_20170508_20170515_01_T1_%s.tif", band_name))
  raster::writeRaster(bands[[band_name]], fname)
  fname
}, simplify = FALSE)

test_that("test rasters were generated correctly", {
  expect_true(all(file.exists(unlist(band_fnames))))
  rasters <- lapply(band_fnames, raster::raster)
  is_layer <- vapply(rasters, methods::is, "RasterLayer", FUN.VALUE = logical(1))
  expect_true(all(is_layer))
})

test_that("landsat_scene with no arguments generates error", {
  expect_error(landsat_scene(), "Zero bands were loaded in call to landsat_scene()")
})

test_that("NA is treated the same as NULL in landsat_scene", {
  expect_error(landsat_scene(B1 = NA, B2 = NA, B3 = NA, B4 = NA,
                             B5 = NA, B6 = NA, B7 = NA, B8 = NA,
                             B9 = NA, B10 = NA, B11 = NA, BQA = NA),
               "Zero bands were loaded in call to landsat_scene()")
})

test_that("landsat_scene returns a raster stack with correct attributes", {
  # test with raster objects
  ls <- landsat_scene(B1 = bands$B1)
  expect_true(is.landsat_scene(ls))
  expect_is(ls, "RasterStack")
  expect_equal(attr(ls, "landsat_attrs")$.band_names, "B1")
  ls <- landsat_scene(B1 = bands$B1, B2 = bands$B2)
  expect_true(is.landsat_scene(ls))
  expect_is(ls, "RasterStack")
  expect_equal(attr(ls, "landsat_attrs")$.band_names, c("B1", "B2"))
  # and raster filenames
  ls <- landsat_scene(B1 = band_fnames$B1)
  expect_true(is.landsat_scene(ls))
  expect_is(ls, "RasterStack")
  expect_equal(attr(ls, "landsat_attrs")$.band_names, "B1")
  ls <- landsat_scene(B1 = band_fnames$B1, B2 = band_fnames$B2)
  expect_true(is.landsat_scene(ls))
  expect_is(ls, "RasterStack")
  expect_equal(attr(ls, "landsat_attrs")$.band_names, c("B1", "B2"))
})

test_that("landsat_scene custom attributes get set", {
  ls <- landsat_scene(B1 = bands$B1, attrs = list("custom_key" = "custom_value"))
  landsat_attrs <- attr(ls, "landsat_attrs")
  expect_equal(landsat_attrs$custom_key, "custom_value")
})

test_that("non-matching extents are handled correctly", {
  ls <- landsat_scene(B1 = bands$B1, B2 = bands$B2)
  expect_is(ls, "RasterStack")

  expect_error(landsat_scene(B1 = bands$B1, B8 = bands$B8),
               "Bands do not have the same extents, and no concensus could be established.")

  expect_warning(landsat_scene(B1 = bands$B1, B2 = bands$B2, B8 = bands$B8),
                "The following bands were ignored due to non-matching extents:.*?")
})

test_that("load scenes returns the correct format", {
  # zero-length input is just a tibble
  expect_identical(landsat_load_scenes(character(0)), tibble::tibble())

  lsdf <- landsat_load_scenes(unlist(band_fnames))
  expect_is(lsdf, "data.frame")
  expect_is(lsdf, "landsat_scene_df")
  expect_true("scene" %in% colnames(lsdf))
  expect_is(lsdf$scene, "list")
  expect_is(lsdf$scene, "landsat_scene_list")
  expect_true(all(vapply(lsdf$scene, is.landsat_scene, logical(1))))
})

test_that("load scenes respects band filter", {
  expect_warning(landsat_load_scenes(unlist(band_fnames), include_bands = character(0)),
                 "Zero scenes were found with bands")
  lsdf0 <- landsat_load_scenes(unlist(band_fnames), include_bands = character(0))
  expect_is(lsdf0, "data.frame")

  expect_error(landsat_load_scenes(unlist(band_fnames), include_bands = list()),
               "`include_bands` must be a character vector")

  lsdf1 <- landsat_load_scenes(unlist(band_fnames), include_bands = "B1")
  expect_is(lsdf1, "data.frame")
  expect_is(lsdf1, "landsat_scene_df")
  expect_true("scene" %in% colnames(lsdf1))
  expect_is(lsdf1$scene, "list")
  expect_is(lsdf1$scene, "landsat_scene_list")
  expect_true(all(vapply(lsdf1$scene, is.landsat_scene, logical(1))))
  expect_equal(raster::nlayers(lsdf1$scene[[1]]), 1)
})

# remove temp directory
unlink(temp_dir, recursive = TRUE)
