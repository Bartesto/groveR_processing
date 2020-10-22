# Function for conversion of mosaics to veg density rasters

#' Reads in index raster layers, converts to a vegetation density, and writes to 
#' file.
#' 
#' \code{veg_dens} Takes a file path to raster/s of a vegetation index for the 
#'     purposes of converting to a vegetation density product. Output product will
#'     be written to geotiff.
#'     
#' @details This function converts a vegetation index raster layer to 
#'     vegetation density using predetermined calibration coefficients