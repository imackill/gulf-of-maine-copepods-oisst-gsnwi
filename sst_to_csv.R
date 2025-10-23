library(ncdf4)
library(dplyr)
library(lubridate)

indir <- "NOAA GoM OISST/raw_netcdf"
out_csv <- "NOAA GoM OISST/gom_noaa_oisst.csv"

lon_min <- -71; lon_max <- -65
lat_min <- 41; lat_max <- 45.5
convert_lon <- function(lon) ifelse(lon > 180, lon - 360, lon)

extract_sst_points <- function(file) {
  nc <- tryCatch(nc_open(file), error = function(e) return(NULL))
  if (is.null(nc)) return(NULL)
  
  lon <- convert_lon(ncvar_get(nc, "lon"))
  lat <- ncvar_get(nc, "lat")
  
  lon_idx <- which(lon >= lon_min & lon <= lon_max)
  lat_idx <- which(lat >= lat_min & lat <= lat_max)
  if (length(lon_idx) == 0 || length(lat_idx) == 0) {
    nc_close(nc)
    return(NULL)
  }
  
  sst_raw <- ncvar_get(nc, "sst")
  dims <- length(dim(sst_raw))
  if (dims == 2) sst <- sst_raw[lon_idx, lat_idx]
  else if (dims == 3) sst <- sst_raw[lon_idx, lat_idx, 1]
  else if (dims == 4) sst <- sst_raw[lon_idx, lat_idx, 1, 1]
  else { nc_close(nc); stop("Unsupported SST dimensions in ", file) }
  
  scale <- ncatt_get(nc, "sst", "scale_factor")$value
  offset <- ncatt_get(nc, "sst", "add_offset")$value
  fill <- ncatt_get(nc, "sst", "_FillValue")$value
  
  sst <- sst * scale + offset
  sst[sst_raw[1:length(sst)] == fill] <- NA
  
  time_days <- ncvar_get(nc, "time")
  time_units <- ncatt_get(nc, "time", "units")$value
  base_date <- as.Date(strsplit(time_units, "since ")[[1]][2])
  date <- base_date + time_days[1]
  
  df <- expand.grid(
    lon = lon[lon_idx],
    lat = lat[lat_idx]
  )
  df$temp_c <- as.vector(sst)
  df$date <- as.Date(date)
  df <- df[, c("date", "lat", "lon", "temp_c")]
  
  nc_close(nc)
  df
}

files <- list.files(indir, full.names = TRUE, pattern = "\\.nc$")
first <- TRUE

for (file in files) {
  df <- try(extract_sst_points(file), silent = TRUE)
  if (!is.null(df) && !inherits(df, "try-error")) {
    write.table(df, out_csv, sep = ",", row.names = FALSE,
                col.names = first, append = !first)
    first <- FALSE
  }
  cat("Processed:", file, "\n")
}
cat("Saved Gulf of Maine daily SST to:", out_csv, "\n")