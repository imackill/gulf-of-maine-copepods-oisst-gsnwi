# =========================================================
# NOAA OISST v2.1 (AVHRR) — Gulf of Maine Daily Mean SST
# Gets Daily SST in ºC Since 1981 (For MacOS)
# Output to CSV
# by Iain MacKillop
# =========================================================

library(rvest)
library(dplyr)
library(ncdf4)
library(parallel)

outdir <- "NOAA GoM OISST"
rawdir <- file.path(outdir, "raw_netcdf")
dir.create(rawdir, recursive = TRUE, showWarnings = FALSE)

out_csv <- file.path(outdir, "gom_restricted_sst_data.csv")
base_url <- "https://www.ncei.noaa.gov/data/sea-surface-temperature-optimum-interpolation/v2.1/access/avhrr/"

options(timeout = 600)

# ----------------------------------------------------------------------
# Function to safely download a single file (with retries)
# ----------------------------------------------------------------------
safe_download <- function(url, dest) {
  if (file.exists(dest) && file.info(dest)$size > 1e5) {
    message("✔ Already exists: ", basename(dest))
    return(TRUE)
  }
  
  for (i in 1:3) {
    try({
      download.file(url, dest, mode = "wb", method = "curl", quiet = TRUE)
      if (file.exists(dest) && file.info(dest)$size > 1e5) {
        message("Downloaded: ", basename(dest))
        return(TRUE)
      }
    }, silent = TRUE)
    Sys.sleep(runif(1, 0.5, 2))
  }
  warning("⚠ Failed: ", url)
  FALSE
}

# ----------------------------------------------------------------------
# Step 1: Check existing data
# ----------------------------------------------------------------------
existing_files <- list.files(rawdir, pattern = "\\.nc$", full.names = TRUE, recursive = TRUE)
existing_size <- if (length(existing_files) > 0) sum(file.info(existing_files)$size, na.rm = TRUE) else 0

if (existing_size > 10e6) {  # If >10 MB of data already exists
  cat("Existing data (", length(existing_files), "files, ", round(existing_size/1e6, 1), "MB). Skipping download.\n")
} else {
  month_links <- read_html(base_url) %>%
    html_nodes("a") %>%
    html_attr("href") %>%
    setdiff("..") %>%
    paste0(base_url, .)
  
  get_files_in_month <- function(month_link) {
    tryCatch({
      page <- read_html(month_link)
      day_links <- page %>% html_nodes("a") %>% html_attr("href")
      day_links <- setdiff(day_links, "..")
      paste0(month_link, "/", day_links)
    }, error = function(e) character(0))
  }
  
  all_file_urls <- unlist(mclapply(month_links, get_files_in_month, mc.cores = detectCores() - 1))
  dest_files <- file.path(rawdir, basename(all_file_urls))
  
  cat("Found", length(all_file_urls), "files to check/download.\n")
  
  todo <- which(!file.exists(dest_files) | file.info(dest_files)$size < 1e5)
  if (length(todo) > 0) {
    cat("Downloading", length(todo), "missing files in parallel...\n")
    mclapply(todo, function(i) safe_download(all_file_urls[i], dest_files[i]),
             mc.cores = max(1, detectCores() - 1))
  } else {
    cat("All files already downloaded.\n")
  }

}

files <- list.files(path = rawdir, full.names = TRUE, recursive = TRUE, pattern = "\\.nc$")
cat("Found", length(files), "NetCDF files ready for processing.\n")
