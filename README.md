# ardafetch

An R package to search, download, cache, and load datasets from the Association of Religion Data Archives (ARDA) hosted on OSF.

## Installation

Install from the local folder (or build tarball):

```r
# Install dependencies if needed
install.packages(c("dplyr", "haven", "httr2", "readr", "stringr", "tibble", "readxl"))

# Install ardafetch from the unzipped directory
install.packages("path/to/ardafetch", repos = NULL, type = "source")
```

## Quick Start

```r
library(ardafetch)

# 1. Register your file-level crosswalk once (caches it automatically for future sessions)
arda_set_catalog("path/to/ARDA_OSF_file_level_crosswalk.csv")

# 2. Search datasets by keyword, ARDA ID, or year
arda_search("Harris 1979")
arda_search("Catholic", year = 2002)

# 3. Inspect available files & formats for a dataset
arda_files("1979ABC")

# 4. Download, cache, and open the dataset directly into a tibble/data.frame
df <- arda_load("1979ABC")                  # Automatically picks .sav / .dta / .por / .csv
df_stata <- arda_load("1979ABC", format = "dta")

# 5. Or download a codebook PDF without loading into memory
cb_path <- arda_load("1979ABC", format = "pdf", download_only = TRUE)
```
