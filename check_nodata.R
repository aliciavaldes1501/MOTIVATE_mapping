library(dplyr)
library(readr)
library(purrr)

# Carpeta raíz
root_dir <- "C:/Data/MOTIVATE/MOTIVATE_RS_data/mapping"

# Buscar todos los csv en subcarpetas
files <- list.files(
  root_dir,
  pattern = "\\.csv$",
  recursive = TRUE,
  full.names = TRUE
)

# Leer y combinar
datos <- files %>%
  map_dfr(read_csv, show_col_types = FALSE)

# PlotObservationID con nodata
nodata_obs <- datos %>%
  filter(data_source == "nodata") %>%
  distinct(PlotObservationID)

# PlotObservationID con datos
data_obs <- datos %>%
  filter(data_source == "ok") %>%
  distinct(PlotObservationID)

# Número de PlotObservationID diferentes con nodata y cpn datos
n_nodata <- nrow(nodata_obs)
n_data <- nrow(data_obs)

n_nodata
n_data

# Cuántas veces aparece cada PlotObservationID como nodata
nodata_count <- datos %>%
  filter(data_source == "nodata") %>%
  count(PlotObservationID, sort = TRUE)

nodata_count

# Resumen indicando para cada PlotObservationID si alguna vez tuvo nodata en cualquiera de los archivos
resumen <- datos %>%
  group_by(PlotObservationID) %>%
  summarise(
    tiene_nodata = any(data_source == "nodata"),
    n_nodata = sum(data_source == "nodata"),
    .groups = "drop"
  )

resumen
