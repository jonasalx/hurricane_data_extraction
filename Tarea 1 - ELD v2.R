# Con su grupo, cree un script R que siga todos los pasos descritos hasta ahora para extraer los datos de seguimiento de huracanes desordenados en línea y limpiarlos.
# 
# Luego, pruebe los siguientes pasos de limpieza adicionales:


## Scrapiando TXT de 1851 de Hurdat Webpage.

library(rvest)
library(dplyr)

page <- read_html("https://www.nhc.noaa.gov/data/hurdat/")

web <- page %>%
  html_nodes("table") %>% html_nodes("tr") %>% html_nodes("a") %>%
  html_attr("href")

web <- as.data.frame(web)

hurdat_1851 <- web %>% 
  filter(grepl('hurdat2-1851',web))

## Toma ultimo archivo TXT cargado en la webpage.

library(readr)

tracks_url <- paste0("http://www.nhc.noaa.gov/data/hurdat/",
                     hurdat_1851[nrow(hurdat_1851),'web'])
hurr_tracks <- read_lines(tracks_url)

length(hurr_tracks)

## 

library(purrr)
hurr_tracks <- purrr::map(hurr_tracks, str_split,
                          pattern = ",", simplify = TRUE)

hurr_lengths <- map_int(hurr_tracks, length)

hurr_meta <- hurr_tracks[hurr_lengths == 4]
hurr_obs <- hurr_tracks[hurr_lengths == 21]

library(dplyr); library(tibble)
hurr_meta <- hurr_meta %>% 
  purrr::map(as_tibble) %>% 
  bind_rows()

hurr_meta <- hurr_meta %>%
  select(-V4) %>%
  rename(storm_id = V1, storm_name = V2, n_obs = V3) %>%
  mutate(storm_name = str_trim(storm_name),
         n_obs = as.numeric(n_obs))

storm_id <- rep(hurr_meta$storm_id, times = hurr_meta$n_obs)

hurr_obs <- hurr_obs %>% 
  purrr::map(as_tibble) %>% 
  bind_rows() %>% 
  mutate(storm_id = storm_id)


# Seleccione solo las columnas con fecha, hora, estado de la tormenta, ubicación (latitud y longitud), vientos máximos sostenidos y presión mínima y 
# cambie el nombre de las mismas.

head(hurr_obs)

hurr_obs2 <- select(hurr_obs, V1, V2, V3, V5, V6, V7, V8, storm_id) %>%
  rename(fecha = V1, hora = V2, est_torm = V3, lat = V5, long = V6, v_max_sost = V7, pres_min = V8)

# Crea una columna con la fecha y hora de cada observación, en una clase de fecha y hora.

hurr_obs2['fecha_hora'] <-
  as.POSIXct(
    paste(
      substr(hurr_obs2$fecha, 1, 4),
      "/",
      substr(hurr_obs2$fecha, 5, 6),
      "/",
      substr(hurr_obs2$fecha, 7, 8),
      " ",
      substr(hurr_obs2$hora, 2, 3),
      ":",
      substr(hurr_obs2$hora, 4, 5),
      ":",
      "00",
      sep = ""
    ),
    format = "%Y/%m/%d %H:%M:%S",
    tz = Sys.timezone()
  )

# Limpie la latitud y la longitud para que tengas columnas separadas para los valores numéricos y para el indicador de dirección (por ejemplo, N, S, E, W)

hurr_obs2['lat_number'] <- as.numeric(substr(hurr_obs2$lat,1,nchar(hurr_obs2$lat)-1))
hurr_obs2['lat_ind'] <- substr(hurr_obs2$lat,nchar(hurr_obs2$lat),nchar(hurr_obs2$lat))
hurr_obs2 <- select(hurr_obs2, -'lat')

hurr_obs2['long_number'] <- as.numeric(substr(hurr_obs2$long,1,nchar(hurr_obs2$long)-1))
hurr_obs2['long_ind'] <- substr(hurr_obs2$long,nchar(hurr_obs2$long),nchar(hurr_obs2$long))
hurr_obs2 <- select(hurr_obs2, -'long')


# Limpiar la columna de viento, por lo que da la velocidad del viento como un número y NA en los casos en los que falta la velocidad del viento.

hurr_obs2['v_max_sost'] <- ifelse(hurr_obs2$v_max_sost == ' -99', NA, as.integer(hurr_obs2$v_max_sost))

# Intente averiguar qué significan las abreviaturas status. Cree una nueva columna de factor nombrada status_longcon el estado detallado.

library(gdata)

hurr_obs2['est_torm'] <- as.factor(trim(hurr_obs2$est_torm))

hurr_obs2 <- hurr_obs2 %>% 
  mutate(status_longcon = case_when(
    est_torm == 'C' ~ 'Closest approach to a coast, not followed by a landfall',
    est_torm == 'G' ~ 'Genesis',
    est_torm == 'I' ~ 'An intensity peak in terms of both pressure and wind',
    est_torm == 'L' ~ 'Landfall (center of system crossing a coastline)',
    est_torm == 'P' ~ 'Minimum in central pressure',
    est_torm == 'R' ~ 'Provides additional detail on the intensity of the cyclone when rapid changes are underway',
    est_torm == 'S' ~ 'Change of status of the system',
    est_torm == 'T' ~ 'Provides additional detail on the track (position) of the cyclone',
    est_torm == 'W' ~ 'Maximum sustained wind speed',
    TRUE ~ 'No determinado'))
