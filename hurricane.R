
library(httr)
meso_url <- paste0("https://mesonet.agron.iastate.edu/",
                   "cgi-bin/request/asos.py/")
denver <- GET(url = meso_url,
              query = list(station = "DEN", data = "sped",
                           year1 = "2016", month1 = "6",
                           day1 = "1", year2 = "2016",
                           month2 = "6", day2 = "30",
                           tz = "America/Denver",
                           format = "comma"))


str(denver, max.level = 1, list.len = 6)

library(tidyverse)
denver %>% content() %>% 
  read_csv(skip = 5, na = "M") %>%
  slice(1:3)

install.packages("riem")
library(riem)
denver_2 <- riem_measures(station = "DEN", 
                          date_start = "2016-06-01",
                          date_end = "2016-06-30")


denver_2 %>% slice(1:3) 

#  Ejemplo de paquetes de envoltura de API de R.

install.packages("tigris")

# acs: descargue, manipule y presente datos de la Encuesta de la Comunidad Estadounidense y Decenal del Censo de los EE. UU.

# USAFronteras: fronteras hist�ricas y contempor�neas de los Estados Unidos de Am�rica

# idbr: interfaz R con la API de la base de datos internacional de la Oficina del Censo de EE. UU. (por ejemplo, poblaciones de otros pa�ses)



# Algunos ejemplos (todas las descripciones de rOpenSci):

# AntWeb: acceda a datos de la base de datos de hormigas m�s grande del mundo

# cromador: interact�a con la base de datos de recuentos de cromosomas (CCDB)

# g�nero: codifica el g�nero seg�n los nombres y las fechas de nacimiento

# musemeta: R Client para Scraping Museum Metadata, incluido el Metropolitan Museum of Art, la Canadian Science & Technology Museum Corporation, la National Gallery of Art y el Getty Museum, y m�s por venir.

# rusda: Interfaz con algunas bases de datos del USDA

# webchem: recupera informaci�n qu�mica de muchas fuentes.

# Actualmente incluye: Chemical Identifier Resolver, ChemSpider, PubChem y Chemical Translation Service. 


install.packages("countyweather")

# Los paquetes de USGS R incluyen:

# dataRetrieval: Obtenga datos de muestras de calidad del agua, datos de flujo de arroyos y metadatos directamente de USGS o EPA
# EGRET: An�lisis de cambios a largo plazo en la calidad del agua y el caudal, incluido el m�todo de la calidad del agua Regresiones ponderadas por tiempo, descarga y temporada (WRTDS)
# laketemps: paquete de datos de temperatura de los lagos para el Proyecto de colaboraci�n global de la temperatura de los lagos
# lakeattributes: datos de atributos de lago �tiles comunes
# soilmoisturetools: herramientas para la recuperaci�n y visualizaci�n de datos de humedad del suelo


# A continuaci�n, se muestran algunos ejemplos de otros paquetes de R que facilitan el uso de una API para datos abiertos:

# TwitteR: Twitter
# Quandl: Quandl (datos financieros)
# RGoogleAnalytics: Google Analytics
# WDI, wbstats: Banco Mundial
# GuardianR, rdian: The Guardian Media Group
# blsAPI: Oficina de Estad�sticas Laborales
# rtimes: New York Times


#  Limpiar datos muy sucios

tracks_url <- paste0("http://www.nhc.noaa.gov/data/hurdat/",
                     "hurdat2-1851-2017-050118.txt")
hurr_tracks <- read_lines(tracks_url, n_max = 3)
hurr_tracks

class(hurr_tracks)
length(hurr_tracks)
hurr_tracks[1]

library(stringr)
str_split(hurr_tracks[1], pattern = ",")

tracks_url <- paste0("http://www.nhc.noaa.gov/data/hurdat/",
                     "hurdat2-1851-2017-050118.txt")
hurr_tracks <- read_lines(tracks_url)
length(hurr_tracks)

library(purrr)
hurr_tracks <- purrr::map(hurr_tracks, str_split,
                          pattern = ",", simplify = TRUE)
hurr_tracks[[1]]

hurr_tracks[[2]][1:2]

hurr_lengths <- map_int(hurr_tracks, length)
hurr_lengths[1:17]

unique(hurr_lengths)

hurr_meta <- hurr_tracks[hurr_lengths == 4]
hurr_obs <- hurr_tracks[hurr_lengths == 21]

hurr_meta[1:3]

hurr_obs[1:2]

library(dplyr); library(tibble)
hurr_meta <- hurr_meta %>% 
  purrr::map(as_tibble) %>% 
  bind_rows()

hurr_meta %>% slice(1:3)
unique(hurr_meta$V4)

hurr_meta$V2[1:2]

hurr_meta <- hurr_meta %>%
  select(-V4) %>%
  rename(storm_id = V1, storm_name = V2, n_obs = V3) %>%
  mutate(storm_name = str_trim(storm_name),
         n_obs = as.numeric(n_obs))
hurr_meta %>% slice(1:3)

storm_id <- rep(hurr_meta$storm_id, times = hurr_meta$n_obs)
head(storm_id, 3)

length(storm_id)
length(hurr_obs)

hurr_obs <- hurr_obs %>% 
  purrr::map(as_tibble) %>% 
  bind_rows() %>% 
  mutate(storm_id = storm_id)
hurr_obs %>% select(V1:V2, V5:V6, storm_id) %>% slice(1:3)


#   EJEMPLO: Trabajar con un paquete contenedor API
# El rplos es un paquete que proporciona un contenedor para la API de Public Library of Science (PLoS). 
# PLOS tiene una colecci�n de revistas acad�micas que abarcan una variedad de temas.

library(rplos)
wn_papers <- searchplos(q = "West Nile", 
                        fl = c("publication_date", "title", "journal", "subject",
                               "abstract", "article_type"))

str(wn_papers)
wn_papers$meta

wn_papers_titles <- searchplos(q = "title:West Nile", 
                               fl = c("publication_date", "title", "journal", "subject",
                                      "abstract", "article_type"))
wn_papers_titles$meta

wn_papers_titles <- searchplos(q = "title:West Nile", 
                               fl = c("publication_date", "title", "journal", "subject",
                                      "abstract", "article_type"),
                               limit = 190)
nrow(wn_papers_titles$data)

library(lubridate)
library(dplyr)
library(ggplot2)
wn_papers_titles$data %>% 
  mutate(publication_date = ymd_hms(publication_date),
         pub_year = year(publication_date),
         research_article = article_type == "Research Article") %>% 
  group_by(pub_year, research_article) %>% 
  count() %>% 
  ggplot(aes(x = pub_year, y = n, fill = research_article)) + 
  geom_col() + 
  labs(x = "A�o de publicacion", 
       y = "Numero de articulos publicados",
       fill = "Art�culo de investigaci�n") + 
  ggtitle("Art�culos del West Nile publicados en revistas PLoS",
          subtitle = "Basado en art�culos con 'West Nile' en el t�tulo")

# Determinaremos qu� revistas han publicado estos art�culos y el n�mero de art�culos publicados en cada revista:

library(forcats)
library(stringr)
wn_papers_titles$data %>% 
  mutate(journal = str_replace(journal, "PLOS", "PLoS")) %>% 
  group_by(journal) %>% 
  count() %>% 
  ungroup() %>% 
  filter(!is.na(journal)) %>% 
  arrange(desc(n))

#  Con su grupo, cree un script R que siga todos los pasos descritos hasta ahora 
# para extraer los datos de seguimiento de huracanes desordenados en l�nea y 
# limpiarlos.

# Luego, pruebe los siguientes pasos de limpieza adicionales:

# Seleccione solo las columnas con fecha, hora, estado de la tormenta, ubicaci�n (latitud y longitud), vientos m�ximos sostenidos y presi�n m�nima y cambie el nombre de las mismas.
# Crea una columna con la fecha y hora de cada observaci�n, en una clase de fecha y hora.
# Limpie la latitud y la longitud para que tengas columnas separadas para los valores num�ricos y para el indicador de direcci�n (por ejemplo, N, S, E, W)
# Limpiar la columna de viento, por lo que da la velocidad del viento como un n�mero y NAen los casos en los que falta la velocidad del viento.
# Intente averiguar qu� significan las abreviaturas status. Cree una nueva columna de factor nombrada status_longcon el estado detallado.

