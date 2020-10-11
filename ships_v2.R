##### LOADING PACKAGES #####

pkgs = c('data.table', 'dplyr', 'htmltools', 'leaflet', 'lubridate', 'shiny', 'SoDA')
lapply(pkgs, library, character.only = TRUE)

##### DEFINING CUSTOM FUNCTIONS #####

relevant = function(data, int_min = 15, spd_min = 1) {
  res = data[data$INTERVAL >= int_min & data$KMH >= spd_min, ]
  return(res)
}

fastest = function(data, ...) {
  rel = relevant(data)
  if(nrow(rel) >= 1) {
    row_max = which(rel$KMH == max(rel$KMH))
    res = rel[max(row_max), ]
  } else res = NULL
  return(res)
}

list_names = function(type, data = df, exclude_doubtful = TRUE) {
  res = unique(data[data$TYPE == type, ]$NAME)
  if (exclude_doubtful) {
    rel = relevant(data)
    doubtful_ships = rel[rel$KMH > 100, 'NAME'] %>% 
      unlist %>% 
      as.character %>% 
      unique
    res = setdiff(res, doubtful_ships)
  }
  return(res)
}

map_ship_movement = function(shipname, data = df) {
  data_sub = data %>% filter(NAME == shipname)
  data_max = fastest(data_sub)
  map = data_sub
  map = leaflet() %>%
    addTiles() %>%
    addPolylines(data = data_sub, lng = ~LON, lat = ~LAT, color = 'black', weight = 1) %>%
    addCircles(data = data_sub, lng = ~LON, lat = ~LAT, color = 'black', weight = 1)
  if (!is.null(data_max)) {
    coords = rbind(data_max %>% select(LAT = LAT_from, LON = LON_from),
                   data_max %>% select(LAT, LON))
    note = paste0('<p>', shipname, ':</p><p>', round(data_max$DIST),
                  ' m in ', data_max$INTERVAL, ' secs (max speed: ',
                  round(data_max$KMH, 1), ' km/h) </p>')
    map = map %>%
      addPolylines(data = coords, lng = ~LON, lat = ~LAT, color = 'red', weight = 3) %>%
      addCircles(data = coords, lng = ~LON, lat = ~LAT, color = 'red') %>%
      addLabelOnlyMarkers(data = data_max, lng = ~LON, lat = ~LAT, label = HTML(note),
                          labelOptions = labelOptions(noHide = TRUE))
  } else {
    note = paste0('<p>', shipname, ':</p><p> parked (max speed < 1 km/h) </p>')
    map = map %>%
      addLabelOnlyMarkers(data = data_sub[nrow(data_sub), ], lng = ~LON, lat = ~LAT,
                          label = HTML(note), labelOptions = labelOptions(noHide = TRUE))
  }
  return(map)
}

##### READING DATA #####

df = fread('C:/MAREK/PRACA/! aplikacje !/Appsilon/ships.csv', encoding = 'UTF-8')

##### DATA PREPARATION #####

#### sorting and selecting key variables ####

df = df %>% 
  mutate(NAME = paste0(SHIPNAME, ' (ID: ', SHIP_ID, ')')) %>% 
  select(TYPE = ship_type, NAME, DATETIME, LAT, LON) %>% 
  setorder(TYPE, NAME, DATETIME)

#### dealing with time ####

df$TIME = lubridate::parse_date_time2(df$DATETIME, 'Ymd HMS')
df$INTERVAL = c(NA, diff(df$TIME))
df$is_first = df$NAME != lag(df$NAME)
df$INTERVAL = ifelse(df$is_first, NA, df$INTERVAL)

#### calculating distance & speed ####

df$LAT_from = ifelse(df$is_first, NA, lag(df$LAT))
df$LON_from = ifelse(df$is_first, NA, lag(df$LON))
df$DIST = SoDA::geoDist(df$LAT_from, df$LON_from, df$LAT, df$LON)
df$KMH = ifelse(df$INTERVAL == 0, NA, round(3.6 * df$DIST / df$INTERVAL, 2))

#### labeling doubtful data ####

#### data grooming ####

df = df %>%
  select(TYPE, NAME, TIME, LON_from, LAT_from, LON, LAT, DIST, INTERVAL, KMH)

#### test: selecting random vessel and displaying a map ####

typelist = unique(df$TYPE)
(type = sample(typelist, 1))
namelist = list_names(type)
(name = sample(namelist, 1))
map_ship_movement(name)

##### TODOs #####

#### problemy z danymi ####

# 1. zaadresowac problem wielu nazw przypisanych do jednego ID (np. 4666609)
# 2. ulepszyc filtr wylaczajacy 'doubtful data' (duze skoki predkosci)

#### kwestie wygladu ####

# A. uzaleznic wielkosc kropek od poziomu zoomu (zamiast stosowac maxZoom = 1)
# B. opakowac w aplikacje Shiny z nazwa statku wybierana z drop-downa
# C. informacje o dystansie i szybkosci umiescic gdzie indziej, aby nie zaslaniala trasy
# D. dostosowac graficznie do standardow Appsilon (fonty, kolory, schematyczna mapa)

#### deployment ####

# I. uniezaleznic od zrodla danych na dysku lokalnym
# II. przetestowac za pomoca pakietu 'testthat'
# III. zdeployowac na shiny.io

#### statki nietypowe, dobre do testow ####

# VIKING INGVAR (ID: 349456)
# PILOT-5 (ID: 315705)
# VIKHREVOY (ID: 346012)
# OCEAN RAINBOW (ID: 459517)
# BALTICA (ID: 148995) + BALTICA (ID: 316599)
# BLACKPEARL 7.3V (ID: 4666609) + BLACKPEARL 7.4V (ID: 4666609) +
#   BLACKPEARL 7.5V (ID: 4666609) + BLACKPEARL 7.6V (ID: 4666609) + 
#   BLACKPEARL 7.7V (ID: 4666609) + BLACKPEARL 7.8V (ID: 4666609)
# najszybsze statki na poczatku zestawienia:
# max_speed = readRDS('C:/MAREK/PRACA/! aplikacje !/Appsilon/max_speed.rds')