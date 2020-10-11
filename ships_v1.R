##### LOADING PACKAGES #####

pkgs = c('data.table', 'dplyr', 'htmltools', 'leaflet', 'lubridate', 'shiny', 'SoDA')
lapply(pkgs, library, character.only = TRUE)

# library(janitor)
# library(microbenchmark)
# library(vroom)

##### DEFINING CUSTOM FUNCTIONS #####

shipnames_by_type = function(type) unique(df[df$ship_type == type, ]$SHIPNAME)

fastest = function(data) {
  sub = data[data$interval >= 15, ]
  if(nrow(sub) >= 1) {
    row_max = which(sub$speed == max(sub$speed))
    res = sub[max(row_max), ]
  } else res = NULL
  return(res)
}

##### READING DATA #####

df = fread('C:/MAREK/PRACA/! aplikacje !/Appsilon/ships.csv', encoding = 'UTF-8')

##### DATA PREPARATION #####

#### sorting and selecting key variables ####

df = df %>% 
  select(ship_type, SHIPNAME, SHIP_ID, DATETIME, LAT, LON) %>% 
  setorder(ship_type, SHIP_ID, DATETIME)

#### dealing with time ####

df$DATETIME = parse_date_time2(df$DATETIME, 'Ymd HMS')
df$interval = c(NA, diff(df$DATETIME))
df$is_first = df$SHIP_ID != lag(df$SHIP_ID)
df$interval = ifelse(df$is_first, NA, df$interval)

#### calculating distance & speed ####

df$LAT_from = ifelse(df$is_first, NA, lag(df$LAT))
df$LON_from = ifelse(df$is_first, NA, lag(df$LON))
df$distance = geoDist(df$LAT_from, df$LON_from, df$LAT, df$LON)
df$speed = ifelse(df$interval == 0, NA, round(3.6 * df$distance / df$interval, 2))

#### data preview ####

df = df %>% 
  select(ship_type, SHIPNAME, SHIP_ID, DATETIME, interval, 
         LAT_from, LON_from, LAT, LON, distance, speed)

#### visualizing on a map ####

shipnames = df$SHIPNAME %>% unique
(shipname = sample(shipnames, 1))

df_sub = df %>% 
  filter(SHIPNAME == shipname) %>% 
  select(SHIPNAME, DATETIME, LON_from, LAT_from, LON, LAT, distance, interval, speed)
df_max = fastest(df_sub)

map = leaflet(options = leafletOptions(maxZoom = 15)) %>%
  addTiles() %>%
  addPolylines(data = df_sub, lng = ~LON, lat = ~LAT, color = 'black', weight = 1) %>% 
  addCircles(data = df_sub, lng = ~LON, lat = ~LAT, color = 'black', weight = 1)

if (!is.null(df_max)) {
  coords = rbind(df_max %>% select(LAT = LAT_from, LON = LON_from), df_max %>% select(LAT, LON))
  note = paste0('<p>', shipname, ':</p><p>', round(df_max$distance), ' m in ', df_max$interval,
                ' secs (max speed: ', round(df_max$speed, 1), ' km/h) </p>')
  map = map %>% 
    addPolylines(data = coords, lng = ~LON, lat = ~LAT, color = 'red', weight = 3) %>% 
    addCircles(data = coords, lng = ~LON, lat = ~LAT, color = 'red') %>% 
    addLabelOnlyMarkers(data = df_max, lng = ~LON, lat = ~LAT, label = HTML(note),
                        labelOptions = labelOptions(noHide = TRUE))
} else {
  note = paste0('<p>', shipname, ':</p><p> parked (max speed < 1 km/h) </p>')
  map = map %>%
    addLabelOnlyMarkers(data = df_sub[nrow(df_sub), ], lng = ~LON, lat = ~LAT, 
                        label = HTML(note), labelOptions = labelOptions(noHide = TRUE))
}

map

##### TODO #####

# 1. identyfikacja jednostek po SHIP_ID, nie po SHIPNAME (dorobic unikalne nazwy)
# 2. uzaleznic wielkosc kropek od poziomu zoomu (zamiast stosowac maxZoom = 1)
# 3. opakowac w aplikacje Shiny z nazwa statku wybierana z drop-downa
# 4. informacje o dystansie i szybkosci umiescic gdzie indziej, aby nie zaslaniala trasy
# 5. dostosowac graficznie do standardow Appsilon (fonty, kolory, schematyczna mapa)
# 6. uniezaleznic od zrodla danych na dysku lokalnym
# 7. przetestowac za pomoca pakietu 'testthat'
# 9. zdeployowac na shiny.io i wyslac link Paulinie

##### REFINING QUESTIONS #####

# # 1. Niektorym wartosciom SHIPNAME odpowiada wiecej niz jedna wartosc SHIP_ID
# # Czy obserwacje o tym samym SHIPNAME ale roznych SHIP_ID to rozne jednostki 
# # o identycznej nazwie (ktore nalezy rozdzielic), czy jedna jednostka, a SHIP_ID 
# # nie jest identyfikatorem jednostki, tylko np. identyfikatorem rejsu?

# # skrypt identyfikujacy nazwy jednostek, ktorym odpowiada kilka identyfikatorow:
# shipnames = df$SHIPNAME %>% unique %>% sort
# for (shipname in shipnames) {
#   ids = df %>% filter(SHIPNAME == shipname) %>% select(SHIP_ID) %>% unique
#   if (nrow(ids) > 1) print(paste0(shipname, ': ', ids))
# }
# ship_id_by_name = function(name) unique(df[df$SHIPNAME == name, ]$SHIP_ID)
# ship_id_by_name('[SAT-AIS]')

# # 2. Interwaly czasowe miedzy pomiarami polozenia statku nie wynosza, jak w tresci 
# # zadania, 30 sekund, ale ok. 120 sekund, z tym ze czesto wystepuja odstepstwa od 
# # tej reguly: sygnal geolokalizacji bywa wysylany na przemian z dwoch lokalizatorow 
# # (efekt: interwaly np. 115 - 5 - 115 - 5 etc.), a czasami brakuje jednego lub 
# # wiekszej liczby sygnalow (efekt: interwaly od 240 sekund do nawet kilku godzin).
# # W tej sytuacji warto ustalic, czy slepo trzymamy sie specyfikacji (najwieksze
# # dystanse zostana wowczas oznaczone w sytuacjach najdluzszej utraty lacznosci),
# # czy refine'ujemy zadanie zgodnie z domyslnym celem. Proponuje oznaczac na mapie: 
# # dystans przeplyniety z najwieksza srednia predkoscia, wyliczona na podstawie 
# # odleglosci oraz interwalu czasowego miedzy kolejnymi sygnalami. Dla interwalu
# # czasowego warto zalozyc dolna granice, np. 15 sekund, by wykluczyc bledy pomiaru.
