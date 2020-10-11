##### LOADING PACKAGES #####

pkgs = c('data.table', 'dplyr', 'htmltools', 'leaflet', 
         'lubridate', 'shiny', 'shinyWidgets', 'SoDA')
lapply(pkgs, library, character.only = TRUE)

##### DEFINING CUSTOM FUNCTIONS #####

#### relevant() filters out records with low speed or short time interval ####

relevant = function(data, min_interval = 15, min_speed = 2) {
  res = data[data$INTERVAL >= min_interval & data$KMH >= min_speed, ]
  return(res)
}

#### fastest() returns a record of highest speed ####

fastest = function(data, ...) {
  rel = relevant(data)
  if(nrow(rel) >= 1) {
    row_max = which(rel$KMH == max(rel$KMH))
    res = rel[max(row_max), ]
  } else res = NULL
  return(res)
}

#### list_names_by_type() lists all ship names within selected vessel type ####

rel = relevant(df)
doubtful_ships = rel[rel$KMH > 100, 'NAME'] %>% 
  unlist %>% 
  as.character %>% 
  unique

list_names_by_type = function(type, exclude_doubtful = TRUE, max_speed = 100, data = df) {
  res = unique(data[data$TYPE == type, ]$NAME)
  if (exclude_doubtful) res = setdiff(res, doubtful_ships)
  return(res)
}

#### annotate_ship_movement() creates a note to be used on a map ####

annotate_ship_movement = function(shipname, data = df) {
  data_sub = data %>% filter(NAME == shipname)
  data_max = fastest(data_sub)
  if (!is.null(data_max)) {
    note = paste0('</p><p><p>', shipname, ':</p><p>', round(data_max$DIST), 
                  ' m in ', data_max$INTERVAL, ' secs (max speed: ', 
                  round(data_max$KMH, 1), ' km/h)</p>')
  } else note = paste0('</p><p><p>', shipname, ':</p><p>parked (max speed < 2 km/h)</p>')
  return(note)
}

#### map_ship_movement() prepares a leaflet map ####

map_ship_movement = function(shipname, data = df) {
  data_sub = data %>% filter(NAME == shipname)
  data_max = fastest(data_sub)
  note = annotate_ship_movement(shipname)
  map = leaflet() %>%
    addTiles() %>%
    addPolylines(data = data_sub, lng = ~LON, lat = ~LAT, color = 'black', weight = 1) %>%
    addCircles(data = data_sub, lng = ~LON, lat = ~LAT, color = 'black', weight = 1) %>% 
    addRectangles(lng1 = min(data_sub$LON) - 0.02, lat1 = min(data_sub$LAT) - 0.005, 
                  lng2 = max(data_sub$LON) + 0.02, lat2 = max(data_sub$LAT) + 0.005, 
                  color = 'transparent', fillColor = 'transparent', label = HTML(note))
  if (!is.null(data_max)) {
    coords = rbind(data_max %>% select(LAT = LAT_from, LON = LON_from),
                   data_max %>% select(LAT, LON))
    map = map %>%
      addPolylines(data = coords, lng = ~LON, lat = ~LAT, color = 'red', weight = 3) %>%
      addCircles(data = coords, lng = ~LON, lat = ~LAT, color = 'red')
  }
  return(map)
}

##### READING DATA #####

df = fread('C:/MAREK/PRACA/! aplikacje !/Appsilon/ships.csv', encoding = 'UTF-8')

##### DATA PREPARATION #####

#### replacing misspelled ship names ####

df[df$SHIPNAME == '. PRINCE OF WAVES', 'SHIPNAME'] = 'PRINCE OF WAVES'
df[df$SHIPNAME == '.WLA-311', 'SHIPNAME'] = 'WLA-311'

#### sorting and selecting key variables ####

df = df %>% 
  mutate(NAME = paste0(SHIPNAME, ' (ID: ', SHIP_ID, ')')) %>% 
  select(TYPE = ship_type, NAME, DATETIME, LAT, LON) %>% 
  setorder(TYPE, NAME, DATETIME)

#### parsing time & calculating intervals ####

df$TIME = lubridate::parse_date_time2(df$DATETIME, 'Ymd HMS')
df$INTERVAL = c(NA, diff(df$TIME))
df$is_first = df$NAME != lag(df$NAME)
df$INTERVAL = ifelse(df$is_first, NA, df$INTERVAL)

#### calculating distance & speed ####

df$LAT_from = ifelse(df$is_first, NA, lag(df$LAT))
df$LON_from = ifelse(df$is_first, NA, lag(df$LON))
df$DIST = SoDA::geoDist(df$LAT_from, df$LON_from, df$LAT, df$LON)
df$KMH = ifelse(df$INTERVAL == 0, NA, round(3.6 * df$DIST / df$INTERVAL, 2))

#### data grooming ####

df = df %>% select(TYPE, NAME, TIME, LON_from, LAT_from, LON, LAT, DIST, INTERVAL, KMH)

#### create handy objects ####

typelist = unique(df$TYPE)
namelist = unique(df$NAME)

##### RUNNING SHINY APP #####

ui = fluidPage(
  titlePanel('Maximum speeds of Baltic Sea ships'),
  sidebarLayout(
    sidebarPanel(width = 4,
                 selectInput(inputId = 'type', 
                             label = 'Select vessel type:', 
                             choices = typelist),
                 selectInput(inputId = 'name',
                             label = 'Select vessel name:',
                             choices = namelist),
                 tags$span('Exclude doubtful data?'),
                 switchInput(inputId = 'exclude_doubtful', 
                             label = '', 
                             value = TRUE,
                             onLabel = 'yes',
                             offLabel = 'no',
                             size = 'mini',
                             inline = TRUE),
                 helpText('Data are limited to December 13-20th, 2016')
    ),
    mainPanel(
      leafletOutput('map')
    )
  )
)

server = function(input, output, session) {
  namelist = reactive({
    list_names_by_type(type = input$type, exclude_doubtful = input$exclude_doubtful)
  })
  observeEvent(namelist(), {
    updateSelectInput(session = session, inputId = 'name', choices = namelist())
  })
  output$map = renderLeaflet({
    map_ship_movement(shipname = input$name, data = df)
  })
}

shinyApp(ui = ui, server = server)

##### TODOs #####

#### problemy z danymi ####

# 1. zaadresowac problem wielu nazw przypisanych do jednego ID (np. 4666609)
# 2. ulepszyc filtr wylaczajacy 'doubtful data' (duze skoki predkosci)

#### implementacja w Shiny / grafika / deployment ####

# A. rozpoczynac od sytuacji, w ktorej nie jest wybrany zaden typ statku ani statek
# B. dostosowac graficznie do standardow Appsilon (biblioteka 'shiny.semantic 0.4.0')
# C. uniezaleznic od zrodla danych na dysku lokalnym
# D. zdeployowac na shiny.io
# E. przetestowac za pomoca pakietu 'testthat'
# F. wrzucic na Gita
# G. przyspieszyc dzialanie
# H. opisac napotkane problemy i sposoby ich rozwiazania i wyslac wraz z linkami do P.K.

#### statki nietypowe, dobre do testow ####

# parked ships:
# NBAAR MOON (ID: 364937)
# KBV 497 (ID: 327375)
# BOA BISON (ID: 2962)

# other ships:
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