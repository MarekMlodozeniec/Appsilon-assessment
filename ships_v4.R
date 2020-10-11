##### LOADING PACKAGES #####

pkgs = c('data.table', 'dplyr', 'htmltools', 'leaflet', 
         'shiny', 'shinyWidgets', 'SoDA', 'vroom')
lapply(pkgs, library, character.only = TRUE)

##### DEFINING CUSTOM FUNCTIONS #####

#### relevant() filters out records with low speed or short time interval ####

relevant = function(data, min_interval = 15, min_speed = 2) {
  res = data[data$INTERVAL >= min_interval & !is.na(data$KMH) & data$KMH >= min_speed, ]
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

list_names_by_type = function(type, exclude_doubtful = TRUE, max_speed = 100, data = df) {
  rel = relevant(data)
  doubtful_ships = rel[rel$KMH > 100, 'NAME'] %>% 
    unlist %>% 
    as.character %>% 
    unique
  res = unique(data[data$TYPE == type, ]$NAME)
  if (exclude_doubtful) res = setdiff(res, doubtful_ships)
  return(res)
}

#### annotate_ship_movement() creates a note to be used on a map ####

annotate_ship_movement = function(shipname = '', data = df) {
  note = 'Select ship type, then ship name from drop-down lists on the left<br>
          to see its movement, with the fastest part of the cruise marked red.'
  if (shipname %in% names) {
    data_sub = data %>% filter(NAME == shipname)
    data_max = fastest(data_sub)
    if (!is.null(data_max)) {
      note = paste0(shipname, '<br>', round(data_max$DIST), ' m in ', data_max$INTERVAL, 
                    ' secs (max speed: ', round(data_max$KMH, 1), ' km/h)')
    } else note = paste0(shipname, ':<br>parked (max speed < 2 km/h)')
  }
  return(note)
}

#### map_ship_movement() prepares a leaflet map ####

map_ship_movement = function(shipname = '', data = df) {
  note = annotate_ship_movement(shipname)
  map = leaflet() %>%
    addTiles()
  if (shipname %in% names) {
    data_sub = data[data$NAME == shipname, ]
    data_max = fastest(data_sub)
    map = map %>%
      addPolylines(data = data_sub, lng = ~LON, lat = ~LAT, color = 'black', weight = 1) %>%
      addRectangles(lng1 = min(data_sub$LON) - 0.02, lat1 = min(data_sub$LAT) - 0.005, 
                    lng2 = max(data_sub$LON) + 0.02, lat2 = max(data_sub$LAT) + 0.005, 
                    color = 'transparent', fillColor = 'transparent', label = HTML(note))
    if (!is.null(data_max)) {
      coords = rbind(data_max %>% select(LAT = LAT_from, LON = LON_from),
                     data_max %>% select(LAT, LON))
      map = map %>%
        addPolylines(data = coords, lng = ~LON, lat = ~LAT, color = 'red', weight = 3) %>%
        addCircles(data = coords, lng = ~LON, lat = ~LAT, color = 'red')
    } else { # is.null(data_max)
      map = map %>% addCircles(data = data_sub[nrow(data_sub), ], 
                               lng = ~LON, lat = ~LAT, color = 'red')
    }
  } else { # !(shipname %in% names)
    map = map %>% 
      setView(lng = mean(data$LON), lat = mean(data$LAT), zoom = 05) %>% 
      addRectangles(lng1 = -90, lat1 = 0, lng2 = 90, lat2 = 90, 
                    color = 'transparent', fillColor = 'transparent', label = HTML(note))
  }
  return(map)
}

##### READING DATA #####

df = vroom('C:/MAREK/PRACA/aplikacje/Appsilon/ships.csv')

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

df$INTERVAL = c(NA, diff(df$DATETIME))
df$is_first = (df$NAME != lag(df$NAME))
df$INTERVAL = ifelse(df$is_first, NA, df$INTERVAL)

#### calculating distance & speed ####

df$LAT_from = ifelse(df$is_first, NA, lag(df$LAT))
df$LON_from = ifelse(df$is_first, NA, lag(df$LON))
df$DIST = SoDA::geoDist(df$LAT_from, df$LON_from, df$LAT, df$LON)
df$KMH = ifelse(df$INTERVAL == 0, NA, round(3.6 * df$DIST / df$INTERVAL, 2))

#### data grooming ####

df = df %>% select(TYPE, NAME, LON_from, LAT_from, LON, LAT, DIST, INTERVAL, KMH)

#### create handy objects ####

types = unique(df$TYPE)
names = unique(df$NAME) %>% sort

##### RUNNING SHINY APP #####

#### ui.R ####

ui = fluidPage(
  titlePanel(p(h3('Maximum speeds of Baltic Sea ships'),
               h5(id = 'subtitle', '(December 13-20th, 2016)'),
               tags$style(HTML('#subtitle{color: grey;}')))),
  sidebarLayout(
    sidebarPanel(width = 4,
                 selectizeInput(inputId = 'type',
                                label = 'Select vessel type:',
                                choices = types,
                                options = list(
                                  placeholder = 'Select type',
                                  onInitialize = I('function() { this.setValue(""); }'))),
                 selectizeInput(inputId = 'name',
                                label = 'Select vessel name:',
                                choices = names,
                                options = list(
                                  placeholder = 'Select name (only after type is selected)',
                                  onInitialize = I('function() { this.setValue(""); }'))),
                 tags$span('Exclude doubtful data?'),
                 switchInput(inputId = 'exclude_doubtful',
                             label = '',
                             value = TRUE,
                             onLabel = 'yes',
                             offLabel = 'no',
                             onStatus = FALSE,
                             size = 'mini',
                             inline = TRUE),
                 helpText('Drag a cursor close to ship position to get more info')
    ),
    mainPanel(
      leafletOutput('map')
    )
  )
)

#### server.R ####

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

#### app.R ####

shinyApp(ui = ui, server = server)