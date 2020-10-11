##### LOADING PACKAGES #####

pkgs = c('data.table', 'dplyr', 'SoDA', 'vroom')
lapply(pkgs, library, character.only = TRUE)

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

#### saving data as RDS file ###

saveRDS(df, 'C:/MAREK/PRACA/aplikacje/Appsilon/ships.rds')
