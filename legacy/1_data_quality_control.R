#####DATA BASE FOR STANDARDIZATION UTPB#####
# tidy data, check errors and  split data by species #


# date start:        2014
# Last update: 04-02-2019
# many code string were copy from file "0_data_base_casgass.R" created by jotero
# updated data: "CONSULTA BDP_UTPB_CAPTURAS_01-02-2019.csv"
# (original: CONSULTA BDP_UTPB_CAPTURAS y CONSULTA TALLAS_01-02-2019.zip)
# General objective: to split data by species from monitoring program data base of UTPB
#
# Project: Convenio 2016-2019 XUNTA-CSIC
#
# Authors: Jaime Otero (jotero@iim.csic.es)
# Further modifications by: Alexandre Alonso (alex@iim.csic.es) and Jaime Otero (jotero@iim.csic.es)
# Institute of Marine Research (Vigo)



# 1 # LIBRARIES ---------------------------------------------

# keyboard shortcuts
# https://support.rstudio.com/hc/en-us/articles/200711853-Keyboard-Shortcuts
# Run current line/selection	Ctrl+Enter	Command+Enter
# Comment/uncomment current line/selection	Ctrl+Shift+C
# Reformat Selection	Ctrl+Shift+A	Command+Shift+A
# Insert code section	Ctrl+Shift+R	Command+Shift+R
# Insert pipe operator	Ctrl+Shift+M	Cmd+Shift+M

# tidy data
# guide for tidy data: https://r4ds.had.co.nz/
library(tidyverse)
# date-time formats
library(lubridate)

# multipanel plots
# https://github.com/thomasp85/patchwork
# https://gotellilab.github.io/GotelliLabMeetingHacks/NickGotelli/ggplotPatchwork.html
# devtools::install_github("thomasp85/patchwork")
library(patchwork)



# 2 # LOAD DATA ----------------------------------------------------------

# Load original data from utpb
utpb <- read_csv2("data\\CONSULTA BDP_UTPB_CAPTURAS_01-02-2019.csv")
utpb
names(utpb)

View(problems(utpb)) # SP_obj_2

# change inapropiate var names "Campa\xf1a
# related to encoding 
colnames(utpb)[35] <- "Survey"
colnames(utpb)[7] <- "N_TRIPUS"
colnames(utpb)[18] <- "LAT_inicio"
colnames(utpb)[19] <- "LON_inicio"
colnames(utpb)[20] <- "LAT_final"
colnames(utpb)[21] <- "LON_final"
str(utpb)
View(utpb)
dim(utpb)



# 3 # QUALITY CONTROL ------------------------------------------------

# 3.1. Date-Time ----------------------------------------------------------
# https://r4ds.had.co.nz/dates-and-times.html

# dia = dia muestreo
# FLARG = final largada
# FVIR = final virada
# HorafL = final largada
# HorafV = final virada  

names(utpb)
utpb %>%
  select(dia, FLARG:HorafV)

utpb <- utpb %>%
  mutate(
    dia =  dmy_hms(dia),
    FLARG = dmy_hms(FLARG),
    HorafL = dmy_hms(HorafL),
    FVIR = dmy_hms(FVIR),
    HorafV = dmy_hms(HorafV)) %>%
  mutate(Year = year(dia),
         Month = month(dia),
         Julian = yday(dia)) 

utpb %>%
  select(Year, Month, Julian) %>%
  sapply(. %>% range) 

# 3.2. Soaktime -----------------------------------------------------------

dim(utpb)
# 864633     41

length(which(is.na(utpb$HorafL)))  #556564
length(which(is.na(utpb$FLARG)))   #18398
length(which(is.na(utpb$HorafV)))  #34311
length(which(is.na(utpb$FVIR)))    #105

# soaktime usando datos de inicio de virada (HorafV) y fin de largada (FLARG) 
utpb <- utpb %>%
  mutate(
    # new var fin largada donde usaremos HorafL excepto cuando NA, then, FLARG
    FinLargada = ymd_hms(if_else(
      is.na(utpb$HorafL),
      as.character(FLARG),
      as.character(HorafL)
    )),
    # new var para calcular inicio largada
    InitLargada = ymd_hms(if_else(
      is.na(FLARG), as.character(HorafL), as.character(FLARG)
    )),
    # si fin largada es anterior a inicio largada sustituimos
    LARGADA = ymd_hms(
      if_else(
        InitLargada > FinLargada,
        as.character(FinLargada),
        as.character(InitLargada)
      )
    ),
    
    # new var init virada donde usaremos FVIR excepto cuando NA, then, HorafV
    InitVirada = ymd_hms(if_else(
      is.na(FVIR), as.character(HorafV), as.character(FVIR)
    )),
    # new var para calcular fin virada
    FinVirada = ymd_hms(if_else(
      is.na(HorafV), as.character(FVIR), as.character(HorafV)
    )),
    # si fin virada es anterior a inicio virada sustituimos
    VIRADA = ymd_hms(if_else(
      FinVirada < InitVirada,
      as.character(InitVirada),
      as.character(FinVirada)
    ))
  )


View(
  utpb %>%
    filter(LARGADA < InitLargada) %>%
    select(InitLargada, FinLargada, LARGADA, dia, Idlance)
)

# bad correspondence with year
utpb <- utpb %>%
  mutate(LARGADA = ymd_hms(if_else(
    LARGADA < InitLargada,
    as.character(InitLargada),
    as.character(LARGADA)
  )))

utpb %>%
  filter(VIRADA < LARGADA) %>%
  select(LARGADA, VIRADA, mcarte1) # ok

# estimate soaktime
utpb <- utpb %>%
  mutate(Soaktime = as.numeric(VIRADA - LARGADA)) %>%
  mutate(Soaktime = if_else(is.na(Soaktime), mcarte1, Soaktime))

dim(utpb)                                     # 864633     48
length(which(is.na(utpb$mcarte1)))            #13
length(which(is.na(utpb$Soaktime)))           #13
length(which(utpb$mcarte1 < 0))               #0
length(which(utpb$Soaktime < 0))              #0
length(which(utpb$Soaktime == utpb$mcarte1))  #60456
length(which(utpb$mcarte1 == 0))              #0
length(which(utpb$Soaktime == 0))             #0
length(which(utpb$Soaktime > utpb$mcarte1))   #803315
length(which(utpb$Soaktime < utpb$mcarte1))   #849

utpb %>% 
  select(Soaktime,mcarte1) %>% 
  ggplot(aes(x=mcarte1,y=Soaktime))+geom_point()+geom_line()
# extreme values of soaktime and mcarte1?

length(which(is.na(utpb$mcarte1)))  #13
length(which(is.na(utpb$Soaktime))) #13

# overestimate
p1 <- utpb %>%
  filter(Soaktime > mcarte1) %>%
  ggplot(aes(x = mcarte1, y = Soaktime)) + geom_point() + geom_line() +
  ggtitle("overestimate")
# underestimate
p2 <- utpb %>%
  filter(Soaktime < mcarte1) %>%
  ggplot(aes(x = mcarte1, y = Soaktime)) + geom_point() + geom_line() +
  ggtitle("underestimate")
# errors >10%
p3 <- utpb %>%
  mutate(soakdif = abs(100 * (Soaktime - mcarte1) / mcarte1)) %>%
  filter(soakdif > 10) %>%
  select(LARGADA, VIRADA, Soaktime, mcarte1, soakdif) %>%
  ggplot(aes(x = mcarte1, y = Soaktime)) + geom_point() + geom_line()

(p1 | p2) / p3

utpb %>%
  summarise(
    minutpb = min(mcarte1, na.rm = T),
    maxutpb = max(mcarte1, na.rm = T),
    minus = min(mcarte1, na.rm = T),
    maxus = max(mcarte1, na.rm = T)
  ) 

View(
  utpb %>%
    filter(mcarte1 < 100 & Soaktime > 1000) %>%
    select(LARGADA, VIRADA, Soaktime, mcarte1, Idlance,dia)
)

# No encuentro un criterio claro para corregir
# solución: mantener las dos estimas de tiempo de calado
# se debe chequear las diferencias entre soaktime y mcarte1 en
# las base de datos definitivas para los análisis por especie/arte

# 3.3. Factors ------------------------------------------------------------

#problems with encoding
utpb %>% 
  select(ZONA, PUERTO_EMBARQUE, ARTE, Modalidad_ARTE, Metier_palangrillo, valor, ESPECIE)

utpb <- utpb %>%
  mutate(
    ZONA = parse_character(ZONA, locale = locale(encoding = "ASCII")),
    PUERTO_EMBARQUE = parse_character(PUERTO_EMBARQUE, locale = locale(encoding = "ASCII")),
    ARTE = parse_character(ARTE, locale = locale(encoding = "ASCII")),
    Modalidad_ARTE = parse_character(Modalidad_ARTE, locale = locale(encoding = "ASCII"))
  ) %>%
  # q=nh
  mutate(
    ZONA = str_replace(ZONA, "q", "nh"),
    PUERTO_EMBARQUE = str_replace(PUERTO_EMBARQUE, "q", "nh"),
    ARTE = str_replace(ARTE, "Q", "NH"),
    Modalidad_ARTE = str_replace(Modalidad_ARTE, "Q", "NH")
  )

sort(unique(utpb$ZONA))
sort(unique(utpb$PUERTO_EMBARQUE))
sort(unique(utpb$ARTE))
sort(unique(utpb$Modalidad_ARTE))
sort(unique(utpb$Metier_palangrillo))
sort(unique(utpb$valor))
sort(unique(utpb$ESPECIE))

utpb <- utpb %>%
  mutate(ARTE = if_else(ARTE == "NASA NICORA E CAMARSN", "NASA NECORA E CAMARON", ARTE))

# reducir levels in valor/seafloor
utpb <- utpb %>%
  mutate(seafloor = factor(
    if_else(
      valor == "piedra" |
        valor == "roca y algas" |
        valor == "piedra dura",
      "hard",
      if_else(
        valor == "cascos de barcos y/o bateas" |
          valor == "piedra y arena" |
          valor == "algas, roca y arena" |
          valor == "piedra y fango" |
          valor == "fango-pedra-cascallo" |
          valor == "piedra y coral" |
          valor == "pedra-cascallo" |
          valor == "arena-coral-piedra" |
          valor == "cascajo con arena y piedras",
        "mixed",
        "soft"
      )
    )
  ))

sort(unique(utpb$seafloor))
table(utpb$valor,utpb$seafloor)
length(which(is.na(utpb$valor)))    #12543
length(which(is.na(utpb$seafloor))) #12543

# change characters to factor
utpb <- utpb %>%
  mutate(
    ZONA = factor(ZONA),
    PUERTO_EMBARQUE = factor(PUERTO_EMBARQUE),
    ARTE = factor(ARTE),
    Modalidad_ARTE = factor(Modalidad_ARTE),
    Metier_palangrillo = factor(Metier_palangrillo),
    valor = factor(valor),
    ESPECIE = factor(ESPECIE)
  )

utpb %>%
  select(
    ZONA,
    PUERTO_EMBARQUE,
    ARTE,
    Modalidad_ARTE,
    Metier_palangrillo,
    valor,
    ESPECIE,
    seafloor
  ) %>%
  str()

# 3.4. Depth --------------------------------------------------------------

utpb %>%
  select(PROFMax, PROFMin) %>% 
  filter(PROFMax < PROFMin) # no aparente errors, so...

utpb <- utpb %>%
  mutate(Depth = (PROFMax + PROFMin) / 2)

utpb %>%
  select(PROFMax, PROFMin, Depth)
utpb %>%
  filter(Depth == 0) %>%
  select(PROFMax, PROFMin, Depth)
utpb %>%
  filter(PROFMax < Depth) %>%
  select(PROFMax, PROFMin, Depth)
utpb %>%
  filter(PROFMin > Depth) %>%
  select(PROFMax, PROFMin, Depth)

p4 <- utpb %>%
  ggplot(aes(x = Depth, y = PROFMin)) +
  geom_point() +
  geom_smooth(method = "lm", se = F)
p5 <- utpb %>%
  ggplot(aes(x = Depth, y = PROFMax)) +
  geom_point() +
  geom_smooth(method = "lm", se = F)

p4 | p5

# 3.5. Catch data ---------------------------------------------------------

# pasar NAs a ceros
# (asumiendo que no data==0 captura, este punto es crítico ya que no siempre es así)
summary(utpb$NUMc)  #781250 NAs NUMc
summary(utpb$NUMd)  #818529 NAs NUMd
summary(utpb$PESOc) #781264 NAs PESOc
summary(utpb$PESOd) #818582 NAs PESOd

head(table(utpb$NUMc))  #NO zeros
head(table(utpb$NUMd))  #NO zeros
head(table(utpb$PESOc)) #NO zeros
head(table(utpb$PESOd)) #NO zeros

#pasamos todos los NAs a zeros
utpb <- utpb %>%
  mutate(
    NUMc = if_else(is.na(NUMc), 0, NUMc),
    NUMd = if_else(is.na(NUMd), 0, NUMd),
    PESOc = if_else(is.na(PESOc), 0, PESOc),
    PESOd = if_else(is.na(PESOd), 0, PESOd)
  ) 

head(table(utpb$NUMc))  #781250 zeros (OK)
head(table(utpb$NUMd))  #818529 zeros (OK)
head(table(utpb$PESOc)) #781264 zeros (OK)
head(table(utpb$PESOd)) #818582 zeros (OK)

# false zeros
#no se anoto captura => NUMc=NA
utpb %>% filter(NUMc == 0 & PESOc > 0) %>% select(NUMc, PESOc) #correct
#no se anoto peso => PESOc=NA
utpb %>% filter(NUMc > 0 & PESOc == 0) %>% select(NUMc, PESOc) #14
utpb <- utpb %>% mutate(PESOc = ifelse(NUMc > 0 & PESOc == 0, NA, PESOc))
#no se anoto captura => NUMd=NA
utpb %>% filter(NUMd == 0 & PESOd > 0) %>% select(NUMd, PESOd) #2
utpb <- utpb %>% mutate(NUMd = ifelse(NUMd == 0 & PESOd > 0, NA, NUMd))
#no se anoto peso => PESOd=NA
utpb %>% filter(NUMd > 0 & PESOd == 0) %>% select(NUMd, PESOd) #55
utpb <- utpb %>% mutate(PESOd = ifelse(NUMd > 0 & PESOd == 0, NA, PESOd))

summary(utpb$NUMc)  #0  NAs
summary(utpb$NUMd)  #2  NAs
summary(utpb$PESOc) #14 NAs 
summary(utpb$PESOd) #55 NAs 

# combinar datos capturas y descartes

utpb <- utpb %>% mutate(Ntot = NUMc + NUMd, Wtot = (PESOc + PESOd) / 1000)

summary(utpb$Ntot) #2 NAs
summary(utpb$Wtot) #68 NAs
length(which(utpb$Wtot==0))
length(which(utpb$Ntot==0))
dim(utpb)

# CPUE

summary(utpb$Piezas)     #0 NAs
head(table(utpb$Piezas)) 

utpb <- utpb %>%
  mutate(Piezas = if_else(is.na(Piezas), 0, Piezas)) %>%
  mutate(Piezas = if_else(Piezas == 0.5, 1, Piezas)) %>%
  mutate(Piezas = if_else(Piezas == 1.5, 2, Piezas)) %>%
  mutate(Piezas = if_else(Piezas == 2.5, 3, Piezas)) %>%
  mutate(Piezas = if_else(Piezas == 3.5, 4, Piezas)) %>%
  mutate(Piezas = if_else(Piezas == 4.5, 5, Piezas)) %>%
  mutate(Piezas = if_else(Piezas == 5.5, 6, Piezas)) %>%
  mutate(
    CPUEct = Ntot / (Piezas * (Soaktime / 60)),
    CPUEwt = Wtot / (Piezas * (Soaktime / 60)),
    CPUEc = Ntot / Piezas,
    CPUEw = Wtot / Piezas
  )

p6 <- utpb %>%
  ggplot(aes(x = CPUEc, y = CPUEw)) +
  geom_point() +
  geom_smooth(method = "lm", se = F) +
  ggtitle("catch/s")
p7 <- utpb %>%
  ggplot(aes(x = CPUEct, y = CPUEwt)) +
  geom_point() +
  geom_smooth(method = "lm", se = F) +
  ggtitle("catch/(s*t)")

p6 | p7

# 3.6. Georeference data --------------------------------------------------

# create function to transform lat/lon to decimal coordinates
fLat <- function(x) {
  trunc(x / 100000) + ((x - (100000 * trunc(x / 100000))) / 1000) / 60
}

fLon <- function(x) {
  -(trunc(x / 100000) + ((x - (
    100000 * trunc(x / 100000)
  )) / 1000) / 60)
}

summary(utpb$LAT_inicio) #18455 NAs
summary(utpb$LON_inicio) #18485 NAs
summary(utpb$LAT_final)  #19263 NAs
summary(utpb$LON_final)  #19283 NAs

# combinar LAT.inicio y LON.inicio con LAT.final y LON.final
# cambiar unidades de las coordenadas a decimales

utpb <- utpb %>%
  mutate(
    LATdef = if_else(is.na(LAT_inicio), LAT_final, LAT_inicio),
    LONdef = ifelse(is.na(LON_inicio), LON_final, LON_inicio)
  ) %>%
  mutate(Lat = fLat(LATdef), Lon = fLon(LONdef))

summary(utpb$Lat)  #16157 NAs
summary(utpb$Lon)  #16157 NAs

p8 <- utpb %>%
  ggplot(aes(x = Lon, y = Lat, col = ZONA)) + geom_point() + coord_fixed(1.3)

p8

#puntos interiores
utpb %>%
  filter(ZONA == "Zona V - Fisterra" & Lon > (-8.75) & Lat < 43) %>%
  select(LONdef, LATdef, Lon, Lat, Idlance) #1596156547
utpb %>%
  filter(Lon > (-2.5) ) %>%
  select(LONdef, LATdef, Lon, Lat, Idlance) #581756450
utpb %>%
  filter(ZONA == "Zona III - Arousa" & Lon > (-8.7)) %>%
  select(LONdef, LATdef, Lon, Lat, Idlance) #3742

utpb <- utpb %>%
  mutate(Lon = if_else(Idlance == 1596156547, fLon(914279), Lon)) %>%
  mutate(Lon = if_else(Idlance == 581756450, fLon(904515), Lon)) %>%
  mutate(Lon = if_else(Idlance == 3742, fLon(936144), Lon))

# Corregir datos raros fueron usando la profundidad de calado, el tipo de arte, puerto etc 
# (anotados por Alex). Los que ahora parece que quedan alejados creemos que 
# estan bien. No obstante hay errores en algunas de las clasificaciones por zonas
# que las arreglamos manualmente en funcion de la Lat y Lon

# known errors
# errores longitudes < -9.75
utpb %>%
  filter(Idlance == c(-2058898896, -1609890278, 1032298269, 912176706)) %>%
  select(LONdef, LATdef, Lon, Lat, Idlance,Depth) # aguas someras, ptos no coherentes
  # posiblemente el error esté en LONdef=9xxxxx => LONdef=8xxxxx

utpb <- utpb %>%
  mutate(Lon = if_else(Idlance == -2058898896, fLon(859760), Lon)) %>%
  mutate(Lon = if_else(Idlance == -1609890278, fLon(859753), Lon)) %>%
  mutate(Lon = if_else(Idlance == 1032298269, fLon(859952), Lon)) %>% 
  mutate(Lon = if_else(Idlance == 912176706, fLon(821124), Lon))

# obtenemos mapa
# linea de costa
galicia.coast <-
  read_csv2("data\\Galicia_coast.csv", col_names = c("lon", "lat"))
galicia.coast

galicia.coast <- galicia.coast %>%
  mutate(lon = ifelse(lon > -8.85 & lat < 42.1, NA, lon)) %>%
  mutate(lon = ifelse(lon > -8.45 & lat < 42.5, NA, lon))

# bathimetry
galicia.bathy <-
  read_table("data\\Galicia_bathy.txt", col_names = c("lon", "lat", "depth"))
galicia.bathy

galicia.bathy <-
  galicia.bathy %>% mutate(depth = ifelse(depth > 0, NA, depth))

galicia.bathy.mat<-matrix(galicia.bathy$depth,
                          nrow=length(unique(galicia.bathy$lon)),
                          ncol=length(unique(galicia.bathy$lat)))[,order(unique(galicia.bathy$lat))]

# mapa
pdf("plots\\map_sample_dist.pdf", width = 10, height = 10)

plot(
  galicia.coast,
  type = "n",
  xlim = c(-10.1,-7),
  ylim = c(41.9, 44.1),
  xlab = "Lonxitude",
  ylab = "Latitude",
  cex.lab = 2.5,
  lwd = 1.3,
  main = "Muestreo UTPB",
  cex.main = 2.5,
  axes = TRUE
)
points(galicia.coast, type = "l")
contour(
  unique(galicia.bathy$lon),
  sort(unique(galicia.bathy$lat)),
  galicia.bathy.mat,
  levels = -seq(0, 500, by = 50),
  labcex = 0.4,
  col = 'gray80',
  add = T
)
points(utpb$Lon,
       utpb$Lat,
       cex = 1,
       pch = 16,
       col = "light blue")

dev.off()

# Nueva asignacion de Zonas Administrativas y Zonas Oceanograficas
# Coordenadas decimales de los puntos de separación

BaresLat<-43.791336
BaresLon<--7.688769

PriorinhoLat<-43.463275
PriorinhoLon<--8.348032

LangosteiraLat<-43.361084
LangosteiraLon<--8.486756

TourinhanLat<-43.059324
TourinhanLon<--9.293350

InsuaLat<-42.770942
InsuaLon<--9.127085

SieiraLat<-42.653324
SieiraLon<--9.042177

FaxildaLat<-42.414997
FaxildaLon<--8.881116

SoavelaLat<-42.277784
SoavelaLon<--8.864851

# Zonas Administrativas

utpb <- utpb %>%
  # Zona 9 (A Mariña)
  mutate(ZoneA = ifelse(Lon >= BaresLon, 9, 8)) %>%
  # Zona 8 (Cedeira)
  mutate(ZoneA = ifelse(Lat < BaresLon &
                          Lat >= 43.51, 8, ZoneA)) %>%
  # Zona 7 (Coruña-Ferrol)
  mutate(ZoneA = ifelse(Lon >= LangosteiraLon &
                          Lat <= 43.51, 7, ZoneA)) %>%
  # Zona 6 (Costa da Morte)
  mutate(ZoneA = ifelse(Lon < LangosteiraLon &
                          Lat >= TourinhanLat, 6, ZoneA)) %>%
  # Zona 5 (Fisterra)
  mutate(ZoneA = ifelse(Lat < TourinhanLat &
                          Lat >= InsuaLat &
                          Lon <= -9.08, 5, ZoneA)) %>%
  # Zona 4 (Muros)
  mutate(ZoneA = ifelse(Lat < InsuaLat &
                          Lat >= SieiraLat, 4, ZoneA)) %>%
  # Zona 4 (Interior Ria Muros)
  mutate(ZoneA = ifelse(Lon > -9.08 &
                          Lon <= -8.8 &
                          Lat > InsuaLat & Lat < 42.85, 4, ZoneA)) %>%
  # Zona 4 (Interior Ria Muros)
  mutate(ZoneA = ifelse(Lat < SieiraLat &
                          Lat >= FaxildaLat, 3, ZoneA)) %>%
  # Zona 3 (Interior Ria Arousa)
  mutate(ZoneA = ifelse(Lon > -8.9 &
                          Lon <= -8.7 &
                          Lat > SieiraLat & Lat < 42.7, 3, ZoneA)) %>%
  # Zona 2 (Pontevedra)
  mutate(ZoneA = ifelse(Lat < FaxildaLat &
                          Lat >= SoavelaLat, 2, ZoneA)) %>%
  # Zona 2 (Interior Ria Pontevedra 1)
  mutate(ZoneA = ifelse(Lon > -8.75 &
                          Lon <= -8.65 &
                          Lat > FaxildaLat & Lat < 42.45, 2, ZoneA)) %>%
  # Zona 2 (Interior Ria Pontevedra 2)
  mutate(ZoneA = ifelse(Lon > -8.8229 &
                          Lon <= -8.822 &
                          Lat > 42.277 & Lat < 42.278, 2, ZoneA)) %>%
  # Zona 1 (Vigo)
  mutate(ZoneA = ifelse(Lat < SoavelaLat, 1, ZoneA)) %>%
  # Zona 1 (Interior Ria Vigo 1)
  mutate(ZoneA = ifelse(Lon > -8.68 &
                          Lon <= -8.5 &
                          Lat > SoavelaLat & Lat < 42.36, 1, ZoneA)) %>%
  # Zona 1 (Interior Ria Vigo 2)
  mutate(ZoneA = ifelse(Lon > -8.74 &
                          Lon <= -8.67 &
                          Lat > SoavelaLat & Lat < 42.3, 1, ZoneA)) %>%
  # is.na => asignar zona en función de puerto de embarque
  mutate(ZoneA = ifelse(is.na(ZoneA),
                        ifelse(
                          ZONA == "Zona I - Vigo",
                          1,
                          ifelse(
                            ZONA == "Zona II - Pontevedra",
                            2,
                            ifelse(
                              ZONA == "Zona III - Arousa",
                              3,
                              ifelse(
                                ZONA == "Zona IV - Muros",
                                4,
                                ifelse(
                                  ZONA == "Zona V - Fisterra",
                                  5,
                                  ifelse(
                                    ZONA == "Zona VI - Costa da Morte",
                                    6,
                                    ifelse(
                                      ZONA == "Zona VII - Corunha-Ferrol",
                                      7,
                                      ifelse(ZONA ==
                                               "Zona VIII - Cedeira", 8, 9)
                                    )
                                  )
                                )
                              )
                            )
                          )
                        ),
                        ZoneA))

utpb %>% 
  select(ZONA,ZoneA)
table(utpb$ZONA,utpb$ZoneA)

# Zonas Oceanograficas
utpb <- utpb %>%
  mutate(ZoneO = ifelse(ZoneA >= 8, 3, ifelse(ZoneA == 7 |
                                                ZoneA == 6, 2, 1)))
table(utpb$ZoneO,utpb$ZoneA)

# Zonas ICES
utpb <- utpb %>%
  mutate(ZoneI = factor(ifelse(ZoneO == 1, "9.a", "8.c")))
table(utpb$ZoneO, utpb$ZoneI)
table(utpb$ZoneA, utpb$ZoneI)
table(utpb$ZONA, utpb$ZoneI)

# Comprobar zonación

p9 <- utpb %>%
  ggplot(aes(x = Lon, y = Lat)) + geom_point(aes(colour = factor(ZoneA))) + coord_fixed(1.3)
p10 <- utpb %>%
  ggplot(aes(x = Lon, y = Lat)) + geom_point(aes(colour = factor(ZoneO))) + coord_fixed(1.3)
p11 <- utpb %>%
  ggplot(aes(x = Lon, y = Lat)) + geom_point(aes(colour = factor(ZoneO))) + coord_fixed(1.3)

p9 | p10 | p11

ggsave("plots\\map_zones.pdf", width = 15, height = 8)

# 3.7. Add environmental data ---------------------------------------------

oceano <- read_csv("data\\oceano_98-18.csv")
oceano

# Calculate environmental covariate for the day of catch
utpb$QX <- as.vector(rep(NA, dim(utpb)[1]))
for (i in 1:dim(utpb)[1]) {
  utpb$QX[i] <- oceano$qxAno[(which(oceano$Year == utpb$Year[i] &
                                      oceano$DoY ==
                                      utpb$Julian[i]))]
}

utpb$QY <- as.vector(rep(NA, dim(utpb)[1]))
for (i in 1:dim(utpb)[1]) {
  utpb$QY[i] <- oceano$qyAno[(which(oceano$Year == utpb$Year[i] &
                                      oceano$DoY ==
                                      utpb$Julian[i]))]
}

utpb$SST <- as.vector(rep(NA, dim(utpb)[1]))
for (i in 1:dim(utpb)[1]) {
  utpb$SST[i] <-
    if (is.na(utpb$ZoneA[i])) {
      NA
    }
  else {
    if (utpb$ZoneA[i] == "1") {
      oceano$sstAnoZ1[(which(oceano$Year == utpb$Year[i] &
                               oceano$DoY ==
                               utpb$Julian[i]))]
    }
    else {
      if (utpb$ZoneA[i] == "2") {
        oceano$sstAnoZ2[(which(oceano$Year == utpb$Year[i] &
                                 oceano$DoY ==
                                 utpb$Julian[i]))]
      }
      else {
        if (utpb$ZoneA[i] == "3") {
          oceano$sstAnoZ3[(which(oceano$Year == utpb$Year[i] &
                                   oceano$DoY ==
                                   utpb$Julian[i]))]
        }
        else {
          if (utpb$ZoneA[i] == "4") {
            oceano$sstAnoZ4[(which(
              oceano$Year == utpb$Year[i] &
                oceano$DoY ==
                utpb$Julian[i]
            ))]
          }
          else {
            if (utpb$ZoneA[i] == "5") {
              oceano$sstAnoZ5[(which(
                oceano$Year == utpb$Year[i] &
                  oceano$DoY ==
                  utpb$Julian[i]
              ))]
            }
            else {
              if (utpb$ZoneA[i] == "6") {
                oceano$sstAnoZ6[(which(
                  oceano$Year == utpb$Year[i] &
                    oceano$DoY ==
                    utpb$Julian[i]
                ))]
              }
              else {
                if (utpb$ZoneA[i] == "7") {
                  oceano$sstAnoZ7[(which(
                    oceano$Year == utpb$Year[i] &
                      oceano$DoY ==
                      utpb$Julian[i]
                  ))]
                }
                else {
                  if (utpb$ZoneA[i] == "8") {
                    oceano$sstAnoZ8[(which(
                      oceano$Year == utpb$Year[i] &
                        oceano$DoY ==
                        utpb$Julian[i]
                    ))]
                  }
                  else {
                    oceano$sstAnoZ9[(which(
                      oceano$Year == utpb$Year[i] &
                        oceano$DoY == utpb$Julian[i]
                    ))]
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}


utpb %>%
  select(QX, QY, SST)

# check environmental vars
length(which(is.na(utpb$QX)))  # 14822
length(which(is.na(utpb$QY)))  # 14822
length(which(is.na(utpb$SST))) # 0

# 3.8.Accomodate col types ------------------------------------------------

# names(utpb)
# utpb
# 
# # as integer to avoid problems with scientific notation
# # https://github.com/tidyverse/readr/issues/645
# utpb <- utpb %>% 
#   mutate(Idflota = parse_character(),
#          Idjornada = parse_character(),
#          Idlance = parse_character(),
#          Idarte = parse_character(),
#          Idnasa = parse_character(),
#          idespecie = parse_character(),
#          idarte = parse_character(),
#          Soaktime = parse_integer(),
#          mcarte1 = parse_integer(),
#          Piezas = parse_integer(),
#          Ntot = parse_integer(),
#          NUMc = parse_integer(),
#          NUMd = parse_integer(),
#          Wtot = parse_double(),
#          PESOc = parse_double(),
#          PESOd = parse_double()
#          )



# 4 # SPLIT DATA BY SPP -----------------------------------------------

# 4.1. Rename and select vars ---------------------------------------------

# vector of new names
names(utpb)
newnames <- c("Date", "Observer", "Idflota", "Zone", "Harbour", "TRB", "Crew", "Gear",
             "Idjornada", "Idlance", "Idnasa", "Pieces", "FLARG", "HorafL", "FVIR",
             "HorafV", "SoakUTPB", "LAT_inicio", "LON_inicio", "LAT_final", "LON_final",
             "Substrato", "PROFMax", "PROFMin", "PROFMED", "SP",
             "NUMc", "PESOc", "NUMd", "PESOd", "Metier", "MetierPAL", "Idarte", "Idespecie",
             "Survey", "MetierNecora", "SP_obj_1", "SP_obj_2", "Year", "Month", "Julian",
             "FinLargada", "InitLargada", "LARGADA", "InitVirada", "FinVirada", "VIRADA",
             "Soaktime", "seafloor", "Depth", "Ntot", "Wtot", "CPUEct", "CPUEwt", "CPUEc", "CPUEw",
             "LATdef", "LONdef", "Lat", "Lon", "ZoneA", "ZoneO", "ZoneI",
             "QX", "QY", "SST")

length(names(utpb))-length(newnames)

# rename
colnames(utpb) <- newnames 

ddbb_utpb <- utpb %>%
  select(SP, Ntot, Wtot, NUMc, PESOc, NUMd, PESOd, CPUEc, CPUEw,
         Idflota, Idjornada, Idlance, Idarte, Observer, Harbour,
         TRB, Crew, Gear, Metier, MetierPAL, MetierNecora, SP_obj_1, SP_obj_2,
         Survey, Date, Year, Month, Julian,
         Lat, Lon, Zone, ZoneA, ZoneO, ZoneI,
         Pieces, LARGADA, VIRADA, SoakUTPB, Soaktime,
         Depth, Substrato, seafloor,
         QX, QY, SST)

ddbb_utpb
names(ddbb_utpb)

# 4.2. save csv -----------------------------------------------------------

# export full data base
write_csv(ddbb_utpb, "data\\ddbb_utpb_9918.csv")

# 4.3. Split data by spp --------------------------------------------------

especie <- as.vector(sort(unique(ddbb_utpb$SP)))
especie
 
congrio <- utpb[utpb$SP == especie[1], ]
lubina <- utpb[utpb$SP == especie[2], ]
sargo <- utpb[utpb$SP == especie[3], ]
pinto <- utpb[utpb$SP == especie[4], ]
calamar <- utpb[utpb$SP == especie[5], ]
centolla <- utpb[utpb$SP == especie[6], ]
salmonete <- utpb[utpb$SP == especie[7], ]
necora <- utpb[utpb$SP == especie[8], ]
pulpo <- utpb[utpb$SP == especie[9], ]
platija <- utpb[utpb$SP == especie[10], ]
abadejo <- utpb[utpb$SP == especie[11], ]
raya <- utpb[utpb$SP == especie[12], ]
rodaballo <- utpb[utpb$SP == especie[13], ]
coruxo <- utpb[utpb$SP == especie[14], ]
pintaroja <- utpb[utpb$SP == especie[15], ]
choco <- utpb[utpb$SP == especie[16], ]
lengArena <- utpb[utpb$SP == especie[17], ]
lengSenegal <- utpb[utpb$SP == especie[18], ]
lenguado <- utpb[utpb$SP == especie[19], ]
faneca <- utpb[utpb$SP == especie[20], ]
 


# 5 # EXPLORATOTY ANALISYS ------------------------------------------------

# 5.1. Frequency gear/spp ----------------------------------------------------------

fanecas <-
  as.numeric(tapply(faneca$Ntot, faneca$Gear, sum, na.rm = T)) / sum(faneca$Ntot, na.rm =
                                                                       T)
abadejos <-
  as.numeric(tapply(abadejo$Ntot, abadejo$Gear, sum, na.rm = T)) / sum(abadejo$Ntot, na.rm =
                                                                         T)
pintos <-
  as.numeric(tapply(pinto$Ntot, pinto$Gear, sum, na.rm = T)) / sum(pinto$Ntot, na.rm =
                                                                     T)
lenguados <-
  as.numeric(tapply(lenguado$Ntot, lenguado$Gear, sum, na.rm = T)) / sum(lenguado$Ntot, na.rm =
                                                                           T)
lengArenas <-
  as.numeric(tapply(lengArena$Ntot, lengArena$Gear, sum, na.rm = T)) / sum(lengArena$Ntot, na.rm =
                                                                             T)
lengSenegales <-
  as.numeric(tapply(lengSenegal$Ntot, lengSenegal$Gear, sum, na.rm = T)) /
  sum(lengSenegal$Ntot, na.rm = T)
rodaballos <-
  as.numeric(tapply(rodaballo$Ntot, rodaballo$Gear, sum, na.rm = T)) / sum(rodaballo$Ntot, na.rm =
                                                                             T)
coruxos <-
  as.numeric(tapply(coruxo$Ntot, coruxo$Gear, sum, na.rm = T)) / sum(coruxo$Ntot, na.rm =
                                                                       T)
platijas <-
  as.numeric(tapply(platija$Ntot, platija$Gear, sum, na.rm = T)) / sum(platija$Ntot, na.rm =
                                                                         T)
congrios <-
  as.numeric(tapply(congrio$Ntot, congrio$Gear, sum, na.rm = T)) / sum(congrio$Ntot, na.rm =
                                                                         T)
lubinas <-
  as.numeric(tapply(lubina$Ntot, lubina$Gear, sum, na.rm = T)) / sum(lubina$Ntot, na.rm =
                                                                       T)
sargos <-
  as.numeric(tapply(sargo$Ntot, sargo$Gear, sum, na.rm = T)) / sum(sargo$Ntot, na.rm =
                                                                     T)
salmonetes <-
  as.numeric(tapply(salmonete$Ntot, salmonete$Gear, sum, na.rm = T)) / sum(salmonete$Ntot, na.rm =
                                                                             T)
rayas <-
  as.numeric(tapply(raya$Ntot, raya$Gear, sum, na.rm = T)) / sum(raya$Ntot, na.rm =
                                                                   T)
pintarojas <-
  as.numeric(tapply(pintaroja$Ntot, pintaroja$Gear, sum, na.rm = T)) / sum(pintaroja$Ntot, na.rm =
                                                                             T)
pulpos <-
  as.numeric(tapply(pulpo$Ntot, pulpo$Gear, sum, na.rm = T)) / sum(pulpo$Ntot, na.rm =
                                                                     T)
calamares <-
  as.numeric(tapply(calamar$Ntot, calamar$Gear, sum, na.rm = T)) / sum(calamar$Ntot, na.rm =
                                                                         T)
chocos <-
  as.numeric(tapply(choco$Ntot, choco$Gear, sum, na.rm = T)) / sum(choco$Ntot, na.rm =
                                                                     T)
centollas <-
  as.numeric(tapply(centolla$Ntot, centolla$Gear, sum, na.rm = T)) / sum(centolla$Ntot, na.rm =
                                                                           T)
necoras <-
  as.numeric(tapply(necora$Ntot, necora$Gear, sum, na.rm = T)) / sum(necora$Ntot, na.rm =
                                                                       T)

peixesArtes <-
  data.frame(
    rbind(
      fanecas,
      abadejos,
      pintos,
      lenguados,
      lengArenas,
      lengSenegales,
      rodaballos,
      coruxos,
      platijas,
      congrios,
      lubinas,
      sargos,
      salmonetes,
      rayas,
      pintarojas,
      pulpos,
      calamares,
      chocos,
      centollas,
      necoras
    )
  )

peixesArtes <- stack(peixesArtes)
colnames(peixesArtes) <- c("Freq", "Species")
head(peixesArtes)
dim(peixesArtes)

sps <-
  c(
    "Trisopterus luscus",
    "Pollachius pollachius",
    "Labrus bergylta",
    "Solea vulgaris",
    "Solea lascaris",
    "Solea senegalensis",
    "Scophthalmus rhombus ",
    "Psetta maxima ",
    "Platichthys flesus",
    "Conger conger",
    "Dicentrarchus labrax",
    "Diplodus sargus",
    "Mullus surmuletus",
    "Raja undulata",
    "Scyliorhinus canicula",
    "Octopus vulgaris",
    "Loligo vulgaris",
    "Sepia officinalis",
    "Maja brachydactyla",
    "Necora puber"
  )

peixesArtes$Species <- rep(sps, times = 31)

length(unique(utpb$Gear))
artes <- as.vector(sort(levels(utpb$Gear)))
peixesArtes$Gear <- rep(artes, each = 20)
peixesArtes$Freq <- ifelse(is.na(peixesArtes$Freq), 0, peixesArtes$Freq)
head(peixesArtes)

peixesArtes$Gear <-
  ifelse(
    peixesArtes$Gear == "RASTRO VIEIRA, VOLANDEIRA, ZAMBURINHA, OSTRA",
    "RASTRO VVZO",
    peixesArtes$Gear
  )
peixesArtes$Gear <-
  ifelse(peixesArtes$Gear == "NASA NECORA E CAMARON",
         "NASA N&C",
         peixesArtes$Gear)

peixesArtes$Gear <- as.factor(peixesArtes$Gear)
peixesArtes$Code <- as.numeric(peixesArtes$Gear)
peixesArtes %>%
  ggplot(aes(x = Code, y = Freq * 100)) +
  geom_bar(stat = "identity") +
  facet_wrap( ~ Species, nrow = 4) +
  scale_y_continuous(limits = c(0, 100), "Frequency") +
  scale_x_continuous("Gear", breaks = seq(1, 32, 1)) +
  theme(
    axis.text.x = element_text(
      angle = 90,
      hjust = 1,
      vjust = 0.5,
      size = 8
    ),
    axis.text.y = element_text(size = 14)
  )

ggsave("plots\\gear_freq.pdf", width = 15, height = 10)

fanecaHauls <- as.numeric(tapply(faneca$Ntot, faneca$Gear, length))
abadejoHauls <-
  as.numeric(tapply(abadejo$Ntot, abadejo$Gear, length))
pintoHauls <- as.numeric(tapply(pinto$Ntot, pinto$Gear, length))
lenguadoHauls <-
  as.numeric(tapply(lenguado$Ntot, lenguado$Gear, length))
lengArenaHauls <-
  as.numeric(tapply(lengArena$Ntot, lengArena$Gear, length))
lengSenegalHauls <-
  as.numeric(tapply(lengSenegal$Ntot, lengSenegal$Gear, length))
rodaballoHauls <-
  as.numeric(tapply(rodaballo$Ntot, rodaballo$Gear, length))
coruxoHauls <- as.numeric(tapply(coruxo$Ntot, coruxo$Gear, length))
platijaHauls <-
  as.numeric(tapply(platija$Ntot, platija$Gear, length))
congrioHauls <-
  as.numeric(tapply(congrio$Ntot, congrio$Gear, length))
lubinaHauls <- as.numeric(tapply(lubina$Ntot, lubina$Gear, length))
sargoHauls <- as.numeric(tapply(sargo$Ntot, sargo$Gear, length))
salmoneteHauls <-
  as.numeric(tapply(salmonete$Ntot, salmonete$Gear, length))
rayaHauls <- as.numeric(tapply(raya$Ntot, raya$Gear, length))
pintarojaHauls <-
  as.numeric(tapply(pintaroja$Ntot, pintaroja$Gear, length))

pulpoHauls <- as.numeric(tapply(pulpo$Ntot, pulpo$Gear, length))
calamarHauls <-
  as.numeric(tapply(calamar$Ntot, calamar$Gear, length))
chocoHauls <- as.numeric(tapply(choco$Ntot, choco$Gear, length))

centollaHauls <-
  as.numeric(tapply(centolla$Ntot, centolla$Gear, length))
necoraHauls <- as.numeric(tapply(necora$Ntot, necora$Gear, length))

haulsTot <-
  as.data.frame(
    cbind(
      fanecaHauls,
      abadejoHauls,
      pintoHauls,
      lenguadoHauls,
      lengArenaHauls,
      lengSenegalHauls,
      rodaballoHauls,
      coruxoHauls,
      platijaHauls,
      congrioHauls,
      lubinaHauls,
      sargoHauls,
      salmoneteHauls,
      rayaHauls,
      pintarojaHauls,
      pulpoHauls,
      calamarHauls,
      chocoHauls,
      centollaHauls,
      necoraHauls
    )
  )
haulsTot$Gear <- artes
haulsSp <- colSums(haulsTot[, 1:20], na.rm = T)
haulsTot$Hauls <- apply(haulsTot[, 1:20], 1, max, na.rm = T)
haulsTot$HaulsFreq <- (haulsTot$Hauls * 100) / sum(haulsTot$Hauls)

ggplot(data = haulsTot, aes(x = seq(1:31), y = HaulsFreq)) +
  geom_bar(stat = "identity",
           colour = "blue",
           fill = "black") +
  scale_y_continuous("Frequency", limits = c(0, 40)) +
  scale_x_continuous(
    "",
    breaks = c(
      1,
      2,
      3,
      4,
      5,
      6,
      7,
      8,
      9,
      10,
      11,
      12,
      13,
      14,
      15,
      16,
      17,
      18,
      19,
      20,
      21,
      22,
      23,
      24,
      25,
      26,
      27,
      28,
      29,
      30,
      31
    ),
    labels = c(artes)
  ) +
  theme(
    axis.text.x = element_text(
      angle = 90,
      hjust = 1,
      vjust = 0.5,
      size = 10
    ),
    axis.text.y = element_text(size = 10)
  )

ggsave("plots\\geartot_freq.pdf", width = 15, height = 10)

#codigo de artes
data.frame(cbind(as.numeric(unique(peixesArtes$Code)),
                 as.character(unique(peixesArtes$Gear))))

# 5.2. Frequency year/zone ------------------------------------------------

haulsYeGe <- table(utpb$Year, utpb$Gear, utpb$ZoneO)

# En numero total de lances
z1 <- as.data.frame(haulsYeGe[, , 1])
z2 <- as.data.frame(haulsYeGe[, , 2])
z3 <- as.data.frame(haulsYeGe[, , 3])

zonesYeGe <- rbind(stack(z1), stack(z2), stack(z3))
colnames(zonesYeGe) <- c("Hauls", "Gear")

zonesYeGe$Year <- rep(c(1999:2018), times = 31*3)
zonesYeGe$GearNum <- rep(c(1:31), times = 3, each = 20)
zonesYeGe$Zone <-
  rep(c("Rías Baixas", "Arco Ártabro", "Cantábrico"), each = 620)

artes[27] <- "RASTRO VVZO"
artes[16] <- "NASA NC"

GRB <-
  ggplot(data = zonesYeGe[zonesYeGe$Zone == "Rías Baixas",], aes(x = Year, y =
                                                                   GearNum)) +
  geom_point(aes(size = Hauls, col = Hauls)) +
  facet_wrap( ~ Zone, nrow = 1) +
  scale_size(range = c(1, 10)) +
  scale_colour_gradient2(
    low = "blue",
    mid = "white",
    high = "red",
    midpoint = 4000
  ) +
  scale_y_continuous(
    "Gear",
    breaks = c(2, 3, 4, 13, 17, 19, 20, 21, 22, 27, 28, 29, 31),
    labels = artes[c(2, 3, 4, 13, 17, 19, 20, 21, 22, 27, 28, 29, 31)]
  )

GAA <-
  ggplot(data = zonesYeGe[zonesYeGe$Zone == "Arco Ártabro", ], aes(x = Year, y =
                                                                     GearNum)) +
  geom_point(aes(size = Hauls, col = Hauls)) +
  facet_wrap( ~ Zone, nrow = 1) +
  scale_size(range = c(1, 10)) +
  scale_colour_gradient2(
    low = "blue",
    mid = "white",
    high = "red",
    midpoint = 1000
  ) +
  scale_y_continuous(
    "Gear",
    breaks = c(2, 3, 4, 13, 17, 19, 20, 21, 22, 27, 28, 29, 31),
    labels = artes[c(2, 3, 4, 13, 17, 19, 20, 21, 22, 27, 28, 29, 31)]
  )

GC <-
  ggplot(data = zonesYeGe[zonesYeGe$Zone == "Cantábrico", ], aes(x = Year, y =
                                                                   GearNum)) +
  geom_point(aes(size = Hauls, col = Hauls)) +
  facet_wrap( ~ Zone, nrow = 1) +
  scale_size(range = c(1, 10)) +
  scale_colour_gradient2(
    low = "blue",
    mid = "white",
    high = "red",
    midpoint = 1000
  ) +
  scale_y_continuous(
    "Gear",
    breaks = c(2, 3, 4, 13, 17, 19, 20, 21, 22, 27, 28, 29, 31),
    labels = artes[c(2, 3, 4, 13, 17, 19, 20, 21, 22, 27, 28, 29, 31)]
  )

GRB / GAA / GC

ggsave("plots\\zone_freq.pdf", width = 15, height = 10)

# 5.3. Spatial distribution -----------------------------------------------

# Modelar diferencias en calado entre artes

gearTest <- data.frame(rbind(abadejo[abadejo$Gear == "TRASMALLOS",],
                             abadejo[abadejo$Gear == "MINHOS",],
                             abadejo[abadejo$Gear == "VETAS",],
                             lubina[lubina$Gear == "PALANGRILLO",],
                             pulpo[pulpo$Gear == "NASA POLBO",],
                             calamar[pulpo$Gear == "BOLICHE",],
                             necora[necora$Gear == "NASA NECORA E CAMARON",]))

gearTest$lDepth <- log(gearTest$Depth)
summary(gearTest$lDepth)
gearTest$lSoak <- log(gearTest$Soaktime)
summary(gearTest$lSoak)
gearTest$fgear <- factor(gearTest$Gear)
summary(gearTest$fgear)

# Differencias en profundidad

gModel1 <- lm(lDepth ~ fgear, data = gearTest)
summary(gModel1)

pred1.T <-
  predict(gModel1,
          newdata = data.frame(fgear = "TRASMALLOS"),
          interval = "confidence")
pred1.M <-
  predict(gModel1,
          newdata = data.frame(fgear = "MINHOS"),
          interval = "confidence")
pred1.V <-
  predict(gModel1,
          newdata = data.frame(fgear = "VETAS"),
          interval = "confidence")
pred1.P <-
  predict(gModel1,
          newdata = data.frame(fgear = "PALANGRILLO"),
          interval = "confidence")
pred1.N <-
  predict(gModel1,
          newdata = data.frame(fgear = "NASA POLBO"),
          interval = "confidence")
pred1.B <-
  predict(gModel1,
          newdata = data.frame(fgear = "BOLICHE"),
          interval = "confidence")
pred1.NNC <-
  predict(gModel1,
          newdata = data.frame(fgear = "NASA NECORA E CAMARON"),
          interval = "confidence")

FIT1 <-
  data.frame(c(
    pred1.T[1],
    pred1.M[1],
    pred1.V[1],
    pred1.P[1],
    pred1.N[1],
    pred1.B[1],
    pred1.NNC[1]
  ))
colnames(FIT1) <- "FIT1"
LWR1 <-
  data.frame(c(
    pred1.T[2],
    pred1.M[2],
    pred1.V[2],
    pred1.P[2],
    pred1.N[2],
    pred1.B[2],
    pred1.NNC[3]
  ))
colnames(LWR1) <- "LWR1"
UPR1 <-
  data.frame(c(
    pred1.T[3],
    pred1.M[3],
    pred1.V[3],
    pred1.P[3],
    pred1.N[3],
    pred1.B[2],
    pred1.NNC[3]
  ))
colnames(UPR1) <- "UPR1"

# Differencias en tiempo de calado

gModel2 <- lm(lSoak ~ fgear, data = gearTest)
summary(gModel2)
plot(allEffects(gModel2))

pred2.T <-
  predict(gModel2,
          newdata = data.frame(fgear = "TRASMALLOS"),
          interval = "confidence")
pred2.M <-
  predict(gModel2,
          newdata = data.frame(fgear = "MINHOS"),
          interval = "confidence")
pred2.V <-
  predict(gModel2,
          newdata = data.frame(fgear = "VETAS"),
          interval = "confidence")
pred2.P <-
  predict(gModel2,
          newdata = data.frame(fgear = "PALANGRILLO"),
          interval = "confidence")
pred2.N <-
  predict(gModel2,
          newdata = data.frame(fgear = "NASA POLBO"),
          interval = "confidence")
pred2.B <-
  predict(gModel2,
          newdata = data.frame(fgear = "BOLICHE"),
          interval = "confidence")
pred2.NNC <-
  predict(gModel2,
          newdata = data.frame(fgear = "NASA NECORA E CAMARON"),
          interval = "confidence")

FIT2 <-
  data.frame(c(
    pred2.T[1],
    pred2.M[1],
    pred2.V[1],
    pred2.P[1],
    pred2.N[1],
    pred2.B[1],
    pred2.NNC[1]
  ))
colnames(FIT2) <- "FIT2"

# Predicciones

predGears <- cbind(FIT1, LWR1, UPR1, FIT2)
predGears$Gear <-
  c("Trasmallo",
    "Minho",
    "Veta",
    "Palangrillo",
    "Nasa pulpo",
    "Boliche",
    "Nasa necora")

# Galician map

pdf("plots\\spatial_distribution.pdf",width = 15, height = 10)

par(mfrow = c(2, 4))

# Trasmallos

par(mar = c(5, 5, 3.5, 3.5))
plot(
  galicia.coast,
  type = "n",
  xlim = c(-9.5, -7.1),
  ylim = c(41.9, 43.9),
  xlab = "",
  ylab = "Latitude (ºN)",
  cex.lab = 2.5,
  lwd = 1.3,
  main = "Trasmallo",
  cex.main = 2.5,
  axes = FALSE
)
axis(side = 2, cex.axis = 2)

contour(
  unique(galicia.bathy$lon),
  sort(unique(galicia.bathy$lat)),
  galicia.bathy.mat,
  levels = -seq(0, 500, by = 50),
  labcex = 0.4,
  col = 'gray80',
  add = T
)

points(
  Lat ~ Lon,
  data = abadejo[abadejo$ZoneO == 1 & abadejo$Gear == "TRASMALLOS", ],
  pch = 16,
  col = "lightblue",
  cex = 2
)
points(
  Lat ~ Lon,
  data = abadejo[abadejo$ZoneO == 2 & abadejo$Gear == "TRASMALLOS", ],
  pch = 16,
  col = "darkgreen",
  cex = 2
)
points(
  Lat ~ Lon,
  data = abadejo[abadejo$ZoneO == 3 & abadejo$Gear == "TRASMALLOS", ],
  pch = 16,
  col = "orange",
  cex = 2
)

points(galicia.coast, type = "l")

legend(
  -9,
  43,
  legend = c("Cantábrico", "Arco Ártabro", "Rías Baixas"),
  bty = "n",
  cex = 1.25,
  pch = 16,
  col = c("orange", "darkgreen", "lightblue")
)#,title="Oceanographic Zone")

# Miños

plot(
  galicia.coast,
  type = "n",
  xlim = c(-9.5, -7.1),
  ylim = c(41.9, 43.9),
  xlab = "",
  ylab = "",
  cex.lab = 2.5,
  lwd = 1.3,
  main = "Miño",
  cex.main = 2.5,
  axes = FALSE
)

contour(
  unique(galicia.bathy$lon),
  sort(unique(galicia.bathy$lat)),
  galicia.bathy.mat,
  levels = -seq(0, 500, by = 50),
  labcex = 0.4,
  col = 'gray80',
  add = T
)

points(
  Lat ~ Lon,
  data = abadejo[abadejo$ZoneO == 1 & abadejo$Gear == "MINHOS", ],
  pch = 16,
  col = "lightblue",
  cex = 2
)
points(
  Lat ~ Lon,
  data = abadejo[abadejo$ZoneO == 2 & abadejo$Gear == "MINHOS", ],
  pch = 16,
  col = "darkgreen",
  cex = 2
)
points(
  Lat ~ Lon,
  data = abadejo[abadejo$ZoneO == 3 & abadejo$Gear == "MINHOS", ],
  pch = 16,
  col = "orange",
  cex = 2
)

points(galicia.coast, type = "l")

# Vetas

plot(
  galicia.coast,
  type = "n",
  xlim = c(-9.5, -7.1),
  ylim = c(41.9, 43.9),
  xlab = "",
  ylab = "",
  cex.lab = 2.5,
  lwd = 1.3,
  main = "Veta",
  cex.main = 2.5,
  axes = FALSE
)

contour(
  unique(galicia.bathy$lon),
  sort(unique(galicia.bathy$lat)),
  galicia.bathy.mat,
  levels = -seq(0, 500, by = 50),
  labcex = 0.4,
  col = 'gray80',
  add = T
)

points(
  Lat ~ Lon,
  data = abadejo[abadejo$ZoneO == 1 & abadejo$Gear == "VETAS", ],
  pch = 16,
  col = "lightblue",
  cex = 2
)
points(
  Lat ~ Lon,
  data = abadejo[abadejo$ZoneO == 2 & abadejo$Gear == "VETAS", ],
  pch = 16,
  col = "darkgreen",
  cex = 2
)
points(
  Lat ~ Lon,
  data = abadejo[abadejo$ZoneO == 3 & abadejo$Gear == "VETAS", ],
  pch = 16,
  col = "orange",
  cex = 2
)

points(galicia.coast, type = "l")

# Palangrillo

plot(
  galicia.coast,
  type = "n",
  xlim = c(-9.5, -7.1),
  ylim = c(41.9, 43.9),
  xlab = "Longitude (ºW)",
  ylab = "",
  cex.lab = 2.5,
  lwd = 1.3,
  main = "Palangrillo",
  cex.main = 2.5,
  axes = FALSE
)
axis(side = 1, cex.axis = 2)

contour(
  unique(galicia.bathy$lon),
  sort(unique(galicia.bathy$lat)),
  galicia.bathy.mat,
  levels = -seq(0, 500, by = 50),
  labcex = 0.4,
  col = 'gray80',
  add = T
)

points(
  Lat ~ Lon,
  data = lubina[lubina$ZoneO == 1 & lubina$Gear == "PALANGRILLO", ],
  pch = 16,
  col = "lightblue",
  cex = 2
)
points(
  Lat ~ Lon,
  data = lubina[lubina$ZoneO == 2 & lubina$Gear == "PALANGRILLO", ],
  pch = 16,
  col = "darkgreen",
  cex = 2
)
points(
  Lat ~ Lon,
  data = lubina[lubina$ZoneO == 3 & lubina$Gear == "PALANGRILLO", ],
  pch = 16,
  col = "orange",
  cex = 2
)

points(galicia.coast, type = "l")

# Nasa polbo

plot(
  galicia.coast,
  type = "n",
  xlim = c(-9.5, -7.1),
  ylim = c(41.9, 43.9),
  xlab = "Longitude (ºW)",
  ylab = "Latitude (ºN)",
  cex.lab = 2.5,
  lwd = 1.3,
  main = "Nasa pulpo",
  cex.main = 2.5,
  axes = FALSE
)
axis(side = 1, cex.axis = 2)
axis(side = 2, cex.axis = 2)

contour(
  unique(galicia.bathy$lon),
  sort(unique(galicia.bathy$lat)),
  galicia.bathy.mat,
  levels = -seq(0, 500, by = 50),
  labcex = 0.4,
  col = 'gray80',
  add = T
)

points(
  Lat ~ Lon,
  data = pulpo[pulpo$ZoneO == 1 & pulpo$Gear == "NASA POLBO", ],
  pch = 16,
  col = "lightblue",
  cex = 2
)
points(
  Lat ~ Lon,
  data = pulpo[pulpo$ZoneO == 2 & pulpo$Gear == "NASA POLBO", ],
  pch = 16,
  col = "darkgreen",
  cex = 2
)
points(
  Lat ~ Lon,
  data = pulpo[pulpo$ZoneO == 3 & pulpo$Gear == "NASA POLBO", ],
  pch = 16,
  col = "orange",
  cex = 2
)

points(galicia.coast, type = "l")

# Boliche

plot(
  galicia.coast,
  type = "n",
  xlim = c(-9.5, -7.1),
  ylim = c(41.9, 43.9),
  xlab = "Longitude (ºW)",
  ylab = "",
  cex.lab = 2.5,
  lwd = 1.3,
  main = "Boliche",
  cex.main = 2.5,
  axes = FALSE
)
axis(side = 1, cex.axis = 2)

contour(
  unique(galicia.bathy$lon),
  sort(unique(galicia.bathy$lat)),
  galicia.bathy.mat,
  levels = -seq(0, 500, by = 50),
  labcex = 0.4,
  col = 'gray80',
  add = T
)

points(
  Lat ~ Lon,
  data = calamar[calamar$ZoneO == 1 & calamar$Gear == "BOLICHE", ],
  pch = 16,
  col = "lightblue",
  cex = 2
)
points(
  Lat ~ Lon,
  data = calamar[calamar$ZoneO == 2 & calamar$Gear == "BOLICHE", ],
  pch = 16,
  col = "darkgreen",
  cex = 2
)
points(
  Lat ~ Lon,
  data = calamar[calamar$ZoneO == 3 & calamar$Gear == "BOLICHE", ],
  pch = 16,
  col = "orange",
  cex = 2
)

points(galicia.coast, type = "l")

# Nasa nécora

plot(
  galicia.coast,
  type = "n",
  xlim = c(-9.5, -7.1),
  ylim = c(41.9, 43.9),
  xlab = "Longitude (ºW)",
  ylab = "",
  cex.lab = 2.5,
  lwd = 1.3,
  main = "Nasa nécora",
  cex.main = 2.5,
  axes = FALSE
)
axis(side = 1, cex.axis = 2)

contour(
  unique(galicia.bathy$lon),
  sort(unique(galicia.bathy$lat)),
  galicia.bathy.mat,
  levels = -seq(0, 500, by = 50),
  labcex = 0.4,
  col = 'gray80',
  add = T
)

points(
  Lat ~ Lon,
  data = necora[necora$ZoneO == 1 &
                  necora$Gear == "NASA NECORA E CAMARON", ],
  pch = 16,
  col = "lightblue",
  cex = 2
)
points(
  Lat ~ Lon,
  data = necora[necora$ZoneO == 2 &
                  necora$Gear == "NASA NECORA E CAMARON", ],
  pch = 16,
  col = "darkgreen",
  cex = 2
)
points(
  Lat ~ Lon,
  data = necora[necora$ZoneO == 3 &
                  necora$Gear == "NASA NECORA E CAMARON", ],
  pch = 16,
  col = "orange",
  cex = 2
)

points(galicia.coast, type = "l")

# Diferencias en operabilidad de las artes

plot(
  exp(FIT1) * -1 ~ seq(1, 7, 1),
  data = predGears,
  xlab = "Gear",
  ylab = "Depth (m)",
  type = "n",
  axes = F,
  ylim = c(-40, 0),
  cex.lab = 2.5
)
grid()

points(
  exp(FIT1) * -1 ~ seq(1, 7, 1),
  data = predGears,
  pch = 16,
  col = c(1, 1, 1, 2, 3, 4, 3),
  cex = 12 * (exp(FIT2) / max(exp(FIT2)))
)
axis(
  side = 1,
  at = c(1, 2, 3, 4, 5, 6, 7),
  labels = predGears$Gear,
  cex.axis = 0.75
)
axis(
  side = 2,
  at = c(-40, -30, -20, -10, 0),
  labels = c("40", "30", "20", "10", "0"),
  cex.axis = 2
)

legend(
  3.5,
  -25,
  legend = c("Gillnet", "Hook", "Creel", "Beach Seine"),
  bty = "n",
  cex = 1,
  pch = 16,
  col = c(1, 2, 3, 4)
)

dev.off()