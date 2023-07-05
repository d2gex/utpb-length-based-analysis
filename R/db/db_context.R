library("R6")

DbContext <- R6Class("DbContext", public = list(
  especies = 'ESPECIES',
  especies_muestreadas = 'EspeciesM',
  artes = 'codigos_xunta',
  artes_lances = 'Artes',
  jornadas = 'Jornada',
  lances = 'Lances',
  muestreos = 'Muestreos',
  zonas = 'zonas2',
  talla_columns = c(
    "j.idjornada",
    "l.Idlance",
    "e.Idespecie",
    "em.Idartes",
    "j.dia",
    "e.ESPECIE",
    "z2.ZONA",
    "cax.ARTE",
    "l.FLARG",
    "l.HorafL",
    "l.FVIR",
    "l.HorafV",
    "l.mcarte1",
    "l.[LAT inicio]",
    "l.[LON inicio]",
    "l.[LAT final]",
    "l.[LON final]",
    "l.TIPOFONDO",
    "l.PROFMax",
    "l.PROFMin",
    "l.PROFMed",
    "m.CoD",
    "m.NUMINDIVS",
    "m.PESO",
    "m.TALLA"
  )
))