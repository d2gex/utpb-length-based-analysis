library(stringr)
build_talla_query_interval <- function(from_date, to_date, db_context) {

  columns <- paste(db_context$talla_columns, collapse = ",")
  query <- sprintf("
SELECT
   %s
FROM
((((((((%s j INNER JOIN %s ob on j.OBSER1 = ob.Id)
INNER JOIN %s f ON f.Idflota = j.Idflota)
INNER JOIN %s z2 ON z2.Idzona = j.Idzona2)
INNER JOIN %s l ON l.idjorn = j.idjornada)
INNER JOIN %s ar ON ar.Idlance = l.Idlance)
INNER JOIN %s cax ON cax.Idarte = ar.Idarte)
INNER JOIN %s em ON em.Idartes = ar.Idnasa)
INNER JOIN %s uem ON uem.idespecie = em.IdespM)
INNER JOIN %s m ON m.Idesp = em.Idesp
WHERE j.dia >= #%s# and j.dia <=#%s#",
                   columns,
                   db_context$jornadas,
                   db_context$observador,
                   db_context$flota,
                   db_context$zonas,
                   db_context$lances,
                   db_context$artes_lances,
                   db_context$artes,
                   db_context$especies_muestreadas,
                   db_context$especies,
                   db_context$muestreos,
                   from_date,
                   to_date)
  return(query)
  return(str_replace_all(query, "[\r\n]" , ""))
}