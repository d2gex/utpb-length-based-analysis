library(stringr)
build_talla_query_interval <- function(from_date, to_date, db_context) {

  columns <- paste(db_context$talla_columns, collapse = ",")
  query <- sprintf("
SELECT
    %s
  FROM
  (
  ((%s cax inner join %s a on cax.Idarte = a.Idarte)
  inner join %s l on l.Idlance = a.Idlance)
  inner join
  ((%s z2 right join %s j on z2.Idzona = j.Idzona2)) on j.idjornada = l.idjorn
  )
  inner join
  ((%s e inner join %s em on e.Idespecie = em.IdespM) inner join %s m on em.Idesp = m.Idesp
  ) on a.Idnasa = em.Idartes
  WHERE j.dia >=#%s# AND j.dia <=#%s#",
                   columns,
                   db_context$artes,
                   db_context$artes_lances,
                   db_context$lances,
                   db_context$zonas,
                   db_context$jornadas,
                   db_context$especies,
                   db_context$especies_muestreadas,
                   db_context$muestreos,
                   from_date,
                   to_date)

  return(str_replace_all(query, "[\r\n]" , ""))
}

