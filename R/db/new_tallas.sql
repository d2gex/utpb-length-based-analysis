-- noinspection SqlNoDataSourceInspectionForFile

-- noinspection SqlDialectInspectionForFile

query <- "SELECT
j.idjornada,
    l.Idlance,
    e.Idespecie,
    em.Idartes,
    j.dia,
    e.ESPECIE,
    z2.ZONA,
    cax.ARTE,
    l.FLARG,
    l.HorafL,
    l.FVIR,
    l.HorafV,
    l.mcarte1,
    l.[LAT inicio],
    l.[LON inicio],
    l.[LAT final],
    l.[LON final],
    l.TIPOFONDO,
    l.PROFMax,
    l.PROFMin,
    l.PROFMed,
    m.CoD,
    m.NUMINDIVS,
    m.PESO,
    m.TALLA
from
((
([Códigos artes Xunta] cax inner join Artes a on cax.Idarte = a.Idarte)
inner join Lances l on l.Idlance = a.Idlance
)
inner join (
(zonas2 z2 right join Jornada j on z2.Idzona = j.Idzona2)
) on j.idjornada = l.idjorn)
inner join (
(Especies e inner join EspeciesM em on e.Idespecie = em.IdespM) inner join Muestreos m on em.Idesp = m.Idesp
) on a.Idnasa = em.Idartes"