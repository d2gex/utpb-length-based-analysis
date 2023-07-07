SELECT ob.[Observador 1]
    j.idjornada,
    j.dia,
    z2.ZONA,
    l.Idlance,
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
    cax.ARTE,
    em.Idartes,
    uem.idespecie,
    uem.ESPECIE,
    m.CoD,
    m.NUMINDIVS,
    m.PESO,
    m.TALLA
FROM
    ((((((((Jornada j INNER JOIN [Observador 1] ob on j.OBSER1 = ob.Id)
    INNER JOIN FLOTA f ON f.Idflota = j.Idflota)
    INNER JOIN zonas2 z2 ON z2.Idzona = j.Idzona2)
    INNER JOIN Lances l ON l.idjorn = j.idjornada)
    INNER JOIN Artes ar ON ar.Idlance = l.Idlance)
    INNER JOIN codigos_xunta cax ON cax.Idarte = ar.Idarte)
    INNER JOIN EspeciesM em ON em.Idartes = ar.Idnasa)
    INNER JOIN unique_especies_seleccionadas uem ON uem.idespecie = em.IdespM)
    INNER JOIN Muestreos m
ON m.Idesp = em.Idesp
WHERE j.dia >= #01/01/1999# and j.dia <=#21/12/2017#;
