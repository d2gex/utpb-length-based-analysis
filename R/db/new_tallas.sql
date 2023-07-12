SELECT count(*)
FROM
    ((((((((Jornada j INNER JOIN [Observador 1] ob on j.OBSER1 = ob.Id)
    INNER JOIN FLOTA f ON f.Idflota = j.Idflota)
    INNER JOIN CDTOTS as cap ON cap.idjornada = j.idjornada)
    INNER JOIN zonas2 z2 ON z2.Idzona = j.Idzona2)
    INNER JOIN Lances l ON l.idjorn = j.idjornada)
    INNER JOIN Artes ar ON ar.Idlance = l.Idlance)
    INNER JOIN codigos_xunta cax ON cax.Idarte = ar.Idarte)
    INNER JOIN EspeciesM em ON em.Idartes = ar.Idnasa)
    INNER JOIN unique_especies_seleccionadas uem ON uem.idespecie = em.IdespM
WHERE j.dia >= #01/01/1999# and j.dia <=#21/12/2017#;


SELECT distinct l.Idlance
FROM
    (((((((Jornada j INNER JOIN [Observador 1] ob on j.OBSER1 = ob.Id)
    INNER JOIN FLOTA f ON f.Idflota = j.Idflota)
    INNER JOIN zonas2 z2 ON z2.Idzona = j.Idzona2)
    INNER JOIN Lances l ON l.idjorn = j.idjornada)
    INNER JOIN Artes ar ON ar.Idlance = l.Idlance)
    INNER JOIN codigos_xunta cax ON cax.Idarte = ar.Idarte)
    INNER JOIN CDTOTS as cap ON (cap.idjornada = j.idjornada) and (cap.id_arte = cax.idarte))
    INNER JOIN unique_especies_seleccionadas uem ON uem.idespecie = cap.IdespT
WHERE j.dia >= #01/01/1999# and j.dia <=#21/12/2017#;

SELECT  ss.idespecie,
        ss.ESPECIE,
        ss.ARTE,
        catches.idjornada,
        catches.idarte,
        catches.Idartes,
        catches.Idlance,
        catches.IdespM,
        catches.NumC,
        catches.PesoC,
        catches.NumD,
        catches.PesoD
    FROM especies_seleccionadas as ss
        LEFT JOIN
        (SELECT
             j.idjornada,
            ar.idarte,
            l.Idlance,
            em.Idartes,
            em.IdespM,
            em.NumC,
            em.PesoC,
            em.NumD,
            em.PesoD
            FROM ((((((Jornada j INNER JOIN [Observador 1] ob on j.OBSER1 = ob.Id)
                INNER JOIN FLOTA f ON f.Idflota = j.Idflota)
        INNER JOIN zonas2 z2 ON z2.Idzona = j.Idzona2)
        INNER JOIN Lances l ON l.idjorn = j.idjornada)
        INNER JOIN Artes ar ON ar.Idlance = l.Idlance)
        INNER JOIN EspeciesM em ON em.Idartes = ar.Idnasa)
        INNER JOIN unique_especies_seleccionadas uem on uem.idespecie = em.IdespM) as catches on catches.IdespM = ss.idespecie
WHERE catches.Idartes in (21464379,957572841,-1657205501,-868540539,-557962433)
order by catches.Idartes

SELECT count(*)
FROM
(SELECT
    ar.Idarte,
    em.IdespM,
    em.Idartes
    FROM ((((((Jornada j INNER JOIN [Observador 1] ob on j.OBSER1 = ob.Id)
        INNER JOIN FLOTA f ON f.Idflota = j.Idflota)
        INNER JOIN zonas2 z2 ON z2.Idzona = j.Idzona2)
        INNER JOIN Lances l ON l.idjorn = j.idjornada)
        INNER JOIN Artes ar ON ar.Idlance = l.Idlance)
        INNER JOIN EspeciesM em ON em.Idartes = ar.Idnasa)
        INNER JOIN unique_especies_seleccionadas uem on uem.idespecie = em.IdespM) as catches
RIGHT JOIN
(SELECT idarte, idespecie, ESPECIE FROM especies_seleccionadas) as ss
ON (ss.idarte = catches.Idarte) AND  (ss.idespecie = catches.IdespM)
WHERE catches.Idartes in (21464379,957572841,-1657205501,-868540539,-557962433)

order by catches.Idartes



        RIGHT JOIN
        (SELECT idarte, idespecie, ESPECIE FROM especies_seleccionadas) as ss
        ON (ss.idarte = catches.idarte) AND  (ss.idespecie = catches.IdespM)
WHERE catches.Idartes in (21464379,957572841,-1657205501,-868540539,-557962433)
order by catches.Idartes


SELECT
    count(*)
    FROM ((((((Jornada j INNER JOIN [Observador 1] ob on j.OBSER1 = ob.Id)
        INNER JOIN FLOTA f ON f.Idflota = j.Idflota)
        INNER JOIN zonas2 z2 ON z2.Idzona = j.Idzona2)
        INNER JOIN Lances l ON l.idjorn = j.idjornada)
        INNER JOIN Artes ar ON ar.Idlance = l.Idlance)
        INNER JOIN EspeciesM em ON em.Idartes = ar.Idnasa)
        INNER JOIN unique_especies_seleccionadas uem on uem.idespecie = em.IdespM as catches
WHERE catches.Idartes in (21464379,957572841,-1657205501,-868540539,-557962433)
order by catches.Idartes