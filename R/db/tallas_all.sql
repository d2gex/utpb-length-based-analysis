SELECT
    Jornada.dia,
    EspeciesM.Idartes,
    zonas2.ZONA,
    [Códigos artes Xunta].ARTE,
    Jornada.idjornada,
    Lances.Idlance,
    Artes.Piezas,
    Lances.FLARG,
    Lances.HorafL,
    Lances.FVIR,
    Lances.HorafV,
    Lances.mcarte1,
    Lances.[LAT inicio],
    Lances.[LON inicio],
    Lances.[LAT final],
    Lances.[LON final],
    Lances.TIPOFONDO,
    Lances.PROFMax,
    Lances.PROFMin,
    ESPECIES.ESPECIE,
    Muestreos.CoD,
    Muestreos.NUMINDIVS,
    Muestreos.PESO,
    Muestreos.TALLA 
FROM
    ((zonas2 
RIGHT JOIN
    Jornada 
        ON zonas2.Idzona = Jornada.Idzona2
    ) 
RIGHT JOIN
(
    Lances 
RIGHT JOIN
    (
        [Códigos artes Xunta] 
    RIGHT JOIN
        Artes 
            ON [Códigos artes Xunta].Idarte = Artes.Idarte
        ) 
            ON Lances.Idlance = Artes.Idlance
        ) 
            ON Jornada.idjornada = Lances.idjorn
        ) 
RIGHT JOIN
    (
        (
            ESPECIES 
        RIGHT JOIN
            EspeciesM 
                ON ESPECIES.Idespecie = EspeciesM.IdespM
            ) 
    RIGHT JOIN
        Muestreos 
            ON EspeciesM.Idesp = Muestreos.Idesp
        ) 
            ON Artes.Idnasa = EspeciesM.Idartes 
    ORDER BY
        Jornada.dia DESC; 