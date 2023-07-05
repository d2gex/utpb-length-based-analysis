SELECT
    Jornada.dia,
    Jornada.OBSER1,
    FLOTA1.Idflota,
    EspeciesM.Idartes,
    zonas2.ZONA,
    zonas2.PUERTO_EMBARQUE,
    FLOTA1.TRB,
    Jornada.[N TRIPUS],
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
    Muestreos.SEXO,
    Muestreos.OVADA,
    Muestreos.TALLA,
    Muestreos.Escog,
    Muestreos.Madurez,
    Muestreos.Colorhuevos,
    Muestreos.MUDA,
    Muestreos.DUREZA,
    Muestreos.[eje 1A-P],
    Muestreos.[eje 2D-V],
    Muestreos.[eje 3],
    Muestreos.[Tallcf-ojo]
FROM
    ((FLOTA1
RIGHT JOIN
    (
        zonas2
    RIGHT JOIN
        (
            [Observador 1]
        RIGHT JOIN
            Jornada
                ON [Observador 1].Id = Jornada.OBSER1
            )
                ON zonas2.Idzona = Jornada.Idzona2
            )
                ON FLOTA1.Idflota = Jornada.Idflota
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
                    (
                        ESPECIES
                    LEFT JOIN
                        FABIOGruposEspecies
                            ON ESPECIES.Idespecie = FABIOGruposEspecies.Idespecie
                        )
                RIGHT JOIN
                    EspeciesM
                        ON ESPECIES.Idespecie = EspeciesM.IdespM
                    )
            RIGHT JOIN
                Muestreos
                    ON EspeciesM.Idesp = Muestreos.Idesp
                )
                    ON Artes.Idnasa = EspeciesM.Idartes
            GROUP BY
                Jornada.dia,
                Jornada.OBSER1,
                FLOTA1.Idflota,
                EspeciesM.Idartes,
                zonas2.ZONA,
                zonas2.PUERTO_EMBARQUE,
                FLOTA1.TRB,
                Jornada.[N TRIPUS],
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
                Muestreos.SEXO,
                Muestreos.OVADA,
                Muestreos.TALLA,
                Muestreos.Escog,
                Muestreos.Madurez,
                Muestreos.Colorhuevos,
                Muestreos.MUDA,
                Muestreos.DUREZA,
                Muestreos.[eje 1A-P],
                Muestreos.[eje 2D-V],
                Muestreos.[eje 3],
                Muestreos.[Tallcf-ojo]
            HAVING
                (
                    (
                        (
                            Jornada.dia
                        )>#12/29/2011#
                    )
                )
            ORDER BY
                Jornada.dia DESC;