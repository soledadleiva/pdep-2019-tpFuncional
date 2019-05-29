--Punto 2
sacarAlPistero :: Trampa
sacarAlPistero unaCarrera= unaCarrera{participante= sacarPrimerElemento.participante $ unaCarrera}

sacarPrimerElemento :: [Auto]->[Auto]
sacarPrimerElemento = drop 1


lluvia :: Trampa
lluvia unaCarrera = unaCarrera {participante = map bajarVelocidad (participante unaCarrera)}

bajarVelocidad :: Auto -> Auto
bajarVelocidad unAuto = unAuto {velocidad = (+(-10)).velocidad $ unAuto}


pocaReserva:: Trampa
pocaReserva unaCarrera = unaCarrera {participante = filter masDe30Litros (participante unaCarrera)} 

masDe30Litros :: Auto -> Bool
masDe30Litros (Auto _ nivelNafta _ _ _) = (>30)nivelNafta


neutralizarTrucos :: Trampa 
neutralizarTrucos unaCarrera = unaCarrera {participante = map modificarTruco (participante unaCarrera)}

modificarTruco :: Auto -> Auto
modificarTruco unAuto = unAuto {trucoParticular = inutilidad}

inutilidad :: Truco
inutilidad = id 


podio :: Trampa
podio unaCarrera = unaCarrera {participante = dejarPrimerosTres.participante $ unaCarrera}

dejarPrimerosTres :: [Auto]->[Auto]
dejarPrimerosTres = take 3
