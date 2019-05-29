-- 2) 
aplicarTruco :: Trampa
aplicarTruco unaCarrera = unaCarrera {participante = combinarListas unaCarrera}

nombreEnPublico :: Carrera -> Auto -> Bool
nombreEnPublico unaCarrera unAuto = elem (nombreEnamorade unAuto) (nombreIntegrantesPublico unaCarrera)

filtrarEnamoradaEnPublico :: Carrera -> [Auto]
filtrarEnamoradaEnPublico unaCarrera = filter(nombreEnPublico unaCarrera).participante $ unaCarrera

noApareceEnPublico :: Carrera->[Auto]
noApareceEnPublico unaCarrera =filter(not.(nombreEnPublico unaCarrera)).participante $unaCarrera

mapearParticipantesYAplicarTruco :: Carrera -> [Auto]
mapearParticipantesYAplicarTruco unaCarrera= (map(hacerTruco)).filtrarEnamoradaEnPublico $ unaCarrera

combinarListas :: Carrera -> [Auto]
combinarListas unaCarrera= (mapearParticipantesYAplicarTruco unaCarrera) ++ (noApareceEnPublico unaCarrera)

hacerTruco :: Auto -> Auto
hacerTruco unAuto = (trucoParticular unAuto) unAuto