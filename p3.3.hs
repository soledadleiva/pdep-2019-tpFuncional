-- 3)
todosSufrenTrampa :: Trampa
todosSufrenTrampa unaCarrera = (trampa unaCarrera) unaCarrera


--Implementar 1)
darVuelta :: Trampa
darVuelta = todosSufrenTrampa.aplicarTruco.restarCombustible

--Implementar 2)
correrCarrera :: Trampa
correrCarrera unaCarrera
  | (>0).cantidadDeVueltas $ unaCarrera = (correrCarrera.disminuirVueltas.darVuelta) unaCarrera
  | otherwise = unaCarrera

disminuirVueltas :: Trampa
disminuirVueltas unaCarrera = unaCarrera {cantidadDeVueltas= (+(-1)).cantidadDeVueltas $ unaCarrera}
