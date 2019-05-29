--Punto 4
quienGana :: Carrera -> Auto
quienGana unaCarrera = head.participante $ (correrCarrera unaCarrera)