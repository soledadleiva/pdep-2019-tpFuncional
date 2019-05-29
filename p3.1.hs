-- Punto 3
-- 1)
restarCombustible :: Carrera -> Carrera
restarCombustible unaCarrera = unaCarrera {participante = map(nuevoCombustibleDeUnAuto unaCarrera).participante $ unaCarrera}

nuevoCombustibleDeUnAuto ::Carrera-> Auto -> Auto
nuevoCombustibleDeUnAuto unaCarrera unAuto= unAuto {nivelNafta =(+(-calculoCombustible unaCarrera unAuto)).nivelNafta $ unAuto} 

calculoCombustible :: Carrera->Auto->Velocidad
calculoCombustible unaCarrera unAuto = (*pistaDivididoDiez unaCarrera).velocidad $ unAuto

pistaDivididoDiez :: Carrera -> LongitudPista
pistaDivididoDiez unaCarrera = (/10).longitudPista $ unaCarrera
