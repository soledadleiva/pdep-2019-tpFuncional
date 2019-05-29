import Text.Show.Functions

data Auto = Auto {
    nombre :: Nombre,
    nivelNafta :: NivelNafta,
    velocidad :: Velocidad,
    nombreEnamorade :: Nombre,
    trucoParticular :: Truco
} deriving Show

pista = 1000

type Nombre = String
type Truco = Auto -> Auto 
type NivelNafta = Float
type Velocidad = Float

--deReversa :: Truco
--deReversa unAuto = unAuto {nivelNafta = (+pista/5).nivelNafta $ unAuto}

-- Punto 0
deReversa :: Truco 
deReversa unAuto = unAuto {nivelNafta = (+unQuintoDeVelocidad unAuto).nivelNafta $ unAuto}

unQuintoDeVelocidad:: Auto -> Velocidad
unQuintoDeVelocidad = (/5).velocidad

impresionar :: Truco 
impresionar unAuto = unAuto {velocidad = (*2).velocidad $ unAuto}

sumarVelocidad :: Auto -> Velocidad -> Auto
sumarVelocidad unAuto numeroX = unAuto {velocidad = (+numeroX).velocidad $ unAuto}

--nitro :: Int -> Truco -- Int -> Auto -> Auto
--nitro numero unAuto = sumarVelocidad unAuto numero
nitro:: Truco
nitro unAuto = sumarVelocidad unAuto 15

fingirAmor :: Nombre -> Truco 
fingirAmor otroAuto unAuto  = unAuto {nombreEnamorade = otroAuto}

rochaMcQueen = Auto {
    nombre = "RochaMcQueen",
    nivelNafta = 300,
    velocidad = 0,
    nombreEnamorade = "Ronco",
    trucoParticular = deReversa
}

biankerr = Auto {
    nombre = "Biankerr",
    nivelNafta = 500,
    velocidad = 20,
    nombreEnamorade = "Tinch",
    trucoParticular = impresionar
}

gushtav = Auto {
    nombre = "Gushtav",
    nivelNafta = 200,
    velocidad = 130,
    nombreEnamorade = "PetiLaLinda",
    trucoParticular = nitro
     --(nitro 20) 
}

rodra = Auto {
    nombre = "Rodra",
    nivelNafta = 0,
    velocidad = 50,
    nombreEnamorade = "Taisa",
    trucoParticular = (fingirAmor "Petra")
}

-- Punto 2
esVocal :: Auto -> Nombre
esVocal = filter (`elem`  "aeiouAEIOU").nombreEnamorade

cuantasVocales :: Auto -> Int 
cuantasVocales = length.esVocal 

incrementarVelocidad :: Truco
incrementarVelocidad unAuto
   | (<=2) (cuantasVocales unAuto) = sumarVelocidad unAuto 15
   | (<=4) (cuantasVocales unAuto) = sumarVelocidad unAuto 20
   | otherwise = sumarVelocidad unAuto 30

-- Punto 3
puedeRealizarTruco :: Auto -> Bool
puedeRealizarTruco (Auto _ nivelNafta velocidad _ _) = (>0)nivelNafta && (<100)velocidad 

-- Punto 4
comboLoco :: Truco
comboLoco = nitro.deReversa

queTrucazo :: Auto -> Nombre -> Auto 
queTrucazo unAuto nombre = incrementarVelocidad $ (fingirAmor nombre unAuto)

turbo :: Truco 
turbo = naftaCero.nuevaVelocidad
--turbo unAuto = unAuto {nivelNafta=0 , velocidad= nuevaVelocidad unAuto  }

nuevaVelocidad :: Auto->Auto
nuevaVelocidad unAuto = sumarVelocidad unAuto (naftaPorDiez  unAuto)  

naftaPorDiez :: Auto -> NivelNafta   
naftaPorDiez =  (*10).nivelNafta

naftaCero :: Truco 
naftaCero unAuto = unAuto {nivelNafta=0}


-- PARTE 2
-- Punto 0 arriba
-- Punto 1 
data Carrera = Carrera {
    cantidadDeVueltas :: CantidadDeVueltas,
    longitudPista :: LongitudPista,
    nombreIntegrantesPublico :: [Nombre],
    trampa :: Trampa,
    participante :: [Auto]
} deriving Show

type Trampa = Carrera -> Carrera
type CantidadDeVueltas = Int
type LongitudPista = Float

potreroFunes = Carrera {
   cantidadDeVueltas = 3,
   longitudPista = 5,      
   nombreIntegrantesPublico = ["Ronco", "Tinch", "Dodain"],
   trampa = sacarAlPistero,
   participante = [rochaMcQueen, biankerr, gushtav, rodra]
} 

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

--Punto 4
quienGana :: Carrera -> Auto
quienGana unaCarrera = head.participante $ (correrCarrera unaCarrera)

-- Punto 5
elGranTruco :: [Truco] -> Auto -> Auto
elGranTruco [] unAuto = unAuto
elGranTruco (x:xs) unAuto = elGranTruco xs $(x unAuto)


-- Punto 6
--a) Si, pero jamas terminaria.
--b) Si, pues se puede extraer el primer elemento de la lista.
--c) En este caso no va a devolver nada, pues son infinitos elementos y al aplicarle una función jamas terminaria.



