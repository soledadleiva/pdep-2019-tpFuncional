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
unQuintoDeVelocidad unAuto = (/5).velocidad $ unAuto

impresionar :: Truco 
impresionar unAuto = unAuto {velocidad = (*2).velocidad $ unAuto}

sumarVelocidad :: Auto -> Velocidad -> Auto
sumarVelocidad unAuto numeroX = unAuto {velocidad = (+numeroX).velocidad $ unAuto}
nitro :: Truco
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
esVocal (Auto _ _ _ nombreEnamorade _) = filter(`elem`  "aeiouAEIOU") $  nombreEnamorade

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
nuevaVelocidad :: Auto->Auto
nuevaVelocidad unAuto = sumarVelocidad unAuto (naftaPorDiez  unAuto)  
naftaPorDiez :: Auto -> NivelNafta   
naftaPorDiez unAuto =  (*10).nivelNafta $ unAuto
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
   participante = [rochaMcQueen, biankerr, gushtav,rodra]
} 

--Punto 2
sacarAlPistero :: Trampa
sacarAlPistero unaCarrera= unaCarrera{participante= (sacarPrimerElemento.participante) unaCarrera}
sacarPrimerElemento :: [Auto]->[Auto]
sacarPrimerElemento = drop 1
cuantosParticipantesQuedan :: [Auto]->Int
cuantosParticipantesQuedan = length.sacarPrimerElemento


lluvia :: Carrera -> [Auto]
lluvia unaCarrera = map bajarVelocidad (participante unaCarrera)
bajarVelocidad :: Auto -> Auto
bajarVelocidad unAuto = unAuto {velocidad = (+(-10)).velocidad $ unAuto}


pocaReserva:: Carrera -> [Auto]
pocaReserva unaCarrera =filter masDe30Litros (participante unaCarrera)
masDe30Litros :: Auto -> Bool
masDe30Litros (Auto _ nivelNafta _ _ _) = (>30)nivelNafta


neutralizarTrucos :: Carrera->[Auto]
neutralizarTrucos unaCarrera = map modificarTruco (participante unaCarrera)
modificarTruco :: Auto->Auto
modificarTruco unAuto = unAuto {trucoParticular = inutilidad}
inutilidad :: Truco
inutilidad= id 


podio :: Trampa
podio unaCarrera = unaCarrera {participante = dejarPrimerosTres.participante $ unaCarrera}
dejarPrimerosTres :: [Auto]->[Auto]
dejarPrimerosTres = take 3


-- Punto 3
-- 1)
restarCombustible :: Auto -> Carrera -> Auto
restarCombustible unAuto unaCarrera = unAuto {nivelNafta =(+(-calculoCombustible unaCarrera unAuto)).nivelNafta $ unAuto}
calculoCombustible :: Carrera -> Auto -> Velocidad
calculoCombustible unaCarrera unAuto = (*pistaDivididoDiez unaCarrera).velocidad $ unAuto
pistaDivididoDiez :: Carrera -> LongitudPista
pistaDivididoDiez unaCarrera = (/10).longitudPista $ unaCarrera

-- 2)
nombreEnPublico :: Auto -> Carrera ->  Bool
nombreEnPublico unAuto unaCarrera = elem (nombreEnamorade unAuto) (nombreIntegrantesPublico  unaCarrera)
hacerTruco :: Auto -> Auto
hacerTruco rochaMcQueen = deReversa rochaMcQueen
hacerTruco biankerr= impresionar biankerr
hacerTruco rodra = fingirAmor "Petra" rodra
hacerTruco gushtav = nitro gushtav
aplicarTruco :: Auto -> Carrera -> Auto
aplicarTruco unAuto unaCarrera 
 |(nombreEnPublico unAuto unaCarrera) == True = hacerTruco unAuto
 |otherwise = unAuto

-- 3)
todosSufrenTrampa :: Carrera -> Carrera
todosSufrenTrampa unaCarrera = sacarAlPistero.sacarAlPistero.sacarAlPistero.sacarAlPistero $ unaCarrera

-- Implementar 1)
darVuelta :: Carrera -> Auto -> Auto
darVuelta unaCarrera unAuto = aplicarTruco (restarCombustible unAuto (todosSufrenTrampa unaCarrera)) unaCarrera
--Implementar 2)


--Punto 4
quienGana :: Carrera -> Auto -> Auto
quienGana unaCarrera unAuto = participante (darVuelta.darVuelta.darVuelta $ unaCarrera) 

-- Punto 5
elGranTruco :: [Truco] -> Auto -> Auto
elGranTruco [] unAuto = unAuto
elGranTruco (x:xs) unAuto = elGranTruco xs $(x unAuto)

-- Punto 6
--a) Si, pero jamas terminaria.
--b) Si, pues se puede extraer el primer elemento de la lista.
--c) En este caso no va a devolver nada, pues son infinitos elementos y al aplicarle una función jamas terminaria.








