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

-- 1) Type Alias en Data y en las funciones (L)
-- 2) Definir datas de auto con funciones de truco (L)
-- 3) Delegar esVocal en cuantasVocales (L)
-- 4) incrementarVelocidad sacar ultima guarda (L)
-- 5) definir funcion "acelerar" o "sumarVelocidad" (L)
-- 6) Delegar turbo en otras funciones, aprovechar la funcion de la correccion 5) y componer. (L)

deReversa :: Truco
deReversa unAuto = unAuto {nivelNafta = (+pista/5).nivelNafta $ unAuto}

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

