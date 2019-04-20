data Auto = Auto {
    nombre :: String,
    nivelNafta :: Float,
    velocidad :: Float,
    nombreEnamorade :: String,
    trucoParticular :: String
} deriving Show

pista = 1000

deReversa :: Auto -> Auto
deReversa unAuto = unAuto {nivelNafta = (+pista/5).nivelNafta $ unAuto}

impresionar :: Auto -> Auto
impresionar unAuto = unAuto {velocidad = (*2).velocidad $ unAuto}

nitro :: Auto -> Auto
nitro unAuto = unAuto {velocidad = (+15).velocidad $ unAuto}

fingirAmor :: Auto -> String  -> Auto 
fingirAmor unAuto otroAuto = unAuto {nombreEnamorade = otroAuto}

rochaMcQueen = Auto {
    nombre = "RochaMcQueen",
    nivelNafta = 300,
    velocidad = 0,
    nombreEnamorade = "Ronco",
    trucoParticular = "deReversa"
}

biankerr = Auto {
    nombre = "Biankerr",
    nivelNafta = 500,
    velocidad = 20,
    nombreEnamorade = "Tinch",
    trucoParticular = "impresionar"
}

gushtav = Auto {
    nombre = "Gushtav",
    nivelNafta = 200,
    velocidad = 130,
    nombreEnamorade = "PetiLaLinda",
    trucoParticular = "nitro"
}

rodra = Auto {
    nombre = "Rodra",
    nivelNafta = 0,
    velocidad = 50,
    nombreEnamorade = "Taisa",
    trucoParticular = "fingirAmor"
}

-- Punto 2
cuantasVocales :: Auto -> Int 
cuantasVocales (Auto _ _ _ nombreEnamorade _) = length.filter(`elem` "aeiou") $ nombreEnamorade

incrementarVelocidad :: Auto -> Auto
incrementarVelocidad unAuto
   | (<=2) (cuantasVocales unAuto) = unAuto {velocidad = (+15).velocidad $ unAuto}
   | (<=4) (cuantasVocales unAuto) = unAuto {velocidad = (+20).velocidad $ unAuto}
   | (>4) (cuantasVocales unAuto) = unAuto {velocidad = (+30).velocidad $ unAuto}
   | otherwise = unAuto {velocidad = (+0).velocidad $ unAuto}

-- Punto 3
puedeRealizarTruco :: Auto -> Bool
puedeRealizarTruco (Auto _ nivelNafta velocidad _ _) = (>0)nivelNafta && (<100)velocidad 

-- Punto 4
comboLoco :: Auto -> Auto
comboLoco = nitro.deReversa

queTrucazo :: Auto -> String -> Auto 
queTrucazo unAuto nombre = incrementarVelocidad $ (fingirAmor unAuto nombre)

turbo :: Auto -> Auto 
turbo unAuto = unAuto {velocidad =  (+)((*10).nivelNafta $ unAuto)(velocidad $ unAuto), nivelNafta = 0}




















