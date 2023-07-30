module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

data Hechicero = UnHechicero {
    nombre :: String,
    antiguedad :: Number,
    clan :: String,
    grado :: Number
} deriving Show

nobara :: Hechicero
nobara = UnHechicero {
    nombre = "Nobara",
    antiguedad = 1,
    clan = "Kugisaki",
    grado = 3
}

satoru :: Hechicero
satoru = UnHechicero "Satoru" 15 "Gojo" 0

maki :: Hechicero
maki = UnHechicero "Maki" 3 "Zenin" 4

yuji :: Hechicero
yuji = UnHechicero "Yuji" 0 "Itadori" 1

tieneExperiencia :: Hechicero -> Bool
tieneExperiencia hechicero = antiguedad hechicero > 1

type Equipo = [Hechicero]

grupo1 :: Equipo
grupo1 = [nobara,maki,yuji,satoru]

estaPreparado :: Equipo -> Bool
estaPreparado equipo = length equipo > 3

subirDeGrado :: Hechicero -> Hechicero
subirDeGrado hechicero
    |gradoEspecial hechicero = hechicero 
    |otherwise = hechicero {grado = grado hechicero -1}

gradoEspecial :: Hechicero -> Bool
gradoEspecial hechicero = grado hechicero == 0

clanesPrestigiosos :: [String]
clanesPrestigiosos = ["Zenin","Kamo","Gojo"]

esPrestigioso :: Hechicero -> Bool
esPrestigioso hechicero = elem (clan hechicero) clanesPrestigiosos

invencible :: Equipo -> Bool
invencible grupo = any gradoEspecial grupo

esFavorito :: Equipo -> Bool
esFavorito grupo = all esPrestigioso grupo

experto :: Equipo -> [Hechicero]
experto grupo = filter tieneExperiencia grupo

leHaceFrente :: Equipo -> Bool
leHaceFrente grupo
    |invencible grupo || estaPreparado grupo = True
    |otherwise = False

powerUp :: Equipo -> Equipo
powerUp grupo = map subirDeGrado grupo

elMasApto :: Ord a => (Hechicero -> a) -> Hechicero -> Hechicero -> Hechicero
elMasApto nivel hechicero1 hechicero2
    |nivel hechicero1 > nivel hechicero2 = hechicero1
    |otherwise = hechicero2

nivelTryhard :: Hechicero -> Number
nivelTryhard hechicero = 1 / (grado hechicero + 1)

nivelBurocratico :: Hechicero -> Number
nivelBurocratico hechicero = length (clan hechicero)

nivelIntimidante :: Hechicero -> Char
nivelIntimidante hechicero = maximum (clan hechicero)

nivelSigiloso :: Hechicero -> Number
nivelSigiloso hechicero = antiguedad hechicero * 6