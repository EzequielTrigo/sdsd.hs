-- Completar con los datos del grupo
--
-- Nombre de Grupo: xx
-- Integrante 1: Nombre Apellido, email, LU
-- Integrante 2: Nombre Apellido, email, LU
-- Integrante 3: Nombre Apellido, email, LU
-- Integrante 4: Nombre Apellido, email, LU

type Usuario = (Integer, String) -- (id, nombre)
type Relacion = (Usuario, Usuario) -- usuarios que se relacionan
type Publicacion = (Usuario, String, [Usuario]) -- (usuario que publica, texto publicacion, likes)
type RedSocial = ([Usuario], [Relacion], [Publicacion])

-- Funciones basicas

usuarios :: RedSocial -> [Usuario]
usuarios (us, _, _) = us

relaciones :: RedSocial -> [Relacion]
relaciones (_, rs, _) = rs

publicaciones :: RedSocial -> [Publicacion]
publicaciones (_, _, ps) = ps

idDeUsuario :: Usuario -> Integer
idDeUsuario (id, _) = id 

nombreDeUsuario :: Usuario -> String
nombreDeUsuario (_, nombre) = nombre 

usuarioDePublicacion :: Publicacion -> Usuario
usuarioDePublicacion (u, _, _) = u

likesDePublicacion :: Publicacion -> [Usuario]
likesDePublicacion (_, _, us) = us

-- Ejercicios

--primeroLista (eq t) => [t] -> t  
primeroLista (x:xs) = x

nombresDeUsuarios :: RedSocial -> [String]
nombresDeUsuarios r | usuarios(r)==[]=[]
                    | otherwise = proyectarNombres(usuarios(r))

proyectarNombres :: [Usuario] -> [[Char]]
proyectarNombres x = quitarRepetidos(proyectarNombresAUX x)

proyectarNombresAUX :: [Usuario] -> [[Char]]
proyectarNombresAUX [x] = [nombreDeUsuario(x)]
proyectarNombresAUX (x:xs) = nombreDeUsuario(x) : proyectarNombresAUX xs

quitarRepetidos :: (Eq t) => [t] -> [t]
quitarRepetidos [x] = [x]
quitarRepetidos (x:xs) | hayIgualesDe x xs == True = quitarRepetidos xs
                       | otherwise= x : quitarRepetidos xs 

hayIgualesDe :: (Eq t) => t -> [t] -> Bool
hayIgualesDe x  [] = False
hayIgualesDe x (y:ys) |x==y= True
                      |otherwise = hayIgualesDe x ys

-- describir qué hace la función: .....
amigosDe :: RedSocial -> Usuario -> [Usuario]
amigosDe r u = amigosDeAUX (relaciones r) (idDeUsuario u)

amigosDeAUX :: [Relacion] -> Integer -> [Usuario]
amigosDeAUX [] _ = []
amigosDeAUX (x:xs) u | pertenece x u == True = usuarioDelAmigo x u : amigosDeAUX xs u
                     | otherwise = amigosDeAUX xs u

usuarioDelAmigo :: (Usuario,Usuario) -> Integer -> Usuario
usuarioDelAmigo (a,b) u |idDeUsuario a == u = b
                        |otherwise = a

pertenece :: (Usuario,Usuario) -> Integer -> Bool
pertenece (a,b) n |idDeUsuario a==n = True
                  |idDeUsuario b==n = True
                  |otherwise=False

-- describir qué hace la función: .....
cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos r u = length (amigosDe r u) 

-- describir qué hace la función: .....
usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos = undefined

-- describir qué hace la función: .....
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos = undefined

-- describir qué hace la función: .....
publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe = undefined

-- describir qué hace la función: .....
publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA = undefined

-- describir qué hace la función: .....
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones = undefined

-- describir qué hace la función: .....
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel = undefined

-- describir qué hace la función: .....
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos = undefined
