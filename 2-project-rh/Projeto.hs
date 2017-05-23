module Projeto where

data Cargo = Estagiario | Programador | Coordenador | Gerente deriving Show
data Pessoa = Pessoa {cargo :: Cargo, nome :: String} deriving Show

pegarSalario :: Pessoa -> Double
pegarSalario (Pessoa Estagiario _) = 1500
pegarSalario (Pessoa Programador _) = 5750.15
pegarSalario (Pessoa Coordenador _) = 8000
pegarSalario (Pessoa Gerente _) = 10807.2

promover :: Pessoa -> Pessoa
promover (Pessoa Estagiario n) = Pessoa Programador n
promover (Pessoa Programador n) = Pessoa Coordenador n
promover (Pessoa Coordenador n) = Pessoa Gerente n
promover (Pessoa _ n) = Pessoa Gerente n

verPropriedade :: String -> String -> String
verPropriedade label value = label ++ ": \"" ++ value ++ "\""

verNome :: Pessoa -> String
verNome pessoa = verPropriedade "nome" (nome pessoa)

verCargo :: Pessoa -> String
verCargo pessoa = verPropriedade "cargo" $ show (cargo pessoa)

verSalario :: Pessoa -> String
verSalario pessoa = verPropriedade "salário" $ show (pegarSalario pessoa)

juntar :: String -> [String] -> String
juntar separator list = (reverse . tail . reverse) $ foldr (\a b -> a ++ separator ++ b) "" list

paraJson :: Pessoa -> String
paraJson pessoa = "{" ++ juntar "," [verNome pessoa, verCargo pessoa, verSalario pessoa] ++  "}"
