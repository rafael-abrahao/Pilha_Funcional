empty :: [a] -> Bool
empty [] = True
empty pilha = False

push :: a -> [a] -> [a]
push valor pilha = pilha ++ [valor]

push_valores :: [a] -> [a] -> [a]
push_valores valores pilha = pilha ++ valores

pop :: [a] -> (a, [a])
pop pilha = (last pilha, init pilha)

getvalor :: (a, [a]) -> a
getvalor (val, list) = val

getlista :: (a, [a]) -> [a]
getlista (val, list) = list

pop_loop :: Int -> [a] -> [a]
pop_loop qtd pilha = reverse (drop qtd (reverse pilha))

topo :: [a] -> a
topo pilha = last pilha
