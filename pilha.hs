empty :: [a] -> Bool
empty [] = True
empty pilha = False

push :: a -> [a] -> [a]
push valor pilha = pilha ++ [valor]

push_valores :: [a] -> [a] -> [a]
push_valores valores pilha = pilha ++ valores

pop :: [a] -> (a, [a])
pop pilha = pop_aux pilha []

pop_aux :: [a] -> [a] -> (a, [a])
pop_aux pilha pilha_aux
    | length pilha == 1 = (head pilha, pilha_aux)
    | otherwise = pop_aux (tail pilha) (pilha_aux ++ [(head pilha)])

getvalor :: (a, [a]) -> a
getvalor (val, list) = val

getlista :: (a, [a]) -> [a]
getlista (val, list) = list

pop_loop :: Int -> [a] -> [a]
pop_loop qtd pilha = pop_loop_aux qtd 0 pilha

pop_loop_aux :: Int -> Int -> [a] -> [a]
pop_loop_aux _ _ [] = []
pop_loop_aux qtd curr pilha
    | qtd == curr = pilha
    | otherwise = pop_loop_aux qtd (curr + 1) (getlista (pop pilha))

topo :: [a] -> a
topo pilha = getvalor (pop pilha)
