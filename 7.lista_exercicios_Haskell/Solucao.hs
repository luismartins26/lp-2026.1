{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Exception (try, evaluate, SomeException)
import Data.List (sortBy, sort)

-------------------------------------------------------------------------------
-- IMPLEMENTAÇÕES (Gabarito)
-------------------------------------------------------------------------------

-- 1) Defina uma função que retorne o maior entre quatro inteiros.
maior :: Int -> Int -> Int
maior x y = if x > y then x else y

maior4 :: Int -> Int -> Int -> Int -> Int
maior4 x y w z = maior (maior (maior x y) w) z


-- 2) Defina uma função que receba uma nota e retorne a menção do aluno.
converterNotaParaMencao :: Float -> String
converterNotaParaMencao n
    | 9 <= n && n <= 10  = "SS"
    | 7 <= n && n <= 8.9  = "MS"
    | 5 <= n && n <= 6.9  = "MM"
    | 3 <= n && n <= 4.9  = "MI"
    | otherwise = "Nota inválida"


-- 3) Implemente funções que satisfaçam a cada um dos requisitos abaixo:

-- a) Diferença
diferencaLista :: [Int] -> [Int] ->  [Int]
diferencaLista [] [] = []
diferencaLista (h1:tl1) (h2:tl2) = (h1-h2):(diferencaLista tl1 tl2)
diferencaLista _ _ = []

-- b) Interseção
intersecaoLista :: [Int] -> [Int] ->  [Int]
intersecaoLista (h1:tl1) (h2:tl2)
    | h1==h2 = h1:intersecaoLista tl1 tl2
    | otherwise =  intersecaoLista tl1 tl2
intersecaoLista _ _ = []

-- c) União com repetição
uniaoListaRepetida :: [Int] -> [Int] ->  [Int]
uniaoListaRepetida l1 l2 = l1 ++ l2

-- d) União sem repetição
listaUnica :: [Int] -> [Int]
listaUnica [] = []
listaUnica (h1:tl1)
    | h1 `elem` tl1 = listaUnica tl1
    | otherwise = h1:listaUnica tl1

uniaoListaNaoRepetida :: [Int] -> [Int] ->  [Int]
uniaoListaNaoRepetida l1 l2 = listaUnica (uniaoListaRepetida l1 l2)

-- e) Último elemento
ultimoElementoLista :: [Int] -> Int
ultimoElementoLista l = head (reverse l)

ultimoElementoLista' :: [Int] -> Int
ultimoElementoLista' l = last l

-- f) N-ésimo elemento
nElementoLista :: [Int] -> Int -> Int
nElementoLista (h:tl) n
    | n == 0 = h
    | n>0 = nElementoLista tl (n-1)
    | otherwise = error ""
nElementoLista [] _ = error "Lista vazia"

-- g) Inverter lista
inverterLista :: [Int] -> [Int]
inverterLista l = reverse l

-- h) Ordenar lista decrescente
ordenaLista :: [Int] -> [Int]
ordenaLista [] = []
ordenaLista [e] = [e]
ordenaLista (a:b:tl) = ordenaLista [e|e<-b:tl, e>a] ++ [a] ++ ordenaLista [e|e<-b:tl, e<a]

-- i) Verificar se é decrescente
listaOrdenada :: [Int]->Bool
listaOrdenada l = l == ordenaLista l


-- 4) Histograma
histograma :: [String] -> [(String,Int)]
histograma l = foldr (\(s1,n1) b -> if not ((s1,n1) `elem` b) then [(s1,n1)]++b else [(s2,n2+1)|(s2,n2)<-b,s2==s1]++[(s2,n2)|(s2,n2)<-b, s2/=s1]) [] (map (\s->(s,1)) l) 


-- 5) myZipWith
myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f l1 l2 = map (\(a,b) -> f a b) (zip l1 l2)


-- 6) Aprovados
aprovadosOrdemDeMedia :: [(String,Float,Float)] -> [(String,Float)]
aprovadosOrdemDeMedia l = sortBy (\(nome1,media1) (nome2,media2)-> compare media1 media2) (filter (\(nome,media)->media>=5.0) (map (\(nome,nota1,nota2)->(nome,(nota1+nota2)/2)) l))


-- 7) Matrizes
somaMatricial :: [[Int]] -> [[Int]] -> [[Int]]
somaMatricial m1 m2 = map (\(l1,l2)-> map (\(e1,e2)->e1+e2) (zip l1 l2)) (zip m1 m2)

multiplicacaoMatricial :: [[Int]] -> [[Int]] -> [[Int]]
multiplicacaoMatricial a b = map (\la -> map (\lb -> foldr (\(ea,eb) r -> ea*eb + r) 0 (zip la lb)) (matrizTransposta b)) a

matrizTransposta :: [[Int]] -> [[Int]]
matrizTransposta m = map (\n -> map (\l -> nElementoLista l n) m ) [0..(length (head m) - 1)]


-- 8) Avaliação algébrica
data Expr = Lit Int | Add Expr Expr | Sub Expr Expr | Mul Expr Expr
    deriving (Eq, Show) -- Adicionado deriving para testes

eval :: Expr -> Int
eval (Lit n) = n
eval (Add e1 e2) = (eval e1) + (eval e2)
eval (Sub e1 e2) = (eval e1) - (eval e2)
eval (Mul e1 e2) = (eval e1) * (eval e2)


-- 9) foldTree
data Tree t = NilT | Node t (Tree t) (Tree t)
    deriving (Eq, Show) -- Adicionado deriving para testes

foldTree :: (t -> u -> u -> u) -> Tree t -> u -> u
foldTree f NilT v = v
foldTree f (Node n t1 t2) v = f n (foldTree f t1 v) (foldTree f t2 v)


-- 10) Soma Árvore
somaArvoreBinaria :: Tree Int -> Int
somaArvoreBinaria t = foldTree (\n b1 b2 -> n+b1+b2) t 0

somaArvoreBinaria' :: Tree Int -> Int
somaArvoreBinaria' NilT = 0
somaArvoreBinaria' (Node n t1 t2) = n + (somaArvoreBinaria' t1) + (somaArvoreBinaria' t2)


-- 11) Menção Algébrica
data Mencao = SS | MS | MM | MI | II | SR
    deriving (Eq, Show) -- Adicionado deriving para testes

converterNotaParaMencao' :: Float -> Mencao
converterNotaParaMencao' n
    | 9 <= n && n <= 10  = SS
    | 7 <= n && n <= 8.9  = MS
    | 5 <= n && n <= 6.9  = MM
    | 3 <= n && n <= 4.9  = MI
    | otherwise = SR


-------------------------------------------------------------------------------
-- AVALIADOR CUSTOMIZADO DE TESTES
-------------------------------------------------------------------------------

data Resultado = Passou | Falhou String String | NaoImplementado

verificar :: (Eq a, Show a) => a -> a -> IO Resultado
verificar esperado obtido = do
    res <- try (evaluate (esperado == obtido)) :: IO (Either SomeException Bool)
    case res of
        Left _ -> return NaoImplementado
        Right True -> return Passou
        Right False -> return $ Falhou (show esperado) (show obtido)

verificarSemOrdem :: (Eq a, Ord a, Show a) => [a] -> [a] -> IO Resultado
verificarSemOrdem esperado obtido = do
    res <- try (evaluate (sort esperado == sort obtido)) :: IO (Either SomeException Bool)
    case res of
        Left _ -> return NaoImplementado
        Right True -> return Passou
        Right False -> return $ Falhou ("Elementos: " ++ show esperado) ("Obtido: " ++ show obtido)

rodarTestes :: String -> [(String, IO Resultado)] -> IO ()
rodarTestes nomeGrupo testesIO = do
    putStrLn $ "\n▶ " ++ nomeGrupo
    resultados <- mapM (\(nome, t) -> do r <- t; return (nome, r)) testesIO
    
    let naoImplementados = length [ () | (_, NaoImplementado) <- resultados ]
        falhas           = [ (n, esp, obt) | (n, Falhou esp obt) <- resultados ]
        total            = length resultados

    if naoImplementados == total
        then putStrLn "   ⚠️ Status: Não implementada."
        else if naoImplementados > 0
        then putStrLn $ "   ⚠️ Status: Parcialmente implementada (" ++ show naoImplementados ++ " testes não executaram)."
        else if null falhas
        then putStrLn "   ✅ Status: Passou em todos os testes."
        else do
            putStrLn "   ❌ Status: Falhou nos seguintes testes:"
            mapM_ (\(n, esp, obt) -> putStrLn $ "      - " ++ n ++ " | Esperado: " ++ esp ++ " | Obtido: " ++ obt) falhas

-------------------------------------------------------------------------------
-- EXECUÇÃO DOS TESTES
-------------------------------------------------------------------------------
main :: IO ()
main = do
    putStrLn "====================================================="
    putStrLn "  RESULTADOS DA LISTA DE EXERCÍCIOS (GABARITO)       "
    putStrLn "====================================================="
    
    rodarTestes "Questão 1 - Maior de 4 inteiros" [
        ("maior4 de 1,2,3,4", verificar 4 (maior4 1 2 3 4)),
        ("maior4 com negativos", verificar (-1) (maior4 (-1) (-5) (-3) (-4)))
      ]
      
    rodarTestes "Questão 2 - Menção" [
        ("Nota 10.0 (SS)", verificar "SS" (converterNotaParaMencao 10.0)),
        ("Nota 8.0 (MS)", verificar "MS" (converterNotaParaMencao 8.0)),
        ("Nota 11.0 (Inválida)", verificar "Nota inválida" (converterNotaParaMencao 11.0))
      ]
      
    rodarTestes "Questão 3 - Operações com Listas" [
        ("a) Diferença", verificar [4,2,0] (diferencaLista [5,4,3] [1,2,3])),
        ("b) Interseção posicional", verificar [1] (intersecaoLista [1,2] [1,3])),
        ("c) União com Repetição", verificar [1,2,2,3] (uniaoListaRepetida [1,2] [2,3])),
        ("d) União sem Repetição", verificar [1,2,3] (uniaoListaNaoRepetida [1,2] [2,3])),
        ("e) Último Elemento 1", verificar 3 (ultimoElementoLista [1,2,3])),
        ("e) Último Elemento 2", verificar 3 (ultimoElementoLista' [1,2,3])),
        ("f) N-ésimo Elemento", verificar 20 (nElementoLista [10,20,30] 1)),
        ("g) Inverter", verificar [2,1] (inverterLista [1,2])),
        ("h) Ordenar Desc (sem rep)", verificar [3,2,1] (ordenaLista [1,3,2,1])),
        ("i) Lista Ordenada 1", verificar True (listaOrdenada [3,2,1]))
      ]
      
    rodarTestes "Questão 4 - Histograma" [
        ("Contagem 'a' e 'b'", verificarSemOrdem [("b",1),("a",2)] (histograma ["a", "b", "a"]))
      ]
      
    rodarTestes "Questão 5 - myZipWith" [
        ("Soma de listas", verificar [4,6] (myZipWith (+) [1,2] [3,4]))
      ]
      
    rodarTestes "Questão 6 - Aprovados por Média" [
        ("Aprova 'B' com media 10", verificar [("B",10.0)] (aprovadosOrdemDeMedia [("A",4.0,4.0), ("B",10.0,10.0)]))
      ]
      
    rodarTestes "Questão 7 - Matrizes" [
        ("a) Soma", verificar [[3]] (somaMatricial [[1]] [[2]])),
        ("b) Transposta", verificar [[1,3],[2,4]] (matrizTransposta [[1,2],[3,4]])),
        ("c) Multiplicação", verificar [[7,10],[15,22]] (multiplicacaoMatricial [[1,2],[3,4]] [[1,2],[3,4]]))
      ]
      
    rodarTestes "Questão 8 - Expressões (eval)" [
        ("Avaliação de Add e Mul", verificar 7 (eval (Add (Lit 1) (Mul (Lit 2) (Lit 3)))))
      ]
      
    rodarTestes "Questões 9 e 10 - Árvores Binárias" [
        ("Soma (com foldTree)", verificar 3 (somaArvoreBinaria (Node 1 (Node 2 NilT NilT) NilT))),
        ("Soma (recursão direta)", verificar 3 (somaArvoreBinaria' (Node 1 (Node 2 NilT NilT) NilT)))
      ]
      
    rodarTestes "Questão 11 - Menção (Tipo Algébrico)" [
        ("Nota 10.0 (SS alg)", verificar SS (converterNotaParaMencao' 10.0)),
        ("Nota 4.5 (MI alg)", verificar MI (converterNotaParaMencao' 4.5))
      ]
      
    putStrLn "\n====================================================="