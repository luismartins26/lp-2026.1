{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Exception (try, evaluate, SomeException)
import Data.List (sortBy, sort)

-------------------------------------------------------------------------------
-- DECLARAÇÃO DAS FUNÇÕES (A SEREM IMPLEMENTADAS)
-------------------------------------------------------------------------------

-- 1) Defina uma função que retorne o maior entre quatro inteiros.
maior :: Int -> Int -> Int
maior = undefined

maior4 :: Int -> Int -> Int -> Int -> Int
maior4 = undefined


-- 2) Defina uma função que receba uma nota e retorne a menção do aluno. 
-- A nota é um valor do tipo Float entre 0.0 (inclusive) e 10.0 (inclusive).
converterNotaParaMencao :: Float -> String
converterNotaParaMencao = undefined


-- 3) Implemente funções que satisfaçam a cada um dos requisitos abaixo:

-- a) Retorna a diferença entre duas listas.
diferencaLista :: [Int] -> [Int] ->  [Int]
diferencaLista = undefined

-- b) Retorna a interseção entre duas listas.
intersecaoLista :: [Int] -> [Int] ->  [Int]
intersecaoLista = undefined

-- c) Retorna a união entre duas listas (pode haver repetição de elementos).
uniaoListaRepetida :: [Int] -> [Int] ->  [Int]
uniaoListaRepetida = undefined

-- d) Retorna a união entre duas listas (não há repetição de elementos).
listaUnica :: [Int] -> [Int]
listaUnica = undefined

uniaoListaNaoRepetida :: [Int] -> [Int] ->  [Int]
uniaoListaNaoRepetida = undefined

-- e) Retorna o último elemento de uma lista (duas formas).
ultimoElementoLista :: [Int] -> Int
ultimoElementoLista = undefined

ultimoElementoLista' :: [Int] -> Int
ultimoElementoLista' = undefined

-- f) Retorna o n-ésimo elemento de uma lista.
nElementoLista :: [Int] -> Int -> Int
nElementoLista = undefined

-- g) Inverte uma lista.
inverterLista :: [Int] -> [Int]
inverterLista = undefined

-- h) Ordena uma lista em ordem descrescente, removendo as eventuais repetições.
ordenaLista :: [Int] -> [Int]
ordenaLista = undefined

-- i) Retorna um booleano indicando se uma lista de inteiros é decrescente ou não.
listaOrdenada :: [Int] -> Bool
listaOrdenada = undefined


-- 4) Defina uma função que recebe uma lista de strings como entrada e computa o histograma.
histograma :: [String] -> [(String,Int)]
histograma = undefined


-- 5) Defina a função myZipWith.
myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith = undefined


-- 6) Determinar a lista dos alunos aprovados (média >= 5.0), ordenada por média.
aprovadosOrdemDeMedia :: [(String,Float,Float)] -> [(String,Float)]
aprovadosOrdemDeMedia = undefined


-- 7) Operações com matrizes:
somaMatricial :: [[Int]] -> [[Int]] -> [[Int]]
somaMatricial = undefined

multiplicacaoMatricial :: [[Int]] -> [[Int]] -> [[Int]]
multiplicacaoMatricial = undefined

matrizTransposta :: [[Int]] -> [[Int]]
matrizTransposta = undefined


-- 8) Estenda o tipo Expr para representar multiplicação.
data Expr = Lit Int | Add Expr Expr | Sub Expr Expr | Mul Expr Expr
    deriving (Eq, Show)

eval :: Expr -> Int
eval (Lit n) = n
eval (Add e1 e2) = (eval e1) + (eval e2)
eval (Sub e1 e2) = (eval e1) - (eval e2)


-- 9) Crie a função foldTree.
data Tree t = NilT | Node t (Tree t) (Tree t)
    deriving (Eq, Show)

foldTree :: (t -> u -> u -> u) -> Tree t -> u -> u
foldTree = undefined


-- 10) Soma dos elementos de uma árvore (duas formas).
somaArvoreBinaria :: Tree Int -> Int
somaArvoreBinaria = undefined

somaArvoreBinaria' :: Tree Int -> Int
somaArvoreBinaria' = undefined


-- 11) Refaça o Exercício 2 usando um tipo algébrico.
data Mencao = SS | MS | MM | MI | II | SR
    deriving (Eq, Show)

converterNotaParaMencao' :: Float -> Mencao
converterNotaParaMencao' = undefined



-------------------------------------------------------------------------------
-- AVALIADOR CUSTOMIZADO DE TESTES (NÃO PRECISA MODIFICAR)
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
    putStrLn "  RESULTADOS DA LISTA DE EXERCÍCIOS                  "
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