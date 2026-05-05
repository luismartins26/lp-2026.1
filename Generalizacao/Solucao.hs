{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Exception (try, evaluate, SomeException)

-------------------------------------------------------------------------------
-- 1) PERGUNTAS CONCEITUAIS (Avaliação de Expressões)
-- Substitua o 'undefined' pelo resultado final da avaliação (ex: um número, lista, etc.)
-------------------------------------------------------------------------------

-- map length ["abc", "defg"] = ?
avalMapLength :: [Int]
avalMapLength = [3,4]

-- fold (||) [False, True, True] = ?  (Atenção: em Haskell padrão usa-se foldr ou foldl)
avalFold1 :: Bool
avalFold1 = True

-- fold (++) ["Bom", " ", "Dia"] = ?
avalFold2 :: String
avalFold2 = "Bom Dia"

-- fold min [6] = ?
avalFold3 :: Int
avalFold3 = 6

-- fold (*) [1..6] = ?
avalFold4 :: Int
avalFold4 = 1*2*3*4*5*6


-------------------------------------------------------------------------------
-- 2) FUNÇÕES DE ALTA ORDEM (Generalização)
-------------------------------------------------------------------------------

-- Simulação de vendas para a questão maxSales
sales :: Int -> Int
sales 0 = 12
sales 1 = 14
sales 2 = 12
sales 3 = 20
sales _ = 0

-- Define uma função genérica maxFun que, dada uma função f e um limite n, 
-- retorna o valor máximo de f(i) para i variando de 0 a n.
maxFun :: (Int -> Int) -> Int -> Int
maxFun f n = foldr max 0 (map f [0..n])

-- Use a função maxFun para implementar maxSales, que retorna o maior 
-- número de vendas de uma semana de 0 a n semanas.
maxSales :: Int -> Int
maxSales n = maxFun sales n

-- Dada uma função, verificar se ela é crescente em um intervalo de 0 a n
isCrescent :: (Int -> Int) -> Int -> Bool
isCrescent f n = foldr (&&) True (map (\(n1,n2)->f n1<=f n2) (zip [0..n] [1..n]))


-------------------------------------------------------------------------------
-- 3) OPERAÇÕES SOBRE LISTAS (Mapping, Folding, Filtering)
-------------------------------------------------------------------------------

-- eleva os itens ao quadrado (mapping)
sqItems :: [Int] -> [Int]
sqItems = map (\n->n*n)

-- retorna a soma dos quadrados dos itens (folding)
sumSqItems :: [Int] -> Int
sumSqItems l = foldr (+) 0 (sqItems l)

-- manter na lista todos os itens maiores que zero (filtering)
filterPositive :: [Int] -> [Int]
filterPositive = filter (\n->n>0)


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
    putStrLn "  RESULTADOS DOS EXERCÍCIOS DE GENERALIZAÇÃO         "
    putStrLn "====================================================="
    
    rodarTestes "Questões Conceituais (Map e Fold)" [
        ("map length", verificar [3, 4] avalMapLength),
        ("fold (||)", verificar True avalFold1),
        ("fold (++)", verificar "Bom Dia" avalFold2),
        ("fold min", verificar 6 avalFold3),
        ("fold (*)", verificar 720 avalFold4)
      ]
      
    rodarTestes "Funções de Alta Ordem" [
        ("maxFun com (*2) até 3", verificar 6 (maxFun (*2) 3)),
        ("maxSales até semana 3", verificar 20 (maxSales 3)),
        ("maxSales até semana 1", verificar 14 (maxSales 1)),
        ("isCrescent com (*2) até 4", verificar True (isCrescent (*2) 4)),
        ("isCrescent com vendas até 3", verificar False (isCrescent sales 3))
      ]
      
    rodarTestes "Operações com Listas (Map, Fold, Filter)" [
        ("sqItems de [1,2,3]", verificar [1,4,9] (sqItems [1,2,3])),
        ("sumSqItems de [1,2,3]", verificar 14 (sumSqItems [1,2,3])),
        ("filterPositive de [-1, 0, 1, 2]", verificar [1,2] (filterPositive [-1, 0, 1, 2]))
      ]
      
    putStrLn "\n====================================================="