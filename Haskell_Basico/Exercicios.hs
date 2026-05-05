{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Exception (try, evaluate, SomeException)

-------------------------------------------------------------------------------
-- DECLARAÇÃO DAS FUNÇÕES (A SEREM IMPLEMENTADAS)
-------------------------------------------------------------------------------

-- 1) Fatorial
fatorialfat :: Int -> Int
fatorialfat = undefined

-- 2) Compara se quatro números são iguais 
all4Equal :: Int -> Int -> Int -> Int -> Bool
all4Equal = undefined

-- 3) Compara se quatro números são iguais (Usando allEqual na solução)
allEqual :: Int -> Int -> Int -> Bool
allEqual n m p = (n == m) && (m == p)

all4Equal' :: Int -> Int -> Int -> Int -> Bool
all4Equal' = undefined

-- 4) Retorna quantos parâmetros são iguais 
howManyEqual :: Int -> Int -> Int -> Int
howManyEqual = undefined

-------------------------------------------------------------------------------
-- SIMULAÇÃO DE BANCO DE DADOS (Para as questões de Vendas)
-- Considere esta função pronta para implementar as próximas questões.
-------------------------------------------------------------------------------
sales :: Int -> Int
sales 0 = 12
sales 1 = 14
sales 2 = 12
sales 3 = 20
sales _ = 0

-- 5) Função que dado um valor inteiro s (venda) e um número de semanas n, 
-- retorna quantas semanas de 0 a n tiveram venda igual a s.
quantasSemanasVenda :: Int -> Int -> Int
quantasSemanasVenda = undefined

-- 6) Produz um string com uma quantidade n de espaços
makeSpaces :: Int -> String
makeSpaces = undefined

-- 7) Utilizando a definição de makeSpaces, adiciona uma quantidade n de espaços à esquerda de uma string
-- (Nota: Dependendo da interpretação de "Right", pode ser espaços no início ou no fim. 
-- O teste abaixo assume que os espaços são inseridos ANTES da string, empurrando-a para a direita).
pushRight :: Int -> String -> String
pushRight = undefined

-- 8) Dado um número de semanas n, retorna a média de vendas das semanas de 0 a n.
averageSales :: Int -> Float
averageSales = undefined


-------------------------------------------------------------------------------
-- AVALIADOR CUSTOMIZADO DE TESTES (NÃO PRECISA MODIFICAR)
-------------------------------------------------------------------------------

data Resultado = Passou | Falhou String String | NaoImplementado

verificar :: (Eq a, Show a) => a -> a -> IO Resultado
verificar esperado obtido = do
    -- O 'evaluate' força a execução da função. Se for 'undefined', ele gera uma exceção.
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
    putStrLn "  RESULTADOS DOS EXERCÍCIOS                          "
    putStrLn "====================================================="
    
    rodarTestes "Questão 1 - fatorialfat" [
        ("Fatorial de 0", verificar 1 (fatorialfat 0)),
        ("Fatorial de 5", verificar 120 (fatorialfat 5))
      ]
    
    rodarTestes "Questões 2 e 3 - allEqual e all4Equal" [
        ("allEqual: 3 iguais", verificar True (allEqual 5 5 5)),
        ("allEqual: diferentes", verificar False (allEqual 5 4 5)),
        ("all4Equal: 4 iguais", verificar True (all4Equal' 2 2 2 2)),
        ("all4Equal: último diferente", verificar False (all4Equal' 2 2 2 3))
      ]
      
    rodarTestes "Questão 4 - howManyEqual" [
        ("Todos os 3 iguais", verificar 3 (howManyEqual 7 7 7)),
        ("Apenas 2 iguais", verificar 2 (howManyEqual 7 8 7)),
        ("Nenhum igual", verificar 0 (howManyEqual 1 2 3))
      ]

    rodarTestes "Questão 5 - Semanas com Venda Igual a s" [
        ("Venda 12 até semana 3 (Ocorre na sem 0 e 2)", verificar 2 (quantasSemanasVenda 12 3)),
        ("Venda 20 até semana 3 (Ocorre na sem 3)", verificar 1 (quantasSemanasVenda 20 3)),
        ("Venda 50 até semana 3 (Nenhuma ocorrencia)", verificar 0 (quantasSemanasVenda 50 3))
      ]

    rodarTestes "Questão 6 - makeSpaces" [
        ("Zero espaços", verificar "" (makeSpaces 0)),
        ("Três espaços", verificar "   " (makeSpaces 3))
      ]

    rodarTestes "Questão 7 - pushRight" [
        ("Empurrar 'Haskell' 3 espaços", verificar "   Haskell" (pushRight 3 "Haskell")),
        ("Empurrar 'LP' 0 espaços", verificar "LP" (pushRight 0 "LP"))
      ]

    rodarTestes "Questão 8 - averageSales" [
        ("Média até semana 1: (12+14)/2", verificar 13.0 (averageSales 1)),
        ("Média até semana 3: (12+14+12+20)/4", verificar 14.5 (averageSales 3))
      ]
      
    putStrLn "\n====================================================="