{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Exception (try, evaluate, SomeException)

-------------------------------------------------------------------------------
-- DECLARAÇÃO DAS FUNÇÕES (A SEREM IMPLEMENTADAS)
-------------------------------------------------------------------------------

-- 1) Dada uma função f do tipo t -> u -> v, defina uma expressão da forma 
-- (\... -> ...) para uma função do tipo u -> t -> v que se comporta como f 
-- mas recebe seus argumentos na ordem inversa.
--
-- DICA: A sua resposta deve ter o formato: inverteArgs f = \x y -> ...
inverteArgs :: (t -> u -> v) -> u -> t -> v
inverteArgs f = \u t -> f t u 


-- 2) Use aplicação parcial para definir a função addNum.
-- A função deve somar dois números.
--
-- DICA: Em vez de declarar 'addNum x y = x + y', tente definir usando 
-- apenas 'addNum =' e a aplicação parcial do operador (+).
addNum :: Int -> Int -> Int
addNum = \n-> \m-> n + m


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
    putStrLn "  RESULTADOS DOS EXERCÍCIOS: FUNÇÕES COMO VALORES    "
    putStrLn "====================================================="
    
    rodarTestes "Questão 1 - inverteArgs (Ordem Inversa)" [
        -- Testando com a função de subtração: inverteArgs (-) 5 10 = 10 - 5 = 5
        ("Subtração invertida", verificar 5 (inverteArgs (-) 5 10)),
        
        -- Testando com concatenação de strings: inverteArgs (++) "Mundo" "Ola" = "OlaMundo"
        ("Concatenação invertida", verificar "OlaMundo" (inverteArgs (++) "Mundo" "Ola"))
      ]
      
    rodarTestes "Questão 2 - addNum (Aplicação Parcial)" [
        ("Soma simples (addNum 5 10)", verificar 15 (addNum 5 10)),
        
        -- Testando se a função se comporta bem quando passada para outras funções de alta ordem
        ("Uso como valor em map", verificar [3, 4, 5] (map (addNum 2) [1, 2, 3]))
      ]
      
    putStrLn "\n====================================================="