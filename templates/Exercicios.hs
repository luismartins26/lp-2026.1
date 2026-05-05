{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Exception (try, evaluate, SomeException)

-------------------------------------------------------------------------------
-- DECLARAÇÃO DAS FUNÇÕES (A SEREM IMPLEMENTADAS)
-- Substitua estas funções pelas questões do novo tópico.
-------------------------------------------------------------------------------

-- 1) Descrição da primeira questão de exemplo.
funcaoExemplo1 :: Int -> Int -> Int
funcaoExemplo1 a b = a + b

-- 2) Descrição da segunda questão de exemplo.
funcaoExemplo2 :: String -> String
funcaoExemplo2 = undefined


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
    
    -- Substitua os blocos abaixo pelos testes correspondentes às suas funções
    
    rodarTestes "Questão 1 - Nome do Tópico 1" [
        ("Cenário de teste 1", verificar 5 (funcaoExemplo1 2 3)),
        ("Cenário de teste 2", verificar 10 (funcaoExemplo1 5 5))
      ]
    
    rodarTestes "Questão 2 - Nome do Tópico 2" [
        ("Cenário com String", verificar "OlaMundo" (funcaoExemplo2 "Mundo"))
      ]
      
    putStrLn "\n====================================================="