{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Exception (try, evaluate, SomeException)

-------------------------------------------------------------------------------
-- 1) Classe Eq e Polimorfismo Ad Hoc
-- Implemente a função 'allEqual' que recebe três elementos do mesmo tipo
-- e verifica se todos são iguais entre si.
-- Como a função precisa comparar valores, o tipo 't' deve pertencer à classe 'Eq'.
-------------------------------------------------------------------------------
allEqual :: Eq t => t -> t -> t -> Bool
allEqual = undefined


-------------------------------------------------------------------------------
-- 2) Definindo Classes e Instâncias (Visible)
-- Considere a classe 'Visible' abaixo, que descreve tipos de dados que podem 
-- ser convertidos para texto ('toString') e que possuem um tamanho ('size').
--
-- Sua tarefa é implementar as instâncias dessa classe para 'Bool' e 'Char', 
-- seguindo estas regras:
-- Para 'Bool': 'toString' deve retornar "True" ou "False" (como String); 'size' é sempre 1.
-- Para 'Char': 'toString' deve retornar uma String contendo apenas aquele caractere; 'size' é sempre 1.
-------------------------------------------------------------------------------
class Visible t where
    toString :: t -> String
    size :: t -> Int

instance Visible Bool where
    toString = undefined
    size = undefined

instance Visible Char where
    toString = undefined
    size = undefined


-------------------------------------------------------------------------------
-- 3) Instâncias com Restrições Múltiplas
-- Considere o tipo de dados customizado 'Par a b', que agrupa dois valores.
-- Defina a instância da classe 'Eq' para este tipo, de modo que dois valores 
-- do tipo 'Par' sejam considerados iguais APENAS SE os primeiros elementos 
-- forem iguais entre si E os segundos elementos forem iguais entre si.
--
-- Dica: Para que o Haskell permita comparar os elementos internos (a == c e b == d), 
-- os tipos genéricos 'a' e 'b' também precisam ser restritos à classe 'Eq' 
-- na declaração da instância.
-------------------------------------------------------------------------------
data Par a b = Par a b

-- Substitua o corpo da instância abaixo pela sua solução
instance (Eq a, Eq b) => Eq (Par a b) where
    (==) = undefined


-------------------------------------------------------------------------------
-- 4) Herança de Operações e Múltiplos Contextos
-- Crie a função 'imprimeMaior', que recebe dois valores. 
-- Esses valores pertencem simultaneamente às classes 'Ord' (podem ser comparados 
-- com < ou >) e 'Show' (podem ser transformados em String usando a função 'show').
--
-- A função deve descobrir qual dos dois argumentos é o maior e retornar a 
-- representação em String (show) desse maior valor.
-------------------------------------------------------------------------------
imprimeMaior :: (Ord a, Show a) => a -> a -> String
imprimeMaior = undefined


-------------------------------------------------------------------------------
-- 5) Classes Numéricas
-- Implemente a função 'rep', que recebe um número contador 'n' e um elemento 'ch',
-- e retorna uma lista onde o elemento 'ch' se repete 'n' vezes.
-- Exemplo: rep 3 'a' retorna "aaa" (ou seja, ['a','a','a']).
--
-- A assinatura exige que o tipo do contador pertença à classe 'Num' (para permitir 
-- subtrações recursivas como n-1) e à classe 'Eq' (para permitir comparar com 0 
-- para o caso base da recursão).
-------------------------------------------------------------------------------
rep :: (Eq a, Num a) => a -> b -> [b]
rep = undefined



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
    putStrLn "  RESULTADOS DOS EXERCÍCIOS: TYPE CLASSES            "
    putStrLn "====================================================="
    
    rodarTestes "Questão 1 - allEqual (Classe Eq)" [
        ("Três elementos iguais", verificar True (allEqual 5 5 5)),
        ("Último elemento diferente", verificar False (allEqual "A" "A" "B")),
        ("Todos diferentes", verificar False (allEqual 1 2 3))
      ]
      
    rodarTestes "Questão 2 - Instâncias da Classe Visible" [
        ("toString True", verificar "True" (toString True)),
        ("toString False", verificar "False" (toString False)),
        ("size Bool", verificar 1 (size False)),
        ("toString Char", verificar "X" (toString 'X')),
        ("size Char", verificar 1 (size 'a'))
      ]

    rodarTestes "Questão 3 - Instância Eq para tipo Par" [
        ("Par igual", verificar True (Par 1 'a' == Par 1 'a')),
        ("Par com primeiro elemento diferente", verificar False (Par 2 'a' == Par 1 'a')),
        ("Par com segundo elemento diferente", verificar False (Par 1 'b' == Par 1 'a'))
      ]

    rodarTestes "Questão 4 - imprimeMaior (Ord e Show)" [
        ("Maior inteiro", verificar "10" (imprimeMaior 5 10)),
        ("Maior string", verificar "\"Zebra\"" (imprimeMaior "Abelha" "Zebra"))
      ]

    rodarTestes "Questão 5 - Função rep (Num e Eq)" [
        ("Repetir Char 3 vezes", verificar "aaa" (rep 3 'a')),
        ("Repetir Int 0 vezes", verificar [] (rep 0 5)),
        ("Repetir Booleano 2 vezes", verificar [True, True] (rep 2 True))
      ]
      
    putStrLn "\n====================================================="