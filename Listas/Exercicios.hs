{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Exception (try, evaluate, SomeException)

-------------------------------------------------------------------------------
-- 1) PERGUNTAS CONCEITUAIS
-- Substitua o 'undefined' pela sua resposta (ex: um número ou uma lista manual)
-------------------------------------------------------------------------------

-- Quantos elementos existem na lista [2,3]?
qtdElementosLista1 :: Int
qtdElementosLista1 = undefined

-- Quantos elementos existem na lista [[2,3]]?
qtdElementosLista2 :: Int
qtdElementosLista2 = undefined

-- Qual o tipo da lista [[2,3]]? 
-- (Responda em formato de comentário abaixo, pois tipos não podem ser testados em tempo de execução desta forma)
{-
Sua resposta aqui: 
-}

-- Qual o resultado da avaliação das seguintes expressões? 
-- (Escreva a lista final explicitamente, ex: [1,2,3])
aval1 :: [Int] -- equivalente a [2,4..9]
aval1 = undefined

aval2 :: [Int] -- equivalente a [2..2]
aval2 = undefined

aval3 :: [Int] -- equivalente a [2,7..4]
aval3 = undefined

aval4 :: [Int] -- equivalente a [10,9..1]
aval4 = undefined

aval5 :: [Int] -- equivalente a [10..1]
aval5 = undefined


-------------------------------------------------------------------------------
-- 2) FUNÇÕES BÁSICAS DE LISTAS
-------------------------------------------------------------------------------

-- dobrar os elementos de uma lista
double :: [Int] -> [Int]
double = undefined

-- checar se um elemento está na lista
member :: [Int] -> Int -> Bool
member = undefined

-- filtragem: manter apenas os números de uma string
digits :: String -> String
digits = undefined

-- soma de uma lista de pares
sumPairs :: [(Int,Int)] -> [Int]
sumPairs = undefined


-------------------------------------------------------------------------------
-- 3) ORDENAÇÃO (Insertion Sort)
-------------------------------------------------------------------------------

-- Insere um elemento de forma ordenada em uma lista já ordenada
insert :: Int -> [Int] -> [Int]
insert = undefined

-- Ordena uma lista usando o método de inserção
sort :: [Int] -> [Int]
sort = undefined


-------------------------------------------------------------------------------
-- 4) EXEMPLO: BIBLIOTECA
-------------------------------------------------------------------------------
type Person = String
type Book = String
type Database = [(Person, Book)]

exampleBase :: Database
exampleBase = [("Alice","PostmanPat"),("Anna","AllAlone"),("Alice","Spot"), ("Rory","PostmanPat")]

-- Retorna os livros que uma pessoa pegou emprestado
books :: Database -> Person -> [Book]
books = undefined

-- Retorna as pessoas que pegaram um determinado livro emprestado
borrowers :: Database -> Book -> [Person]
borrowers = undefined

-- Verifica se um livro está emprestado
borrowed :: Database -> Book -> Bool
borrowed = undefined

-- Conta quantos livros uma pessoa pegou
numBorrowed :: Database -> Person -> Int
numBorrowed = undefined

-- Faz um empréstimo (adiciona o registro no banco)
makeLoan :: Database -> Person -> Book -> Database
makeLoan = undefined

-- Devolve um empréstimo (remove o registro do banco)
returnLoan :: Database -> Person -> Book -> Database
returnLoan = undefined


-------------------------------------------------------------------------------
-- 5) REESCREVENDO COM COMPREENSÃO DE LISTAS
-------------------------------------------------------------------------------

memberComp :: [Int] -> Int -> Bool
memberComp = undefined

booksComp :: Database -> Person -> [Book]
booksComp = undefined

borrowersComp :: Database -> Book -> [Person]
borrowersComp = undefined

borrowedComp :: Database -> Book -> Bool
borrowedComp = undefined

returnLoanComp :: Database -> Person -> Book -> Database
returnLoanComp = undefined


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

verificarMembroBase :: (Eq a, Show a) => [a] -> [a] -> IO Resultado
verificarMembroBase esperado obtido = do
    res <- try (evaluate (esperado == obtido)) :: IO (Either SomeException Bool)
    case res of
        Left _ -> return NaoImplementado
        Right _ -> 
            if length esperado == length obtido && all (`elem` obtido) esperado
                then return Passou
                else return $ Falhou ("Elementos: " ++ show esperado) ("Obtido: " ++ show obtido)

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
    putStrLn "  RESULTADOS DOS EXERCÍCIOS DE LISTAS                "
    putStrLn "====================================================="
    
    rodarTestes "Questões Conceituais" [
        ("Qtd [2,3]", verificar 2 qtdElementosLista1),
        ("Qtd [[2,3]]", verificar 1 qtdElementosLista2),
        ("[2,4..9]", verificar [2,4,6,8] aval1),
        ("[2..2]", verificar [2] aval2),
        ("[2,7..4]", verificar [2] aval3),
        ("[10,9..1]", verificar [10,9,8,7,6,5,4,3,2,1] aval4),
        ("[10..1]", verificar [] aval5)
      ]
      
    rodarTestes "Funções Básicas" [
        ("double [1,2,3]", verificar [2,4,6] (double [1,2,3])),
        ("member: existente", verificar True (member [1,2,3] 2)),
        ("member: inexistente", verificar False (member [1,2,3] 4)),
        ("digits 'a1b2c3'", verificar "123" (digits "a1b2c3")),
        ("sumPairs [(1,2), (3,4)]", verificar [3, 7] (sumPairs [(1,2), (3,4)]))
      ]

    rodarTestes "Ordenação" [
        ("insert no meio", verificar [1,2,3] (insert 2 [1,3])),
        ("sort de lista invertida", verificar [1,2,3,4] (sort [4,3,2,1]))
      ]

    rodarTestes "Exemplo: Biblioteca (Tradicional)" [
        ("books da Alice", verificarMembroBase ["PostmanPat", "Spot"] (books exampleBase "Alice")),
        ("borrowers do PostmanPat", verificarMembroBase ["Alice", "Rory"] (borrowers exampleBase "PostmanPat")),
        ("borrowed 'Spot'", verificar True (borrowed exampleBase "Spot")),
        ("borrowed 'Dune'", verificar False (borrowed exampleBase "Dune")),
        ("numBorrowed da Anna", verificar 1 (numBorrowed exampleBase "Anna")),
        ("makeLoan ('John', 'Dune')", verificar True (("John", "Dune") `elem` makeLoan exampleBase "John" "Dune")),
        ("returnLoan da Alice devolvendo Spot", verificar False (("Alice", "Spot") `elem` returnLoan exampleBase "Alice" "Spot"))
      ]

    rodarTestes "Exemplo: Biblioteca (Compreensão de Listas)" [
        ("memberComp: existente", verificar True (memberComp [1,2,3] 2)),
        ("booksComp da Alice", verificarMembroBase ["PostmanPat", "Spot"] (booksComp exampleBase "Alice")),
        ("borrowersComp do PostmanPat", verificarMembroBase ["Alice", "Rory"] (borrowersComp exampleBase "PostmanPat")),
        ("borrowedComp 'Spot'", verificar True (borrowedComp exampleBase "Spot")),
        ("returnLoanComp devolvendo Spot", verificar False (("Alice", "Spot") `elem` returnLoanComp exampleBase "Alice" "Spot"))
      ]

    putStrLn "\n====================================================="