{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Exception (try, evaluate, SomeException)

-------------------------------------------------------------------------------
-- QUESTÃO 1 (Avaliação de Expressões)
-- Preencha as variáveis com a String correspondente ao resultado da avaliação.
-- Caso a expressão seja mal tipada, responda exatamente com a string: "Erro de Tipo"
-------------------------------------------------------------------------------

-- a) 1 + 2
q1_a :: String
q1_a = undefined 

-- b) True && (False || True)
q1_b :: String
q1_b = undefined

-- c) (1,2) == (2,1)
q1_c :: String
q1_c = undefined

-- d) 1 + True
q1_d :: String
q1_d = undefined

-- e) "Ola" ++ " Mundo"
q1_e :: String
q1_e = undefined

-- f) "Ola" ++ [1]
q1_f :: String
q1_f = undefined

-- g) [5..10] ++ [0..4]
q1_g :: String
q1_g = undefined

-- h) let fst (x,y) = x in fst (fst ((8,9),"Joao"))
q1_h :: String
q1_h = undefined


-------------------------------------------------------------------------------
-- QUESTÃO 2
-------------------------------------------------------------------------------

-- a) Usando COMPREENSÃO DE LISTAS, retorne a lista em ordem decrescente.
ordenaDecrescente :: [Int] -> [Int]
ordenaDecrescente = undefined

-- b) Retorna um valor booleano indicando se a lista é decrescente ou não.
isDecrescente :: [Int] -> Bool
isDecrescente = undefined


-------------------------------------------------------------------------------
-- QUESTÃO 3
-------------------------------------------------------------------------------
-- Sendo f e g funções definidas conforme abaixo:
-- f :: Integer -> Integer
-- f e = e + 2
--
-- g :: [Integer] -> Integer
-- g [] = 0
-- g (a : as) = f a + g as
--
-- Indique o resultado numérico final da expressão: g [4,5,6,7]
resultadoQ3 :: Integer
resultadoQ3 = undefined


-------------------------------------------------------------------------------
-- QUESTÃO 4 (Árvores)
-------------------------------------------------------------------------------
data Tree = NilT | Node Int Tree Tree deriving (Show, Eq)

-- a) Retornar a soma de todos os nós de uma árvore binária de inteiros.
addTree :: Tree -> Int
addTree = undefined

-- b) Retornar uma lista de inteiros resultante do percorrimento infixado.
percorreInfix :: Tree -> [Int]
percorreInfix = undefined


-------------------------------------------------------------------------------
-- QUESTÃO 5 (Expressões e Instâncias de Classes)
-------------------------------------------------------------------------------

-- O tipo Expr já foi modificado para suportar Mul e Div (Questão 5a)
data Expr = Lit Int 
          | Add Expr Expr 
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          deriving (Show)
          -- Nota: Removido o 'deriving Eq' padrão pois você implementará o seu próprio na 5b!

-- a) Modifique a função eval para também tratar multiplicação e divisão.
eval :: Expr -> Int
eval (Lit n) = n
eval (Add e1 e2) = (eval e1) + (eval e2)
eval (Sub e1 e2) = (eval e1) - (eval e2)
-- Adicione as regras de Mul e Div abaixo:


-- b) Torne o tipo Expr uma instância da type class Eq, de forma que a igualdade 
-- seja baseada na avaliação (eval) e não na igualdade estrutural.
-- Ex: Add (Lit 1) (Lit 2) == Lit 3 deve retornar True.
-- 
-- ATENÇÃO: Substitua o corpo da instância abaixo pela sua solução!
instance Eq Expr where
    (==) e1 e2 = error "Nao implementado"



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
    putStrLn "  RESULTADOS DA PROVA 1                              "
    putStrLn "====================================================="
    
    rodarTestes "Questão 1 - Avaliação Mental" [
        ("a) 1 + 2", verificar "3" q1_a),
        ("b) Booleano", verificar "True" q1_b),
        ("c) Tuplas", verificar "False" q1_c),
        ("d) 1 + True", verificar "Erro de Tipo" q1_d),
        ("e) Concatenação Strings", verificar "Ola Mundo" q1_e),
        ("f) String ++ [1]", verificar "Erro de Tipo" q1_f),
        ("g) Listas", verificar "[5,6,7,8,9,10,0,1,2,3,4]" q1_g),
        ("h) let fst...", verificar "8" q1_h)
      ]
      
    rodarTestes "Questão 2 - Ordenação de Listas" [
        ("a) ordenaDecrescente", verificar [4,3,2,1] (ordenaDecrescente [3,1,4,2])),
        ("a) ordenaDecrescente vazia", verificar [] (ordenaDecrescente [])),
        ("b) isDecrescente verdadeiro", verificar True (isDecrescente [5,4,3,1])),
        ("b) isDecrescente falso", verificar False (isDecrescente [5,4,6,1]))
      ]

    rodarTestes "Questão 3 - Avaliação Passo-a-Passo" [
        ("Resultado de g [4,5,6,7]", verificar 30 resultadoQ3)
      ]

    rodarTestes "Questão 4 - Árvores (Tree)" [
        ("a) addTree simples", verificar 10 (addTree (Node 5 (Node 2 NilT NilT) (Node 3 NilT NilT)))),
        ("a) addTree vazia", verificar 0 (addTree NilT)),
        ("b) percorreInfix", verificar [2,5,3] (percorreInfix (Node 5 (Node 2 NilT NilT) (Node 3 NilT NilT))))
      ]

    rodarTestes "Questão 5 - Expressões (Expr)" [
        ("a) eval Mul", verificar 6 (eval (Mul (Lit 2) (Lit 3)))),
        ("a) eval Div", verificar 5 (eval (Div (Lit 10) (Lit 2)))),
        ("b) Instância Eq - Igualdade Lógica 1", verificar True (Add (Lit 1) (Lit 2) == Lit 3)),
        ("b) Instância Eq - Igualdade Lógica 2", verificar True (Mul (Lit 3) (Lit 4) == Sub (Lit 20) (Lit 8))),
        ("b) Instância Eq - Desigualdade", verificar False (Lit 4 == Lit 5))
      ]
      
    putStrLn "\n====================================================="