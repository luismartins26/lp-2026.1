{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Exception (try, evaluate, SomeException)

-------------------------------------------------------------------------------
-- DEFINIÇÃO DOS TIPOS ALGÉBRICOS E FUNÇÕES FORNECIDAS
-------------------------------------------------------------------------------

data List t = Nil | Cons t (List t)
  deriving (Eq, Ord, Show)

data Tree t = NilT | Node t (Tree t) (Tree t)
  deriving (Eq, Ord, Show)

data Expr = Lit Int | Add Expr Expr | Sub Expr Expr
  deriving (Eq, Show)

-- Função de avaliação fornecida no material
eval :: Expr -> Int
eval (Lit n) = n
eval (Add e1 e2) = (eval e1) + (eval e2)
eval (Sub e1 e2) = (eval e1) - (eval e2)


-------------------------------------------------------------------------------
-- DECLARAÇÃO DAS FUNÇÕES (A SEREM IMPLEMENTADAS)
-------------------------------------------------------------------------------

-- 1) Transformar uma expressão em String. 
showExpr :: Expr -> String
showExpr (Lit n) = show n
showExpr (Add e1 e2) = "("++showExpr e1++" + "++showExpr e2++")"
showExpr (Sub e1 e2) = "("++showExpr e1++" - "++showExpr e2++")"

-- 2) Converter do tipo 'List t' (customizado) para a lista padrão do Haskell '[t]'
toList :: List t -> [t]
toList Nil = []
toList (Cons h tl) = h:toList tl

-- 3) Converter da lista padrão do Haskell '[t]' para o tipo 'List t' (customizado)
fromList :: [t] -> List t
fromList [] = Nil
fromList (h:tl) = Cons h (fromList tl)

-- 4) Retornar a profundidade da árvore.
-- DICA DE TESTE: O teste considera NilT como profundidade 0.
depth :: Tree t -> Int
depth NilT = 0
depth (Node _ st1 st2) = 1 + max (depth st1) (depth st2)

-- 5) Colapsar a árvore em uma lista.
-- DICA DE TESTE: O teste espera um percurso "em-ordem" (esquerda, raiz, direita).
colapse :: Tree t -> [t]
colapse NilT = []
colapse (Node n st1 st2) = colapse st1 ++ [n] ++ colapse st2

-- 6) Aplicar uma função a todos os nós da árvore
mapTree :: (t -> u) -> Tree t -> Tree u
mapTree _ NilT = NilT
mapTree f (Node n st1 st2) = Node (f n) (mapTree f st1) (mapTree f st2)


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
    putStrLn "  RESULTADOS DOS EXERCÍCIOS: TIPOS ALGÉBRICOS        "
    putStrLn "====================================================="
    
    rodarTestes "Questão 1 - Expr (Expressões)" [
        ("showExpr Lit", verificar "5" (showExpr (Lit 5))),
        ("showExpr Add", verificar "(2 + 3)" (showExpr (Add (Lit 2) (Lit 3)))),
        ("showExpr Sub", verificar "(10 - 4)" (showExpr (Sub (Lit 10) (Lit 4))))
      ]
      
    rodarTestes "Questões 2 e 3 - Listas Customizadas" [
        ("toList", verificar [1, 2, 3] (toList (Cons 1 (Cons 2 (Cons 3 Nil))))),
        ("fromList", verificar (Cons 5 (Cons 6 Nil)) (fromList [5, 6])),
        ("Ida e volta (fromList . toList)", verificar [10, 20] (toList (fromList [10, 20])))
      ]
      
    rodarTestes "Questões 4, 5 e 6 - Árvores (Tree)" [
        ("depth folha (NilT)", verificar 0 (depth NilT)),
        ("depth nó simples", verificar 1 (depth (Node 5 NilT NilT))),
        ("depth árvore maior", verificar 3 (depth (Node 5 (Node 3 (Node 1 NilT NilT) NilT) (Node 8 NilT NilT)))),
        
        ("colapse em-ordem", verificar [1, 3, 5, 8] (colapse (Node 5 (Node 3 (Node 1 NilT NilT) NilT) (Node 8 NilT NilT)))),
        
        ("mapTree (*2)", verificar (Node 10 (Node 6 NilT NilT) NilT) (mapTree (*2) (Node 5 (Node 3 NilT NilT) NilT)))
      ]
      
    putStrLn "\n====================================================="