
-- Definição das árvore sintática para representação dos programas:

data E = Num Int
      |Var String
      |Soma E E
      |Sub E E
      |Mult E E
   deriving(Eq,Show)

data B = TRUE
      | FALSE
      | Not B
      | And B B
      | Or  B B
      | Leq E E
      | Igual E E  -- verifica se duas expressões aritméticas são iguais
   deriving(Eq,Show)

data C = While B C
    | If B C C
    | Seq C C
    | Atrib E E
    | Skip
    | Throw -- gera uma exceção
    | Try C C -- Try C1 C2 --tenta executar C1, caso ocorra exceção, executa o catch (C2). Caso não ocorra exceção em C1, C2 nunca é executado
    | ThreeTimes C   ---- Executa o comando C 3 vezes
    | DoWhile C B --- DoWhile C B: executa C enquanto B é verdadeiro
    | Loop C E      ---- Loop E C: executa E vezes o comando C 
    | Assert B C --- Assert B C: caso B seja verdadeiro, executa o comando C
    | ExecWhile E E C -- ExecWhile E1 E2 C: Enquanto a expressão E1 for menor que a expressão E2, executa C 
    | DAtrrib E E E E -- Dupla atribuição: recebe duas variáveis "e1" e "e2" e duas expressões "e3" e "e4". Faz e1:=e3 e e2:=e4.
   deriving(Eq,Show)         

-----------------------------------------------------
-----
----- As próximas funções, servem para manipular a memória (sigma)
-----
------------------------------------------------


--- A próxima linha de código diz que o tipo memória é equivalente a uma lista de tuplas, onde o
--- primeiro elemento da tupla é uma String (nome da variável) e o segundo um Inteiro
--- (conteúdo da variável):


type Memoria = [(String,Int)]

exSigma :: Memoria
exSigma = [ ("x", 10), ("temp",0), ("y",0)]


--- A função procuraVar recebe uma memória, o nome de uma variável e retorna o conteúdo
--- dessa variável na memória. Exemplo:
---
--- *Main> procuraVar exSigma "x"
--- 10


procuraVar :: Memoria -> String -> Int
procuraVar [] s = error ("Variavel " ++ s ++ " nao definida no estado")
procuraVar ((s,i):xs) v
  | s == v     = i
  | otherwise  = procuraVar xs v


--- A função mudaVar, recebe uma memória, o nome de uma variável e um novo conteúdo para essa
--- variável e devolve uma nova memória modificada com a varíável contendo o novo conteúdo. A
--- chamada
---
--- *Main> mudaVar exSigma "temp" 20
--- [("x",10),("temp",20),("y",0)]
---
---
--- essa chamada é equivalente a operação exSigma[temp->20]

mudaVar :: Memoria -> String -> Int -> Memoria
mudaVar [] v n = error ("Variavel " ++ v ++ " nao definida no estado")
mudaVar ((s,i):xs) v n
  | s == v     = ((s,n):xs)
  | otherwise  = (s,i): mudaVar xs v n


-------------------------------------
---
--- Completar os casos comentados das seguintes funções:
---
---------------------------------

smallStepE :: (E, Memoria) -> (E, Memoria)
smallStepE (Var x, s)                  = (Num (procuraVar s x), s)
smallStepE (Soma (Num n1) (Num n2), s) = (Num (n1 + n2), s)
smallStepE (Soma (Num n) e, s)         = let (el,sl) = smallStepE (e,s)
                                         in (Soma (Num n) el, sl)
smallStepE (Soma e1 e2,s)              = let (el,sl) = smallStepE (e1,s)
                                         in (Soma el e2,sl)
smallStepE (Mult (Num n1) (Num n2), s) = (Num (n1 * n2), s)
smallStepE (Mult (Num n) e, s)         = let (el,sl) = smallStepE (e,s)
                                         in (Mult (Num n) el, sl)
smallStepE (Mult e1 e2,s)              = let (el,sl) = smallStepE (e1,s)
                                         in (Mult el e2,sl)
smallStepE (Sub (Num n1) (Num n2), s)  = (Num (n1 - n2), s)
smallStepE (Sub (Num n) e, s)          = let (el,sl) = smallStepE (e,s)
                                         in (Sub (Num n) el, sl)
smallStepE (Sub e1 e2,s)               = let (el,sl) = smallStepE (e1,s)
                                         in (Sub el e2,sl)

smallStepB :: (B,Memoria) -> (B, Memoria)
smallStepB (Not FALSE,s)               = (TRUE,s)
smallStepB (Not TRUE,s)                = (FALSE,s)
smallStepB (Not b,s)                   = let (bdash,s1) = smallStepB (b,s)
                                         in (Not bdash, s1)
smallStepB (And FALSE b,s)             = (FALSE,s)
smallStepB (And TRUE b,s)              = (b,s)
smallStepB (And b1 b2,s)               = let (b1,s1) = smallStepB (b1,s)
                                         in (And b1 b2,s1)
smallStepB (Or FALSE b,s)              = (b,s)
smallStepB (Or TRUE b,s)               = (TRUE,s)
smallStepB (Or b1 b2,s)                = let (b1,s1) = smallStepB (b1,s)
                                         in (Or b1 b2,s1)
smallStepB (Leq (Num n1) (Num n2),s)
 | n1 <= n2                            = (TRUE,s)
 | otherwise                           = (FALSE,s)
smallStepB (Leq (Num n) e,s)           = let (e1,s1) = smallStepE (e,s)
                                         in (Leq (Num n) e1, s1)
smallStepB (Leq e1 e2,s)               = let (e1dash,s1) = smallStepE (e1,s)
                                         in (Leq e1dash e2, s1)
smallStepB (Igual (Num n1) (Num n2),s)
 | n1 == n2                            = (TRUE,s)
 | otherwise                           = (FALSE,s)
smallStepB (Igual (Num n) e,s)         = let (e1,s1) = smallStepE (e,s)
                                         in (Igual (Num n) e1, s1)
smallStepB (Igual e1 e2,s)             = let (e1,s1) = smallStepE (e1,s)
                                         in (Igual e1 e2, s1)

smallStepC :: (C,Memoria) -> (C,Memoria)
smallStepC (If FALSE c1 c2,s)          = (c2,s)
smallStepC (If TRUE c1 c2,s)           = (c1,s)
smallStepC (If b c1 c2,s)              = let (bdash,s) = smallStepB (b,s)
                                         in (If bdash c1 c2,s)
smallStepC (Seq Skip c,s)              = (c,s)
smallStepC (Seq c1 c2,s)               = let (c1dash,s1) = smallStepC (c1,s)
                                         in (Seq c1dash c2,s1)
smallStepC (Atrib (Var x) (Num n),s)   = (Skip,(mudaVar s x n))
smallStepC (Atrib (Var x) e,s)         = let (e1,s1) = smallStepE (e1,s)
                                         in (Atrib (Var x) e1,s1)
smallStepC (While b c, s)              = (If b (Seq c (While b c)) Skip,s)
 -- Throw -- gera uma exceção
 -- Try C C -- Try C1 C2 --tenta executar C1, caso ocorra exceção, executa o catch (C2). Caso não ocorra exceção em C1, C2 nunca é executado
 -- ThreeTimes C   ---- Executa o comando C 3 vezes
 
 -- DoWhile C B --- DoWhile C B: executa C enquanto B é verdadeiro
 --smallStepC (DoWhile b c,s)            = (Seq c (While b c),s)
 -- Loop C E      ---- Loop E C: executa E vezes o comando C
-- smallStepC (Loop c (Num 0),s)         = (Skip,s)
 --smallStepC (Loop c (Num n),s)         = (Seq c (Loop c (Num n - 1)),s) -- Verificar se esse é mesmo o smallStep
 -- Assert B C --- Assert B C: caso B seja verdadeiro, executa o comando C
 --smallStepC (Assert b c,s)             = (If b c Skip,s)
 -- ExecWhile E E C -- ExecWhile E1 E2 C: Enquanto a expressão E1 for menor que a expressão E2, executa C
 -- DAtrrib E E E E -- Dupla atribuição: recebe duas variáveis "e1" e "e2" e duas expressões "e3" e "e4". Faz e1:=e3 e e2:=e4.
 --smallStepC (DAtrrib e1 e2 e3 e4,s)    = (Seq (Atrib e1 e3) (Atrib e2 e4),s)


----------------------
--  INTERPRETADORES
----------------------


--- Interpretador para Expressões Aritméticas:
isFinalE :: E -> Bool
isFinalE (Num n) = True
isFinalE _       = False


interpretadorE :: (E,Memoria) -> (E, Memoria)
interpretadorE (e,s) = if (isFinalE e) then (e,s) else interpretadorE (smallStepE (e,s))

--- Interpretador para expressões booleanas


isFinalB :: B -> Bool
isFinalB TRUE    = True
isFinalB FALSE   = True
isFinalB _       = False

-- Descomentar quanto a função smallStepB estiver implementada:

interpretadorB :: (B,Memoria) -> (B, Memoria)
interpretadorB (b,s) = if (isFinalB b) then (b,s) else interpretadorB (smallStepB (b,s))


-- Interpretador da Linguagem Imperativa

isFinalC :: C -> Bool
isFinalC Skip    = True
isFinalC _       = False

-- Descomentar quando a função smallStepC estiver implementada:

--interpretadorC :: (C,Memoria) -> (C, Memoria)
--interpretadorC (c,s) = if (isFinalC c) then (c,s) else interpretadorC (smallStepC (c,s))


--------------------------------------
---
--- Exemplos de programas para teste
---
--- O ALUNO DEVE IMPLEMENTAR EXEMPLOS DE PROGRAMAS QUE USEM 
--- OS COMANDOS NOVOS PRINCIPALMENTE O TRATAMENTO DE EXCEÇÕES
--

exSigma2 :: Memoria
exSigma2 = [("x",3), ("y",0), ("z",0)]


---
--- O progExp1 é um programa que usa apenas a semântica das expressões aritméticas. Esse
--- programa já é possível rodar com a implementação que fornecida:

progExp1 :: E
progExp1 = Soma (Num 3) (Soma (Var "x") (Var "y"))

---
--- para rodar:
-- A função smallStepE anda apenas um passo na avaliação da Expressão

-- *Main> smallStepE (progExp1, exSigma)
-- (Soma (Num 3) (Soma (Num 10) (Var "y")),[("x",10),("temp",0),("y",0)])

-- Note que no exemplo anterior, o (Var "x") foi substituido por (Num 10)

-- Para avaliar a expressão até o final, deve-se usar o interpretadorE:

-- *Main> interpretadorE (progExp1 , exSigma)
-- (Num 13,[("x",10),("temp",0),("y",0)])

-- *Main> interpretadorE (progExp1 , exSigma2)
-- (Num 6,[("x",3),("y",0),("z",0)])


--- Para rodar os próximos programas é necessário primeiro implementar as regras que faltam
--- e descomentar os respectivos interpretadores


---
--- Exemplos de expressões booleanas:


teste1 :: B
teste1 = (Leq (Soma (Num 3) (Num 3))  (Mult (Num 2) (Num 3)))

teste2 :: B
teste2 = (Leq (Soma (Var "x") (Num 3))  (Mult (Num 2) (Num 3)))


---
-- Exemplos de Programas Imperativos:

testec1 :: C
testec1 = (Seq (Seq (Atrib (Var "z") (Var "x")) (Atrib (Var "x") (Var "y"))) 
               (Atrib (Var "y") (Var "z")))

fatorial :: C
fatorial = (Seq (Atrib (Var "y") (Num 1))
                (While (Not (Igual (Var "x") (Num 1)))
                       (Seq (Atrib (Var "y") (Mult (Var "y") (Var "x")))
                            (Atrib (Var "x") (Sub (Var "x") (Num 1))))))
