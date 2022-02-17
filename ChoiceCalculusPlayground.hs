-- Uma dimensão 'DimId' pode ter múltiplas tags 'TagId' representando possíveis escolhas
type DimId = String
type TagId = String

-- Vamos construir um objeto ciente de variabilidade.
data V a = Obj a -- Não precisamos retornar o objeto em uma estrutura de árvore somente retornamos a representação do objeto na Object Language
    | Dim DimId [TagId] (V a) -- Definindo uma nova dimenção 'Dim' com nome 'DimId', as tags 'TagId' e uma expressão variacional
    | Chc DimId [V a] -- Para cada tag da dimensão 'DimId', apresentamos uma expressão 'V a' para escolha
    deriving (Show)

-- Uma dimensão atômica é uma que limitamos o escopo a uma única decisão, e retornamos a expressão de cara logo.
-- A sagada: a expressão retornada por 'atomic' é justamente uma única expressão Chc
atomic :: DimId -> [TagId] -> [V a] -> V a
atomic d ts cs = Dim d ts $ Chc d cs

-- Exercícios:
dimA :: V a -> V a
dimA = Dim "A" ["a1", "a2"]
--
chcA :: [V a] -> V a -- Note: uma construção de uma 'Choice' aceita uma lista de expressões e retorna apenas uma expressão "encodando" as choices
chcA = Chc "A" -- Eu não teria que enfiar a tag em algum lugar?


-- Choice Elimination
-- Obs: veja que a TagId não serve pra nada...
choiceElimination::V a -> DimId -> Int -> V a
choiceElimination (Obj a) _ _ = Obj a
choiceElimination (Dim d' ts' v') d t
    | d' == d = Dim d' ts' v'
    | otherwise = Dim d' ts' (choiceElimination v' d t)
choiceElimination (Chc d' vs) d t
    | d' == d = choiceElimination (vs !! t) d t 
    | otherwise = Chc d $ map (\v' -> choiceElimination v' d t) vs

