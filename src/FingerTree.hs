-- Emilio Francesquini <e.francesquini@ufabc.edu.br>
-- CC-BY-SA
-- 11/2019

-- Esta implementação foi adaptada do código do paper "Fingertrees: a
-- simple general-purpose data structure" por Ralf Hinze e Ross
-- Paterson. http://www.staff.city.ac.uk/~ross/papers/FingerTree.pdf
-- Também foi salpicada com diversos insights de Andrew Gibiansky:
-- http://andrew.gibiansky.com/blog/haskell/finger-trees/

{-# LANGUAGE GADTs #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module FingerTree where

import qualified Data.Foldable as F
import Data.Maybe

-- Representa os nós intermediários da finger tree. Nossa árvore pode
-- ter 2 ou 3 filhos por nó. Note que como o tipo é paramétrico, o
-- tipo Node guarda tanto os elementos propriamente ditos quanto os
-- nós intermediários.
data Node v a where
  Node2 :: Monoid v => v -> a -> a -> Node v a
  Node3 :: Monoid v => v -> a -> a -> a -> Node v a

deriving instance (Show a, Show v) => Show (Node v a)

-- Construtores auxiliares que já calculam measure dos parâmetros
node2 :: Measured a v => a -> a -> Node v a
node2 a b = Node2 (measure a <> measure b) a b

node3 :: Measured a v => a -> a -> a -> Node v a
node3 a b c = Node3 (measure a <> measure b <> measure c) a b c

-- Guarda os elementos apontados pela espinha da árvore. A princípio
-- poderíamos ter usado uma lista, mas o fato de usarmos uma estrutura
-- de dados específica garante ao mesmo tempo que teremos uma árvore
-- bem formada (1 a 4 dígitos) e também de quebra ganhamos um pouco de
-- desempenho.
data Digit a where
  One   :: a -> Digit a
  Two   :: a -> a -> Digit a
  Three :: a -> a -> a -> Digit a
  Four  :: a -> a -> a -> a -> Digit a

deriving instance Show a => Show (Digit a)

-- Equivalente ao cons
concatL :: Digit a -> a -> Digit a
concatL (One a) x   = Two x a
concatL (Two a b) x = Three x a b
concatL (Three a b c) x = Four x a b c
concatL Four{} _ = error "Não deveria tentar criar um dígito > 3"

-- Equivalente ao snoc
concatR :: Digit a -> a -> Digit a
concatR (One a) = Two a
concatR (Two a b) = Three a b
concatR (Three a b c) = Four a b c
concatR Four{} = error "Não deveria tentar criar um dígito > 3"

-- Dada uma lista de dígitos, junta-os até que ele cheque a capacidade
-- (4 elementos) ou a lista tenha acabado.
dJoin :: [Digit a] -> [Digit a]
dJoin [] = []
dJoin [x] = [x]
dJoin (x@Four{}:xs) = x : dJoin xs
dJoin (x:xs) =
  dJoin $ concatR x d : xs'
  where
    (d, xs') = dTakeOne xs

-- Retira e devolve o elemento mais a esquerda na lista de
-- Dígitos. Rearranja a estrutura para manter os elementos na lista
-- consistentes.
dTakeOne :: [Digit a] -> (a, [Digit a])
dTakeOne [] = undefined
dTakeOne (One a:xs) = (a, xs)
dTakeOne (x:xs) =
  (dFirst x, fromJust (dTail x):xs)

-- Primeiro elemento de um Digit
dFirst :: Digit a -> a
dFirst (One a)        = a
dFirst (Two a _)      = a
dFirst (Three a _ _)  = a
dFirst (Four a _ _ _) = a

-- Último elemento de um Digit
dLast :: Digit a -> a
dLast (One z)        = z
dLast (Two _ z)      = z
dLast (Three _ _ z)  = z
dLast (Four _ _ _ z) = z

-- Cauda de um Digit
dTail :: Digit a -> Maybe (Digit a)
dTail One{}          = Nothing
dTail (Two _ b)      = Just $ One b
dTail (Three _ b c)  = Just $ Two b c
dTail (Four _ b c d) = Just $ Three b c d

-- O início de um Digit
dInit :: Digit a -> Maybe (Digit a)
dInit One{}          = Nothing
dInit (Two a _)      = Just $ One a
dInit (Three a b _)  = Just $ Two a b
dInit (Four a b c _) = Just $ Three a b c

-- Converte um digito para uma lista
dToList :: Digit a -> [a]
dToList (One a) = [a]
dToList (Two a b) = [a, b]
dToList (Three a b c) = [a, b, c]
dToList (Four a b c d) = [a, b, c, d]



-- Estrutura de dadoc que representa uma FingerTree.
-- Na espinha, a cada nível aumentamos a altura da árvore armazenada.
data FingerTree v a where
  Empty :: Measured a v => FingerTree v a
  -- Como digits precisam ser >0 precisamos de um caso para tratar
  -- árvores de tamanho 1.
  Single :: Measured a v => a -> FingerTree v a
  -- Note que a cada nível mais baixo da espinha, temos como raiz uma
  -- do tipo FingerTree (Node a)^n
  --                    label   left               spine              right
  Deep :: Measured a v => v -> Digit a -> FingerTree v (Node v a) -> Digit a -> FingerTree v a

deriving instance (Show a, Show v)  => Show (FingerTree v a)

-- Auxiliar para construir árvores profundas. Toma o cuidado de
-- calcular a medida de cada nó a partir dos valores de l s e r
deep :: (Measured a v) => Digit a -> FingerTree v (Node v a) -> Digit a -> FingerTree v a
deep l s r = Deep (measure l <> measure s <> measure r) l s r

-- ---------------------
-- ---------------------
-- Construção
-- ---------------------
-- ---------------------

-- Imagine o símbolo <| indicando que a operação é na extremidade
-- esquerda
infixr 5 <|
(<|) :: a -> FingerTree v a -> FingerTree v a
-- Caso inicial e trivial: cria uma árvore single
x <| Empty = Single x
-- Já temos elementos suficientes para uma árvore de verdade
x <| Single y = deep (One x) Empty (One y)
-- Adicionar um novo elemento é trivial exceto no caso da ãrvore já
-- conter 2 elementos. Neste caso, empurramos 3 para um um novo nó e
-- deixamos 2 para trás.
x <| Deep _ (Four a  b  c  d) s r =
  deep (Two x a) (node3 b c d <| s) r
-- Se chegou aqui então tem espaço no nó inicial
x <| (Deep _ l s r) = deep (concatL l x) s r

-- A implementação de |> é simétrica à de <|.  Imagine o símbolo |>
-- indicando que a operação é na extremidade direita. Note que por
-- esta razão a ordem dos parâmetros é invertida.
infixl 5 |>
(|>) :: FingerTree v a -> a -> FingerTree v a
Empty |> x = Single x
Single y |> x = deep (One y) Empty (One x)
Deep _ l s (Four a b c d) |> x =
  deep l (s |> node3 a b c) (Two d x)
(Deep _ l s r) |> x =
  deep l s (concatR r x)

-- Folding para Digit
instance Foldable Digit where
  foldMap f (One a) = f a
  foldMap f (Two a b) = f a <> f b
  foldMap f (Three a b c) = f a <> f b <> f c
  foldMap f (Four a b c d) = f a <> f b <> f c <> f d

instance Foldable (Node v) where
  foldMap f (Node2 _ a b) = f a <> f b
  foldMap f (Node3 _ a b c) = f a <> f b <> f c

-- -- Elementos da esquerda para a direita
instance Foldable (FingerTree v) where
  foldMap _ Empty = mempty
  foldMap f (Single x) = f x
  foldMap f (Deep _ l s r) =
    foldMap f l <> foldMap (foldMap f) s <> foldMap f r

-- Transforma Foldables em uma árvore
fromFoldable :: (Measured a v, Foldable f) => f a -> FingerTree v a
fromFoldable = foldr (<|) Empty

-- Desconstrução da árvore. Poderíamos usar um Maybe mas a escolha por
-- um tipo específico deixa o código mais claro
data View s a = Nil | ConsV a (s a) deriving Show

viewL :: FingerTree v a -> View (FingerTree v) a
viewL Empty          = Nil -- Caso trivial 1
viewL (Single x)     = ConsV x Empty -- Caso trivial 2
-- Caso recursivo, usamos a função auxiliar para "emprestar"
-- elementos da espinha ou de r
viewL (Deep _ l s r) = ConsV (dFirst l) (deepL (dTail l) s r)


-- Recebe o que sobrou de l (que pode ser nada) e devolve uma árvore
-- válida.
deepL :: Measured a v
      => Maybe (Digit a) -- Sobra de l
      -> FingerTree v (Node v a) -- Espinha
      -> Digit a -- r
      -> FingerTree v a
-- Se sobrou algo fica fácil, só remonta a árvore e devolve
deepL (Just l) s r = deep l s r
-- Não sobrou nada do lado esquerdo. Tenta emprestar.
deepL Nothing s r =
  case viewL s of
    -- A espinha não tem o que emprestar. Devolve r transformado em
    -- uma árvore (já que l e s estão vazios)
    Nil -> fromFoldable r
    ConsV a s'-> -- A espinha tem pelo menos um elemento, joga ele
                 -- para a esquerda. Pode não ter sobrado nada na
                 -- espinha (s'). Neste caso s' é Empty e este valor é
                 -- colocado de volta espinha.
      case a of
        Node2 _ x y   -> deep (Two x y) s' r
        Node3 _ x y z -> deep (Three x y z) s' r


-- Simétrico ao caso à esquerda
viewR :: FingerTree v a -> View (FingerTree v) a
viewR Empty          = Nil
viewR (Single x)     = ConsV x Empty
viewR (Deep _ l s r) = ConsV (dLast r) (deepR l s (dInit r))

deepR ::  Measured a v
      => Digit a
      -> FingerTree v (Node v a)
      -> Maybe (Digit a)
      -> FingerTree v a
deepR l s (Just r) = deep l s r
deepR l s Nothing =
  case viewR s of
    Nil -> fromFoldable l
    ConsV a s'->
      case a of
        Node2 _ x y   -> deep l s' (Two x y)
        Node3 _ x y z -> deep l s' (Three x y z)

-- O uso da view permite implementarmos as funções abaixo.  Note que o
-- uso de Lazyness é o que nos permite usar viewL e não ter que
-- calcular o valor da view completa. Caso a linguagem utilizada fosse
-- estrita, talvez valesse a pena criar duas versões de views diferentes
treeHead :: FingerTree v a -> a
treeHead t =
  case viewL t of
    ConsV x _ -> x
    Nil       -> error "treeHead: empty tree"

treeTail :: FingerTree v a -> FingerTree v a
treeTail t =
  case viewL t of
    ConsV _ xs -> xs
    Nil        -> error "treeTail: empty tree"

treeLast :: FingerTree v a -> a
treeLast t =
  case viewR t of
    ConsV x _ -> x
    Nil       -> error "treeLast: empty tree"

treeInit :: FingerTree v a -> FingerTree v a
treeInit t =
  case viewR t of
    ConsV _ xs -> xs
    Nil        -> error "treeInit: empty tree"

-- Uma implementação alternativa seria fazer:
--
-- treeNull Empty = True
-- treeNull _     = False
--
-- Mas esta implementação, em uma linguagem lazy tipo Haskell é tão
-- boa quanto. Também poderíamos usar o viewR neste caso.
treeNull :: FingerTree v a -> Bool
treeNull t = case viewL t of
  Nil         -> True
  (ConsV _ _) -> False


-- ---------------------
-- ---------------------
-- Concatenação
-- ---------------------
-- ---------------------

-- Qual é a complexidade de naiveConcat?
naiveConcat :: FingerTree v a -> FingerTree v a -> FingerTree v a
naiveConcat l Empty = l
naiveConcat l r =
  naiveConcat (l |> x) xs
  where
    ConsV x xs = viewL r

-- Note que a função nodes roda em tempo constante:
-- l tem no máximo 4 elementos
-- mm, se Just, tem no máximo 4 elementos
-- r tem no máximo 4 elementos
-- No total temos, portanto, 12 elementos no máximo
-- que são combinados em Nodes2 ou 3 totalizando no máximo um Four
nodes :: Measured a v => Digit a -> Maybe (Digit a) -> Digit a -> Digit (Node v a)
nodes l mm r =
  -- Junta os Digits menores em um só Digit. Isso só é possível pois o
  -- tamanho máximo de l mm e r combinado é 12.
  let [dig] = dJoin nds in dig
  where
    l' = dToList l
    mm' = maybe [] dToList mm
    r'= dToList r

    nodes' []  = undefined -- 2 primeiros casos impossíveis pois
    nodes' [_] = undefined -- l e m juntos tem >= 2 elementos
    nodes' [a, b]       = [One $ node2 a b]
    nodes' [a, b, c]    = [One $ node3 a b c]
    nodes' [a, b, c, d] = [Two (node2 a b) (node2 c d)]
    nodes' (a:b:c:xs)   = (One $ node3 a b c) : nodes' xs

    nds = nodes' $ l' ++ mm' ++ r'

(><) :: FingerTree v a -> FingerTree v a -> FingerTree v a
l >< r = concat3 l Nothing r

concat3 :: FingerTree v a -> Maybe (Digit a) -> FingerTree v a -> FingerTree v a
-- Casos triviais à esquerda
concat3 Empty      Nothing  r = r
concat3 Empty      (Just d) r = dFirst d <| concat3 Empty (dTail d) r
concat3 (Single y) md       r = y <| concat3 Empty md r
-- Casos triviais à direita
concat3 l Nothing  Empty      = l
concat3 l (Just d) Empty      = concat3 l (dInit d) Empty |> dLast d
concat3 l md       (Single y) = concat3 l md Empty |> y
-- Caso cabeludo: ambas as árvores são profundas
concat3 (Deep _ l0 s0 r0) md (Deep _ l1 s1 r1) =
  deep l0 nextLevel r1
  where
    -- Chamada recursiva para gerar o próximo nível
    nextLevel = concat3 s0 dNodes s1
    -- Pega a lista dos elementos da direita de l
    -- Pega a lista dos elementos da esquerda de r
    -- Converte esses elementos em nós para passar para concat3.
    dNodes = Just $ nodes r0 md l1


-- ---------------------
-- ---------------------
-- Augmented trees
-- ---------------------
-- ---------------------

class Monoid v => Measured a v | a -> v a where
  measure :: a -> v

instance (Monoid v, Measured a v) => Measured (Digit a) v where
  measure = foldMap measure

instance Monoid v => Measured (Node v a) v where
  measure (Node2 v _ _) = v
  measure (Node3 v _ _ _) = v

instance Monoid v => Measured (FingerTree v a) v where
  measure Empty          = mempty
  measure (Single x)     = measure x
  measure (Deep v _ _ _) = v


-- Devolvem o ponto mais a esquerda onde o predicado p
-- se torna verdadeiro. A função só faz sentido de
-- p node for verdadeira.
splitNode :: (Measured a v)
          => (v -> Bool)
          -> v
          -> Node v a
          -> (Maybe (Digit a), a, Maybe (Digit a))
splitNode p i (Node2 _ a b)
  | p va        = (Nothing, a, Just (One b))
  | otherwise   = (Just (One a), b, Nothing)
  where
    va      = i <> measure a
splitNode p i (Node3 _ a b c)
  | p va        = (Nothing, a, Just (Two b c))
  | p vab       = (Just (One a), b, Just (One c))
  | otherwise   = (Just (Two a b), c, Nothing)
  where
    va      = i <> measure a
    vab     = va <> measure b

-- Devolvem o ponto mais a esquerda onde o predicado p
-- se torna verdadeiro. A função só faz sentido de
-- p d for verdadeira.
splitDigit :: Measured a v
           => (v -> Bool)
           -> v
           -> Digit a
           -> (Maybe (Digit a), a, Maybe (Digit a))
splitDigit _ _ (One d) = (Nothing, d, Nothing)
splitDigit p i d
  | p i'      = (Nothing, a, as)
  | otherwise = let (l, x, r) = splitDigit p i' (fromJust as)
                in  (Just (maybe (One a) (`concatL` a) l), x, r)
  where
    a  = dFirst d
    i' = i <> measure a
    as = dTail d

-- splittree só deve ser chamado se p tree for verdadeiro.
splitTree :: Measured a v => (v -> Bool) -> v -> FingerTree v a -> (FingerTree v a, a, FingerTree v a)
splitTree _ _ Empty      = error "não é possivel splitar árvore vazia"
splitTree _ _ (Single x) = (Empty, x, Empty)
splitTree p i (Deep _ pr m sf)
  | p vpr     = let (l, x, r) = splitDigit p i pr
                in  (maybe Empty fromFoldable l, x, deepL r m sf)
  | p vm      = let (ml, xs, mr) = splitTree p vpr m
                    (l, x, r) = splitNode p (vpr <> measure ml) xs
                in  (deepR pr ml l, x, deepL r mr sf)
  | otherwise = let (l, x, r) = splitDigit p vm sf
                in  (deepR pr m l, x, maybe Empty fromFoldable r)
  where
    vpr = i   <> measure pr
    vm  = vpr <> measure m

split ::  (Measured a v)
      => (v -> Bool)
      -> FingerTree v a
      -> (FingerTree v a, FingerTree v a)
split _ Empty  =  (Empty, Empty)
split p xs
  | p (measure xs) =  (l, x <| r)
  | otherwise   =  (xs, Empty)
  where
    (l, x, r) = splitTree p mempty xs

takeUntil :: (Measured a v) => (v -> Bool) -> FingerTree v a -> FingerTree v a
takeUntil p  =  fst . split p


dropUntil :: (Measured a v) => (v -> Bool) -> FingerTree v a -> FingerTree v a
dropUntil p  =  snd . split p


-- ---------------------
-- ---------------------
-- Aplicações
-- ---------------------
-- ---------------------

class GeneralFingerTreeOps f where
  empty :: f a
  empty = fromList []
  toList   :: f a -> [a]
  fromList :: [a] -> f a
  cons     :: a -> f a -> f a
  snoc     :: f a -> a -> f a
  catenate :: f a -> f a -> f a
  head     :: f a -> a
  tail     :: f a -> f a
  last     :: f a -> a
  init     :: f a -> f a

-- -------------------------------
-- Deques
-- -------------------------------
newtype DequeV a = DequeV {getDequeV :: a} deriving Show

instance Measured (DequeV a) () where
  measure _ = ()

newtype Deque a = Deque (FingerTree () (DequeV a)) deriving Show

instance GeneralFingerTreeOps Deque where
  fromList  = Deque . fromFoldable . map DequeV
  toList (Deque ft) = map getDequeV $ F.toList ft
  cons x (Deque ft) = Deque $ DequeV x <| ft
  snoc (Deque ft) x = Deque $ ft |> DequeV x
  catenate (Deque l) (Deque r) = Deque $ l >< r
  head (Deque ft) = getDequeV $ treeHead ft
  tail (Deque ft) = Deque $ treeTail ft
  last (Deque ft) = getDequeV $ treeLast ft
  init (Deque ft) = Deque $ treeInit ft

-- -------------------------------
-- Sequences
-- -------------------------------
newtype SeqV a = SeqV {getSeqV :: a} deriving Show
newtype Size = Size {getSize :: Int} deriving (Eq, Ord, Show)

newtype Seq a = Seq (FingerTree Size (SeqV a))

deriving instance Show a => Show (Seq a)

instance GeneralFingerTreeOps Seq where
  fromList  = Seq . fromFoldable . map SeqV
  toList (Seq ft) = map getSeqV $ F.toList ft
  cons x (Seq ft) = Seq $ SeqV x <| ft
  snoc (Seq ft) x = Seq $ ft |> SeqV x
  catenate (Seq l) (Seq r) = Seq $ l >< r
  head (Seq ft) = getSeqV $ treeHead ft
  tail (Seq ft) = Seq $ treeTail ft
  last (Seq ft) = getSeqV $ treeLast ft
  init (Seq ft) = Seq $ treeInit ft

instance Semigroup Size where
  (Size x) <> (Size y) = Size $ x + y

instance Monoid Size where
  mempty = Size 0

instance Measured (SeqV a) Size where
  measure _ = Size 1

seqLength :: Seq a -> Int
seqLength (Seq ft) = getSize $ measure ft

seqSplitAt :: Int -> Seq a -> (Seq a, Seq a)
seqSplitAt i s@(Seq ft)
  | i <= 0          = (Seq Empty, Seq ft)
  | seqLength s < i = (Seq ft, Seq Empty)
  | otherwise       = (Seq l, Seq r)
  where
    (l, r) = split (Size i <) ft

(!) :: Seq a -> Int -> a
Seq ft ! i = x
  where
    (_, SeqV x, _) = splitTree (Size i < ) (Size 0) ft

-- -------------------------------
-- (min)Heaps
-- -------------------------------

-- A finger tree armazena os itens com suas prioridades
data HeapV a = HeapV {
  priority :: Int,
  item :: a
  } deriving Show

-- PositiveInfinity é o elemento identidade
data Priority = Priority Int | PositiveInfinity
  deriving (Eq, Ord, Show)

instance Semigroup Priority where
  -- PositiveInfinity é tradado como o menos prioritário de todos
  PositiveInfinity <> x                = x
  x                <> PositiveInfinity = x
  (Priority x)     <> (Priority y)     = Priority $ min x y

instance Monoid Priority where
  mempty = PositiveInfinity

instance Measured (HeapV a) Priority where
  measure = Priority . priority

newtype Heap a = Heap (FingerTree Priority (HeapV a))

deriving instance Show a => Show (Heap a)

heapPush :: Heap a -> a -> Int -> Heap a
heapPush (Heap ft) x prio = Heap $ HeapV prio x <| ft

heapPop :: Show a => Heap a -> (a, Heap a)
heapPop (Heap ft) = (x, Heap $ l >< r)
  where
    (l, HeapV _ x, r) = splitTree (measure ft >=) mempty ft

heapToOrderedList :: Show a => Heap a -> [a]
heapToOrderedList (Heap Empty) = []
heapToOrderedList h = a : heapToOrderedList h'
  where
    (a, h') = heapPop h






-- A FingerTree da página 4 do paper do Hinze e Patterson ficaria
level3 :: Measured a v => FingerTree v a
level3 = Empty

level2 :: FingerTree () (Node () (DequeV Char))
level2 = deep l level3 r
  where
    l = Two
          (node2 (DequeV 'i') (DequeV 's'))
          (node2 (DequeV 'i') (DequeV 's'))
    r = Two
          (node3 (DequeV 'n') (DequeV 'o') (DequeV 't'))
          (node2 (DequeV 'a') (DequeV 't'))


level1 :: FingerTree () (DequeV Char)
level1 = deep l level2 r
  where
    l = Two (DequeV 't') (DequeV 'h')
    r = Three (DequeV 'r') (DequeV 'e') (DequeV 'e')

hinzePattersonDeque :: Deque Char
hinzePattersonDeque = Deque level1
