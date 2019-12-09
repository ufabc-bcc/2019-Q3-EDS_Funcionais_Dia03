# Estruturas de dados puramente funcionais - Dia 03
http://pesquisa.ufabc.edu.br/haskell/cursos/19.q3.eds_funcionais/

Universidade Federal do ABC

Emilio Francesquini

e.francesquini@ufabc.edu.br

2019.Q3

Código e exercícios relativos ao segundo dia do curso.
- [Slides](http://pesquisa.ufabc.edu.br/haskell/cursos/19.q3.eds_funcionais/files/dia03.pdf)
  - Toda a explicação do código pode ser encontrada nos slides de aula.


Estes exercícios envolvem a alteração do código dado em aula. Uma suite de testes (parcial!) que testa apenas os casos 'happy day' foi fornecida. Para executá-la basta fazer `stack test` no diretório raiz. Caso deseje, estenda a suite de testes com os seus próprios.

## Exercícios


  1. A função `nodes` recebe até 12 valores guardados em 2 `Digit` e um  `Maybe Digit`. Para simplificar a explicação do algoritmo, o código transforma os `Digit` em listas e depois transforma o resultado `[Digit]` (lista de `Digit`) em um só `Digit` através de uma chamada à função `djoin`. Esta não é, contudo, a maneira mais simples de implementar o código. Aproveite o fato de que tanto `Digit` quanto `Node` serem `Foldable` e reescreva a função para que não seja necessário transformar nem os `Digit` de entrada em listas nem que seja utilizada a função `dJoin`. Dica, use folds em conjunto com as funções auxiliares para `Digit` que utilizamos para implementar as views (talvez seja necessário criar funções semelhantes para `Node`).

  2. Considere a implementação de min-heap que fizemos utilizando Finger Trees.

     a) A operação para localizar o elemento com a menor prioridade leva tempo O(lg n). Altere a implementação do Heap para que ele passe a levar O(1). Atenção, a implementação deve continuar sendo baseada em Finger Trees e a complexidade das demais operações deve continuar a ser O(lg n).

     b) Implemente a operação de merge para o heaps com a seguinte assinatura: `heapMerge :: Heap a -> Heap a -> Heap a`. A função recebe dois heaps e devolve um heap que é a junção destes.


  3. A operação binária que devolve como resultado o segundo operando é associativa. Considerando essa operação e um elemento neutro `Nada` podemos definir:

``` haskell
data Valor a = Nada | UmValor a deriving (Eq, Ord, Show)

instance Semigroup (Valor a) where
  v <> Nada = v
  _ <> v    = v

instance Monoid (Valor a) where
  mempty = Nada
```

     Considerando esse monoide e uma finger tree, implemente o tipo `OrdSeq` que mantém uma lista ordenada. A sua implementação deve seguir o mesmo modelo utilizado para a implementação de Deques, Seq e Heap além de fornececer as seguintes funções:

       - `partition x t` - devolve uma tupla contendo duas `OrdSeq`. Uma contendo todos os elementos em `t` à esqueda de `x` (ou, de forma equivalente, < x) e outra com todos os elementos `>=xs`.
       - `insert x t` - Recebe um elemento `x` e o insere na `OrdSeq` `t` mantendo a  os elementos ordenados.
       - `deleteAll x t` - devolve uma `OrdSeq` com todas as ocorrências de `x` removidas.
