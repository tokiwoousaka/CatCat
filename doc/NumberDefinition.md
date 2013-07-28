CatCatにおける自然数型実装までの流れ
====================================

CatCatは @its_out_of_tune が構想中（2013/7/28現在)の純粋関数型プログラミング言語である。  
本言語はプリミティブな形を__一切__持たず、BoolやTupleといった基本的な型も関数の型の組み合わせとして表現する。

ここでは、本言語を用いて、最終的に自然数を定義するまでの流れを追うことで、CatCatの型に対する考え方を説明する。

基本的な構文
------------

λ2(second-order lambda calculus)に法った文法を採用している。

```
Id    := /\a . \x^a . x : Forall a . a -> a
Const := /\a, b . \x^a . \y^b . x : Forall a, b . a -> b -> a
```

Unit型とCatCatにおけるパターンマッチの考え方
--------------------------------------------

```
Unit := Forall a . a -> a
UNIT := /\a . \x^a . x : Unit

MatchUnit := /\a . \x^a . \f^(Unit -> a) . x : Forall a . Unit -> (Unit -> a) -> a 
```

Bool型と論理演算
----------------

```
TRUE := /\a . \x^a . \y^a . x : Forall a . a -> a -> a
FALSE := /\a . \x^a . \y^a . y : Forall a . a -> a -> a

--型に名前を付ける、記述を簡略化する。実質、関数定義と同じ
Bool := Forall a . a -> a -> a

--ブール演算の定義
And := \x^Bool . \y^Bool . x Bool y False : Bool -> Bool -> Bool
Or  := \x^Bool . \y^Bool . x Bool True y  : Bool -> Bool -> Bool
Not := \x^Bool . x Bool False True : Bool -> Bool
```

Tuple型、Either型
-----------------

```
Tuple := ^a, b . Forall c . (a -> b -> c) -> c
TUPLE := /\a, b . \x^a . \y^b . 
  /\c . \f^(a -> b -> c) . f x y : Forall a, b . a -> b -> Tuple a b

Fst := /\a, b . \f^Tuple a b . f a (\x^a . \y^b . x) : Forall a, b . Tuple a b -> a
Snd := /\a, b . \f^Tuple a b . f b (\x^a . \y^b . y) : Forall a, b . Tuple a b -> b
```

```
Either := ^a, b . Forall c . (a -> c) -> (b -> c) -> c

LEFT := /\a, b . \x^a . /\c . \f^(a -> c) . \g^(b -> c) . f x
  : Forall a, b . a -> Either a b
RIGHT := /\a, b . \x^b . /\c . \f^(a -> c) . \g^(b -> c) . g x
  : Forall a, b . b -> Either a b

MatchEither := /\a, b . /\c . \x^(Either a b) . \f^(a -> c) . \g^(b -> c) 
  . x c f g : Forall a, b . (Forall c . Either a b -> (a -> c) -> (b -> c) -> c)
```

Num型の実装
-----------

```
Num := Either Unit Num

Zero := LEFT Unit
Succ := \x^Num -> RIGHT x : Num -> Num
```
