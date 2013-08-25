CatCatにおける自然数型実装までの流れ
====================================

CatCatは @its_out_of_tune が構想中（2013/8/26現在)の純粋関数型プログラミング言語である。  
本言語はプリミティブな形を**一切**持たず、BoolやTupleといった基本的な型も関数の型の組み合わせとして表現する。

ここでは、本言語を用いて、最終的に自然数を定義するまでの流れを追うことで、CatCatの型に対する考え方を説明する。

基本的な構文
------------

CatCatの文法は基本的にλ2(second-order lambda calculus)に法っている。  
λ2は単純型付きラムダ計算に全称量化を追加したもので、`System F`とも呼ばれ、多くの関数プログラミング言語の基礎となっている。
詳しくはWikipediaを参照されたし。

* [Wikipedia : System F](http://ja.wikipedia.org/wiki/System_F)([英](http://en.wikipedia.org/wiki/System_F))

まず、与えられた引数をそのまま返す関数`Id`について考えてみよう。  
`Id`関数の型は`任意の型aについて、aを引数に取りaを返す`をそのまま記述し、次のようになる。

```
Forall a . a -> a
```

次に、上記の型へ推論可能なラムダ項を考えよう。
CatCatのラムダ項には、ラムダ項を引数に取るラムダ抽象と、型を引数に取るラムダ抽象の２種類のラムダ抽象がある。

まず形を引数に取るラムダ抽象化から説明する。
型を引数に取るラムダ抽象は`/\<型束縛変数> . <項>`のように書く(記号`/\`は大文字のラムダ（Λ）を表す)  
`<型束縛変数>`には任意の変数名を指定する。この変数は`<項>`の任意の箇所で使う事ができる。

次いで、ラムダ項を引数に取るラムダ抽象についてで説明する。    
基本的には通常のラムダ抽象の事だが、CatCatは型推論機能を持たないため、束縛変数に対して明示的に型を指定しなくてはならない。  
ラムダ項を引数に取るラムダ抽象は、文字λの代わりに`\`を使い、`\<束縛変数>^<型> . <項>` のように記述する。

従って`任意の型aをとり、型aに推論される束縛変数xを取り、xを返すラムダ抽象`は以下のようになる。

```
/\a . \x^a . x
```

さて、これでラムダ抽象とそれによって推論される型が定まったので、あとはこの定義に`Id`という名前を付ければ良い。  
名付けは`<名> := <ラムダ項> : <型>`という構文で行う。  
この構文にそのまま当てはめれば、次のようになる事が解るだろう。

```
Id    := /\a . \x^a . x : Forall a . a -> a
```

これで我々は基本的なCatCatの構文を読むことができるようになったが、例としてもう一つ、`Const`の定義について見てみよう。

Constは`任意の型a、bについて、aを取って「bを取ってaを返す関数」を返す高階関数`である。  
従って、型は`Forall a . Forall b . a -> b -> a`といった形になるが、複数回`Forall`が続く場合、`Forall a, b`のように略記する事ができる。

対してラムダ項は`/\a . /\b . \x^a . \y^b . x`となるが、型を引数に取るラムダ項にも略記方が用意されており、`/\a, b`のように書くことができる。

従って、関数`Const`の定義は次のようになるだろう。

```
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
