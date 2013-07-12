CatCat
======

※ この言語は妄想中だよ！処理系の無い言語なんてまったく評するに値しないってばっちゃが言ってた！！

CatCatは、λ2(second-order lambda calculus)をベースとした純粋関数型プログラミング言語です。  
この言語は、有用な開発環境を目的としたものではなく、型付きラムダ計算における全称量化による多相化の基本中の基本をコンピュータによって再現する事を目標としています。  
そのため、浮動小数点数の計算や演算子の中置記法はおろか、標準入出力ですら言語仕様には含まれません。  
また、依存型はλΠと呼ばれるλ2とは異なる型付きラムダ計算の概念となるため、本言語には含まれません。  

初歩的な静的型付けや簡単な多相化といった、型理論の基礎の学習に使える程度のプログラミング言語になる事を想定しています。

### サンプルプログラム ###

ブール値の定義
```
--ラムダ項 Γ |- Λa . λx^a . λy^a . x : ∀a . a -> a- > a を次のように記述する
--今の所、型推論を導入する予定はないため、型は明示的に指定する必要がある
True  := /\a . \x^a . \y^a . x : Forall a . a -> a -> a
False := /\a . \x^a . \y^a . y : Forall a . a -> a -> a

--型に名前を付ける、記述を簡略化する。実質、関数定義と同じ
Bool := Forall a . a -> a -> a

--ブール演算の定義
And := \x^Bool . \y^Bool . x Bool y False : Bool -> Bool -> Bool
Or  := \x^Bool . \y^Bool . x Bool True y  : Bool -> Bool -> Bool
Not := \x^Bool . x Bool False True   : Bool -> Bool

--上記のAndは次のように展開される
--And := \x^(Forall a . a -> a -> a) . \y^(Forall a . a -> a -> a) . 
--  x (Forall a . a -> a -> a) y (/\a . \x^a . \y^a . y : Forall a . a -> a -> a)
--    : (Forall a . a -> a -> a) -> (Forall a . a -> a -> a) -> (Forall a . a -> a -> a)


--If-Then-Else
If := /\a . \x^Bool . \y^a . \z^a . x a y z : Forall a . Bool -> a -> a -> a
```

二組のタプル
```
--<a, b> は <a> . <b>の略記方 ... この辺の書き方はもうちょっと練り込む必要あり
Tuple := <a, b> . Forall c . (a -> b -> c) -> c
MakeTuple := /\a, b . \x^a . \y^b . 
  /\c . \f^(a -> b -> c) . f x y : Forall a, b . a -> b -> Tuple a b

Fst := /\a, b . \f^Tuple a b . f a (\x^a . \y^b . x) : Forall a, b . Tuple a b -> a
Snd := /\a, b . \f^Tuple a b . f b (\x^a . \y^b . y) : Forall a, b . Tuple a b -> b
```

チャーチ数による自然数の定義
```
--0〜100 くらいまでは標準で用意しておいても良いかもしれないけど
--それ以上は各々用意したってちょーだい
0 := /\a . \x^a . \f^(a -> a) . x           : Forall a . a -> (a -> a) -> a
1 := /\a . \x^a . \f^(a -> a) . f x         : Forall a . a -> (a -> a) -> a
2 := /\a . \x^a . \f^(a -> a) . f (f x)     : Forall a . a -> (a -> a) -> a
3 := /\a . \x^a . \f^(a -> a) . f (f (f x)) : Forall a . a -> (a -> a) -> a
...
```

その他
```
Id    := /\a . \x^a . x : Forall a . a -> a
Const := /\a, b . \x^a . \y^b . x : Forall a, b . a -> b -> a

--Constの定義は以下のように書いたものの略記方
--Const := /\a . /\b . \x^a . \y^b . x : Forall a . Forall b . a -> b -> a
```

モジュール
```
--上に並べたような基礎的な定義は標準モジュールとして提供する予定
--ただし標準モジュールも、以下のように明示的にインポートする必要がある。
#import Base

--理由は、学習用の言語と考えた時に、基礎的な関数をあえて定義したい場合が
--往々にしてあると考えられるため
```
