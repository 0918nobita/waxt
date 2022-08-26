# WAXT における型推論について

型環境と式から型の連立方程式を求める手続き Extract を以下のように定義します。

## 型の方程式の抽出

```text
Extract(Γ, i) = (∅, i32)

Extract(Γ, x) = (∅, Γ(x))

Extract(Γ, (i32.eqz e)) =
    let (E0, τ) = Extract(Γ, e) in
    let E = E0 ∪ { τ = i32 } in
    (E, i32)

Extract(Γ, (i32.add e1 e2) | (i32.sub e1 e2) | (i32.mul e1 e2) | (i32.store e1 e2)) =
    let (E1, τ1) = Extract(Γ, e1) in
    let (E2, τ2) = Extract(Γ, e2) in
    let E3 = E1 ∪ E2 ∪ { τ1 = i32, τ2 = i32 } in
    (E3, i32)

Extract(Γ, (let [x : τ0 e1] e2)) =
    let (E1, τ1) = Extract(Γ, e1) in
    let (E2, τ2) = Extract((Γ, x:τ0), e2) in
    let E3 = E1 ∪ E2 ∪ { τ1 = τ0 } in
    (E3, τ2)

Extract(Γ, (let [x e1] e2)) =
    let (E1, τ1) = Extract(Γ, e1) in
    let (E2, τ2) = Extract((Γ, x:τ1), e2) in
    let E3 = E1 ∪ E2 in
    (E3, τ2)

Extract(Γ, (f e1 ... en)) =
    if let ((τ11, ..., τ1n) => τ0) = Γ(f)
    then
        let (E1, τ21) = Extract(Γ, e1) in
        ...
        let (En, τ2n) = Extract(Γ, en) in
        let E = E1 ∪ ... ∪ En ∪ { τ11 = τ21, ..., τ1n = τ2n } in
        (E, τ0)
    else error

Extract(Γ, (if e1 e2 e3)) =
    let (E1, τ1) = Extract(Γ, e1) in
    let (E2, τ2) = Extract(Γ, e2) in
    let (E3, τ3) = Extract(Γ, e3) in
    let E = E1 ∪ E2 ∪ E3 ∪ { τ2 = τ3 } in
    (E, τ2)
```

## 単一化

↑で得られた型の連立方程式の解を求めるための、一階の単一化アルゴリズム Unify は以下のように表されます。

```text
Unify(∅) = []

Unify(E ⨄ { τ = τ }) = Unify(E)

Unify(E ⨄ { α = τ } | E ⨄ { τ = α }) (when τ ≠ α) =
    if α ∈ FTV(τ)  // τ 中に自由な型変数 α が含まれている場合にはエラー
    then error
    else
        let S = Unify([τ/α]E) in
        S ○ [τ/α]

Unify(E ⨄ { (τ11, ..., τ1n) => τ10 = (τ21, ..., τ2n) => τ20 }) =
    Unify(E ⨄ { τ10 = τ20, τ11 = τ21, ..., τ1n = τ2n })

Unify(E ⨄ { τ1 = τ2 }) = error
```

## 型検査で成功する例

以下のような関数定義を型検査することを考えます。

```wasm
(func foo (x y)
    (i32.mul (i32.add x 3) y))
```

まず型環境をつくり、これを `Γ` とします。

| 名前 | 型 |
| ---- | ---- |
| `x` | `'t1` |
| `y` | `'t2` |
| `foo` | `('t1, 't2) => 't0` |

次に、型環境 `Γ` , 式 `(i32.mul (i32.add x 3) y)` を `Extract` に渡して、型の連立方程式をつくります。

```text
Extract(Γ, (i32.mul (i32.add x 3) y))
    Extract(Γ, (i32.add x 3))
        Extract(Γ, x)
        <- (∅, 't1)
        Extract(Γ, 3)
        <- (∅, i32)
    <- ({ 't0 = i32, i32 = i32 }, i32)
    Extract(Γ, y)
    <- (∅, 't2)
<- ({ 't1 = i32, i32 = i32, 't2 = i32 }, i32)
```

式全体の型は `i32` なので、連立方程式に `'t0 = i32` を加えます。

続けて、単一化を行います。

```text
Unify({ 't1 = i32, i32 = i32, 't2 = i32 } ⨄ { 't0 = i32 })
    Unify({ 't1 = i32, i32 = i32 } ⨄ { 't2 = i32 })
        Unify({ 't1 = i32 } ⨄ { i32 = i32 })
            Unify({ 't1 = i32 })
                Unify(∅)
                <- []
            <- [i32/'t1]
        <- [i32/'t1]
    <- [i32/'t1, i32/'t2]
<- [i32/'t1, i32/'t2, i32/'t0]
```

結果：

| 名前 | 型 |
| ---- | ---- |
| `x` | `i32` |
| `y` | `i32` |
| `foo` | `(i32, i32) => i32` |

## 型検査で失敗する例

```wasm
(func bar (x [y : i64])
    (i32.add x y))
```

型環境 `Γ`：

| 名前 | 型 |
| ---- | ---- |
| `x` | `'t1` |
| `y` | `i64` |
| `bar` | `('t1, i64) => 't0` |

型の方程式の抽出：

```text
Extract(Γ, (i32.add x y))
    Extract(Γ, x)
    <- (∅, 't1)
    Extract(Γ, y)
    <- (∅, i64)
<- ({ 't1 = i32, i64 = i32 }, i32)
```

式全体の型は `i32` なので、型の連立方程式に `'t0 = i32` を加えます。

単一化：

```text
Unify({ 't1 = i32, i64 = i32 } ⨄ { 't0 = i32 })
    Unify({ 't1 = i32 } ⨄ { i64 = i32 })
    <- error
```

## 再帰的な関数を型検査する例

```wasm
(func fact (n)
    (if (i32.eqz n)
        1
        (i32.mul n (fact (i32.sub n 1)))))
```

型環境 `Γ`：

| 名前 | 型 |
| ---- | ---- |
| `n` | `'t1` |
| `fact` | `('t1) => 't0` |

型の方程式の抽出：

```text
Extract(Γ, (if (i32.eqz n) 1 (i32.mul n (fact i32.sub n 1))))
    Extract(Γ, (i32.eqz n))
        Extract(Γ, n)
        <- (∅, 't1)
    <- ({ 't1 = i32 }, i32)
    Extract(Γ, 1)
    <- (∅, i32)
    Extract(Γ, (i32.mul n (fact (i32.sub n 1))))
        Extract(Γ, n)
        <- (∅, 't1)
        Extract(Γ, (fact (i32.sub n 1)))
            Extract(Γ, (i32.sub n 1))
                Extract(Γ, n)
                <- (∅, 't1)
                Extract(Γ, 1)
                <- (∅, i32)
            <- ({ 't1 = i32, i32 = i32 }, i32)
        <- ({ 't1 = i32, i32 = i32 }, i32)
    <- ({ 't1 = i32, i32 = i32 }, i32)
<- ({ 't1 = i32, i32 = i32 }, i32)
```

式全体の型は `i32` なので、型の連立方程式に `'t0 = i32` を加えます。

単一化：

```text
Unify({ 't1 = i32, i32 = i32 } ⨄ { 't0 = i32 })
    Unify({ 't1 = i32 } ⨄ { i32 = i32 })
        Unify({ 't1 = i32 })
            Unify(∅)
            <- []
        <- [i32/'t1]
    <- [i32/'t1]
<- [i32/'t1, i32/'t0]
```

結果：

| 名前 | 型 |
| ---- | ---- |
| `n` | `i32` |
| `fact` | `(i32) => i32` |
