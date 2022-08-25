# WAXT における型推論について

型環境と式から型の連立方程式を求める手続き Extract を以下のように定義します。

```text
Extract(Γ, i) = (∅, i32)

Extract(Γ, x) = (∅, Γ(x))

Extract(Γ, (i32.add e1 e2) | (i32.mul e1 e2) | (i32.store e1 e2)) =
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

Extract(Γ, (e0 e1 ... en)) =
    let (E0, τ0) = Extract(Γ, e0) in
    let (E1, τ1) = Extract(Γ, e1) in
    ...
    let (En, τn) = Extract(Γ, en) in
    let α be a fresh type variable in
    let E = E0 ∪ E1 ∪ ... ∪ En ∪ { τ0 = (τ1, ..., τn) => α } in
    (E, α)
```

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

以下のような関数定義で型推論を行うことを考えます。

```wasm
(func foo i32 (x y)
    (i32.mul (i32.add x 3) y))
```

`x`, `y` の型注釈は省略されているので、とりあえず `x` の型を `'t0`、`y` の型を `'t1` とし、これらを含めた型環境 `Γ = { x: 't0, y: 't1 }` を用意します。
次に、型環境 `Γ` , 式 `(i32.mul (i32.add x 3) y)` を `Extract` に渡して、型の連立方程式をつくります。

```text
Extract(Γ, (i32.mul (i32.add x 3) y))
  Extract(Γ, (i32.add x 3))
    Extract(Γ, x)
    <- (∅, 't0)
    Extract(Γ, 3)
    <- (∅, i32)
  <- ({ 't0 = i32, i32 = i32 }, i32)
  Extract(Γ, y)
  <- (∅, 't1)
<- ({ 't0 = i32, i32 = i32, 't1 = i32 }, i32)
```

続けて、単一化を行います。

```text
Unify({ 't0 = i32, i32 = i32 } ⨄ { 't1 = i32 })
  Unify({ 't0 = i32 } ⨄ { i32 = i32 })
    Unify({ 't0 = i32 })
      Unify(∅)
      <- []
    <- [i32/'t0]
  <- [i32/'t0]
<- [i32/'t0, i32/'t1]
```

`'t0` は `i32` に、`'t1` は `i32` に推論されました。
