# WAXT - WebAssembly eXtended Text Format

WAT (WebAssembly Text Format) を少し拡張して、人間にとって書きやすく malloc / free や GC の実装で役立つことを目指した中間レベルの言語です。

WAT と同じく S 式で記述します。

WAT に変換するコンパイラを実装する予定です。

## 関数定義

WAXT :

```clojure
(func add-and-store [addr *i32 x i32 y i32]
    (store addr (+ x y)))
```

WAT (コンパイル後) :

```wasm
(module
    (memory 1)
    (func $add-and-store (export "add-and-store")
        (param $addr i32) (param $x i32) (param $y i32)
        (i32.store
            (local.get $addr)
            (i32.add (local.get $x) (local.get $y)))))
```

## 実装状況

- [x] レキサ
- [x] S式パーサ
- [ ] プリミティブ型 ( `i32` , `i64`, `f32` , `f64` )
- [ ] ポインタ型 ( `*i32` 等)
- [ ] リテラル
- [ ] 基本的な関数定義
- [ ] WAT の各命令と対応する組み込み関数 ( `i32.add` , `i32.store` 等)
  - [ ] 実引数の型に応じてコンパイル時に命令を決定するジェネリックな演算 / メモリ操作関数
- [ ] `let` 束縛
- [ ] `if` 特殊形式
- [ ] `cond` 特殊形式
- [ ] `while` 特殊形式
- [ ] スレッディングマクロ
