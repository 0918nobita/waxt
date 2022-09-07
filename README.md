# WAXT - WebAssembly eXtended Text Format

WAT (WebAssembly Text Format) を少し拡張して、人間にとって書きやすく malloc / free や GC の実装で役立つことを目指した中間レベルの言語です。

コンパイラを用いて WASM (バイナリ形式) に変換して、各種 WASM ランタイムで実行できます。

## 実装したい言語機能

### トップレベルでの関数定義

WAXT :

```wasm
(func add-and-store ([addr : i32] [x : i32] [y : i32])
    (i32.store addr (+ x y)))
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

### 定数

デフォルトですべての引数・束縛はイミュータブルであり、再代入はコンパイルエラーとなります。

```wasm
(func foo i32 ([x : i32] [y : i32])
    (let [x' (+ x 2) y' (+ y 3)]
        (* x' y')))
```

### 変数

`$` プレフィックス付きの引数・束縛はミュータブルとなり、 `set!` 特殊形式による再代入が許可されます。

```wasm
(func bar i32 ([$x : i32] [$y : i32])
    (set! $x (+ $x 2))
    (set! $y (+ $y 3))
    (* $x $y))
```

## 開発

[Task](https://taskfile.dev/) というタスクランナーを採用しています。

### 準備

ツール、依存パッケージのインストールを行います。

```bash
task prepare
```

### ビルド

```bash
task build
```

### テストの実行方法

```bash
task test
```

### カバレッジの計測

```bash
task coverage
```

### プロジェクトの依存関係

```mermaid
graph TB
  Token-->Location
  Lexer-->Token
  Parser-->Token
  Parser-->Ast
  Ast-->Token
  Ast-->Type
  Ir-->Location
  Ir-->Type
  TypeInferrer-->Ast
  TypeInferrer-->Ir
  CodeGen-->Ir
  CodeGen-->Wasm
  Compiler-->Lexer
  Compiler-->Parser
  Compiler-->TypeInferrer
  Compiler-->CodeGen
  Cli-->Compiler
```
