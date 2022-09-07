(module
  (func $fact (export "fact") (param $n i32) (result i32)
    (if (result i32) (i32.eqz (local.get $n))
      (then i32.const 1)
      (else
        (i32.mul
          (local.get $n)
          (call $fact (i32.sub (local.get $n) (i32.const 1))))))))
