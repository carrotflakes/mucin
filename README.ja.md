# Mucin

Mucin は、シンプルで使いやすいスクリプト言語です。Rust のような構文と、JavaScript のようなデータ型を持っています。Rust のライブラリとして使用することができます。

## 構文

### 変数

変数は `let` キーワードを使って宣言します。

```rust
let x = 10;
let y = 20;
let z = x + y;
print(z);
```

### 関数

関数は `fn` キーワードを使って宣言します。

```rust
fn add(x, y) {
  return x + y;
}
```

ブロックの最後で値を返す場合は、`return` キーワードを省略することができます。

```rust
fn add(x, y) {
  x + y
}
```

コロンでブロックを省略することもできます。

```rust
fn add(x, y): x + y
```

### 関数式 (クロージャ)

```rust
let add = fn(x, y) {
  x + y
};

let add = fn(x, y): x + y;

let add = |x, y| x + y;
```

### Dict

Dict は JavaScript のオブジェクトのようなものです。

```rust
let obj = {
  x: 10,
  y: 20
};

print(obj.x);
print(obj["y"]);
```

### 配列 (Vec)

```rust
let vec = [1, 2, 3, 4, 5];

print(vec[0]);

print(vec.len);
```

### 構造体

```rust
struct Point {
  x,
  y,
}

let p = Point { x: 10, y: 20 };

print(p.x);
```

### if 式

```rust
if x == 10 {
  print("x is 10");
} else if x == 20 {
  print("x is 20");
} else {
  print("x is not 10 or 20");
};
```

式なので値を返すことができます。

```rust
let x = if y == 10 {
  20
} else {
  30
};
```

`:` でカッコを省略することもできます。

```rust
let x = if y == 10: 20 else: 30;
```

### loop 式

```rust
let x = loop {
  if y == 10 {
    break 20;
  }
};
```

### while 式

```rust
var x = 0;
while x < 10 {
  x += 1;
};
```

### match 式

```rust
let x = match y {
  1 => 2,
  "hello" => "world",
  [a, b] => a + b,
  { x, y } => x + y,
  _ => {
    print("default");
  },
};
```

### defer 文

ブロックを抜けるときに必ず実行されるコードを書くことができます。

```rust
fn main() {
  defer {
    print("defer");
  };

  print("main");
}
```

### マクロ

Rust の `macro_rules!` に似たマクロを使うことができます。

```rust
macro! s {
  (+ $a $b) => ($a + $b)
  (list $($args)*) => [$(s!$args ,)]
  ($f $($args)*) => {$f ($(s!$args ,))}
  $x => $x
}

fn main(): s!(print (list (+ 1 2) "hello"))
```

### コメント

```rust
// 1 行コメント
/*
複数行コメント
*/
```

### メソッド

```rust
struct Point {
  x,
  y,
}

fn Point new(x, y) {
    Point { x, y }
  }

fn Point add(self, other) {
  Point { x: self.x + other.x, y: self.y + other.y }
}

let p1 = Point {
  x: 10,
  y: 20,
};
let p2 = Point.new(30, 40);
let p3 = p1.add(p2);
```
