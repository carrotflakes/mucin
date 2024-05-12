#[test]
fn test() {
    let srcs = [
        r#"
fn fib(n) {
    if 2 > n {
        1
    } else {
        fib(n - 1) + fib(n - 2)
    }
}

fn main() {
    [fib(1), fib(2), fib(3), fib(4), fib(5), fib(6)]
}
"#,
        r#"
var count = 0;

fn tarai(x, y, z) {
    count = count + 1;
    if x <= y {
        y
    } else {
        tarai(tarai(x - 1, y, z), tarai(y - 1, z, x), tarai(z - 1, x, y))
    }
}

fn main() {
    tarai(10, 5, 0);
    count
}
"#,
        r#"
fn counter() {
    var count = 0;
    [
        fn () {
            count = count + 1;
        },
        fn () {
            count
        }
    ]
}

fn main() {
    let c = counter();
    c[0]();
    c[0]();
    c[1]()
}
"#,
        r#"
fn user(name, age) {
    {
        getName: || name,
        getAge: || age,
        setName: |newName| {
            name = newName;
        },
        setAge: |newAge| {
            age = newAge;
        }
    }
}

fn main() {
    let c = user("niko", 8);
    [
        c.getName(),
        c.getAge(),
        c.setName("niko!"),
        c.setAge(9),
        c.getName(),
        c.getAge()
    ]
}
"#,
        r#"
fn main() {
    let a = newVec();
    vecPush(a, 1);
    vecPush(a, 2);
    vecGet(a, 0) + vecGet(a, 1)
}
"#,
        r#"
fn main() {
    let a = {name: "niko", age: 8};
    a["name"] = "niko!";
    a.age = 9;
    a
}
"#,
        r#"
fn main() {
    [false && 1, true && 2, false || 3, true || 4]
}
"#,
        r#"
fn main() {
    var a = 0;
    var i = 0;
    loop {
        a = a + i;
        i = i + 1;
        if i == 4 {
            break;
        };
    };
    a
}"#,
        r#"
fn f(a):
    if a < 2:
        if a < 1:
            0
        else:
            1
    else:if a < 3:
        2
    else:
        3

fn main():
    [f(0), f(1), f(2), f(3)]"#,
        r#"
fn main() {
    let a = newVec();
    let x = 0;
    vecPush(a, x);
    let x = "x";
    vecPush(a, x);
    let f = || x;
    let x = 1;
    vecPush(a, f());
    a
}
    "#,
        r#"
fn main() {
    var a = 0;
    var i = 0;
    while i < 4 {
        a = a + i;
        i = i + 1;
    };
    a
}
    "#,
        r#"
fn main() {
    let vs = [1, 2, [1, 2, 3], {a: "a", b: "b"}, {,}];
    let res = [];
    var i = 0;
    while i < vecLen(vs) {
        vecPush(res, match vs[i] {
            1 => "1",
            x:"int" => x,
            [1, 2, x] => x,
            {a: "a", b: b} => b,
            _ => "!!!",
        });
        i = i + 1;
    };
    res
}
"#,
        r#"
fn main() {
    var i = 0;
    'outer: while i < 4 {
        var j = 0;
        while j < 4 {
            if i == 2 && j == 2 {
                break 'outer [i, j];
            };
            j = j + 1;
        };
        i = i + 1;
    }
}
"#,
        r#"
macro! s {
    (+ $a $b) => ($a + $b)
    (list $($args)*) => [$(s!$args ,)]
    ($f $($args)*) => {$f ($(s!$args ,))}
    $x => $x
}

fn main(): s!(print (list (+ 1 2) "hello"))
  "#,
        r#"
fn main(): {
    let a = 0;
    let b = 1 && null || {
        let a = 1;
        || a
    };
    [a, b()]
}
"#,
        r#"
struct User {
    name,
    age,
}

fn User incAge(self) {
    self.age = self.age + 1;
}

fn main(): {
    let u = User {name: "niko", age: 8};
    u.name = "niko!";
    // structTypeMethods(User).incAge(u);
    u.incAge();
    match u {
        User {name: name, age: age} => [name, age]
    }
}
"#,
        r#"
fn main() {
    let a = [1, 2];
    [a[0], a[1], a[2]]
}"#,
        r#"
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

fn main() {
    let p1 = Point { x: 10, y: 20 };
    let p2 = Point.new(30, 40);
    let p3 = p1.add(p2);
    p3
}
"#,
        r#"
fn main(): 1 + 2 - 3 * 4 % 5
"#,     r#"
fn main() {
    var a = 1;
    a -= 2;
    var b = {x: 1};
    b.x -= 2;
    [a, b]
}
"#,
r#"
fn main() {
    let a = [1, 2, 3];
    a.push(4);
    a.pop();
    a.insert(2, 5);
    a.remove(0);
    a
}
"#,
r#"
// Non-strict arity check
fn main() {
    [[1, 2, 3].map(|x| x + 1), [1, 2, 3].map(|x, i, y| i)]
}
"#,
r#"
fn main(): [1, ..[2, 3], 4, ..[5, 6]]
"#,
r#"
fn main(): f(1, ..[2, 3], 4, ..[5, 6])
fn f(a, b, c, d, e, f): [a, b, c, d, e, f]
"#,
r#"
let a = [];
fn main() {f(); a}
fn f() {
    defer a.push(6);
    let x = 5;
    defer a.push(x);
    a.push(1);
    let x = 3;
    {
        defer a.push(x);
        a.push(2);
    };
    a.push(4);
}
"#,
r#"
fn main() {
    var i = 0;
    let a = [];
    while i < 4 {
        defer {
            i += 1;
        };
        a.push(i);
    };
    a
}
"#,
// r#"
// macro! foo {
//     ($x) => {
//         let x = $x;
//         x
//     }
// }

// fn main(): foo!(1)
// "#,
    ];
    for (i, src) in srcs.iter().enumerate() {
        println!("test {}", i);

        let mut runtime = crate::runtime::Runtime::new();
        runtime.push_env_from_src(src).unwrap();
        runtime
            .call_fn(
                "main",
                |_| vec![],
                |_, value| {
                    gilder::assert_golden!(format!("{:?}", value));
                },
            )
            .unwrap();
    }
}
