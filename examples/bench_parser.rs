use mucin::{parser, string::compact_str_pool};

fn main() {
    let t = std::time::Instant::now();

    for _ in 0..100 {
        parser::parse(SRC).unwrap();
        compact_str_pool();
    }

    println!("{:?}", t.elapsed());
}

const SRC: &str = r#"
fn main(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z) f(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z)
let a={1+2*3/4-5==6&&7||1+2*3/4-5==6&&7||1+2*3/4-5==6&&7||1+2*3/4-5==6&&7||1+2*3/4-5==6&&7};
fn f() a.b.c.d.e.f.g.h.i.j.k.l.m.n()[a.b.c.d.e.f.g.h.i.j.k.l.m.n[0]]([a.b.c.d.e.f.g.h.i.j.k.l.m.n[0]])[a.b.c.d.e.f.g.h.i.j.k.l.m.n[0]]
fn g()while x loop match x {() => if x while x while x loop match x {() => if x while x {} else loop {}} else loop {}}
fn h(){a:{{a:{{a:{{a:{{a:{{a:{{a:{{a:{}}}}}}}}}}}}}}}}
"#;
