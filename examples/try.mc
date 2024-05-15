let contStack = [];

fn pushCatch(cont) {
  contStack.push(cont);
}

fn popCatch() {
  contStack.pop()
}

fn throw(e) {
  if (contStack.len == 0) {
    panic("Can't use throw outside of a try form");
  } else {
    contStack[contStack.len - 1](e);
  }
}

macro! try {
  { $body catch $exception $catchBody } => {
    calljp(|$escape| {
      pushCatch(|$exception| {
        $catchBody;
        jump($escape);
      });

      $body;

      // $escape();
    });
    popCatch();
  }
}

fn main() {
  try! {
    {
      print(1);
      throw("An error occured");
    }
    catch e {
      print("Caught exception: " + toString(e));
    }
  };
  try! {
    {
      print(2);
    }
    catch e {
      print("Caught exception: " + toString(e));
    }
  };
}
