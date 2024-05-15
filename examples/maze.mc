struct Rand {
  seed,
}

fn Rand new(seed) {
  Rand {
    seed,
  }
}

fn Rand range(self, max) {
  self.seed = (self.seed * 48271) % 2147483647;
  (self.seed / 16) % max
}

struct Maze {
  width,
  height,
  cells,
}

fn Maze new(width, height) {
  let cells = [];
  var i = 0;
  while i < width * height {
    cells.push(0);
    i += 1;
  };
  Maze {
    width,
    height,
    cells,
  }
}

fn Maze print(self) {
  var y = 0;
  while y < self.height {
    var x = 0;
    var line = "";
    while x < self.width {
      line += if self.cells[y * self.width + x] == 0 { " " } else { "#" };
      x += 1;
    };
    print(line);
    y += 1;
  };
}

fn Maze generate(self) {
  let rand = Rand.new(1);
  var x = 0;
  while x < self.width {
    var y = 0;
    while y < self.height {
      self.cells[y * self.width + x] = if x == 0 || y == 0 || x == self.width - 1 || y == self.height - 1 || rand.range(4) == 0 1 else 0;
      y += 1;
    };
    x += 1;
  };
}

fn main() {
  let maze = Maze.new(20, 10);
  maze.generate();
  maze.print();
}
