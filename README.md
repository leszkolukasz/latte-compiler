# Latte Compiler

Latte is an imperative language, almost a subset of Java. This compiler was created as part of the course "Compiler Construction" at MIMUW.

Full description of the language can be found at: [Latte Language Description](https://www.mimuw.edu.pl/~ben/Zajecia/Mrj2024/Latte/description.html)

## General

The compiler compiles Latte code to x86-64 assembly (AT&T syntax). It consists of three parts:

- **Transpiler** (written in Haskell) - parses Latte code and transpiles it to an intermediate representation (IR)
- **Frontend** (written in Rust) - checks for the correctness of the code, performs simple optimizations like expression evaluation
- **Backend** (written in Rust) - translates IR to x86-64 assembly

## Language introduction

### Basic Program Structure
A Latte program consists of function definitions. The main function must be defined as:
```java
int main() { ... }
```
All functions must return values of their specified types, except `void` functions, which can omit return statements.

### Types
- **Primitive Types**: `int`, `string`, `boolean` (alias: `char`), `void`
- **Object Types**: Classes, with support for inheritance and basic polymorphism
- **Arrays**: Similar to Java arrays, created using the `new` keyword
- **Special Features**:
  - `cast<Type>(Expr)` for casting class types to their supertypes
  - Strings support indexing, returning a `char` (alias of `boolean`)

### Predefined Functions
- `void printInt(int)`
- `void printString(string)`
- `void printChar(char)`
- `int readInt()`
- `string readString()`
- `void error()` (prints runtime errors and halts)

### Example Snippets

#### Hello World
```java
int main() {
  printString("hello world");
  return 0;
}
```

#### Print Even Numbers Below 10
```java
int main() {
  int i = 0;
  while (i < 10) {
    if (i % 2 == 0) printInt(i);
    i++;
  }
  return 0;
}
```

#### Factorial (Iterative and Recursive)
```java
int fact(int n) {
  int i = 1, r = 1;
  while (i <= n) {
    r *= i;
    i++;
  }
  return r;
}

int factr(int n) {
  if (n < 2) return 1;
  return n * factr(n - 1);
}

int main() {
  printInt(fact(7));
  printInt(factr(7));
  return 0;
}
```

### Object-Oriented Features

#### Classes and Objects
```java
class Counter {
  int val;

  void incr() { val++; }
  int value() { return val; }
}

int main() {
  Counter c = new Counter;
  c.incr();
  c.incr();
  printInt(c.value());
  return 0;
}
```

#### Inheritance and Polymorphism
```java
class Point2 {
  int x, y;

  void move(int dx, int dy) {
    x += dx;
    y += dy;
  }
}

class Point3 extends Point2 {
  int z;
  void moveZ(int dz) { z += dz; }
}

int main() {
  Point3 p = new Point3;
  p.move(3, 4);
  p.moveZ(5);
  printInt(p.x);
  printInt(p.y);
  printInt(p.z);
  return 0;
}
```

#### Virtual Methods
```java
class Shape {
  void tell() { printString("I'm a shape"); }
}

class Rectangle extends Shape {
  void tell() { printString("I'm a rectangle"); }
}

int main() {
  Shape s = new Rectangle;
  s.tell();  // Outputs: I'm a rectangle
  return 0;
}
```

### Arrays
Arrays are heap-allocated and have a predefined `length` property.

```java
int[] sum(int[] a, int[] b) {
  int[] res = new int[a.length];
  for (int i = 0; i < a.length; i++) {
    res[i] = a[i] + b[i];
  }
  return res;
}

int main() {
  int[] a = new int[3];
  int[] b = new int[3];
  a[0] = 1; b[0] = 2;
  a[1] = 3; b[1] = 4;
  a[2] = 5; b[2] = 6;

  int[] c = sum(a, b);
  for (int x : c) printInt(x);
  return 0;
}
```

## Building

### Requirements
- **Haskell** (GHC)
- **Rust** (Cargo)

Projects contains Makefile with following targets:

- `make` - builds the compiler
- `make test` - runs tests
- `make clean` - cleans the project

## Running

After building the compiler, you can run it using the following command:
```bash
./latc_x86_64 <input_file>
```

This will generate both assembly and executable file.