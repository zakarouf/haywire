<div align="center">
  <h1>Haywire</h1>
  A Small Language Implementation written in Modern C.
</div>

A small register based Programmable VM made to be fast and modular.

## Source Files Description
* `hw_compbc.c`: Haywire Byte Code Compiler.
* `hw_debug.c`: Debug Utilities (i.e. Decompiler, Stack Trace etc.)
* `hw_vm.c`: HayWire Virtual Machine.
* `hw_dev_...`: All Utility functions and modules for haywire.

# Roadmap
* [x] VM
  - [x] Instructions
  - [x] Type System
  - [x] Type Interface
      - [x] Primitives
      - [x] String
      - [x] List
      - [x] Array
      - [x] Table
* [x] Byte Code Compiler
    - [x] Interlope with VM
    - [x] Haywire ByteCode IR
* [ ] Module System
    - [x] Module Structure 
    - [ ] Import & Export
    - [ ] Runtime Linking
    - [ ] Compile Time Linking
    - [ ] Module Pack
* [ ] Haywire Language Compiler
* [ ] C Transpiler
* [ ] JIT

# Prospectus

## About
A small language implementation written in C.
* Very Small Binary Size (100K)
* Fast & Small VM
* Static Typed
* Modular Code design, can be easily improved upon and mutted to comply the user need
* Multi-purpose language; Can be used as a -> Data File Loader, Configure File or A Programming Language
* Built-in bytecode compiler is a VM itself which can evaluate code as it compiles.
* No Macros, instead usage of compile-time called functions to generate code
* Exhaustive Type system which can be easily used to implement user defined types.

## Installation

```
make -j
```
This will result in the `./bin/hw` binary executable created.
Optionally you can use the following command below to build a debug executable.
```
make debug -j 
```
> Haywire Only depends on the C Standard Library & CRT. Removing/Replacing them
> is a low priority goal.

## Byte-code, IR & VM
The Virtual Machine is fully implemented and ready. Along side the bytecode compiler. You can write a human readable of IR for the VM. 
```asm
; file: hello_world.hws
@const MESSAGE "Hello, World!\n"
@defn main()
    @defvar msg:string
    loadknst msg #MESSAGE
    prnt msg
    @endfn

```
Save the file, run it.
```text 
$ hw hello_world.hws
Hello, World!
```
> See more in `./examples/hwasm`

## Language Prospectus
While not yet totally implemented `haywire` will have c-like syntax;

```python
# `//` or `#`` for writting comments
const std = @import("std")  // Importing modules
const error = std.error     // Importing specific symbols or sub-modules from modules.

export main                 // export symbols of the current module.

let main = fn(args: [string]) ?error {
    if(args.len() < 4) return error.any()
    let x = args[2].parseInt()
    let y = args[3].parseInt()
    std.println("Sum of $x any $y is $(x + y)")
}
```

Running it.
```sh
$ haywire ./test 40 23
Sum of 40 and 23 is 63
```


### Meta Programming
`@` symbol is used for invoking compiler functions.
Haywire code can be executed compile time using '!' prefixing a normal function or using `@call` compiler invoke
```rust
const std = @import("std")  // Importing modules
const error = std.error     // Importing specific symbols or sub-modules from modules.
export main

let makelist = fn(_from: int, _upto: int, _step: int) list {
    let x = []
    iter i in _from,_upto: _step {
        x.push(i)
    }

    return x
}

let main = fn(args: [str]) ?error {
    let odd_nums = !makelist(1, 10000, 2)
    let eve_nums = @call makelist(2, 10000, 2)
}
```

### Function Overloading
```sh
let max = fn(x, y)
    => (x: int, y: int) int {
        if x > y {
            return x
        }
        return y
    }
    => (x: [int]) int {
        let max_: x[0];
        for i in x[1:] {
            if max_ < i {
                max_ = i
            }
        }
        return max_
    }

let main = fn(args: [str]) ?error {
    # Using compile time assert
    @assert(!max(112, 225) == 225)
    @assert(!max([41]) == 41)
    @assert(!max([6, 2, 12, 89, 2]) == 89)
}
```

### Type System
For the Implementation of Haywire, I have decided on a Strong Dynamic Typing System. The compiler will always keep records of the variables; their types and values. This leads to optimized code generation even on a simple single pass compiler. 
For Instance
```sh
# A function that takes two `ints` and throws out an `int`
let add(a: int, b: int) int { return a + b }
# Compiler Keeps a record of variable x, y; their types (int 
# and int) and also tries to guess their values. Here it is
# possible for the compiler to assume the value of x will be
# 10 and y will be 19 until they are subject to change further down the raod.
let x = 10
let y = 19

# Compiler know the type of z is `int` seen through the
# function `add(a: int, b: int) int` return type but does not know the value
# of it, as the function is executed runtime.
let z = add(x, y)

# When a new variable is declared with the value of x which hasnt changed since its declaration.
# the value stored in `prev_x` is also assume to yield `10`, so the compiler again keeps a record.
let prev_x = x

# Here we re-define a variable. It is possible and completely valid in haywire to do so.
# For the compiler, its just the case of changing some attribues recorded for the
# vauriable named `x; such as its type is set as `list` (or `int_array` if its a smart compiler)
# instead of `int`
x = [1, 2, 3]
```

Haywire also has type unions or sum types built in.
> As haywire is a vm language, each variable already hold in their type info.
> Haywite facilitates a way to utilize it properly
`?` is used for defining a sum type and pipe `|` for declaring possible types
```sh
let x: ?int | float | str = 0.23
```

If there is no other types declared using `|` then the second type is assumed to be nil.
```sh
let x: ?int
let x: ?int | nil
# Are the same
```

You have already seen in the defination of `main` function
```sh
let main = fn(args: [str]): ?error {
    # Here writting `return nil` is optional as it assumed if the main does not invoke any errors
    # it should return nil
}
```

**Please Note**
* `anytype` is a sumtype of all the possible types in the VM
* `error` is a enum type, we will talk about it later

Types can be checked using `@typeis()` compiler invoke

### Types in Haywire
0. Type: Type type, Stores type information
1. Nil: Nil type, only value it stores is nil
2. Integer: Signed 64bit interger
3. Integer (Unsigned): Unsigned 64bit interger
4. Float: 64bit, double precision floating point number
5. String: ASCII string type
6. List: Collection of Iterative values, each element can have disjoint types
7. Array: Collection of Iterative values, all the element share the same type
8. Hashset: Hashset of (k,v) pairs
9. Enum: Symbol Collection of Literal values
10. Symbol Table: Symbol Collection of Variables
11. Function: Function Pointer
12. Module: Module Type
13. Thread: Thread Type
