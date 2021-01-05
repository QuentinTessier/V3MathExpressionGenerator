# V3MathExpressionGenerator
Generate the C code required to perform a math expression in V3Engine
# V3MathExpressionGenerator

This project is part of the V3 Engine developpement.

This is a transpiler from a custom language dedicated to vectorial maths to C.
We will use this tool to generate our math library for the engine and all math related code with vector operation.

Our first step will be to transpile simple maths expression to C using macros :
```c

/**
 * First iteration:
 * No variable, simple numbers
 * 1 + 1
 */
#define V3M_TEST_MACRO() (1 + 1)

/**
 * Second iteration:
 * Simple numbers and variable
 * 1 + a
 */
#define V3M_TEST_MACRO(a) (1 + a)

/**
 * Third iteration:
 * Define basics types (e.g : float, int, unsigned..)
 */

```

This is the current grammar, we are still in developpement, it will most likely change:

```
# Type definition with constructor and operator overload
define tuple<x : f32, y : f32> as vec2f where
    constructor(x : f32, y : f32) -> {x, y}
    # Binary operator
    operator{+}(a : vec2f, b : vec2f) -> vec2f(a.x + b.x, a.y + b.y)
    # Unary operator
    operator{-}(a : vec2f) -> vec2f(-a.x, -b.y)

# Global variable definition

define PI : f32 -> 3.14...

# "Function" definition (every function is pure and can only produce a return value, can't modify arguments)
formula myFormula(x : f32) -> x

# Possible addon to the language
# the 'any' type could be added to import non pure math types from C, for instance:
# struct SomeECSComponent {
#   vec2f SomeVector;
#   void *SomePointerToSomeNonMathData;
# };
#
# tuple<SomeVector : vec2f, any SomePointerToSomeNonMathData> as SomeECSComponent where
#   ...

```
