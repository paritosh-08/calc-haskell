# calc-haskell
An awesome (?) calculator in Haskell.

## What can I do with this?
You can do a lot of things with this piece of code:
* Show someone the worst way to parse an expression.
* Delete the code and save some space for gifs like [this](https://i.ibb.co/XshDfjS/awesome.gif "Awesome GIF").
* Do some simple calculations (if you have a lot of time).

## I want to do some calculations
If you really want to do calculations then please follow the following:

First, install and run the `cabal` project:
``` sh
git clone https://github.com/paritosh-08/calc-haskell.git
cd calc-haskell
cabal new-run
```

Now enter the expression that needs to be evaluated.

### Expression Docs
The expression is of three types:
* **Number**: This is an INTEGER (only as of now) and you need to directly enter the number, eg `5` is an expression but `5.0` will give an error
* **Function**: There are two functions as of now:
    * **sqrt**: This takes an `expression` and returns the square root of the evaluted `expression`. i.e. I can write `sqrt(4)` and this will be evauated to `2`.
    * **ifzero**: This function has the syntax: `ifzero(<expression1> ? <expression2> : <expression3>)`. `<expression1>` is the condition that will be evaluated and checked if it is zero or not. If `<expression1>` turns out to be zero then, the function will return `<expression2>`, else it will return `<expression3>`.
* **Binary Operations**: This will always be enclosed within brackets, i.e the syntax is `(<expression1> <OPERATOR> <expression2>)`. Here `<OPERATOR>` can be anything from the list: [`+`,`-`,`/`,`*`,`^`]. The meaning of each operator is defined below:
    | Operator | Meaning                                        | Example | Result |
    |:--------:|------------------------------------------------|:-------:|:------:|
    | +        | Adds two entities                              | 2+2     | 4      |
    | -        | Subtracts second entity from the first one     | 3-2     | 1      |
    | *        | Multiplies two entities                        | 3*2     | 6      |
    | /        | Divides first entity by the second one         | 6/3     | 2      |
    | ^        | Raises first entity by the power of second one | 2^3     | 8      |

### Examples of expressions
| Expression    | Result    | Explanation (if any)                                          |
|---------------|:---------:|---------------------------------------------------------------|
| 45            | 45        | Integers can act as an expression                             |
| (2+5)         | 7         | This is an example of *Binary Operation*                      |
| ((3+5)/4)     | 2         | Binary operation where first argument is a *Binary operation* |
| ifzero(0?5:6) | 5         | An example of *Function Expression*                           |
| sqrt(16)      | 4         | Another example of *Function Expression*                      |

### Errors in expression evaluation
Sometimes the programme will return `Something went wrong`. The error handling will be done in future, but for now, you can debug by the following steps:
* First run `cabal repl`.
* Now write `parseTest parseExpression <EXPRESSION>` and hit enter (replace `<EXPRESSION>` by the expression which caused some error).
* Hopefully, you will get some human readable error.
* To exit, just write `:q` and hit enter.

## Things to keep in Mind
> Please use integers only in the expression.

> Always use brackets to envelope the operator expression, `2+2` will give error for sure. Instead write `(2+2)`.
