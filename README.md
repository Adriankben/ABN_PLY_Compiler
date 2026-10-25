# ABN_PLY_Compiler
The syntax for this programming language allows source code that assigns values to variables, computes simple arithmetic expressions, performs conditional branching or conditional execution of statements depending on the value of variables, and displays the value of variables and strings.

ABN is a small procedural language implemented in Python with [PLY](https://github.com/dabeaz/ply) (Python Lex-Yacc). `compiler.py` contains the lexer, the grammar, and an evaluator, and runs as an interactive `abn>>>` prompt that reads and executes one statement per line.

## Language at a glance

| Feature | Syntax | Example |
|---|---|---|
| Comment | `# text` | `# my first program` |
| Numbers | integers and floats, optional sign | `42`, `-7`, `3.14`, `.5` |
| Strings | double-quoted | `"Hello"` |
| Assignment | `name = expr` or `name = "string"` | `x = 10` |
| Arithmetic | `+  -  *  /` and parentheses; `*` and `/` bind tighter than `+` and `-` | `(x + 2) * 3` |
| Comparison | `==  >  >=  <  <=` | `y > 20` |
| Display a variable | `"label" name` | `"y is" y` |
| If | `if (condition) then statement` | `if (x == 10) then z = x / 4` |
| If / else | `if (condition) then { statement } else { statement }` | `if (y > 20) then { "big" y } else { "small" y }` |
| Exit | `leave` | `leave` |

Each branch of an `if` can hold an expression, an assignment, or a display statement. Keywords are `if`, `then`, `else`, and `leave`.

## Example session

```
abn>>> x = 10
abn>>> y = x * 2 + 5
abn>>> y
25
abn>>> "y is" y
"y is" 25
abn>>> if (y > 20) then { "big" y } else { "small" y }
"big" 25
abn>>> if (x == 10) then z = x / 4
abn>>> "z is" z
"z is" 2.5
abn>>> w
Undeclared variable
abn>>> leave
Thanks for using my program! Goodbye!
```

Statements that produce no value (assignments, displays) also echo `None` after running.

## Error handling

- Syntax errors report the offending token, for example `Syntax error in input at '...'`.
- Using a variable before assigning it prints `Undeclared variable`.
- Illegal characters are reported and skipped.
- Errors play a short beep through Windows `winsound`.

## Running it

Requirements: Python 3 on **Windows** (the error beep uses the Windows-only `winsound` module).

PLY isn't distributed through pip. As its README describes, copy the `ply` folder from [dabeaz/ply](https://github.com/dabeaz/ply) next to `compiler.py`, then run:

```
python compiler.py
```

## Grammar

The full BNF grammar and design write-up are in [`docs/APL Project.docx`](docs/APL%20Project.docx). In summary:

```
<abn>        ::= <comment> | <statement> | <condition> | <expr> | <var_assign> | <display> | 'leave'
<statement>  ::= 'if' '(' <condition> ')' 'then' '{' <branch> '}' 'else' '{' <branch> '}'
               | 'if' '(' <condition> ')' 'then' <branch>
<branch>     ::= <expr> | <var_assign> | <display>
<condition>  ::= <expr> ( '==' | '>' | '>=' | '<' | '<=' ) <expr>
<expr>       ::= <expr> ( '+' | '-' | '*' | '/' ) <expr> | '(' <expr> ')' | <id> | <int> | <float>
<var_assign> ::= <id> '=' <expr> | <id> '=' <string>
<display>    ::= <string> <id>
```
