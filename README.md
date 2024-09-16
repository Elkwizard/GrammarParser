# Grammar Parser
A parser generator for my grammar file format (`.grammar`, though I'm sure it's been used before).

## Example

```js
symbol = /\W+/
word = /\w+/

Sentence = words:word* "."

root = Sentence;

```

## Overview

Grammars are defined as a series names with definitions. Definitions come in one of two types:
1. Tokens: These are represented with a regular expression. This describes the pieces of the input source code that will be ascribed to each token type. By prefixing the Regex with `literals`, the token type will only match tokens that both match the Regex and appear in your grammar as literal strings. 
2. Terms: These represent composite (and possibly recursive) combinations of tokens.

A grammar must define a `root` term, which will be used as the starting point for all parsing.

## Term Syntax
The syntax is somewhat reminiscent of regular expressions.

### Basics
There are two basic ways to match a token:
* Literal: Using either single or double quotes, a string literal will match any token that is exactly equal to it in content.
* Type: The name of any token type will match a token of that type.

The name of any term in the program can also be used to match that term.

### Labels
To define the structure of the parse tree produced, pieces of a term's definition can be labeled. The labeled portion will then be included as a property on a node of the parse tree. Labels are applied with `label:A`, where when `A` is matched, it will be stored into `node.label`.

### Operations & Quantifiers
The basic matches can be combined through a variety of different features:
* `A*` will match `A` 0 or more times
* `A+` will match `A` 1 or more times
* `A?` will match `A`, or nothing
* `A|B` matches `A` or `B`, prioritizing `A`
* `A[B]` will match as many `A` as possible, requiring a `B` between each one. (if `{B}` is used instead of `[B]`, this is required to match at least one `A`)
* `(...)` can be used to group symbols
* `?` after a quantifier (`?`, `+`, `*`, `{...}`, `[...]`) will make it match as few instances as possible

## Result
The compiler, `compileGrammar.js`, takes two arguments, a `.grammar` file path, and a `.js` file path. The `.js` path represents the destination for the compiled output.

The resulting file contains two global variables, `class AST` and `function parse(source)`.
`parse` takes in a string in the specified language and returns the root node of a parse tree representing it.
`AST` is the base class for all parse tree nodes, and contains a static subclass for each term defined in the grammar, e.g. The class `AST.Expression` would represent the term `Expression` in the parse tree.
