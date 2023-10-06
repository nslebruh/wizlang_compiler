# WizLang
Silly language to build a lexer, parser and compiler for

## Spec
### Basics
Wizlang is a full stop delimited language. For example, 
```js
let x = "Hello world!".
```
is the same as
```js
let x = "Hello world!";
```
in other languages


Everything is as verbose as possible. For example,
```js
let x = 1 + 2 * 3 / 4;
if x >= 2 {
  console.log(x, " is 2 or greater");
} else {
  console.log(x, " is less than 2");
}
```
would be 
```
Imbue x as 1 plus 2 times 3 over 4.
Should x be greater than or equal to 2, then
  Invoke console's log, which takes x and " is 2 or greater".
Otherwise, 
  Invoke console's log, which takes x and " is less than 2".
Conclude.
```
in Wizlang
### Variables
You can declare variables with the `imbue` keyword  
```
Imbue x with 1.
Imbue y as "y".
```