# Wir — word IR

A textual surface for the `Ast.*` nodes in
[`share/aquarius/lib/aqua/ast`](../../lib/aqua/ast). Each grammar production in
[`wir.ebnf`](wir.ebnf) maps 1:1 to one `Ast` node class; each field of a
production is one argument to that node's `Make`. Wir is the human-readable
form of the imperative IR that sits between the program tree and
`Tagatha.Code`.

The ebnf meta-grammar has no comment syntax, so this file — not inline
comments — is the annotated reference.

## Nature of the IR

- **Post-lowering.** Names are already resolved to integer offsets, so
  arguments, locals and result slots are integers, not identifiers.
- **Indices are 1-based.** `arg`, `local`, and `result` indices map straight
  through to Tagatha's `Argument_Index` / `Local_Index` / `Result_Index`,
  which are `range 1 .. Last`. The first argument is `arg 1`, the first local
  `local 1`; index `0` is out of range.
- **Untyped words.** Every value is a word. The sole type hook is the
  `: float` content tag, which sets `Ast.Expression.Content := 1`
  (`Set_Content`); absent leaves the default word (`0`), threaded into each
  Tagatha push/pop. A double is two words, so the tag is what makes a load or
  store two words wide — it belongs on an lvalue as much as on an expression.
  A binary needs no tag: Tagatha derives the result content from the operands.
- **Doubles.** A real literal (`1.5`, `-1.25`, `2.5e2`) builds
  `Ast.Expression.Real_Literal`. Only the four arithmetic operators have float
  forms (`fadd fsub fmul fdiv`); `neg`, `mod` and the comparisons take their
  float behaviour from operand content, so the backend emits the sign flip,
  `frem` and `fcmp` without being told. `to_float` and `to_word` convert
  between the two contents.
- **No precedence.** Binary operations are fully parenthesised — the IR spells
  every operation out, one `Ast.Expression.Binary` per `( … )`.
- **Boolean flags are keywords:** `public`, `rw` (read/write), `extern`,
  `addr` (address-of).

## Structure

A unit is routines first, then data items — matching `Ast.Unit`, which
generates all routines before any data (data ops need no active routine).

```
unit ::= { routine } { data_item }
```

## Node → production

| `Ast` node | wir production | Notes |
|---|---|---|
| `Unit` | `unit` | routines then data |
| `Routine` | `routine [public] N args I locals I is … end` | `I` = `Argument_Count`, then `Local_Total`; `public` = `Is_Public` |
| `Statement.Sequence` | `begin … end;` | ordered group, no local frame |
| `Statement.Block` | `block locals I … end block;` | nested scope with its own `I` locals |
| `Statement.Assign` | `lvalue := expression;` | evaluate value, then `Target.Set_Value` |
| `Statement.Return` | `return [expression];` | value → result slot 1, then exit |
| `Statement.Call` | `call N ( args );` | direct call for effect, result count 0 |
| `Statement.Conditional` | `if e then … [else …] end if;` | then/else lists lower to `Sequence` |
| `Statement.Iteration` | `loop e do … end loop;` | pre-tested loop |
| `Statement.Label` | `label N;` | routine-scoped label definition |
| `Statement.Goto` | `goto N;` | local unconditional branch |
| `Statement.Jump` | `jump N;` | non-local jump (no return) |
| `Statement.Fail` | `fail;` | `Fail_Routine` |
| `Statement.Raise` | `raise e;` | evaluate then `Raise_Exception` |
| `Statement.Evaluate` | `eval e;` | evaluate for effect, `Drop` the value |
| `Statement.Retry_Routine` | `retry;` | restart routine from the top |
| `Expression.Literal` | `[-] int` | `Integer` value |
| `Expression.Real_Literal` | `[-] real` | binary64 value; sets its own float content |
| `Expression.Argument` | `arg I` | 1-based argument index |
| `Expression.Local_Variable` | `local I` | 1-based local index |
| `Expression.Result_Value` | `result I` | 1-based; **lvalue only** — write-only slot |
| `Expression.Binary` | `( e op e )` | `op` → `Ast.Operator` ordinal |
| `Expression.Unary` | `unop e` | `+` / `neg` / `not` / `test` |
| `Expression.Conversion` | `to_float e` / `to_word e` | changes content, not value: `flot` / `fix` |
| `Expression.Name` | `[extern] N` | value at the name (`Push_Name`) |
| `Expression.Name` (address) | `addr [extern] N` | `Make_Address`, `Is_Address` |
| `Expression.Call` | `N ( args )` | direct call as value, result count 1 |
| `Expression.Indirect_Call` | `* e ( args )` | call through computed address |
| `Expression.Dereference` | `[ e [+ off] ]` | value at `Address + Offset` |
| `Data.Label` | `[rw] data N:` | `rw` = writable (`Data_Label_RW`) |
| `Data.Word` | `[rw] word [-] int;` | one word (`Data_Int` / `Data_RW`) |

| `Data.Reference` | `ref N;` | word holding a named label's address |
| `Data.Text` | `text "…";` | string constant |

## Operators

Tokens map to the `Ast.Operator` ordinals in
[`ast-operator.aqua`](../../lib/aqua/ast/ast-operator.aqua):

| Token | Op | Ordinal | | Token | Op | Ordinal |
|---|---|---|---|---|---|---|
| `+` (unary) | Identity | 0 | | `mod` | Modulo | 8 |
| `neg` | Negate | 1 | | `&` | Bit_And | 13 |
| `not` | Logical_Not | 2 | | `\|` | Bit_Or | 14 |
| `test` | Test | 3 | | `xor` | Bit_Xor | 15 |
| `+` | Add | 4 | | `=` | Compare_Equal | 18 |
| `-` | Subtract | 5 | | `/=` | Compare_Not_Equal | 19 |
| `*` | Multiply | 6 | | `<` | Less | 20 |
| `/` | Divide | 7 | | `<=` | Less_Equal | 21 |
| `fadd` | Float_Add | 9 | | `>` | Greater | 22 |
| `fsub` | Float_Subtract | 10 | | `>=` | Greater_Equal | 23 |
| `fmul` | Float_Multiply | 11 | | | | |
| `fdiv` | Float_Divide | 12 | | | | |

`Bit_Xor` is the keyword `xor`, not `^`: `^` risks a token-class clash with
`ada_symbol`, whereas `&` and `|` are declared as delimiter symbols and are
safe. Codes 16 `Dereference` / 17 `Store` are expressed at the node level
(`[ … ]` and `:=`), not as infix operators.

There are only four float operators because the rest need none: `neg`, `mod`
and the six comparisons take their float form from the content of their
operands, so the backend emits the sign flip, `frem`, `fcmp` and `feql`
without a distinct opcode in the IR.

## lvalues

Only the `Set_Value`-capable expressions may sit left of `:=`:

```
lvalue ::= ( argument | local_variable | result_slot | store_name
           | dereference ) [ ': float' ]
store_name ::= [ 'extern' ] name
```

The content tag is as necessary here as in an expression: `Set_Value` pops by
`Content`, so `local 1 := 1.5` would store one word of a double. Write
`local 1 : float := 1.5`.

Every other expression form raises on assignment. In particular `addr N`
(an address) is rvalue-only — it is excluded from `store_name` — and
`result I` is write-only: it appears in `lvalue` but reading it (in
expression position) is a semantic error (`Generate` raises "routine result
slot is write-only").

## Lexical

| Class | Definition |
|---|---|
| `identifier` | `!\l[\w]*!` — letter then word chars |
| `real` | `standard ada_real_literal` — **must be declared before `integer`** |
| `integer` | `standard ada_integer_literal` |
| `string_constant` | `standard ada_string_literal` |
| delimiters | `()[]+,;&\|` |
| symbols | `standard ada_symbol` (covers `-` `*` `/` `:` `:=` `=` `/=` `<` `<=` `>` `>=`) |
| line comment | `--` |

`real` before `integer` is not cosmetic. The tokeniser takes the first declared
class that matches rather than the longest, so with `integer` first, `3.14`
lexes as `3` and leaves `.14` stranded. For the same reason neither class can be
`ada_numeric_literal`, which matches both forms.

Case-insensitive.

Case-insensitive.

`rw` is **per item**: it flags the label (`rw data`) or the word (`rw word`)
independently. A writable label and its writable contents both need `rw`;
mixing a writable label with read-only words puts them in different sections.

## Example

```wir
routine public add
   args 2
   locals 1
is
   local 1 := (arg 1 + arg 2);
   if (local 1 > 10) then
      return (local 1 - 10);
   else
      result 1 := local 1;
      return;
   end if;
end

data message:
   text "hello";
rw data counter:
   rw word 0;
   ref message;
```

## Code generation

The `generate` action group (`stage: code`, one visitor class per
non-terminal in [`aqua/`](aqua/)) walks the parse tree bottom-up, building the
`Ast.*` nodes as synthesized attributes, and in the top `unit` node opens a
`Tagatha.Code`, lowers the whole unit into it, and closes it. The visitor
framework it uses is documented in
[`docs/visitor-mechanism.md`](../../docs/visitor-mechanism.md). Run it with:

```
bin/aquarius --code-trigger <path>.wir
```

which emits PDP-11 assembly to `tagatha.pdp11.s`. Add `--arch` to retarget —
`pdp11` (the default), `aqua` or `6502` — and the output file is named to
match: `tagatha.<arch>.s`. (Plain `bin/aquarius --check <path>` parses and
validates only, without generating code.)
