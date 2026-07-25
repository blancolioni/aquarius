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
- **Untyped words.** Every value is a word. The sole type hook is the
  `: float` content tag, which sets `Ast.Expression.Content := 1`
  (`Set_Content`); absent leaves the default word (`0`), threaded into each
  Tagatha push/pop.
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
| `Expression.Argument` | `arg I` | by integer offset |
| `Expression.Local_Variable` | `local I` | by integer offset |
| `Expression.Result_Value` | `result I` | **lvalue only** — write-only slot |
| `Expression.Binary` | `( e op e )` | `op` → `Ast.Operator` ordinal |
| `Expression.Unary` | `unop e` | `+` / `neg` / `not` / `test` |
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
| | | | | `>` | Greater | 22 |
| | | | | `>=` | Greater_Equal | 23 |

`Bit_Xor` is the keyword `xor`, not `^`: `^` risks a token-class clash with
`ada_symbol`, whereas `&` and `|` are declared as delimiter symbols and are
safe. Operator codes 9..12 (float ops) are omitted from the IR; codes
16 `Dereference` / 17 `Store` are expressed at the node level (`[ … ]` and
`:=`), not as infix operators.

## lvalues

Only the `Set_Value`-capable expressions may sit left of `:=`:

```
lvalue ::= argument | local_variable | result_slot | store_name | dereference
store_name ::= [ 'extern' ] name
```

Every other expression form raises on assignment. In particular `addr N`
(an address) is rvalue-only — it is excluded from `store_name` — and
`result I` is write-only: it appears in `lvalue` but reading it (in
expression position) is a semantic error (`Generate` raises "routine result
slot is write-only").

## Lexical

| Class | Definition |
|---|---|
| `identifier` | `!\l[\w]*!` — letter then word chars |
| `integer` | `standard ada_numeric_literal` |
| `string_constant` | `standard ada_string_literal` |
| delimiters | `()[]+,;&\|` |
| symbols | `standard ada_symbol` (covers `-` `*` `/` `:` `:=` `=` `/=` `<` `<=` `>` `>=`) |
| line comment | `--` |

Case-insensitive.

## Example

```wir
routine public add
   args 2
   locals 1
is
   local 0 := (arg 0 + arg 1);
   if (local 0 > 10) then
      return (local 0 - 10);
   else
      result 1 := local 0;
      return;
   end if;
end

data message:
   text "hello";
rw data counter:
   word 0;
   ref message;
```

Validate any `.wir` file (or this grammar) with
`bin/aquarius --check <path>`.
