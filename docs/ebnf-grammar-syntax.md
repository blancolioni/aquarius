# Aquarius EBNF grammar syntax

Reference for the `.ebnf` files under `share/aquarius/grammar/<lang>/<lang>.ebnf`.
A `.ebnf` file defines a language: its tokens, its syntax rules, and its
formatting. It is itself parsed by a bootstrap grammar defined in code
([aquarius_programs/src/aquarius-grammars-ebnf.adb](../aquarius_programs/src/aquarius-grammars-ebnf.adb),
lexer classes in the same file's `Create_Terminals`). That reader is the source
of truth; this doc describes what it accepts.

## File shape

A file is a flat list of **definitions**, one per logical line. Three kinds:

| Kind | Form | Purpose |
|------|------|---------|
| value-definition  | `name = expr`            | set a configuration property |
| rule-definition   | `name ::= body`          | define a token or a syntax rule |
| format-definition | `.format target kw...`   | layout/pretty-print hints |

The reader has no reserved words: the only keywords (`.format`, `.standard`,
`.delimiters`) carry a leading `.` so they never collide with a rule or token
named `format`, `standard`, etc. Everything else is a sigil.

Comments are Ada-style `--` to end of line (the `.ebnf` file's own comment).

## Lexical tokens (of the .ebnf file)

- **identifier** — Ada identifier: letter then letters/digits/underscores
  (`json_value`, `withed_unit_name`).
- **terminal** — single-quoted literal: `'package'`, `';'`, `'=>'`. The lone
  apostrophe token is written `'''`.
- **string** — Ada double-quoted: `"{},=:"`, `"--"` (`""` escapes a quote).
- **integer** — Ada numeric literal.
- **regex** — bang-delimited: `!\l[\w_]*!` (see [Regex](#regex-bodies)).
- **symbol** — `::=`, `=`, etc. **delimiter** — one of `{}[]<>|()/`.

## Value definitions (`name = expr`)

`expr` is a string, identifier, or integer. `name` is a plain identifier drawn
from a fixed set of configuration settings; an unrecognised name is an **error**
(`unknown setting: <name>`). Recognised settings:

```ebnf
case_sensitive       = false          -- keyword/identifier case folding
line_comment         = "--"           -- start of line comment
block_comment_start  = "(*"
block_comment_end    = "*)"
continuation         = "\"            -- single-char line-continuation
```

The start symbol is simply the **first** rule-definition in the file. Its name
is arbitrary — `top_level` is a convention, nothing relies on it:

```ebnf
top_level ::= compilation_unit
```

## Token (terminal) definitions

Rule-definitions whose body is not plain syntax define lexical classes:

```ebnf
identifier ::= .standard ada_identifier     -- use a built-in lexer
integer    ::= !\d+!                         -- regex body
string     ::= !\x22[^\x22]*\x22!
delimiter  ::= .delimiters "{},=:"           -- each char is its own token
```

Three token-body forms:

- **`.standard <name>`** — a built-in lexer. Available:
  `ada_identifier`, `ada_numeric_literal`, `ada_integer_literal`,
  `ada_real_literal`, `ada_character_literal`, `ada_string_literal`,
  `ada_symbol`, `ada_comment`.

  `ada_numeric_literal` covers both integers and reals, so a grammar with a
  *separate* real token must use `ada_integer_literal` (no decimal point;
  based literals like `16#ff#` are integers) and `ada_real_literal`
  (`digits . digits` with an optional exponent) instead — otherwise both
  token classes match `3.14` with the same length and the tie is resolved by
  whichever the parse context happens to accept.
- **`.delimiters "<chars>"`** — declares each character in the string as a
  single-character delimiter token.
- **regex** (`!...!`) — see below.

### Regex bodies

Delimited by `!`. Aquarius regex syntax (`Aquarius.Regexes`):

- classes `\l` letter, `\w` word char, `\d` digit; hex `\x22` (= `"`);
  escaped literals `\.` `\'`
- char sets `[...]`, negation `[^...]`
- quantifiers `*` `+` `?`, groups `( )`, alternation `|`

```ebnf
identifier         ::= !\l[\w]*!
real               ::= ![0-9]+\.[0-9]+([eE][0-9]+)?!
character_constant ::= !\'[.]\'!
```

## Syntax rules

Rule body = a **choice** of one or more **sequences**, separated by `|`:

```ebnf
json_value ::= primitive_value | array_value | object_value

primitive_value ::= null_value
        | boolean_value
        | integer_value
        | string_value
```

A **sequence** is a run of rule-items. Each item is one of:

| Item | Meaning |
|------|---------|
| `'literal'`        | terminal keyword/symbol |
| `rule_name`        | reference to another rule |
| `[ ... ]`          | optional (zero or one) |
| `{ repeater }`     | repeat zero or more |
| `< repeater >`     | repeat one or more |
| `( body )`         | nested group (a full choice-body) |

### Repeaters and separators

Inside `{ }` or `< >`, an optional separator follows `/`:

```ebnf
array_value ::= '[' { json_value / ',' } ']'          -- 0+, comma-separated
use_clause  ::= 'use' < external_unit_name / ',' > ';' -- 1+, comma-separated
qualified_reference ::= < identifier / '.' >
```

`{ x }` = 0+, `< x >` = 1+. With a separator, it appears only *between*
elements.

### Named children (class spec)

A reference may carry a role name via `name : rule`:

```ebnf
with_clause ::= 'with' < withed_unit_name : package_name / ',' > ';'
```

Here each child parses as `package_name` but is labelled `withed_unit_name` in
the tree.

## Format definitions

Layout hints for the pretty-printer. Target is a terminal (`';'`) or a rule
name; followed by one or more format keywords:

```ebnf
.format context_clause new_line_after
.format declarative_part indented_child
.format ';' no_space_before no_space_after new_line_after
.format '(' space_before no_space_after
.format ':' space_after space_before
```

Keywords in use across the shipped grammars:

`space_before` · `space_after` · `no_space_before` · `no_space_after` ·
`new_line_before` · `new_line_after` · `soft_new_line` · `soft_new_line_before`
· `soft_new_line_after` · `indent_before` · `outdent_after` · `indented_child` ·
`closing`

## Grammar-of-the-grammar (summary)

Distilled from the bootstrap reader:

```ebnf
source-file      ::= { definition }
definition       ::= value-definition | format-definition | rule-definition
value-definition ::= identifier '=' ( string | identifier | integer )
format-definition::= '.format' terminal-or-rule { identifier }
rule-definition  ::= identifier '::=' definition-body
definition-body  ::= standard-body | delimiter-body
                   | regex | syntax-body
standard-body    ::= '.standard' identifier
delimiter-body   ::= '.delimiters' string
syntax-body      ::= sequence-of-rules { '|' sequence-of-rules }
sequence-of-rules::= { rule }
rule             ::= '{' repeater '}'          -- repeat 0+
                   | '<' repeater '>'          -- repeat 1+
                   | '[' sequence-of-rules ']' -- optional
                   | '(' syntax-body ')'       -- nested
                   | terminal-rule
repeater         ::= sequence-of-rules [ '/' terminal-rule ]
terminal-rule    ::= rule_name | terminal
rule_name        ::= identifier [ ':' identifier ]
terminal-or-rule ::= identifier | terminal
```

## Worked example — complete JSON grammar

```ebnf
top_level ::= json_value
case_sensitive = false

.format ':' no_space_before
.format ',' no_space_before new_line_after
.format '{' new_line_after
.format '}' new_line_before
.format component_value indent_before outdent_after

identifier ::= !\l[\w_]*!
integer    ::= !\d+!
string     ::= !\x22[^\x22]*\x22!
delimiter  ::= .delimiters "{},=:"

json_value      ::= primitive_value | array_value | object_value
primitive_value ::= null_value | boolean_value | integer_value | string_value
null_value      ::= 'null'
boolean_value   ::= 'true' | 'false'
integer_value   ::= integer
string_value    ::= string
array_value     ::= '[' { json_value / ',' } ']'
object_value    ::= '{' { component_value / ',' } '}'
component_value ::= string ':' json_value
```
