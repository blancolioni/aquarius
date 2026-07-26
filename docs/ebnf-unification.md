# Unifying the regex and syntax notations

**Proposal.** Retire the PCRE-style regex notation (`!\l[\w_]*!`) and express
character-level (token) rules with the **same EBNF operators** used for syntax
rules. One notation for the whole `.ebnf` file.

Direction matters: keep the EBNF brackets for syntax — they are readable and
universally recognised — and port the *lexical* rules onto them, not the other
way round. Terseness is not the goal; a single, familiar notation is.

The trade is deliberately lopsided in EBNF's favour. Lexing is a tiny bootstrap
into the real work — parsing files. A handful of token rules per grammar buy a
consistent notation across the entire tool. Losing PCRE muscle-memory in that
small corner costs almost nothing.

## Why it fits

Aquarius already has the algebra on both levels:

- Syntax rules map 1:1 to `Syntax_Tree` constructors (`New_Sequence`,
  `New_Choice`, `New_Repeat`, `New_Optional`) — see
  [aquarius-grammars-ebnf.adb](../aquarius_programs/src/aquarius-grammars-ebnf.adb).
- Token rules map 1:1 to **Lexer** combinators — `Literal`, `One_Of`, `Repeat`,
  `not`, `or`, `&` — in the same file's `Create_Terminals` (:229).

Both are Kleene-algebra expression languages; they differ only in **alphabet**
(tokens vs characters) and **output** (a tree vs a lexeme). The combinators are
alphabet-independent, so one operator set can drive both. The PCRE spelling was
an accident of history, not a semantic necessity.

## Operator mapping

`|`, `( )`, and juxtaposition (sequence) are already identical in both notations
— nothing changes there. Only repetition/optional need porting:

| concept | regex (old) | EBNF-over-chars (new) |
|---------|-------------|-----------------------|
| sequence    | `ab`     | `a b`       |
| alternation | `a\|b`   | `a \| b`    |
| group       | `(ab)`   | `( a b )`   |
| 0 or more   | `x*`     | `{ x }`     |
| 1 or more   | `x+`     | `< x >`     |
| optional    | `x?`     | `[ x ]`     |

## Character classes dissolve into rules

`[...]` is the most familiar regex construct, but in EBNF `[]` already means
optional. Don't overload it — **dissolve it**. In Unicode, character
classification is **not** a matter of ranges (see [Ranges are the wrong
primitive](#ranges-are-the-wrong-primitive)); a class is a named **property**.
Promote the class shorthands (`\d \l \w \s .`) to a **builtin prelude** backed
by Unicode General_Category tables (defined in Ada code, not as `.ebnf`
ranges):

```ebnf
letter     ::= <Unicode L*>          -- Lu | Ll | Lt | Lm | Lo (a property, not a range)
uppercase  ::= <Lu>
lowercase  ::= <Ll>
digit      ::= '0' .. '9'            -- ASCII, on purpose (see below)
whitespace ::= <White_Space>
word_char  ::= letter | digit | '_'
any        ::= <any scalar value>    -- ".": any single Unicode scalar
```

Then `\l` → `letter`, `\d` → `digit`, `\w` → `word_char`, `.` → `any`. These are
ordinary **rule references** — the same atom the syntax level already uses. User
grammars reference them and rarely need raw sets.

## Two new atoms

Both already idiomatic in this (Ada) codebase:

- **Complement** — `not '"'`, or set-difference `any - '"'`. Maps straight to the
  Lexer `not` combinator. Covers `[^"]`. Well-defined over the scalar space
  ("any scalar ≠ U+0022").
- **Codepoint range** — `#0000 .. #001F`. A range over **integer codepoints**,
  for value/validity work only (control chars, surrogate exclusion, encoding
  bounds). Written on hex codepoint literals to make clear it is numeric, not a
  span of "characters".

Small explicit sets stay as `|` alternation or `delimiters "..."`. That is the
entire addition; everything else is existing EBNF.

## Ranges are the wrong primitive

A general `'a' .. 'z'`-style character range is ASCII-era thinking and mostly
breaks under Unicode:

- **letter / whitespace / word / upper / lower** are Unicode *properties*
  (`L*`, `White_Space`, `Nd`, …), scattered across the codespace — **not**
  contiguous ranges. `'a'..'z' | 'A'..'Z'` is just ASCII parochialism and
  excludes every non-Latin letter. These must be property builtins.
- The only genuinely useful character range is **ASCII digit `'0' .. '9'`** —
  contiguous, and lexers usually *want* ASCII-only digits (an integer literal
  should not accept Devanagari or fullwidth digits). Kept as a deliberate
  restriction, not as "digit in general".
- The other legitimate use of ranges is over **codepoint values**, not
  characters: control ranges (`#0000..#001F`), surrogate exclusion
  (`#D800..#DFFF`), max scalar `#10FFFF`. That is the `#.. ..` atom above — a
  separate, honestly-numeric construct.

So: no general character `..`; classification via property builtins; a narrow
codepoint-numeric range for value work; ASCII digit as the one kept character
range.

## Quoting removes all escaping

Because literals are always quoted, metacharacters need no backslashes. A literal
brace is `'{'`; the quote disambiguates. No more `\x22`, `\.`, `\'`. This is a
real readability win and a strong argument for the direction on its own.

## The `!…!` fence

Keep it (or introduce an equivalent `token`-rule marker). It still earns its
place — it declares three things:

1. **alphabet = characters** (atoms are chars / char-rules, not tokens),
2. **output = a lexeme**, not a subtree,
3. **whitespace is layout**, ignored — exactly as in syntax mode.

So `{ word_char }` never matches a space; to match a literal space, write `' '`
— the same quoting rule as syntax mode. Inside the fence: full EBNF, no PCRE.

## Worked rewrites

```ebnf
-- identifier ::= !\l[\w_]*!
identifier ::= letter { word_char }

-- string ::= !\x22[^\x22]*\x22!
string ::= '"' { not '"' } '"'

-- real ::= ![0-9]+\.[0-9]+([eE][0-9]+)?!
real ::= < digit > '.' < digit > [ ( 'e' | 'E' ) < digit > ]

-- character_constant ::= !\'[.]\'!
character_constant ::= "'" any "'"
```

Longer than the PCRE forms, but each reads as plain EBNF, and the grammars have
only a handful of such rules.

## Cost / benefit

**Gained**
- One operator set across the whole file; EBNF-familiar throughout.
- All metacharacter escaping gone (universal quoting).
- Token rules map to Lexer combinators as cleanly as syntax maps to the tree.

**Added (small, mechanical)**
- `..` range and `not` / `-` complement operators.
- A builtin character-rule prelude (`letter`, `digit`, `word_char`, `space`,
  `any`, …).
- Full-EBNF parsing inside `!…!` in the reader.

**Lost — minor**
- PCRE muscle memory and terse `[a-z]` / `[^"]`. Confined to token rules, which
  are a tiny bootstrap ahead of the main business of parsing files. Negligible.

## Implementation sketch

1. **Prelude.** Define the builtin character rules once (in code or a bootstrap
   `.ebnf`), backed by the existing Lexer combinators.
2. **Reader.** Teach the `.ebnf` grammar to parse a full EBNF body inside `!…!`
   (or behind a `token` marker) at the character alphabet; add `..` and
   `not`/`-`. Reuse the syntax-body machinery in
   [aquarius-grammars-ebnf.adb](../aquarius_programs/src/aquarius-grammars-ebnf.adb).
3. **Lowering.** Map the char-level tree to `Lexer`: `{ }`→`Repeat`,
   `< >`→`x & Repeat(x)`, `[ ]`→optional, `|`→`or`, juxtaposition→`&`,
   `'c'`→`Literal`, `a..b`→range, `not x`→`not`.
4. **Migrate** the shipped grammars' token rules (scriptable; ~a dozen rules
   each). Keep the PCRE reader briefly for back-compat, then drop it.

## Related

- [ebnf-grammar-syntax.md](ebnf-grammar-syntax.md) — current syntax reference.
- [ebnf-evaluation.md](ebnf-evaluation.md) — this unification addresses
  weakness #1 (two meta-notations).
- [parser-analysis.md](parser-analysis.md) — the parser this feeds.
