# Aquarius EBNF grammar syntax — design evaluation

Critique of the `.ebnf` notation described in
[ebnf-grammar-syntax.md](ebnf-grammar-syntax.md). Judged as a *tool* language
(parse + format + light semantics in one file) for Aquarius's incremental
editor, not as a general grammar formalism. Snapshot: 2026-07-26.

Organizing principle: the notation maps **1:1 to internal syntax-node kinds**
(`New_Sequence` / `New_Choice` / `New_Repeat` / `New_Optional` / `Terminal` in
[aquarius-grammars-ebnf.adb](../aquarius_programs/src/aquarius-grammars-ebnf.adb)).
Each bracket is a node constructor. That explains most design choices.

## Strengths

- **Unified artifact.** One file = tokens + syntax + pretty-print layout
  (`format`). Rare and valuable; drives the incremental editor. Most toolchains
  split these across lexer/parser/formatter.
- **Bracket = node constructor.** `{}`→repeat-0+, `<>`→repeat-1+, `[]`→optional,
  `()`→nested choice. Principled, direct to the tree.
- **Named children** (`name : rule`) — semantic labels for actions/xref without
  a separate binding file.
- **`standard` lexers** reuse common token classes — no re-spelling
  `ada_identifier` etc.
- **Inline token decls** — regex `!...!`, `delimiters "..."` — compact.
- **`<>` for repeat-1+.** 1+ is common, and the usual EBNF spelling
  `rule { rule }` builds a lopsided tree — head split from the repeated tail —
  which is poison for tree-driven actions/xref/formatting. `<>` yields one clean
  repeat node. The angle brackets are *free*: Aquarius delimits nothing for
  nonterminals (bare identifiers), so there's no BNF `<name>` collision, and a
  single-char bracket beats any multi-char alternative (`{+ … +}`).

## Weaknesses — ranked

1. **Two meta-notations for repetition.** Brackets (`{} <> []`) in syntax, but
   `* + ?` postfix in regex bodies. Same concepts, different spelling, one file.
   Cognitive tax.

2. **No precedence/associativity.** Expression grammars must be hand-stratified
   (term/factor/…) — verbose. No `%left`/`%right`. Offloaded to the parser's
   parallel-ambiguity engine + `when` guards → slower, harder to reason about
   (see [parser-analysis.md](parser-analysis.md) issues #1, #10).

3. **Contextual keywords, not reserved.** `format` `standard` `delimiters`
   `xref` `when` `top_level` `case_sensitive` share the namespace with rule
   names. A target language rule named `format` or `when` clashes.

4. **Silent typos.** Value-definitions (`case_sensitive = false`) and format
   keywords are validated in the *semantic* layer, not the grammar.
   `case_sensitve = false` or `format x indnet_before` → likely ignored, no
   error. Wide silent-failure surface.

5. **No modularity.** No `include`/import. `ada.ebnf` is monolithic. No
   parametric rules — repeated patterns re-spelled (mitigated a bit by
   `< x / ',' >`).

6. **Global case sensitivity.** Single `case_sensitive` flag. Can't mix
   case-insensitive keywords with case-sensitive identifiers (common need).

7. **Lexical escape gaps.**
   - Terminal containing an apostrophe impossible except the lone `'''` hack.
   - Regex can't contain a literal `!` (`!`-delimited, no escape).
   - Separator (`/ x`) limited to a single terminal-rule — no multi-token
     separators.

8. **`when` opacity.** Guards reference external flags/properties with no
   in-grammar definition or doc. Tightly couples grammar to the action/property
   layer; unexercised in shipped grammars = likely under-tested.

## Verdict

Pragmatic and well-fitted to Aquarius's incremental-edit + format use case; the
bracket-to-node mapping is genuinely elegant. Weakest as a *grammar formalism*:
no precedence, no modularity, contextual-keyword clashes, and a broad
silent-typo surface that pushes error detection downstream.

Highest-leverage fixes:

1. Validate property and format-keyword names at load → hard error on typo.
2. Reserve the structural keywords, or namespace them.
3. Add precedence declarations to shrink expression grammars and cut the
   parser's ambiguity work.
