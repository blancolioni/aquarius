# repro grammar

Minimal grammar that reproduces the ambiguous-token parser crash — issue #76
(the crash itself is tracked in #75).

```
bin/aquarius --check share/aquarius/tests/repro/case_label.rpr
```

raises `AQUARIUS.TREES.CURSORS.CURSOR_ERROR : attempt to move to right of null
parent`.

## Why it crashes

The crash needs **three grammar features together**; removing any one makes the
same input parse cleanly:

1. Numeric statement **labels** — `labelled_statement ::= label ':' statement`
   with `label ::= numeric_literal`.
2. The case alternative list written as the ambiguous
   `< case_element / ';' > [ ';' ]` (list immediately followed by an optional
   copy of its own separator), rather than `case_element { ';' [ case_element ] }`.
3. The `case` nested inside an outer `< statement / ';' >` list (a `begin … end`).

Root cause: a statement label `N:` and a case constant `N:` are **both a
`numeric_literal` followed by `:`**, and a case alternative's body *is* a
`statement` (which may itself be labelled). So a `N:` / `;` inside a nested case
is genuinely ambiguous. The parser enters `Parse_Ambiguous_Token`
(`aquarius-programs-parser.adb`) and the cursor walk crashes in
`Aquarius.Trees.Cursors.Move_To_Right_Of_Parent`.

Note the earlier assumption — that the `< X / ';' > [ ';' ]` shape alone is
enough — is **wrong**: that shape by itself parses or gives a clean syntax
error. It only crashes in combination with (1) and (3).
