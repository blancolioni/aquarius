# Visitor mechanism — design notes

A critical assessment of the Aqua visitor mechanism (see
[`visitor-mechanism.md`](visitor-mechanism.md) for how it works), with proposed
improvements. Written after building the `wir` code-generation pass (53 visitor
classes lowering a textual IR to Tagatha), which stressed most of the corners.

The intent is to record *why* the mechanism feels the way it does, where it
holds up, where it forces workarounds, and which changes would pay off — not to
argue for a rewrite. Everything here is actionable in isolation.

## 1. What the design optimizes for

The mechanism is a **bottom-up synthesized-attribute walker**. One class per
grammar rule; the framework instantiates a node object per tree node and calls
`After_<child>` / `After_Node` hooks as it reduces the subtree. Results flow
**upward** through public fields.

For the workload it targets — formatters, simple code generators, serializers,
tree-shaped semantic checks over a stable grammar — this is a good fit:

- Behaviour sits next to syntax; one class per rule is easy to locate and grow.
- No visitor-dispatch boilerplate, no hand-written recursion, no explicit
  traversal order to maintain.
- Rules with no class cost nothing, so a pass can be built incrementally.
- Multiple passes (`stage: semantic`, `stage: code`) share the tree without
  interfering.

`normandy` (JSON serialization) and `wir` (Tagatha code-gen) both land squarely
in this sweet spot and are pleasant to read.

## 2. Where it forces workarounds

The following are not style complaints — each one changed how the grammar or
the classes had to be written.

### 2.1 The binding is stringly-typed and unchecked

A hook binds by **name**: `After_Statement_List` matches a child named
`statement_list`. Nothing validates the match. A typo, or a renamed rule, makes
the hook **silently never fire** — no "hook names a child that does not exist"
diagnostic. Whole classes can go dead-quiet after a grammar edit. During
bring-up this is the single biggest time sink, because the failure looks like a
logic bug, not a wiring bug.

### 2.2 Keyword hooks crash instead of no-op

A literal keyword terminal (`'public'`) is registered syntax, so a plausible
`After_Public` (zero arguments) matches it and then **crashes the binder**
(`Add_Feature_Binding` calls `Feature.Argument (1)` on a zero-arg feature). The
worst failure mode: a natural thing to write takes down the entire plugin load
with an assertion, not a source-located error.

### 2.3 No inherited (downward) attributes

Only synthesized attributes exist — data flows up. There is **no channel to
pass context down** to children (enclosing scope, expected type, a target hint).
`Before_Node` fires before children but cannot hand them anything. This is the
deepest limit: name resolution, type checking, and most non-trivial semantics
need downward context, and the mechanism has no answer. `wir` sidesteps it only
because Tagatha wants a flat bottom-up stream; a real front end would hit a wall.

### 2.4 Positional disambiguation is fragile

When a rule has two children of the same kind, hooks cannot tell them apart by
name — you count firings or track a flag:

```
--  conditional: if <expr> then <statement_list> [ else <statement_list> ]
After_Statement_List (Child : ...)
   do
      if attached Then_Part then Else_Part := Child.Stmt
      else Then_Part := Child.Stmt end
   end
```

Add or reorder a child and this silently shifts. To get *distinct* hook names I
added wrapper rules (`arg_count ::= integer`, `local_count ::= integer`) purely
to serve the binder — the grammar is distorted to make the mechanism usable.

### 2.5 Optional-keyword detection is a stacked hack

You cannot hook a keyword (§2.2), so an optional flag becomes: wrap it in its
own rule, then string-compare the wrapper's image:

```ebnf
routine_scope ::= [ 'public' ]
```
```
if Child.Concatenated_Image = "public" then Is_Public := True end
```

This relies on the **undocumented** quirk that an empty node's
`Concatenated_Image` is the *rule name* (so you must test the positive value,
not emptiness). Three non-obvious facts stack up to answer "is this flag set,"
and every flag costs a throwaway rule plus a throwaway class.

### 2.6 `Concatenated_Image` is overloaded as a data accessor

It is a rendering function (tokens glued, no separators; empty → rule name)
pressed into semantic duty. Reading a signed integer means taking the glued
image and peeling the `-` by hand because `To_Integer` rejects signs. Any change
to tokenization or formatting moves this ground.

### 2.7 Correctness lives in hidden firing order + guard flags

A node is correct only if hooks fire in the expected order and the guard flags
are right (`if attached Left then Right := ... else Left := ...`). There is no
declarative "this hook is the left operand." An ordering assumption that breaks
produces a wrong tree with no error.

### 2.8 Boilerplate scales with the grammar, not the logic

Roughly half of the 53 `wir` classes are plumbing: dispatch rules that copy a
child's attribute up (`primary` = ten identical `After_X do Copy(Child.Expr)
end`), empty stub classes for wrapper rules, and attribute base classes. There
is no default/fallthrough hook to say "copy whichever child fired."

## 3. The tell

In `wir`, the visitor layer is a thin **tree → builder adapter**: it does no
real work beyond constructing `Ast.*` nodes, and *all* semantics (lowering,
operand-stack discipline, label interning) live in the separate `Ast.*` model
that the top node's `After_Node` drives via `Generate`. That split is not an
accident of this grammar — it is the mechanism telling you it is not expressive
enough to hold the logic, so the logic moves out into an object model. That is
fine as an architecture, but it means the visitor layer's real job is narrow,
and several of the weaknesses above (2.3 especially) are why.

## 4. Proposed improvements

Ordered by payoff. Each is independent.

### P1 — Inherited-attribute channel (addresses 2.3)

The highest-value change. Give `Before_Node` (or a dedicated `Inherit` hook) a
way to publish a context object that children read before they reduce. Even a
single typed `Context` slot per node, defaulting to the parent's, would unlock
scope- and type-directed passes without leaving the mechanism. Without this, any
front end more serious than a formatter is forced into an external model.

### P2 — Binder diagnostics (addresses 2.1, 2.2)

At bind time, for each `<pos>_<child>` feature, check that `<child>` names a real
child/token/`node` of the class's rule; otherwise emit a source-located warning
("hook `After_Foo` names no child of rule `bar`"). Make a keyword match a
warn-and-skip, never a crash. This removes the largest bring-up time sink and
the nastiest failure mode at once, and is cheap.

### P3 — First-class flags and typed leaves (addresses 2.5, 2.6)

Bind an optional keyword to a boolean hook — `After_Public (Present : Boolean)`
— instead of forcing a wrapper rule and an image comparison. Similarly, give
token leaves a typed value (`After_Integer (Value : Integer)`), so signed and
based literals are parsed once, correctly, by the tokenizer rather than by hand
in every consumer.

### P4 — Positional / indexed hooks (addresses 2.4, 2.7)

Allow a hook to bind to a specific occurrence — `After_Statement_List_1`,
`_2` — or expose a small typed accessor for a rule's named children, so
then/else and arg/local counts read by position instead of by mutable flag. Kills
the wrapper-rule-per-scalar tax and the hidden-order fragility.

### P5 — Fallthrough / reduce helpers (addresses 2.8)

A default `After_Child` (fires for any child not otherwise handled) collapses the
ten-way dispatch copy-ups into one line. Optionally, a declared "this rule's
value is its single child's value" shorthand would remove pure pass-through
classes entirely.

## 5. Bottom line

The mechanism is well matched to bottom-up synthesis over a stable grammar and
should stay the default for that. Its expressiveness ceiling is **downward
context** (P1); its usability ceiling is **unchecked string binding and the
keyword-crash** (P2). P1 and P2 together would move it from "great for
adapters, escape-hatch for everything else" to "viable for real semantic
passes." The rest (P3–P5) are ergonomics that pay for themselves in reduced
grammar distortion and boilerplate.
