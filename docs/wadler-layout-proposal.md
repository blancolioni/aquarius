# Proposal: Wadler-style combinator layout for Aquarius.Formats

## Problem

`Aquarius.Formats` (`aquarius_syntax/src/aquarius-formats.ads`) currently
specifies layout as a bag of independent, token-level rules attached by
name to grammar productions/terminals:

```
.format ';' no_space_before new_line_after
.format 'is' closing space_before new_line_after
.format statement_list indented_child
```

(see `share/aquarius/grammar/wir/wir.ebnf:18-31`). Each rule is a local,
imperative instruction ("space here", "maybe a newline there") with a
`Rule_Priority 0 .. 9` used to arbitrate when rules collide, and the
doc comment for `Rule_Priority` admits the tie-break for equal priorities
"should not be relied upon" (`aquarius-formats.ads:14-18`).

Deciding *when* a soft new line actually fires is a second, separate
mechanism: `Arrange` lays a line out flat, and on detecting
`Partial_Length > Right_Margin` (`aquarius-programs-arrangements.adb`,
`Reformatting.Reformat`) walks *backward* to the last remembered
soft-break candidate and retroactively flips a flag on that node. This
works, but it's a heuristic patch, not a decision with a clean
correctness argument, and `Opening`/`Closing` bracket-matching
(`aquarius-formats.ads:103-112`) is bespoke logic layered on top to
special-case one recurring pattern.

## Proposal

Introduce a small `Doc` algebra (Wadler, *A Prettier Printer*) as the
thing productions describe themselves in terms of, and let a `fits`-based
layout algorithm — not priorities, not backward-patching — decide where
lines break.

### New package: `Aquarius.Docs`

Rather than depending on `Aquarius.Programs` directly, `Aquarius.Docs`
declares a minimal interface covering only what it needs from a leaf
node, and `Program_Tree_Type` implements it. The dependency points the
other way round from the earlier draft: `Aquarius.Programs` will `with
Aquarius.Docs`, not the reverse.

```ada
package Aquarius.Docs is

   type Terminal_Node is limited interface;
   --  Everything Aquarius.Docs needs from a leaf. Nothing here
   --  mentions Program_Tree or grammars.

   function Text (Item : Terminal_Node) return String is abstract;
      --  Literal text of this leaf; used to measure whether a
      --  Group's flat form fits on the current line.

   procedure Set_Position
     (Item   : in out Terminal_Node;
      Line   : Positive;
      Column : Positive) is abstract;
      --  Record where Layout placed this leaf.

   type Terminal_Node_Access is access all Terminal_Node'Class;

   type Doc is private;

   function Nil return Doc;                              -- empty document
   function Leaf (Terminal : Terminal_Node_Access) return Doc;
   function Line return Doc;                              -- space when flat, newline when broken
   function Break return Doc;                             -- always a newline (old "New_Line")
   function "&" (Left, Right : Doc) return Doc;            -- concatenation
   function Nest (Offset : Integer; D : Doc) return Doc;   -- indent while broken
   function Group (D : Doc) return Doc;                    -- try flat; break only if it doesn't fit

   procedure Layout
     (D            : Doc;
      Width        : Positive;
      Start_Column : Positive);
      --  Decides each Group's flat-vs-broken form via Text lengths,
      --  and calls Set_Position on every leaf it visits.

end Aquarius.Docs;
```

`Group` is the whole point: it tries to render its contents flat, and
only breaks (turning every `Line` inside it into a newline, recursively
re-deciding nested `Group`s) if the flat form doesn't fit in the
remaining width. Nesting order *is* the tie-break — an outer group only
breaks if breaking every inner group first still isn't enough — so
`Rule_Priority` has no job left to do.

Giving the interface a `Set_Position` operation, not just `Text`, means
`Layout` writes positions straight through the interface as it walks —
no `Set_Position` callback parameter needed, and no downcast back to a
concrete tree type either.

`Program_Tree_Type` (`aquarius_programs/src/aquarius-programs.ads:30`)
picks up the interface as an extra progenitor:

```ada
type Program_Tree_Type is
  new ... and Aquarius.Docs.Terminal_Node with ...
```

`Text (Item : Program_Tree_Type) return String` already exists
(`aquarius-programs.ads:162`) and satisfies the interface as-is; only
`Set_Position` needs a small override that writes into the `Line`/
`Column` fields `Arrange` already mutates.

This restores full standalone testability for `Aquarius.Docs`: tests
can define a trivial test-double type implementing `Terminal_Node`
(a string field plus a captured `Line`/`Column`) with no grammar,
parser, or `Program_Tree` involved at all.

### Mapping the existing vocabulary onto the algebra

| Current (`Aquarius.Formats`)              | Becomes                                   |
|---|---|
| `Space_Always` / `Space_Never` / `Space_Sometimes` | adjacency choice between two `Text` docs — unchanged in spirit, no breaking involved |
| `New_Line (Position)`                     | `Break` inserted at that point in the concatenation |
| `Soft_New_Line (Position)`                | `Line` inserted at that point, inside the nearest enclosing `Group` |
| `Indent_Child (Offset)`                   | `Nest (Offset, ...)` wrapped around the children |
| `Opening` / `Closing`                     | mostly subsumed by `Group` itself: a production's own children are naturally one group, so the opening/closing terminal just sits at the group's boundary. Worth confirming there's no case `Group` doesn't cover before deleting the concept outright. |
| `Rule_Priority`                            | removed; conflicts resolve via group nesting, and `Join`'s existing "Right supersedes Left" rule for same-position rules |

Grammar-facing syntax barely changes — `.format` directives stay
declarative and per-name — but a production that currently just
decorates its own terminals implicitly becomes a `Group`, and
`indented_child` becomes literally `Nest`:

```
.format ';' no_space_before line_after     -- was: new_line_after (now soft by default)
.format statement_list group nest(2)       -- was: indented_child
```

### Where it plugs into `Arrangements`

`Arrange_Non_Terminal` currently walks children, calling
`Arrange_Terminal` per terminal and column-tracking as it goes; overflow
triggers `Reformatting.Reformat`'s backward patch. Replace that with:

1. Build a `Doc` for the node, bottom-up, from its children's already-built
   `Doc`s plus its `Aquarius_Format` rules (this is a pure tree fold —
   no position/column state yet). Each terminal becomes exactly one
   `Leaf (Terminal)` node.
2. Call `Docs.Layout` once, with the current column as starting
   position.

There's no separate "walk the result and match it back to nodes" pass,
and no callback to wire up — `Layout` calls `Set_Position` on each leaf
directly through the `Terminal_Node` interface as it decides positions.
`Render` and the GTK `Buffer_Renderer`
(`aquarius_ui_gtk/src/aquarius-ui-gtk_views-tree_render.adb`) are
untouched either way — they only ever consumed line/column/text per
terminal.

### Incrementality

Out of scope for v1, as agreed — every `Arrange` call rebuilds the `Doc`
for the whole subtree and re-lays it out from scratch. This is strictly
simpler than what exists today (no backward walk, no remembered
candidate state) and the `Program_Tree`/`Renderer` contract doesn't
change, so nothing downstream (GTK `Source_View`) needs to know.

The natural future improvement: `Group` is already the right *unit* of
re-layout (unlike today's "walk back to last remembered soft break"),
so a later incremental pass can restrict rebuild-and-relayout to the
smallest enclosing `Group` of an edit, rather than the whole tree. Not
needed to land v1.

## Suggested delivery steps

1. `Aquarius.Docs`: the algebra + `Layout`, unit-tested standalone
   against a throwaway `Terminal_Node` test double — no grammar,
   parser, or `Program_Tree` involved.
2. Add `Terminal_Node` as a progenitor of `Program_Tree_Type`, with a
   `Set_Position` override; then Doc-building glue inside
   `Arrangements` that folds a `Program_Tree` node + its
   `Aquarius_Format` into a `Doc`, using the mapping above.
3. Swap `Arrange_Non_Terminal`'s body to build-then-`Layout` instead of
   `Arrange_Terminal` + `Reformat`, piloted on one grammar (Wir — it's
   small and already has `.format` directives to migrate).
4. Diff rendered output against existing Pascal/Wir test fixtures to
   catch regressions before touching other grammars.
5. Revisit incremental re-layout only if full-rebuild `Arrange` proves
   too slow for GTK live editing in practice.
