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

   type Terminal_Node is interface;
   --  Everything Aquarius.Docs needs from a leaf. Nothing here
   --  mentions Program_Tree or grammars.

   function Text (Item : Terminal_Node) return String is abstract;
      --  Literal text of this leaf; used to measure whether a
      --  Group's flat form fits on the current line.

   procedure Set_Position
     (Item   : in out Terminal_Node;
      Offset : Natural;
      Line   : Positive;
      Column : Positive) is abstract;
      --  Record where Layout placed this leaf.

   type Terminal_Node_Access is access all Terminal_Node'Class;

   type Doc is private;

   function Nil return Doc;                              -- empty document
   function Leaf (Terminal : Terminal_Node_Access) return Doc;
   function Line return Doc;                              -- space when flat, newline when broken
   function Break return Doc;                             -- always a newline (old "New_Line")
   function Space return Doc;                              -- exactly one space, never a line break
   function "&" (Left, Right : Doc) return Doc;            -- concatenation
   function Nest (Offset : Integer; D : Doc) return Doc;   -- indent while broken
   function Group (D : Doc) return Doc;                    -- try flat; break only if it doesn't fit

   procedure Layout
     (D            : Doc;
      Width        : Positive;
      Start_Offset : Natural  := 0;
      Start_Line   : Positive := 1;
      Start_Column : Positive := 1);
      --  Decides each Group's flat-vs-broken form via Text lengths,
      --  and calls Set_Position on every leaf it visits.

end Aquarius.Docs;
```

(`Start_Line` was missing from the first draft — without it every
call would be forced to assume line 1, which is wrong for a subtree
arranged mid-file. `Offset`/`Start_Offset` were added once wiring
`Program_Tree_Type` up as a real `Terminal_Node` turned up that
`Aquarius.Programs` already tracks a third coordinate — a running
character offset — alongside line/column; see below.)

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
(`aquarius-programs.ads:162`) and satisfies the interface as-is.

`Set_Position` is a thin forward to the existing `Update_Location`
(`aquarius-programs.adb:1676-1701`), not a fresh field-writer: that
procedure already derives `End_Offset`/`End_Column` from
`Layout_Length` and bubbles the position up to a not-yet-positioned
parent, and there's no reason to re-derive any of that. It's built
from a `Location_Interface'Class` value via the existing
`Aquarius.Locations.To_Location (Offset, Line, Column)`
(`aquarius-locations.ads:68-72`), which is exactly why `Set_Position`
needed the `Offset` parameter — `Update_Location` requires one.
`Offset` here is a plain running character-count over whatever was
most recently rendered (confirmed against `Arrangement_Context.
Current_Position`, which restarts at 0 on every `Arrange` call) — not
a position in the original source text, so `Aquarius.Docs`'s `Emit`
can compute it as one more counter alongside line/column, no
different in kind from what it already tracks.

This restores full standalone testability for `Aquarius.Docs`: tests
can define a trivial test-double type implementing `Terminal_Node`
(a string field plus captured `Offset`/`Line`/`Column`) with no
grammar, parser, or `Program_Tree` involved at all.

**Fourth correction, found while actually building `Doc`s from real
`Aquarius.Formats` rules (see below):** there was no way to represent
a plain, non-breaking space between two real terminals. `Leaf`
requires a real `Terminal_Node`, and `Line` is *conditionally*
breakable — neither is "exactly one space, never a line break," which
is what `Space_Always` (mandatory inter-token spacing, unrelated to
line-breaking) needs. Added `Space` alongside `Line`/`Break`: always
one column/offset, regardless of the enclosing `Group`'s flat-or-broken
mode — the one primitive that ignores ambient mode entirely.

### Mapping the existing vocabulary onto the algebra

Confirmed against the real consumers (`Aquarius.Programs.Arrangements`,
`Arrangements.Reformatting` — the *only* consumers anywhere in the
repo; nothing in the GTK UI, `.aqua` runtime, or doc tooling reads
`Aquarius.Formats` directly):

| Current (`Aquarius.Formats`)              | Becomes                                   |
|---|---|
| `Space_Always` / `Space_Never` / `Space_Sometimes` (+ `Rule_Priority`) | adjacency choice between two `Leaf`s — unchanged, no breaking involved. `Rule_Priority` **stays** for this — it turned out to have a real, live job here, just never the line-breaking job its own doc comments claimed (confirmed: `Re_Arrange`'s break-candidate selection never actually reads it). |
| `New_Line (Position)`                     | `Break` inserted at that point in the concatenation |
| `Soft_New_Line (Position)`                | `Line` inserted at that point, inside the nearest enclosing `Group` |
| a separator (e.g. `,`) with a plain `Space_After` and *no* explicit soft-line rule | **also** gets a `Line` after it — `Reformatting.Breakable_Separator` treats a trailing space alone as breakable (confirmed: Wir's own `,` format is `no_space_before space_after`, no soft-line marker at all, and still wraps today), so the `Doc`-builder has to replicate that predicate, not just look for an explicit soft-line flag |
| `Indent_Child (Offset)`                   | `Nest (Offset, ...)` wrapped around the children |
| `Closing`                                 | confirmed real and exercised (forces e.g. `then`/`is`/`do` onto its own line when the construct it closes is reformatted) but fully subsumed by `Group`: a `Line` immediately before that terminal, inside the *same* `Group` as the rest of the construct, gets exactly the same effect for free — when that `Group` breaks, every `Line` in it breaks, not just one hand-picked candidate the way `Re_Arrange`'s backward walk works today |
| `Opening`                                 | confirmed **dead** — `Immediate_Rules.Opening` is declared but never read anywhere. Dropped, not migrated. |
| `Governed_By_Content_Soft` (operator-expression break governance, `reformatting.adb:45-83`) | not ported (yet) — no `Doc`-builder support for it exists. Confirmed irrelevant for the Wir pilot specifically (no `*_operator` node in `wir.ebnf` carries any format rule at all), but this is a real gap if the approach later extends to Ada/Pascal, where operators do carry soft-line rules. |

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
triggers `Reformatting.Reformat`'s backward patch. The replacement:

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

**Landed as a separate entry point, not a swap.** `Arrange_Via_Docs`
(new, `aquarius-programs-arrangements.ads`/`.adb`) builds a `Doc` via a
new `Doc_Builder` child package and calls `Layout`; the existing
`Arrange`/`Arrange_Terminal`/`Arrange_Non_Terminal`/`Reformatting` are
**not modified at all**. Only code that explicitly calls
`Arrange_Via_Docs` — currently just an internal Wir-only test harness
(`src/aquarius-tests-wir.adb`) — exercises the new path; every other
grammar keeps using the old engine exactly as before. This makes
"pilot on one grammar" zero-risk rather than merely low-risk, at the
cost of the two engines coexisting until the pilot is trusted enough
to extend or replace the old one.

**List-wrapping is a known simplification for now.** `Reformatting.
Reformat` converts exactly one separator per call and relies on being
re-invoked as arrangement continues — the net effect is "pack as many
items per line as fit" (Wadler's `Fill` combinator), not all-or-nothing
breaking. `Aquarius.Docs` doesn't have `Fill` yet, so the `Doc`-builder
wraps separator-lists in a plain `Group` instead: a list either stays
flat or breaks to one item per line. Adding `Fill` (and switching lists
to it) is deferred, not designed yet.

**Two bugs found and fixed while getting the Wir pilot's actual output
right** (both would recur in any future `Doc_Builder`-style tree fold,
worth knowing before extending this):

1. **EBNF optionals that matched zero times still appear as a child.**
   An absent `['public']`/`[content]`/`[else ...]` produces a real
   node with no content of its own — treating it as an ordinary
   neighbour for spacing purposes double-counts a separator on *both*
   sides of it (`routine  foo` — two spaces — instead of one; a
   spurious space before a following `,`/`)`; a phantom blank line
   where an absent `else` branch would have gone). Fix: check
   recursively whether a child's subtree contains any non-empty
   terminal at all before letting it participate in a separator
   decision or become the new "previous sibling."
2. **A hard `Break` as the first thing inside a `Group` makes that
   group's own fits check trivially succeed.** `Fits` treats reaching
   a `Break` as "the rest doesn't matter, whatever follows starts a
   fresh line" and returns `True` immediately — reasonable in general,
   but if the break is the *entry point* into an indented block (e.g.
   `is` before `statement_list`), folding it inside that block's own
   `Group` (to get the indent right, per the point above) makes the
   group's fits-check exit before ever measuring its real content,
   forcing it — and everything nested inside it — flat regardless of
   actual width. Fix: keep the leading separator inside the block's
   `Nest` (so it gets the right indent when it fires) but *outside*
   its `Group` (so it can't short-circuit that group's own decision).
   Caught by comparing an isolated `Aquarius.Docs`-only unit test
   (nested group correctly re-breaks on its own merits, passed) against
   the real Wir output (didn't) — the discrepancy pointed straight at
   `Doc_Builder`'s tree shape rather than the layout algorithm itself.

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

## Delivery steps

1. **Done.** `Aquarius.Docs`: the algebra + `Layout`, unit-tested
   standalone against a throwaway `Terminal_Node` test double — no
   grammar, parser, or `Program_Tree` involved.
2. **Done.** `Terminal_Node` added as a progenitor of `Program_Tree_Type`,
   with a `Set_Position` override forwarding to the existing
   `Update_Location`.
3. **Done, scoped to Wir.** `Doc_Builder` (new child package) folds a
   `Program_Tree` node + its `Aquarius_Format` rules into a `Doc`; a new
   `Arrange_Via_Docs` entry point calls it and `Layout`. Existing
   `Arrange`/`Reformatting` untouched — this is a pilot living alongside
   the old engine, not a replacement of it, verified via an internal
   `--self-test` harness (`src/aquarius-tests-wir.adb`), not yet wired
   into the GTK `Source_View` or any other grammar.
4. Diff rendered output against existing Pascal/Wir test fixtures before
   extending `Arrange_Via_Docs` to any other grammar.
5. Design and add `Fill` to `Aquarius.Docs` if/when list-wrapping
   fidelity (pack-as-many-per-line, not all-or-nothing) is needed —
   deferred out of the Wir pilot.
6. Port `Governed_By_Content_Soft` (operator-expression break
   governance) if/when this extends to a grammar whose operators carry
   soft-line rules (Ada/Pascal do; Wir doesn't).
7. Revisit incremental re-layout only if full-rebuild `Arrange` proves
   too slow for GTK live editing in practice.
