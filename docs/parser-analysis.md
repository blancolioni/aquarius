# Parser analysis — `aquarius-programs-parser.adb`

Analysis of `Aquarius.Programs.Parser` body
([aquarius_programs/src/aquarius-programs-parser.adb](../aquarius_programs/src/aquarius-programs-parser.adb),
spec [.ads](../aquarius_programs/src/aquarius-programs-parser.ads)).
Snapshot: 2026-07-26.

## What it is

Grammar-directed, top-down **incremental** parser. No parse tables. Builds
`Program_Tree` directly, driven by a **cursor** walking a pre-shaped tree plus a
**list of live parse states** (`Ambiguity`) that explore local ambiguity in
parallel.

## Core model

- **`Parseable`** (:19) — variant record: a token, or a subtree to splice in.
  One drive path serves both.
- **`Ambiguity`** (.ads:107) — one live parse state: `Location` cursor,
  `Parent`, `Top`, `Right`, `Previous` (link to the state it forked from),
  `Active`.
- **`Parse_Token`** (list version, :1120) — walk all ambiguities back-to-front.
  `Token_OK` true → advance; false → delete state. `Count = 0` →
  `raise Constraint_Error "no parses"`.
- **`Parse_Token`** (single, :1223) — main dispatch: repeater / off-right /
  terminal / choice / new-children. Recurses.
- **`Update_Ambiguities`** (:1947) — collapse: a parent with one surviving
  state → resolve, splice `Top`, run deferred parse actions.
- Free-list recycle (`Free_Ambiguity_List`) + `Ambiguity_Counters` are
  **package-level globals**.

Rough cost `O(tokens × active_states × tree_depth)` — `Token_OK` walks up to
root on each miss (:1864).

## Issues — ranked

1. **Error strategy incoherent.** Syntax-valid-lexeme-wrong-place → hard
   `Constraint_Error` (:863), `"no parses"` (:1176), `Program_Error` (:1511).
   Only **lexical** errors produce error-nodes (:874). Backtrack/recovery code
   all commented out (:133, .ads:57). Bad for interactive-editor use.

2. **Comment double-attach bug (likely).** `Set_User_Whitespace` loops over
   **all** `Context.Comments` on every terminal and adds them as left siblings
   (:1675). The clear lines are commented out (:1680). Nothing internal clears —
   the same comment re-attaches to each later terminal unless the caller invokes
   `Clear_Comments`. Verify against callers.

3. **Null deref.** `Make_Parseable(From_Tree)`: `First_Token` can return null;
   `Tok.Get_Token` (:384) then dereferences null. No guard.

4. **EOF ambiguity silent.** `Finish_Parse` (:197) walks `First_Element` to
   root with no check for unresolved active states. Ambiguous-at-EOF not
   reported.

5. **Globals not reentrant.** `Free_Ambiguity_List` + `Ambiguity_Counters`
   shared across all parses. Not task-safe; free-list never shrinks
   (high-water leak). Error paths leak trees + states (no cleanup on exception).

6. **Known incompleteness.** `Has_Ambiguous_Optional` (:711) — hardcoded
   two-child nullable lookahead, self-labeled "not very general". Mid-rule
   nullable unhandled (:754).

7. **Two column sources.** `Token_OK` uses `Item.Column`; `Parse_Token`
   snapshots `Context.Column` once (:1237) then recurses while `Update_Location`
   mutates the context. Possible drift on indent checks.

8. **`Has_Ambiguities` vs `Is_Ambiguous`.** Former counts any 2nd element incl.
   inactive (:234) and gates parse-action firing. Inactive leftovers can
   suppress actions. Subtle, documented (:28), but fragile.

9. **Dead code.** `Interactive` field stored, never read. ~160 lines commented
   cruft (:888-1046, :1076, :133). `Add_Comment` `Location` param unreferenced
   (:151).

10. **Deep mutual recursion**, no memoization/depth guard
    (`Parse_Token` ↔ `Token_OK` ↔ `Parse_Into_*`). Stack risk on large or
    pathological input.

## Verdict

Clever cursor + parallel-state design; handles indentation and incremental
splice well. Weak spots: **error recovery (none)**, **comment handling
(probable bug)**, **null guard**, **dead cruft**. Chase first: #2 and #3.
