# Tagatha double-precision floating point — codegen plan

Status: proposal (2026-07-28). Scope: the Aqua backend of Tagatha
(`tagatha/src/tagatha-arch-aqua.{ads,adb}`) and the IR that feeds it
(`tagatha-code.*`, `tagatha.ads`, `tagatha-conversions.*`).

Prereqs already landed: the Aqua VM decodes/executes the FP opcode set
(issue blancolioni/aqua_vm#3), and `aqua_as` assembles the mnemonics
(`fadd fsub fmul fdiv frem fsqrt fint fcmp feql flot flotu`). A float operand
names the register pair `(R, R + 1)` for **any** `R`; the **lower-numbered
register holds the high 32 bits**, matching the big-endian memory image.

**Alignment update (2026-07-29).** The even-alignment rule has been dropped —
see D2 below for the reasoning. `Get_FR`/`Set_FR` no longer mask the operand
to an even register.

## 1. Where things already work

- IR has the operators: `Op_Fadd, Op_Fsub, Op_Fmul, Op_Fdiv`
  (`tagatha.ads`), the `Floating_Point_Content` operand tag, and
  `Floating_Point_Constant is new Long_Float` (a true 64-bit double).
- `Constant_Operand (Floating_Point_Constant)` stores the exact IEEE-754
  binary64 bit pattern in its `Word` via
  `Conversions.Floating_Point_To_Word_64` (unchecked conversion of
  `Long_Float`). The bits are correct 64-bit doubles.
- Compile-time folding of float constant expressions works
  (`tagatha-code.adb` ~line 779).
- The Aqua backend's `Transfer` already maps `Op_Fadd => "fadd"` etc. and, for
  a binary op, emits `<op> Dst, Src_1, Src_2`.

So a float add *already produces* `fadd %d, %s1, %s2`. The problem is that
every operand is a single 32-bit register.

## 2. The core mismatch

The Aqua backend assumes **one operand == one 32-bit register**:

- `Register_Index` is one value per operand
  (`tagatha-arch-aqua.ads`).
- `Claim` hands out single registers.
- Constant `Move_To_Register` loads only 32 bits (`setl`/`inch`/`seth`).
- `ld`/`st` move 32 bits.
- Args/locals/results are laid out one register per index
  (`First_Arg + Index - 1`, etc.).

A double needs a **register pair** (8 bytes). Emitting `fadd %d,%s1,%s2`
against single registers today would operate on truncated / garbage bits —
the VM reads `(%R, %R+1)`.

## 3. Gap list

| # | Area | Current | Needed |
|---|------|---------|--------|
| 1 | Register model | 1 reg / operand | float operand = reg `R`; `R+1` = low word |
| 2 | Allocator | `Claim` → single reg | `Claim_Pair` → any `R`, `R+1` reserved |
| 3 | Const load | 32-bit `setl/inch/seth` | split Word_64: hi→`R`, lo→`R+1` |
| 4 | Load / store | one `ld`/`st` | double = two `ld`/`st` (hi at lower address) |
| 5 | Compare | float falls into `sub`+`zs*` (wrong) | `fcmp`/`feql` when Content = Floating |
| 6 | Negate | integer `neg` | xor high word sign bit `0x80000000` |
| 7 | Convert int↔float | no path | `flot` / `fint`, triggered by Content mismatch |
| 8 | Calling convention / frame | 1 slot / arg,local,result | float takes 2 slots; `Begin_Routine` has no Content |
| 9 | Data emission | one `.word` per Word_64 | double literal = two words, hi then lo |

## 4. Design decisions

- **D1 — Pair is implicit.** A Floating operand's `R` holds the high word;
  `R+1` is implicitly its low word. `Image` stays `%R`. This matches the VM
  (`R` names the pair) and keeps `Transfer` almost unchanged. No second
  register field on the operand record.
- **D2 — No even alignment; pairs start anywhere.** `Claim_Pair` returns any
  `R` with both `R` and `R+1` marked claimed. `Release_Pair` frees both.

  The rule was originally even-aligned, by analogy with ARM EABI / SPARC.
  Dropped 2026-07-29, because:

  1. **Nothing in Aqua wants parity.** Real ABIs align 64-bit values because
     the paired load/store *encodes a pair index* (ARM `LDRD`) or because an
     8-byte access must be naturally aligned. Aqua's FPU operand fields are
     full 8-bit register numbers and `Get_R`/`Set_R` resolve
     marginal/local/global per register, so each half is reached
     independently. Parity buys no cycles and no correctness.
  2. **The register window makes it unmaintainable for free.** `pushj %X`
     shifts the callee's numbering down by `X+1`, so a caller-side even pair
     lands wherever the shift puts it: pass `%1, %2, %3` and the callee sees
     `%0, %1, %2`, with the double now at odd `%1`. Keeping alignment means
     computing the outgoing arg layout's parity from the *shift base* and
     inserting a padding register when it is wrong — cost, for no gain.
  3. **It was unenforced and failed silently.** `Get_FR`/`Set_FR` masked the
     operand with `and 16#FE#`, so an odd register quietly aliased to the
     pair below; `aqua_as` did not check either. A codegen slip produced
     plausible wrong numbers instead of an error.

  Consequences, all handled as allocator/assembler rules rather than ABI
  rules:

  - `%255` has no successor. It is not a legal pair operand: `aqua_as`
    rejects it per-position (so `fcmp %255, %y, %z` and `fix %255, %z`, whose
    `X` is an integer, are still fine), and `Get_FR`/`Set_FR` raise
    `Bad_Instruction` rather than wrapping the low word round to `%0`.
    `Claim_Pair` never returns `Last_Register`.
  - A pair must not straddle the local/global boundary `rG` — the high word
    would sit in the register window and the low word in the global file.
    Even alignment never protected this either (`rG` can be odd).
  - Window wraparound is safe: `To_Window_Index` applies the modulo per
    register, so a pair spanning the wrap still resolves to the correct two
    logical registers.
- **D3 — Conversion via Content mismatch on `Op_Identity`.** When an identity
  transfer has dst Content = Floating and src Content = General, emit `flot`
  (signed) / `flotu` (unsigned); the reverse emits `fix` (`fint` only rounds
  to an integral *double* — it is not an extractor). `fix`/`fixu` were added
  to the VM and assembler for this (opcodes 05/07, MMIX numbering; round to
  nearest, saturate on overflow, NaN → 0). No new IR operators. (If
  unsigned-source distinction is needed later, thread it through operand
  Content or add `Op_Flt/Op_Fix`.)
- **D4 — Negate by sign flip.** No `fneg` opcode. Negate a double by xor-ing
  the high word (`%R`) with `0x80000000`. Cheaper than materialising a zero
  double and `fsub`. Needs one scratch reg for the mask (or a `setl/seth`
  into a temp).
- **D6 — Multi-register returns are rotated by `Exit_Routine`.** `pop n` moves
  exactly **one** register into the caller's hole — the callee's `%(n-1)` — and
  the other `n-1` become visible only because the register-window base shifts
  down by one. So after `pushj %H` the caller sees

  | caller | gets |
  |---|---|
  | `%H` | callee `%(n-1)` |
  | `%(H+k)`, k in 1 .. n-1 | callee `%(k-1)` |

  The callee's `%0 .. %(n-1)` therefore arrive **rotated left by one**. For a
  one-register result the rotation is the identity, which is why it went
  unnoticed for years; for a double it hands the caller the low word at `%H`
  with the high word above — reversed, and not a usable `(R, R+1)` pair.

  Fixed on the callee side, in `Exit_Routine`: word 0 goes to `%(Width - 1)`
  and word *w* to `%(w - 1)`, so the caller sees the words in order and
  contiguous from `%H` — which is what `Call`'s `Return_Reg` prefix sum
  assumes. Chosen over changing the VM because `pop` is MMIX's semantics and
  Tagatha is the layer that conforms to an ABI. Word 0 is stashed in a claimed
  scratch first, because the shift overwrites it when the result region starts
  at `%0` (a routine with no arguments). `Width <= 1` emits exactly the same
  single `set` as before, so all-integer output is byte-identical.

  This also depends on the callee's `rL` exceeding `Width`: `pop n` fetches the
  hole value with `Get_R (n)`, which returns 0 for a *marginal* register when
  `rL = n` exactly. Linkage guarantees it — `Saved_J` is claimed at or above
  `First_Temp >= Result_Bound` — and `Exit_Routine` only emits a `pop` when
  Linkage is set. Worth remembering before hand-writing a `pop n` in asm.

- **D5 — Big-endian pair/data order everywhere.** Hi word first, at the lower
  register number and the lower memory address. Fixed once; must agree with
  the VM and `aqua_as`. This is endianness, not alignment, and is unaffected
  by D2. Memory-side alignment is also a separate question: doubles move as
  two 32-bit `ld`/`st`, so 4-byte is all that is required today; revisit only
  if a 64-bit `ldd`/`std` is ever added.

## 5. Phasing

### Phase A — MVP: floats in temporaries and expressions

Stays entirely inside `tagatha`; no frontend changes. Delivers double
arithmetic at expression level.

1. **Allocator (gap 2).** `Claim_Pair`/`Release_Pair`. First-fit scan of
   `First_Temp .. Last_Register - 1` for an `R` with `R` and `R+1` both free.
2. **Temporary operands (gap 1).** When `Temporary_Operand` has
   `Floating_Point_Content`, back it with a pair; its `R` is the high word.
3. **Constant load (gap 3).** Override for `Constant_Operand` with Floating
   Content: `hi := Value / 2**32; lo := Value mod 2**32`, load hi→`R`,
   lo→`R+1` (each via the existing 32-bit `setl/inch/seth` sequence).
4. **Arithmetic (gap 1 wiring).** `Op_Fadd..Op_Fdiv` already emit the right
   mnemonic; just ensure operands are pair-backed. `frem` maps from `Op_Mod`
   when Content = Floating (or add `Op_Frem`).
5. **Compare (gap 5).** In `Transfer`, when `Compare_Operator` and operands
   are Floating: emit `fcmp %t, %y, %z` (or `feql` for `Op_EQ`/`Op_NE`),
   then map the −1/0/1 result to a boolean with the existing `zs*` ops.
6. **Negate (gap 6).** `Op_Negate` with Floating Content → xor sign bit.
7. **Convert (gap 7).** `Op_Identity` Content-mismatch → `flot`/`fint`.
8. **Deref / store of doubles (gap 4).** `Op_Dereference`/`Op_Store` with
   Floating Content → two `ld`/`st` at `offset` and `offset+4`
   (hi at the lower address).
9. **Data (gap 9).** Float datum in `.data` → two `.word`s (hi, lo).

Out of scope for A: float **args, locals, results** living in the register
frame. Expressions can still read/write doubles that live in memory
(via deref/store), so real code that keeps doubles in memory works.

### Phase B — Full: calling convention and frame slots

**Done 2026-07-29** (tagatha + aquarius). Option (a) throughout: per-slot
Content travels with `Begin_Routine`.

1. **Frontend declares Content.** `Tagatha.Frame_Layout` carries an
   `Operand_Content` per argument, result and local slot;
   `Arch.Begin_Routine` takes one. The frontend declares argument and result
   contents through `Routine_Options`
   (`Set_Argument_Content` / `Set_Result_Content`), and `Tagatha.Code.Update`
   *widens* each slot from the accesses it sees, so a declaration is never
   narrowed and an undeclared slot accessed as a double is still promoted.

   Declaration is required, not merely helpful: caller and callee must agree
   on the layout, and a float argument that the body never touches would be
   sized as one word by inference alone while the caller passes two. Locals
   need no declaration — they are routine-private, and `Add_Local` hands them
   out dynamically, so inference is both sound and necessary there.

   Returns are the mirror image: the register of return *N* depends on the
   widths of returns 1 .. N−1, so `Tagatha.Code.Call` / `Indirect_Call` take a
   `Return_Content_Array`, defaulting to all-general (correct whenever no
   return before the last is a double).

2. **Frame slotting.** `Begin_Routine` walks arguments, results and locals in
   turn, laying each slot at the running total and advancing by
   `Slot_Width (Content)` — 2 for a double, 1 otherwise. The index → register
   maps (`Arg_Reg`, `Result_Reg`, `Local_Reg`) are those prefix sums;
   `First_Arg/Arg_Bound/…/First_Temp` fall out of the same walk. All-general
   frames come out identical to the old `First_X + Index - 1`.

3. **Call / return.** `Call` advances the outgoing argument register by
   `Slot_Width` per actual, using the actual operand's own Content, so a
   double actual fills a pair (`Move_To_Register` already copies both halves).
   It also builds `Return_Reg` as the prefix sum over `Returns`, based at
   `Call_Return`.

   `Exit_Routine` **does** need a change, and it is the subtle part of B —
   see D6 below. It was initially believed to need none.

4. **Device protocol.** The aquarius Tagatha binding gained commands 47/48/49
   (`Set_Arg_Content`, `Set_Res_Content`, `Set_Ret_Content`), which accumulate
   on the device and are consumed and cleared by the next `Begin_Routine` or
   `Call`. Exposed to Aqua code as `Tagatha.Code.Set_Argument_Content`,
   `Set_Result_Content` and `Set_Return_Content`.

Still open after B:

- **`ack` has no floating point type**, so nothing in the aquarius frontend
  declares a float slot yet. The capability is in place on both sides of the
  binding; the language work is separate.
- **Storing a double to a *named* object** still raises
  "aqua: float store to named object not implemented"
  (`Op_Identity` with float source and a non-register destination). That is
  Phase A gap 4 territory — a global double, not a frame slot — and wants
  `geta` plus two `st`.
- **`fixu` / `flotu` signedness** remains unthreaded (see D3).

## 6. Code touch-points

- `tagatha-arch-aqua.ads`: `Claim_Pair`/`Release_Pair` decls; possibly a
  helper `Is_Float (Operand)`.
- `tagatha-arch-aqua.adb`: `Claim`/`Release` pair variants; `Move_To_Register`
  override for Floating constants; `Transfer` branches for compare / negate /
  convert / float deref+store; `Put_Data_Buffer` for double data.
- `tagatha.ads` / `tagatha-code.*`: only if we decide to add explicit
  `Op_Frem` / `Op_Flt` / `Op_Fix` instead of overloading existing ops; and
  (Phase B) Content/size on `Begin_Routine`.
- `tagatha-code.adb:377`: the `Bits => 32` on a float constant is misleading
  for doubles — audit whether anything downstream keys off it (data emission
  width in particular).

## 7. Risks / watch-list

- **~~Even alignment under register pressure.~~** Gone with D2: any two
  consecutive free registers will do, so single-register claims can no longer
  strand a pair. Fragmentation is now the ordinary kind — first-fit is fine.
- **`%255` as a pair base.** Rejected at three levels (`Claim_Pair` bound,
  `aqua_as` per-position check, `Get_FR`/`Set_FR` raise). Note `%255` also
  carries the halt-message address for `trap 0,0,0`, so it is a poor float
  register for unrelated reasons.
- **`Bits => 32` on float constants.** Confirm data-segment width for doubles
  is driven by Content, not this field, or fix the field.
- **`frem` semantics.** VM `FREM` uses IEEE `'Remainder` (round-to-nearest
  quotient), not C `fmod`. Make sure the language front-end expects that, or
  add a separate op.
- **No `fneg`/no unordered compare.** Sign-flip negate is fine; NaN-unordered
  compare has no opcode (VM maps NaN → 0 in `fcmp`). If the source language
  needs unordered detection, that is a new VM opcode first.
- **Convert signedness.** `flot` vs `flotu` (and `fix` vs `fixu`) needs the
  source's signedness; `Op_Identity` Content mismatch loses it unless we
  thread it. Signed is the default today.

## 8. Testing

- **Landed (2026-07-29), alignment removal.** `Aqua.Tests.Test`
  (`aqua_vm/src/aqua-tests.adb`, run by `aqua_vm` with no arguments) asserts
  `fadd` over odd, odd-with-non-zero-low-word, and mixed-parity pair bases,
  that a `%255` pair operand raises `Bad_Instruction`, and that
  `fcmp %255, %y, %z` still works. Verified discriminating: with the old
  `and 16#FE#` mask restored, the first case fails. `aqua_as` rejection
  checked by hand for `fadd %255,…` in X and Y, `fsqrt`/`fix` in Z and
  `flot %255,…` in X, with `fcmp %255` / `fix %255` / `flot %5, %255`
  assembling clean.
- **Landed (2026-07-29), Phase B.** `tagatha/tests` generates
  `double scale (double x, int n, double y)` with a double local and a double
  result, plus a caller `call_scale` doing `scale (1.5, 7, 2.0)`, and asserts
  the register layout of both: `x` at `%0/%1`, the *integer* `n` displaced to
  `%2`, `y` at `%3/%4`, result at `%5/%6`, local at `%7/%8`; on the caller
  side the actuals at `%4/%5`, `%6`, `%7/%8` and the double return read from
  `%3`. A third routine `pick (double unused, int n)` pins the
  declared-but-unread case — `n` must still be `%2`. `next_3x_1` guards
  against regression in all-general frames.

  Run under the VM with either fixture in `tagatha/tests/share`
  (`aqua_as -m -o t.o <fixture>.s float_frame.s`, then `aqua_vm t.o` from a
  directory containing a `.aqua-config`):

  - `float_frame_direct.s` — main calls `scale` **directly**. Prints `P` when
    the returned pair is bit-exactly 4.5. **This is the one that
    discriminates**: it is the only fixture that catches D6.
  - `float_frame_main.s` — main calls `call_scale`, which calls `scale`. Prints
    `P5`. Two call levels, so it passes *whether or not* D6 is fixed: two
    rotations of a two-element sequence cancel. It was the original Phase B
    test and gave a **false pass** until `float_frame_direct.s` was added.

  Note `artl.s` ends with a bare `main` label, so a fixture is the *body* of
  main, and must save/restore `rJ` around its own `pushj` in a register below
  the `pushj` boundary.

  Regression: the 54-test Aqua suite
  (`bin/aquarius --start-class share/aquarius/tests/aqua/test.aqua`) still
  passes, confirming prefix sums are a no-op for all-general frames.
- Unit-level: assemble the emitted text and check opcode bytes (as done for
  `aqua_as`), then run under the VM and read back the result pair.
- End-to-end for Phase A: an expression that loads two doubles from
  memory, computes, and stores the result.
- Reuse the aqua VM's float behaviour already validated by the throwaway spike
  (const round-trip, FLOT lossless, FCMP).
