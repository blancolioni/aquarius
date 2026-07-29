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

Requires cross-repo work.

1. **Frontend passes Content/size.** `Begin_Routine` (and the operand
   accessors) must learn which arg/local/result slots are doubles. Options:
   (a) pass per-slot Content vectors from the aquarius Tagatha binding;
   (b) conservatively pair every frame slot (wasteful); (c) a size table.
   Recommended: (a).
2. **Frame slotting.** `First_Arg/Result/Local/Temp` bounds must advance by 2
   for each float slot; the `Index → register` maps
   (`First_Arg + Index - 1`) become size-aware prefix sums. With D2 these are
   *plain* prefix sums — no parity rounding and no padding slots.
3. **Call / return.** `Call` copies a double actual into a pair; `Exit_Routine`
   moves double results through pairs. Nothing to align: a pair is valid at
   whatever offset the prefix sum puts it, on either side of the `pushj`
   window shift.

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
- Unit-level: assemble the emitted text and check opcode bytes (as done for
  `aqua_as`), then run under the VM and read back the result pair.
- End-to-end: a small routine `double f(double a, double b) { return a*b + a; }`
  once Phase B lands; for Phase A, an expression that loads two doubles from
  memory, computes, and stores the result.
- Reuse the aqua VM's float behaviour already validated by the throwaway spike
  (const round-trip, FLOT lossless, FCMP).
