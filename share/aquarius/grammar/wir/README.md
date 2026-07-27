# WIR — Word Intermediate Representation

WIR is Aquarius's low-level imperative intermediate representation. It is the
textual surface syntax for the `Ast.*` word-IR node classes, with (broadly) one
grammar production per node class. A WIR file is parsed by `wir.ebnf`, walked
bottom-up by the `generate` action group (visitor classes in `aqua/`), which
rebuilds the `Ast.*` tree as synthesized attributes and then drives a
`Tagatha.Code` object to emit machine code for a target device (e.g. PDP-11).

WIR sits between the high-level program tree and the target-specific code
generator. It is deliberately small and target-agnostic: type and ABI decisions
are pushed down into the Tagatha device rather than being encoded in the IR.

## Files

- `wir.ebnf` — the grammar (parsing).
- `wir.json` — action-group configuration.
- `wir.md` — additional grammar notes.
- `aqua/` — the `generate` visitor classes (parse tree → `Ast.*` → Tagatha).

## Running

- `bin/aquarius --check <f>.wir` — parse and validate only, no code emitted.
- `bin/aquarius --code-trigger <f>.wir` — emit target code (writes
  `tagatha.pdp11` in the current directory).

## Language sketch

A unit is a sequence of routines followed by data items:

```
routine public main
   args 0
   locals 1
is
   local 1 := ( arg 0 + 1 );
   return local 1;
end
```

- **Values are machine words.** The only type annotation is `: float` on an
  expression; everything else is an untyped word.
- **Storage is indexed slots**, all 1-based: `arg N`, `local N`, `result N`.
  (`result N` is write-only.)
- **Memory** is a single deref form: `[ expr + offset ]`.
- **Control flow is hybrid**: structured (`if/then/else/end if`, `loop/do/end
  loop`, `block`, `begin/end` sequence) *and* unstructured (`label`, `goto`,
  `jump`).
- **Exceptions are native**: `fail`, `raise expr`, `retry`, `eval`.
- **Calls**: `call name(...)`, `name(...)` as an expression, and `*expr(...)`
  for indirect calls.
- **Data**: `data`/`word`/`ref`/`text`, with an optional per-item `rw` scope.

## Comparison with C-- (Cmm)

C-- (as embodied by GHC's Cmm) is the best-known "portable assembly" IR, so it
is a useful reference point. Both WIR and C-- are target-agnostic imperative
back-end IRs with a C-like core (assignment, calls, branches) and both have a
textual form. They differ sharply in altitude and scope.

| Axis | WIR | C-- (Cmm) |
|---|---|---|
| Word model | Untyped words; only a `: float` hint on expressions | Width-typed (`bits8/16/32/64`, `float32/64`); every value carries a type |
| Variables | Indexed slots `arg N` / `local N` / `result N` (1-based) | Named virtual registers (unbounded) plus named machine globals (`Sp`, `Hp`, `R1`, …) |
| Memory | Single deref form `[expr + offset]` | Typed, width-explicit loads/stores `bits64[addr]` |
| Control flow | Hybrid: structured `if`/`loop`/`block` **and** `goto`/`jump` | Pure basic-block CFG; all structure already flattened to labels + branches |
| Exceptions | Native: `fail`, `raise`, `retry`, `eval` | None; exceptions are runtime library + explicit control flow |
| Calls | `call`, expression call, `*expr(...)` indirect | `call`/`jump`, `foreign import` C calls, explicit calling/return conventions |
| Data | `data`/`word`/`ref`/`text`, per-item `rw` | Rich static data, closures, info tables, section directives (GC-aware) |
| Runtime coupling | None baked in; ABI decided by the Tagatha device | Tightly bound to GHC's STG runtime (heap/stack pointers, GC safe points, info tables) |
| Scope | Small (~30 node kinds), one production per node | Large, mature, many extensions (safe foreign calls, prim-ops, stack maps) |

### Key contrasts

1. **Typing.** C-- is a width-typed calculus of machine words. WIR is untyped
   words plus a single `float` flag, deferring type decisions to the Tagatha
   device.
2. **Control-flow altitude.** WIR *retains* structured constructs alongside
   `goto`, so it sits higher than C--, which is a strict CFG of basic blocks
   with all structure already flattened. WIR is closer to "portable structured
   assembly"; C-- is closer to "typed RTL".
3. **Exceptions.** WIR has them as first-class primitives, reflecting its
   Ada-flavoured source semantics. C-- has no exception model at all — it
   provides mechanism only.
4. **Runtime coupling.** Despite its name, C-- is effectively welded to GHC's
   runtime (registers, heap layout, GC, info tables). WIR is runtime-neutral.
5. **Storage model.** WIR uses fixed indexed slots, matching Tagatha's
   stack-based operand model. C-- targets a register allocator with infinite
   virtual registers plus named machine globals.

### Summary

WIR sits **higher and thinner**: structured control flow retained, exceptions
native, untyped words, a tiny node set, runtime-neutral — a clean, typed-later
feed into Tagatha's stack-based device. C-- sits **lower and fatter**:
width-typed, a pure CFG of basic blocks, no exceptions, a large feature surface,
and tightly bound to GHC's runtime. WIR trades C--'s type precision and register
model for structural simplicity and a near 1:1 correspondence with its `Ast.*`
nodes.
