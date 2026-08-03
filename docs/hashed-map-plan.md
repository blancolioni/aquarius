# A hash-table container for aqua

**Proposal.** Fill in `Aqua.Containers.Hashed_Map`'s deferred stub with a real
separate-chaining hash table — but ship it first as a concrete
`Aqua.Containers.Hashed_String_Map [Element_Type]`, not the fully generic
`Hashed_Map [Key_Type, Element_Type]`, for reasons below. The immediate payoff
is `Pascal.Checks.Scope`'s symbol table, which today linear-scans a
`Linked_List` of bindings on every lookup.

## Why not the generic version yet

A hash table needs to hash `Key_Type`. Two ways to get that, and both are
blocked in this codebase today:

- **Constrained generics** (`Key_Type -> Aqua.Hashable`). The grammar parses
  the `->` constraint
  ([aqua.ebnf:308](../share/aquarius/grammar/aqua/aqua.ebnf)), but there is no
  semantic handling of it anywhere in
  [aquarius_ack/src](../aquarius_ack/src) — it parses and is then ignored.
  Not usable without compiler work.
- **`Key.Hash_Code` inside the generic.** Calling a feature *on* an entity
  whose static type is a formal generic parameter doesn't dispatch — that's
  issue #60, and it's exactly this shape:
  `List_Map [String, Integer].Contains_Key` already fails today because
  `Item.First = K` resolves `=` against the formal's constraint (`Any`)
  instead of dispatching to `String.Equal`.

So a generic `Hashed_Map [Key_Type, Element_Type]` either waits on #60, or
needs a spike confirming that hashing via an *injected object* (receiver
concrete, `Key_Type` only ever appears as an argument, never as a receiver)
dodges the bug. Unverified either way, and not needed yet: `String` keys are
the only real use case in the tree. Build the concrete version now; generalize
only when a second key type actually shows up.

## Representation

Reuse existing containers rather than a new backing store:

```
Buckets : Aqua.Containers.Vector [Aqua.Containers.Linked_List [(String, Element_Type)]]
```

- `Vector` ([aqua-containers-vector.aqua](../share/aquarius/lib/aqua/standard/aqua-containers-vector.aqua))
  already gives a growable, word-backed array; its element contract
  ("word-representable") holds for `Linked_List` references.
- Each bucket is a `Linked_List` of `(String, Element_Type)` tuples — the same
  entry shape `List_Map`
  ([aqua-containers-list_map.aqua](../share/aquarius/lib/aqua/standard/aqua-containers-list_map.aqua))
  already uses. `Contains_Key`/`Element`/`Delete` bodies are near-identical to
  `List_Map`'s, just scanning one short chain instead of the whole table.
- `default_create` pre-populates `Buckets` with `Initial_Bucket_Count` (e.g. 8)
  empty `Linked_List`s.

## Hash function

No bitwise ops are exposed at the aqua-lang level — `system-word_32.aqua` and
`integer.aqua` only expose `Add`/`Subtract`/`Multiply`/`Divide`/`Modulus` plus
comparisons (the VM ALU has `Xor`/shift, per
[aqua_vm/src/aqua-instruction.ads](../aqua_vm/src/aqua-instruction.ads), but
nothing surfaces them to aqua). That's fine — a polynomial rolling hash only
needs `+`, `*`, `mod`, already available:

```
Hash_Code (S : String) : Integer
   local
      Index : Integer
   do
      from
         Index := 1
      until
         Index > S.Length
      loop
         Result := (Result * 31 + S.Element (Index).UTF_32_Code) \\ Large_Prime
         Index := Index + 1
      end
   end
```

(`Character.UTF_32_Code` and `String.Element`/`.Length` are already used this
way, e.g. `Pascal.Checks.Types.Is_Digit`.) Bucket index is
`Hash_Code (Key) \\ Buckets.Count`.

Case sensitivity is the caller's job, not the map's: `Pascal.Checks.Scope`
already does `Name.To_Lower` before comparing. Keep that convention — the map
hashes/compares whatever `String` it's given; callers normalize case before
`Insert`/`Element`/`Contains_Key`, same as today. Don't bake case-folding into
the map itself.

## Growth policy

Mirror `Vector.Grow`: when `Count > Buckets.Count * 3 / 4` (load factor 0.75),
double the bucket count, walk every existing chain, and re-insert each
`(K, E)` into a freshly sized `Buckets`. O(1) amortized, same doubling shape
`Vector` already uses.

## `Map` contract

`Hashed_String_Map [Element_Type]` inherits
`Aqua.Containers.Map [String, Element_Type]`
([aqua-containers-map.aqua](../share/aquarius/lib/aqua/standard/aqua-containers-map.aqua)),
which via `Container [Element_Type]` requires:

| Feature | Source |
|---|---|
| `Is_Empty` | own — `Buckets` all empty |
| `Contains (Element)` | own — scan every bucket |
| `Contains_Key (Key)` | own — scan `Buckets [Hash_Code (Key) \\ Buckets.Count]` |
| `Element (Key)` | own — same bucket scan |
| `Insert (Key, Element)` | own — append to bucket, then maybe `Grow`/rehash |
| `Delete (Key)` | own — scan bucket, `Linked_List_Iterator.Delete` |
| `Replace (Key, Element)` | free, from `Map` (`Delete` + `Insert`) |

## Files

- `share/aquarius/lib/aqua/standard/aqua-containers-hashed_string_map.aqua` —
  new class. Leave the existing generic `Hashed_Map` stub as-is (or note in
  its header that `Hashed_String_Map` is the practical implementation until
  #60 / constrained generics land).
- `share/aquarius/tests/aqua/` — a `Map_Conformance_Test` instantiated over
  `Hashed_String_Map [Integer]`, **plus** its own explicit structural-key test
  (insert two distinct-but-equal `String` objects built from different
  concatenations, confirm lookup hits). This is exactly the case the existing
  conformance test was noted (in issue #60) to *not* cover, and is precisely
  what this class needs to prove it dodges that bug.
- [containers.md](containers.md) — update the hierarchy diagram/notes once
  this lands.

## Follow-on (separate piece of work)

Switching `Pascal.Checks.Scope`'s `Bindings`/`Declarations` linear-scan
`Linked_List`s over to this map is the actual payoff, but it's a second,
smaller change once the container exists and has passed conformance —
tracked separately, not part of this scope.
