# Aqua Container Library

The standard container classes live in
`share/aquarius/lib/aqua/standard/aqua-containers-*.aqua` (namespace
`Aqua.Containers`). This document describes how they fit together.

## Design at a glance

The library is organised around three independent concerns, each with its
own root, kept deliberately separate so no single class has to be everything
at once:

1. **Structure** — what a container *is* and how membership works. Rooted at
   `Container`.
2. **Iteration** — external, `across … loop … end`-style traversal. Rooted at
   `Aqua.Iterable` / `Aqua.Iteration_Cursor` (these live outside the
   `Containers` namespace, in `Aqua`).
3. **Random access** — addressing elements by a contiguous integer index.
   Provided by the rootless `Indexable` protocol mixin.

A concrete container combines the concerns it needs by inheriting from more
than one root (e.g. `Vector` is a `Sequence` *and* an `Indexable`). Because the
iteration and random-access roots are independent of `Container`, mixing them
in never produces a repeated-`Container` (diamond) clash.

## Structural hierarchy

```
Container [G]                     Contains, Is_Empty
├─ Collection [G]                 Can_Delete, Can_Include, Include, Delete,
│  │                              Delete_All, Clear
│  └─ Bag [G]                     Occurrences
│     └─ Sequence [G]             First_Element, Last_Element,
│        │                        Delete_First, Append
│        ├─ Linked_List [G]       (also Iterable) chain of Linkable nodes
│        └─ Vector [Element]      (also Indexable) growable, word-backed
├─ Box [G]                        Is_Full            (abstraction, no heirs)
├─ Traversable [G]                = Container + Iterable  (abstraction, no heirs)
└─ Map [Key, Element]             Contains_Key, Element "[]", Insert,
   │                              Delete, Replace
   ├─ List_Map [Key, Element]     composes a Linked_List of (key, value)
   └─ Hashed_Map [Key, Element]   (deferred; hash-table impl pending)

Array [Element] = Container + Indexable   fixed bounds, no growth (deferred)
```

Notes:

- **`Container`** is the universal root: everything you can ask "does it hold
  this element, and is it empty?" of.
- **`Collection` → `Bag` → `Sequence`** is the "arranged, countable, appendable"
  spine. `Bag` counts occurrences; `Sequence` orders them and appends at the end.
- **`Box`** (bounded / "can become full") and **`Traversable`**
  (an iterable container) are valid abstractions with no current
  implementations — kept as extension points. `Traversable` in particular is
  the natural place to root spine-wide iterability later
  (e.g. `Collection : Traversable`).
- **`Map`** is the associative branch: sparse, arbitrary keys, keyed
  insert/delete. `List_Map` **composes** an internal `Linked_List` rather than
  inheriting it, to avoid inheriting `Container` along two incompatible
  instantiations.
- **`Array`** is *not* a `Map`. A fixed integer range is dense random access,
  not sparse associative storage — its keys are a contiguous `Lower .. Upper`
  band you cannot insert outside of. It is a `Container` plus the `Indexable`
  protocol.
- Both `Map` and `Indexable` provide a bracket accessor `alias "[]"`, but the
  underlying feature is named differently — `Map.Element (Key)` versus
  `Indexable.Item (Index)` — because one is keyed and the other indexed.

## Random-access protocol: `Indexable`

`Indexable [Element_Type]` is a **rootless** deferred mixin — it inherits
nothing, so a class can add it to any structural parent without dragging in a
second `Container`.

| Feature | Kind | Meaning |
|---|---|---|
| `Lower`, `Upper` | deferred | inclusive index bounds |
| `Count` | effective | `Upper - Lower + 1` |
| `Valid_Index (i)` | effective | `i >= Lower and then i <= Upper` |
| `Item alias "[]" (i)` | deferred | element at index `i` |
| `Put (i, v)` | deferred | store `v` at index `i` |

Implementors:

- **`Array`** — fixed bounds, no growth (currently a deferred stub: no backing
  store yet).
- **`Vector`** — growable, 1-based, backed by a raw `System.Memory.Block_32`
  word block. `Lower = 1`, `Upper = Item_Count`; gains `[]` and `Put` from the
  protocol while keeping `Append`/`First_Element`/… from `Sequence`.

## Iteration model

External iteration drives `across … loop … end`. It is rooted in the `Aqua`
namespace, not `Aqua.Containers`.

```
Iterable [E]                      New_Cursor : Iteration_Cursor [E]
Iteration_Cursor [E]              Element, Next, After
├─ Forward_Iterator [E]           + At_Start, At_End
│  └─ Linked_List_Iterator [G]    (also Update_Iteration_Cursor)
├─ Update_Iteration_Cursor [E]    + Delete, Insert
└─ Interval_Iterator [E]
```

- An `Iterable` yields a fresh cursor from `New_Cursor`. The cursor exposes the
  current `Element`, advances with `Next`, and reports end-of-sequence with
  `After`.
- `Forward_Iterator` adds boundary queries (`At_Start` / `At_End`).
  `Update_Iteration_Cursor` adds in-place `Delete` / `Insert`.
- `Linked_List_Iterator` combines both: it walks a `Linked_List` and can mutate
  it mid-traversal.
- `Aqua.Interval` (a range over an `Enumerable` type) is also an `Iterable`,
  producing an `Interval_Iterator`.

There is a **single** cursor family. An earlier parallel family
(`Traversable_Iterator` / `Linear_Iterator` / `Linear`, with `Item` / `Off_End`
names) duplicated this one, had no users, and has been removed.

## Implementation building blocks

```
Cell [G]                          Element, Put           (single-value node)
└─ Linkable [G]                   Next, Put_Next, Join   (singly-linked node)

Linked_List [G] = Iterable + Sequence
                  (First_Cell / Last_Cell : Linkable)
```

- **`Cell`** is a concrete single-value holder. It is an *implementation*
  helper, **not** an abstraction base — only `Linkable` (a value plus a link to
  the next node) inherits it. Earlier, several iterator/iterable classes
  inherited `Cell` purely to reuse its `Element` field; that has been undone.
- **`Linked_List`** is the reference `Sequence`/`Bag` implementation: a chain of
  `Linkable` nodes, iterable via `Linked_List_Iterator`.

## Conformance tests

Deferred contracts are exercised polymorphically by
`*_conformance_test.aqua` in `share/aquarius/tests/aqua/` (a deferred
`Unit_Test` subclass per contract with a factory returning the container typed
as the *deferred* contract). `Linked_List` and `Vector` are bound to the `Bag`
and `Sequence` conformance tests; `List_Map` to the `Map` test. `Vector` also
has a dedicated index test (`test_vector_index`).
