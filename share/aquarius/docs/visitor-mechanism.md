# The Aqua visitor mechanism

Aquarius runs *actions* over a parsed program tree by binding **Aqua classes**
to grammar non-terminals. Each non-terminal you care about gets a class; the
framework instantiates one object per matching tree node and calls hook
features on it as it walks the tree. This is how a grammar attaches
behaviour — semantic checks, code generation, serialization — to its syntax.

This document describes the mechanism itself. For a worked code-generation
example see the `wir` grammar (`grammar/wir/`, especially
`grammar/wir/aqua/`); for a serializer see `grammar/normandy/`; for the
minimal shape see `grammar/oberon/` and `grammar/lox/`.

## 1. Layout: grammars, groups, and classes

A grammar lives in `grammar/<name>/` and has:

- `<name>.ebnf` — the syntax.
- `<name>.json` — metadata, including the **action groups**.
- `aqua/` — the visitor classes (one file per class), present only if the
  grammar has action groups.

A group is declared in `<name>.json`:

```json
{
    "name": "generate",
    "stage": "code",
    ... other groups ...
}
```

The `stage` decides *when* the group runs:

| stage      | trigger              | run by                                   |
|------------|----------------------|------------------------------------------|
| `semantic` | `Semantic_Trigger`   | `bin/aquarius --check <f>` and normal load |
| `code`     | `Code_Trigger`       | `bin/aquarius --code-trigger <f>`        |

A grammar may have several groups (e.g. lox has `checks`/semantic and
`interpret`/code). Each group is an independent pass with its own classes.

## 2. Class ↔ non-terminal binding

A visitor class is named `<Grammar>.<Group>.<Non_Terminal>` and binds to the
grammar rule whose name matches its **final component**:

```
class
   Wir.Generate.Statement_List      -- binds to the `statement_list` rule
```

The file is named to match: `wir-generate-statement_list.aqua`. Casing is
folded (`Statement_List` ↔ `statement_list`).

You only write classes for the non-terminals you need to act on. Rules with no
class simply contribute nothing to the pass.

> **Keyword collision.** The final component becomes an Aqua class name, so it
> cannot be an Aqua keyword. If a rule is named `retry`, `local`, `if`, or
> `loop`, rename the rule (e.g. `retry_statement`) — otherwise the class fails
> to compile with `syntax error at X (expected identifier)`.

## 3. Hook features

The framework calls features on a node object as it visits that node's
subtree. A feature is a hook when its name is `<position>_<child>`:

- **position** — `Before` or `After` (the text before the first underscore).
- **child** — the rest of the name, naming a *direct child* of this node, a
  token class, or the special word `Node`.

### 3.1 Child non-terminal hooks

```
After_Term (Child : Lox.Interpret.Term)
   do
      ... Child.Value ...
   end
```

`After_Term` fires once for each direct `term` child, **after** that child's
entire subtree has been visited. The argument is the child node object, typed
as the child's visitor class, so you can read attributes it has computed (see
§4). `Before_Term` fires *before* the child is visited (rarely needed).

Hooks fire for **direct children only** — a `term` nested three levels down
does not trigger the enclosing node's `After_Term`.

### 3.2 Token hooks

A terminal token class (as declared at the top of the `.ebnf`: `integer`,
`identifier`, `string_constant`, …) produces a hook whose argument is the token
**text** as a `String`:

```
After_Integer (Value : String)
   do
      Offset := Value.To_Integer
   end
```

`After_String_Constant` gives the literal *with* its surrounding quotes — strip
them with `Text.Slice (2, Text.Length - 1)`.

If a node has several children of the same token class, the hook fires once per
occurrence, in source order — count them:

```
--  routine ... args <integer> locals <integer> ...
After_Integer (Value : String)
   do
      if Got_Args then
         Local_Count := Value.To_Integer
      else
         Arg_Count := Value.To_Integer
         Got_Args  := True
      end
   end
```

### 3.3 The whole-node hook: `After_Node`

`After_Node` (child = the special word `node`, no argument) fires **once**,
**after** every child hook of this node. It is the reduce step — the place to
assemble this node's result from the children you have gathered:

```
After_Node
   do
      if attached Nm as N then
         if attached Body as B then
            create Rtn.Make (Current, N, Arg_Count, Local_Count, Is_Public, B)
         end
      end
   end
```

`Before_Node` exists symmetrically (fires before any child).

### 3.4 Firing order

For a node, the framework fires, in order:

1. `Before_Node`
2. for each child, in source order: the child's *entire* subtree, then this
   node's `After_<child>` (or `Before_<child>` before the subtree).
3. `After_Node`

So processing is **bottom-up**: a child has finished (including its own
`After_Node`) before the parent's `After_<child>` sees it. Compute a node's
value in its `After_Node`; read children's values in `After_<child>`.

> **No keyword hooks.** A literal keyword terminal (e.g. `'public'`) is
> registered syntax, so a zero-argument `After_Public` feature will match it
> and then crash the binder (it expects one argument). Never write hooks named
> after literal keywords. To detect an optional keyword, use §6.

## 4. Synthesized attributes

Nodes communicate results **upward** through ordinary public fields
("synthesized attributes"). A child sets a field in its `After_Node`; the
parent reads `Child.<field>` in its `After_<child>`. Fields are public for
read by default, so no accessor is needed.

The common idiom is a small base class that declares the attribute, which every
node of that kind inherits:

```
class
   Wir.Generate.Expr_Node
inherit
   Wir.Generate.Node
feature
   Expr : detachable Ast.Expression
   Set_Expr (E : Ast.Expression)
      do
         Expr := E
      end
end
```

A dispatch rule (`primary ::= literal | argument | ...`) then just copies the
chosen alternative's attribute up:

```
class
   Wir.Generate.Primary
inherit
   Wir.Generate.Expr_Node
feature
   After_Literal (Child : Wir.Generate.Literal)
      do
         if attached Child.Expr as E then
            Set_Expr (E)
         end
      end
   ... one After_ per alternative ...
end
```

## 5. Node identity and tree access

Every node object is a program tree node once it inherits
`Aquarius.Trees.Program_Tree`. Put that in a shared base so every visitor has
tree access:

```
class
   Wir.Generate.Node
inherit
   Aquarius.Trees.Program_Tree
end
```

This gives each node:

- `Concatenated_Image : String` — the node's source text, **tokens glued with
  no separators** (`routinepublicfoo...`). For an *empty* optional node the
  image is the **rule name**, not the empty string.
- `Node_Text`, `Standard_Text`, `Start_Location_Image`.
- `Error (Message : String)` — attach an error message to this node (raises the
  tree's highest message level; a file with an error exits non-zero).
- The object itself conforms to `Aquarius.Trees.Program_Tree`, so you can pass
  `Current` wherever a source-tree reference is wanted (e.g. as the `Src`
  argument to a model constructor).

Calling `Concatenated_Image` requires the class to inherit `Program_Tree`;
otherwise you get `Concatenated_Image not declared`.

## 6. Optional keywords and flags

Because you cannot hook a keyword directly (§3.4), wrap an optional keyword in
its own non-terminal and read that node's image:

```ebnf
routine ::= 'routine' routine_scope name ...
routine_scope ::= [ 'public' ]
```

```
After_Routine_Scope (Child : Wir.Generate.Routine_Scope)
   do
      if Child.Concatenated_Image = "public" then
         Is_Public := True
      end
   end
```

The wrapper node **always** exists (it is a mandatory child), and its
`After_Node` always fires. When the keyword is present the image is `"public"`;
when absent it is the rule name (`"routine_scope"`), so test for the positive
value. The wrapper class itself can be an empty stub inheriting the tree base.

## 7. Lists and repetition

For `{ x }` / `< x / ',' >`, the per-child hook fires once per element in order.
Accumulate them:

```
class
   Wir.Generate.Argument_List
inherit
   Wir.Generate.Node
feature
   Args : Aqua.Containers.Linked_List [Ast.Expression]   -- auto-created
   After_Expression (Child : Wir.Generate.Expression)
      do
         if attached Child.Expr as E then
            Args.Append (E)
         end
      end
end
```

> **Self-initialization.** This is standard Eiffel: an attached attribute of a
> self-initializing type (one with a default, argumentless creation procedure,
> like `Linked_List`) is created for you on first use — no `create` needed. A
> type whose creation takes arguments (a mandatory `Make`) is not
> self-initializing, so it must be created explicitly, e.g. with a lazy getter:
>
> ```
> Held : detachable Ast.Statement.Sequence
> The_Sequence : Ast.Statement.Sequence
>    do
>       if attached Held as S then
>          Result := S
>       else
>          create Result.Make (Void, Current)
>          Held := Result
>       end
>    end
> ```

## 8. Running a pass

- **Semantic group** (`stage: semantic`): runs on `bin/aquarius --check <f>`
  and on a normal load. Report problems with `Error (...)`; a message above
  `Warning` sets a non-zero exit status.
- **Code group** (`stage: code`): runs on `bin/aquarius --code-trigger <f>`.
  Side effects (writing a file, driving a device) belong in the top node's
  `After_Node`.

Both are dispatched by `Grammar.Run_Action_Trigger`. On Windows the executable
links GtkAda, so the MSYS2 GTK DLLs must be on `PATH` even for `--check`.

## 9. Worked example: two hooks and a reduce

```ebnf
binary ::= '(' expression binary_operator expression ')'
```

```
class
   Wir.Generate.Binary
inherit
   Wir.Generate.Expr_Node
feature

   After_Expression (Child : Wir.Generate.Expression)
      do
         if attached Left then          -- second expression = right operand
            Right := Child.Expr
         else                            -- first  expression = left operand
            Left := Child.Expr
         end
      end

   After_Binary_Operator (Child : Wir.Generate.Binary_Operator)
      do
         Op := Child.Op
      end

   After_Node
      do
         if attached Left as L then
            if attached Right as R then
               create B.Make (Void, Current, Op, L, R)
               Set_Expr (B)
            end
         end
      end

feature { None }
   Left  : detachable Ast.Expression
   Right : detachable Ast.Expression
   Op    : Integer
   B     : detachable Ast.Expression.Binary
end
```

The two `expression` children arrive left-then-right (source order); the
operator arrives between them; `After_Node` reduces all three into one node.

## 10. Aqua bugs and gotchas that bite in visitors

These are Aqua toolchain bugs, not properties of the visitor mechanism. Listed
here because you *will* hit them writing visitor code, with the workaround for
each.

- **Chained `attached … as` fails to parse
  ([#41](https://github.com/blancolioni/aquarius/issues/41)).** `if attached X
  as A and then attached Y as B then` gives `syntax error at and`. This is valid
  standard Eiffel — an object-test local is in scope across `and then` — so the
  parser is at fault, not the language. Workaround: nest the object tests (see
  §9).
- **Integer negation is awkward
  ([#19](https://github.com/blancolioni/aquarius/issues/19)).** `- x` and
  `0 - x` fail to type (`Any with no type` / `undeclared: -`). Declare a
  `Zero : Integer` local (self-initializes to 0) and write `Zero - x`. Avoid
  `.Negate` on a chained call — it can crash the compiler.
- **`To_Integer` ignores sign
  ([#40](https://github.com/blancolioni/aquarius/issues/40)).** It parses
  digits only; a leading `-` yields garbage. Detect the sign yourself:
  `if Image.Element (1) = '-' then ...`.
- **Cold-cache compile can fail (bug, no issue filed yet).** The first compile
  of a new group in a cleared cache can fail with `predefined class 'string'
  not found` while `generating any`. It is transient — run any other grammar
  once (or just re-run) to warm the standard-library objects, then it compiles.

## 11. Checklist for a new pass

1. Add the group to `<name>.json` with the right `stage`.
2. `class <Grammar> end` and `class <Grammar>.<Group> end` (namespaces).
3. A tree base: `class <Grammar>.<Group>.Node inherit Aquarius.Trees.Program_Tree end`.
4. Attribute bases for each result kind you synthesize.
5. One class per non-terminal you act on, named to match the rule (avoid
   keyword names). Read children in `After_<child>`; build in `After_Node`.
6. Wrap optional keywords in their own rules to detect them (§6).
7. Run with `--check` (semantic) or `--code-trigger` (code) and iterate.
