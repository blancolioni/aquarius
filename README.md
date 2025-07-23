# aquarius
 An EBNF compiler

## Build

```
git clone https://github.com/blancolioni/aquarius.git
cd aquarius
git clone https://github.com/blancolioni/aqua_as.git
git clone https://github.com/blancolioni/aqua_vm.git
git clone https://github.com/blancolioni/tagatha.git
git clone https://github.com/blancolioni/wl_lib.git
(cd aqua_as ; alr build)
alr build
./bin/aquarius-driver --start-class ./share/aquarius/tests/aqua/test.aqua
```

## Aqua

### Object Layout

#### Example: Aqua.Containers.Linked_List

```
0000: vptr Aqua.Containers.Linked_List
0004: prop First_Cell
0008: prop Last_Cell
000C: vptr Aqua.Containers.Sequence
0010: vptr Aqua.Containers.Bag
0014: vptr Aqua.Containers.Collection
0018: vptr Aqua.Containers.Container
001C: vptr Aqua.Containers.Forward_Iterable
0020: vptr Aqua.Iterable
0024: vptr Aqua.Containers.Cell
0028: prop Element
002C: vptr Any
0030: prop Void
```

### Virtual Table Layout

- Object record offset for this class (zero)
- Start of first inherited class
- ...
- Start of last inherited class
- Address of first class feature
- ...
- Address of last class feature
- Object record offset for first inherited class
- Start of first inherited class for this class relative to offset
- ...
- Start of last inherited class for this class relative to offset
- Address of first class feature
- ...
- Address of last class feature
- Rest of inherited classes

#### Example: Aqua.Containers.Linked_List

```
class Aqua.Containers.Linked_List[G]
   inherit Aqua.Containers.Forward_Iterable[G]
           Aqua.Containers.Sequence [G]
              
feature{Aqua.Containers.Linked_List_Iterator}

   First_Cell : detachable Aqua.Containers.Linkable[G]
   Last_Cell  : detachable Aqua.Containers.Linkable[G]
   
feature

```

```
0000: 0000: start Aqua.Containers.Linked_List
0004: 002C: start Linked_List/Any
0008: 0024: start Linked_List/Cell
000C: 0020: start Linked_List/Iterable
0010: 001C: start Linked_List/Forward_Iterable
0014: 0018: start Linked_List/Container
0018: 0014: start Linked_List/Collection
001C: 0010: start Linked_List/Bag
0020: 000C: start Linked_List/Sequence
0024: aqua.containers.linked_list.first_cell
0028: aqua.containers.linked_list.last_cell
002C: 000C: start Aqua.Containers.Sequence
0030: 0020: start Sequence/Any
0034: 000C: start Sequence/Container
0038: 0008: start Sequence/Collection
003C: 0004: start Sequence/Bag
0040: aqua.containers.linked_list.first_element
0044: aqua.containers.linked_list.last_element
0048: aqua.containers.linked_list.delete_first
004C: aqua.containers.linked_list.append
0050: 0010: start Aqua.Containers.Bag
0054: 001C: start Bag/Any
0058: 0008: start Bag/Container
005C: 0004: start Bag/Collection
0060: aqua.containers.linked_list.occurrences
0064: aqua.containers.linked_list.insert
0068: 0014: start Aqua.Containers.Collection
006C: 0018: start Collection/Any
0070: 0004: start Collection/Container
0074: aqua.containers.linked_list.can_delete
0078: aqua.containers.linked_list.can_include
007C: aqua.containers.linked_list.include
0080: aqua.containers.linked_list.delete
0084: aqua.containers.linked_list.delete_all
0088: aqua.containers.linked_list.clear
008C: 0018: start Aqua.Containers.Container
0090: 0014: start Container/Any
0094: aqua.containers.linked_list.contains
0098: aqua.containers.linked_list.is_empty
009C: 001C: start Aqua.Containers.Forward_Iterable
00A0: 0010: start Forward_Iterable/Any
00A4: 0008: start Forward_Iterable/Cell
00A8: 0004: start Forward_Iterable/Iterable
00AC: 0020: start Aqua.Iterable
00B0: 000C: start Iterable/Any
00B4: aqua.containers.linked_list.new_cursor
00B8: 0024: start Aqua.Containers.Cell
00BC: 0008: start Cell/Any
00C0: aqua.containers.cell.element
00C4: aqua.containers.cell.put
00C8: 002C: start Any
00CC: any.default_create
00D0: any.default_rescue
00D4: any.void
00D8: any.equal
00DC: any.not_equal

```

