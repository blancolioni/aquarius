{ A string literal assigned to an array-of-char target (issue #104's
  string-literal follow-up): ISO Pascal's rule that a literal of length n is
  assignable to any array [1 .. n] of char, matched by shape and length, not
  by declared type identity the way a whole-structure copy is
  (test_structured_assignment.pas).

  Covers: a literal matching a packed array of char exactly, one written
  through a type alias, one assigned to an array ELEMENT (basics.pas's actual
  shape, keywd[k] := '...'), and a single-character literal, which was
  already a plain char and is unaffected by any of this. Expect NO errors.

  Check with: bin/aquarius --check test_string_literal_assignment.pas }

program Test_String_Literal_Assignment;

type
   String10 = packed array [1 .. 10] of char;
   Key      = String10;

var
   greeting : String10;
   word     : Key;
   keywd    : array [1 .. 3] of String10;
   letter   : char;

begin
   greeting := 'hello     ';
   word     := 'world     ';
   keywd[1] := 'input     ';
   keywd[2] := 'print     ';
   letter   := 'x'
end.
