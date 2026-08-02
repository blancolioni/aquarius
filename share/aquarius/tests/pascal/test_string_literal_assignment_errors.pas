{ The boundaries of string-literal assignment (issue #104's string-literal
  follow-up). Expect exactly SEVEN errors, two "unsupported type" messages
  (one per side, same as any other unmatched Structured_Type/String_Literal_
  Type combination) for each mismatch except the scalar target, which only
  reports on its structured-literal source:

     short_target := 'too short'   -- 9 characters, target wants 10: 2 errors
     n := 'abcde'                  -- target is an array of INTEGER, not
                                       char, so shape doesn't match: 2 errors
     i := 'x-y'                    -- a scalar target: 1 error (the literal
                                       side only -- the integer side is fine)
     g := 'abcd'                   -- target has TWO dimensions, not one:
                                       2 errors

  Check with: bin/aquarius --check test_string_literal_assignment_errors.pas }

program Test_String_Literal_Assignment_Errors;

type
   String10 = packed array [1 .. 10] of char;
   Ints5    = array [1 .. 5] of integer;
   Grid     = array [1 .. 2, 1 .. 2] of char;

var
   short_target : String10;
   n            : Ints5;
   g            : Grid;
   i            : integer;

begin
   short_target := 'too short';    { error, error }
   n := 'abcde';                   { error, error }
   i := 'x-y';                     { error }
   g := 'abcd'                     { error, error }
end.
