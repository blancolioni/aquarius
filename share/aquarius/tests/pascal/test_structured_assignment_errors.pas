{ The boundaries of whole-structure assignment (issue #104's remaining gap).
  Expect exactly FOUR errors:

     unsupported type for an assignment target: only integer, real, boolean
       and char are modelled
     unsupported type for an assigned value: only integer, real, boolean and
       char are modelled
        -- Point and Pair are two DIFFERENT declared record types, shaped
           alike but with different Structure_Index -- matching shape is not
           enough, so this falls back to the ordinary unsupported-type
           reporting, one message per side, same as any other unmodelled
           combination
     unsupported type for an assigned value: only integer, real, boolean and
       char are modelled
        -- a record assigned to a scalar
     unsupported type for an assignment target: only integer, real, boolean
       and char are modelled
        -- a scalar assigned to a record

  Check with: bin/aquarius --check test_structured_assignment_errors.pas }

program Test_Structured_Assignment_Errors;

type
   Point = record
      x, y : integer
   end;

   Pair = record
      x, y : integer
   end;

var
   a : Point;
   p : Pair;
   i : integer;

begin
   a := p;              { two errors: different record types }
   a := i;              { error: a scalar assigned to a record }
   i := a               { error: a record assigned to a scalar }
end.
