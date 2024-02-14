--  with Ada.Characters.Handling;

package body Aquarius.Lexers.Parser is

   -----------------
   -- Parse_Lexer --
   -----------------

   function Parse_Lexer (Text : String) return Lexer is

      Scan_Error : exception;

      function Scan
        (Index : in out Positive)
         return Lexer;

      function Parse_Atomic
        (Index : in out Positive)
         return Lexer;

      function Scan_Choice
        (Index : in out Positive)
         return Lexer;

      function Parse_Escape
        (Index : in out Positive)
         return Lexer;

      ------------------
      -- Parse_Atomic --
      ------------------

      function Parse_Atomic
        (Index : in out Positive)
         return Lexer
      is
         Ch : constant Character := Text (Index);
         Lex : Lexer;
      begin
         Index := Index + 1;
         case Ch is
            when '[' =>
               Lex := Scan_Choice (Index);
               if Index <= Text'Last and then Text (Index) = ']' then
                  Index := Index + 1;
               else
                  raise Scan_Error;
               end if;
            when '(' =>
               Lex := Scan (Index);
               if Index <= Text'Last and then Text (Index) = ')' then
                  Index := Index + 1;
               else
                  raise Scan_Error;
               end if;
            when '\' =>
               if Index <= Text'Last then
                  Lex := Parse_Escape (Index);
               else
                  Lex := Literal ('\');
               end if;
            when '.' =>
               Lex := Any;
            when others =>
               Lex := Literal (Ch);
         end case;

         if Index <= Text'Last then
            declare
               Suffix : constant Character := Text (Index);
            begin
               Index := Index + 1;

               case Suffix is
                  when '*' =>
                     Lex := Optional (Repeat (Lex));
                  when '+' =>
                     Lex := Repeat (Lex);
                  when '?' =>
                     Lex := Optional (Lex);
                  when others =>
                     Index := Index - 1;
               end case;
            end;
         end if;
         return Lex;
      end Parse_Atomic;

      ------------------
      -- Parse_Escape --
      ------------------

      function Parse_Escape
        (Index : in out Positive)
         return Lexer
      is
         Ch : constant Character := Text (Index);
      begin
         Index := Index + 1;
         case Ch is
            when 'd' =>
               return Digit;
            when 'D' =>
               return not Digit;
            when 'l' =>
               return Letter;
            when 'n' =>
               return new Lexer_Node'
                 (Terminal, new Lexer_Rule'(Condition, False, End_Of_Line));
            when 'w' =>
               return Alphanumeric or Literal ('_');
            when 'x' =>
               if Index + 1 > Text'Last then
                  raise Scan_Error;
               end if;
               declare
                  function To_Digit (Ch : Character) return Natural
                  is (if Ch in '0' .. '9'
                      then Character'Pos (Ch) - Character'Pos ('0')
                      elsif Ch in 'a' .. 'f'
                      then Character'Pos (Ch) - Character'Pos ('a') + 10
                      elsif Ch in 'A' .. 'F'
                      then Character'Pos (Ch) - Character'Pos ('A') + 10
                      else raise Scan_Error with "bad hex digit: " & Ch);
                  X : constant Natural :=
                        To_Digit (Text (Index)) * 16
                        + To_Digit (Text (Index + 1));
               begin
                  Index := Index + 2;
                  return Literal (Character'Val (X));
               end;

            when others =>
               return Literal (Ch);
         end case;
      end Parse_Escape;

      ----------
      -- Scan --
      ----------

      function Scan
        (Index : in out Positive)
         return Lexer
      is
         Result : Lexer := Parse_Atomic (Index);
      begin
         while Index <= Text'Last
           and then Text (Index) /= ')'
         loop
            if Index < Text'Last
              and then Text (Index) = '|'
            then
               Index := Index + 1;
               Result := Result or Parse_Atomic (Index);
            else
               Result := Result & Parse_Atomic (Index);
            end if;
         end loop;
         return Result;
      end Scan;

      -----------------
      -- Scan_Choice --
      -----------------

      function Scan_Choice
        (Index : in out Positive)
         return Lexer
      is
         Negated : Boolean := False;
         Lex     : Lexer := Null_Lexer;
      begin
         if Index <= Text'Last
           and then Text (Index) = '^'
         then
            Index := Index + 1;
            Negated := True;
         end if;

         while Index <= Text'Last
           and then Text (Index) /= ']'
         loop
            declare
               Start : constant Character := Text (Index);
               New_Lex : Lexer;
            begin
               Index := Index + 1;
               if Index > Text'Last then
                  raise Scan_Error;
               end if;
               if Text (Index) = '-' then
                  Index := Index + 1;
                  if Index > Text'Last then
                     raise Scan_Error;
                  elsif Text (Index) = ']' then
                     New_Lex := Literal (Start) or Literal ('-');
                  else
                     New_Lex := In_Range (Start, Text (Index));
                     Index := Index + 1;
                  end if;
               elsif Start = '.' then
                  New_Lex := Any;
               elsif Start = '\' then
                  New_Lex := Parse_Escape (Index);
               else
                  New_Lex := Literal (Start);
               end if;
               if Lex = Null_Lexer then
                  Lex := New_Lex;
               else
                  Lex := Lex or New_Lex;
               end if;
            end;
         end loop;

         if Negated then
            Lex := not Lex;
         end if;

         return Lex;
      end Scan_Choice;

      Index  : Positive := Text'First;
   begin
      return Scan (Index);
   end Parse_Lexer;

end Aquarius.Lexers.Parser;
