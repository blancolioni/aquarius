with Ada.Text_IO;

package body Aquarius.Regexes.Parser is

   procedure Parse_Regex
     (Text    : String;
      Result  :    out Aquarius.Regexes.Regex;
      Success :    out Boolean);

   procedure Parse_Atomic_Regex
     (Text    : String;
      Index   : in out Positive;
      Result  :    out Aquarius.Regexes.Regex;
      Success :    out Boolean);

   procedure Parse_Character_Match
     (Text    : String;
      Index   : in out Positive;
      Result  :    out Aquarius.Regexes.Regex;
      Success :    out Boolean);

   procedure Parse_Regex
     (Text    : String;
      Index   : in out Positive;
      Result  :    out Aquarius.Regexes.Regex;
      Success :    out Boolean);

   procedure Error (Ex      : String;
                    Index   : Positive;
                    Message : String);

   -----------
   -- Error --
   -----------

   procedure Error (Ex      : String;
                    Index   : Positive;
                    Message : String)
   is
   begin
      Ada.Text_IO.Put_Line (Ex);

      declare
         Indicator : constant String (1 .. Index - 1) :=
           (others => '-');
      begin
         Ada.Text_IO.Put_Line (Indicator & '^');
      end;
      Ada.Text_IO.Put_Line ("error: " & Message);
   end Error;

   ------------------------
   -- Parse_Atomic_Regex --
   ------------------------

   procedure Parse_Atomic_Regex
     (Text    : String;
      Index   : in out Positive;
      Result  :    out Aquarius.Regexes.Regex;
      Success :    out Boolean)
   is
   begin
      case Text (Index) is
         when '[' =>
            Index := Index + 1;
            Parse_Character_Match (Text, Index, Result, Success);
            if Index <= Text'Last and then
              Text (Index) = ']'
            then
               Index := Index + 1;
            else
               Error (Text, Index, "missing ']'");
               Success := False;
            end if;

         when '(' =>
            Index := Index + 1;
            Parse_Regex (Text, Index, Result, Success);
            if Index <= Text'Last and then
              Text (Index) = ')'
            then
               Index := Index + 1;
            else
               Error (Text, Index, "missing ')'");
               Success := False;
            end if;

         when others =>
            if Text (Index) = '\' then
               if Index = Text'Last then
                  Error (Text, Index, "missing completion for escape");
                  Success := False;
                  return;
               else
                  Index := Index + 1;
               end if;
            end if;

            declare
               Char_Match : Aquarius.Regexes.Character_Match :=
                 [others => False];
            begin
               Char_Match (Text (Index)) := True;
               Result := Aquarius.Regexes.New_Character_Match (Char_Match);
               Index := Index + 1;
               Success := True;
            end;
      end case;

      if Index <= Text'Last then
         if Text (Index) = '?' then
            Result := Aquarius.Regexes.New_Optional (Result);
            Index := Index + 1;
         elsif Text (Index) = '*' then
            Result := Aquarius.Regexes.New_Repeat (Result, False);
            Index := Index + 1;
         elsif Text (Index) = '+' then
            Result := Aquarius.Regexes.New_Repeat (Result, True);
            Index := Index + 1;
         end if;
      end if;

   end Parse_Atomic_Regex;

   ---------------------------
   -- Parse_Character_Match --
   ---------------------------

   procedure Parse_Character_Match
     (Text    : String;
      Index   : in out Positive;
      Result  :    out Aquarius.Regexes.Regex;
      Success :    out Boolean)
   is
      Negative    : Boolean := False;
      Char_Match  : Aquarius.Regexes.Character_Match := [others => False];
   begin
      if Text (Index) = '~' then
         Negative := True;
         Index := Index + 1;
      end if;

      if Text (Index) = '-' then
         Char_Match ('-') := True;
      end if;

      while Index <= Text'Last and then Text (Index) /= ']' loop
         if Text (Index) = '.' then
            Char_Match (Character'Val (0) .. Character'Val (31)) :=
              [others => False];
            Char_Match (Character'Val (32) .. Character'Val (255)) :=
              [others => True];
            Char_Match (Character'Val (9)) := True;
            Index := Index + 1;
         elsif Text (Index) = '\' then
            Index := Index + 1;
            if Index <= Text'Last then
               Char_Match (Text (Index)) := True;
               Index := Index + 1;
            else
               Error (Text, Index, "missing completion for escape");
               Success := False;
               return;
            end if;
         else
            if Index < Text'Last - 1 and then Text (Index + 1) = '-' then
               declare
                  Low, High : Character;
               begin
                  Low := Text (Index);
                  Index := Index + 2;
                  if Text (Index) = '\' then
                     Index := Index + 1;
                     if Index <= Text'Last then
                        High := Text (Index);
                        Index := Index + 1;
                     else
                        Error (Text, Index, "missing completion for escape");
                        Success := False;
                        return;
                     end if;
                  elsif Text (Index) = ']' then
                     High := Character'Last;
                  else
                     High := Text (Index);
                     Index := Index + 1;
                  end if;

                  for I in Character range Low .. High loop
                     Char_Match (I) := True;
                  end loop;
               end;
            else
               Char_Match (Text (Index)) := True;
               Index := Index + 1;
            end if;
         end if;

      end loop;

      if Negative then
         Char_Match := Aquarius.Regexes."not" (Char_Match);
      end if;

      Result := Aquarius.Regexes.New_Character_Match (Char_Match);
      Success := True;

   end Parse_Character_Match;

   -----------------
   -- Parse_Regex --
   -----------------

   procedure Parse_Regex
     (Text    : String;
      Index   : in out Positive;
      Result  :    out Aquarius.Regexes.Regex;
      Success :    out Boolean)
   is
      Left, Right : Aquarius.Regexes.Regex;
   begin
      Parse_Atomic_Regex (Text, Index, Left, Success);
      while Success and then Index <= Text'Last and then
        Text (Index) /= ')'
      loop
         Parse_Atomic_Regex (Text, Index, Right, Success);
         if Success then
            Left := Aquarius.Regexes.New_Sequence (Left, Right);
         end if;
      end loop;

      if Success then
         Result := Left;
      end if;
   end Parse_Regex;

   -----------------
   -- Parse_Regex --
   -----------------

   procedure Parse_Regex
     (Text    : String;
      Result  :    out Aquarius.Regexes.Regex;
      Success :    out Boolean)
   is
      Index       : Positive := Text'First;
   begin
      Parse_Regex (Text, Index, Result, Success);
      if Index <= Text'Last then
         Error (Text, Index, "extra ignored");
         Success := False;
      end if;

   end Parse_Regex;

   -----------------
   -- Parse_Regex --
   -----------------

   function Parse_Regex (Ex : String) return Regex is
      Result  : Regex;
      Success : Boolean;
   begin
      Parse_Regex (Ex, Result, Success);
      if not Success then
         return null;
      else
         return Result;
      end if;
   end Parse_Regex;

end Aquarius.Regexes.Parser;
