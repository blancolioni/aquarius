package body Aquarius.Regexes is

   type Trit is (No, Maybe, Yes);

   type Regex_Class is (Any, Characters, Repeat, Optional, Sequence);

   type Regex_Record (Class : Regex_Class) is
      record
         case Class is
            when Any =>
               null;
            when Characters =>
               Matches : Character_Match;
            when Repeat =>
               At_Least_One : Boolean;
               Repeat_Child : Regex;
            when Optional =>
               Optional_Child : Regex;
            when Sequence =>
               Left, Right : Regex;
               Nullable    : Trit := Maybe;
         end case;
      end record;

   procedure Match_Character (Text    : String;
                              Ex      : Regex;
                              Current : in out Natural;
                              Success :    out Boolean);

   function Nullable (Ex : Regex) return Boolean;

   procedure Do_Match (Text     : String;
                       Ex       : Regex;
                       Current  : in out Positive;
                       Partial  :    out Boolean;
                       Complete :    out Boolean);

   -------------------------
   -- Any_Character_Match --
   -------------------------

   function Any_Character_Match return Regex is
   begin
      return new Regex_Record (Any);
   end Any_Character_Match;

   --------------
   -- Do_Match --
   --------------

   procedure Do_Match (Text     : String;
                       Ex       : Regex;
                       Current  : in out Positive;
                       Partial  :    out Boolean;
                       Complete :    out Boolean)
   is
   begin
      if Current > Text'Last then
         Partial := True;
         Complete := Nullable (Ex);
      else
         case Ex.Class is
            when Any =>
               Current  := Current + 1;
               Partial  := True;
               Complete := True;
            when Characters =>
               if Ex.Matches (Text (Current)) then
                  Current := Current + 1;
                  Partial := True;
                  Complete := True;
               else
                  Partial := False;
                  Complete := False;
               end if;
            when Repeat =>
               declare
                  Old_Child_Current : Positive;
                  Child_Current     : Positive := Current;
                  Child_Partial     : Boolean;
                  Child_Complete    :  Boolean;
                  Got_One           : Boolean  := False;
               begin
                  loop
                     Old_Child_Current := Child_Current;
                     Do_Match (Text, Ex.Repeat_Child, Child_Current,
                               Child_Partial, Child_Complete);
                     exit when not Child_Complete or else
                       Old_Child_Current = Child_Current;
                     Got_One := True;
                  end loop;

                  if not Got_One then
                     if Ex.At_Least_One then
                        Partial  := Child_Partial;
                        Complete := False;
                        Current  := Child_Current;
                     else
                        Partial  := True;
                        Complete := True;
                        --  no change to current
                     end if;
                  else
                     if not Child_Complete then
                        Partial := True;
                        Complete := True;
                        Current := Old_Child_Current;
                     else
                        Partial  := Child_Partial;
                        Complete := True;
                        Current  := Child_Current;
                     end if;
                  end if;
               end;
            when Optional =>
               declare
                  Child_Current  : Positive := Current;
                  Child_Partial  : Boolean;
                  Child_Complete : Boolean;
               begin
                  Do_Match (Text, Ex.Optional_Child, Child_Current,
                            Child_Partial, Child_Complete);
                  if Child_Complete then
                     Current := Child_Current;
                  end if;

                  Partial := True;
                  Complete := True;
               end;

            when Sequence =>

               declare
                  Child_Current  : Positive := Current;
                  Child_Partial  : Boolean;
                  Child_Complete : Boolean;
               begin
                  Do_Match (Text, Ex.Left, Child_Current,
                            Child_Partial, Child_Complete);

                  if Child_Complete then
                     Do_Match (Text, Ex.Right, Child_Current,
                               Partial, Complete);
                     Current := Child_Current;
                  elsif Nullable (Ex.Left) then
                     declare
                        Right_Child_Current : Positive := Current;
                        Right_Child_Partial  : Boolean;
                        Right_Child_Complete : Boolean;
                     begin
                        Do_Match (Text, Ex.Right, Right_Child_Current,
                                  Right_Child_Partial,
                                  Right_Child_Complete);
                        if Right_Child_Partial then
                           Current := Right_Child_Current;
                           Partial := True;
                           Complete := Right_Child_Complete;
                        else
                           Current := Child_Current;
                           Partial := Child_Partial;
                           Complete := Child_Complete;
                        end if;
                     end;
                  else
                     Current := Child_Current;
                     Partial := Child_Partial;
                     Complete := False;
                  end if;
               end;

         end case;
      end if;

   end Do_Match;

   -----------
   -- Image --
   -----------

   function Image (Ex : Regex) return String is
      function Range_Image (Chars   : Character_Match;
                            Index   : Character        := Character'First)
                           return String;

      -----------------
      -- Range_Image --
      -----------------

      function Range_Image (Chars   : Character_Match;
                            Index   : Character        := Character'First)
                           return String
      is
         Local_Index : Character := Index;
         Start       : Character;
      begin
         while Local_Index < Character'Last
           and then not Chars (Local_Index)
         loop
            Local_Index := Character'Succ (Local_Index);
         end loop;
         if Local_Index = Character'Last then
            if Chars (Local_Index) then
               return [Local_Index];
            else
               return "";
            end if;
         else
            Start := Local_Index;
            while Local_Index < Character'Last
              and then Chars (Local_Index)
            loop
               Local_Index := Character'Succ (Local_Index);
            end loop;

            if Local_Index = Character'Last
              and then Chars (Local_Index)
            then
               return Start & "-";
            elsif Character'Pred (Local_Index) = Start then
               return Start & Range_Image (Chars, Local_Index);
            else
               return Start & "-" & Character'Pred (Local_Index) &
                 Range_Image (Chars, Local_Index);
            end if;
         end if;
      end Range_Image;

   begin
      case Ex.Class is
         when Any =>
            return "[.]";
         when Characters =>
            return "[" & Range_Image (Ex.Matches) & "]";
         when Repeat =>
            if Ex.At_Least_One then
               return "(" & Image (Ex.Repeat_Child) & ")+";
            else
               return "(" & Image (Ex.Repeat_Child) & ")*";
            end if;
         when Optional =>
            return "(" & Image (Ex.Optional_Child) & ")?";
         when Sequence =>
            return "(" & Image (Ex.Left) & Image (Ex.Right) & ")";
      end case;
   end Image;

   -------------
   -- Is_Null --
   -------------

   function Is_Null (Ex : Regex) return Boolean is
   begin
      return Ex = null;
   end Is_Null;

   -----------
   -- Match --
   -----------

   procedure Match (Text     : String;
                    Ex       : Regex;
                    Last     :    out Natural;
                    Partial  :    out Boolean;
                    Complete :    out Boolean)
   is
      Current : Positive := Text'First;
   begin
      Do_Match (Text, Ex, Current, Partial, Complete);
      Last := Current - 1;
   end Match;

   ---------------------
   -- Match_Character --
   ---------------------

   procedure Match_Character (Text    : String;
                              Ex      : Regex;
                              Current : in out Natural;
                              Success :    out Boolean)
   is
   begin
      Success := False;
      if Current <= Text'Last then
         case Ex.Class is
            when Any =>
               Current := Current + 1;
               Success := True;
            when Characters =>
               if Ex.Matches (Text (Current)) then
                  Current := Current + 1;
                  Success := True;
               end if;
            when Repeat =>
               declare
                  Repeat_Matched : Boolean;
                  Old_Current    : constant Natural := Current;
                  New_Current    : Natural;
               begin
                  loop
                     New_Current := Current;
                     Match_Character (Text, Ex.Repeat_Child,
                                      New_Current, Repeat_Matched);
                     if Repeat_Matched then
                        Success := True;
                     end if;
                     exit when not Repeat_Matched
                       or else Current = New_Current;
                     Current := New_Current;
                  end loop;

                  if not Success then
                     Success := not Ex.At_Least_One;
                  end if;
                  if not Success then
                     Current := Old_Current;
                  end if;
               end;
            when Optional =>
               Match_Character (Text, Ex.Optional_Child, Current, Success);
               Success := True;
            when Sequence =>
               declare
                  Old_Current : constant Natural := Current;
               begin
                  Match_Character (Text, Ex.Left, Current, Success);
                  if Success then
                     Match_Character (Text, Ex.Right, Current, Success);
                  end if;
                  if not Success then
                     Current := Old_Current;
                  end if;
               end;
         end case;
      else
         Success := Ex.Class = Optional
           or else (Ex.Class = Repeat and then not Ex.At_Least_One);
      end if;

   end Match_Character;

   -------------------------
   -- New_Character_Match --
   -------------------------

   function New_Character_Match (Match : Character_Match) return Regex is
   begin
      return new Regex_Record'(Characters, Match);
   end New_Character_Match;

   ------------------
   -- New_Optional --
   ------------------

   function New_Optional (Ex : Regex) return Regex is
   begin
      return new Regex_Record'(Optional, Ex);
   end New_Optional;

   ----------------
   -- New_Repeat --
   ----------------

   function New_Repeat
     (Ex            : Regex;
      At_Least_Once : Boolean)
      return Regex
   is
   begin
      return new Regex_Record'(Repeat, At_Least_Once, Ex);
   end New_Repeat;

   ------------------
   -- New_Sequence --
   ------------------

   function New_Sequence (Left, Right : Regex) return Regex is
   begin
      return new Regex_Record'(Sequence, Left, Right, Maybe);
   end New_Sequence;

   ----------------
   -- Null_Regex --
   ----------------

   function Null_Regex return Regex is
   begin
      return null;
   end Null_Regex;

   --------------
   -- Nullable --
   --------------

   function Nullable (Ex : Regex) return Boolean is
   begin
      case Ex.Class is
         when Any =>
            return False;
         when Characters =>
            return False;
         when Repeat =>
            return not Ex.At_Least_One;
         when Optional =>
            return True;
         when Sequence =>
            if Ex.Nullable = Maybe then
               if Nullable (Ex.Left)
                 and then Nullable (Ex.Right)
               then
                  Ex.Nullable := Yes;
               else
                  Ex.Nullable := No;
               end if;
            end if;
            return Ex.Nullable = Yes;
      end case;
   end Nullable;

end Aquarius.Regexes;
