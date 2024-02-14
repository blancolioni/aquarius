with Ada.Characters.Handling;
with Ada.Characters.Latin_1;

package body Aquarius.Lexers is

   ---------
   -- "&" --
   ---------

   function "&" (Left, Right : Lexer) return Lexer is
   begin
      return new Lexer_Node'(Sequence, Left, Right);
   end "&";

   -----------
   -- "not" --
   -----------

   function "not" (Left : Lexer) return Lexer is
   begin
      return new Lexer_Node'(Negate, Left);
   end "not";

   ----------
   -- "or" --
   ----------

   function "or" (Left, Right : Lexer) return Lexer is
   begin
      return new Lexer_Node'(Choice, Left, Right);
   end "or";

   ------------------
   -- Alphanumeric --
   ------------------

   function Alphanumeric return Lexer is
      use Ada.Characters.Handling;
   begin
      return new Lexer_Node'(Terminal,
                             new Lexer_Rule'
                               (Built_In, False,
                                Is_Alphanumeric'Access));
   end Alphanumeric;

   ---------
   -- Any --
   ---------

   function Any return Lexer is
   begin
      --  we totally fake this
      return not Literal (Character'Val (0));
   end Any;

   -----------
   -- Digit --
   -----------

   function Digit return Lexer is
      use Ada.Characters.Handling;
   begin
      return new Lexer_Node'(Terminal,
                             new Lexer_Rule'(Built_In, False,
                                             Is_Digit'Access));
   end Digit;

   -----------------
   -- End_Of_Line --
   -----------------

   function End_Of_Line  return Lexer is
   begin
      return new Lexer_Node'(Terminal,
                             new Lexer_Rule'
                               (Condition, False, End_Of_Line));
   end End_Of_Line;

   -------------
   -- Graphic --
   -------------

   function Graphic return Lexer is
      use Ada.Characters.Handling;
   begin
      return new Lexer_Node'(Terminal,
                             new Lexer_Rule'
                               (Built_In, False,
                                Is_Graphic'Access));
   end Graphic;

   ---------------
   -- Hex_Digit --
   ---------------

   function Hex_Digit  return Lexer is
      use Ada.Characters.Handling;
   begin
      return new Lexer_Node'(Terminal,
                             new Lexer_Rule'
                               (Built_In, False,
                                Is_Hexadecimal_Digit'Access));
   end Hex_Digit;

   --------------
   -- In_Range --
   --------------

   function In_Range (Low, High : Character) return Lexer is
   begin
      return new Lexer_Node'(Terminal,
                             new Lexer_Rule'
                               (Character_Range, False,
                                Low, High));
   end In_Range;

   ------------
   -- Letter --
   ------------

   function Letter       return Lexer is
      use Ada.Characters.Handling;
   begin
      return new Lexer_Node'(Terminal,
                             new Lexer_Rule'(Built_In, False,
                                             Is_Letter'Access));
   end Letter;

   -------------
   -- Literal --
   -------------

   function Literal (Ch : Character) return Lexer is
   begin
      return new Lexer_Node'(Terminal,
                             new Lexer_Rule'
                               (Single_Character, False, Ch));
   end Literal;

   ---------------
   -- Lowercase --
   ---------------

   function Lowercase return Lexer is
      use Ada.Characters.Handling;
   begin
      return new Lexer_Node'(Terminal,
                             new Lexer_Rule'(Built_In, False,
                                             Is_Lower'Access));
   end Lowercase;

   ----------------------
   -- Matches_New_Line --
   ----------------------

   function Matches_New_Line
     (Lex : Lexer)
      return Boolean
   is
      Test : constant Natural :=
               Run (Lex, [Ada.Characters.Latin_1.LF]);
   begin
      return Test > 1;
   end Matches_New_Line;

   ----------------
   -- Null_Lexer --
   ----------------

   function Null_Lexer return Lexer is
   begin
      return null;
   end Null_Lexer;

   ------------
   -- One_Of --
   ------------

   function One_Of (S : String) return Lexer is
      Result : Lexer := Literal (S (S'First));
   begin
      for I in S'First + 1 .. S'Last loop
         Result := Result or Literal (S (I));
      end loop;
      return Result;
   end One_Of;

   --------------
   -- Optional --
   --------------

   function Optional (Item : Lexer) return Lexer is
   begin
      return new Lexer_Node'(Optional, Item);
   end Optional;

   --------------
   -- Overlaps --
   --------------

   function Overlaps
     (Lex_1, Lex_2 : Lexer)
      return Boolean
   is
      use Ada.Strings.Maps;
      Start_1 : constant Character_Sequence :=
                  To_Sequence (Start (Lex_1));
      Start_2 : constant Character_Set :=
                  Start (Lex_2);
   begin
      for Ch of Start_1 loop
         if Is_In (Ch, Start_2) then
            return True;
         end if;
      end loop;
      return False;
   end Overlaps;

   ------------
   -- Repeat --
   ------------

   function Repeat (Item : Lexer) return Lexer is
   begin
      return new Lexer_Node'(Repeat, Item);
   end Repeat;

   ---------
   -- Run --
   ---------

   function Run (Lex    : Lexer;
                 Text   : String)
                return Natural
   is
      function Scan (Current : not null access Lexer_Node;
                     Index   : Positive)
                    return Natural;

      function Match_Rule (Rule : not null access Lexer_Rule;
                           Ch   : Character)
                          return Boolean;

      ----------------
      -- Match_Rule --
      ----------------

      function Match_Rule (Rule : not null access Lexer_Rule;
                           Ch   : Character)
                          return Boolean
      is
         Result : Boolean;
      begin
         case Rule.Rule_Type is
            when Built_In =>
               Result := Rule.Fn (Ch);
            when Condition =>
               case Rule.State is
                  when End_Of_Line =>
                     Result := Ch = Ada.Characters.Latin_1.LF;
                  when End_Of_File =>
                     Result := Ch = Ada.Characters.Latin_1.NUL;
               end case;
            when Single_Character =>
               Result := Ch = Rule.Match;
            when Character_Range =>
               Result := Ch in Rule.Lo .. Rule.Hi;
            when Or_Rule =>
               Result := Match_Rule (Rule.Left, Ch) or else
                 Match_Rule (Rule.Right, Ch);
         end case;
         if Rule.Negate then
            return not Result;
         else
            return Result;
         end if;
      end Match_Rule;

      ----------
      -- Scan --
      ----------

      function Scan (Current : not null access Lexer_Node;
                     Index   : Positive)
                    return Natural
      is
      begin
         case Current.Node_Type is
            when Terminal =>
               if Index > Text'Last then
                  return 0;
               elsif Match_Rule (Current.Rule, Text (Index)) then
                  return Index + 1;
               end if;
            when Sequence =>
               declare
                  First  : constant Natural :=
                    Scan (Current.First, Index);
                  Second : Natural;
               begin
                  if First > Index then
                     Second := Scan (Current.Rest, First);
                     if Second > 0 then
                        return Second;
                     end if;
                  end if;
               end;
            when Repeat =>
               if Index > Text'Last then
                  return Index;
               end if;

               declare
                  Last : Natural := Index;
                  Next : Natural := Scan (Current.Child, Index);
               begin
                  if Next > Last then
                     while Next > Last loop
                        Last := Next;
                        Next := Scan (Current.Child, Next);
                     end loop;
                     if Next > 0 then
                        return Next;
                     else
                        return Last;
                     end if;
                  else
                     return 0;
                  end if;
               end;
            when Optional =>
               declare
                  Scan_Op : constant Natural := Scan (Current.Child, Index);
               begin
                  if Scan_Op > 0 then
                     return Scan_Op;
                  else
                     return Index;
                  end if;
               end;
            when Choice =>
               declare
                  Left_Choice  : constant Natural :=
                    Scan (Current.Left, Index);
                  Right_Choice : constant Natural :=
                    Scan (Current.Right, Index);
               begin
                  if Left_Choice > Right_Choice then
                     return Left_Choice;
                  else
                     return Right_Choice;
                  end if;
               end;
            when Negate =>
               if Index > Text'Last then
                  return 0;
               end if;
               declare
                  Tmp_Index : constant Natural :=
                                Scan (Current.Negated, Index);
               begin
                  if Tmp_Index > 0 then
                     return 0;
                  else
                     return Index + 1;
                  end if;
               end;
         end case;

         return 0;
      end Scan;

   begin
      if Lex = Null_Lexer then
         return 0;
      end if;

      return Scan (Lex, Text'First);
   end Run;

   --------------
   -- Sequence --
   --------------

   function Sequence (S : String) return Lexer is
   begin
      if S'Length = 1 then
         return Literal (S (S'First));
      else
         return Literal (S (S'First)) & Sequence (S (S'First + 1 .. S'Last));
      end if;
   end Sequence;

   ----------
   -- Show --
   ----------

   function Show (Lex : Lexer) return String is

      function Show_Rule (Rule : access Lexer_Rule) return String;

      function Show_Node (Node : access Lexer_Node) return String;

      ---------------
      -- Show_Node --
      ---------------

      function Show_Node (Node : access Lexer_Node) return String is
      begin
         if Node = null then
            return "";
         end if;

         case Node.Node_Type is
            when Terminal =>
               return (if Node.Rule.Negate then "~" else "")
                 & Show_Rule (Node.Rule);
            when Sequence =>
               return Show_Node (Node.First) & Show_Node (Node.Rest);
            when Repeat =>
               return "<" & Show_Node (Node.Child) & ">";
            when Optional =>
               if Node.Child.Node_Type = Repeat then
                  return "{" & Show_Node (Node.Child.Child) & "}";
               else
                  return "[" & Show_Node (Node.Child) & "]";
               end if;
            when Choice =>
               return "(" & Show_Node (Node.Left)
                 & " or " & Show_Node (Node.Right) & ")";
            when Negate =>
               return "not (" & Show_Node (Node.Negated) & ")";
         end case;
      end Show_Node;

      ---------------
      -- Show_Rule --
      ---------------

      function Show_Rule (Rule : access Lexer_Rule) return String is
      begin
         case Rule.Rule_Type is
            when Built_In =>
               return "#fn";
            when Condition =>
               case Rule.State is
                  when End_Of_Line =>
                     return "\n";
                  when End_Of_File =>
                     return "\eof";
               end case;
            when Single_Character =>
               return [Rule.Match];
            when Character_Range =>
               return Rule.Lo & "-" & Rule.Hi;
            when Or_Rule =>
               return Show_Rule (Rule.Left) & "|" & Show_Rule (Rule.Right);
         end case;
      end Show_Rule;

   begin
      return Show_Node (Lex);
   end Show;

   -----------
   -- Start --
   -----------

   function Start (Lex : Lexer)
                  return Ada.Strings.Maps.Character_Set
   is
      use Ada.Strings.Maps;
      Start_List : Character_Sequence (1 .. 256);
      Count      : Natural := 0;
   begin
      for I in Character loop
         if Run (Lex, [I]) > 1 then
            Count := Count + 1;
            Start_List (Count) := I;
         end if;
      end loop;
      return To_Set (Start_List (1 .. Count));
   end Start;

   ------------------
   -- Symbol_Lexer --
   ------------------

   function Symbol_Lexer (S : String) return Lexer is
      Lexers : array (Character range ' ' .. '~') of Lexer;
      Start  : Positive := S'First;
      Next   : Positive := Start;
   begin
      while Start <= S'Last loop
         Next := Start;
         while Next <= S'Last and then
           S (Next) /= ' '
         loop
            Next := Next + 1;
         end loop;
         declare
            Lex : Lexer := Literal (S (Next - 1));
         begin
            for I in reverse Next - 1 .. Start loop
               Lex := Literal (S (I)) & Lex;
            end loop;
            if Lexers (S (Start)) = null then
               Lexers (S (Start)) := Lex;
            else
               Lexers (S (Start)) := Lex or Lexers (S (Start));
            end if;
         end;
         Start := Next + 1;
      end loop;
      declare
         Result : Lexer;
      begin
         for I in Lexers'Range loop
            if Lexers (I) /= null then
               if Result = null then
                  Result := Lexers (I);
               else
                  Result := Result or Lexers (I);
               end if;
            end if;
         end loop;
         return Result;
      end;
   end Symbol_Lexer;

   ---------------
   -- Uppercase --
   ---------------

   function Uppercase return Lexer is
      use Ada.Characters.Handling;
   begin
      return new Lexer_Node'(Terminal,
                             new Lexer_Rule'(Built_In, False,
                                             Is_Upper'Access));
   end Uppercase;

end Aquarius.Lexers;
