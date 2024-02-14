with Ada.Characters.Handling;

package body Aquarius.Formats is

   No_Format_Rule : constant Format_Rule := (No_Rule, Before, 1);

   Default_Indent_Priority    : constant Rule_Priority := 5;
   Default_New_Line_Priority  : constant Rule_Priority := 5;

   Default_Space_Priority     : constant Rule_Priority := 7;
   Default_No_Space_Priority  : constant Rule_Priority := 8;
   Low_Space_Priority         : constant Rule_Priority := 5;

   function Image (Item : Format_Rule) return String;

   --------------
   -- Add_Rule --
   --------------

   procedure Add_Rule (Format : in out Aquarius_Format;
                       Rule   : Format_Rule)
   is
   begin
      if Format.Num_Rules < Max_Rules then
         Format.Num_Rules := Format.Num_Rules + 1;
         Format.Rules (Format.Num_Rules) := Rule;
      else
         raise Constraint_Error with
           "too many rules in format";
      end if;
   end Add_Rule;

   -------------
   -- Closing --
   -------------

   function Closing return Format_Rule is
   begin
      return (Bracketing_Rule, After, 0);
   end Closing;

   ---------------------------------
   -- Default_Non_Terminal_Format --
   ---------------------------------

   function Default_Non_Terminal_Format return Aquarius_Format is
   begin
      return (Num_Rules => 2,
              Rules     => [1 => Space_Always (Before),
                            2 => Space_Always (After),
                            others => (No_Rule, Before, 1)]);
   end Default_Non_Terminal_Format;

   -----------------------------
   -- Default_Terminal_Format --
   -----------------------------

   function Default_Terminal_Format return Aquarius_Format is
   begin
      return (Num_Rules => 2,
              Rules     => [1 => Space_Always (Before),
                            2 => Space_Always (After),
                            others => (No_Rule, Before, 1)]);
   end Default_Terminal_Format;

   -------------
   -- Enabled --
   -------------

   function Enabled  (Rule : Format_Rule) return Boolean is
   begin
      return Rule.Rule_Type /= No_Rule;
   end Enabled;

   ----------------
   -- Equivalent --
   ----------------

   function Equivalent (Left, Right : Format_Rule) return Boolean is
   begin
      return Left.Rule_Type = Right.Rule_Type and then
        Left.Position = Right.Position;
   end Equivalent;

   -----------
   -- Image --
   -----------

   function Image (Item : Format_Rule) return String is
   begin
      case Item.Rule_Type is
         when No_Rule =>
            return "none";
         when Space_Rule =>
            if Item.Position = Before then
               if Item.Add_Space then
                  return "space-before";
               else
                  return "no-space-before";
               end if;
            else
               if Item.Add_Space then
                  return "space-after";
               else
                  return "no-space-after";
               end if;
            end if;
         when New_Line_Rule =>
            if Item.Position = Before then
               if Item.Soft then
                  return "soft-new-line-before";
               else
                  return "new-line-before";
               end if;
            else
               if Item.Soft then
                  return "soft-new-line-after";
               else
                  return "new-line-after";
               end if;
            end if;
         when Bracketing_Rule =>
            case Item.Position is
               when Before =>
                  return "opening";
               when After =>
                  return "closing";
            end case;
         when Indent_Rule =>
            if Item.Offset < 0 then
               return "outdent" &
                 Indentation_Offset'Image (abs Item.Offset);
            else
               return "indent" & Indentation_Offset'Image (Item.Offset);
            end if;
         when Indent_Child_Rule =>
            return "indent-child" & Indentation_Offset'Image (Item.Offset);
      end case;
   end Image;

   -----------
   -- Image --
   -----------

   function Image (Item : Aquarius_Format) return String is
      function Img (Item  : Aquarius_Format;
                    Index : Rule_Count)
                   return String;

      ---------
      -- Img --
      ---------

      function Img (Item  : Aquarius_Format;
                    Index : Rule_Count)
                   return String
      is
      begin
         if Index = Item.Num_Rules then
            return Image (Item.Rules (Index));
         else
            return Image (Item.Rules (Index)) & "," &
              Img (Item, Index + 1);
         end if;
      end Img;
   begin
      if Item.Num_Rules = 0 then
         return "<null format>";
      else
         return "<" & Img (Item, 1) & ">";
      end if;
   end Image;

   ------------
   -- Indent --
   ------------

   function Indent (Position : Rule_Position;
                    Offset   : Indentation_Offset)
                   return Format_Rule
   is
   begin
      return (Indent_Rule, Position, Default_Indent_Priority, Offset);
   end Indent;

   ------------
   -- Indent --
   ------------

   function Indent (Format   : Aquarius_Format;
                    Position : Rule_Position)
                   return Indentation_Offset
   is
   begin
      for I in 1 .. Format.Num_Rules loop
         if Format.Rules (I).Rule_Type = Indent_Rule and then
           Format.Rules (I).Position = Position
         then
            return Format.Rules (I).Offset;
         end if;
      end loop;
      return 0;
   end Indent;

   ------------------
   -- Indent_Child --
   ------------------

   function Indent_Child (Offset : Indentation_Offset)
                         return Format_Rule
   is
   begin
      return (Indent_Child_Rule, Before, Default_Indent_Priority, Offset);
   end Indent_Child;

   ------------------
   -- Indent_Child --
   ------------------

   function Indent_Child (Format : Aquarius_Format)
                         return Indentation_Offset
   is
   begin
      for I in 1 .. Format.Num_Rules loop
         if Format.Rules (I).Rule_Type = Indent_Child_Rule then
            return Format.Rules (I).Offset;
         end if;
      end loop;
      return 0;
   end Indent_Child;

   ----------
   -- Join --
   ----------

   function Join (Left, Right : Aquarius_Format)
                 return Aquarius_Format
   is
      Result : Aquarius_Format := Left;
   begin
      for I in 1 .. Right.Num_Rules loop
         declare
            Found : Boolean := False;
         begin
            for J in 1 .. Result.Num_Rules loop
               if Equivalent (Result.Rules (J), Right.Rules (I)) then
                  Result.Rules (J) := Right.Rules (I);
                  Found := True;
                  exit;
               end if;
            end loop;

            if not Found then
               Result.Num_Rules := Result.Num_Rules + 1;
               Result.Rules (Result.Num_Rules + 1) := Right.Rules (I);
            end if;
         end;
      end loop;
      return Result;
   end Join;

   -----------------
   -- Make_Format --
   -----------------

   function Make_Format (Rule : Format_Rule) return Aquarius_Format is
      Result : Aquarius_Format;
   begin
      Add_Rule (Result, Rule);
      return Result;
   end Make_Format;

   -----------------
   -- Make_Format --
   -----------------

   function Make_Format (Rules : Array_Of_Rules) return Aquarius_Format is
      Result : Aquarius_Format;
   begin
      for I in Rules'Range loop
         Add_Rule (Result, Rules (I));
      end loop;
      return Result;
   end Make_Format;

   --------------
   -- Negative --
   --------------

   function Negative (Rule : Format_Rule) return Boolean is
   begin
      return Rule.Rule_Type = Space_Rule and then
        not Rule.Add_Space;
   end Negative;

   --------------
   -- New_Line --
   --------------

   function New_Line (Position : Rule_Position) return Format_Rule is
   begin
      return (New_Line_Rule, Position, Default_New_Line_Priority,
              Soft => False);
   end New_Line;

   -------------
   -- Opening --
   -------------

   function Opening return Format_Rule is
   begin
      return (Bracketing_Rule, Before, 0);
   end Opening;

   --------------
   -- Priority --
   --------------

   function Priority (Rule : Format_Rule) return Rule_Priority is
   begin
      return Rule.Priority;
   end Priority;

   -----------
   -- Rules --
   -----------

   function Rules (Format : Aquarius_Format) return Immediate_Rules is
      Result   : Immediate_Rules := (others => No_Format_Rule);
      Position : Rule_Position;
      Rule     : Format_Rule;
   begin
      for I in 1 .. Format.Num_Rules loop
         Rule := Format.Rules (I);
         Position := Rule.Position;
         case Format.Rules (I).Rule_Type is
            when Space_Rule =>
               case Position is
                  when Before =>
                     Result.Space_Before := Rule;
                  when After =>
                     Result.Space_After := Rule;
               end case;
            when New_Line_Rule =>
               case Position is
                  when Before =>
                     if Format.Rules (I).Soft then
                        Result.Soft_New_Line_Before := Rule;
                     else
                        Result.New_Line_Before := Rule;
                     end if;
                  when After =>
                     if Format.Rules (I).Soft then
                        Result.Soft_New_Line_After := Rule;
                     else
                        Result.New_Line_After := Rule;
                     end if;
               end case;
            when Bracketing_Rule =>
               case Position is
                  when Before =>
                     Result.Opening := Rule;
                  when After =>
                     Result.Closing := Rule;
               end case;
            when others =>
               null;
         end case;
      end loop;
      return Result;
   end Rules;

   -------------------
   -- Soft_New_Line --
   -------------------

   function Soft_New_Line
     (Position : Rule_Position)
     return Format_Rule
   is
   begin
      return (New_Line_Rule, Position, Default_New_Line_Priority,
              Soft => True);
   end Soft_New_Line;

   -----------
   -- Space --
   -----------

   function Space (Add_Space  : Boolean;
                   Position   : Rule_Position;
                   Priority   : Rule_Priority)
                  return Format_Rule
   is
   begin
      return (Space_Rule, Position, Priority, Add_Space);
   end Space;

   ------------------
   -- Space_Always --
   ------------------

   function Space_Always    (Position : Rule_Position) return Format_Rule is
   begin
      return (Space_Rule, Position, Default_Space_Priority,
              Add_Space => True);
   end Space_Always;

   -----------------
   -- Space_Never --
   -----------------

   function Space_Never     (Position : Rule_Position) return Format_Rule is
   begin
      return (Space_Rule, Position, Default_No_Space_Priority,
              Add_Space => False);
   end Space_Never;

   ---------------------
   -- Space_Sometimes --
   ---------------------

   function Space_Sometimes (Position : Rule_Position) return Format_Rule is
   begin
      return (Space_Rule, Position, Low_Space_Priority, Add_Space => True);
   end Space_Sometimes;

   ---------------
   -- To_Format --
   ---------------

   function To_Format (Text : String) return Aquarius_Format is
      S : constant String :=
        Ada.Characters.Handling.To_Lower (Text);
   begin
      if S = "indented_child" then
         declare
            Rules : constant Array_Of_Rules :=
              [New_Line (Aquarius.Before),
               New_Line (Aquarius.After),
               Indent (Aquarius.Before, 3),
               Indent (Aquarius.After, -3)];
         begin
            return Make_Format (Rules);
         end;
      else
         return Make_Format (To_Rule (S));
      end if;

   end To_Format;

   -------------
   -- To_Rule --
   -------------

   function To_Rule (Text : String) return Format_Rule is
      S : constant String :=
        Ada.Characters.Handling.To_Lower (Text);
   begin
      if S = "space_before" then
         return Space_Always (Before);
      elsif S = "space_after" then
         return Space_Always (After);
      elsif S = "no_space_before" then
         return Space_Never (Before);
      elsif S = "no_space_after" then
         return Space_Never (After);
      elsif S = "new_line_after" then
         return New_Line (After);
      elsif S = "new_line_before" then
         return New_Line (Before);
      elsif S = "soft_new_line" or else S = "soft_new_line_before" then
         return Soft_New_Line (Before);
      elsif S = "soft_new_line_after" then
         return Soft_New_Line (After);
      elsif S = "indent_before" then
         return Indent (Before, 3);
      elsif S = "indent_after" then
         return Indent (After, 3);
      elsif S = "outdent_before" then
         return Indent (Before, -3);
      elsif S = "outdent_after" then
         return Indent (After, -3);
      elsif S = "indent_child" then
         return Indent_Child (3);
      elsif S = "opening" then
         return Opening;
      elsif S = "closing" then
         return Closing;
      else
         raise Constraint_Error with "unknown format rule: " & Text;
      end if;
   end To_Rule;

end Aquarius.Formats;
