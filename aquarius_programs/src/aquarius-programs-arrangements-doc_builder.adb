with Aquarius.Formats;

package body Aquarius.Programs.Arrangements.Doc_Builder is

   use Aquarius.Docs;
   use Aquarius.Formats;

   function First_Terminal_Of (Item : Program_Tree) return Program_Tree
   is (if Item.Is_Terminal then Item else Program_Tree (Item.First_Leaf));
   --  The actual terminal whose Space_Before rule governs spacing
   --  before Item, regardless of how deeply Item's first leaf is
   --  nested inside non-terminal wrappers.

   function Last_Terminal_Of (Item : Program_Tree) return Program_Tree
   is (if Item.Is_Terminal then Item else Program_Tree (Item.Last_Leaf));
   --  Likewise, the terminal whose Space_After rule governs spacing
   --  after Item.

   function Has_Content (Item : Program_Tree) return Boolean;
   --  True if Item's subtree contains at least one terminal with
   --  non-empty text. An EBNF optional/repeated construct that
   --  matched zero times (e.g. Wir's "routine_scope ::= ['public']"
   --  with no 'public', or an absent "[content]"/"[else ...]") still
   --  appears as a child -- whether as a non-terminal with no
   --  children, or one wrapping an empty-text terminal depends on
   --  the grammar engine, so this checks recursively rather than
   --  assuming a specific shape.

   function Is_Empty (Item : Program_Tree) return Boolean
   is (not Has_Content (Item));
   --  Must be skipped for separator purposes -- otherwise it is
   --  treated as a real neighbour on both sides, producing a double
   --  space / spurious space / phantom blank line where nothing is
   --  actually rendered.

   function Is_Breakable_Separator (Item : Program_Tree) return Boolean;
   --  Mirrors Reformatting.Breakable_Separator: a separator counts as
   --  a soft-break point even with only a plain trailing space and no
   --  explicit soft-new-line rule (Wir's own ',' relies on exactly
   --  this: "no_space_before space_after", no soft-line marker).

   function Want_Space
     (Left_After, Right_Before : Format_Rule) return Boolean;
   --  Ports Arrange_Terminal's Insert_Space decision
   --  (arrangements.adb) to a pure function over the two adjacent
   --  rules, rather than context state threaded incrementally.

   function Separator_Before
     (Previous : Program_Tree; Item : Program_Tree) return Doc;
   --  The Doc fragment joining two adjacent siblings: a hard Break, a
   --  soft Line, a plain Space, or nothing, in that priority order --
   --  matching how Arrange_Non_Terminal/Arrange_Terminal treat a hard
   --  new-line rule as overriding a soft one, which in turn overrides
   --  plain spacing.

   function Build (Item : Program_Tree; Leading : Doc) return Doc;
   --  Leading is the separator (Nil/Space/Line/Break) that belongs
   --  immediately before Item's own content. It is folded inside
   --  Item's own Nest (if Item introduces one), not concatenated
   --  outside it by the caller -- otherwise a break that enters a
   --  newly-indented block (e.g. 'is' before an indented
   --  statement_list) resets the column using the OUTER indent
   --  instead of the block's own, leaving its first line unindented.
   --  It is folded OUTSIDE Item's own Group, though: a hard Break as
   --  the first thing inside a Group makes that Group's own fits
   --  check trivially succeed (Fits treats reaching a Break as "the
   --  rest doesn't matter, whatever follows starts a fresh line"),
   --  which would force-flatten every group nested inside it
   --  regardless of its actual width.

   -----------
   -- Build --
   -----------

   function Build (Item : Program_Tree) return Doc is
   begin
      return Build (Item, Nil);
   end Build;

   -----------
   -- Build --
   -----------

   function Build (Item : Program_Tree; Leading : Doc) return Doc is
   begin
      if Item.Is_Terminal then
         return Leading & Leaf (Terminal_Node_Access (Item));
      end if;

      declare
         Format        : constant Aquarius_Format := Item.Syntax.Get_Format;
         Child_Indent  : constant Indentation_Offset :=
                           Formats.Indent_Child (Format);
         Before_Indent : constant Indentation_Offset :=
                           Formats.Indent (Format, Before);
         Content       : Doc := Nil;
         Previous      : Program_Tree := null;
      begin
         for I in 1 .. Item.Child_Count loop
            declare
               Child         : constant Program_Tree := Item.Program_Child (I);
               Child_Leading : Doc := Nil;
            begin
               if not Is_Empty (Child) then
                  if Previous /= null then
                     Child_Leading := Separator_Before (Previous, Child);
                  end if;
                  Previous := Child;
               end if;
               Content := Content & Build (Child, Child_Leading);
            end;
         end loop;

         if Child_Indent /= 0 or else Before_Indent /= 0 then
            return Nest
              (Integer (Child_Indent + Before_Indent),
               Leading & Group (Content));
         else
            return Leading & Group (Content);
         end if;
      end;
   end Build;

   ------------------
   -- Has_Content --
   ------------------

   function Has_Content (Item : Program_Tree) return Boolean is
   begin
      if Item.Is_Terminal then
         return Item.Text'Length > 0;
      end if;
      for I in 1 .. Item.Child_Count loop
         if Has_Content (Item.Program_Child (I)) then
            return True;
         end if;
      end loop;
      return False;
   end Has_Content;

   ----------------------------
   -- Is_Breakable_Separator --
   ----------------------------

   function Is_Breakable_Separator (Item : Program_Tree) return Boolean is
      Rules : constant Immediate_Rules := Item.Rules;
   begin
      return Item.Is_Separator
        and then ((Enabled (Rules.Space_After)
                   and then not Negative (Rules.Space_After))
                  or else Enabled (Rules.New_Line_After)
                  or else Enabled (Rules.Soft_New_Line_After));
   end Is_Breakable_Separator;

   -----------------------
   -- Separator_Before --
   -----------------------

   function Separator_Before
     (Previous : Program_Tree; Item : Program_Tree) return Doc
   is
      Prev_Rules : constant Immediate_Rules := Previous.Rules;
      Item_Rules : constant Immediate_Rules := Item.Rules;
   begin
      if Enabled (Prev_Rules.New_Line_After)
        or else Enabled (Item_Rules.New_Line_Before)
      then
         return Break;
      elsif Enabled (Prev_Rules.Soft_New_Line_After)
        or else Enabled (Item_Rules.Soft_New_Line_Before)
        or else Enabled (Item_Rules.Closing)
        or else Is_Breakable_Separator (Previous)
      then
         return Line;
      elsif Want_Space
        (Last_Terminal_Of (Previous).Rules.Space_After,
         First_Terminal_Of (Item).Rules.Space_Before)
      then
         return Space;
      else
         return Nil;
      end if;
   end Separator_Before;

   ----------------
   -- Want_Space --
   ----------------

   function Want_Space
     (Left_After, Right_Before : Format_Rule) return Boolean
   is
   begin
      if not Enabled (Right_Before) then
         return Enabled (Left_After) and then not Negative (Left_After);
      elsif not Negative (Right_Before) then
         return (not Enabled (Left_After))
           or else (not Negative (Left_After))
           or else Priority (Left_After) < Priority (Right_Before);
      elsif Enabled (Left_After) and then not Negative (Left_After) then
         return Priority (Left_After) >= Priority (Right_Before);
      else
         return False;
      end if;
   end Want_Space;

end Aquarius.Programs.Arrangements.Doc_Builder;
