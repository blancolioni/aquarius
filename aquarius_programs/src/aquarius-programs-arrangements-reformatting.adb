with Ada.Containers.Vectors;

with Aquarius.Formats;

with Aquarius.Programs.Arrangements.Logging;

package body Aquarius.Programs.Arrangements.Reformatting is

   function Breakable_Separator (Tree : Program_Tree) return Boolean;
   --  A separator is only a candidate reflow break point if it carries a
   --  trailing space or a (soft) new-line rule.  A separator such as the
   --  '.' in a dotted name has no-space rules on both sides and must never
   --  be broken, so it is excluded here.

   --------------------------
   -- Breakable_Separator --
   --------------------------

   function Breakable_Separator (Tree : Program_Tree) return Boolean is
      use Aquarius.Formats;
      Rules : constant Immediate_Rules := Tree.Rules;
   begin
      return Tree.Is_Separator
        and then ((Enabled (Rules.Space_After)
                   and then not Negative (Rules.Space_After))
                  or else Enabled (Rules.New_Line_After)
                  or else Enabled (Rules.Soft_New_Line_After));
   end Breakable_Separator;

   function Has_Separator_Descendant (Tree : Program_Tree) return Boolean;
   --  True if Tree has a breakable separator anywhere in its subtree.

   function Has_Separator_Descendant (Tree : Program_Tree) return Boolean is
   begin
      for C of Tree.Direct_Children (Skip_Separators => False) loop
         if Breakable_Separator (C)
           or else Has_Separator_Descendant (C)
         then
            return True;
         end if;
      end loop;
      return False;
   end Has_Separator_Descendant;

   function Governed_By_Content_Soft
     (Tree : Program_Tree;
      Top  : Program_Tree) return Boolean;
   --  True when an ancestor of Tree (up to Top) has a direct child that
   --  carries a soft-new-line rule but no separators of its own (a content
   --  soft operator such as '+').  Such a separator sits within an operand
   --  of a breaking operator and must not itself break; see the matching
   --  logic in Aquarius.Programs.Arrangements.Re_Arrange.

   function Governed_By_Content_Soft
     (Tree : Program_Tree;
      Top  : Program_Tree) return Boolean
   is
      Node : Program_Tree := Tree;

      function Is_Operator_Node (P : Program_Tree) return Boolean is
         Name   : constant String := P.Name;
         Suffix : constant String := "_operator";
      begin
         return Name'Length >= Suffix'Length
           and then Name (Name'Last - Suffix'Length + 1 .. Name'Last)
                    = Suffix;
      end Is_Operator_Node;

   begin
      while Node /= null and then Node /= Top loop
         Node := Program_Tree (Node.Parent);
         exit when Node = null;
         for C of Node.Direct_Children (Skip_Separators => False) loop
            if Is_Operator_Node (C)
              and then C.Has_Soft_New_Line_Rule_Before
              and then not Has_Separator_Descendant (C)
            then
               return True;
            end if;
         end loop;
      end loop;
      return False;
   end Governed_By_Content_Soft;

   type Reformat_Domain is
      record
         Top, Start, Finish : Program_Tree;
      end record;

   procedure Scan
     (Domain  : Reformat_Domain;
      Process : not null access
        procedure (Program : Program_Tree;
                   Depth   : Natural));

   type Separator_Info is
      record
         Syntax : Aquarius.Syntax.Syntax_Tree;
         Depth  : Natural;
      end record;

   function "<" (Left, Right : Separator_Info) return Boolean
   is (Left.Depth < Right.Depth);

   package Separator_Info_Vectors is
     new Ada.Containers.Vectors (Positive, Separator_Info);

   package Separator_Sorting is
     new Separator_Info_Vectors.Generic_Sorting ("<");

   procedure Find_Separators
     (Domain     : Reformat_Domain;
      Separators : out Separator_Info_Vectors.Vector);

   procedure Apply_Separator_New_Lines
     (Domain     : Reformat_Domain;
      Separators : Separator_Info_Vectors.Vector);

   procedure Fire_Container (Sep : Program_Tree; Top : Program_Tree);
   --  When a separator breaks, move the opening bracket of its enclosing
   --  list onto its own line by setting the soft-new-line rule on the
   --  nearest ancestor (bounded by Top) that carries one.

   --------------------
   -- Fire_Container --
   --------------------

   procedure Fire_Container (Sep : Program_Tree; Top : Program_Tree) is
      Container : Program_Tree := Program_Tree (Sep.Parent);
   begin
      while Container /= null
        and then Container /= Top
        and then not Container.Has_Soft_New_Line_Rule_Before
      loop
         Container := Program_Tree (Container.Parent);
      end loop;
      if Container /= null
        and then Container.Has_Soft_New_Line_Rule_Before
      then
         Container.Set_Soft_New_Line;
      end if;
   end Fire_Container;

   -------------------------------
   -- Apply_Separator_New_Lines --
   -------------------------------

   procedure Apply_Separator_New_Lines
     (Domain     : Reformat_Domain;
      Separators : Separator_Info_Vectors.Vector)
   is
      procedure Apply
        (Tree  : Program_Tree;
         Depth : Natural);

      -----------
      -- Apply --
      -----------

      procedure Apply
        (Tree  : Program_Tree;
         Depth : Natural)
      is
         use type Aquarius.Syntax.Syntax_Tree;
      begin
         if Tree.Syntax = Separators.First_Element.Syntax
           and then Depth = Separators.First_Element.Depth
         then
            Tree.Separator_NL := True;
            Fire_Container (Tree, Domain.Top);
         end if;
      end Apply;

   begin
      Scan (Domain, Apply'Access);
   end Apply_Separator_New_Lines;

   ---------------------
   -- Find_Separators --
   ---------------------

   procedure Find_Separators
     (Domain     : Reformat_Domain;
      Separators : out Separator_Info_Vectors.Vector)
   is

      procedure Check_Separator
        (Tree : Program_Tree;
         Depth : Natural);

      ---------------------
      -- Check_Separator --
      ---------------------

      procedure Check_Separator
        (Tree : Program_Tree;
         Depth : Natural)
      is
      begin
         if Breakable_Separator (Tree)
           and then not Governed_By_Content_Soft (Tree, Domain.Top)
         then
            declare
               use Aquarius.Syntax, Separator_Info_Vectors;
               Syntax : constant Syntax_Tree := Tree.Syntax;
               Position : constant Cursor :=
                            Separators.Find ((Syntax, Depth));
            begin
               if not Has_Element (Position) then
                  Separators.Append
                    (Separator_Info'(Syntax, Depth));
               end if;
            end;
         end if;
      end Check_Separator;

   begin
      Scan (Domain, Check_Separator'Access);
      Separator_Sorting.Sort (Separators);
   end Find_Separators;

   --------------
   -- Reformat --
   --------------

   procedure Reformat
     (Context : in out Contexts.Arrangement_Context;
      Top     : Program_Tree;
      Start   : Program_Tree;
      Finish  : Program_Tree)
   is
      Domain     : constant Reformat_Domain := (Top, Start, Finish);
      Separators : Separator_Info_Vectors.Vector;
   begin
      Find_Separators (Domain, Separators);
      for S of Separators loop
         Logging.Log (Context, Top, S.Depth'Img & ": " & S.Syntax.Image);
      end loop;
      if not Separators.Is_Empty then
         Apply_Separator_New_Lines (Domain, Separators);
      end if;
   end Reformat;

   ----------
   -- Scan --
   ----------

   procedure Scan
     (Domain  : Reformat_Domain;
      Process : not null access
        procedure (Program : Program_Tree;
                   Depth   : Natural))
   is
      Active : Boolean := False;

      procedure Perform_Scan
        (Tree  : Program_Tree;
         Depth : Natural);

      ------------------
      -- Perform_Scan --
      ------------------

      procedure Perform_Scan
        (Tree  : Program_Tree;
         Depth : Natural)
      is
      begin
         if Tree = Domain.Start then
            Active := True;
         end if;

         if Active then
            Process (Tree, Depth);
         end if;

         for I in 1 .. Tree.Child_Count loop
            Perform_Scan (Tree.Program_Child (I), Depth + 1);
         end loop;

         if Tree = Domain.Finish then
            Active := False;
         end if;

      end Perform_Scan;

   begin
      Perform_Scan (Domain.Top, 0);
   end Scan;

end Aquarius.Programs.Arrangements.Reformatting;
