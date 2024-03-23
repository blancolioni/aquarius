with Ada.Containers.Vectors;

with Aquarius.Programs.Arrangements.Logging;

package body Aquarius.Programs.Arrangements.Reformatting is

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
         if Tree.Is_Separator then
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
