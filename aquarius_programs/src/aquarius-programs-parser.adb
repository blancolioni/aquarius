with Ada.Text_IO;

with Aquarius.Errors;
with Aquarius.Properties;
with Aquarius.Syntax.Checks;

with Aquarius.Trace;

package body Aquarius.Programs.Parser is

   Free_Ambiguity_List : List_Of_Ambiguities.List;

   Ambiguity_Counters : Aquarius.Counters.Counter_Source;

   Enable_Trace : constant Boolean := False;

   type Parseable_Type is (Parseable_Token, Parseable_Tree);

   type Parseable (Class : Parseable_Type) is
      record
         First_Token   : Aquarius.Tokens.Token;
         Offset        : Aquarius.Locations.Location_Offset := 1;
         Line          : Aquarius.Locations.Line_Index      := 1;
         Column        : Aquarius.Locations.Column_Index    := 1;
         Subtree       : Program_Tree;
      end record;

   function Has_Ambiguities (Context : Parse_Context) return Boolean;
   --  Returns True if context ambiguity list has more than one
   --  element.  Compare with Is_Ambiguous, which returns True if
   --  context ambiguity list has more than one *active* element.

   function Make_Parseable
     (From_Token : Aquarius.Tokens.Token;
      Location   : Aquarius.Locations.Location_Interface'Class)
      return Parseable;

   function Make_Parseable (From_Tree  : Program_Tree)
                           return Parseable;

   procedure Update_Ambiguities (Context : in out Parse_Context);

   procedure Set_User_Whitespace
     (Context  : in out Parse_Context;
      Current  : List_Of_Ambiguities.Cursor;
      At_Tree  : Program_Tree);

   procedure Move_To_Right_Of_Parent
     (Context : in out Parse_Context;
      Current : List_Of_Ambiguities.Cursor);

   procedure Move_To_Left_Of_First_Child
     (Context : in out Parse_Context;
      Current : List_Of_Ambiguities.Cursor);

   function Token_OK
     (Item           : Parseable;
      Location       : Aquarius.Trees.Cursors.Cursor;
      Top            : Program_Tree)
      return Boolean;
   --  Never move above Top when looking for
   --  a good tree.  If Top = null, search will continue to the root

   function Token_OK
     (Item           : Parseable;
      Location       : Aquarius.Trees.Cursors.Cursor)
      return Boolean
   is (Token_OK (Item, Location, null));

   procedure Parse_Token
     (Item           : Parseable;
      Tok_Text       : String;
      Context        : in out Parse_Context);

   procedure Parse_Token
     (Item             : Parseable;
      Tok_Text         : String;
      Current          : List_Of_Ambiguities.Cursor;
      Context          : in out Parse_Context;
      No_Left_Repeater : Boolean := False);

   procedure Parse_Ambiguous_Token
     (Item             : Parseable;
      Tok_Text         : String;
      Current          : List_Of_Ambiguities.Cursor;
      Context          : in out Parse_Context;
      Parent           : Program_Tree;
      Right            : Program_Tree;
      Options          : Array_Of_Program_Trees);

   --  Create an ambiguous parse context for each of the options When
   --  resolved, the selected tree will be placed to the left of the
   --  tree named as Right (if right is not null).  If the selected
   --  tree has the same syntax as Right, it replaces it. If Right is
   --  null, the tree is added to Parent as the last child.

   procedure Parse_Into_Choice
     (Choice         : Program_Tree;
      Item           : Parseable;
      Tok_Text       : String;
      Current        : List_Of_Ambiguities.Cursor;
      Context        : in out Parse_Context);

   procedure Parse_Into_New_Children
     (Parent         : Program_Tree;
      Item           : Parseable;
      Tok_Text       : String;
      Current        : List_Of_Ambiguities.Cursor;
      Context        : in out Parse_Context);

   procedure Parse_Terminal
     (Tok      : Aquarius.Tokens.Token;
      Tok_Text : String;
      Current  : List_Of_Ambiguities.Cursor;
      Context  : in out Parse_Context);

   function New_Ambiguity
     (Current : List_Of_Ambiguities.Cursor;
      Parent  : Program_Tree;
      Child   : Program_Tree)
      return Ambiguity
   with Pre => Child /= null;

   function Show_Ambiguity
     (Item : Ambiguity)
      return String;

   procedure Parse_String (Context : in out Parse_Context;
                           Line    : String);

   procedure Free (A : in out Ambiguity);

--     procedure Retry_Choice
--       (Location       : in out Ambiguity;
--        New_Try        : in     Positive;
--        Tok            : in     Aquarius.Tokens.Token;
--        Tok_Pos        : in     Aquarius.Layout.Position;
--        Tok_Text       : in     String;
--        Context        : in out Parse_Context;
--        Recovered      :    out Boolean);

   -----------------
   -- Add_Comment --
   -----------------

   procedure Add_Comment
     (Context  : in out Parse_Context;
      Location : Aquarius.Locations.Location_Interface'Class;
      Comment  : Program_Tree)
   is
      pragma Unreferenced (Location);
      use type Aquarius.Locations.Line_Count;
   begin

      --  Comment.File_Start := Position;
      Context.Comments.Append (Comment);
      if Context.Vertical_Space > 0 then
         Comment.Set_Vertical_Gap_Before
           (Context.Vertical_Space);
         Context.Vertical_Space := 0;
      end if;
   end Add_Comment;

   ---------------
   -- Add_Error --
   ---------------

   procedure Add_Error (Context : in out Parse_Context;
                        Error   : Program_Tree)
   is
   begin
      Context.Errors.Append (Error);
   end Add_Error;

   --------------------
   -- Clear_Comments --
   --------------------

   procedure Clear_Comments (Context  : in out Parse_Context) is
   begin
      Context.Comments.Clear;
   end Clear_Comments;

   ------------------
   -- Finish_Parse --
   ------------------

   procedure Finish_Parse (Context : in out Parse_Context) is

      Current  : constant List_Of_Ambiguities.Cursor :=
        Context.Ambiguities.First;
      Location : Aquarius.Trees.Cursors.Cursor renames
        Context.Ambiguities.First_Element.Location;
   begin
      while not Aquarius.Trees.Cursors.Is_At_Root (Location) loop
         Move_To_Right_Of_Parent (Context, Current);
      end loop;
   end Finish_Parse;

   ----------
   -- Free --
   ----------

   procedure Free (A : in out Ambiguity) is
   begin
      Free_Ambiguity_List.Append (A);
      A := null;
   end Free;

   ----------------
   -- Get_Cursor --
   ----------------

   function Get_Cursor (Context : Parse_Context)
                       return Aquarius.Trees.Cursors.Cursor
   is
   begin
      return Context.Ambiguities.First_Element.Location;
   end Get_Cursor;

   ---------------------
   -- Has_Ambiguities --
   ---------------------

   function Has_Ambiguities (Context : Parse_Context) return Boolean is
      use List_Of_Ambiguities;
   begin
      return Has_Element (Next (Context.Ambiguities.First));
   end Has_Ambiguities;

   ------------------------------
   -- Initialise_Parse_Context --
   ------------------------------

   procedure Initialise_Parse_Context
     (Context     : out Parse_Context;
      Grammar     : Aquarius.Grammars.Aquarius_Grammar;
      Root        : Program_Tree;
      Interactive : Boolean;
      Run_Actions : Boolean := True)
   is
      use type Aquarius.Grammars.Aquarius_Grammar;
      Start_Ambiguity : constant Ambiguity :=
        New_Ambiguity (List_Of_Ambiguities.No_Element, null, Root);

   begin
      Context := (Grammar        => Grammar,
                  Root           => Root,
                  Ambiguities    => List_Of_Ambiguities.Empty_List,
                  Errors         => Program_Tree_Vector.Empty_Vector,
                  Comments       => Program_Tree_Vector.Empty_Vector,
                  Interactive    => Interactive,
                  Run_Actions    => Run_Actions,
                  Vertical_Space => 0,
                  Location       =>
                    Locations.To_Location
                      (Root.Offset, Root.Line, Root.Column));
      if Grammar = null then
         Context.Grammar :=
           Aquarius.Grammars.Aquarius_Grammar
             (Root.Property (Aquarius.Properties.Grammar_Property));
      end if;
      Context.Ambiguities.Append (Start_Ambiguity);
   end Initialise_Parse_Context;

   ------------------
   -- Is_Ambiguous --
   ------------------

   function Is_Ambiguous (Context : Parse_Context) return Boolean is
      use List_Of_Ambiguities;
      Result : Boolean := False;
      It     : Cursor := Context.Ambiguities.First;
      Found  : Boolean := False;
   begin
      while Has_Element (It) loop
         if Element (It).Active then
            if Found then
               Result := True;
               exit;
            else
               Found := True;
            end if;
         end if;
         Next (It);
      end loop;

      --  if Result then
      --     Ada.Text_IO.Put_Line ("Ambiguous context");
      --     declare
      --        A  : Ambiguity;
      --     begin
      --        It := Context.Ambiguities.First;
      --        while Has_Element (It) loop
      --           A := Element (It);
      --           if A.Active then
      --              Ada.Text_IO.Put ("[active]   ");
      --           else
      --              Ada.Text_IO.Put ("[inactive] ");
      --           end if;
      --           Ada.Text_IO.Put_Line
      --             (Aquarius.Trees.Cursors.Image (A.Location));
      --           Next (It);
      --        end loop;
      --     end;
      --  end if;

      return Result;

   end Is_Ambiguous;

   --------------------
   -- Make_Parseable --
   --------------------

   function Make_Parseable
     (From_Token : Aquarius.Tokens.Token;
      Location   : Aquarius.Locations.Location_Interface'Class)
      return Parseable
   is
   begin
      return Parseable'
        (Class       => Parseable_Token,
         First_Token => From_Token,
         Offset      => Location.Offset,
         Line        => Location.Line,
         Column      => Location.Column,
         Subtree     => null);
   end Make_Parseable;

   --------------------
   -- Make_Parseable --
   --------------------

   function Make_Parseable
     (From_Tree  : Program_Tree)
      return Parseable
   is

      function First_Token (T : Program_Tree) return Program_Tree;

      -----------------
      -- First_Token --
      -----------------

      function First_Token (T : Program_Tree) return Program_Tree is
      begin
         if T.Is_Terminal then

            if T.Is_Filled then
               return T;
            else
               return null;
            end if;
         else
            declare
               Ts     : constant Array_Of_Program_Trees := T.Direct_Children;
               Result : Program_Tree;
            begin
               for I in Ts'Range loop
                  Result := First_Token (Ts (I));
                  if Result /= null then
                     return Result;
                  end if;
               end loop;
               return null;
            end;
         end if;
      end First_Token;

      Tok : constant Program_Tree := First_Token (From_Tree);
   begin
      return Parseable'
        (Class       => Parseable_Tree,
         First_Token => Tok.Get_Token,
         Offset      => Tok.Offset,
         Line        => Tok.Line,
         Column      => Tok.Column,
         Subtree     => From_Tree);
   end Make_Parseable;

   ---------------------------------
   -- Move_To_Left_Of_First_Child --
   ---------------------------------

   procedure Move_To_Left_Of_First_Child
     (Context : in out Parse_Context;
      Current : List_Of_Ambiguities.Cursor)
   is
      use Aquarius.Trees.Cursors;
      A : constant Ambiguity := List_Of_Ambiguities.Element (Current);
   begin
      if Context.Run_Actions and then not Has_Ambiguities (Context) then
         Aquarius.Grammars.Run_Parse_Actions
           (Tree     => Program_Tree (Get_Right_Tree (A.Location)).all,
            Position => Before);
      end if;

      if Program_Tree (Get_Right_Tree (A.Location)) = A.Parent then
         A.Location := Left_Of_Tree (A.Top);
      else
         Move_To_Left_Of_First_Child (A.Location);
      end if;
   end Move_To_Left_Of_First_Child;

   -----------------------------
   -- Move_To_Right_Of_Parent --
   -----------------------------

   procedure Move_To_Right_Of_Parent
     (Context : in out Parse_Context;
      Current : List_Of_Ambiguities.Cursor)
   is
      use Aquarius.Trees.Cursors;
      A        : constant Ambiguity := List_Of_Ambiguities.Element (Current);
      Location : Cursor renames A.Location;
      Program : constant Program_Tree := Program_Tree (Get_Tree (Location));
   begin

      if Has_Ambiguities (Context) and then
        List_Of_Ambiguities.Element (Current).Parent = Program.Program_Parent
      then

         --  This indicates an unresolvable ambiguity, because
         --  once we leave the sub-tree that contains the
         --  ambiguity, no further disambiguation is available.

         declare
            use List_Of_Ambiguities;
            It : List_Of_Ambiguities.Cursor := Context.Ambiguities.Last;
         begin

            Aquarius.Errors.Error (A.Top, "ambiguity was not resolved");

            while Has_Element (It) loop
               if It /= Current and then Element (It).Parent = A.Parent then
                  declare
                     Prev : constant List_Of_Ambiguities.Cursor :=
                       Previous (It);
                     Lose : Ambiguity := Element (It);
                  begin
                     Free (Lose);
                     Delete (Context.Ambiguities, It);
                     It := Prev;
                  end;
               else
                  Previous (It);
               end if;
            end loop;

            if Has_Element (A.Previous) then
               declare
                  Prev : Ambiguity := Element (A.Previous);
               begin
                  Delete (Context.Ambiguities, A.Previous);
                  A.Parent.Add_Child (A.Top);
                  A.all := Prev.all;
                  A.Active := True;
                  Free (Prev);
               end;
            end if;
         end;
      end if;

      if Context.Run_Actions and then not Has_Ambiguities (Context) then

         if Program.Program_Parent /= null then
            Aquarius.Grammars.Run_Parse_Actions
              (Tree     => Program.Program_Parent.all,
               Position => After);
         else
            raise Constraint_Error with
            Program.Show_Location & ": expected this to have a parent";
         end if;
      end if;

      Move_To_Right_Of_Parent (Location);

   end Move_To_Right_Of_Parent;

   -------------------
   -- New_Ambiguity --
   -------------------

   function New_Ambiguity
     (Current : List_Of_Ambiguities.Cursor;
      Parent  : Program_Tree;
      Child   : Program_Tree)
     return Ambiguity
   is
      use List_Of_Ambiguities;
      A : Ambiguity;
   begin

      if Free_Ambiguity_List.Is_Empty then
         A := new Ambiguity_Record;
      else
         A := Free_Ambiguity_List.First_Element;
         Free_Ambiguity_List.Delete_First;
      end if;
      A.all := (Active     => True,
                Identity   => Ambiguity_Counters.Next,
                Parent     => Parent,
                Top        => Child,
                Right      => null,
                Last_Parse => null,
                Location   => Aquarius.Trees.Cursors.Left_Of_Tree (Child),
                Previous   => Current);
      return A;
   end New_Ambiguity;

   ---------------------------
   -- Parse_Ambiguous_Token --
   ---------------------------

   procedure Parse_Ambiguous_Token
     (Item             : Parseable;
      Tok_Text         : String;
      Current          : List_Of_Ambiguities.Cursor;
      Context          : in out Parse_Context;
      Parent           : Program_Tree;
      Right            : Program_Tree;
      Options          : Array_Of_Program_Trees)
   is
      This_A      : constant Ambiguity :=
        List_Of_Ambiguities.Element (Current);
   begin
      if Enable_Trace then
         Aquarius.Trace.Trace_Put_Line
           (Aquarius.Trace.Parsing,
            "Parse_Ambiguous_Token: [" & Tok_Text & "]");
      end if;

--        if Right /= null then
--           Ada.Text_IO.Put_Line ("   right: " & Right.Image);
--        end if;

      This_A.Active := False;
      for I in Options'Range loop
         declare
            Child : constant Program_Tree := Options (I);
            A     : constant Ambiguity :=
              New_Ambiguity (Current, Parent, Child);
         begin
            if Enable_Trace then
               Aquarius.Trace.Trace_Put_Line
                 (Aquarius.Trace.Parsing,
                  "  option" & I'Img &
                    ": " & Child.Image);
            end if;

            Child.Set_Foster_Parent (Parent);
            Child.Expand_All;
            A.Location :=
              Aquarius.Trees.Cursors.Left_Of_Tree (Child);
            A.Right := Right;
            Aquarius.Trees.Cursors.Move_To_Left_Of_First_Child (A.Location);

            Context.Ambiguities.Append (A);
            Parse_Token (Item, Tok_Text,
                         Context.Ambiguities.Last, Context);
         end;
      end loop;

   end Parse_Ambiguous_Token;

   -----------------------
   -- Parse_Into_Choice --
   -----------------------

   procedure Parse_Into_Choice
     (Choice         : Program_Tree;
      Item           : Parseable;
      Tok_Text       : String;
      Current        : List_Of_Ambiguities.Cursor;
      Context        : in out Parse_Context)
   is
      Tok         : constant Aquarius.Tokens.Token := Item.First_Token;
      Match_Count : Natural := 0;
      Match       : array (1 .. Choice.Syntax.Child_Count) of Boolean :=
        [others => False];
      Match_Index : Positive;
      Syn         : constant Aquarius.Syntax.Syntax_Tree :=
        Choice.Syntax;
      This_A      : constant Ambiguity :=
        List_Of_Ambiguities.Element (Current);
      Bad_Match   : Natural := 0;
      --  Bad_Match is a child which syntactically matches, but
      --  doesn't satisfy the precondition.  We keep track of it so we
      --  can distinguish between failures caused by no matches, and
      --  failures caused by only bad matches
   begin
      for I in 1 .. Syn.Child_Count loop
         if Aquarius.Syntax.Checks.Begins
           (Tok, Syn.Syntax_Child (I))
         then
            if Syn.Syntax_Child (I).Check_Precondition (Choice) then
               Match_Count := Match_Count + 1;
               Match_Index := I;
               Match (I) := True;
            else
               Bad_Match := I;
            end if;
         end if;
      end loop;

      if Match_Count = 0 and then Bad_Match = 0 then
         raise Constraint_Error with
           "no parse opportunities at " & Choice.Show_Location;
      elsif Match_Count = 1 or else
        (Match_Count = 0 and then Bad_Match /= 0)
      then

         --  If only bad matches are available, we take the first and
         --  ignore the ambiguity.  This is probably not optimal (we
         --  should really treat this as a normal ambiguous tree, and
         --  see if it's resolved syntactically).

         if Match_Count = 0 then
            Match_Index := Bad_Match;
         end if;

         declare
            Child       : constant Program_Tree :=
                            New_Program_Tree
                              (Syn.Syntax_Child (Match_Index),
                               Choice.Source,
                               Choice.all);
         begin
            Choice.Add_Child (Child);
            Child.Expand;
            Move_To_Left_Of_First_Child (Context, Current);
            Parse_Token (Item, Tok_Text,
                         Current, Context);
         end;
      else

         if Enable_Trace then
            Aquarius.Trace.Trace_Put_Line
              (Aquarius.Trace.Parsing,
               "Choice has" & Natural'Image (Match_Count) & " matches: "
               & Choice.Path_Image);
         end if;

         This_A.Active := False;
         for I in 1 .. Syn.Child_Count loop
            if Match (I) then
               declare
                  Child : constant Program_Tree :=
                            New_Program_Tree
                              (Syn.Syntax_Child (I),
                               Choice.Source,
                               Choice.all);
                  A     : constant Ambiguity :=
                    New_Ambiguity (Current, Choice, Child);
               begin
                  Child.Set_Foster_Parent (Choice);
                  Child.Expand;
                  Context.Ambiguities.Append (A);
                  Parse_Token (Item, Tok_Text,
                               Context.Ambiguities.Last, Context);
               end;
            end if;
         end loop;
      end if;

   end Parse_Into_Choice;

   -----------------------------
   -- Parse_Into_New_Children --
   -----------------------------

   procedure Parse_Into_New_Children
     (Parent         : Program_Tree;
      Item           : Parseable;
      Tok_Text       : String;
      Current        : List_Of_Ambiguities.Cursor;
      Context        : in out Parse_Context)
   is
      Tok         : constant Aquarius.Tokens.Token := Item.First_Token;
      Syn         : constant Aquarius.Syntax.Syntax_Tree :=
        Parent.Syntax;
      This_A      : constant Ambiguity :=
        List_Of_Ambiguities.Element (Current);
      Location    : Aquarius.Trees.Cursors.Cursor renames This_A.Location;

      function Has_Ambiguous_Optional return Boolean;
      --  Returns true if Parent has exactly one child, this child has
      --  at least two children, the first grandchild of Parent is
      --  nullable, and Tok begins it, and Tok also begins the second
      --  grandchild.
      --  Obviously, this is not very general.

      function Make_Subtree return Program_Tree;
      --  Make a new program tree from the syntax of the (only) child
      --  of Parent.

      ----------------------------
      -- Has_Ambiguous_Optional --
      ----------------------------

      function Has_Ambiguous_Optional return Boolean is
      begin
         return Syn.Child_Count = 1 and then
           Syn.Child (1).Child_Count >= 2 and then
           Aquarius.Syntax.Checks.Begins
           (Tok,
            Syn.Syntax_Child (1).Syntax_Child (1)) and then
           Aquarius.Syntax.Checks.Begins
           (Tok,
            Syn.Syntax_Child (1).Syntax_Child (2)) and then
           Aquarius.Syntax.Checks.Nullable
           (Syn.Syntax_Child (1).Syntax_Child (1));
      end Has_Ambiguous_Optional;

      ------------------
      -- Make_Subtree --
      ------------------

      function Make_Subtree return Program_Tree is
         pragma Assert (Syn.Child_Count = 1);
         Result : constant Program_Tree :=
                    New_Program_Tree
                      (Syn.Syntax_Child (1),
                       Parent.Source,
                       Parent.all);
      begin
         Result.Set_Foster_Parent (Parent);

         --  Can't call expand because it doesn't add children to
         --  optional nodes.

         for I in 1 .. Syn.Syntax_Child (1).Child_Count loop
            Result.Add_Child
              (New_Program_Tree
                 (Syn.Syntax_Child (1).Syntax_Child (I),
                  Parent.Source, Parent.all));
         end loop;
         return Result;
      end Make_Subtree;

   begin

      --  A nullable node at the beginning can cause
      --  an ambiguity at this point.  So could one in
      --  the middle, but we don't handle that yet.

      if Has_Ambiguous_Optional then

         This_A.Active := False;

         declare
            use Aquarius.Trees.Cursors;
            With_Optional    : constant Program_Tree := Make_Subtree;
            Without_Optional : constant Program_Tree := Make_Subtree;
            A : Ambiguity;
         begin

            if Enable_Trace then
               Aquarius.Trace.Trace_Put_Line
                 (Aquarius.Trace.Parsing,
                  "Ambiguous optional node at [" & Tok_Text & "]");
               Aquarius.Trace.Trace_Put_Line
                 (Aquarius.Trace.Parsing,
                  "    parent node is [" & Parent.Image & "]");
               Aquarius.Trace.Trace_Put_Line
                 (Aquarius.Trace.Parsing,
                  "    optional node is [" & With_Optional.Image & "]");
            end if;

            A := New_Ambiguity (Current, Parent, With_Optional);
            A.Location :=
              Aquarius.Trees.Cursors.Left_Of_Tree (With_Optional);
            Move_To_Left_Of_First_Child (A.Location);

            Context.Ambiguities.Append (A);
            Parse_Token (Item, Tok_Text,
                         Context.Ambiguities.Last, Context);

            A := New_Ambiguity (Current, Parent, Without_Optional);
            A.Location :=
              Aquarius.Trees.Cursors.Left_Of_Tree (Without_Optional);
            Move_To_Left_Of_First_Child (A.Location);
            Move_Right (A.Location);

            --  HACK: don't create a new left repeater, since
            --  we would have already handled that.
            Context.Ambiguities.Append (A);
            Parse_Token (Item, Tok_Text,
                         Context.Ambiguities.Last, Context,
                         No_Left_Repeater => True);

         end;

      else

         for I in 1 .. Syn.Child_Count loop
            declare
               New_Child : constant Program_Tree :=
                             New_Program_Tree
                               (Syn.Syntax_Child (I),
                                Parent.Source, Parent.all);
            begin

               Parent.Add_Child (New_Child);
               New_Child.Expand;
            end;
         end loop;

         Move_To_Left_Of_First_Child (Context, Current);

         while not Aquarius.Syntax.Checks.Begins
           (Tok,
            Program_Tree
              (Aquarius.Trees.Cursors.Get_Right_Tree (Location)).Syntax)
         loop
            Aquarius.Trees.Cursors.Move_Right (Location);
         end loop;

         Parse_Token (Item, Tok_Text,
                      Current, Context);

      end if;

   end Parse_Into_New_Children;

   ------------------
   -- Parse_String --
   ------------------

   procedure Parse_String (Context : in out Parse_Context;
                           Line    : String)
   is
      Next, First       : Natural := Line'First;
      Class             : Aquarius.Tokens.Token_Class;
      Tok               : Aquarius.Tokens.Token;
      Complete          : Boolean;
      Unique            : Boolean;
      Have_Class        : Boolean;
      Have_Error        : Boolean;
      Grammar           : constant Aquarius.Grammars.Aquarius_Grammar :=
        Context.Grammar;
   begin
      while First <= Line'Last loop
         Have_Error := False;

         Aquarius.Tokens.Scan (Grammar.Frame, Line (Line'First .. Line'Last),
                               False, Complete, Have_Class, Unique,
                               Class, Tok, First, Next, null);
         if Have_Class then
            if Token_OK (Tok, Context) then
               Parse_Token (Tok, Line (First .. Next), Context);
            else
               raise Constraint_Error with "Parse_Line" &
                 ": syntax error at " &
                 Line (First .. Next) & " on [" & Line & "]";
            end if;
         else
            Have_Error := True;
            --  Next is set to zero by the token scanner
            --  if we don't get a class back.
            Next := First;
         end if;

         if Have_Error then
            Add_Error (Context,
                       Grammar.Make_Error_Tree
                         (Source   => Context.Root.Source,
                          Location => Context,
                          Message  => Line (First .. Next)));
         end if;

         First := Next + 1;

      end loop;

   end Parse_String;

--           for I in 1 .. Syn.Child_Count loop
--              if Aquarius.Syntax.Checks.Begins
--                (Tok, Syn.Syntax_Child (I))
--           then
--             if Syn.Syntax_Child (I).Check_Precondition (Choice) then
--                Match_Count := Match_Count + 1;
--                Match_Index := I;
--                Match (I) := True;
--             else
--                Bad_Match := I;
--             end if;
--           else
--              exit;
--           end if;
--           exit when not Aquarius.Syntax.Checks.Nullable
   --      (Syn.Syntax_Child (I));
--        end loop;

--        if Match_Count = 0 and Bad_Match = 0 then
--           raise Constraint_Error with
--             "no parse opportunities at " &
--             Aquarius.Source.Show (Tok_Pos);
--        end if;

--        if Match_Count = 1 or (Match_Count = 0 and Bad_Match /= 0) then

--           for I in 1 .. Syn.Child_Count loop
--              declare
--                 New_Child : constant Program_Tree :=
--                   New_Program_Tree (Syn.Syntax_Child (I));
--              begin

--                 Program.Add_Child (New_Child);
--                 Expand (New_Child);
--              end;
--           end loop;

--           Move_To_Left_Of_First_Child (Context, Current);

--           while not Aquarius.Syntax.Checks.Begins
--             (Tok,
--              Program_Tree (Get_Right_Tree (Location)).Syntax)
--           loop
--              Move_Right (Location);
--           end loop;

--           Parse_Token (Tok, Tok_Pos, Tok_Text,
--                        Current, Context);

--        else
--           pragma Assert (Program.Parent.Child_Count = 1);
--           Program.Parent.Remove_Child (Program);

--           for I in Match'Range loop
--              exit when not Match (I);

--              This_A.Active := False;

--              for J in 1 .. Syn.Child_Count loop
--                 declare
--                    New_Child : constant Program_Tree :=
--                      New_Program_Tree (Syn.Syntax_Child (I));
--                 begin

--                 Program.Add_Child (New_Child);
--                 Expand (New_Child);
--              end;
--           end loop;

--           Move_To_Left_Of_First_Child (Context, Current);

--           while not Aquarius.Syntax.Checks.Begins
--             (Tok,
--              Program_Tree (Get_Right_Tree (Location)).Syntax)
--           loop
--              Move_Right (Location);
--           end loop;

--           Parse_Token (Tok, Tok_Pos, Tok_Text,
--                        Current, Context);

--           end loop;

--        for I in 1 .. Syn.Child_Count loop
--           if Aquarius.Syntax.Checks.Begins
--             (Tok, Syn.Syntax_Child (I))
--           then
--             if Syn.Syntax_Child (I).Check_Precondition (Choice) then
--                Match_Count := Match_Count + 1;
--                Match_Index := I;
--                Match (I) := True;
--             else
--                Bad_Match := I;
--             end if;
--           else
--              exit;
--           end if;
--           exit when not Nullable (Syn.Syntax_Child (I));
--        end loop;

--           if Match_Count = 0 then
--              Match_Index := Bad_Match;
--           end if;

--           declare
--              Child       : constant Program_Tree :=
--                New_Program_Tree (Syn.Syntax_Child (Match_Index));
--           begin
--              Choice.Add_Child (Child);
--              Debug_Dump_Program (Choice);
--              Child.Expand;
--              Move_To_Left_Of_First_Child (Context, Current);
--              Parse_Token (Tok, Tok_Pos, Tok_Text,
--                           Current, Context);
--           end;
--        else
--           This_A.Active := False;
--           for I in 1 .. Syn.Child_Count loop
--              if Match (I) then
--                 declare
--                    Child : constant Program_Tree :=
--                      New_Program_Tree (Syn.Syntax_Child (I));
--                    A     : constant Ambiguity :=
--                      New_Ambiguity (Current, Choice, Child);
--                 begin
--                    Child.Set_Foster_Parent (Choice);
--                    Child.Expand;
--                    Context.Ambiguities.Append (A);
--                    Parse_Token (Tok, Tok_Pos, Tok_Text,
--                                 Context.Ambiguities.Last, Context);
--                 end;
--              end if;
--           end loop;
--        end if;

--        for I in 1 .. Syn.Child_Count loop
--           declare
--              New_Child : constant Program_Tree :=
--                New_Program_Tree (Syn.Syntax_Child (I));
--           begin

--              Program.Add_Child (New_Child);
--              Expand (New_Child);
--           end;
--        end loop;

--        Move_To_Left_Of_First_Child (Context, Current);

--        while not Aquarius.Syntax.Checks.Begins
--          (Tok,
--           Program_Tree (Get_Right_Tree (Location)).Syntax)
--        loop
--           Move_Right (Location);
--        end loop;

--        Parse_Token (Tok, Tok_Pos, Tok_Text,
--                     Current, Context);

--     end Parse_Into_New_Children;

   --------------------
   -- Parse_Terminal --
   --------------------

   procedure Parse_Terminal
     (Tok      : Aquarius.Tokens.Token;
      Tok_Text : String;
      Current  : List_Of_Ambiguities.Cursor;
      Context  : in out Parse_Context)
   is
      use Aquarius.Trees.Cursors;
      use type Aquarius.Tokens.Token;
      A : constant Ambiguity := List_Of_Ambiguities.Element (Current);
      Location : Cursor renames A.Location;
      Program : constant Program_Tree :=
        Program_Tree (Get_Right_Tree (Location));
   begin
      pragma Assert (not Program.Is_Filled);
      pragma Assert (Program.Syntax.Token = Tok);
      if Tok_Text /= "" then
         Program.Fill (Tok_Text);
      else
         Program.Fill;
      end if;

      Set_User_Whitespace (Context, Current, Program);
      Program.Update_Location (Context);

--        if Tok_Pos /= Aquarius.Source.No_Source_Position then
--           declare
--              use Aquarius.Layout;
--           begin
--              Program.Start_Position :=
--                (Positive_Count (Aquarius.Source.Get_Line (Tok_Pos)),
--                 Positive_Count (Aquarius.Source.Get_Column (Tok_Pos)));
--              Program.File_Start := Program.Start_Position;
--              Program.End_Position := (Program.Start_Position.Line,
--                                       Program.Start_Position.Column
--                                       + Count (Tok_Text'Length));
--           end;
--        end if;

      if Context.Run_Actions and then not Has_Ambiguities (Context) then

         Aquarius.Grammars.Run_Parse_Actions
           (Tree     => Program.all,
            Position => After);
      end if;

      Aquarius.Trees.Cursors.Move_Right (Location);

      A.Last_Parse := Program;

   end Parse_Terminal;

   -----------------
   -- Parse_Token --
   -----------------

   procedure Parse_Token
     (Tok      : Aquarius.Tokens.Token;
      Tok_Text : String;
      Context  : in out Parse_Context)
   is
   begin
      Parse_Token (Make_Parseable (Tok, Context), Tok_Text, Context);
   end Parse_Token;

   -----------------
   -- Parse_Token --
   -----------------

   procedure Parse_Token
     (Item           : Parseable;
      Tok_Text       : String;
      Context        : in out Parse_Context)
   is
      use List_Of_Ambiguities;
      It : Cursor := Context.Ambiguities.Last;
      T  : Cursor;
      A  : Ambiguity;
      Count : Natural := 0;
   begin

      if Enable_Trace then
         Aquarius.Trace.Trace_Put_Line
           (Aquarius.Trace.Parsing,
            "Parse: " & Tok_Text);
      end if;

      while Has_Element (It) loop
         if Element (It).Active then
            if Token_OK (Item, Element (It).Location) then
               if Enable_Trace then
                  Aquarius.Trace.Trace_Put_Line
                    (Aquarius.Trace.Parsing,
                     "  into: " & Show_Ambiguity (Element (It)));
               end if;

               Count := Count + 1;
               Parse_Token (Item, Tok_Text, It, Context);
               Previous (It);
            else
               if Enable_Trace then
                  Aquarius.Trace.Trace_Put_Line
                    (Aquarius.Trace.Parsing,
                     "  [deleting: " & Show_Ambiguity (Element (It)));
               end if;

               T := It;
               Previous (It);
               A := Element (T);
               Free (A.Top);
               Free (A);
               Delete (Context.Ambiguities, T);
            end if;
         else
            if Enable_Trace then
               Aquarius.Trace.Trace_Put_Line
                 (Aquarius.Trace.Parsing,
                  "  [skip inactive: " & Show_Ambiguity (Element (It)));
            end if;

            Previous (It);
         end if;
      end loop;

      if Count = 0 then
         raise Constraint_Error with "no parses";
      end if;

      --  It := OK.Last;
      --  while Has_Element (It) loop
      --     Parse_Token (Item, Tok_Text, It, Context);
      --     Previous (It);
      --  end loop;

      Update_Ambiguities (Context);

      if Enable_Trace then
         for A of Context.Ambiguities loop
            Aquarius.Trace.Trace_Put_Line
              (Aquarius.Trace.Parsing,
               "  keeping: " & Show_Ambiguity (A));
         end loop;
      end if;

      if Enable_Trace then
         declare
            Active_Count : Natural := 0;
         begin
            for A of Context.Ambiguities loop
               if A.Active then
                  Active_Count := Active_Count + 1;
               end if;
            end loop;

            Aquarius.Trace.Trace_Put_Line
              (Aquarius.Trace.Parsing,
               "active ambiguity count at "
               & "'" & Tok_Text & "'"
               & Item.Offset'Image
               & ":"
               & Natural'Image (Active_Count)
               & " of"
               & Natural'Image (Natural (Context.Ambiguities.Length)));
         end;
      end if;

   end Parse_Token;

   -----------------
   -- Parse_Token --
   -----------------

   procedure Parse_Token
     (Item             : Parseable;
      Tok_Text         : String;
      Current          : List_Of_Ambiguities.Cursor;
      Context          : in out Parse_Context;
      No_Left_Repeater : Boolean := False)
   is
      use Aquarius.Locations;
      use Aquarius.Syntax;
      use Aquarius.Trees.Cursors;

      Tok         : constant Aquarius.Tokens.Token := Item.First_Token;
      A        : constant Ambiguity := List_Of_Ambiguities.Element (Current);
      Location : Aquarius.Trees.Cursors.Cursor renames A.Location;
      Column      : constant Aquarius.Locations.Column_Index :=
                      Context.Column;

      procedure Parse_Into_New_Repeater
        (Repeater   : Program_Tree;
         Right_Tree : Program_Tree := null);

      -----------------------------
      -- Parse_Into_New_Repeater --
      -----------------------------

      procedure Parse_Into_New_Repeater
        (Repeater   : Program_Tree;
         Right_Tree : Program_Tree := null)
      is
         Syn    : constant Syntax_Tree := Repeater.Syntax;
      begin

         if not Is_Empty (Syn.Separator) then

            --  Because of our precondition, we know that Tok
            --  is a separator symbol
            declare
               New_Separator : Program_Tree;
               New_Repeater  : Program_Tree;
            begin
               New_Separator :=
                 New_Program_Tree (Syn.Separator, Repeater.Source, Context);
               New_Separator.Separator_Node := True;
               Repeater.Add_Child (New_Separator);
               New_Repeater :=
                 New_Program_Tree (Syntax_Tree (Syn.First_Child),
                                   Repeater.Source, Context);
               Repeater.Add_Child (New_Repeater);
               New_Repeater.Expand;
               Location := Left_Of_Tree (New_Separator);
               Parse_Token (Item, Tok_Text, Current, Context);
            end;

         else

            --  Add a new repeater tree
            declare
               New_Repeater : constant Program_Tree :=
                                New_Program_Tree
                                  (Syntax_Tree (Syn.First_Child),
                                   Repeater.Source, Context);
            begin
               if Right_Tree = null then
                  Add_Child (Repeater, New_Repeater);
               else
                  Right_Tree.Add_Left_Sibling (New_Repeater);
               end if;

               New_Repeater.Expand;
               Location := Left_Of_Tree (New_Repeater);
               Parse_Token (Item, Tok_Text, Current, Context);

            end;

         end if;

      end Parse_Into_New_Repeater;

   begin

      if not Is_Off_Right (Location) then
         Program_Tree (Get_Right_Tree (Location)).Update_Location (Context);
      end if;

      --  pragma Assert (Token_OK (Tok, Tok_Pos, Location, Search_Parents));

      while not Is_Off_Right (Location) and then
        Program_Tree (Get_Right_Tree (Location)).Is_Comment
      loop
         Move_Right (Location);
      end loop;

      if not Is_Off_Left (Location) and then
        not No_Left_Repeater
      then

         declare
            Program : constant Program_Tree :=
              Program_Tree
              (Aquarius.Trees.Cursors.Get_Left_Tree (Location));
            Right_Tree : constant Program_Tree :=
              Program_Tree
              (Aquarius.Trees.Cursors.Get_Right_Tree (Location));
            Syn    : constant Syntax_Tree := Program.Syntax;
         begin

            if Syn.Repeatable and then
              Column >= Program.Minimum_Indent and then
              (((not Is_Empty (Syn.Separator)) and then
                  Aquarius.Syntax.Checks.Begins (Tok, Syn.Separator))
               or else
                 (Is_Empty (Syn.Separator) and then
                    Aquarius.Syntax.Checks.Begins (Tok, Syn)))
            then

               --  If the current token does not begin the next
               --  program tree, then we can go straight to the
               --  repeater.  However, if it does, and if the repeater
               --  is nullable (lower bound is zero), then create an
               --  ambiguity

               if not Is_Off_Right (Location) and then
                 Aquarius.Syntax.Checks.Begins
                 (Tok, Right_Tree.Syntax)
               then
                  Parse_Ambiguous_Token
                    (Item       => Item,
                     Tok_Text   => Tok_Text,
                     Current    => Current,
                     Context    => Context,
                        Parent     => Program.Program_Parent,
                     Right      => Right_Tree,
                     Options    => [New_Program_Tree
                                    (Syn, Program.Source, Program.all),
                                    New_Program_Tree
                                      (Right_Tree.Syntax,
                                       Program.Source, Program.all)]);
               else
                  Parse_Into_New_Repeater (Program);
               end if;
               return;

            end if;

         end;

      end if;

      if Is_Off_Right (Location) then

         declare
            Program : constant Program_Tree :=
              Program_Tree (Get_Left_Tree (Location));
            Syn    : constant Syntax_Tree := Program.Syntax;
         begin

            if Syn.Repeatable and then
              Column >= Program.Minimum_Indent and then
              ((not Is_Empty (Syn.Separator) and then
                  Aquarius.Syntax.Checks.Begins (Tok, Syn.Separator))
               or else
                 (Is_Empty (Syn.Separator) and then
                    Aquarius.Syntax.Checks.Begins (Tok, Syn)))
            then

               Parse_Into_New_Repeater (Program);

            else

               Move_To_Right_Of_Parent (Context, Current);

               Parse_Token (Item, Tok_Text, Current, Context);
            end if;
         end;

      else  --  not off right

         declare
            Program : constant Program_Tree :=
              Program_Tree (Get_Right_Tree (Location));
            Syn    : constant Syntax_Tree := Program.Syntax;
         begin

            if Column < Program.Minimum_Indent or else
              not Aquarius.Syntax.Checks.Begins (Tok, Syn)
            then
               --  node must be nullable, since Token_OK
               Move_Right (Location);

               Parse_Token (Item, Tok_Text,
                            Current, Context);
            else
               if Item.Class = Parseable_Tree and then
                 Item.Subtree.Syntax = Syn
               then
                  declare
                     Parent : constant Program_Tree := Program.Program_Parent;
                  begin
                     --  We are parsing a sub-tree into the parse
                     --  tree.  Replace the existing stub with the
                     --  sub-tree, then remove it.
                     Program.Add_Left_Sibling (Item.Subtree);
                     Parent.Remove_Child (Program);

                     --  Have to be a bit careful about moving right,
                     --  because the location still references the
                     --  child tree we just deleted
                     Location := Right_Of_Tree (Item.Subtree);

                  end;
               elsif Program.Program_Parent /= null
                 and then Program.Program_Parent.Syntax.Repeatable
                 and then Program.Has_Children
                 and then not Token_OK (Item,
                                        Get_Left_Of_First_Child
                                          (Location),
                                        Top => Program)
               then
                  --  Insert a new repeater between the two trees at location
                  --  FIXME: should also check token ok for right tree

                  Parse_Into_New_Repeater
                    (Program.Program_Parent, Right_Tree => Program);

               elsif Syn.Syntax_Class = Terminal then

                  Parse_Terminal (Tok, Tok_Text, Current, Context);

               elsif Program.Child_Count = 0 then

                  if Syn.Syntax_Class = Choice then

                     Parse_Into_Choice (Program, Item, Tok_Text,
                                        Current, Context);

                  else

                     --  It's not a choice, but it has no children,
                     --  so we expand it (not using expand, which only
                     --  works on required nodes)

                     Parse_Into_New_Children (Program, Item, Tok_Text,
                                              Current, Context);

                  end if;

               else  --  not terminal or leaf

                  case Syn.Syntax_Class is
                     when Terminal =>
                        --  already taken care of
                        pragma Assert (False);
                        null;

                     when Choice =>

                        Move_To_Left_Of_First_Child (Context, Current);
                        Parse_Token (Item, Tok_Text,
                                     Current, Context);

                     when Non_Terminal =>

                        if Token_OK (Item,
                                     Get_Left_Of_First_Child
                                       (Location),
                                     Top => Program)
                        then

                           Move_To_Left_Of_First_Child (Context, Current);
                           Parse_Token (Item, Tok_Text,
                                        Current, Context);

                        else
                           --  Since Token_OK was true, we must be in an
                           --  unbounded repeat node.
                           if Syn.Repeatable then
                              Parse_Into_New_Repeater
                                (Repeater   => Program,
                                 Right_Tree => Program.First_Program_Child);

                           else
--                                Ada.Text_IO.Put_Line
--                                  ("Can't parse token '" & Tok_Text &
--                                     "' into what was not " &
--                                     "an unbounded repeat node" &
--                                     " (" &
--                                     Aquarius.Trees.Cursors.Image
--                                     (Get_Cursor (Context)) & ")");
                              raise Program_Error with
                                "Can't parse token '" & Tok_Text &
                                "' into what was not " &
                                "an unbounded repeat node";
                           end if;
                        end if;
                  end case;
               end if;
            end if;
         end;
      end if;

   end Parse_Token;

   ----------------
   -- Parse_Tree --
   ----------------

   procedure Parse_Tree (Top  : Program_Tree;
                         Code : String)
   is
      Context : Parse_Context;
   begin
      Initialise_Parse_Context
        (Context,
         Aquarius.Grammars.Get_Grammar (Top.all),
         Top,
         Interactive => False,
         Run_Actions => False);
      Parse_String (Context, Code);
   end Parse_Tree;

   ----------------
   -- Parse_Tree --
   ----------------

   procedure Parse_Tree (Top    : Program_Tree;
                         Before : String;
                         Child  : Aquarius.Programs.Program_Tree;
                         After  : String)
   is
      Context : Parse_Context;
   begin
      Initialise_Parse_Context
        (Context,
         Aquarius.Grammars.Get_Grammar (Top.all),
         Top,
         Interactive => False,
         Run_Actions => False);
      Parse_String (Context, Before);
      Parse_Token (Make_Parseable (Child), Child.Name, Context);
      Parse_String (Context, After);

   end Parse_Tree;

   ----------------
   -- Parse_Tree --
   ----------------

   procedure Parse_Tree (Top    : Program_Tree;
                         Tree   : Aquarius.Programs.Program_Tree)
   is
      Context : Parse_Context;
   begin
      Initialise_Parse_Context
        (Context,
         Aquarius.Grammars.Get_Grammar (Top.all),
         Top,
         Interactive => False,
         Run_Actions => False);
      Parse_Token (Make_Parseable (Tree), Tree.Name, Context);

   end Parse_Tree;

   ----------------
   -- Parse_Tree --
   ----------------

   procedure Parse_Tree
     (Tree           : Program_Tree;
      Context        : in out Parse_Context)
   is
   begin
      Parse_Token (Make_Parseable (Tree), Tree.Name, Context);
   end Parse_Tree;

   ------------------
   -- Repeat_Child --
   ------------------

   procedure Repeat_Child (Top   : Program_Tree;
                           Child : Array_Of_Program_Trees)
   is
      T : Program_Tree := Top;
   begin
      while not T.Syntax.Is_Repeat loop
         if not T.Has_Children then
            declare
               use Aquarius.Syntax;
            begin
               Add_Child (T,
                          New_Program_Tree
                            (Syntax_Tree (T.Syntax.First_Child),
                             Top.Source, Top.all));
            end;
         end if;
         T := T.First_Program_Child;
      end loop;

      for I in Child'Range loop
         T.Add_Child (Child (I));
      end loop;
   end Repeat_Child;

   ----------------
   -- Set_Cursor --
   ----------------

   procedure Set_Cursor (Context : in out Parse_Context;
                         Cursor  : Aquarius.Trees.Cursors.Cursor)
   is
      A : constant Ambiguity := Context.Ambiguities.First_Element;
   begin
      A.Location := Cursor;
   end Set_Cursor;

   -------------------------
   -- Set_User_Whitespace --
   -------------------------

   procedure Set_User_Whitespace
     (Context  : in out Parse_Context;
      Current  : List_Of_Ambiguities.Cursor;
      At_Tree  : Program_Tree)
   is
      use Aquarius.Locations;
      use Aquarius.Trees;
      Ancestor       : Aquarius.Trees.Tree;
      Left_Ancestor  : Aquarius.Trees.Tree;
      Right_Ancestor : Aquarius.Trees.Tree;
      A : constant Ambiguity := List_Of_Ambiguities.Element (Current);
   begin
      if Context.Vertical_Space > 0 then
         At_Tree.Set_Vertical_Gap_Before (Context.Vertical_Space);
      end if;

      if A.Last_Parse = null then
         Ancestor := Tree (At_Tree);
         while Ancestor.Parent /= null loop
            Ancestor := Ancestor.Parent;
         end loop;
         Right_Ancestor := Ancestor.First_Child;
      else
         A.Last_Parse.Common_Ancestor
           (At_Tree, Ancestor, Left_Ancestor, Right_Ancestor);
         --  if Right_Ancestor = null then
         --     Ada.Text_IO.Put_Line
         --       ("Set_User_Whitespace: no common ancestor");
         --     Ada.Text_IO.Put_Line ("  last parse = " & A.Last_Parse.Image);
         --     Ada.Text_IO.Put_Line ("  at tree    = " & At_Tree.Image);
         --  end if;
      end if;

      if Right_Ancestor /= null then
         for I in 1 .. Context.Comments.Last_Index loop
            Right_Ancestor.Add_Left_Sibling (Context.Comments.Element (I));
         end loop;
      end if;

--        Context.Vertical_Space := 0;
--        Context.Comments.Clear;

   end Set_User_Whitespace;

   ------------------------
   -- Set_Vertical_Space --
   ------------------------

   procedure Set_Vertical_Space
     (Context  : in out Parse_Context;
      Space    : Aquarius.Locations.Line_Count)
   is
   begin
      Context.Vertical_Space := Space;
   end Set_Vertical_Space;

   --------------------
   -- Show_Ambiguity --
   --------------------

   function Show_Ambiguity
     (Item : Ambiguity)
      return String
   is
      Result : constant String :=
                 Aquarius.Counters.Show (Item.Identity)
                 & " "
                 & (if not Item.Active then "[inactive] " else "")
                 & "["
                 & (if Item.Parent = null then "" else Item.Parent.Path_Image)
                 & "] "
                 & Aquarius.Trees.Cursors.Image (Item.Location);
   begin
      return Result;
   end Show_Ambiguity;

   -----------------------
   -- Summarise_Context --
   -----------------------

   procedure Summarise_Context
     (Context : Parse_Context)
   is
   begin
      for A of reverse Context.Ambiguities loop
         Ada.Text_IO.Put_Line (Show_Ambiguity (A));
      end loop;
   end Summarise_Context;

   ---------------
   -- Token_OK --
   ---------------

   function Token_OK
     (Item           : Parseable;
      Location       : Aquarius.Trees.Cursors.Cursor;
      Top            : Program_Tree)
      return Boolean
   is
      use Aquarius.Syntax;
      use Aquarius.Trees.Cursors;
      use type Aquarius.Tokens.Token;
      use type Aquarius.Locations.Column_Count;
      Tok     : constant Aquarius.Tokens.Token := Item.First_Token;
      Result : Boolean;
      Left_Tree : constant Program_Tree :=
        Program_Tree (Get_Left_Tree (Location));
      Right_Tree : constant Program_Tree :=
        Program_Tree (Get_Right_Tree (Location));
   begin

      if not Is_Off_Right (Location) then
         if Right_Tree.Is_Comment then
            return Token_OK (Item,  Get_Right (Location), Top);
         end if;
      end if;

      if Is_Off_Right (Location) then
         --  deal with this later ...

         Result := False;

      else

         declare
            Program : constant Program_Tree := Right_Tree;
            Syn     : constant Syntax_Tree  := Program.Syntax;
         begin

            if Item.Column < Program.Minimum_Indent then
               if not Program.Has_Children and then
                 Aquarius.Syntax.Checks.Nullable (Syn)
               then
                  Result := Token_OK (Item, Get_Right (Location), Top);
               else
                  Result := False;
               end if;
            elsif Item.Class = Parseable_Tree and then
              Item.Subtree.Syntax = Syn and then
              Program.Child_Count = 0
            then
               Result := True;
            elsif Syn.Syntax_Class = Terminal then
               Result := not Program.Is_Filled and then Syn.Token = Tok;
            elsif not Program.Has_Children then
               if Aquarius.Syntax.Checks.Begins (Tok, Syn) then
                  --  If it's a choice, make sure the precondition holds
                  if Aquarius.Syntax.Checks.Satisfies
                    (Program, Syn)
                  then
                     Result := True;
                  else
                     Result := False;
                  end if;
               elsif Aquarius.Syntax.Checks.Nullable (Syn) then
                  Result := Token_OK (Item, Get_Right (Location), Top);
               else
                  Result := False;
               end if;
            else
               case Syn.Syntax_Class is
                  when Terminal =>
                     --  already taken care of
                     pragma Assert (False);
                     Result := False;

                  when Choice =>
                     Result := Token_OK (Item,
                                         Get_Left_Of_First_Child (Location),
                                         Top);

                  when Non_Terminal =>

                     if Token_OK (Item,
                                  Get_Left_Of_First_Child (Location),
                                  Top)
                     then
                        Result := True;
                     elsif Syn.Repeatable then
                        Result := Aquarius.Syntax.Checks.Begins (Tok, Syn);
                     else
                        Result := False;
                     end if;
               end case;
            end if;

         end;

      end if;

      if not Result and then not Is_Off_Left (Location) then
         --  maybe the left node is repeatable
         declare
            Syn     : constant Syntax_Tree  := Left_Tree.Syntax;
         begin

            if Item.Column < Left_Tree.Minimum_Indent then
               Result := False;
            elsif Syn.Repeatable then
               if not Is_Empty (Syn.Separator) then
                  Result :=
                    Aquarius.Syntax.Checks.Begins (Tok,
                                                   Syn.Separator);
               else
                  Result := Aquarius.Syntax.Checks.Begins (Tok, Syn);
               end if;
            end if;

            if not Result then
               if not Is_Off_Right (Location) or else
                 Is_At_Root (Location)
               then
                  Result := False;
               else
                  Result := Token_OK (Item,
                                      Get_Right_Of_Parent (Location),
                                      Top);
               end if;
            end if;

         end;
      end if;

      if not Result then
         declare
            Tree : Program_Tree :=
              Program_Tree (Get_Tree (Location).Parent);
         begin
            while Tree /= null loop
               if Tree = Top then
                  exit;
               end if;

               declare
                  Syn : constant Syntax_Tree := Tree.Syntax;
               begin
                  if Syn.Repeatable and then
                    Item.Column >= Tree.Minimum_Indent and then
                    Aquarius.Syntax.Checks.Begins (Tok, Syn)
                  then
                     Result := True;
                     exit;
                  end if;
               end;
               Tree := Program_Tree (Tree.Parent);
            end loop;
         end;
      end if;

      return Result;

   end Token_OK;

   --------------
   -- Token_OK --
   --------------

   function Token_OK
     (Tok            : Aquarius.Tokens.Token;
      Context        : Parse_Context)
     return Boolean
   is
      use List_Of_Ambiguities;
      It : Cursor := Context.Ambiguities.First;
   begin
      while Has_Element (It) loop
         if Element (It).Active then
            if Element (It).Active and then
              Token_OK (Make_Parseable (Tok, Context),
                        Element (It).Location)
            then
               return True;
            end if;
         end if;
         Next (It);
      end loop;
      return False;
   end Token_OK;

   -------------
   -- Tree_OK --
   -------------

   function Tree_OK
     (Tree           : Program_Tree;
      Context        : Parse_Context)
     return Boolean
   is
      use List_Of_Ambiguities;
      It : Cursor := Context.Ambiguities.First;
   begin
      while Has_Element (It) loop
         if Element (It).Active and then
           Token_OK (Make_Parseable (Tree), Element (It).Location)
         then
            return True;
         end if;
         Next (It);
      end loop;
      return False;
   end Tree_OK;

   ------------------------
   -- Update_Ambiguities --
   ------------------------

   procedure Update_Ambiguities (Context : in out Parse_Context) is

      use List_Of_Ambiguities;
      It : Cursor := Context.Ambiguities.Last;
      A  : Ambiguity;

      --  Remember the top parent of resolved ambiguities,
      --  so that we can run its parse actions.
      Top_Parent  : Program_Tree;

   begin

      while Has_Element (It) and then
        Has_Element (Previous (It))
      loop

         A := Element (It);

         if A.Active then
            --  Check for other ambiguities with the same parent
            --  if none, resolve

            declare
               Other : Cursor := Context.Ambiguities.Last;
            begin
               while Has_Element (Other) loop
                  exit when Other /= It and then
                    Element (Other).Parent = A.Parent;
                  Previous (Other);
               end loop;

               --  If the loop reached the beginning of the list
               --  (i.e. Has_Element returns false) then we didn't
               --  find another node with a matching parent, therefore
               --  the ambiguity is resolved.

               if not Has_Element (Other) then

                  Top_Parent := A.Parent;

                  Other := A.Previous;
                  Element (Other).Location := A.Location;
                  Element (Other).Active := True;

                  --  Resolve the ambiguity by adding it to the right
                  --  of A.Left if it exists, otherwise as a new child
                  --  for A.Parent

                  if A.Right /= null then
                     declare
                        use type Aquarius.Syntax.Syntax_Tree;
                     begin
                        if A.Right.Syntax = A.Top.Syntax then
                           --  replace Right with our new, parsed version

                           A.Parent.Replace_Child (A.Right, A.Top);

                        else
                           --  A.Parent.Replace_Child (A.Right, A.Top);
                           A.Right.Add_Left_Sibling (A.Top);
                        end if;
                     end;
                  else
                     Element (It).Parent.Add_Child (Element (It).Top);
                  end if;

                  Other := Previous (It);
                  Delete (Context.Ambiguities, It);
                  It := Other;
               else
                  Previous (It);
               end if;
            end;
         else
            --  see if this inactive ambiguity still has children
            --  if not, remove it
            declare
               Child : Cursor := Next (It);
            begin
               while Has_Element (Child) and then
                 Element (Child).Previous /= It
               loop
                  Next (Child);
               end loop;
               if not Has_Element (Child) then
                  declare
                     Tmp : constant Cursor := Previous (It);
                     A   : Ambiguity := Element (It);
                  begin
                     Free (A.Top);
                     Free (A);
                     Delete (Context.Ambiguities, It);
                     It := Tmp;
                  end;
               else
                  Previous (It);
               end if;
            end;
         end if;
      end loop;

      if Context.Run_Actions and then
        not Has_Ambiguities (Context) and then
        Top_Parent /= null
      then

         --  Since we just disambiguated, run parse actions
         --  for nodes between the start of the ambiguity and
         --  the node which resolved the ambiguity (inclusive)
         declare
            Stop_At : constant Program_Tree :=
              Program_Tree
              (Aquarius.Trees.Cursors.Get_Left_Tree
                 (Context.Ambiguities.First_Element.Location));
         begin

            Context.Grammar.Run_Action_Trigger
              (Top_Parent, Stop_At,
               Aquarius.Actions.Parse_Trigger);
         end;
      end if;
   end Update_Ambiguities;

   ---------------------
   -- Update_Location --
   ---------------------

   overriding procedure Update_Location
     (This : in out Parse_Context;
      From : Aquarius.Locations.Location_Interface'Class)
   is
   begin
      This.Location.Update_Location (From);
   end Update_Location;

end Aquarius.Programs.Parser;
