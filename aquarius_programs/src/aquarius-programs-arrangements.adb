with Ada.Exceptions;
with Ada.Strings.Unbounded;

with Aquarius.Formats;
with Aquarius.Messages;

with Aquarius.Programs.Arrangements.Contexts;
with Aquarius.Programs.Arrangements.Logging;
with Aquarius.Programs.Arrangements.Reformatting;

package body Aquarius.Programs.Arrangements is

   use Aquarius.Formats;
   use Aquarius.Programs.Arrangements.Contexts;

   procedure Arrange
     (Item    : Program_Tree;
      Context : in out Arrangement_Context);

   procedure Arrange_Terminal
     (Item    : Program_Tree;
      Context : in out Arrangement_Context);

   procedure Arrange_Non_Terminal
     (Item    : Program_Tree;
      Context : in out Arrangement_Context);

   procedure Check_Previous_Line_Length
     (Context : in out Arrangement_Context);

   procedure Re_Arrange
     (Item    : Program_Tree;
      Context : in out Arrangement_Context;
      Start   :        Program_Tree;
      Finish  :        Program_Tree);

   function Before_Point (Location : Program_Tree;
                          Point    : Aquarius.Trees.Cursors.Cursor)
                         return Boolean;

   procedure New_Line
     (Context : in out Arrangement_Context;
      Count   : Natural := 1);

   procedure Skip
     (Context : in out Arrangement_Context;
      Count   : Natural := 1);

   procedure Indent
     (Context : in out Arrangement_Context);

   -------------
   -- Arrange --
   -------------

   procedure Arrange (Item    : Program_Tree;
                      Context : in out Arrangement_Context)
   is
      use type Aquarius.Messages.Message_Level;
      Old_Message_Level : constant Aquarius.Messages.Message_Level :=
        Context.Message_Level;
   begin

      if Context.Stopped then
         return;
      end if;

      Item.Has_Position := False;

      if True or else Item.Name /= "" then
         Logging.Log (Context, Item);
      end if;

      if Item.Has_Messages and then
        Item.Get_Message_Level > Old_Message_Level
      then
         Context.Message_Level := Item.Get_Message_Level;
      end if;

      Item.Set_Inherited_Message_Level (Context.Message_Level);

      if Item.Is_Terminal then
         if Context.Skip_Terminal then
            Logging.Log (Context, Item, "skipping update terminal");
            Context.Skip_Terminal := False;
         else
            Arrange_Terminal (Item, Context);
         end if;
      else
         Arrange_Non_Terminal (Item, Context);
      end if;

      Context.Message_Level := Old_Message_Level;

      if Context.Stop_Tree /= null and then Item = Context.Stop_Tree then
         Context.Stopped := True;
      end if;

   end Arrange;

   -------------
   -- Arrange --
   -------------

   procedure Arrange
     (Item        : not null Program_Tree;
      Messages    : out Aquarius.Messages.Message_List;
      Line_Length : Positive      := 72)
   is
      Context : Arrangement_Context;
   begin
      Context.Right_Margin :=
        Aquarius.Locations.Column_Index (Line_Length);
      Context.User_Cursor := Aquarius.Trees.Cursors.Left_Of_Tree (Item);
      Arrange (Item, Context);
      Check_Previous_Line_Length (Context);
      Messages := Context.Logging;
   exception
      when E : others =>
         Logging.Log (Context, Item,
              "exception: "
                      & Ada.Exceptions.Exception_Message (E));
         raise;
   end Arrange;

   -------------
   -- Arrange --
   -------------

   procedure Arrange
     (Item             : not null Program_Tree;
      Point            : Aquarius.Trees.Cursors.Cursor;
      Partial          : String;
      Updating         : Boolean;
      Partial_Line     : out Aquarius.Locations.Line_Index;
      Partial_Column   : out Aquarius.Locations.Column_Index;
      Line_Length      : Positive      := 72)
   is
      Context : Arrangement_Context;
   begin
      Context.Right_Margin :=
        Aquarius.Locations.Column_Index (Line_Length);
      Context.User_Text :=
        Ada.Strings.Unbounded.To_Unbounded_String (Partial);
      Context.Updating := Updating;
      Context.User_Cursor := Point;
      Arrange (Item, Context);
      Check_Previous_Line_Length (Context);
      Partial_Line := Context.User_Text_Line;
      Partial_Column := Context.User_Text_Column;
   end Arrange;

   --------------------------
   -- Arrange_Non_Terminal --
   --------------------------

   procedure Arrange_Non_Terminal (Item    : Program_Tree;
                                   Context : in out Arrangement_Context)
   is
      Format       : constant Aquarius_Format :=
        Item.Syntax.Get_Format;
      Rules        : constant Immediate_Rules := Formats.Rules (Format);
      Child_Indent : constant Indentation_Offset :=
                       Indent_Child (Format);
      Before_Indent : constant Indentation_Offset :=
                        Indent (Format, Before);
      After_Indent : constant Indentation_Offset :=
                        Indent (Format, After);
   begin

      if Enabled (Rules.New_Line_Before) then
         Context.Need_New_Line := True;
      end if;

      if Item.Soft_New_Line then
         Context.Need_Soft_New_Line := True;
      end if;

      Context.Current_Indent :=
        Aquarius.Locations.Column_Index
          (Indentation_Offset (Context.Current_Indent)
           + Child_Indent + Before_Indent);

      for I in 1 .. Item.Child_Count loop
         Arrange (Item.Program_Child (I), Context);

         if not Item.Has_Position
           and then Item.Program_Child (I).Has_Position
         then
            Item.Start_Offset := Item.Program_Child (I).Start_Offset;
            Item.Start_Line := Item.Program_Child (I).Start_Line;
            Item.Start_Column := Item.Program_Child (I).Start_Column;
            Item.Has_Position := True;
            if Item.Name /= "" then
               Logging.Log (Context, Item,
                            "update position from child");
            end if;
         end if;

         exit when Context.Stopped;
      end loop;

      if Item.Has_Position then
         for I in reverse 1 .. Item.Child_Count loop
            if Item.Program_Child (I).Has_Position then
               Item.End_Offset := Item.Program_Child (I).End_Offset;
               exit;
            end if;
         end loop;
      end if;

      Item.End_Line := Context.Current_Line;
      Item.End_Column := Context.Current_Column;

      Context.Current_Indent :=
        Aquarius.Locations.Column_Index
          (Indentation_Offset (Context.Current_Indent)
           - Child_Indent + After_Indent);

      if Enabled (Rules.New_Line_After) then
         Context.Need_New_Line := True;
      end if;

      Logging.Log (Context, Item);
   end Arrange_Non_Terminal;

   ----------------------
   -- Arrange_Terminal --
   ----------------------

   procedure Arrange_Terminal (Item    : Program_Tree;
                               Context : in out Arrangement_Context)
   is
      use Aquarius.Locations;
      Format    : constant Aquarius_Format := Item.Syntax.Get_Format;
      Rules     : constant Immediate_Rules := Formats.Rules (Format);
      Vertical_Gap : constant Natural :=
                       Natural (Item.Vertical_Gap_Before);
   begin

      if not Context.Rearranging and then Vertical_Gap > 0 then

         New_Line (Context, Vertical_Gap);

         Context.Need_Space    := False;
         Context.First_On_Line := True;
      end if;

      if Enabled (Rules.New_Line_Before) then
         Context.Need_New_Line := True;
      end if;

      if Item.Soft_New_Line then
         Context.Need_Soft_New_Line := True;
      end if;

      if Context.Need_Soft_New_Line then
         Context.Need_New_Line := True;

         Context.Soft_Indent := Context.Soft_Indent + 2;

         Logging.Log (Context, Item,
              "enter soft new line; new indent ="
                      & Context.Current_Indent'Img
                      & Context.Soft_Indent'Img);
      end if;

      if Enabled (Rules.New_Line_Before) or else Item.New_Line_Before then
         Context.Need_New_Line := True;
      end if;

      if Context.Need_New_Line then
         if not Context.Got_New_Line then
            New_Line (Context);
            Context.Need_New_Line := False;
            Context.Got_New_Line  := True;
            Context.Need_Space    := False;
            Context.First_On_Line := True;
            Logging.Log
              (Context, Item, "got new line because need new line");
         end if;
      end if;

      if Context.First_On_Line then

         if not Context.Rearranging
           and then Context.Previous_Terminal /= null
         then
            Check_Previous_Line_Length (Context);
         end if;

         Context.First_Terminal := Item;
      end if;

      if Context.First_On_Line then
         if Context.Cancel_Indent then
            Context.Cancel_Indent := False;
         else
            Indent (Context);
         end if;
      else
         declare
            Insert_Space : Boolean;
         begin
            if not Formats.Enabled (Rules.Space_Before) then
               Insert_Space := Context.Need_Space;
            elsif not Formats.Negative (Rules.Space_Before) then
               Insert_Space := not Context.No_Space or else
                 Context.Space_Priority < Priority (Rules.Space_Before);
            else
               if Context.Need_Space then
                  Insert_Space :=
                    Context.Space_Priority >= Priority (Rules.Space_Before);
               else
                  Insert_Space := False;
               end if;
            end if;

            if Insert_Space then
               Skip (Context);
            end if;
         end;
      end if;

      Item.Update_Location
        (Context.Current_Position,
         Context.Current_Line, Context.Current_Column);
      Item.Has_Position := True;

      Logging.Log (Context, Item,
                   "start: " & Item.Start_Offset'Image);

      if Context.Need_Soft_New_Line then
         Context.Soft_Indent := Context.Soft_Indent - 2;

         Logging.Log
           (Context, Item,
            "leave soft new line; new indent ="
            & Context.Current_Indent'Img & Context.Soft_Indent'Img);
         Context.Need_Soft_New_Line := False;
      end if;

      if Before_Point (Item, Context.User_Cursor)
        and then Context.Updating
      then
         null;
      else
         Skip (Context, Item.Layout_Length);
      end if;

      if Before_Point (Item, Context.User_Cursor) then

         Context.User_Text_Line := Context.Current_Line;
         Context.User_Text_Column := Context.Current_Column;

         declare
            Text : constant String :=
                     Ada.Strings.Unbounded.To_String (Context.User_Text);
         begin
            for Ch of Text loop
               if Ch = Character'Val (10) then
                  Logging.Log (Context, Item,
                               "got a user new line after");
                  New_Line (Context);
               else
                  Skip (Context);
               end if;
            end loop;

         end;

         Logging.Log (Context, Item,
                      "Calculate user edit start at "
                      & Context.User_Text_Line'Img
                      & Context.User_Text_Column'Img
                      & "with length"
                      & Context.User_Text_Length'Image);

      end if;

      if Item.Separator_New_Line
        or else
          (Enabled (Rules.New_Line_After) and then Item.Is_Separator)
      then
         Context.Current_Line := Context.Current_Line + 1;
         declare
            Align_With : constant Program_Tree :=
              Program_Tree (Item.Parent.First_Leaf);
         begin
            Context.Current_Column := Align_With.Start_Column;
         end;
         Context.Got_New_Line  := True;
         if Item.Separator_New_Line then
            Logging.Log (Context, Item, "got new line because of separator");
         end if;
         if Enabled (Rules.New_Line_After)
           and then Item.Is_Separator
         then
            Logging.Log (Context, Item, "got new line because of rule");
         end if;
         Context.Need_New_Line := False;
         Context.First_On_Line := True;
         Context.Cancel_Indent := True;
      else
         Context.First_On_Line := False;
         Context.Got_New_Line  := False;

         if Enabled (Rules.Space_After) then
            if Negative (Rules.Space_After) then
               Context.No_Space := True;
               Context.Need_Space := False;
            else
               Context.Need_Space := True;
               Context.No_Space := False;
            end if;
            Context.Space_Priority := Priority (Rules.Space_After);
         end if;

         if Enabled (Rules.New_Line_After)
           and then not Item.Is_Separator
         then
            Context.Need_New_Line := True;
         end if;
      end if;

      Context.Previous_Terminal := Item;
      Context.Previous_Indent := Context.Current_Indent;

   end Arrange_Terminal;

   ------------------
   -- Before_Point --
   ------------------

   function Before_Point (Location : Program_Tree;
                          Point    : Aquarius.Trees.Cursors.Cursor)
                         return Boolean
   is
      use Aquarius.Trees.Cursors;
   begin
      return not Is_Off_Left (Point) and then
        Location = Program_Tree (Get_Left_Tree (Point));
   end Before_Point;

   --------------------------------
   -- Check_Previous_Line_Length --
   --------------------------------

   procedure Check_Previous_Line_Length
     (Context : in out Arrangement_Context)
   is
      use Aquarius.Locations;
   begin

      if Context.First_Terminal = null
        or else Context.Previous_Terminal = null
      then
         return;
      end if;

      Logging.Log (Context, Context.First_Terminal, "checking previous line");
      Logging.Log (Context, Context.Previous_Terminal,
                   "ends in column"
                   & Context.Previous_Terminal.End_Column'Image);

      if Context.Previous_Terminal.End_Column > Context.Right_Margin then
         declare
            Ancestor_Tree   : Aquarius.Trees.Tree;
            Left_Ancestor   : Aquarius.Trees.Tree;
            Right_Ancestor  : Aquarius.Trees.Tree;
            Ancestor        : Program_Tree;
            Old_Soft_Indent : constant Column_Count := Context.Soft_Indent;
         begin
            Context.Soft_Indent := 0;
            Aquarius.Trees.Common_Ancestor
              (Left           => Context.First_Terminal,
               Right          => Context.Previous_Terminal,
               Ancestor       => Ancestor_Tree,
               Left_Ancestor  => Left_Ancestor,
               Right_Ancestor => Right_Ancestor);
            Ancestor := Program_Tree (Ancestor_Tree);
            Logging.Log (Context, Ancestor, "re-arranging");

            Aquarius.Programs.Arrangements.Reformatting.Reformat
              (Context => Context,
               Top     => Ancestor,
               Start   => Context.First_Terminal,
               Finish  => Context.Previous_Terminal);

            Re_Arrange (Ancestor, Context,
                        Context.First_Terminal,
                        Context.Previous_Terminal);

            Logging.Log (Context, Ancestor, "after re-arrangement");
            Logging.Log (Context, Ancestor);
            Context.Soft_Indent := Old_Soft_Indent;
         end;
      end if;
   end Check_Previous_Line_Length;

   ------------
   -- Indent --
   ------------

   procedure Indent
     (Context : in out Arrangement_Context)
   is
      use Aquarius.Locations;
   begin
      if Context.Current_Column < Context.Current_Indent then
         Context.Current_Position :=
           Context.Current_Position
             + Location_Offset (Context.Current_Indent
                                - Context.Current_Column);
         Context.Current_Column := Context.Current_Indent;
      end if;
   end Indent;

   --------------
   -- New_Line --
   --------------

   procedure New_Line
     (Context : in out Arrangement_Context;
      Count   : Natural := 1)
   is
      use Aquarius.Locations;
   begin
      Context.Current_Line :=
        Context.Current_Line + Line_Count (Count);
      Context.Current_Position :=
        Context.Current_Position + Location_Offset (Count);
      Context.Current_Column := 1;
   end New_Line;

   ----------------
   -- Re_Arrange --
   ----------------

   procedure Re_Arrange
     (Item    : Program_Tree;
      Context : in out Arrangement_Context;
      Start   : Program_Tree;
      Finish  : Program_Tree)
   is

      use Aquarius.Locations;
      use Aquarius.Syntax;

      function Separator_With_New_Line
        (Item : Aquarius.Trees.Root_Tree_Type'Class)
         return Boolean;

      -----------------------------
      -- Separator_With_New_Line --
      -----------------------------

      function Separator_With_New_Line
        (Item : Aquarius.Trees.Root_Tree_Type'Class)
         return Boolean
      is
         Program : Program_Tree_Type'Class
         renames Program_Tree_Type'Class (Item);
      begin
         return Program.Is_Separator
           and then ((Enabled (Program.Rules.Space_After)
                      and then not Negative (Program.Rules.Space_After))
                     or else Enabled (Program.Rules.New_Line_After)
                     or else Enabled (Program.Rules.Soft_New_Line_After));
      end Separator_With_New_Line;

      Separator : constant Program_Tree :=
        Program_Tree
          (Item.Breadth_First_Search
             (Separator_With_New_Line'Access));

      Got_Start         : Boolean := False;
      Got_Finish        : Boolean := False;
      Applied_Separator : Boolean := False;
      Cancel_Separator  : Boolean := False;
      Separator_Level   : Natural := 0;

      Partial_Length   : Column_Count := Context.Current_Indent;
      New_Line_Indent  : constant Column_Index :=
                           Context.Current_Indent + 2;
      Last_Column_Index : Column_Count := Context.Current_Indent;

--        Remaining_Length  : Count :=
--                              Finish.End_Offset.Column
--                                - Start.Start_Offset.Column;

      Had_Soft_New_Line_After : Boolean := False;
      Last_Soft_New_Line      : Program_Tree := null;
      Last_Soft_Column        : Column_Count := 0;
      Last_Soft_Level         : Natural := 0;

      Closing : constant Boolean := Enabled (Finish.Rules.Closing);

      procedure Apply_Newlines
        (Program : Program_Tree;
         Level   : Natural);

      pragma Style_Checks (Off);

      --------------------
      -- Apply_Newlines --
      --------------------

      procedure Apply_Newlines
        (Program : Program_Tree;
         Level   : Natural)
      is
      begin
         if Got_Finish then
            return;
         end if;

         if Program = Start then
            Got_Start := True;
            Last_Column_Index := Program.Start_Column;
         elsif Program = Finish then
            if Closing then
               Finish.Set_New_Line_Before (Enabled => True);
            end if;
            Got_Finish := True;
         end if;

         if Got_Start then

            if not Cancel_Separator
              and then Separator /= null
              and then Program.Syntax = Separator.Syntax
            then
               Logging.Log (Context, Program, "separator level:" & Level'Img);
               Separator_Level := Level;
            end if;

            if Program.Is_Separator then
               if not Cancel_Separator and then Separator /= null
                 and then Program.Syntax = Separator.Syntax
               then
                  Logging.Log (Context, Program, "setting separator new line");
                  Program.Separator_NL := True;
                  Partial_Length := New_Line_Indent;
                  Last_Soft_New_Line := null;
                  Applied_Separator := True;
               end if;
            elsif (Program.Has_Soft_New_Line_Rule_Before
                   or else Had_Soft_New_Line_After)
            then
               declare
                  New_Length : constant Column_Count :=
                                 Program.End_Column
                                   - Last_Column_Index
                                 + Partial_Length;
               begin
                  if New_Length > Context.Right_Margin then
                     Logging.Log (Context, Program, "setting last soft new line");
                     Last_Soft_New_Line := Program;
                     Last_Soft_Column := Partial_Length;
                     Last_Soft_Level := Level;
                  else
                     Logging.Log (Context, Program,
                          "ignoring soft new line because line length is"
                          & New_Length'Image);
                  end if;
               end;

--                 Program.Set_Soft_New_Line;
--                 Partial_Length := New_Line_Indent;
            end if;

            Had_Soft_New_Line_After := False;

            if Program.Is_Terminal then
               Partial_Length := Partial_Length
                 + Program.End_Column - Last_Column_Index;
               if Partial_Length > Context.Right_Margin then
                  Logging.Log (Context, Program,
                       "overflow: separator = "
                       & (if Separator_Level > 0
                         then Separator.Text & ":" & Separator_Level'Img
                         else "<>")
                       & "; soft new line level ="
                       & Last_Soft_Level'Img);
                  if Last_Soft_New_Line /= null
                    and then (True or else Separator = null
                              or else Separator_Level = 0
                              or else Separator_Level >= Last_Soft_Level)
                  then
                     Logging.Log (Context, Program, "using last soft new line");
                     Last_Soft_New_Line.Set_Soft_New_Line;
                     Partial_Length :=
                       New_Line_Indent + (Partial_Length - Last_Soft_Column);
                     Last_Soft_New_Line := null;
                     if Partial_Length <= Context.Right_Margin
                       and then not Applied_Separator
                     then
                        Cancel_Separator := True;
                     end if;
                  elsif Separator /= null then
                     Logging.Log (Context, Program, "using last separator");
                  end if;
               end if;

               Last_Column_Index := Program.End_Column;
--                 Remaining_Length :=
--                   Finish.End_Offset.Column
--                     - Program.End_Offset.Column;
            end if;

            if Program.Has_Soft_New_Line_Rule_After then
               Had_Soft_New_Line_After := True;
            end if;

         end if;

         declare
            Children : constant Array_Of_Program_Trees :=
                         Program.Direct_Children
                           (Skip_Separators => False);
         begin
            for I in Children'Range loop
               Apply_Newlines (Children (I), Level + 1);
            end loop;
         end;
      end Apply_Newlines;

      Current_Indent : constant Column_Count := Context.Current_Indent;

   begin

      Apply_Newlines (Item, 0);

      Context.Current_Indent := Context.Previous_Indent;
      Context.Current_Line := Item.Start_Line;
      Context.Current_Column := Item.Start_Column;
      Context.Current_Position := Item.Start_Offset;

      Context.Rearranging := True;
      Context.Stop_Tree := Finish;
      Context.Stopped   := False;

      Logging.Log (Context, Item, "starting re-arrangement");

      Arrange (Item, Context);
      Context.Rearranging := False;
      Context.Stop_Tree := null;
      Context.Stopped := False;

      New_Line (Context);

      Context.Need_New_Line := False;
      Context.Got_New_Line  := True;
      Logging.Log (Context, Item, "got new line because of rearrangement");
      Context.Need_Space    := False;
      Context.First_On_Line := True;
      Context.Previous_Indent := Context.Current_Indent;
      Context.Current_Indent := Current_Indent;

   end Re_Arrange;

   ------------
   -- Render --
   ------------

   procedure Render
     (Program        : not null Program_Tree;
      Renderer       : in out Rendering.Root_Aquarius_Renderer'Class;
      Point          : Aquarius.Trees.Cursors.Cursor;
      Partial        : String;
      Updating       : Boolean;
      Partial_Line   : Aquarius.Locations.Line_Index;
      Partial_Column : Aquarius.Locations.Column_Index)
   is

      procedure Perform_Render (P : Program_Tree);
      procedure Render_Terminal (Terminal : Program_Tree);

      --------------------
      -- Perform_Render --
      --------------------

      procedure Perform_Render (P : Program_Tree) is
      begin
         if P.Is_Terminal then
            Render_Terminal (P);
         else
            for I in 1 .. P.Child_Count loop
               Perform_Render (P.Program_Child (I));
            end loop;
         end if;
      end Perform_Render;

      ---------------------
      -- Render_Terminal --
      ---------------------

      procedure Render_Terminal (Terminal : Program_Tree) is
         use Aquarius.Messages;
         Line : constant Aquarius.Locations.Line_Index :=
                  Terminal.Line;
         Column : constant Aquarius.Locations.Column_Index :=
                    Terminal.Start_Column;
         Msg_Level       : constant Message_Level :=
                             Terminal.Get_Inherited_Message_Level;
         At_Point        : constant Boolean :=
                             Before_Point (Terminal, Point);
      begin
         if At_Point and then Updating then
            null;
         elsif Msg_Level > No_Message then
            Renderer.Set_Text
              (Terminal, Line, Column,
               Level_Name (Msg_Level),
               Terminal.Text);
         elsif not Terminal.Is_Filled then
            Renderer.Set_Text
              (Terminal, Line, Column,
               "implied_token",
               Terminal.Text);
         elsif not Aquarius.Syntax.Is_Empty (Terminal.Render_Class) then
            Renderer.Set_Text (Terminal, Line, Column,
                               Terminal.Render_Class.Render_Class,
                               Terminal.Text);
         elsif Terminal.Syntax.Has_Token then
            Renderer.Set_Text (Terminal, Line, Column,
                               Aquarius.Tokens.Get_Token_Class_Name
                                 (Terminal.Syntax.Frame,
                                  Terminal.Syntax.Token),
                               Terminal.Text);
         else
            Renderer.Set_Text (Terminal, Line, Column,
                               "normal", Terminal.Text);
         end if;

         if At_Point then
            Renderer.Set_Text
              (Terminal, Partial_Line, Partial_Column, "normal", Partial);
         end if;

      end Render_Terminal;

   begin

      Renderer.Begin_Render;
      Perform_Render (Program);
--      Renderer.Set_Point (Partial_Line, Partial_Column);
      Renderer.End_Render;
   end Render;

   ------------
   -- Render --
   ------------

   procedure Render
     (Program     : not null Program_Tree;
      Renderer    : in out Aquarius.Rendering.Root_Aquarius_Renderer'Class)
   is
   begin
      Render (Program, Renderer,
              Aquarius.Trees.Cursors.Left_Of_Tree (Program),
              "", False, 1, 1);
   end Render;

   ----------
   -- Skip --
   ----------

   procedure Skip
     (Context : in out Arrangement_Context;
      Count   : Natural := 1)
   is
      use Aquarius.Locations;
   begin
      Context.Current_Column :=
        Context.Current_Column + Column_Count (Count);
      Context.Current_Position :=
        Context.Current_Position + Location_Offset (Count);
   end Skip;

end Aquarius.Programs.Arrangements;
