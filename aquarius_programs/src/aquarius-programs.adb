with Ada.Characters.Handling;
with Ada.Containers.Vectors;
with Ada.Directories;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with Aquarius.Properties;

package body Aquarius.Programs is

   Num_Allocated_Trees : Natural := 0;

   package Program_Tree_Vectors is
      new Ada.Containers.Vectors (Positive, Program_Tree);

   Free_List      : Program_Tree_Vectors.Vector;
   Allocated_List : Program_Tree_Vectors.Vector;

   Empty_Program_Node : constant Program_Tree_Type :=
     (Aquarius.Trees.Root_Tree_Type with
      Free                 => False,
      Temporary            => False,
      Comment              => False,
      Error_Node           => False,
      Error_Tree           => False,
      Filled               => False,
      Separator_Node       => False,
      Separator_NL         => False,
      Soft_NL              => False,
      NL                   => False,
      Overflow_Checked     => False,
      Have_Symbol_Table    => False,
      Is_Declaration       => False,
      Has_Position         => False,
      Has_Environment      => False,
      Self                 => null,
      Sequence             => 0,
      Source_File          => Aquarius.Source.No_Source_File,
      Tree_Name            => Aquarius.Names.Null_Aquarius_Name,
      Source_File_Name     => Aquarius.Names.Null_Aquarius_Name,
      Msg_Level            => Aquarius.Messages.No_Message,
      Vertical_Gap         => 0,
      Syntax               => null,
      Fill_Text            => Aquarius.Tokens.To_Token_Text (""),
      Local_Start          => 1,
      Start_Position       => 1,
      End_Position         => 1,
      Start_Line           => 1,
      End_Line             => 1,
      Start_Column         => 1,
      End_Column           => 1,
      Indent_Rule          => False,
      Offset_Rule          => Aquarius.Source.No_Source_Position,
      Render_Class         => null,
      Local_Env            => null,
      String_Props         => String_Property_Maps.Empty_Map);

   --  After a node is changed, update any entry it references
   procedure Update_Entry
     (Item    : in out Program_Tree_Type'Class);

   -----------------------
   -- Actionable_Source --
   -----------------------

   overriding
   function Actionable_Source
     (Item : Program_Tree_Type)
     return access Aquarius.Actions.Action_Source'Class
   is
   begin
      return Item.Syntax;
   end Actionable_Source;

   ---------------
   -- Add_Child --
   ---------------

   overriding
   procedure Add_Child
     (Item      : not null access Program_Tree_Type;
      New_Child : not null access Aquarius.Trees.Root_Tree_Type'Class)
   is
      Program_Child : constant Program_Tree := Program_Tree (New_Child);
   begin
      pragma Assert (not Item.Free);
      Aquarius.Trees.Root_Tree_Type (Item.all).Add_Child (New_Child);
      Program_Child.Indent_Rule :=
        Program_Child.Indent_Rule or else Item.Indent_Rule;
      if not Aquarius.Syntax.Is_Empty (Item.Render_Class) and then
        Aquarius.Syntax.Is_Empty (Program_Child.Render_Class)
      then
         Program_Child.Render_Class := Item.Render_Class;
      end if;
      Program_Tree (New_Child).Source_File := Item.Source_File;
   end Add_Child;

   -----------------
   -- Chosen_Tree --
   -----------------

   function Chosen_Tree (Item : not null access Program_Tree_Type)
                        return Program_Tree
   is
      use Aquarius.Syntax;
      It : Program_Tree := Program_Tree (Item);
   begin
      pragma Assert (not Item.Free);
      while It.Syntax.Syntax_Class /= Choice loop
         pragma Assert (It.Child_Count = 1);
         It := It.Program_Child (1);
      end loop;

      pragma Assert (It.Child_Count = 1);
      It := It.Program_Child (1);

      while It.Internal_Tree
        and then It.Child_Count = 1
      loop
         It := It.Program_Child (1);
      end loop;

      return It;

   end Chosen_Tree;

   --------------------------
   -- Concatenate_Children --
   --------------------------

   function Concatenate_Children (Item : Program_Tree_Type) return String is
      use Ada.Strings.Unbounded;
      Result : Unbounded_String := Null_Unbounded_String;
      Items  : constant Array_Of_Program_Trees :=
        Item.Direct_Children (Skip_Separators => False);
   begin
      if Items'Length > 0 then
         for I in Items'Range loop
            Result := Result &  Items (I).Concatenate_Children;
         end loop;
         return To_String (Result);
      else
         return Item.Text;
      end if;
   end Concatenate_Children;

   -----------------------
   -- Contains_Position --
   -----------------------

   function Contains_Position (Item : Program_Tree_Type;
                               Position : Aquarius.Layout.Position)
                               return Boolean
   is
   begin
      return Position in
        Item.Layout_Start_Position .. Item.Layout_End_Position;
   end Contains_Position;

   -------------------------
   -- Create_Symbol_Table --
   -------------------------

   procedure Create_Symbol_Table (Tree : in out Program_Tree_Type) is
      use Aquarius.Properties;
   begin
      if Tree.Has_Property (Symbol_Table_Property) then
         Tree.Set_Property (Symbol_Table_Property,
                            Aquarius.Entries.New_Symbol_Table
                              (Tree.Image, Tree.Symbol_Table));
      else
         Tree.Set_Property (Symbol_Table_Property,
                            Aquarius.Entries.New_Symbol_Table
                              (Tree.Image, null));
      end if;
      Tree.Have_Symbol_Table := True;
   end Create_Symbol_Table;

   --------------------------
   -- Cross_Reference_Name --
   --------------------------

   function Cross_Reference_Name
     (Item : Program_Tree_Type'Class)
      return Program_Tree
   is
      pragma Assert (Item.Has_Cross_Reference);
   begin
      return Item.Program_Child (Item.Syntax.Cross_Reference_Name.Name);
   end Cross_Reference_Name;

   ------------------------
   -- Debug_Dump_Program --
   ------------------------

--     procedure Debug_Dump_Program (Item : Program_Tree) is

--        Indent : Positive := 1;

--        procedure Dump (P : Program_Tree);

--        ----------
--        -- Dump --
--        ----------

--        procedure Dump (P : Program_Tree) is
--        begin
--           Aquarius.Trace.Set_Col (Aquarius.Trace.Program_Trees, Indent);
--           Aquarius.Trace.Put_Line (Aquarius.Trace.Program_Trees, P.Image);
--           if P.Has_Children then
--              Indent := Indent + 2;
--              for I in 1 .. P.Child_Count loop
--                 Dump (P.Program_Child (I));
--              end loop;
--              Indent := Indent - 2;
--           end if;
--        end Dump;
--     begin
--        Dump (Item);
--     end Debug_Dump_Program;

   ------------------------
   -- Declaration_Parent --
   ------------------------

   function Declaration_Parent
     (Item : not null access Program_Tree_Type'Class)
      return Program_Tree
   is
      It : Program_Tree := Program_Tree (Item);
   begin
      while It /= null and then not It.Is_Declaration loop
         It := It.Program_Parent;
      end loop;
      return It;
   end Declaration_Parent;

   ---------------------
   -- Direct_Children --
   ---------------------

   function Direct_Children (Item : Program_Tree_Type;
                             Name : String)
                            return Array_Of_Program_Trees
   is
      Tree_Children : constant Aquarius.Trees.Array_Of_Trees :=
        Item.Get_Matching_Children (Name);
      Result        : Array_Of_Program_Trees (Tree_Children'Range);
   begin
      for I in Result'Range loop
         Result (I) := Program_Tree (Tree_Children (I));
      end loop;
      return Result;
   end Direct_Children;

   ---------------------
   -- Direct_Children --
   ---------------------

   function Direct_Children (Item            : Program_Tree_Type;
                             Skip_Separators : Boolean := True)
                            return Array_Of_Program_Trees
   is
      Tree_Children : constant Aquarius.Trees.Array_Of_Trees :=
        Item.Get_Named_Children;
      Result        : Array_Of_Program_Trees (Tree_Children'Range);
      Count         : Natural := 0;
   begin
      for I in Result'Range loop
         declare
            Item : constant Program_Tree := Program_Tree (Tree_Children (I));
         begin
            if not Skip_Separators or else
              not Item.Is_Separator
            then
               Count := Count + 1;
               Result (Count) := Item;
            end if;
         end;
      end loop;
      return Result (Result'First .. Result'First + Count - 1);
   end Direct_Children;

   -----------------
   -- End_Of_Line --
   -----------------

   function End_Of_Line
     (Tree   : not null access Program_Tree_Type'Class)
      return Program_Tree
   is
      use type Aquarius.Layout.Line_Number;
      Result : Program_Tree := Program_Tree (Tree);
      Next   : Program_Tree := Result.Scan_Terminal (1);
   begin
      while Next /= null
        and then Next.Layout_Line = Tree.Layout_Line
      loop
         Result := Next;
         Next := Next.Scan_Terminal (1);
      end loop;
      return Result;
   end End_Of_Line;

   ---------------------------
   -- Execute_Single_Action --
   ---------------------------

   procedure Execute_Single_Action
     (Tree         : not null access Program_Tree_Type'Class;
      Action_Group : Aquarius.Actions.Action_Group;
      Position     : Rule_Position)
   is
   begin
      Aquarius.Actions.Execute (Tree.Syntax.all, Tree.all,
                                Action_Group, Position);
   end Execute_Single_Action;

   ------------
   -- Expand --
   ------------

   procedure Expand (Item : not null access Program_Tree_Type) is
      use Aquarius.Syntax;
      Syn : constant Syntax_Tree := Item.Syntax;
   begin
      pragma Assert (not Item.Free);
      case Syn.Syntax_Class is
         when Terminal =>
            --  no expansion necessary
            null;

         when Choice =>
            --  no expansion possible
            null;

         when Non_Terminal =>
            if Syn.Optional then
               --  don't expand optional nodes
               null;
            else
               for I in 1 .. Syn.Child_Count loop
                  declare
                     Child : constant Program_Tree :=
                       New_Program_Tree (Syntax_Tree (Syn.Child (I)));
                  begin
                     Item.Add_Child (Child);
                  end;
               end loop;
            end if;
      end case;
   end Expand;

   ----------------
   -- Expand_All --
   ----------------

   procedure Expand_All (Item : in out Program_Tree_Type) is
      use Aquarius.Syntax;
      Syn : constant Syntax_Tree := Item.Syntax;
   begin
      case Syn.Syntax_Class is
         when Terminal =>
            --  no expansion necessary
            null;

         when Choice =>
            --  no expansion possible
            null;

         when Non_Terminal =>
            for I in 1 .. Syn.Child_Count loop
               declare
                  Child : constant Program_Tree :=
                    New_Program_Tree (Syntax_Tree (Syn.Child (I)));
               begin
                  Item.Add_Child (Child);
               end;
            end loop;
      end case;
   end Expand_All;

   ----------
   -- Fill --
   ----------

   procedure Fill (Item : in out Program_Tree_Type;
                   Text : String)
   is
   begin
      Item.Fill_Text := Aquarius.Tokens.To_Token_Text (Text);
      Item.Filled    := True;
      if Has_Entry (Item) then
         Update_Entry (Item);
      end if;
   end Fill;

   ----------
   -- Fill --
   ----------

   procedure Fill (Item : in out Program_Tree_Type) is
   begin
      Item.Filled := True;
   end Fill;

   -----------------------------------
   -- Find_Local_First_Node_At_Line --
   -----------------------------------

   function Find_Local_First_Node_At_Line
     (Top  : not null access Program_Tree_Type'Class;
      Line : Aquarius.Layout.Line_Number)
      return Program_Tree
   is
      Last_Terminal : Program_Tree := null;
      Stop : Boolean := False;

      procedure Find
        (Node : not null access Program_Tree_Type'Class);

      ----------
      -- Find --
      ----------

      procedure Find
        (Node : not null access Program_Tree_Type'Class)
      is
         use Aquarius.Layout;
      begin
         if Node.Is_Terminal then
            if Node.Start_Line < Line then
               Last_Terminal := Program_Tree (Node);
            elsif Node.Start_Line = Line
              and then (Last_Terminal = null
                        or else Last_Terminal.Start_Line < Line)
            then
               Last_Terminal := Program_Tree (Node);
               Stop := True;
            else
               Stop := True;
            end if;
         else
            for I in 1 .. Node.Child_Count loop
               Find (Node.Program_Child (I));
               exit when Stop;
            end loop;
         end if;
      end Find;

   begin
      Find (Top);
      return Last_Terminal;
   end Find_Local_First_Node_At_Line;

   ------------------------
   -- Find_Local_Node_At --
   ------------------------

   function Find_Local_Node_At
     (Top      : not null access Program_Tree_Type'Class;
      Location : Aquarius.Layout.Position)
      return Program_Tree
   is

      use type Aquarius.Layout.Position;
      Last_Terminal : Program_Tree := null;

      procedure Find
        (Node : not null access Program_Tree_Type'Class);

      ----------
      -- Find --
      ----------

      procedure Find
        (Node : not null access Program_Tree_Type'Class)
      is
         use Aquarius.Layout;
      begin
         if Node.Is_Terminal then
            if Node.Start_Position <= Location then
               if Last_Terminal /= null
                 and then not Last_Terminal.Is_Reserved_Terminal
                 and then Node.Is_Reserved_Terminal
                 and then Last_Terminal.End_Position
                   = Node.Start_Position
               then
                  null;   --  prefer an identifier to a keyword
               else
                  Last_Terminal := Program_Tree (Node);
               end if;
            end if;
         else
            for I in 1 .. Node.Child_Count loop
               Find (Node.Program_Child (I));
            end loop;
         end if;
      end Find;

   begin
      Find (Top);
      return Last_Terminal;
   end Find_Local_Node_At;

   ------------------
   -- Find_Node_At --
   ------------------

   function Find_Node_At
     (Top      : not null access Program_Tree_Type'Class;
      Location : Aquarius.Layout.Position)
      return Program_Tree
   is
      use type Aquarius.Layout.Position;
      Last_Terminal : Program_Tree := null;

      procedure Find
        (Current : not null access Program_Tree_Type'Class);

      ----------
      -- Find --
      ----------

      procedure Find
        (Current : not null access Program_Tree_Type'Class)
      is
      begin
         if Current.Is_Terminal then
            if Current.Local_Start <= Location then
               Last_Terminal := Program_Tree (Current);
            else
               return;
            end if;
         else
            for I in 1 .. Current.Child_Count loop
               Find (Current.Program_Child (I));
            end loop;
         end if;
      end Find;

   begin
      Find (Top);
      return Last_Terminal;
   end Find_Node_At;

   ------------------
   -- Find_Node_At --
   ------------------

   function Find_Node_At
     (Parent   : not null access Program_Tree_Type'Class;
      Location : Aquarius.Source.Source_Position)
      return Program_Tree
   is
      use Aquarius.Source;
      Loc : constant Aquarius.Source.Source_Position := Parent.Get_Location;
   begin
      if Parent.Child_Count = 0 then
         if Get_Line (Loc) = Get_Line (Loc) then
            if Get_Column (Loc) >= Get_Column (Location) and then
              Get_Column (Loc) <= Column_Number (Parent.Layout_Length) + 1
            then
               return Program_Tree (Parent);
            else
               return null;
            end if;
         else
            return null;
         end if;
      else
         for I in 1 .. Parent.Child_Count loop
            declare
               Child : constant Program_Tree :=
                 Find_Node_At (Parent.Program_Child (I), Location);
            begin
               if Child /= null then
                  return Child;
               end if;
            end;
         end loop;
         return null;
      end if;
   end Find_Node_At;

   --------------------------
   -- Find_Node_Containing --
   --------------------------

   function Find_Node_Containing
     (Top      : not null access Program_Tree_Type'Class;
      Location : Aquarius.Layout.Position)
      return Program_Tree
   is
      Last_Terminal : Program_Tree := null;

      procedure Find (Current : not null access Program_Tree_Type'Class);

      ----------
      -- Find --
      ----------

      procedure Find
        (Current : not null access Program_Tree_Type'Class)
      is
      begin
         if Current.Is_Terminal then
            if Current.Contains_Position (Location) then
               Last_Terminal := Program_Tree (Current);
            else
               return;
            end if;
         else
            for I in 1 .. Current.Child_Count loop
               Find (Current.Program_Child (I));
            end loop;
         end if;
      end Find;

   begin
      Find (Top);
      return Last_Terminal;
   end Find_Node_Containing;

   -------------------------
   -- First_Program_Child --
   -------------------------

   function First_Program_Child (Item : Program_Tree_Type)
                                return Program_Tree
   is
   begin
      return Program_Tree (Item.First_Child);
   end First_Program_Child;

   -----------
   -- Frame --
   -----------

   function Frame (Item : Program_Tree_Type)
                  return Aquarius.Tokens.Token_Frame
   is
   begin
      return Item.Syntax.Frame;
   end Frame;

   ----------
   -- Free --
   ----------

   procedure Free (Item : in out Program_Tree) is
   begin
      for I in 1 .. Item.Child_Count loop
         declare
            Child_Item : Program_Tree := Program_Tree (Item.Child (I));
         begin
            Free (Child_Item);
         end;
      end loop;
      Free_List.Append (Item);
      Item.Free := True;
      Item := null;
      Num_Allocated_Trees := Num_Allocated_Trees - 1;
   end Free;

   -------------------------
   -- Get_Allocation_Info --
   -------------------------

   procedure Get_Allocation_Info
     (Allocated_Tree_Count : out Natural;
      Free_Tree_Count      : out Natural)
   is
   begin
      Allocated_Tree_Count := Num_Allocated_Trees;
      Free_Tree_Count      := Natural (Free_List.Length);
   end Get_Allocation_Info;

   ---------------
   -- Get_Entry --
   ---------------

   overriding
   function Get_Entry (Item : Program_Tree_Type)
                       return Aquarius.Entries.Table_Entry
   is
   begin
      return Aquarius.Entries.Table_Entry
        (Item.Property (Aquarius.Properties.Entry_Property));
   end Get_Entry;

   ---------------------------------
   -- Get_Inherited_Message_Level --
   ---------------------------------

   function Get_Inherited_Message_Level
     (Item  : Program_Tree_Type)
     return Aquarius.Messages.Message_Level
   is
   begin
      return Item.Msg_Level;
   end Get_Inherited_Message_Level;

   ------------------
   -- Get_Property --
   ------------------

   function Get_Property
     (Program  : in out Program_Tree_Type;
      Name     : String)
      return String
   is
   begin

      declare
         Position : constant String_Property_Maps.Cursor :=
                      Program.String_Props.Find (Name);
      begin
         if String_Property_Maps.Has_Element (Position) then
            return String_Property_Maps.Element (Position);
         end if;
      end;

      return "";
   end Get_Property;

   ---------------
   -- Get_Token --
   ---------------

   function Get_Token (Item : Program_Tree_Type'Class)
                      return Aquarius.Tokens.Token
   is
   begin
      return Item.Syntax.Token;
   end Get_Token;

   ----------------------------
   -- Get_Tree_From_Sequence --
   ----------------------------

   function Get_Tree_From_Sequence
     (Sequence : Positive)
      return Program_Tree
   is
   begin
      return Allocated_List.Element (Sequence);
   end Get_Tree_From_Sequence;

   --------------
   -- Get_Type --
   --------------

   overriding
   function Get_Type (Program : Program_Tree_Type)
                      return Aquarius.Types.Aquarius_Type
   is
   begin
      return Aquarius.Types.Aquarius_Type
        (Program.Property (Aquarius.Properties.Type_Property));
   end Get_Type;

   -------------------------
   -- Has_Cross_Reference --
   -------------------------

   function Has_Cross_Reference
     (Item : Program_Tree_Type'Class)
      return Boolean
   is
   begin
      return not Aquarius.Syntax.Is_Empty (Item.Syntax)
        and then Item.Syntax.Has_Cross_Reference;
   end Has_Cross_Reference;

   ---------------
   -- Has_Entry --
   ---------------

   overriding
   function Has_Entry
     (Item : Program_Tree_Type)
      return Boolean
   is
   begin
      return Item.Has_Property (Aquarius.Properties.Entry_Property);
   end Has_Entry;

   ---------------
   -- Has_Error --
   ---------------

   function Has_Error (Item : Program_Tree_Type)
                      return Boolean
   is
   begin
      return Item.Error_Tree;
   end Has_Error;

   ------------------------
   -- Has_Named_Property --
   ------------------------

   overriding
   function Has_Named_Property (Item : Program_Tree_Type;
                                Name : String)
                               return Boolean
   is
      use Aquarius.Properties;
      Pool : constant Property_Pool'Class :=
        Property_Pool'Class (Item.Property (Pool_Property).all);
   begin
      if Pool.Have_Property_Type (Name) then
         declare
            Prop : constant Property_Type := Pool.Get_Property_Type (Name);
         begin
            return Item.Has_Property (Prop);
         end;
      else
         return False;
      end if;
   end Has_Named_Property;

   ------------------
   -- Has_Property --
   ------------------

   function Has_Property
     (Program : Program_Tree_Type;
      Name    : String)
      return Boolean
   is
   begin
      return Program.String_Props.Contains (Name);
   end Has_Property;

   ----------------------------------
   -- Has_Soft_New_Line_Rule_After --
   ----------------------------------

   function Has_Soft_New_Line_Rule_After
     (Item : Program_Tree_Type'Class)
      return Boolean
   is
      Format       : constant Aquarius.Formats.Aquarius_Format :=
                       Item.Syntax.Get_Format;
      Rules        : constant Aquarius.Formats.Immediate_Rules :=
                       Formats.Rules (Format);
   begin
      return Aquarius.Formats.Enabled (Rules.Soft_New_Line_After)
        and then not Aquarius.Formats.Negative (Rules.Soft_New_Line_After);
   end Has_Soft_New_Line_Rule_After;

   -----------------------------------
   -- Has_Soft_New_Line_Rule_Before --
   -----------------------------------

   function Has_Soft_New_Line_Rule_Before
     (Item : Program_Tree_Type'Class)
      return Boolean
   is
      Format       : constant Aquarius.Formats.Aquarius_Format :=
                       Item.Syntax.Get_Format;
      Rules        : constant Aquarius.Formats.Immediate_Rules :=
                       Formats.Rules (Format);
   begin
      return Aquarius.Formats.Enabled (Rules.Soft_New_Line_Before)
        and then not Aquarius.Formats.Negative (Rules.Soft_New_Line_Before);
   end Has_Soft_New_Line_Rule_Before;

   ---------------------
   -- Has_Space_After --
   ---------------------

   function Has_Space_After
     (Item : Program_Tree_Type)
      return Boolean
   is
      Format       : constant Aquarius.Formats.Aquarius_Format :=
                       Item.Syntax.Get_Format;
      Rules        : constant Aquarius.Formats.Immediate_Rules :=
                       Formats.Rules (Format);
   begin
      return Aquarius.Formats.Enabled (Rules.Space_After)
        and then not Aquarius.Formats.Negative (Rules.Space_After);
   end Has_Space_After;

   ----------------------
   -- Has_Symbol_Table --
   ----------------------

   function Has_Symbol_Table
     (Item : Program_Tree_Type)
     return Boolean
   is
   begin
      return Item.Have_Symbol_Table;
   end Has_Symbol_Table;

   --------------
   -- Has_Type --
   --------------

   overriding
   function Has_Type (Program : Program_Tree_Type) return Boolean is
   begin
      return Program.Has_Property (Aquarius.Properties.Type_Property);
   end Has_Type;

   -----------
   -- Image --
   -----------

   overriding
   function Image (Item : Program_Tree_Type)
                  return String
   is
      use type Aquarius.Source.Source_Position;

      function Prefix return String;
      function Postfix return String;
      function Parent_Text return String;

      -----------------
      -- Parent_Text --
      -----------------

      function Parent_Text return String is
         T : Program_Tree := Item.Program_Parent;
      begin
         while T /= null
           and then T.Syntax.Name = ""
         loop
            T := T.Program_Parent;
         end loop;
         return (if T = null then "" else T.Syntax.Name & "/");
      end Parent_Text;

      -------------
      -- Postfix --
      -------------

      function Postfix return String is
      begin
         return "(" & Item.Syntax.Image & ")";
      end Postfix;

      ------------
      -- Prefix --
      ------------

      function Prefix return String is
      begin
         if Item.Get_Location /= Aquarius.Source.No_Source_Position then
            return "(" & Aquarius.Source.Show (Item.Get_Location) & ")";
         else
            return "";
         end if;
      end Prefix;

   begin
      if Item.Error_Node then
         return Prefix & "<" &
           Aquarius.Tokens.To_String (Item.Fill_Text) &
           ">";
      else
         return Prefix & " " & Parent_Text & Text (Item) & " " & Postfix;
      end if;
   end Image;

   -------------------
   -- Internal_Tree --
   -------------------

   function Internal_Tree (Item : Program_Tree_Type'Class) return Boolean is
   begin
      return not Item.Syntax.Has_Token
        and then Item.Syntax.Name = "";
   end Internal_Tree;

   ---------------
   -- Is_Choice --
   ---------------

   function Is_Choice  (Item : Program_Tree_Type) return Boolean is
      use Aquarius.Syntax;
   begin
      return Item.Syntax.Syntax_Class = Choice;
   end Is_Choice;

   ----------------
   -- Is_Comment --
   ----------------

   function Is_Comment (Item : Program_Tree_Type) return Boolean is
   begin
      return Item.Comment;
   end Is_Comment;

   --------------------
   -- Is_Declaration --
   --------------------

   function Is_Declaration
     (Item : Program_Tree_Type)
      return Boolean
   is
   begin
      return Item.Is_Declaration;
   end Is_Declaration;

   ---------------
   -- Is_Filled --
   ---------------

   function Is_Filled  (Item : Program_Tree_Type) return Boolean is
   begin
      return Item.Filled;
   end Is_Filled;

   --------------------------
   -- Is_Reserved_Terminal --
   --------------------------

   function Is_Reserved_Terminal
     (Item : Program_Tree_Type)
      return Boolean
   is
   begin
      return Item.Is_Terminal
        and then Aquarius.Tokens.Is_Reserved
          (Item.Syntax.Frame, Item.Syntax.Token);
   end Is_Reserved_Terminal;

   ------------------
   -- Is_Separator --
   ------------------

   function Is_Separator (Item : Program_Tree_Type) return Boolean is
   begin
      return Item.Separator_Node;
   end Is_Separator;

   -----------------
   -- Is_Terminal --
   -----------------

   function Is_Terminal  (Item : Program_Tree_Type) return Boolean is
      use Aquarius.Syntax;
   begin
      return Item.Syntax.Syntax_Class = Terminal;
   end Is_Terminal;

   -----------------------
   -- Layout_End_Column --
   -----------------------

   function Layout_End_Column
     (Item : Program_Tree_Type)
      return Aquarius.Layout.Column_Number
   is
   begin
      return Item.End_Column;
   end Layout_End_Column;

   -------------------------
   -- Layout_End_Position --
   -------------------------

   function Layout_End_Position
     (Item : Program_Tree_Type)
      return Aquarius.Layout.Position
   is
   begin
      return Item.End_Position;
   end Layout_End_Position;

   -------------------
   -- Layout_Length --
   -------------------

   function Layout_Length (Item : Program_Tree_Type)
                          return Aquarius.Layout.Count
   is
   begin
      if Item.Child_Count = 0 then
         declare
            Img : constant String := Item.Text;
         begin
            return Img'Length;
         end;
      else
         return 0;
      end if;
   end Layout_Length;

   -----------------
   -- Layout_Line --
   -----------------

   function Layout_Line (Item : Program_Tree_Type)
                         return Aquarius.Layout.Line_Number
   is
   begin
      return Item.Start_Line;
   end Layout_Line;

   -------------------------
   -- Layout_Start_Column --
   -------------------------

   function Layout_Start_Column
     (Item : Program_Tree_Type)
      return Aquarius.Layout.Column_Number
   is
   begin
      return Item.Start_Column;
   end Layout_Start_Column;

   ---------------------------
   -- Layout_Start_Position --
   ---------------------------

   function Layout_Start_Position (Item : Program_Tree_Type)
                                  return Aquarius.Layout.Position
   is
   begin
      return Item.Start_Position;
   end Layout_Start_Position;

   -----------------------
   -- Local_Environment --
   -----------------------

   function Local_Environment
     (Item : in out Program_Tree_Type'Class)
      return access Local_Environment_Interface'Class
   is
   begin
      if Item.Has_Environment then
         return Item.Local_Env;
      elsif Item.Program_Parent /= null then
         declare
            Result : constant Local_Environment_Access :=
                       Item.Program_Parent.Local_Environment;
         begin
            Item.Local_Env := Result;
            Item.Has_Environment := True;
            return Result;
         end;
      else
         Item.Has_Environment := True;
         Item.Local_Env := null;
         return null;
      end if;
   end Local_Environment;

   ---------------------
   -- Location_Column --
   ---------------------

--     overriding
--     function Location_Column (Location : Program_Tree_Type)
--                              return Positive
--     is
--     begin
--        return Positive (Location.Start_Position.Column);
--     end Location_Column;

   -------------------
   -- Location_Line --
   -------------------

--     overriding
--     function Location_Line (Location : Program_Tree_Type)
--                            return Positive
--     is
--     begin
--        return Positive (Location.Start_Position.Line);
--     end Location_Line;

   --------------------
   -- Minimum_Indent --
   --------------------

   function Minimum_Indent (Item : Program_Tree_Type)
                           return Aquarius.Source.Column_Number
   is
   begin
      if Item.Indent_Rule then
         return 2;
      else
         return 0;
      end if;
   end Minimum_Indent;

   --------------------
   -- New_Error_Tree --
   --------------------

   function New_Error_Tree (Position : Aquarius.Source.Source_Position;
                            Syntax   : Aquarius.Syntax.Syntax_Tree;
                            Message  : String)
                           return Program_Tree
   is
      Result : constant Program_Tree := New_Program_Tree (Syntax);
   begin
      Result.Set_Location (Position);
      Result.Fill (Message);
      Result.Error_Node := True;
      return Result;
   end New_Error_Tree;

   ---------------------
   -- New_Line_Before --
   ---------------------

   function New_Line_Before
     (Item : Program_Tree_Type'Class)
      return Boolean
   is
   begin
      return Item.NL;
   end New_Line_Before;

   -----------------
   -- New_Program --
   -----------------

   function New_Program (Syntax   : Aquarius.Syntax.Syntax_Tree;
                         Source   : Aquarius.Source.Source_File)
                        return Program_Tree
   is
      Result : constant Program_Tree := New_Program_Tree (Syntax);
   begin
      Result.Source_File := Source;
      Result.Source_File_Name :=
        Aquarius.Names.To_Aquarius_Name
          (Aquarius.Source.Get_Full_Path (Source));
      return Result;
   end New_Program;

   ----------------------
   -- New_Program_Root --
   ----------------------

   function New_Program_Root
     (Syntax      : Aquarius.Syntax.Syntax_Tree;
      Source      : Aquarius.Source.Source_File;
      Environment : not null access Local_Environment_Interface'Class)
      return Program_Tree
   is
   begin
      return Root : constant Program_Tree := New_Program (Syntax, Source) do
         Root.Has_Environment := True;
         Root.Local_Env := Environment;
      end return;
   end New_Program_Root;

   ----------------------
   -- New_Program_Tree --
   ----------------------

   function New_Program_Tree (Syntax   : Aquarius.Syntax.Syntax_Tree)
                             return Program_Tree
   is
      Result      : Program_Tree;
   begin
      pragma Assert (Aquarius.Syntax."/=" (Syntax, null));

      Num_Allocated_Trees := Num_Allocated_Trees + 1;
      if Free_List.Last_Index > 0 then
         Result := Free_List.Last_Element;
         Free_List.Delete (Free_List.Last_Index);
      else
         Result := new Program_Tree_Type;
      end if;
      Program_Tree_Type (Result.all) := Empty_Program_Node;
      Initialise_Tree (Result.all, Aquarius.Source.No_Source_Position,
                       Keep_Parent => True, Keep_Siblings => True);
      Allocated_List.Append (Result);
      Result.Sequence := Allocated_List.Last_Index;
      Result.Syntax         := Syntax;
      Result.Indent_Rule    := Syntax.Has_Indent_Rule;
      Result.Tree_Name      := Aquarius.Names.To_Aquarius_Name (Syntax.Name);
      Result.String_Props.Insert (Name_Property, Syntax.Name);

      if Syntax.Has_Render_Class then
         Result.Render_Class := Syntax;
      end if;
      Result.Self := Result;
      return Result;
   end New_Program_Tree;

   -----------------------
   -- Parent_Actionable --
   -----------------------

   overriding
   function Parent_Actionable
     (Child    : not null access Program_Tree_Type;
      Parent   : not null access Aquarius.Actions.Action_Source'Class)
     return access Aquarius.Actions.Actionable'Class
   is
      use type Aquarius.Syntax.Syntax_Tree;
      It : Program_Tree := Child.Program_Parent;
      S  : constant Aquarius.Syntax.Syntax_Tree :=
        Aquarius.Syntax.Syntax_Tree (Parent);
   begin
      while It /= null
        and then It.Syntax /= S
        and then It.Name = ""
      loop
         It := It.Program_Parent;
      end loop;

      if It.Syntax = S then
         return It;
      else
         return null;
      end if;

   end Parent_Actionable;

   -------------------
   -- Program_Child --
   -------------------

--     function Program_Child (Item  : Program_Tree_Type;
--                             Index : Positive)
--                            return Program_Tree
--     is
--     begin
--        return Program_Tree (Item.Child (Index));
--     end Program_Child;

   ----------------
   -- Path_Image --
   ----------------

   function Path_Image (Item : Program_Tree_Type) return String is
   begin
      if Item.Program_Parent = null then
         if Item.Name = "" then
            if Item.Syntax.Name = "" then
               return Item.Syntax.Show_Location;
            else
               return Item.Syntax.Name;
            end if;
         else
            return Item.Name;
         end if;
      elsif Item.Name /= "" then
         return Item.Program_Parent.Path_Image
           & "/" & Item.Name;
      else
         return Item.Program_Parent.Path_Image;
      end if;
   end Path_Image;

   -------------------
   -- Program_Child --
   -------------------

   function Program_Child (Item  : Program_Tree_Type;
                           Name  : String;
                           Index : Positive := 1)
                          return Program_Tree
   is
   begin
      return Program_Tree (Item.Breadth_First_Search (Name, Index));
   end Program_Child;

   ------------------
   -- Program_Left --
   ------------------

   function Program_Left (Item : Program_Tree_Type'Class)
                         return Program_Tree
   is
   begin
      return Program_Tree (Item.Left_Sibling);
   end Program_Left;

   --------------------
   -- Program_Parent --
   --------------------

   function Program_Parent (Item : Program_Tree_Type'Class)
                           return Program_Tree
   is
   begin
      return Program_Tree (Item.Parent);
   end Program_Parent;

   -------------------
   -- Program_Right --
   -------------------

   function Program_Right (Item : Program_Tree_Type'Class)
                         return Program_Tree
   is
   begin
      return Program_Tree (Item.Right_Sibling);
   end Program_Right;

   ------------------
   -- Program_Root --
   ------------------

   function Program_Root
     (Item : not null access Program_Tree_Type'Class)
      return Program_Tree
   is
      It : Program_Tree := Program_Tree (Item);
   begin
      while It.Program_Parent /= null loop
         It := It.Program_Parent;
      end loop;
      return It;
   end Program_Root;

   -----------------------
   -- Program_Root_Node --
   -----------------------

   function Program_Root_Node
     (Item : Program_Tree_Type'Class)
      return Program_Tree_Type'Class
   is
   begin
      if Item.Program_Parent = null then
         return Item;
      else
         return Item.Program_Parent.Program_Root.all;
      end if;
   end Program_Root_Node;

   ------------------
   -- Render_Class --
   ------------------

   function Render_Class (Item : Program_Tree_Type'Class) return String is
   begin
      if not Aquarius.Syntax.Is_Empty (Item.Render_Class) then
         return Item.Render_Class.Render_Class;
      elsif Item.Syntax.Has_Token then
         return Aquarius.Tokens.Get_Token_Class_Name
           (Item.Syntax.Frame,
            Item.Syntax.Token);
      else
         return "normal";
      end if;
   end Render_Class;

   -----------
   -- Rules --
   -----------

   function Rules
     (Item : Program_Tree_Type'Class)
      return Aquarius.Formats.Immediate_Rules
   is
      Format    : constant Aquarius.Formats.Aquarius_Format :=
                    Item.Syntax.Get_Format;
   begin
      return Aquarius.Formats.Rules (Format);
   end Rules;

   -----------------
   -- Run_Actions --
   -----------------

   procedure Run_Actions
     (Start        : in out Program_Tree_Type;
      Action_Group : Aquarius.Actions.Action_Group;
      Stop_After   : Program_Tree := null)
   is

      Stop : Boolean := False;

      procedure RA (T       : in out Program_Tree_Type'Class);

      --------
      -- RA --
      --------

      procedure RA (T       : in out Program_Tree_Type'Class) is

         Children : constant Array_Of_Program_Trees :=
           T.Direct_Children (Skip_Separators => False);
      begin

         Aquarius.Actions.Execute (T.Syntax.all, T,
                                   Action_Group, Before);

         for I in Children'Range loop
            RA (Children (I).all);
            exit when Stop;
         end loop;

         if Stop then
            return;
         end if;

         Aquarius.Actions.Execute (T.Syntax.all, T, Action_Group, After);

         if Stop_After /= null and then
           T = Stop_After.all
         then
            Stop := True;
         end if;

--          exception

--           when E : Constraint_Error =>
--              Ada.Text_IO.Put_Line
--                (Ada.Text_IO.Standard_Error,
--                 "Error in " & T.Image & ":");
--              Ada.Text_IO.Put_Line
--                (Ada.Text_IO.Standard_Error,
--                 Ada.Exceptions.Exception_Message (E));
--              raise;

      end RA;

   begin
      RA (Start);
   end Run_Actions;

   -----------------
   -- Run_Actions --
   -----------------

   procedure Run_Actions
     (Start        : in out Program_Tree_Type;
      Stop         : not null access Program_Tree_Type'Class;
      Action_Group : Aquarius.Actions.Action_Group)
   is

      Stopped    : Boolean := False;
      First_Node : constant Program_Tree := Start'Unchecked_Access;
      Last_Node  : Program_Tree;

      procedure RA (T : Program_Tree);

      --------
      -- RA --
      --------

      procedure RA (T : Program_Tree) is
         Children : constant Array_Of_Program_Trees :=
           T.Direct_Children (Skip_Separators => False);
      begin
         Last_Node := T;

         Aquarius.Actions.Execute (T.Syntax.all, T.all, Action_Group, Before);

         for I in Children'Range loop
            RA (Children (I));
            exit when Stopped;
         end loop;

         if not Stopped then
            Aquarius.Actions.Execute (T.Syntax.all, T.all,
                                      Action_Group, After);

            if T = Program_Tree (Stop) then
               Stopped := True;
            end if;

         end if;

      end RA;

   begin

--        Ada.Text_IO.Put_Line
--          (Ada.Text_IO.Standard_Error,
--           "Run_Actions: Start = " & Start.Image);
--        Ada.Text_IO.Put_Line
--          (Ada.Text_IO.Standard_Error,
--           "Run_Actions: Stop = " & Stop.Image);

      RA (First_Node);

      if not Stopped then
         --  continue from our nearest right parent
         declare
            Parent : Program_Tree := Last_Node.Program_Parent;
         begin
            while Aquarius.Trees.Is_Null (Parent.Right_Sibling) loop
               if Parent.Program_Parent = null then
                  Ada.Text_IO.Put_Line
                    (Ada.Text_IO.Standard_Error,
                     "while running actions up to " & Stop.Image);
                  Ada.Text_IO.Put_Line
                    (Ada.Text_IO.Standard_Error,
                     "reached root node or top of ambiguity: " & Parent.Image);
               end if;

               Parent := Parent.Program_Parent;
            end loop;

            Run_Actions (Parent.Program_Right.all, Stop, Action_Group);
         end;
      end if;

   end Run_Actions;

   -------------------
   -- Scan_Terminal --
   -------------------

   function Scan_Terminal
     (Start : not null access Program_Tree_Type'Class;
      Count : Integer)
      return Program_Tree
   is
      It : Program_Tree := Program_Tree (Start);
      Acc : Natural := 0;
   begin

      while Acc < abs Count loop
         if Count > 0 then
            It := Program_Tree (It.Next_Leaf);
         else
            It := Program_Tree (It.Previous_Leaf);
         end if;

         exit when It = null;

         if It.Is_Terminal
           and then It.Text /= ""
         then
            Acc := Acc + 1;
         end if;
      end loop;

      return It;

   end Scan_Terminal;

   ------------------------
   -- Separator_New_Line --
   ------------------------

   function Separator_New_Line (Item : Program_Tree_Type'Class)
                               return Boolean
   is
   begin
      return Item.Separator_NL;
   end Separator_New_Line;

   ---------------------
   -- Set_Declaration --
   ---------------------

   procedure Set_Declaration
     (Item : in out Program_Tree_Type)
   is
   begin
      Item.Is_Declaration := True;
   end Set_Declaration;

   ---------------
   -- Set_Entry --
   ---------------

   overriding
   procedure Set_Entry
     (Item : in out Program_Tree_Type;
      Ent  : not null access Entries.Table_Entry_Record'Class)
   is
   begin
      Item.Set_Property (Aquarius.Properties.Entry_Property, Ent);
   end Set_Entry;

   ---------------
   -- Set_Error --
   ---------------

   procedure Set_Error (Item  : in out Program_Tree_Type;
                        Value : Boolean)
   is
   begin
      Item.Error_Tree := Value;
   end Set_Error;

   ---------------------------------
   -- Set_Inherited_Message_Level --
   ---------------------------------

   procedure Set_Inherited_Message_Level
     (Item : in out Program_Tree_Type;
      Level : Aquarius.Messages.Message_Level)
   is
   begin
      Item.Msg_Level := Level;
   end Set_Inherited_Message_Level;

   -------------------------
   -- Set_Layout_Position --
   -------------------------

   procedure Set_Layout_Position
     (Item : in out Program_Tree_Type;
      Pos  : Aquarius.Layout.Position;
      Line : Aquarius.Layout.Line_Number;
      Col  : Aquarius.Layout.Column_Number)
   is
      use Aquarius.Layout;
   begin
      Item.Start_Position := Pos;
      Item.Start_Line := Line;
      Item.Start_Column := Col;

      Item.End_Position :=
        Item.Start_Position
          + Position_Offset (Item.Layout_Length) - 1;
      Item.End_Line := Line;
      Item.End_Column :=
        Item.Start_Column
          + Column_Offset (Item.Layout_Length) - 1;
   end Set_Layout_Position;

   -------------------------
   -- Set_New_Line_Before --
   -------------------------

   procedure Set_New_Line_Before
     (Item    : in out Program_Tree_Type'Class;
      Enabled : Boolean)
   is
   begin
      Item.NL := Enabled;
   end Set_New_Line_Before;

   ------------------
   -- Set_Property --
   ------------------

   procedure Set_Property
     (Program  : in out Program_Tree_Type;
      Name     : String;
      Value    : String)
   is
   begin
      Program.String_Props.Insert (Name, Value);
   end Set_Property;

   ----------------------------
   -- Set_Separator_New_Line --
   ----------------------------

   procedure Set_Separator_New_Line
     (Item : in out Program_Tree_Type'Class)
   is
   begin
      Item.Separator_NL := True;
   end Set_Separator_New_Line;

   -----------------------
   -- Set_Soft_New_Line --
   -----------------------

   procedure Set_Soft_New_Line (Item : in out Program_Tree_Type'Class) is
   begin
      Item.Soft_NL := True;
   end Set_Soft_New_Line;

   -------------------------
   -- Set_Source_Location --
   -------------------------

   procedure Set_Source_Position
     (Item : in out Program_Tree_Type;
      Pos  : Aquarius.Source.Source_Position)
   is
      use type Aquarius.Source.Source_Position;
      It : Program_Tree := Item.Program_Parent;
   begin
      Item.Set_Location (Pos);
      while It /= null and then
        It.Get_Location = Aquarius.Source.No_Source_Position
      loop
         It.Set_Location (Pos);
         It := Program_Tree (It.Parent);
      end loop;
   end Set_Source_Position;

   ----------------------
   -- Set_Symbol_Table --
   ----------------------

   procedure Set_Symbol_Table (Tree  : in out Program_Tree_Type;
                               Table : Aquarius.Entries.Symbol_Table)
   is
      use Aquarius.Properties;
   begin
      Tree.Set_Property (Symbol_Table_Property, Table);
   end Set_Symbol_Table;

   ---------------
   -- Set_Type --
   ---------------

   overriding
   procedure Set_Type
     (Program  : in out Program_Tree_Type;
      Property : not null access Types.Root_Aquarius_Type'Class)
   is
   begin
      Program.Set_Property (Aquarius.Properties.Type_Property, Property);
   end Set_Type;

   -----------------------------
   -- Set_Vertical_Gap_Before --
   -----------------------------

   procedure Set_Vertical_Gap_Before
     (Item  : in out Program_Tree_Type'Class;
      Gap   : Aquarius.Layout.Count)
   is
   begin
      Item.Vertical_Gap := Gap;
   end Set_Vertical_Gap_Before;

   -------------------
   -- Soft_New_Line --
   -------------------

   function Soft_New_Line (Item : Program_Tree_Type'Class)
                          return Boolean
   is
   begin
      return Item.Soft_NL;
   end Soft_New_Line;

   ------------
   -- Source --
   ------------

   function Source (Item : Program_Tree_Type'Class)
                   return Aquarius.Source.Source_File
   is
   begin
      return Item.Source_File;
   end Source;

   ----------------------
   -- Source_Directory --
   ----------------------

   function Source_Directory
     (Item : Program_Tree_Type'Class)
      return String
   is
   begin
      return Ada.Directories.Containing_Directory
        (Aquarius.Names.To_String (Item.Program_Root_Node.Source_File_Name));
   end Source_Directory;

   ----------------------
   -- Source_File_Name --
   ----------------------

   function Source_File_Name
     (Item : Program_Tree_Type'Class)
      return String
   is
   begin
      return Ada.Directories.Simple_Name
        (Aquarius.Names.To_String (Item.Program_Root_Node.Source_File_Name));
   end Source_File_Name;

   -------------------
   -- Standard_Text --
   -------------------

   overriding
   function Standard_Text (Item : Program_Tree_Type) return String is
   begin
      return Ada.Characters.Handling.To_Lower (Item.Text);
   end Standard_Text;

   -------------------
   -- Start_Of_Line --
   -------------------

   function Start_Of_Line
     (Tree   : not null access Program_Tree_Type'Class)
      return Program_Tree
   is
      use type Aquarius.Layout.Line_Number;
      Result : Program_Tree := Program_Tree (Tree);
      Next   : Program_Tree := Result.Scan_Terminal (-1);
   begin
      while Next /= null
        and then Next.Layout_Line = Tree.Layout_Line
      loop
         Result := Next;
         Next := Next.Scan_Terminal (-1);
      end loop;
      return Result;
   end Start_Of_Line;

   ------------------
   -- Symbol_Table --
   ------------------

   function Symbol_Table (Tree : Program_Tree_Type)
                         return Aquarius.Entries.Symbol_Table
   is
      use Aquarius.Properties;
   begin
      if Tree.Has_Property (Symbol_Table_Property) then
         return Aquarius.Entries.Symbol_Table
           (Tree.Property (Symbol_Table_Property));
      else
         raise Constraint_Error with
           "no symbol table found at " &
           Aquarius.Source.Show (Tree.Get_Location);
      end if;
   end Symbol_Table;

   ------------------
   -- Syntax_Index --
   ------------------

   function Syntax_Index (Tree : not null access Program_Tree_Type)
                         return Natural
   is
      Result : Natural := 0;
      Parent : constant Program_Tree := Program_Tree (Tree.Parent);
      Left   : Program_Tree := null;
   begin
      if Parent = null then
         return 0;
      end if;

      for I in 1 .. Parent.Child_Count loop
         declare
            use type Aquarius.Syntax.Syntax_Tree;
            Child : constant Program_Tree := Program_Tree (Parent.Child (I));
         begin
            if Left = null or else Left.Syntax /= Child.Syntax then
               Result := Result + 1;
            end if;
            exit when Child = Program_Tree (Tree);
            Left := Child;
         end;
      end loop;
      return Result;
   end Syntax_Index;

   ----------
   -- Text --
   ----------

   overriding
   function Text (Item : Program_Tree_Type) return String is
      use Aquarius.Syntax;
   begin
      pragma Assert (not Item.Free);
      if Item.Error_Node then
         return Aquarius.Tokens.To_String (Item.Fill_Text);
      elsif Item.Syntax.Has_Token then
         if Aquarius.Tokens.Is_Reserved (Item.Frame, Item.Syntax.Token) then
            return Aquarius.Tokens.Get_Name (Item.Frame, Item.Syntax.Token);
         else
            return Aquarius.Tokens.To_String (Item.Fill_Text);
         end if;
      else
         return Item.Syntax.Non_Terminal_Name;
      end if;
   end Text;

   ------------------
   -- Update_Entry --
   ------------------

   procedure Update_Entry
     (Item    : in out Program_Tree_Type'Class)
   is
      use Aquarius.Entries;
      E : constant Table_Entry := Item.Get_Entry;
   begin
      if Item.Text /= E.Name then
         if E.Declaration.Same_Node (Item) then
            E.Rename (Item.Standard_Text);
         else
            Item.Clear_Property (Aquarius.Properties.Entry_Property);
         end if;
      end if;
   end Update_Entry;

   -------------------------
   -- Vertical_Gap_Before --
   -------------------------

   function Vertical_Gap_Before
     (Item : Program_Tree_Type'Class)
      return Aquarius.Layout.Count
   is
   begin
      return Item.Vertical_Gap;
   end Vertical_Gap_Before;

end Aquarius.Programs;
