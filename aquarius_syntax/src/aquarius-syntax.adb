with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;

package body Aquarius.Syntax is

   type Named_Format is
      record
         Name    : Aquarius.Names.Aquarius_Name;
         Format  : Aquarius.Formats.Aquarius_Format;
      end record;

   package Named_Format_Vector is
     new Ada.Containers.Vectors (Positive, Named_Format);

   type Syntax_Tree_Node_Record is
      record
         Class             : Node_Class;
         Non_Terminal_Name : Aquarius.Names.Aquarius_Name :=
                               Aquarius.Names.Null_Aquarius_Name;
         Declaration       : Aquarius.Trees.Tree;
         Has_Token         : Boolean := False;
         Token             : Aquarius.Tokens.Token;
         Frame             : Aquarius.Tokens.Token_Frame;
         Formats           : Named_Format_Vector.Vector;
         Format_Option     : Aquarius.Names.Aquarius_Name :=
                               Aquarius.Names.Null_Aquarius_Name;
         Simple_Format     : Aquarius.Formats.Aquarius_Format;
         Special_Format    : Special_Format_Function;
         Vertical_Align    : Syntax_Tree;
         Separator         : Syntax_Tree;
         Symbol_Cached     : Aquarius.Tokens.Token_Set;
         Symbol_Begins     : Aquarius.Tokens.Token_Set;
         Actions           : Aquarius.Actions.Action_Instance_List;
         X_Ref_Child       : Syntax_Tree;
         Has_Table         : Boolean := False;
         Has_Properties    : Boolean := False;
         Has_Format        : Boolean := False;
         Has_Simple_Format : Boolean := False;
         Super_Class       : Boolean := False;
         Has_Format_Option : Boolean := False;
         Optional          : Boolean := False;
         Repeatable        : Boolean := False;
         Is_Separator      : Boolean := False;
      end record;

   --  New_Tree: make a new tree of the given class
   function New_Tree
     (Frame       : Aquarius.Tokens.Token_Frame;
      Declaration : not null access Aquarius.Trees.Root_Tree_Type'Class;
      Class       : Node_Class)
     return Syntax_Tree;

   ---------------
   -- Add_Child --
   ---------------

   overriding
   procedure Add_Child
     (Item      : not null access Syntax_Tree_Record;
      New_Child : not null access Aquarius.Trees.Root_Tree_Type'Class)
   is
   begin
      if Item.Has_Token then
         raise Constraint_Error with
           "attempt to add child <" & New_Child.Image &
           "> to terminal node <" & Item.Image & ">";
      end if;

      Aquarius.Trees.Add_Child
        (Aquarius.Trees.Root_Tree_Type (Item.all)'Access,
         New_Child);
   end Add_Child;

   -----------------------
   -- Add_Format_Option --
   -----------------------

   procedure Add_Format_Option (Item  : not null access Syntax_Tree_Record;
                                Name  : String)
   is
   begin
      Item.Node.Has_Format_Option := True;
      Item.Node.Format_Option     := Aquarius.Names.To_Aquarius_Name (Name);
      Item.Pristine := False;
   end Add_Format_Option;

   -------------------------------
   -- Add_Precondition_Property --
   -------------------------------

   procedure Add_Precondition_Property
     (Item          : not null access Syntax_Tree_Record;
      Property_Name : String)
   is
   begin
      Item.When_Properties.Insert (Property_Name);
      Item.Pristine := False;
   end Add_Precondition_Property;

   ----------------------
   -- Add_Single_Child --
   ----------------------

   procedure Add_Single_Child
     (Item      : not null access Syntax_Tree_Record;
      New_Child : not null access Syntax_Tree_Record)
   is
   begin
      --  under the right conditions (a sequence tree)
      --  we can simply replace the item with the child
      if Item.Syntax_Class = Non_Terminal and then
        Item.Node.Repeatable = False and then
        Item.Node.Optional = False
      then

         --  ok, not so simple
         Item.Node.Class      := New_Child.Node.Class;
         Item.Node.Repeatable := New_Child.Node.Repeatable;
         Item.Node.Optional   := New_Child.Node.Optional;
         Item.Node.Separator  := New_Child.Node.Separator;
         for I in 1 .. New_Child.Child_Count loop
            Item.Add_Child (New_Child.Child (I));
         end loop;
      else
         Item.Add_Child (New_Child);
      end if;
   end Add_Single_Child;

   ------------------
   -- Align_Parent --
   ------------------

   function Align_Parent
     (Item : not null access Syntax_Tree_Record)
     return Syntax_Tree
   is
   begin
      return Item.Node.Vertical_Align;
   end Align_Parent;

   -------------------
   -- Append_Action --
   -------------------

   overriding procedure Append_Action
     (Source : in out Syntax_Tree_Record;
      Action : Aquarius.Actions.Action_Instance)
   is
   begin
      Aquarius.Actions.Append (Source.Node.Actions, Action);
   end Append_Action;

   -------------------
   -- Cached_Begins --
   -------------------

   function Cached_Begins (Tree : not null access Syntax_Tree_Record;
                           Tok  : Aquarius.Tokens.Token)
                          return Boolean
   is
      use Aquarius.Tokens;
   begin
      pragma Assert (Is_Begins_Cached (Tree, Tok));
      return Tok <= Tree.Node.Symbol_Begins;
   end Cached_Begins;

   ------------------------
   -- Check_Precondition --
   ------------------------

   function Check_Precondition
     (For_Syntax : not null access Syntax_Tree_Record;
      Test_Tree  : not null access Aquarius.Trees.Root_Tree_Type'Class)
     return Boolean
   is
      use Aquarius.Names;

      function Test (Name : Aquarius_Name) return Boolean
      is (Test_Tree.Has_Named_Property (Name));

   begin
      return For_Syntax.When_Properties.Is_Empty
        or else For_Syntax.When_Properties.Any (Test'Access);
   end Check_Precondition;

   --------------------------
   -- Cross_Reference_Name --
   --------------------------

   function Cross_Reference_Name
     (Item : Syntax_Tree_Record'Class)
      return Syntax_Tree
   is
   begin
      return Item.Node.X_Ref_Child;
   end Cross_Reference_Name;

   -----------------
   -- Declaration --
   -----------------

   function Declaration (Item : Syntax_Tree_Record)
                        return Aquarius.Trees.Tree
   is
   begin
      return Item.Node.Declaration;
   end Declaration;

   ------------------------
   -- Enable_Indent_Rule --
   ------------------------

   procedure Enable_Indent_Rule (Item : not null access Syntax_Tree_Record) is
   begin
      Item.Has_Indent_Rule := True;
      Item.Pristine := False;
   end Enable_Indent_Rule;

   ----------------
   -- Find_Child --
   ----------------

   function Find_Child (Current    : not null access Syntax_Tree_Record;
                        Child_Name : String)
                       return Syntax_Tree
   is
   begin
      return Find_Child (Current, Child_Name, 1);
   end Find_Child;

   ----------------
   -- Find_Child --
   ----------------

   function Find_Child (Current    : not null access Syntax_Tree_Record;
                        Child_Name : String;
                        Index      : Positive)
                       return Syntax_Tree
   is
      Local_Index : Natural := Index;

      function FC (Child : Syntax_Tree) return Syntax_Tree;

      --------
      -- FC --
      --------

      function FC (Child : Syntax_Tree) return Syntax_Tree is
      begin
         if Child /= Current and then Child.Name = Child_Name then
            Local_Index := Local_Index - 1;
            if Local_Index = 0 then
               return Child;
            else
               return null;
            end if;
         else
            if Child.Separator /= null then
               declare
                  Result : constant Syntax_Tree := FC (Child.Separator);
               begin
                  if Result /= null then
                     return Result;
                  end if;
               end;
            end if;

            for I in 1 .. Child.Child_Count loop
               declare
                  Result : constant Syntax_Tree :=
                             FC (Syntax_Tree (Child.Child (I)));
               begin
                  if Result /= null then
                     return Result;
                  end if;
               end;
            end loop;

            return null;
         end if;
      end FC;

   begin
      return FC (Syntax_Tree (Current));
   end Find_Child;

   ------------
   -- Format --
   ------------

   function Format (Item : not null access Syntax_Tree_Record;
                    Name : String)
                   return Aquarius.Formats.Aquarius_Format
   is
      use Aquarius.Names;
   begin
      for I in 1 .. Item.Node.Formats.Last_Index loop
         if Item.Node.Formats.Element (I).Name = Name then
            return Item.Node.Formats.Element (I).Format;
         end if;
      end loop;
      raise Syntax_Tree_Error with
        "expected to find a format called '" & Name & "'";
   end Format;

   -------------------
   -- Format_Option --
   -------------------

   function Format_Option
     (Item : Syntax_Tree_Record)
     return String
   is
   begin
      return Aquarius.Names.To_String (Item.Node.Format_Option);
   end Format_Option;

   -----------
   -- Frame --
   -----------

   function Frame (Item : Syntax_Tree_Record)
                  return Aquarius.Tokens.Token_Frame
   is
   begin
      return Item.Node.Frame;
   end Frame;

   ---------------------
   -- Get_Action_List --
   ---------------------

   overriding
   function Get_Action_List (Source : not null access Syntax_Tree_Record)
                            return Aquarius.Actions.Action_Instance_List
   is
   begin
      Source.Pristine := False;
      return Source.Node.Actions;
   end Get_Action_List;

   ----------------
   -- Get_Format --
   ----------------

   function Get_Format (Item : Syntax_Tree_Record)
                       return Aquarius.Formats.Aquarius_Format
   is
   begin
      if Item.Node.Has_Simple_Format then
         return Item.Node.Simple_Format;
      elsif Item.Child_Count > 0 then
         return Aquarius.Formats.Default_Non_Terminal_Format;
      else
         return Aquarius.Formats.Default_Terminal_Format;
      end if;
   end Get_Format;

   ------------------------
   -- Get_Special_Format --
   ------------------------

   function Get_Special_Format
     (Item : Syntax_Tree_Record)
     return Special_Format_Function
   is
   begin
      return Item.Node.Special_Format;
   end Get_Special_Format;

   -------------------------
   -- Has_Cross_Reference --
   -------------------------

   function Has_Cross_Reference
     (Item : Syntax_Tree_Record'Class)
      return Boolean
   is
   begin
      return Item.Node.X_Ref_Child /= null;
   end Has_Cross_Reference;

   ----------------
   -- Has_Format --
   ----------------

   function Has_Format (Item : Syntax_Tree_Record) return Boolean is
   begin
      return Item.Node.Has_Format;
   end Has_Format;

   ----------------
   -- Has_Format --
   ----------------

   function Has_Format (Item : not null access Syntax_Tree_Record;
                        Name : String)
                       return Boolean
   is
      use Aquarius.Names;
   begin
      for I in 1 .. Item.Node.Formats.Last_Index loop
         if Item.Node.Formats.Element (I).Name = Name then
            return True;
         end if;
      end loop;
      return False;
   end Has_Format;

   -----------------------
   -- Has_Format_Option --
   -----------------------

   function Has_Format_Option
     (Item : Syntax_Tree_Record)
     return Boolean
   is
   begin
      return Item.Node.Has_Format_Option;
   end Has_Format_Option;

   ---------------------
   -- Has_Indent_Rule --
   ---------------------

   function Has_Indent_Rule (Item : not null access Syntax_Tree_Record)
                            return Boolean
   is
   begin
      return Item.Has_Indent_Rule;
   end Has_Indent_Rule;

   ------------------------
   -- Has_Named_Property --
   ------------------------

   overriding
   function Has_Named_Property (Item : Syntax_Tree_Record;
                                Name : String)
                               return Boolean
   is
      pragma Unreferenced (Item);
      pragma Unreferenced (Name);
   begin
      return False;
   end Has_Named_Property;

   ---------------------
   -- Has_Plugin_Mark --
   ---------------------

   function Has_Plugin_Mark (Item : Syntax_Tree_Record)
                            return Boolean
   is
   begin
      return Item.Has_Plugin_Mark;
   end Has_Plugin_Mark;

   ----------------------
   -- Has_Render_Class --
   ----------------------

   function Has_Render_Class (Item : Syntax_Tree_Record)
                             return Boolean
   is
   begin
      return Item.Has_Render_Class;
   end Has_Render_Class;

   -------------------
   -- Has_Separator --
   -------------------

   function Has_Separator
     (Item : not null access Syntax_Tree_Record'Class)
     return Boolean
   is
   begin
      return Item.Node.Separator /= null;
   end Has_Separator;

   -----------------------
   -- Has_Simple_Format --
   -----------------------

--     function Has_Simple_Format
--       (Item : not null access Syntax_Tree_Record)
--       return Boolean
--     is
--     begin
--        return Has_Format (Item) and then
--          (Item.Node.Has_Simple_Format and Item.Node.Special_Format = null);
--     end Has_Simple_Format;

   ---------------
   -- Has_Token --
   ---------------

   function Has_Token (Item : Syntax_Tree_Record)
                      return Boolean
   is
   begin
      return Item.Node.Has_Token;
   end Has_Token;

   -----------
   -- Image --
   -----------

   overriding function Image (Tree : Syntax_Tree_Record) return String is

      function Node_Class_Image (Class : Node_Class) return String;
      function Children_Image
        (Child : access constant Syntax_Tree_Record'Class)
         return String;
      function Separator_Image return String;

      --------------------
      -- Children_Image --
      --------------------

      function Children_Image
        (Child : access constant Syntax_Tree_Record'Class)
         return String
      is
         use Ada.Strings.Unbounded;
         Result : Unbounded_String := Null_Unbounded_String;
      begin
         for I in 1 .. Child.Child_Count loop
            declare
               Grandchild : constant Syntax_Tree :=
                 Syntax_Tree (Child.Child (I));
               Gc_Image   : constant String :=
                 Node_Class_Image (Grandchild.Node.Class) &
                 "[" & Grandchild.Name & "]";
            begin
               if Result /= Null_Unbounded_String then
                  Result := Result & ",";
               end if;
               Result := Result & Gc_Image;
            end;
         end loop;
         return To_String (Result);
      end Children_Image;

      ----------------------
      -- Node_Class_Image --
      ----------------------

      function Node_Class_Image (Class : Node_Class) return String is
      begin
         case Class is
            when Non_Terminal  => return "Non-Terminal";
            when Terminal      => return "Terminal";
            when Choice        => return "Choice";
         end case;
      end Node_Class_Image;

      ---------------------
      -- Separator_Image --
      ---------------------

      function Separator_Image return String is
      begin
         if Tree.Separator /= null then
            return "/" & Tree.Separator.Image;
         else
            return "";
         end if;
      end Separator_Image;

      Prefix  : constant String :=
                  (if Tree.Name /= ""
                   then "'" & Tree.Name & "' "
                   else "")
                  & "(" & Tree.Show_Location & ")";
      Postfix : constant String := Children_Image (Tree'Access);
   begin
      case Tree.Node.Class is
         when Terminal =>
            return "Terminal[" & Prefix & "]";
         when Choice =>
            return Prefix & ":Choice(" & Postfix & ")";
         when Non_Terminal =>
            if Tree.Node.Optional then
               if Tree.Node.Repeatable then
                  return Prefix & ":optional-repeat@" &
                    Separator_Image & Postfix;
               else
                  return Prefix & ":optional@" &
                    Separator_Image & Postfix;
               end if;
            else
               if Tree.Node.Repeatable then
                  return Prefix & ":required-repeat@" &
                    Separator_Image & Postfix;
               else
                  return Prefix & ":sequence@" &
                    Separator_Image & Postfix;
               end if;
            end if;
      end case;
   end Image;

   ------------
   -- Indent --
   ------------

   function Indent
     (Tree : Syntax_Tree_Record)
      return Aquarius.Locations.Column_Count
   is
   begin
      return Tree.Indent;
   end Indent;

   --------------
   -- Indented --
   --------------

   function Indented (Tree : Syntax_Tree_Record) return Boolean is
      use type Aquarius.Locations.Column_Count;
   begin
      return Tree.Indent /= 0;
   end Indented;

   ----------------------
   -- Is_Begins_Cached --
   ----------------------

   function Is_Begins_Cached (Tree : not null access Syntax_Tree_Record;
                              Tok  : Aquarius.Tokens.Token)
                             return Boolean
   is
      use Aquarius.Tokens;
   begin
      return Tok <= Tree.Node.Symbol_Cached;
   end Is_Begins_Cached;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Item : Syntax_Tree) return Boolean is
   begin
      return Item = null;
   end Is_Empty;

   ---------------
   -- Is_Repeat --
   ---------------

   function Is_Repeat
     (Item : not null access Syntax_Tree_Record'Class)
     return Boolean
   is
   begin
      return Item.Repeatable;
   end Is_Repeat;

   ------------------
   -- Is_Separator --
   ------------------

   function Is_Separator (Item : Syntax_Tree_Record) return Boolean is
   begin
      return Item.Node.Is_Separator;
   end Is_Separator;

   -----------------
   -- Is_Sequence --
   -----------------

   function Is_Sequence
     (Item : not null access Syntax_Tree_Record'Class)
     return Boolean
   is
   begin
      return Item.Syntax_Class = Non_Terminal and then
        Item.Optional = False and then
        Item.Repeatable = False;
   end Is_Sequence;

   -------------------
   -- Is_Superclass --
   -------------------

   function Is_Superclass (Item : Syntax_Tree_Record) return Boolean is
   begin
      return Item.Node.Super_Class;
   end Is_Superclass;

   ----------
   -- Name --
   ----------

   overriding
   function Name (Tree : Syntax_Tree_Record) return String is
   begin
      if not Tree.Has_Token then
         return Aquarius.Names.To_String (Tree.Node.Non_Terminal_Name);
      else
         return Aquarius.Tokens.Get_Name (Tree.Node.Frame, Tree.Token);
      end if;
   end Name;

   ----------------
   -- New_Choice --
   ----------------

   function New_Choice
     (Frame       : Aquarius.Tokens.Token_Frame;
      Declaration : not null access Aquarius.Trees.Root_Tree_Type'Class)
      return Syntax_Tree
   is
   begin
      return New_Tree (Frame, Declaration, Choice);
   end New_Choice;

   ------------------
   -- New_Optional --
   ------------------

   function New_Optional
     (Frame       : Aquarius.Tokens.Token_Frame;
      Declaration : not null access Aquarius.Trees.Root_Tree_Type'Class)
     return Syntax_Tree
   is
      Result : constant Syntax_Tree :=
        New_Tree (Frame, Declaration, Non_Terminal);
   begin
      Result.Node.Optional := True;
      Result.Node.Repeatable := False;
      return Result;
   end New_Optional;

   ----------------
   -- New_Repeat --
   ----------------

   function New_Repeat
     (Frame       : Aquarius.Tokens.Token_Frame;
      Declaration : not null access Aquarius.Trees.Root_Tree_Type'Class;
      Optional    : Boolean;
      Separator   : Syntax_Tree)
     return Syntax_Tree
   is
      Result : constant Syntax_Tree :=
        New_Tree (Frame, Declaration, Non_Terminal);
   begin
      Result.Node.Optional   := Optional;
      Result.Node.Repeatable := True;
      if Separator /= Empty_Tree then
         Result.Node.Separator := Separator;
         Result.Node.Separator.Node.Is_Separator := True;
      end if;
      return Result;
   end New_Repeat;

   ------------------
   -- New_Sequence --
   ------------------

   function New_Sequence
     (Frame       : Aquarius.Tokens.Token_Frame;
      Declaration : not null access Aquarius.Trees.Root_Tree_Type'Class)
     return Syntax_Tree
   is
      Result : constant Syntax_Tree :=
        New_Tree (Frame, Declaration, Non_Terminal);
   begin
      Result.Node.Optional := False;
      Result.Node.Repeatable := False;
      return Result;
   end New_Sequence;

   ------------------
   -- New_Terminal --
   ------------------

   function New_Terminal
     (Frame       : Aquarius.Tokens.Token_Frame;
      Declaration : not null access Aquarius.Trees.Root_Tree_Type'Class;
      Tree_Token  : Aquarius.Tokens.Token)
      return Syntax_Tree
   is
      Result : constant Syntax_Tree :=
        New_Tree (Frame, Declaration, Terminal);
   begin
      Result.Node.Has_Token := True;
      Result.Node.Token     := Tree_Token;
      return Result;
   end New_Terminal;

   --------------
   -- New_Tree --
   --------------

   function New_Tree
     (Frame       : Aquarius.Tokens.Token_Frame;
      Declaration : not null access Aquarius.Trees.Root_Tree_Type'Class;
      Class       : Node_Class)
     return Syntax_Tree
   is
      Result : constant Syntax_Tree := new Syntax_Tree_Record;
   begin
      Result.Initialise_Tree
        (Source        => Declaration.Source,
         Location      => Declaration.all,
         Keep_Parent   => False,
         Keep_Siblings => False);
      Result.Node := new Syntax_Tree_Node_Record;
      Result.Node.Class         := Class;
      Result.Node.Frame         := Frame;
      Result.Node.Declaration   := Aquarius.Trees.Tree (Declaration);
      Result.Node.Has_Token     := False;
      Result.Node.Symbol_Begins := Aquarius.Tokens.Empty;
      Result.Node.Symbol_Cached := Aquarius.Tokens.Empty;
      return Result;
   end New_Tree;

   -----------------------
   -- Non_Terminal_Name --
   -----------------------

   function Non_Terminal_Name
     (Item         : not null access Syntax_Tree_Record)
     return String
   is
   begin
      return Aquarius.Names.To_String (Item.Node.Non_Terminal_Name);
   end Non_Terminal_Name;

   --------------
   -- Optional --
   --------------

   function Optional
     (Item : Syntax_Tree_Record)
      return Boolean
   is
   begin
      return Item.Node.Optional;
   end Optional;

   -----------------
   -- Plugin_Mark --
   -----------------

   procedure Plugin_Mark (Item : in out Syntax_Tree_Record) is
   begin
      Item.Has_Plugin_Mark := True;
   end Plugin_Mark;

   ----------------
   -- Referenced --
   ----------------

   function Referenced (Item : not null access Syntax_Tree_Record)
                       return Boolean
   is
   begin
      return Item.Referenced_Flag;
   end Referenced;

   ------------------
   -- Render_Class --
   ------------------

   function Render_Class (Item : Syntax_Tree_Record)
                         return String
   is
   begin
      pragma Assert (Item.Has_Render_Class);
      return Aquarius.Names.To_String (Item.Render_Class);
   end Render_Class;

   ----------------
   -- Repeatable --
   ----------------

   function Repeatable
     (Item : Syntax_Tree_Record)
      return Boolean
   is
   begin
      return Item.Node.Repeatable;
   end Repeatable;

   ----------
   -- Same --
   ----------

   function Same (Left, Right : Syntax_Tree) return Boolean is
   begin
      return Left.Node = Right.Node;
   end Same;

   ---------------
   -- Separator --
   ---------------

   function Separator
     (Item : Syntax_Tree_Record)
      return Syntax_Tree
   is
   begin
      return Item.Node.Separator;
   end Separator;

   ----------------
   -- Set_Begins --
   ----------------

   procedure Set_Begins (Tree   : not null access Syntax_Tree_Record;
                         Tok    : Aquarius.Tokens.Token;
                         Begins : Boolean)
   is
   begin
      if Begins then
         Aquarius.Tokens.Add (Tok, Tree.Node.Symbol_Begins);
      end if;
      Aquarius.Tokens.Add (Tok, Tree.Node.Symbol_Cached);
      pragma Assert (Is_Begins_Cached (Tree, Tok));
      pragma Assert (Cached_Begins (Tree, Tok) = Begins);
   end Set_Begins;

   -------------------------
   -- Set_Cross_Reference --
   -------------------------

   procedure Set_Cross_Reference
     (Item       : in out Syntax_Tree_Record'Class;
      Name_Child : Syntax_Tree)
   is
   begin
      Item.Node.X_Ref_Child := Name_Child;
   end Set_Cross_Reference;

   ---------------------
   -- Set_Declaration --
   ---------------------

   procedure Set_Declaration
     (Item : in out Syntax_Tree_Record;
      Dec  : not null access Aquarius.Trees.Root_Tree_Type'Class)
   is
   begin
      Item.Node.Declaration := Aquarius.Trees.Tree (Dec);
   end Set_Declaration;

   ----------------
   -- Set_Format --
   ----------------

   procedure Set_Format (Item : in out Syntax_Tree_Record;
                         To   : Aquarius.Formats.Aquarius_Format)
   is
   begin
      Item.Node.Has_Format        := True;
      Item.Node.Has_Simple_Format := True;
      Item.Node.Simple_Format := To;
      Item.Pristine := False;
   end Set_Format;

   ----------------
   -- Set_Format --
   ----------------

   procedure Set_Format (Item : in out Syntax_Tree_Record;
                         To   : Special_Format_Function)
   is
   begin
      Item.Node.Special_Format := To;
      Item.Pristine := False;
   end Set_Format;

   ----------------
   -- Set_Format --
   ----------------

   procedure Set_Format (Item : not null access Syntax_Tree_Record;
                         Name : String;
                         To   : Aquarius.Formats.Aquarius_Format)
   is
   begin
      Item.Node.Formats.Append
        (Named_Format'
           (Aquarius.Names.To_Aquarius_Name (Name), To));
      Item.Pristine := False;
   end Set_Format;

   ---------------------------
   -- Set_Non_Terminal_Name --
   ---------------------------

   procedure Set_Non_Terminal_Name
     (Item         : not null access Syntax_Tree_Record;
      Non_Terminal : String)
   is
   begin
      Item.Node.Non_Terminal_Name :=
        Aquarius.Names.To_Aquarius_Name (Non_Terminal);
      Item.Pristine := False;
   end Set_Non_Terminal_Name;

   --------------------
   -- Set_Referenced --
   --------------------

   procedure Set_Referenced (Item : not null access Syntax_Tree_Record) is
   begin
      Item.Referenced_Flag := True;
   end Set_Referenced;

   ----------------------
   -- Set_Render_Class --
   ----------------------

   procedure Set_Render_Class (Item       : in out Syntax_Tree_Record;
                               Class_Name : String)
   is
   begin
      Item.Render_Class := Aquarius.Names.To_Aquarius_Name (Class_Name);
      Item.Has_Render_Class := True;
      Item.Pristine := False;
   end Set_Render_Class;

   -------------------
   -- Set_Separator --
   -------------------

   procedure Set_Separator (Item      : in out Syntax_Tree_Record;
                            Separator : Syntax_Tree)
   is
   begin
      Item.Node.Separator := Separator;
      Item.Pristine := False;
   end Set_Separator;

   --------------------
   -- Set_Superclass --
   --------------------

   procedure Set_Superclass (Item : in out Syntax_Tree_Record) is
   begin
      Item.Node.Super_Class := True;
      Item.Pristine := False;
   end Set_Superclass;

   ------------------
   -- Syntax_Child --
   ------------------

--     function Syntax_Child
--       (Item  : not null access Syntax_Tree_Record;
--        Index : in     Positive)
--       return Syntax_Tree
--     is
--        Syn_Child : constant Syntax_Tree := Syntax_Tree (Item.Child (Index));
--     begin
--  --        while Syn_Child.Syntax_Class = Non_Terminal and
--  --          not Syn_Child.Node.Optional and
--  --          not Syn_Child.Node.Repeatable and
--  --          Syn_Child.Child_Count = 1 and
--  --          Syn_Child.Pristine
--  --        loop
--  --           Syn_Child := Syntax_Tree (Syn_Child.Child (1));
--  --        end loop;
--        return Syn_Child;
--     end Syntax_Child;

   ------------------
   -- Syntax_Class --
   ------------------

   function Syntax_Class
     (Item : not null access Syntax_Tree_Record)
     return Node_Class
   is
   begin
      return Item.Node.Class;
   end Syntax_Class;

   ----------
   -- Text --
   ----------

   overriding
   function Text (Item : Syntax_Tree_Record) return String is
   begin
      if Item.Has_Token then
         return Aquarius.Tokens.Get_Name (Item.Node.Frame, Item.Token);
      else
         return Aquarius.Names.To_String (Item.Node.Non_Terminal_Name);
      end if;
   end Text;

   -----------
   -- Token --
   -----------

   function Token (Item : Syntax_Tree_Record)
                  return Aquarius.Tokens.Token
   is
   begin
      return Item.Node.Token;
   end Token;

   --------------------
   -- Vertical_Align --
   --------------------

   procedure Vertical_Align (Item : in out Syntax_Tree_Record;
                             Top  : Syntax_Tree)
   is
   begin
      Item.Node.Vertical_Align := Top;
      Item.Pristine := False;
   end Vertical_Align;

   ------------------------
   -- Vertically_Aligned --
   ------------------------

   function Vertically_Aligned
     (Item : Syntax_Tree_Record)
     return Boolean
   is
   begin
      return Item.Node.Vertical_Align /= null;
   end Vertically_Aligned;

end Aquarius.Syntax;
