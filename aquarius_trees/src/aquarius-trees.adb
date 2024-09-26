with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Fixed.Equal_Case_Insensitive;
with Ada.Text_IO;

package body Aquarius.Trees is

   Current_Node_Id : Natural := 0;

   package Tree_Stack is
      new Ada.Containers.Doubly_Linked_Lists (Tree);

   type Internal_Declaration_Tree_Type is
     new Root_Tree_Type with null record;

   overriding
   function Name (Item : Internal_Declaration_Tree_Type)
                 return String;

   overriding
   function Image (Item : Internal_Declaration_Tree_Type)
                 return String;

   Local_Internal_Declaration : Tree := null;

   ---------
   -- "=" --
   ---------

   overriding
   function "=" (Left : Root_Tree_Type;
                 Right : Root_Tree_Type)
                 return Boolean
   is
   begin
      return Left.Identity = Right.Identity;
   end "=";

   ---------------
   -- Add_Child --
   ---------------

   procedure Add_Child (Item      : not null access Root_Tree_Type;
                        New_Child : not null access Root_Tree_Type'Class)
   is
      Last : Tree;
   begin
      if Item.Child_Count > 0 then
         New_Child.Left := Item.Children.Last_Element;
         Last := Item.Children.Last_Element;
         Last.Right := Tree (New_Child);
      else
         New_Child.Left := null;
      end if;
      New_Child.Right := null;
      New_Child.Parent := Tree (Item);
      Item.Children.Append (Tree (New_Child));
   end Add_Child;

   ----------------------
   -- Add_Left_Sibling --
   ----------------------

   procedure Add_Left_Sibling
     (To_Tree     : not null access Root_Tree_Type;
      New_Sibling : not null access Root_Tree_Type'Class)
   is
   begin
      if not To_Tree.Keep_Siblings or else
        not To_Tree.Keep_Parent
      then
         raise Tree_Error with
           "cannot add sibling to tree node if " &
           "siblings or parents are not kept";
      end if;

      New_Sibling.Parent := To_Tree.Parent;
      New_Sibling.Right  := Tree (To_Tree);
      New_Sibling.Left   := To_Tree.Left;
      if New_Sibling.Left /= null then
         New_Sibling.Left.Right := Tree (New_Sibling);
      end if;
      To_Tree.Left := Tree (New_Sibling);
      declare
         use Tree_Vectors;
         Position : Cursor := New_Sibling.Parent.Children.First;
      begin
         while Has_Element (Position) loop
            if Element (Position) = Tree (To_Tree) then
               New_Sibling.Parent.Children.Insert (Position,
                                                   Tree (New_Sibling));
               exit;
            end if;
            Next (Position);
         end loop;
      end;
   exception
      when others =>
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            "caught exception while adding left sibling");
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            "  existing tree: " & Root_Tree_Type'Class (To_Tree.all).Image);
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            "   new sibling: " & New_Sibling.Image);
         raise;
   end Add_Left_Sibling;

   -----------------------
   -- Add_Right_Sibling --
   -----------------------

   procedure Add_Right_Sibling
     (To_Tree     : not null access Root_Tree_Type;
      New_Sibling : not null access Root_Tree_Type'Class)
   is
   begin
      if not To_Tree.Keep_Siblings or else
        not To_Tree.Keep_Parent
      then
         raise Tree_Error with
           "cannot add sibling to tree node if " &
           "siblings or parents are not kept";
      end if;

      New_Sibling.Parent := To_Tree.Parent;
      New_Sibling.Left  := Tree (To_Tree);
      New_Sibling.Right   := To_Tree.Right;
      if New_Sibling.Right /= null then
         New_Sibling.Right.Left := Tree (New_Sibling);
      end if;
      To_Tree.Right := Tree (New_Sibling);
      if New_Sibling.Right = null then
         New_Sibling.Parent.Children.Append (Tree (New_Sibling));
      else
         declare
            use Tree_Vectors;
            Position : Cursor := New_Sibling.Parent.Children.First;
         begin
            while Has_Element (Position) loop
               if Element (Position) = Tree (To_Tree) then
                  Next (Position);
                  New_Sibling.Parent.Children.Insert (Position,
                                                      Tree (New_Sibling));
                  exit;
               end if;
               Next (Position);
            end loop;
         end;
      end if;
   end Add_Right_Sibling;

   --------------------
   -- All_Properties --
   --------------------

   function All_Properties (Top   : not null access Root_Tree_Type;
                            Prop  : Aquarius.Properties.Property_Type)
                           return Array_Of_Objects
   is

      Empty : Array_Of_Objects (1 .. 0);

      function Sibling_Properties (Start : access Root_Tree_Type'Class)
                                  return Array_Of_Objects;

      ------------------------
      -- Sibling_Properties --
      ------------------------

      function Sibling_Properties (Start : access Root_Tree_Type'Class)
                                  return Array_Of_Objects
      is
      begin
         if Start = null then
            return Empty;
         else
            return All_Properties (Start, Prop) &
              Sibling_Properties (Start.Right_Sibling);
         end if;
      end Sibling_Properties;

      Have_Top_Property : constant Boolean :=
        Top.Has_Property (Prop);
      Top_Property      : Aquarius_Object;
   begin
      if Have_Top_Property then
         Top_Property := Top.Property (Prop);
      end if;

      if Top.Has_Children then
         declare
            Result : constant Array_Of_Objects :=
              Sibling_Properties (Top.First_Child);
         begin
            if Have_Top_Property then
               return Top_Property & Result;
            else
               return Result;
            end if;
         end;
      else
         if Have_Top_Property then
            return [1 => Top_Property];
         else
            return Empty;
         end if;
      end if;
   end All_Properties;

   --------------------
   -- Attach_Message --
   --------------------

   overriding
   procedure Attach_Message (To    : in out Root_Tree_Type;
                             Item  : Aquarius.Messages.Message)
   is
   begin
      Aquarius.Messages.Add_Message (To.Messages, Item);
   end Attach_Message;

   ------------------------
   -- Breadth_First_Scan --
   ------------------------

   procedure Breadth_First_Scan
     (Top : Root_Tree_Type;
      Process : not null access
        procedure (Item : Tree))
   is
      Stack : Tree_Stack.List;
   begin
      for I in 1 .. Top.Child_Count loop
         Stack.Append (Top.Child (I));
      end loop;

      while not Stack.Is_Empty loop
         declare
            Item : constant Tree := Stack.First_Element;
         begin
            Stack.Delete_First;
            Process (Item);
            for I in 1 .. Item.Child_Count loop
               Stack.Append (Item.Child (I));
            end loop;
         end;
      end loop;

   end Breadth_First_Scan;

   --------------------------
   -- Breadth_First_Search --
   --------------------------

   function Breadth_First_Search
     (Top         : Root_Tree_Type;
      Match       : not null access
        function (Item : Root_Tree_Type'Class)
      return Boolean;
      Match_Index : Positive := 1;
      Stop_At_Named : Boolean := False)
     return Tree
   is
      Stack : Tree_Stack.List;
      Count : Natural := 0;
   begin
      for I in 1 .. Top.Child_Count loop
         Stack.Append (Top.Child (I));
      end loop;

      while not Stack.Is_Empty loop
         declare
            Item : constant Tree := Stack.First_Element;
         begin
            Stack.Delete_First;
            if Match (Item.all) then
               Count := Count + 1;
               if Count = Match_Index then
                  return Item;
               end if;
            elsif not Stop_At_Named
              or else Item.Name = ""
            then
               for I in 1 .. Item.Child_Count loop
                  Stack.Append (Item.Child (I));
               end loop;
            end if;
         end;
      end loop;
      return null;
   end Breadth_First_Search;

   --------------------------
   -- Breadth_First_Search --
   --------------------------

   function Breadth_First_Search
     (Top         : Root_Tree_Type;
      Child_Name  : String;
      Match_Index   : Positive := 1;
      Stop_At_Named : Boolean := False)
     return Tree
   is
      function Match (Item : Root_Tree_Type'Class) return Boolean;

      -----------
      -- Match --
      -----------

      function Match (Item : Root_Tree_Type'Class) return Boolean is
      begin
         return Ada.Strings.Fixed.Equal_Case_Insensitive
           (Item.Name, Child_Name);
      end Match;

   begin
      return Breadth_First_Search
        (Top, Match'Access, Match_Index, Stop_At_Named);
   end Breadth_First_Search;

   -----------
   -- Child --
   -----------

   function Child (Item  : Root_Tree_Type;
                   Index : Positive)
                  return Tree
   is
   begin
      return Item.Children.Element (Index);
   end Child;

   -----------
   -- Child --
   -----------

   function Child
     (Top  : not null access Root_Tree_Type'Class;
      Path : String)
      return Tree
   is
      Cs : constant Array_Of_Trees := Children (Top, Path);
   begin
      return Cs (Cs'First);
   end Child;

   -----------------
   -- Child_Count --
   -----------------

   function Child_Count (Item : Root_Tree_Type) return Natural is
   begin
      return Item.Children.Last_Index;
   end Child_Count;

   --------------
   -- Children --
   --------------

   function Children
     (Top  : not null access Root_Tree_Type'Class;
      Path : String)
      return Array_Of_Trees
   is
      use Ada.Strings.Fixed;
      Start   : Natural := Path'First;
      Current : Tree_Vectors.Vector;
      Empty   : Array_Of_Trees (1 .. 0);

      function Find_Named_Children
        (T    : Tree;
         Name : String)
         return Array_Of_Trees;

      -------------------------
      -- Find_Named_Children --
      -------------------------

      function Find_Named_Children
        (T    : Tree;
         Name : String)
         return Array_Of_Trees
      is

         function Find_Children
           (T     : Tree;
            Index : Positive)
         return Array_Of_Trees;

         -------------------
         -- Find_Children --
         -------------------

         function Find_Children
           (T     : Tree;
            Index : Positive)
         return Array_Of_Trees
         is
         begin
            if Index > T.Child_Count then
               return Empty;
            else
               return Find_Named_Children (T.Child (Index), Name)
                 & Find_Children (T, Index + 1);
            end if;
         end Find_Children;

         Tree_Name : constant String := T.Name;

      begin
         if Tree_Name = "" then
            return Find_Children (T, 1);
         elsif Tree_Name = Name then
            return [T];
         else
            return Empty;
         end if;
      end Find_Named_Children;

   begin

      for I in 1 .. Top.Child_Count loop
         Current.Append (Top.Child (I));
      end loop;

      while Start > 0 loop
         declare
            Next         : constant Natural :=
                             Index (Path, "/", Start);
            Element      : constant String :=
                             (if Next = 0
                              then Path (Start .. Path'Last)
                              else Path (Start .. Next - 1));
            Next_Current : Tree_Vectors.Vector;
         begin
            Start := (if Next = 0 then 0 else Next + 1);

            if Element'Length > 0 then
               for T of Current loop
                  declare
                     Cs : constant Array_Of_Trees :=
                            Find_Named_Children (T, Element);
                  begin
                     for Match of Cs loop
                        if Next = 0 then
                           Next_Current.Append (Match);
                        else
                           for I in 1 .. Match.Child_Count loop
                              if not Next_Current.Contains
                                (Match.Child (I))
                              then
                                 Next_Current.Append (Match.Child (I));
                              end if;
                           end loop;
                        end if;
                     end loop;
                  end;
               end loop;
               Current := Next_Current;
            end if;

            Start := (if Next = 0 then 0 else Next + 1);

            exit when Current.Is_Empty;

         end;
      end loop;

      return Result : Array_Of_Trees (1 .. Current.Last_Index) do
         for I in Result'Range loop
            Result (I) := Current.Element (I);
         end loop;
      end return;

   end Children;

   --------------------
   -- Clear_Messages --
   --------------------

   overriding
   procedure Clear_Messages (Item : in out Root_Tree_Type) is
   begin
      Aquarius.Messages.Clear_Message_List (Item.Messages);
      for I in 1 .. Item.Child_Count loop
         Item.Child (I).Clear_Messages;
      end loop;
   end Clear_Messages;

   --------------------
   -- Clear_Property --
   --------------------

   procedure Clear_Property (Item  : not null access Root_Tree_Type;
                             Prop  : Aquarius.Properties.Property_Type)
   is
      It : Tree := Tree (Item);
   begin
      while It /= null loop
         if Aquarius.Properties.Exists (It.Properties, Prop) then
            Aquarius.Properties.Clear (It.Properties, Prop);
            exit;
         elsif not Aquarius.Properties.Is_Inherited (Prop) or else
           not It.Keep_Parent or else
           It.Parent = null
         then
            raise Constraint_Error with
              Tree (Item).Name & ": attempt to clear property " &
              Aquarius.Properties.Get_Name (Prop) &
              " but it is not set";
         else
            It := It.Parent;
         end if;
      end loop;
   end Clear_Property;

   ---------------------
   -- Common_Ancestor --
   ---------------------

   procedure Common_Ancestor
     (Left, Right    : not null access Root_Tree_Type'Class;
      Ancestor       : out Tree;
      Left_Ancestor  : out Tree;
      Right_Ancestor : out Tree)
   is
      Ancestors : Tree_Vectors.Vector;
      It : Tree;
   begin
      if not Left.Keep_Parent then
         raise Constraint_Error with
           "Common_Ancestor called on tree that does not keep parents";
      end if;

      if Left = Right then
         Ancestor := Tree (Left);
         Left_Ancestor := null;
         Right_Ancestor := null;
         return;
      end if;

      It := Tree (Left);
      while It /= null loop
         if It = Tree (Right) then
            Ancestor := Tree (Right);
            Right_Ancestor := null;
            --  Ancestors.Length must be > 0, otherwise Left = Right
            --  and we would have already returned
            pragma Assert (Natural (Ancestors.Length) > 0);
            Left_Ancestor := Ancestors.Element (Positive (Ancestors.Length));
            return;
         else
            Ancestors.Append (It);
            It := It.Parent;
         end if;
      end loop;

      It := Tree (Right);
      while It.Parent /= null loop
         for I in 1 .. Ancestors.Last_Index loop
            if Ancestors.Element (I) = It.Parent then
               Ancestor := Ancestors.Element (I);
               if I > 1 then
                  Left_Ancestor := Ancestors.Element (I - 1);
               else
                  Left_Ancestor := null;
               end if;
               Right_Ancestor := It;
               return;
            end if;
         end loop;
         It := It.Parent;
      end loop;

      --  at this point, left and right cannot be on the same tree
      Left_Ancestor := null;
      Right_Ancestor := null;
      Ancestor := null;
   end Common_Ancestor;

   -----------------
   -- First_Child --
   -----------------

   function First_Child (Item : Root_Tree_Type) return Tree is
   begin
      if not Item.Has_Children then
         raise Tree_Error with
           "First_Child called on tree with no children: " &
         Root_Tree_Type'Class (Item).Image;
      end if;
      pragma Assert (Item.Has_Children);
      return Item.Child (1);
   end First_Child;

   ----------------
   -- First_Leaf --
   ----------------

   function First_Leaf (Item : not null access Root_Tree_Type) return Tree is
      It : Tree := Tree (Item);
   begin
      while It.Child_Count > 0 loop
         It := It.First_Child;
      end loop;
      return It;
   end First_Leaf;

   ---------------------------
   -- Get_Matching_Children --
   ---------------------------

   function Get_Matching_Children
     (Top   : Root_Tree_Type;
      Match : not null access function (Item : Tree) return Boolean)
     return Array_Of_Trees
   is
      use type Ada.Containers.Count_Type;
      Acc   : Tree_Stack.List;
      Stack : Tree_Stack.List;
   begin
      for I in 1 .. Top.Child_Count loop
         Stack.Append (Top.Child (I));
      end loop;

      while not Stack.Is_Empty loop
         declare
            Item : constant Tree := Stack.First_Element;
         begin
            Stack.Delete_First;
            if Match (Item) then
               Acc.Append (Item);
            else
               for I in 1 .. Item.Child_Count loop
                  Stack.Append (Item.Child (I));
               end loop;
            end if;
         end;
      end loop;
      if Acc.Length = 0 then
         declare
            Result : Array_Of_Trees (1 .. 0);
         begin
            return Result;
         end;
      else
         declare
            Position : Tree_Stack.Cursor := Acc.First;
            Result   : Array_Of_Trees (1 .. Positive (Acc.Length));
         begin
            for I in Result'Range loop
               Result (I) := Tree_Stack.Element (Position);
               Tree_Stack.Next (Position);
            end loop;
            return Result;
         end;
      end if;
   end Get_Matching_Children;

   ---------------------------
   -- Get_Matching_Children --
   ---------------------------

   function Get_Matching_Children
     (Top   : Root_Tree_Type;
      Name  : String)
     return Array_Of_Trees
   is
      function Match (Item : Tree) return Boolean;

      -----------
      -- Match --
      -----------

      function Match (Item : Tree) return Boolean is
      begin
         return Item.Name = Name;
      end Match;

   begin
      return Top.Get_Matching_Children (Match'Access);
   end Get_Matching_Children;

   -----------------------
   -- Get_Message_Level --
   -----------------------

   function Get_Message_Level (Item : Root_Tree_Type)
                              return Aquarius.Messages.Message_Level
   is
   begin
      if Item.Has_Messages then
         return Aquarius.Messages.Highest_Level (Item.Messages);
      else
         return Aquarius.Messages.No_Message;
      end if;
   end Get_Message_Level;

   ------------------
   -- Get_Messages --
   ------------------

   overriding
   procedure Get_Messages (From  : Root_Tree_Type;
                           List  : in out Aquarius.Messages.Message_List)
   is
   begin
      Aquarius.Messages.Copy_Message_List (From.Messages, List);
      for I in 1 .. From.Child_Count loop
         From.Child (I).Get_Messages (List);
      end loop;
   end Get_Messages;

   ---------------------------
   -- Get_Named_Children --
   ---------------------------

   function Get_Named_Children
     (Top   : Root_Tree_Type)
     return Array_Of_Trees
   is
      Acc   : Tree_Vectors.Vector;

      procedure GNC (Current : Tree);

      ---------
      -- GNC --
      ---------

      procedure GNC (Current : Tree) is
      begin
         if Current.Name /= "" then
            Acc.Append (Current);
         else
            for I in 1 .. Current.Child_Count loop
               GNC (Current.Child (I));
            end loop;
         end if;
      end GNC;

   begin
      for I in 1 .. Top.Child_Count loop
         GNC (Top.Child (I));
      end loop;

      declare
         Result : Array_Of_Trees (1 .. Acc.Last_Index);
      begin
         for I in Result'Range loop
            Result (I) := Acc.Element (I);
         end loop;
         return Result;
      end;

   end Get_Named_Children;

   ------------------
   -- Has_Children --
   ------------------

   function Has_Children (Item : Root_Tree_Type) return Boolean is
   begin
      return Item.Child_Count > 0;
   end Has_Children;

   ------------------
   -- Has_Messages --
   ------------------

   function Has_Messages (Item : Root_Tree_Type)
                         return Boolean
   is
   begin
      return Aquarius.Messages.Message_Count (Item.Messages) > 0;
   end Has_Messages;

   --------------
   -- Has_Name --
   --------------

   function Has_Name (Item : Tree) return Boolean is
   begin
      return Item.Name /= "";
   end Has_Name;

   ------------------------
   -- Has_Named_Property --
   ------------------------

   function Has_Named_Property (Item : Root_Tree_Type;
                                Name : String)
                               return Boolean
   is
      pragma Unreferenced (Item);
      pragma Unreferenced (Name);
   begin
      return False;
   end Has_Named_Property;

   function Has_Named_Property
     (Item : Root_Tree_Type;
      Name : Aquarius.Names.Aquarius_Name)
      return Boolean
   is
      pragma Unreferenced (Item);
      pragma Unreferenced (Name);
   begin
      return False;
   end Has_Named_Property;

   ------------------
   -- Has_Property --
   ------------------

   function Has_Property (Item : Root_Tree_Type;
                          Prop : Aquarius.Properties.Property_Type)
                         return Boolean
   is
      It : access constant Root_Tree_Type'Class := Item'Unchecked_Access;
   begin

      while It /= null loop
         if Aquarius.Properties.Exists (It.Properties, Prop) then
            return True;
         elsif not Aquarius.Properties.Is_Inherited (Prop) or else
           not It.Keep_Parent
         then
            exit;
         else
            if It.Parent = null then
               It := It.Foster_Parent;
            else
               It := It.Parent;
            end if;
         end if;
      end loop;

      return False;
   end Has_Property;

   -----------
   -- Image --
   -----------

   overriding
   function Image (Item : Internal_Declaration_Tree_Type)
                  return String
   is
      pragma Unreferenced (Item);
   begin
      return "Internal declaration";
   end Image;

   ---------------------
   -- Initialise_Tree --
   ---------------------

   procedure Initialise_Tree
     (Item          : in out Root_Tree_Type;
      Source        : Aquarius.Sources.Source_Reference;
      Location      : Aquarius.Locations.Location_Interface'Class;
      Keep_Parent   : Boolean;
      Keep_Siblings : Boolean;
      Temporary     : Boolean := False)
   is
   begin
      Current_Node_Id := Current_Node_Id + 1;
      Item.Identity  := Current_Node_Id;
      Item.Source    := Source;
      Item.Update_Location (Location);
      Item.Temporary := Temporary;
      Item.Keep_Parent := Keep_Parent;
      Item.Keep_Siblings := Keep_Siblings;
      Aquarius.Messages.Create_Message_List (Item.Messages, True);
   end Initialise_Tree;

   --------------------------
   -- Internal_Declaration --
   --------------------------

   function Internal_Declaration return Tree is
   begin
      if Local_Internal_Declaration = null then
         Local_Internal_Declaration := new Internal_Declaration_Tree_Type;
      end if;
      return Local_Internal_Declaration;
   end Internal_Declaration;

   -------------
   -- Is_Null --
   -------------

   function Is_Null (T : Tree) return Boolean is
   begin
      return T = null;
   end Is_Null;

   ----------------
   -- Last_Child --
   ----------------

   function Last_Child (Item : Root_Tree_Type) return Tree is
   begin
      if not Item.Has_Children then
         raise Tree_Error with
           "First_Child called on tree with no children: " &
           Root_Tree_Type'Class (Item).Image;
      end if;
      pragma Assert (Item.Has_Children);
      return Item.Child (Item.Child_Count);
   end Last_Child;

   ---------------
   -- Last_Leaf --
   ---------------

   function Last_Leaf (Item : not null access Root_Tree_Type) return Tree is
      It : Tree := Tree (Item);
   begin
      while It.Child_Count > 0 loop
         It := It.Child (It.Child_Count);
      end loop;
      return It;
   end Last_Leaf;

   ----------
   -- Leaf --
   ----------

   function Leaf (Item      : not null access Root_Tree_Type;
                  Leaf_Name : String)
                 return Tree
   is
      Child : constant Tree := Item.Breadth_First_Search (Leaf_Name);
   begin
      if Child /= null then
         return Child.First_Leaf;
      else
         return null;
      end if;
   exception
      when Constraint_Error =>
         raise Constraint_Error with
           "Leaf: " & Item.Text & " has no child named '" & Leaf_Name & "'";
   end Leaf;

   ------------------
   -- Left_Sibling --
   ------------------

   function Left_Sibling (Item : Root_Tree_Type'Class) return Tree is
   begin
      pragma Assert (Item.Keep_Siblings);
      return Item.Left;
   end Left_Sibling;

   -------------------
   -- Location_Name --
   -------------------

   overriding function Location_Name
     (This      : Root_Tree_Type;
      Show_Path : Boolean := False)
      return String
   is
   begin
      if Show_Path then
         return This.Source.Full_Name;
      else
         return This.Source.Short_Name;
      end if;
   end Location_Name;

   ----------
   -- Name --
   ----------

   overriding
   function Name (Item : Internal_Declaration_Tree_Type)
                 return String
   is
      pragma Unreferenced (Item);
   begin
      return "Internal declaration";
   end Name;

   ---------------
   -- Next_Leaf --
   ---------------

   function Next_Leaf (Current : not null access Root_Tree_Type'Class)
                      return Tree
   is
      It : Tree := Tree (Current);
   begin
      while It /= null and then It.Right_Sibling = null loop
         It := It.Parent;
      end loop;
      if It /= null then
         It := It.Right_Sibling.First_Leaf;
      end if;
      return It;
   end Next_Leaf;

   ------------
   -- Parent --
   ------------

   function Parent (Item : Root_Tree_Type'Class) return Tree is
   begin
      pragma Assert (Item.Keep_Parent);
      return Item.Parent;
   end Parent;

   -------------------
   -- Previous_Leaf --
   -------------------

   function Previous_Leaf (Current : not null access Root_Tree_Type'Class)
                      return Tree
   is
      It : Tree := Tree (Current);
   begin
      while It /= null and then It.Left_Sibling = null loop
         It := It.Parent;
      end loop;
      if It /= null then
         It := It.Left_Sibling.Last_Leaf;
      end if;
      return It;
   end Previous_Leaf;

   --------------
   -- Property --
   --------------

   function Property (Item : Root_Tree_Type;
                      Prop : Aquarius.Properties.Property_Type)
                     return Aquarius_Object
   is
      It : access constant Root_Tree_Type'Class := Item'Access;
   begin
      while It /= null loop
         if Aquarius.Properties.Exists (It.Properties, Prop) then
            return
              Aquarius_Object
              (Aquarius.Properties.Get (It.Properties, Prop));
         elsif not Aquarius.Properties.Is_Inherited (Prop) or else
           not It.Keep_Parent
         then
            exit;
         else
            if It.Parent = null then
               It := It.Foster_Parent;
            else
               It := It.Parent;
            end if;
         end if;
      end loop;

      raise Aquarius.Properties.Property_Error with
        Item.Show_Location
        & ": property " & Properties.Get_Name (Prop) & " not found in tree " &
        Root_Tree_Type'Class (Item).Text;

   end Property;

   ------------------
   -- Remove_Child --
   ------------------

   procedure Remove_Child (Item      : not null access Root_Tree_Type;
                           Index     :        Positive)
   is
      Child : constant Tree := Item.Child (Index);
   begin
      if not Tree (Item).Keep_Siblings then
         Child.Parent := null;
      else
         if Item.Child_Count > Index then
            Child.Right.Left := Child.Left;
         end if;
         if Index > 1 then
            Child.Left.Right := Child.Right;
         end if;
         Child.Parent := null;
         Child.Right  := null;
         Child.Left   := null;
      end if;

      Item.Children.Delete (Index);
   end Remove_Child;

   ------------------
   -- Remove_Child --
   ------------------

   procedure Remove_Child (Item      : not null access Root_Tree_Type;
                           Child     : not null access Root_Tree_Type)
   is
   begin
      for I in 1 .. Item.Child_Count loop
         if Item.Child (I) = Child then
            Item.Remove_Child (I);
            return;
         end if;
      end loop;
      raise Constraint_Error with "attempt to remove non-existent child";
   end Remove_Child;

   -------------------
   -- Replace_Child --
   -------------------

   procedure Replace_Child (Item      : not null access Root_Tree_Type;
                            Old_Child : not null access Root_Tree_Type;
                            New_Child : not null access Root_Tree_Type)
   is
   begin
      Old_Child.Add_Left_Sibling (New_Child);
      Item.Remove_Child (Old_Child);
   end Replace_Child;

   -------------------
   -- Right_Sibling --
   -------------------

   function Right_Sibling (Item : Root_Tree_Type'Class) return Tree is
   begin
      pragma Assert (Item.Keep_Siblings);
      return Item.Right;
   end Right_Sibling;

   ---------------
   -- Same_Node --
   ---------------

   function Same_Node (Item   : Root_Tree_Type;
                       Other  : Root_Tree_Type'Class)
                       return Boolean
   is
   begin
      return Item.Identity = Other.Identity;
   end Same_Node;

   ------------
   -- Search --
   ------------

   function Search
     (Top         : not null access Root_Tree_Type'Class;
      Match       : not null access function (Item : Tree) return Boolean)
     return Array_Of_Trees
   is

      Empty_Result : Array_Of_Trees (1 .. 0);

      function S (T : Tree) return Array_Of_Trees;

      function Rest (T : Tree) return Array_Of_Trees;

      function Rest (T : Tree) return Array_Of_Trees is
      begin
         if T.Has_Children then
            return S (T.First_Child) & S (T.Right_Sibling);
         else
            return S (T.Right_Sibling);
         end if;
      end Rest;

      function S (T : Tree) return Array_Of_Trees
      is
      begin
         if T = null then
            return Empty_Result;
         else
            if Match (T) then
               return T & Rest (T);
            else
               return Rest (T);
            end if;
         end if;
      end S;

   begin
      return S (Tree (Top));
   end Search;

   -------------------
   -- Search_Leaves --
   -------------------

   function Search_Leaves (Start     : not null access Root_Tree_Type;
                           Direction : Scan_Direction;
                           Match     : not null access
                             function (Leaf : not null access constant
                                         Root_Tree_Type'Class)
                             return Boolean)
                          return Tree
   is
      It : Tree := Tree (Start);
   begin
      while It /= null and then not Match (It) loop
         case Direction is
            when Forewards =>
               It := It.Next_Leaf;
            when Backwards =>
               It := It.Previous_Leaf;
         end case;
      end loop;
      return It;
   end Search_Leaves;

   -----------------------
   -- Set_Foster_Parent --
   -----------------------

   procedure Set_Foster_Parent (Item    : not null access Root_Tree_Type;
                                Parent  : not null access Root_Tree_Type)
   is
   begin
      Item.Foster_Parent := Tree (Parent);
   end Set_Foster_Parent;

   ------------------
   -- Set_Property --
   ------------------

   procedure Set_Property (Item  : in out Root_Tree_Type;
                           Prop  : Aquarius.Properties.Property_Type;
                           Value : not null access Root_Aquarius_Object'Class)
   is
   begin
      Aquarius.Properties.Set (Item.Properties, Prop, Value);
   end Set_Property;

   ------------------
   -- Set_Property --
   ------------------

   procedure Set_Property (Item  : in out Root_Tree_Type;
                           Prop  : Aquarius.Properties.Property_Type)
   is
   begin
      Aquarius.Properties.Set (Item.Properties, Prop);
   end Set_Property;

   -------------------------
   -- Show_Set_Properties --
   -------------------------

   function Show_Set_Properties (Item : not null access Root_Tree_Type'Class)
                                return String
   is
   begin
      return Aquarius.Properties.Show_Set_Properties (Item.Properties);
   end Show_Set_Properties;

   -------------------
   -- Standard_Text --
   -------------------

   function Standard_Text (Item : Root_Tree_Type) return String is
   begin
      return Root_Tree_Type'Class (Item).Text;
   end Standard_Text;

   ---------------
   -- Temporary --
   ---------------

   function Temporary (Item : Root_Tree_Type) return Boolean is
   begin
      return Item.Temporary;
   end Temporary;

   ----------
   -- Text --
   ----------

   function Text (Item : Root_Tree_Type) return String is
   begin
      return Root_Tree_Type'Class (Item).Name;
   end Text;

   ---------------------
   -- Update_Location --
   ---------------------

   overriding procedure Update_Location
     (This : in out Root_Tree_Type;
      From : Aquarius.Locations.Location_Interface'Class)
   is
   begin
      This.Offset := From.Offset;
      This.Line := From.Line;
      This.Column := From.Column;
   end Update_Location;

end Aquarius.Trees;
