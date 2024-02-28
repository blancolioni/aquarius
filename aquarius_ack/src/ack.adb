package body Ack is

   Next_Sequence_Number : Natural := 0;
   String_Label_Index   : Natural := 0;

   ------------------
   -- Add_Implicit --
   ------------------

   procedure Add_Implicit
     (Table_Entity    : in out Root_Entity_Type;
      Implicit_Entity : not null access Root_Entity_Type'Class)
   is
   begin
      Table_Entity.Children.Implicits.Append
        (Entity_Type (Implicit_Entity));
   end Add_Implicit;

   ------------
   -- Append --
   ------------

   procedure Append (List : List_Id;
                     Node : Node_Id)
   is
   begin
      List_Table (List).List.Append (Node);
   end Append;

   --------------------
   -- Clear_Attached --
   --------------------

   procedure Clear_Attached
     (Entity : in out Root_Entity_Type'Class)
   is
   begin
      Entity.Attached := False;
   end Clear_Attached;

   -----------------
   -- Conforms_To --
   -----------------

   function Conforms_To
     (Class : not null access constant Root_Entity_Type;
      Other : not null access constant Root_Entity_Type'Class)
      return Boolean
   is
   begin
      return Constant_Entity_Type (Class) = Constant_Entity_Type (Other);
   end Conforms_To;

   --------------
   -- Contains --
   --------------

   function Contains
     (Table_Entity : Root_Entity_Type;
      Name         : String;
      Recursive    : Boolean := True)
      return Boolean
   is
   begin
      if Table_Entity.Children /= null then
         for Item of reverse Table_Entity.Children.Implicits loop
            if Item.Standard_Name = Name then
               return True;
            end if;
         end loop;
      end if;

      if Table_Entity.Children /= null
        and then Table_Entity.Children.Map.Contains (Name)
      then
         return True;
      elsif Recursive and then Table_Entity.Parent_Environment /= null then
         return Table_Entity.Parent_Environment.Contains (Name, Recursive);
      else
         return False;
      end if;
   end Contains;

   -------------------
   -- Contains_Name --
   -------------------

   function Contains_Name
     (List : List_Id;
      Name : Name_Id)
      return Boolean
   is
   begin
      for Item of List_Table (List).List loop
         if Get_Name (Item) = Name then
            return True;
         end if;
      end loop;
      return False;
   end Contains_Name;

   ----------
   -- Copy --
   ----------

   function Copy (Node : Node_Id) return Node_Id is
   begin
      if Node not in Real_Node_Id then
         return Node;
      else
         declare
            New_Record : Node_Record :=
                           Node_Table.Element (Node);
         begin
            for Field of New_Record.Field loop
               Field := Copy (Field);
            end loop;

            New_Record.List := Copy (New_Record.List);
            New_Record.Entity := null;
            New_Record.Context := null;
            New_Record.Node_Type := null;
            New_Record.Error := E_No_Error;
            New_Record.Error_Entity := null;
            New_Record.Integer_Value := 0;
            New_Record.Label := 0;
            Node_Table.Append (New_Record);
            return Node_Table.Last_Index;
         end;
      end if;
   end Copy;

   ----------
   -- Copy --
   ----------

   function Copy (List : List_Id) return List_Id is
      New_List : List_Record;
      Old_List : constant List_Record :=  List_Table.Element (List);
   begin
      for Item of Old_List.List loop
         New_List.List.Append (Copy (Item));
      end loop;
      List_Table.Append (New_List);
      return List_Table.Last_Index;
   end Copy;

   ------------
   -- Create --
   ------------

   procedure Create
     (Entity             : in out Root_Entity_Type'Class;
      Name               : Name_Id;
      Node               : Node_Id;
      Table              : Boolean;
      Parent_Environment : access Root_Entity_Type'Class := null;
      Context            : access Root_Entity_Type'Class := null)
   is
   begin
      Entity.Name := +(To_Standard_String (Name));
      Entity.Source_Name := +(To_String (Name));
      Entity.Declaration_Node := Node;
      Entity.Value_Type := null;
      Entity.Parent_Environment := Entity_Type (Parent_Environment);
      Entity.Declaration_Context := Entity_Type (Context);
      if Table then
         Entity.Children := new Entity_Table_Record;
      end if;
      Entity.Created := True;
      Next_Sequence_Number := Next_Sequence_Number + 1;
      Entity.Sequence_Number := Next_Sequence_Number;
   end Create;

   ----------------------
   -- Depth_First_Scan --
   ----------------------

   procedure Depth_First_Scan
     (Top     : Node_Id;
      Process : not null access
        procedure (Node : Node_Id))
   is
      procedure Scan (Node : Node_Id);

      ----------
      -- Scan --
      ----------

      procedure Scan (Node : Node_Id) is
         Rec : Node_Record renames Node_Table (Node);
      begin
         Process (Node);
         for N of Rec.Field loop
            if N in Real_Node_Id then
               Scan (N);
            end if;
         end loop;

         if Rec.List /= No_List then
            for N of List_Table (Rec.List).List loop
               Scan (N);
            end loop;
         end if;
      end Scan;

   begin
      Scan (Top);
   end Depth_First_Scan;

   -----------
   -- Error --
   -----------

   procedure Error
     (Node    : Node_Id;
      Kind    : Error_Kind;
      Entity  : access constant Root_Entity_Type'Class := null;
      Context : access constant Root_Entity_Type'Class := null)
   is
   begin
      Node_Table (Node).Error := Kind;
      Node_Table (Node).Error_Entity := Constant_Entity_Type (Entity);
      Node_Table (Node).Error_Context := Constant_Entity_Type (Context);
   end Error;

   ------------------
   -- Find_Name_Id --
   ------------------

   function Find_Name_Id
     (Name              : String)
      return Name_Id
   is
   begin
      if Name_Map.Contains (Name) then
         return Name_Map.Element (Name);
      else
         return No_Name;
      end if;
   end Find_Name_Id;

   ---------
   -- Get --
   ---------

   function Get
     (Table_Entity : not null access constant Root_Entity_Type;
      Name         : String)
      return Entity_Type
   is
   begin
      if Table_Entity.Children /= null then
         for Item of reverse Table_Entity.Children.Implicits loop
            if Item.Standard_Name = Name then
               return Item;
            end if;
         end loop;
      end if;
      if Table_Entity.Children /= null
        and then Table_Entity.Children.Map.Contains (Name)
      then
         return Table_Entity.Children.Map.Element (Name);
      else
         return Table_Entity.Parent_Environment.Get (Name);
      end if;
   end Get;

   -----------------
   -- Get_Name_Id --
   -----------------

   function Get_Name_Id
     (Name              : String)
      return Name_Id
   is
   begin
      if Name = "" then
         return No_Name;
      elsif Name_Map.Contains (Name) then
         return Name_Map.Element (Name);
      else
         Name_Table.Append (Name);
         Name_Map.Insert (Name, Name_Table.Last_Index);
         return Name_Table.Last_Index;
      end if;
   end Get_Name_Id;

   -----------
   -- Image --
   -----------

   function Image (Offset : Word_Offset) return String is
      Hex : constant String := "0123456789ABCDEF";
      It  : Word_Offset := Offset * 4;
   begin
      return S : String (1 .. 4) do
         for Ch of reverse S loop
            Ch := Hex (Natural (It mod 16) + 1);
            It := It / 16;
         end loop;
      end return;
   end Image;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Table_Entity : Root_Entity_Type'Class;
      Entity       : not null access Root_Entity_Type'Class)
   is
   begin
      Table_Entity.Children.Map.Insert
        (-(Entity.Name), Entity_Type (Entity));
      Table_Entity.Children.List.Append (Entity_Type (Entity));
   end Insert;

   ------------------
   -- Instantiated --
   ------------------

   procedure Instantiated
     (Entity : in out Root_Entity_Type'Class)
   is
   begin
      Next_Sequence_Number := Next_Sequence_Number + 1;
      Entity.Sequence_Number := Next_Sequence_Number;
   end Instantiated;

   ------------------
   -- Make_Text_Id --
   ------------------

   function Make_Text_Id
     (Text : String)
      return Text_Id
   is
   begin
      Text_Table.Append (Text);
      return Text_Table.Last_Index;
   end Make_Text_Id;

   --------------
   -- New_List --
   --------------

   function New_List return List_Id is
   begin
      return List : constant List_Id := List_Table.Last_Index + 1 do
         List_Table.Append (List_Record'(others => <>));
      end return;
   end New_List;

   --------------
   -- New_Node --
   --------------

   function New_Node
     (Kind       : Node_Kind;
      From       : Aquarius.Programs.Program_Tree;
      Deferred   : Boolean     := False;
      Expanded   : Boolean     := False;
      Frozen     : Boolean     := False;
      Defining   : Boolean     := False;
      Once       : Boolean     := False;
      Detachable : Boolean     := False;
      Inherited  : Boolean     := False;
      Field_1    : Node_Id     := No_Node;
      Field_2    : Node_Id     := No_Node;
      Field_3    : Node_Id     := No_Node;
      Field_4    : Node_Id     := No_Node;
      Field_5    : Node_Id     := No_Node;
      Field_6    : Node_Id     := No_Node;
      List       : List_Id     := No_List;
      Name       : Name_Id     := No_Name;
      Text       : Text_Id := No_Text;
      Entity     : Entity_Type := null)
      return Node_Id
   is
   begin
      return Node : constant Node_Id := Node_Table.Last_Index + 1 do
         Node_Table.Append
           (Node_Record'
              (Kind => Kind, From => From, Deferred => Deferred,
               Expanded => Expanded, Frozen => Frozen,
               Defining => Defining, Single => False, Once => Once,
               Detachable      => Detachable, Inherited => Inherited,
               Implicit_Entity => False, Attached => False,
               Field    =>
                 [1 => Field_1,
                  2 => Field_2,
                  3 => Field_3,
                  4 => Field_4,
                  5 => Field_5,
                  6 => Field_6],
               List            => List,
               Name            => Name,
               Text            => Text,
               Integer_Value   => 0,
               Entity          => Entity,
               Context         => null,
               Node_Type       => null,
               Dest_Type       => null,
               Error_Entity    => null,
               Error_Context   => null,
               Warning         => False,
               Error           => E_No_Error,
               Label           => 0));
      end return;
   end New_Node;

   -----------------------
   -- Next_String_Label --
   -----------------------

   function Next_String_Label
     (Base_Name : String)
      return String
   is
      Head : String := Base_Name;
      S    : constant String := Natural'Image (String_Label_Index);
   begin
      for Ch of Head loop
         if Ch = '-' then
            Ch := '_';
         end if;
      end loop;
      String_Label_Index := String_Label_Index + 1;
      return Head & "_" & S (2 .. S'Last);
   end Next_String_Label;

   -----------------
   -- Push_Offset --
   -----------------

   procedure Push_Offset
     (Unit   : in out Tagatha.Code.Instance'Class;
      Offset : Word_Offset)
   is
   begin
      Unit.Push_Constant (Tagatha.Int_32 (Offset * 4));
   end Push_Offset;

   --------------------------
   -- Push_String_Constant --
   --------------------------

   procedure Push_String_Constant
     (Code : in out Tagatha.Code.Instance'Class;
      Text : String)
   is
      Label : constant String := Next_String_Label ("string-constant");
   begin
      Code.Data_Label (Label);
      Code.String_Constant (Text);
      Code.Push_Name
        (Name    => Label,
         Extern  => False,
         Content => Tagatha.General_Content,
         Address => True);
   end Push_String_Constant;

   ---------------------
   -- Remove_Implicit --
   ---------------------

   procedure Remove_Implicit
     (Table_Entity    : in out Root_Entity_Type)
   is
   begin
      Table_Entity.Children.Implicits.Delete_Last;
   end Remove_Implicit;

   ----------
   -- Scan --
   ----------

   procedure Scan
     (List    : List_Id;
      Process : not null access
        procedure (Node : Node_Id))
   is
   begin
      if List /= No_List then
         for Node of List_Table.Element (List).List loop
            Process (Node);
         end loop;
      end if;
   end Scan;

   ------------------------------
   -- Scan_Entity_Declarations --
   ------------------------------

   procedure Scan_Entity_Declarations
     (Group   : Node_Id;
      Process : not null access
        procedure (Declaration_Node : Node_Id))
   is
      procedure Process_Group (Node : Node_Id);

      -------------------
      -- Process_Group --
      -------------------

      procedure Process_Group (Node : Node_Id) is

         procedure Process_Id (Id_Node : Node_Id);

         ----------------
         -- Process_Id --
         ----------------

         procedure Process_Id (Id_Node : Node_Id) is
         begin
            Process (Id_Node);
         end Process_Id;

      begin
         Scan (Node_Table (Node).List, Process_Id'Access);
      end Process_Group;

   begin
      Scan (Node_Table (Group).List, Process_Group'Access);
   end Scan_Entity_Declarations;

   -----------------
   -- Scan_Errors --
   -----------------

   procedure Scan_Errors
     (Top     : Node_Id;
      Process : not null access
        procedure (Node : Node_Id;
                   Error : Error_Kind;
                   Warning : Boolean))
   is
      procedure Process_Node (Node : Node_Id);

      ------------------
      -- Process_Node --
      ------------------

      procedure Process_Node (Node : Node_Id) is
      begin
         if Has_Error (Node) then
            Process (Node, Get_Error (Node),
                     Node_Table.Element (Node).Warning);
         end if;
      end Process_Node;

   begin
      Depth_First_Scan (Top, Process_Node'Access);
   end Scan_Errors;

   ------------------------------
   -- Set_Assertion_Monitoring --
   ------------------------------

   procedure Set_Assertion_Monitoring
     (Entity : in out Root_Entity_Type'Class;
      Level  : Assertion_Monitoring_Level)
   is
   begin
      Entity.Has_Monitoring_Level := True;
      Entity.Monitoring_Level := Level;
   end Set_Assertion_Monitoring;

   ------------------
   -- Set_Attached --
   ------------------

   procedure Set_Attached
     (Entity : in out Root_Entity_Type'Class)
   is
   begin
      Entity.Attached := True;
   end Set_Attached;

   -----------------
   -- Set_Context --
   -----------------

   procedure Set_Context
     (Node    : Real_Node_Id;
      Context : not null access constant Root_Entity_Type'Class)
   is
   begin
      Node_Table (Node).Context := Constant_Entity_Type (Context);
   end Set_Context;

   ---------------------------
   -- Set_Declaration_Count --
   ---------------------------

   procedure Set_Declaration_Count
     (N     : Node_Id;
      Count : Natural)
   is
   begin
      Node_Table (N).Integer_Value := Count;
   end Set_Declaration_Count;

   ----------------------------------
   -- Set_Default_Monitoring_Level --
   ----------------------------------

   procedure Set_Default_Monitoring_Level
     (Level : Assertion_Monitoring_Level)
   is
   begin
      Local_Default_Monitoring_Level := Level;
   end Set_Default_Monitoring_Level;

   --------------------------
   -- Set_Destination_Type --
   --------------------------

   procedure Set_Destination_Type
     (N       : Node_Id;
      To_Type : not null access constant Root_Entity_Type'Class)
   is
   begin
      Node_Table (N).Dest_Type :=
        Constant_Entity_Type (To_Type);
   end Set_Destination_Type;

   ----------------
   -- Set_Entity --
   ----------------

   procedure Set_Entity
     (Node : Real_Node_Id;
      Entity : not null access Root_Entity_Type'Class)
   is
   begin
      Node_Table (Node).Entity := Entity_Type (Entity);
   end Set_Entity;

   --------------------------------
   -- Set_Explicit_Creation_Call --
   --------------------------------

   procedure Set_Explicit_Creation_Call
     (N    : Node_Id;
      Name : Name_Id)
   is
      Call : constant Node_Id :=
               New_Node (Kind => N_Explicit_Creation_Call,
                         From => Get_Program (N),
                         Name => Name);
   begin
      Node_Table (N).Field (2) := Call;
   end Set_Explicit_Creation_Call;

   -------------------------
   -- Set_Implicit_Entity --
   -------------------------

   procedure Set_Implicit_Entity
     (N : Node_Id)
   is
   begin
      Node_Table (N).Implicit_Entity := True;
   end Set_Implicit_Entity;

   ---------------
   -- Set_Label --
   ---------------

   procedure Set_Label
     (Node  : Real_Node_Id;
      Value : Positive)
   is
   begin
      Node_Table (Node).Label := Value;
   end Set_Label;

   ---------------------
   -- Set_Stack_Check --
   ---------------------

   procedure Set_Stack_Check
     (Stack_Check : Boolean)
   is
   begin
      Stack_Check_Enabled := Stack_Check;
   end Set_Stack_Check;

   ---------------
   -- Set_Trace --
   ---------------

   procedure Set_Trace
     (Class_Analysis : Boolean)
   is
   begin
      Trace_Class_Analysis := Class_Analysis;
   end Set_Trace;

   --------------
   -- Set_Type --
   --------------

   procedure Set_Type
     (Node   : Real_Node_Id;
      Entity : not null access Root_Entity_Type'Class)
   is
   begin
      Node_Table (Node).Node_Type := Entity_Type (Entity);
   end Set_Type;

   ----------------------
   -- Set_Write_Tables --
   ----------------------

   procedure Set_Write_Tables
     (Write_Tables : Boolean)
   is
   begin
      Local_Write_Tables := Write_Tables;
   end Set_Write_Tables;

   --------------
   -- To_Array --
   --------------

   function To_Array
     (List : List_Id)
      return Array_Of_Nodes
   is
      Length : constant Natural :=
                 Natural (List_Table.Element (List).List.Length);
      Count  : Natural := 0;
   begin
      return Result : Array_Of_Nodes (1 .. Length) do
         for Node of List_Table.Element (List).List loop
            Count := Count + 1;
            Result (Count) := Node;
         end loop;
      end return;
   end To_Array;

   -------------
   -- Warning --
   -------------

   procedure Warning
     (Node    : Node_Id;
      Kind    : Error_Kind;
      Entity  : access constant Root_Entity_Type'Class := null;
      Context : access constant Root_Entity_Type'Class := null)
   is
   begin
      Node_Table (Node).Error := Kind;
      Node_Table (Node).Error_Entity := Constant_Entity_Type (Entity);
      Node_Table (Node).Error_Context := Constant_Entity_Type (Context);
      Node_Table (Node).Warning := True;
   end Warning;

end Ack;
