with Ada.Strings.Fixed;

with Ack.Classes;
with Ack.Generate;
with Ack.Types;

with Ack.Generate.Intrinsics;

with Ack.Semantic.Work;

package body Ack.Features is

   ------------------
   -- Add_Argument --
   ------------------

   procedure Add_Argument
     (Feature   : in out Feature_Entity_Record'Class;
      Name_Node : Node_Id;
      Arg_Type  : not null access Ack.Types.Type_Entity_Record'Class)
   is
   begin
      Feature.Arguments.Append
        (Ack.Variables.New_Argument_Entity
           (Get_Name (Name_Node), Name_Node, Arg_Type));
      Feature.Property := False;
   end Add_Argument;

   ------------------
   -- Add_Implicit --
   ------------------

   overriding procedure Add_Implicit
     (Feature         : in out Feature_Entity_Record;
      Implicit_Entity : not null access Root_Entity_Type'Class)
   is
   begin
      Root_Entity_Type (Feature).Add_Implicit (Implicit_Entity);
      if Ack.Variables.Is_Variable (Implicit_Entity) then
         Feature.Local_Count := Feature.Local_Count + 1;
         Ack.Variables.Variable_Entity_Record (Implicit_Entity.all)
           .Set_Offset (Feature.Local_Count);
      end if;
   end Add_Implicit;

   ---------------
   -- Add_Local --
   ---------------

   procedure Add_Local
     (Feature    : in out Feature_Entity_Record'Class;
      Name_Node  : Node_Id;
      Local_Type : not null access Ack.Types.Type_Entity_Record'Class)
   is
   begin
      Feature.Locals.Append
        (Ack.Variables.New_Local_Entity
           (Get_Name (Name_Node), Name_Node, Local_Type));
   end Add_Local;

   -----------------------
   -- Add_Postcondition --
   -----------------------

   procedure Add_Postcondition
     (Feature   : in out Feature_Entity_Record'Class;
      Tag       : Name_Id;
      Condition : Node_Id)
   is
   begin
      Feature.Postconditions.Append ((Tag, Condition));

   end Add_Postcondition;

   ----------------------
   -- Add_Precondition --
   ----------------------

   procedure Add_Precondition
     (Feature   : in out Feature_Entity_Record'Class;
      Tag       : Name_Id;
      Condition : Node_Id)
   is
   begin
      Feature.Preconditions.Append ((Tag, Condition));
   end Add_Precondition;

   ---------------
   -- Add_Shelf --
   ---------------

   overriding procedure Add_Shelf
     (Feature : in out Feature_Entity_Record;
      Name    : String)
   is
   begin
      if not Feature.Shelves.Contains (Name) then
         Feature.Local_Count := Feature.Local_Count + 1;
         Feature.Shelves.Insert (Name, Feature.Local_Count);
      end if;
   end Add_Shelf;

   ----------
   -- Bind --
   ----------

   overriding procedure Bind
     (Feature : not null access Feature_Entity_Record)
   is
      Next_Argument : Positive := 1;
      Next_Local    : Positive := 1;
      Current_Class : constant Ack.Classes.Class_Entity :=
                        Ack.Classes.Class_Entity
                          (if Feature.Effective_Class = null
                           then Feature.Definition_Class
                           else Feature.Effective_Class);
      Current       : constant Ack.Variables.Variable_Entity :=
                        Ack.Variables.New_Argument_Entity
                          (Name          => Get_Name_Id ("Current"),
                           Node          => Feature.Declaration_Node,
                           Argument_Type =>
                             Ack.Types.New_Class_Type
                               (Feature.Declaration_Node, Current_Class,
                                Detachable => False));
   begin
      Current.Set_Attached;
      if Current_Class.Frame_Words > 0 then
         Current.Set_Offset (Current_Class.Frame_Words);
         Feature.Insert (Current);
      end if;

      if Current_Class.Frame_Words <= 1 then
         Next_Argument := 2;
      else
         Next_Argument := Current_Class.Frame_Words + 1;
      end if;

      for Argument of Feature.Arguments loop
         Argument.Set_Offset (Next_Argument);
         Next_Argument := Next_Argument + 1;
         Feature.Insert (Argument);
      end loop;

      if Feature.Has_Result then
         declare
            Result : constant Ack.Variables.Variable_Entity :=
                        Ack.Variables.New_Local_Entity
                         (Get_Name_Id ("Result"),
                          Feature.Declaration_Node,
                          Feature.Value_Type);
         begin
            Result.Set_Offset (1);
            Feature.Insert (Result);
            Next_Local := 2;
         end;
      end if;

      for Local of Feature.Locals loop
         Local.Set_Offset (Next_Local);
         Next_Local := Next_Local + 1;
         Feature.Insert (Local);
      end loop;

      Feature.Local_Count := Next_Local - 1;
   end Bind;

   ------------------------
   -- Check_Precondition --
   ------------------------

   procedure Check_Precondition
     (Feature       : not null access constant Feature_Entity_Record'Class;
      Unit          : in out Tagatha.Code.Instance'Class;
      Success_Label : Tagatha.Code.Label;
      Fail_Label    : Tagatha.Code.Label)
   is
   begin
      if Feature.Redefined_Feature /= null then
         if False then
            declare
               Ancestor_Fail : constant Tagatha.Code.Label :=
                                 Unit.Next_Label;
            begin
               Feature.Redefined_Feature.Check_Precondition
                 (Unit          => Unit,
                  Success_Label => Success_Label,
                  Fail_Label    => Ancestor_Fail);
               Unit.Set_Label (Ancestor_Fail);
            end;
         end if;
      end if;

      for Clause of Feature.Preconditions loop
         declare
            Out_Label : constant Tagatha.Code.Label :=
                          Unit.Next_Label;
         begin
            Ack.Generate.Generate_Expression (Unit, Feature, Clause.Node);
            --  Unit.Operate (Tagatha.Op_Test);
            Unit.Branch (Tagatha.NZ, Out_Label);

            if Tagatha.Code.Has_Label (Fail_Label) then
               Unit.Branch (Fail_Label);
            else
               if Feature.Rescue_Node in Real_Node_Id then
                  Unit.Branch (Feature.Rescue_Label);
               else
                  declare
                     Message : constant String :=
                                 Get_Program (Clause.Node).Show_Location
                               & ": assertion failure in precondition of "
                                 & Feature.Declared_Name
                                 & (if Clause.Tag = No_Name then ""
                                    else ": " & To_String (Clause.Tag));
                  begin
                     Push_String_Constant (Unit, Message);
                     Ack.Generate.Generate_Exit (Unit);
                  end;
               end if;
            end if;

            Unit.Set_Label (Out_Label);
         end;
      end loop;
      Unit.Branch (Success_Label);
   end Check_Precondition;

   -------------------
   -- Class_Context --
   -------------------

   overriding function Class_Context
     (Feature : not null access constant Feature_Entity_Record)
      return Constant_Entity_Type
   is (if Feature.Redefined_Class /= null
       then Constant_Entity_Type (Feature.Redefined_Class)
       else Constant_Entity_Type (Feature.Definition_Class));

   -----------------
   -- Description --
   -----------------

   overriding function Description
     (Feature : Feature_Entity_Record)
      return String
   is
   begin
      return Root_Entity_Type (Feature).Description
        & " parent " & Feature.Declaration_Context.Full_Name;
   end Description;

   ---------------------
   -- Effective_Class --
   ---------------------

   function Effective_Class
     (Feature : Feature_Entity_Record'Class)
      return access constant Ack.Classes.Class_Entity_Record'Class
   is
   begin
      return Feature.Effective_Class;
   end Effective_Class;

   ---------------
   -- Full_Name --
   ---------------

   overriding function Full_Name
     (Feature : Feature_Entity_Record)
      return String
   is
      use Ada.Strings.Unbounded;
      Result : Unbounded_String;
   begin
      if not Feature.Arguments.Is_Empty then
         for Arg of Feature.Arguments loop
            if Result = Null_Unbounded_String then
               Result := Result & "(";
            else
               Result := Result & ",";
            end if;
            Result := Result
              & Entity_Type (Arg).Value_Type.Full_Name;
         end loop;
         Result := Result & ")";
      end if;

      Result := Feature.Qualified_Name & Result;

      if Feature.Value_Type /= null then
         Result := Result & ": " & Feature.Value_Type.Full_Name;
      end if;
      return To_String (Result);
   end Full_Name;

   -------------------------------
   -- Generate_Allocation_Value --
   -------------------------------

   procedure Generate_Allocation_Value
     (Feature : Feature_Entity_Record'Class;
      Unit    : in out Tagatha.Code.Instance'Class)
   is
   begin
      null;
   end Generate_Allocation_Value;

   -------------------------
   -- Generate_Definition --
   -------------------------

   procedure Generate_Routine
     (Feature : not null access Feature_Entity_Record'Class;
      Class   : not null access constant Ack.Classes.Class_Entity_Record'Class;
      Unit    : in out Tagatha.Code.Instance'Class)
   is
      --  pragma Unreferenced (Class);
      Arg_Count    : constant Natural :=
                       1 + Natural (Feature.Arguments.Length);
      Result_Count : constant Natural :=
                       (if Feature.Value_Type /= null
                        then 1
                        elsif Class.Expanded
                        then 1
                        else 0);
      Once_Flag_Label  : constant String :=
                           Feature.Link_Name & "$once_flag";
      Once_Value_Label : constant String :=
                           Feature.Link_Name & "$once_value";
      Exit_Label       : constant Tagatha.Code.Label := Unit.Next_Label;
      Rescue_Label     : constant Tagatha.Code.Label := Unit.Next_Label;
      Line             : constant Positive :=
                           Positive
                             (Get_Program
                                (Feature.Declaration_Node).Location_Line);
      Column           : constant Positive :=
                           Positive
                             (Get_Program
                                (Feature.Declaration_Node).Location_Column);
   begin

      Feature.Rescue_Label := Rescue_Label;

      Unit.Source_Location (Line, Column);

      if Feature.Property then
         --  Generate a property routine, in case it redefines
         --  an actual routine

         Unit.Begin_Routine (Feature.Link_Name);

         Unit.Push_Argument (1);
         Unit.Dereference (Tagatha.General_Content,
                           Tagatha.Int_32 (Feature.Property_Offset));
         Unit.Pop_Result (1);

         Unit.End_Routine;

      elsif Feature.External then
         if Feature.Intrinsic then

            --  Generate a wrapper that can be accessed
            --  via a virtual table entry

            Unit.Begin_Routine
              (Name           => Feature.Link_Name);

            declare
               procedure Push (Index : Positive);

               ----------
               -- Push --
               ----------

               procedure Push (Index : Positive) is
               begin
                  Unit.Push_Argument (Tagatha.Argument_Index (Index + 1));
               end Push;

            begin
               Unit.Push_Argument (1);
               Ack.Generate.Intrinsics.Generate_Intrinsic
                 (Unit, To_Standard_String (Feature.External_Label),
                  Arg_Count, Push'Access);
            end;

            if Result_Count > 0 then
               Unit.Pop_Result (1);
            end if;

            Unit.End_Routine;
         end if;

      elsif Feature.Routine then

         Unit.Begin_Routine
           (Name           => Feature.Link_Name);

         --  restore Current to our needs

--           if not Feature.Active_Class.Expanded
--             and then Feature.Definition_Class /= Class
--           then
--              Unit.Push_Argument (1);
--              Unit.Dereference;
--              Unit.Dereference;
--              Unit.Push_Argument (1);
--              Unit.Operate (Tagatha.Op_Sub);
--              Unit.Pop_Argument (1);
--           end if;

         if Feature.Rescue_Node in Real_Node_Id then
            --  Feature.Rescue_Label := Unit.Next_Label;
            --  Feature.Retry_Label := Unit.Next_Label;

            Unit.Exception_Handler
              (Start_Label   => Feature.Link_Name,
               End_Label     => Feature.Link_Name & "$end",
               Handler_Label => Feature.Link_Name & "$rescue");
            Feature.Retry_Label := Unit.Next_Label;
            Unit.Set_Label (Feature.Retry_Label);
         end if;

         if Feature.Declaration_Context.Monitor_Preconditions then
            declare
               Success_Label : constant Tagatha.Code.Label :=
                                 Unit.Next_Label;
            begin
               Feature.Check_Precondition
                 (Unit          => Unit,
                  Success_Label => Success_Label,
                  Fail_Label    => Tagatha.Code.No_Label);
               Unit.Set_Label (Success_Label);
            end;

         end if;

         if Feature.Once then
            declare
               Continue_Once : constant Tagatha.Code.Label :=
                                 Unit.Next_Label;
            begin
               Unit.Push_Name (Once_Flag_Label, Extern => True);
               Unit.Branch (Tagatha.Z, Continue_Once);
               if Feature.Has_Result then
                  Unit.Push_Name (Once_Value_Label, Extern => True);
                  Unit.Pop_Result (1);
               end if;
               Unit.Branch (Exit_Label);
               Unit.Set_Label (Continue_Once);
            end;
         end if;

         for I in 1 .. Feature.Local_Count loop
            Unit.Push_Constant (Tagatha.Int_32'(0));
            Unit.Pop_Local (Tagatha.Local_Index (I));
         end loop;

         if Feature.Monitor_Postconditions then
            for Old of Feature.Olds loop
               Ack.Generate.Generate_Expression (Unit, Feature, Old);
            end loop;
         end if;

         Ack.Generate.Generate_Compound
           (Unit, Feature, Compound (Feature.Routine_Node));

         if Feature.Monitor_Postconditions then
            for Clause of Feature.Postconditions loop
               declare
                  Out_Label : constant Tagatha.Code.Label :=
                                Unit.Next_Label;
               begin
                  Ack.Generate.Generate_Expression
                    (Unit, Feature, Clause.Node);
                  --  Unit.Operate (Tagatha.Op_Test);
                  Unit.Branch (Tagatha.NZ, Out_Label);
                  Push_String_Constant
                    (Unit,
                     Get_Program (Clause.Node).Show_Location
                     & ": assertion failure in postcondition of "
                     & Feature.Declared_Name
                     & (if Clause.Tag = No_Name then ""
                       else ": " & To_String (Clause.Tag)));

                  if Feature.Rescue_Node in Real_Node_Id then
                     Unit.Branch (Rescue_Label);
                  else
                     Unit.Call ("system.os.halt", 1, 0);
                  end if;
                  Unit.Set_Label (Out_Label);
               end;
            end loop;
         end if;

         if Feature.Has_Result then
            if Feature.Once then
               Unit.Push_Constant (Tagatha.Int_32'(1));
               Unit.Pop_Name (Once_Flag_Label, Extern => True);
               Unit.Push_Local (1);
               Unit.Pop_Name (Once_Value_Label, Extern => True);
            end if;

            Unit.Push_Local (1);
            Unit.Pop_Result (1);
         end if;

         Unit.Branch (Exit_Label);
         Unit.Set_Label (Rescue_Label);

         if Feature.Rescue_Node in Real_Node_Id then
            Ack.Generate.Generate_Compound
              (Unit, Feature, Compound (Feature.Rescue_Node),
               Retry_Target => Feature.Retry_Label);
         end if;

         Unit.Call ("system.os.halt", 0, 0);

         Unit.Set_Label (Exit_Label);
         Unit.End_Routine;

         if Feature.Once then
            Unit.Data_Label_RW (Once_Flag_Label);
            Unit.Data_RW (0);
            Unit.Data_Label_RW (Once_Value_Label);
            Unit.Data_RW (0);
         end if;

      end if;
   end Generate_Routine;

   -----------------
   -- Instantiate --
   -----------------

   overriding function Instantiate
     (Entity             : not null access Feature_Entity_Record;
      Type_Instantiation : not null access
        function (Generic_Type : Entity_Type) return Entity_Type)
      return Entity_Type
   is
   begin

      Ack.Semantic.Work.Check_Work_Item
        (Class        => Entity.Definition_Class,
         Feature_Name => Entity.Entity_Name_Id,
         Category     => Ack.Semantic.Work.Feature_Header);

      declare
         Instan : constant Feature_Entity :=
                    new Feature_Entity_Record'(Entity.all);
      begin

         Ack.Instantiated (Instan.all);

         if Instan.Value_Type /= null then
            Instan.Value_Type :=
              Instan.Value_Type.Instantiate (Type_Instantiation);
         end if;

         for Arg of Instan.Arguments loop
            Arg :=
              Ack.Variables.Variable_Entity
                (Arg.Instantiate (Type_Instantiation));
         end loop;

         Entity.Instantiated.Append (Instan);

         return Entity_Type (Instan);
      end;

   end Instantiate;

   --------------------------
   -- Is_Property_Of_Class --
   --------------------------

   function Is_Property_Of_Class
     (Feature : Feature_Entity_Record'Class;
      Class   : not null access constant
        Ack.Classes.Class_Entity_Record'Class)
      return Boolean
   is
      use Ack.Classes;
   begin
      return not Feature.Deferred
        and then Feature.Is_Property
        and then Constant_Class_Entity (Feature.Active_Class)
        = Constant_Class_Entity (Class);
   end Is_Property_Of_Class;

   -----------------
   -- New_Feature --
   -----------------

   function New_Feature
     (Name        : Name_Id;
      Alias       : Name_Id;
      Declaration : Node_Id;
      Property    : Boolean;
      Class       : not null access Ack.Classes.Class_Entity_Record'Class)
      return Feature_Entity
   is
   begin
      return Feature : constant Feature_Entity := new Feature_Entity_Record do
         Feature.Create
           (Name, Declaration,
            Table              => True,
            Parent_Environment => Class,
            Context            => Class);
         Feature.Alias := Alias;
         Feature.Effective_Class := Class;
         Feature.Definition_Class := Class;
         Feature.Property := Property;
         Feature.Property_Offset := Word_Offset'Last;
      end return;
   end New_Feature;

   ----------------
   -- Pop_Entity --
   ----------------

   overriding procedure Pop_Entity
     (Feature    : Feature_Entity_Record;
      Context    : not null access constant Root_Entity_Type'Class;
      Value_Type : not null access constant Root_Entity_Type'Class;
      Unit       : in out Tagatha.Code.Instance'Class)
   is
      pragma Assert (Feature.Property);
      Current        : constant Ack.Classes.Constant_Class_Entity :=
                         Ack.Classes.Constant_Class_Entity (Context);
   begin
--        Ada.Text_IO.Put_Line
--          ("pop entity: " & Feature.Active_Class.Qualified_Name
--           & "." & Feature.Declared_Name
--           & ": expanded = "
--           & (if Feature.Active_Class.Expanded then "yes" else "no"));

      if Feature.Active_Class.Expanded then
         Unit.Pop_Argument (1);
      else
         if Feature.Get_Type.Proper_Ancestor_Of (Value_Type) then
            Unit.Duplicate;
            Unit.Dereference;

            declare
               use Ack.Classes;
            begin
               Push_Offset
                 (Unit,
                  Constant_Class_Entity
                    (Value_Type.Class_Context).Ancestor_Table_Offset
                      (Constant_Class_Entity
                           (Feature.Get_Type.Class_Context)));
            end;

            Unit.Operate (Tagatha.Op_Add);
            Unit.Dereference;
            Unit.Operate (Tagatha.Op_Add);
         end if;

         Unit.Push_Argument (1);

         if Feature.Definition_Class /= Current
           and then not Current.Expanded
           and then not Feature.Intrinsic
         then
            Unit.Duplicate;
            Unit.Dereference;
            Unit.Dereference;
            --  Unit.Swap;
            Unit.Operate (Tagatha.Op_Subtract);
            Unit.Duplicate;
            Unit.Dereference;
            Push_Offset
              (Unit,
               Current.Ancestor_Table_Offset (Feature.Definition_Class));
            Unit.Operate (Tagatha.Op_Add);
            Unit.Dereference;
            Unit.Operate (Tagatha.Op_Add);
         end if;

         Unit.Pop_Indirect (Tagatha.General_Content,
                            Tagatha.Int_32 (Feature.Property_Offset));
      end if;
   end Pop_Entity;

   ------------------------
   -- Properties_Summary --
   ------------------------

   function Properties_Summary
     (Feature : Feature_Entity_Record'Class)
      return String
   is
      use type Ack.Types.Constant_Type_Entity;
      E_Type : constant Ack.Types.Constant_Type_Entity :=
                 Ack.Types.Constant_Type_Entity
                   (Feature.Get_Type);
   begin
      return Feature.Qualified_Name & ": "
        & (if E_Type = null then "routine" else E_Type.Qualified_Name)
        & (if E_Type /= null and then E_Type.Expanded
           then " expanded" else "")
        & (if Feature.Can_Update then " can-update" else "")
        & (if E_Type /= null and then E_Type.Detachable
           then " detachable" else "")
        & (if Feature.Deferred then " deferred" else "")
        & (if E_Type /= null and then E_Type.Is_Generic_Formal_Type
           then " generic-formal" else "");
   end Properties_Summary;

   -----------------
   -- Push_Entity --
   -----------------

   overriding procedure Push_Entity
     (Feature       : Feature_Entity_Record;
      Have_Current  : Boolean;
      Context       : not null access constant Root_Entity_Type'Class;
      Unit          : in out Tagatha.Code.Instance'Class)
   is
      Current        : constant Ack.Classes.Constant_Class_Entity :=
                         Ack.Classes.Constant_Class_Entity (Context);
   begin

      if Feature.Standard_Name = "void" then
         Unit.Push_Constant (Tagatha.Int_32'(0));
         return;
      elsif Feature.Explicit_Value then
         declare
            Value : constant Node_Id :=
                      Constant_Value (Feature.Explicit_Value_Node);
            K     : constant Node_Kind := Kind (Value);
            Text  : constant String :=
                      (if K = N_Boolean_Constant
                       then ""
                       else To_String (Get_Name (Value)));
         begin
            case N_Constant_Value (K) is
               when N_String_Constant =>
                  Push_String_Constant (Unit, Text);
               when N_Character_Constant =>
                  Unit.Push_Constant
                    (Tagatha.Int_32'
                       (Character'Pos (Text (Text'First))));
               when N_Integer_Constant =>
                  Unit.Push_Constant
                    (Tagatha.Int_32'Value (Text));
               when N_Boolean_Constant =>
                  Unit.Push_Constant
                    (Tagatha.Int_32'
                       (Boolean'Pos
                            (Boolean_Value (Feature.Explicit_Value_Node))));
            end case;
         end;
         return;
      end if;

      if not Have_Current then
         Unit.Push_Argument (1);
      end if;

--        if Feature.Definition_Class /= Current
--          and then not Current.Expanded
--          and then not Feature.Intrinsic
--        then
--           Unit.Duplicate;
--           Unit.Dereference;
--           Push_Offset
--             (Unit,
--              Current.Ancestor_Table_Offset (Feature.Definition_Class));
--           Unit.Operate (Tagatha.Op_Add);
--           Unit.Dereference;
--           Unit.Operate (Tagatha.Op_Add);
--        end if;

      if Feature.Intrinsic then
         null;
      elsif Feature.Is_Property then
         if not Feature.Active_Class.Expanded then
            if Feature.Definition_Class /= Current
              and then not Feature.Intrinsic
            then
               Unit.Duplicate;
               Unit.Dereference;
               Unit.Dereference;
               --  Unit.Swap;
               Unit.Operate (Tagatha.Op_Subtract);
               Unit.Duplicate;
               Unit.Dereference;
               Push_Offset
                 (Unit,
                  Current.Ancestor_Table_Offset (Feature.Definition_Class));
               Unit.Operate (Tagatha.Op_Add);
               Unit.Dereference;
               Unit.Operate (Tagatha.Op_Add);
            end if;
            Push_Offset (Unit, Feature.Property_Offset);
            Unit.Operate (Tagatha.Op_Add);
            Unit.Dereference;
         end if;
      else
         if Current.Expanded then
            Unit.Call (Feature.Link_Name,
                       Feature.Argument_Count + 1,
                       1);
         else
            --  push feature address from virtual table

            Unit.Duplicate;  --  duplicate the object pointer

            --  When we are calling a feature that was
            --  defined in an ancestor, we need to set
            --  up an object pointer that looks like
            --  the ancestor
            if Feature.Definition_Class /= Current
              and then not Current.Expanded
              and then not Feature.Intrinsic
            then
               Unit.Duplicate;      --  new copy of object pointer
               Unit.Dereference;    --  pointer to virtual table
               Unit.Dereference     --  get object pointer offset
                 (Offset =>
                    Tagatha.Int_32
                      (Current.Ancestor_Table_Offset
                           (Feature.Definition_Class)));

               --  add to the object pointer
               Unit.Operate (Tagatha.Op_Add);

               --  now the top of the stack is a pointer to what
               --  looks like an ancestor class object
            end if;

            --  Get the address of the feature from the virtual table
            --  Unit.Duplicate;
            Unit.Dereference;
            Unit.Dereference
              (Offset => Tagatha.Int_32 (Feature.Virtual_Table_Offset));

            declare
               L : constant Tagatha.Code.Label := Unit.Next_Label;
            begin
               Unit.Set_Label (L);
               Unit.Indirect_Call
                 (Feature.Argument_Count + 1, 1);
            end;
         end if;

         if Current.Expanded and then not Feature.Has_Result then
            null;
            --  if Current.Frame_Words = 1 then
            --     if Feature.Argument_Count > 0 then
            --        Unit.Save_Top;
            --        for I in 1 .. Feature.Argument_Count loop
            --           Unit.Drop;
            --        end loop;
            --        Unit.Restore_Top;
            --     end if;
            --  end if;
         else

            null;
            --  Unit.Restore (Feature.Argument_Count + 1);
            --  Unit.Drop;  --  current
            --  for I in 1 .. Feature.Argument_Count loop
            --     Unit.Drop;
            --  end loop;
            --
            --  push result
            if Feature.Has_Result then
               Unit.Push_Return (1);
            end if;
         end if;
      end if;

   end Push_Entity;

   -------------------------
   -- Push_Entity_Address --
   -------------------------

   overriding procedure Push_Entity_Address
     (Feature       : Feature_Entity_Record;
      Have_Current  : Boolean;
      Context       : not null access constant Root_Entity_Type'Class;
      Unit          : in out Tagatha.Code.Instance'Class)
   is
      Current        : constant Ack.Classes.Constant_Class_Entity :=
                         Ack.Classes.Constant_Class_Entity (Context);
   begin
      if Feature.Standard_Name = "void" then
         raise Constraint_Error with "attempted to push address of void";
      elsif Feature.Explicit_Value then
         raise Constraint_Error with
           "attempted to push address of explicit value";
      end if;

      if not Have_Current then
         Unit.Push_Argument (1);
      end if;

      if Feature.Intrinsic then
         raise Constraint_Error with "attempt to push address of intrinsic";
      elsif not Feature.Is_Property then
         raise Constraint_Error with "attempt to push address of routine";
      elsif Feature.Is_Property then
         if Feature.Definition_Class /= Current
           and then not Feature.Intrinsic
         then
            Unit.Duplicate;
            Unit.Dereference;
            Unit.Dereference;
            --  Unit.Swap;
            Unit.Operate (Tagatha.Op_Subtract);
            Unit.Duplicate;
            Unit.Dereference;
            Unit.Dereference
              (Tagatha.General_Content,
               Tagatha.Int_32
                 (Current.Ancestor_Table_Offset (Feature.Definition_Class)));
            Unit.Operate (Tagatha.Op_Add);
         end if;
         Push_Offset (Unit, Feature.Property_Offset);
         Unit.Operate (Tagatha.Op_Add);
      end if;
   end Push_Entity_Address;

   --------------------
   -- Push_Old_Value --
   --------------------

   overriding procedure Push_Old_Value
     (Feature : in out Feature_Entity_Record;
      Unit    : in out Tagatha.Code.Instance'Class;
      Node    : Node_Id)
   is
   begin
      for I in 1 .. Feature.Olds.Last_Index loop
         if Feature.Olds.Element (I) = Node then
            Unit.Push_Local
              (Tagatha.Local_Index
                 (Feature.Local_Count
                  + (if Feature.Has_Result then 1 else 0)
                  + I));
            return;
         end if;
      end loop;
      raise Constraint_Error with
        "can't find old value";
   end Push_Old_Value;

   ---------------------
   -- Remove_Implicit --
   ---------------------

   overriding procedure Remove_Implicit
     (Feature         : in out Feature_Entity_Record)
   is
      Implicit_Entity : constant Entity_Type :=
                          Feature.Children.Implicits.Last_Element;
   begin
      Root_Entity_Type (Feature).Remove_Implicit;
      if Ack.Variables.Is_Variable (Implicit_Entity) then
         Feature.Local_Count := Feature.Local_Count - 1;
      end if;
   end Remove_Implicit;

   --------------------
   -- Save_Old_Value --
   --------------------

   overriding procedure Save_Old_Value
     (Feature : in out Feature_Entity_Record;
      Node    : Node_Id)
   is
   begin
      for N of Feature.Olds loop
         if N = Node then
            return;
         end if;
      end loop;
      Feature.Olds.Append (Node);
   end Save_Old_Value;

   ---------------------------
   -- Scan_Original_Classes --
   ---------------------------

   procedure Scan_Original_Classes
     (Feature : Feature_Entity_Record'Class;
      Process : not null access
        procedure (Class : not null access constant
                     Ack.Classes.Class_Entity_Record'Class))
   is
   begin
      if Feature.Original_Classes.Is_Empty then
         if Feature.Deferred then
            null;
         else
            Process (Feature.Effective_Class);
         end if;
      else
         for Class of Feature.Original_Classes loop
            Process (Ack.Classes.Class_Entity (Class));
         end loop;
      end if;
   end Scan_Original_Classes;

   -----------------
   -- Set_Creator --
   -----------------

   procedure Set_Creator
     (Feature     : in out Feature_Entity_Record'Class)
   is
   begin
      Feature.Creator := True;
   end Set_Creator;

   ------------------
   -- Set_Deferred --
   ------------------

   procedure Set_Deferred
     (Feature     : in out Feature_Entity_Record'Class)
   is
   begin
      Feature.Property := False;
      Feature.Routine := True;
      Feature.Deferred_Feature := True;
      Feature.Effective_Class := null;
   end Set_Deferred;

   ------------------------
   -- Set_Explicit_Value --
   ------------------------

   procedure Set_Explicit_Value
     (Feature : in out Feature_Entity_Record'Class;
      Value   : Node_Id)
   is
   begin
      Feature.Explicit_Value := True;
      Feature.Explicit_Value_Node := Value;
   end Set_Explicit_Value;

   ------------------
   -- Set_External --
   ------------------

   procedure Set_External
     (Feature        : in out Feature_Entity_Record'Class;
      External_Type  : String;
      External_Alias : String)
   is
      Dot_Index       : constant Natural :=
                          Ada.Strings.Fixed.Index (External_Alias, ".");
      External_Object : constant String :=
                          (if Dot_Index > 0
                           then External_Alias
                             (External_Alias'First .. Dot_Index - 1)
                           else "");
      External_Label  : constant String :=
                          External_Alias
                            (Dot_Index + 1 .. External_Alias'Last);
   begin
      Feature.Property := External_Type = "aqua_property";
      Feature.Intrinsic := External_Type = "intrinsic";
      Feature.Routine := not Feature.Property;
      Feature.External := True;
      Feature.External_Object := Get_Name_Id (External_Object);
      Feature.External_Type := Get_Name_Id (External_Type);
      Feature.External_Label := Get_Name_Id (External_Label);
   end Set_External;

   ------------------------
   -- Set_Feature_Entity --
   ------------------------

   procedure Set_Feature_Entity
     (Node    : Node_Id;
      Feature : not null access Feature_Entity_Record'Class)
   is
   begin
      Set_Entity (Node, Feature);
   end Set_Feature_Entity;

   -------------------------
   -- Set_Property_Offset --
   -------------------------

   procedure Set_Property_Offset
     (Feature : in out Feature_Entity_Record'Class;
      Offset  : Word_Offset)
   is
   begin
      Feature.Property_Offset := Offset;
      for Instan of Feature.Instantiated loop
         Instan.Set_Property_Offset (Offset);
      end loop;
   end Set_Property_Offset;

   -------------------
   -- Set_Redefined --
   -------------------

   procedure Set_Redefined
     (Feature          : in out Feature_Entity_Record'Class;
      Class            : not null access
        Ack.Classes.Class_Entity_Record'Class;
      Original_Feature : not null access constant
        Feature_Entity_Record'Class)
   is
   begin
--      Feature.Property := False;
      Feature.Definition_Class :=
        Original_Feature.Definition_Class;
      Feature.Original_Classes.Append
        (Original_Feature.Definition_Class);
      Feature.Redefined_Class := Class;
      Feature.Redefined_Feature :=
        Constant_Feature_Entity (Original_Feature);
   end Set_Redefined;

   ---------------------
   -- Set_Rescue_Node --
   ---------------------

   procedure Set_Rescue_Node
     (Feature : in out Feature_Entity_Record'Class;
      Node    : Node_Id)
   is
   begin
      Feature.Rescue_Node := Node;
   end Set_Rescue_Node;

   ---------------------
   -- Set_Result_Type --
   ---------------------

   procedure Set_Result_Type
     (Feature     : in out Feature_Entity_Record'Class;
      Result_Type : not null access Ack.Types.Type_Entity_Record'Class)
   is
   begin
      Feature.Value_Type := Entity_Type (Result_Type);
   end Set_Result_Type;

   -----------------
   -- Set_Routine --
   -----------------

   procedure Set_Routine
     (Feature      : in out Feature_Entity_Record'Class;
      Routine_Node : Node_Id)
   is
   begin
      Feature.Property := False;
      Feature.Routine_Node := Routine_Node;
      Feature.Routine := True;
      Feature.Once := Once_Routine (Routine_Node);
      Feature.Has_Current := True;
   end Set_Routine;

   ------------------------------
   -- Set_Virtual_Table_Offset --
   ------------------------------

   procedure Set_Virtual_Table_Offset
     (Feature : in out Feature_Entity_Record'Class;
      Offset  : Word_Offset)
   is
   begin
      Feature.VT_Offset := Offset;
   end Set_Virtual_Table_Offset;

   --------------------------
   -- Virtual_Table_Offset --
   --------------------------

   function Virtual_Table_Offset
     (Feature : Feature_Entity_Record'Class)
      return Word_Offset
   is
   begin
      if Feature.VT_Offset = 0 then
         declare
            Original_Feature : constant Feature_Entity :=
                                 Feature.Definition_Class.Feature
                                   (Feature.Entity_Name_Id);
         begin
            return Original_Feature.Virtual_Table_Offset;
         end;
      else
         return Feature.VT_Offset;
      end if;
   end Virtual_Table_Offset;

end Ack.Features;
