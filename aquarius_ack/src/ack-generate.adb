with Ada.Exceptions;
with Ada.Text_IO;

with Ack.Classes;
with Ack.Features;
with Ack.Types;

with Ack.Generate.Intrinsics;

with Ack.IO;

with Aquarius.Configuration;

with Tagatha.Arch.Aqua;

package body Ack.Generate is

   procedure Generate_Feature
     (Unit    : in out Tagatha.Code.Instance'Class;
      Class   : not null access constant
        Ack.Classes.Class_Entity_Record'Class;
      Feature : not null access
        Ack.Features.Feature_Entity_Record'Class);

   procedure Generate_Creation
     (Unit     : in out Tagatha.Code.Instance'Class;
      Context  : not null access constant Root_Entity_Type'Class;
      Creation : Node_Id);

   procedure Generate_Conditional
     (Unit        : in out Tagatha.Code.Instance'Class;
      Context     : not null access constant Root_Entity_Type'Class;
      Conditional : Node_Id);

   procedure Generate_Loop
     (Unit      : in out Tagatha.Code.Instance'Class;
      Context   : not null access constant Root_Entity_Type'Class;
      Loop_Node : Node_Id);

   procedure Generate_Operator_Expression
     (Unit          : in out Tagatha.Code.Instance'Class;
      Context       : not null access constant Root_Entity_Type'Class;
      Operator_Node : Node_Id);

   procedure Generate_Tuple_Expression
     (Unit       : in out Tagatha.Code.Instance'Class;
      Context    : not null access constant Root_Entity_Type'Class;
      Expression : Node_Id);

   procedure Generate_Precursor
     (Unit      : in out Tagatha.Code.Instance'Class;
      Context   : not null access constant Root_Entity_Type'Class;
      Precursor : Node_Id);

   procedure Generate_Check
     (Unit  : in out Tagatha.Code.Instance'Class;
      Context : not null access constant Root_Entity_Type'Class;
      Check   : Node_Id)
   is null;

   procedure Generate_Retry
     (Unit  : in out Tagatha.Code.Instance'Class;
      Context : not null access constant Root_Entity_Type'Class;
      Retry   : Node_Id;
      Target  : Tagatha.Code.Label);

   procedure Generate_Set_Value
     (Unit       : in out Tagatha.Code.Instance'Class;
      Context    : not null access constant Root_Entity_Type'Class;
      Value_Type : not null access constant Root_Entity_Type'Class;
      Node       : Node_Id);

   procedure Generate_Assignment_Conversion
     (Unit               : in out Tagatha.Code.Instance'Class;
      From_Type, To_Type : not null access constant
        Root_Entity_Type'Class);

   ------------------------------------
   -- Generate_Assignment_Conversion --
   ------------------------------------

   procedure Generate_Assignment_Conversion
     (Unit               : in out Tagatha.Code.Instance'Class;
      From_Type, To_Type : not null access constant
        Root_Entity_Type'Class)
   is
      use type Ack.Classes.Class_Entity;
      From : constant Ack.Types.Constant_Type_Entity :=
               Ack.Types.Constant_Type_Entity (From_Type);
      To   : constant Ack.Types.Constant_Type_Entity :=
               Ack.Types.Constant_Type_Entity (To_Type);
   begin
      if From.Class /= To.Class
        and then not From.Class.Expanded
        and then From.Class.Qualified_Name /= "None"
      then
         declare
            Offset : constant Word_Offset :=
                       From.Class.Ancestor_Table_Offset (To.Class);
         begin
            if Offset > 0 then
               Unit.Duplicate;
               Unit.Dereference;
               Unit.Dereference (Tagatha.Int_32 (Offset));
               Unit.Operate (Tagatha.Op_Add);
            end if;
         end;
      end if;
   end Generate_Assignment_Conversion;

   --------------------------------
   -- Generate_Class_Declaration --
   --------------------------------

   procedure Generate_Class_Declaration
     (Node : Node_Id;
      Root : Boolean)
   is
      Unit : Tagatha.Code.Instance;
      Class : constant Ack.Classes.Class_Entity :=
                Ack.Classes.Get_Class_Entity (Node);

   begin

      if Root then
         Unit.Source_Location
           (Line   => Positive (Get_Program (Node).Line),
            Column => Positive (Get_Program (Node).Column));

         Unit.Begin_Routine
           (Name => Class.Link_Name & "$main");
         Unit.Call (Class.Allocator_Name, 0, 1);
         Unit.Push_Return (1);
         Unit.Duplicate;
         Unit.Dereference;

         Unit.Dereference
           (Tagatha.Int_32
              (Class.Feature (Get_Name_Id ("make")).Virtual_Table_Offset));
         Unit.Indirect_Call (1, 0);
         Unit.Push_Constant (Tagatha.Int_32'(0));
         Unit.Pop_Result (1);
         Unit.Exit_Routine;
         Unit.End_Routine;
      end if;

      declare

         function Class_Defined_Feature
           (Feature : not null access constant
              Ack.Features.Feature_Entity_Record'Class)
            return Boolean
         is (not Feature.Deferred
             and then Feature.Effective_Class = Class);

         procedure Check_Feature_Bindings
           (Feature : not null access
              Ack.Features.Feature_Entity_Record'Class);

         ----------------------------
         -- Check_Feature_Bindings --
         ----------------------------

         procedure Check_Feature_Bindings
           (Feature : not null access
              Ack.Features.Feature_Entity_Record'Class)
         is
            Note_Name : constant String :=
                          "aqua_action_binding_"
                          & Feature.Standard_Name;
         begin

            if Class.Has_Note (Note_Name) then
               declare
                  Parent_Name   : constant String :=
                                    Ada.Characters.Handling.To_Lower
                                      (Class.Get_Note_Item (Note_Name, 1));
                  Position_Name : constant String :=
                                    Ada.Characters.Handling.To_Lower
                                      (Class.Get_Note_Item (Note_Name, 2));
                  Child_Name    : constant String :=
                                    Ada.Characters.Handling.To_Lower
                                      (Class.Get_Note_Item (Note_Name, 3));
               begin
                  Unit.Note
                    ("bind_action", 1,
                     Class.Declaration_Context.Standard_Name
                     & " " & Feature.Link_Name
                     & " " & Position_Name
                     & " " & Parent_Name
                     & " " & Child_Name);
               end;
            end if;

         end Check_Feature_Bindings;

      begin

         Class.Scan_Features
           (Class_Defined_Feature'Access,
            Check_Feature_Bindings'Access);
      end;

      Unit.Begin_Routine
        (Class.Link_Name & "$init");
      Unit.Exit_Routine;
      Unit.End_Routine;

      if not Class.Expanded and then not Class.Deferred then
         Class.Generate_Virtual_Table (Unit);
      end if;

      if not Class.Deferred and then not Class.Expanded then
         Class.Generate_Object_Allocator (Unit);
      end if;

      declare

         function Class_Defined_Feature
           (Feature : not null access constant
              Ack.Features.Feature_Entity_Record'Class)
            return Boolean
         is (not Feature.Deferred
             and then Feature.Effective_Class = Class);

         procedure Generate_Feature
           (Feature : not null access
              Ack.Features.Feature_Entity_Record'Class);

         ----------------------
         -- Generate_Feature --
         ----------------------

         procedure Generate_Feature
           (Feature : not null access
              Ack.Features.Feature_Entity_Record'Class)
         is
         begin
            Generate_Feature (Unit, Class, Feature);
         end Generate_Feature;

      begin

         Class.Scan_Features
           (Class_Defined_Feature'Access,
            Generate_Feature'Access);
      end;

      declare
         Target : Tagatha.Arch.Aqua.Instance;
      begin
         Unit.Generate (Target);
         Target.Save
           (Aquarius.Configuration.Assembly_File_Path (Class.Base_File_Name));
      exception
         when E : others =>
            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Standard_Error,
               Class.Base_File_Name & ": "
               & Ada.Exceptions.Exception_Message (E));
            raise;
      end;

   end Generate_Class_Declaration;

   -----------------------
   -- Generate_Compound --
   -----------------------

   procedure Generate_Compound
     (Unit         : in out Tagatha.Code.Instance'Class;
      Context      : not null access constant Root_Entity_Type'Class;
      Node         : Node_Id;
      Retry_Target : Tagatha.Code.Label := Tagatha.Code.No_Label)
   is
      procedure Generate_Instruction
        (Instruction : Node_Id);

      --------------------------
      -- Generate_Instruction --
      --------------------------

      procedure Generate_Instruction
        (Instruction : Node_Id)
      is
      begin
         Unit.Source_Location
           (Line   => Positive (Get_Program (Instruction).Line),
            Column => Positive (Get_Program (Instruction).Column));

         case N_Instruction (Kind (Instruction)) is
            when N_Assignment =>
               Generate_Expression (Unit, Context, Expression (Instruction));
               Generate_Set_Value
                 (Unit, Context, Get_Type (Expression (Instruction)),
                  Variable (Instruction));
            when N_Creation_Instruction =>
               Generate_Creation (Unit, Context, Instruction);
            when N_Conditional =>
               Generate_Conditional (Unit, Context, Instruction);
            when N_Loop =>
               Generate_Loop (Unit, Context, Instruction);
            when N_Precursor =>
               Generate_Precursor (Unit, Context, Instruction);
            when N_Check =>
               Generate_Check (Unit, Context, Instruction);
            when N_Retry =>
               Generate_Retry (Unit, Context, Instruction, Retry_Target);
         end case;
      end Generate_Instruction;

   begin
      Scan (Instructions (Node), Generate_Instruction'Access);
   end Generate_Compound;

   --------------------------
   -- Generate_Conditional --
   --------------------------

   procedure Generate_Conditional
     (Unit        : in out Tagatha.Code.Instance'Class;
      Context     : not null access constant Root_Entity_Type'Class;
      Conditional : Node_Id)
   is

      Out_Label  : constant Tagatha.Code.Label := Unit.Next_Label;
      Else_Label : Tagatha.Code.Label := Tagatha.Code.No_Label;

      procedure Generate_Element (Element : Node_Id);

      ----------------------
      -- Generate_Element --
      ----------------------

      procedure Generate_Element (Element : Node_Id) is
         Condition : constant Node_Id := Field_1 (Element);
         Compound  : constant Node_Id := Field_2 (Element);
      begin
         if Tagatha.Code.Has_Label (Else_Label) then
            Unit.Set_Label (Else_Label);
            Else_Label := Tagatha.Code.No_Label;
         end if;

         if Condition /= No_Node then
            Generate_Expression (Unit, Context, Condition);
            if Implicit_Entity (Condition) then
               declare
                  Local : constant Tagatha.Local_Index :=
                            Unit.Add_Local;
               begin
                  Unit.Duplicate;
                  Unit.Pop_Local (Local);
               end;
            end if;

            Else_Label := Unit.Next_Label;
            Unit.Branch (Tagatha.Z, Else_Label);
         end if;
         Generate_Compound (Unit, Context, Compound);
         if Condition /= No_Node
           and then Implicit_Entity (Condition)
         then
            Unit.Remove_Local;
         end if;

         Unit.Branch (Out_Label);
      end Generate_Element;

   begin
      Scan (Node_Table.Element (Conditional).List,
            Generate_Element'Access);
      if Tagatha.Code.Has_Label (Else_Label) then
         Unit.Set_Label (Else_Label);
      end if;
      Unit.Set_Label (Out_Label);

   end Generate_Conditional;

   -----------------------
   -- Generate_Creation --
   -----------------------

   procedure Generate_Creation
     (Unit     : in out Tagatha.Code.Instance'Class;
      Context  : not null access constant Root_Entity_Type'Class;
      Creation : Node_Id)
   is
      Call_Node : constant Node_Id := Creation_Call (Creation);
      Explicit_Type_Node : constant Node_Id :=
                             Explicit_Creation_Type (Creation);
      Explicit_Call_Node : constant Node_Id :=
                             Explicit_Creation_Call (Call_Node);
      Creation_Type      : constant Entity_Type :=
                             (if Explicit_Type_Node in Real_Node_Id
                              then Get_Entity (Explicit_Type_Node)
                              else Get_Entity (Creation).Get_Type);
      Created_Context    : constant Constant_Entity_Type :=
                             Get_Context (Creation).Class_Context;
      Created_Entity     : constant Entity_Type :=
                             Get_Entity (Creation);
      Creation_Routine   : Entity_Type;
      Local              : constant Tagatha.Local_Index :=
                             Unit.Add_Local;
   begin
      Unit.Call (Creation_Type.Allocator_Name, 0, 1);
      Unit.Push_Return (1);
      Unit.Pop_Local (Local);

      if Explicit_Call_Node not in Real_Node_Id then
         pragma Assert (Creation_Type.Has_Default_Creation_Routine);
         Creation_Routine :=
           Creation_Type.Default_Creation_Routine;
      else
         Creation_Routine :=
           Get_Entity (Explicit_Call_Node);

         declare
            Actual_List_Node  : constant Node_Id :=
                                  Actual_List (Explicit_Call_Node);
            Actual_List       : constant List_Id :=
                                  (if Actual_List_Node /= No_Node
                                   then Node_Table.Element
                                     (Actual_List_Node).List
                                   else No_List);
            Actuals_Node_List : constant List_Of_Nodes.List :=
                                  (if Actual_List = No_List
                                   then List_Of_Nodes.Empty_List
                                   else List_Table.Element (Actual_List)
                                   .List);
         begin
            for Item of reverse Actuals_Node_List loop
               Generate_Expression (Unit, Context, Item);
            end loop;
         end;
      end if;

      if Creation_Routine /= null then

         Unit.Push_Local (Local);

         Creation_Routine.Push_Entity
           (Have_Current => True,
            Context      => Creation_Type.Class_Context,
            Unit         => Unit);

      end if;

      Unit.Push_Local (Local);
      Created_Entity.Pop_Entity (Created_Context, Creation_Type, Unit);

      Unit.Remove_Local;

   exception
      when others =>
         Ada.Text_IO.Put_Line ("no entity in this tree:");
         Ack.IO.Put_Line (Creation);
         raise;

   end Generate_Creation;

   -------------------
   -- Generate_Exit --
   -------------------

   procedure Generate_Exit
     (Unit    : in out Tagatha.Code.Instance'Class)
   is
   begin
      Unit.Call ("system.os.halt", 1, 0);
   end Generate_Exit;

   -------------------------
   -- Generate_Expression --
   -------------------------

   procedure Generate_Expression
     (Unit       : in out Tagatha.Code.Instance'Class;
      Context    : not null access constant Root_Entity_Type'Class;
      Expression : Node_Id)
   is
   begin
      case N_Expression_Node (Kind (Expression)) is
         when N_Operator =>
            Generate_Operator_Expression (Unit, Context, Expression);
         when N_Precursor =>
            Generate_Precursor (Unit, Context, Expression);
         when N_Attachment_Test =>
            Generate_Expression (Unit, Context, Field_1 (Expression));
         when N_Old =>
            Generate_Expression (Unit, Context, Field_1 (Expression));
         when N_Tuple =>
            Generate_Tuple_Expression (Unit, Context, Expression);
         when N_Constant =>
            declare
               Value : constant Node_Id := Constant_Value (Expression);
            begin
               case N_Constant_Value (Kind (Value)) is
                  when N_String_Constant =>
                     declare
                        Label : constant String :=
                                  Next_String_Label ("string_constant");
                        Text  : constant String :=
                                  To_String (Get_Text (Value));
                     begin
                        Unit.Data_Label (Label);
                        Unit.String_Constant (Text);
                        Unit.Call ("string.$create", 0, 1);
                        Unit.Push_Return (1);
                        Unit.Duplicate;
                        Unit.Push_Name
                          (Name    => Label,
                           Extern  => False,
                           Content => Tagatha.General_Content,
                           Address => True);
                        Unit.Swap;
                        Unit.Call ("string.create_from_string_literal", 2, 0);
                     end;
                  when N_Character_Constant =>
                     declare
                        Text : constant String :=
                                 To_String (Get_Name (Value));
                     begin
                        Unit.Push_Constant
                          (Tagatha.Int_32'
                             (Character'Pos (Text (Text'First))));
                     end;

                  when N_Integer_Constant =>
                     Unit.Push_Constant
                       (Tagatha.Word_64'Value
                          (To_String
                               (Get_Name (Value))));
                  when N_Boolean_Constant =>
                     Unit.Push_Constant
                       (Tagatha.Int_32'
                          (if Boolean_Value (Value) then 1 else 0));
               end case;
            end;
      end case;

      if Has_Destination_Type (Expression) then
         Generate_Assignment_Conversion
           (Unit      => Unit,
            From_Type => Get_Type (Expression),
            To_Type   => Get_Destination_Type (Expression));
      end if;
   end Generate_Expression;

   ----------------------
   -- Generate_Feature --
   ----------------------

   procedure Generate_Feature
     (Unit    : in out Tagatha.Code.Instance'Class;
      Class   : not null access constant
        Ack.Classes.Class_Entity_Record'Class;
      Feature : not null access
        Ack.Features.Feature_Entity_Record'Class)
   is
   begin
      Feature.Generate_Routine (Class, Unit);
   end Generate_Feature;

   -------------------
   -- Generate_Loop --
   -------------------

   procedure Generate_Loop
     (Unit      : in out Tagatha.Code.Instance'Class;
      Context   : not null access constant Root_Entity_Type'Class;
      Loop_Node : Node_Id)
   is
      Iteration_Node      : constant Node_Id := Loop_Iteration (Loop_Node);
      Initialization_Node : constant Node_Id :=
                              Loop_Initialization (Loop_Node);
      Exit_Condition_Node : constant Node_Id :=
                              Loop_Exit_Condition (Loop_Node);
      Loop_Body_Node      : constant Node_Id := Loop_Body (Loop_Node);
      Top_Label           : constant Tagatha.Code.Label := Unit.Next_Label;
      Out_Label           : constant Tagatha.Code.Label := Unit.Next_Label;

      Has_Iterator        : constant Boolean := Iteration_Node /= No_Node;
      It_Expression       : Node_Id := No_Node;
      Iterable_Type       : Ack.Types.Type_Entity;
      Iterator_Type       : Ack.Types.Type_Entity;
      New_Cursor_Feature  : Ack.Features.Feature_Entity;
      After_Feature       : Ack.Features.Feature_Entity;
      Next_Feature        : Ack.Features.Feature_Entity;
      --  Iterator_Entity     : Entity_Type;
   begin

      if Has_Iterator then
         It_Expression := Expression (Iteration_Node);
         Iterable_Type := Ack.Types.Type_Entity (Get_Type (It_Expression));
         New_Cursor_Feature :=
           Iterable_Type.Feature (Get_Name_Id ("new_cursor"));
         Iterator_Type :=
           Ack.Types.Type_Entity
             (New_Cursor_Feature.Get_Type);
         After_Feature :=
           Iterator_Type.Feature (Get_Name_Id ("after"));
         Next_Feature :=
           Iterator_Type.Feature (Get_Name_Id ("next"));
         --  Iterator_Entity := Get_Entity (Iteration_Node);

      end if;

      if Initialization_Node /= No_Node then
         Generate_Compound (Unit, Context, Compound (Initialization_Node));
      end if;

      if Has_Iterator then
         Generate_Expression (Unit, Context, It_Expression);
         New_Cursor_Feature.Push_Entity
           (Have_Current => True,
            Context      => Iterable_Type.Class_Context,
            Unit         => Unit);
         declare
            Local : constant Tagatha.Local_Index :=
                      Unit.Add_Local;
         begin
            Unit.Pop_Local (Local);
            Unit.Push_Local (Local);
         end;
      end if;

      Unit.Set_Label (Top_Label);

      if Has_Iterator then
         Unit.Duplicate;
         After_Feature.Push_Entity
           (Have_Current => True,
            Context      => Iterator_Type.Class_Context,
            Unit         => Unit);
         Unit.Branch (Tagatha.NZ, Out_Label);
      end if;

      if Exit_Condition_Node /= No_Node then
         Generate_Expression (Unit, Context, Expression (Exit_Condition_Node));
         Unit.Branch (Tagatha.NZ, Out_Label);
      end if;

      Generate_Compound (Unit, Context, Compound (Loop_Body_Node));

      if Has_Iterator then
         Unit.Duplicate;
         Next_Feature.Push_Entity
           (Have_Current => True,
            Context      => Iterator_Type.Class_Context,
            Unit         => Unit);
      end if;

      Unit.Branch (Top_Label);

      Unit.Set_Label (Out_Label);

      if Iteration_Node /= No_Node then
         Unit.Remove_Local;
      end if;

   end Generate_Loop;

   ----------------------------------
   -- Generate_Operator_Expression --
   ----------------------------------

   procedure Generate_Operator_Expression
     (Unit          : in out Tagatha.Code.Instance'Class;
      Context       : not null access constant Root_Entity_Type'Class;
      Operator_Node : Node_Id)
   is
      Operator  : constant Name_Id := Get_Name (Operator_Node);
      Left      : constant Node_Id := Field_1 (Operator_Node);
      Right     : constant Node_Id := Field_2 (Operator_Node);
      Is_Andthen : constant Boolean :=
                     Operator = Get_Name_Id ("andthen");
      Is_Orelse  : constant Boolean :=
                     Operator = Get_Name_Id ("orelse");
   begin

      if Is_Andthen or else Is_Orelse then
         declare
            Leave : constant Tagatha.Code.Label := Unit.Next_Label;
            Loc   : constant Tagatha.Local_Index := Unit.Add_Local;
         begin
            Generate_Expression (Unit, Context, Left);
            Unit.Duplicate;
            Unit.Pop_Local (Loc);

            if Is_Andthen then
               Unit.Branch (Tagatha.Z, Leave);
            else
               Unit.Branch (Tagatha.NZ, Leave);
            end if;

            Generate_Expression (Unit, Context, Right);
            Unit.Pop_Local (Loc);

            Unit.Set_Label (Leave);
            Unit.Push_Local (Loc);
            Unit.Remove_Local;
         end;
      elsif Operator = Get_Name_Id ("implies") then
         Generate_Expression (Unit, Context, Left);
         Unit.Operate (Tagatha.Op_Not);
         Generate_Expression (Unit, Context, Right);
         Unit.Operate (Tagatha.Op_Or);
      else
         declare
            Entity : constant Entity_Type := Get_Entity (Operator_Node);

            procedure Push (Argument_Index : Positive);

            ----------
            -- Push --
            ----------

            procedure Push (Argument_Index : Positive) is
               Arg : constant Node_Id :=
                       (if Argument_Index = 1
                        then Left else Right);
            begin
               Generate_Expression (Unit, Context, Arg);
            end Push;

         begin
            if Entity.Intrinsic then
               Entity.Push_Entity
                 (Have_Current => True,
                  Context      => Get_Type (Left).Class_Context,
                  Unit         => Unit);
               Ack.Generate.Intrinsics.Generate_Intrinsic
                 (Unit      => Unit,
                  Name      =>
                    To_Standard_String
                      (Ack.Features.Feature_Entity
                           (Entity).External_Label),
                  Arg_Count => (if Right = No_Node then 1 else 2),
                  Push      => Push'Access);
            else
               if Right /= No_Node then
                  Generate_Expression (Unit, Context, Right);
               end if;

               Generate_Expression (Unit, Context, Left);

               Entity.Push_Entity
                 (Have_Current => True,
                  Context      => Get_Type (Left).Class_Context,
                  Unit         => Unit);
            end if;
         end;
      end if;
   end Generate_Operator_Expression;

   ------------------------
   -- Generate_Precursor --
   ------------------------

   procedure Generate_Precursor
     (Unit      : in out Tagatha.Code.Instance'Class;
      Context   : not null access constant Root_Entity_Type'Class;
      Precursor : Node_Id)
   is
      List   : constant List_Id :=
                 Node_Table (Precursor).List;

      Pending : List_Of_Nodes.List;

      First_Element : constant Node_Id :=
                        List_Table (List).List.First_Element;
      Last_Element  : constant Node_Id :=
                        List_Table (List).List.Last_Element;

      Previous_Entity : Ack.Constant_Entity_Type := null;
      Previous_Context : Ack.Classes.Constant_Class_Entity := null;

      procedure Apply_Arguments
        (Actuals_List : List_Id);

      procedure Process
        (Element : Node_Id;
         Last    : Boolean);

      ---------------------
      -- Apply_Arguments --
      ---------------------

      procedure Apply_Arguments
        (Actuals_List : List_Id)
      is
         Actuals_Node_List : constant List_Of_Nodes.List :=
                               List_Table.Element
                                 (Actuals_List).List;

         procedure Apply (Item : Node_Id);

         -----------
         -- Apply --
         -----------

         procedure Apply (Item : Node_Id) is
         begin
            Generate_Expression (Unit, Context, Item);
         end Apply;

      begin
         for Item of reverse Actuals_Node_List loop
            Apply (Item);
         end loop;
      end Apply_Arguments;

      -------------
      -- Process --
      -------------

      procedure Process
        (Element : Node_Id;
         Last    : Boolean)
      is
         use type Ack.Types.Constant_Type_Entity;
         Entity : constant Entity_Type := Get_Entity (Element);
         E_Type : constant Ack.Types.Constant_Type_Entity :=
                    Ack.Types.Constant_Type_Entity (Entity.Get_Type);
      begin
         if not Node_Table.Element (Element).Attached
           and then E_Type /= null
           and then Entity.Standard_Name /= "void"
           and then not E_Type.Expanded
           and then Entity.Can_Update
           and then not Entity.Attached
           and then not E_Type.Detachable
           and then not E_Type.Deferred
           and then not E_Type.Is_Generic_Formal_Type
         then

            --  Ada.Text_IO.Put_Line
            --    (Get_Program (Element).Show_Location
            --     & ": implicit create test: entity = "
            --     & Entity.Description
            --     & "; context = "
            --     & E_Type.Class_Context.Description);

            Entity.Push_Entity_Address
              (Have_Current => Element /= First_Element,
               Context      => Get_Context (Element),
               Unit         => Unit);

            declare
               Label : constant Tagatha.Code.Label := Unit.Next_Label;
            begin
               Unit.Duplicate;
               Unit.Dereference;
               Unit.Branch (Tagatha.NZ, Label);
               Unit.Begin_Block;
               if not E_Type.Has_Default_Creation_Routine then
                  Push_String_Constant
                    (Unit,
                     Get_Program (Element).Show_Location
                     & ": no default create routine for "
                     & Entity.Qualified_Name);
                  Ack.Generate.Generate_Exit (Unit);
               else
                  Unit.Duplicate;
                  Unit.Call (E_Type.Allocator_Name, 0, 1);
                  Unit.Push_Return (1);

                  declare
                     Create_Routine : constant Entity_Type :=
                                        E_Type.Default_Creation_Routine;
                  begin
                     if Create_Routine /= null then
                        Unit.Duplicate;
                        Create_Routine.Push_Entity
                          (Have_Current => True,
                           Context      => E_Type.Class_Context,
                           Unit         => Unit);
                     end if;
                  end;
                  Unit.Swap;
                  Unit.Pop_Indirect;
               end if;
               Unit.End_Block;
               Unit.Set_Label (Label);
            end;

            Unit.Dereference;

         else

            Entity.Push_Entity
              (Have_Current => Element /= First_Element,
               Context      => Get_Context (Element),
               Unit         => Unit);

         end if;

         if not Last then
            Previous_Entity := Constant_Entity_Type (Entity);
            Previous_Context :=
              Ack.Classes.Constant_Class_Entity
                (Get_Context (Element).Class_Context);
         end if;

      exception
         when others =>
            Ada.Text_IO.Put_Line
              (Get_Program (Element).Show_Location
               & ": process failed for "
               & Entity.Qualified_Name);
            raise;
      end Process;

   begin

      for Element of List_Table (List).List loop
         Pending.Append (Element);

         declare
            Actual_List_Node : constant Node_Id :=
                                 Actual_List (Element);
            Actual_List      : constant List_Id :=
                                 (if Actual_List_Node /= No_Node
                                  then Node_Table.Element
                                    (Actual_List_Node).List
                                  else No_List);
            Entity           : constant Entity_Type := Get_Entity (Element);
         begin
            if Element = Last_Element
              or else Actual_List /= No_List
              or else Entity.Intrinsic
            then

               if Entity.Intrinsic then
                  declare
                     Arg_Count : constant Natural :=
                                   Natural
                                     (List_Table.Element (Actual_List)
                                      .List.Length);
                     Args      : Array_Of_Nodes (1 .. Arg_Count);
                     Index     : Natural := 0;

                     procedure Push (Arg : Positive);

                     ----------
                     -- Push --
                     ----------

                     procedure Push (Arg : Positive) is
                     begin
                        Generate_Expression (Unit, Context, Args (Arg));
                     end Push;

                  begin
                     for Arg of List_Table.Element (Actual_List).List loop
                        Index := Index + 1;
                        Args (Index) := Arg;
                     end loop;
                     pragma Assert (Index = Arg_Count);

                     for Item of Pending loop
                        Process (Item, Item = Last_Element);
                     end loop;

                     Ack.Generate.Intrinsics.Generate_Intrinsic
                       (Unit      => Unit,
                        Arg_Count => Args'Length,
                        Name      =>
                          To_Standard_String
                            (Ack.Features.Feature_Entity
                                 (Entity).External_Label),
                        Push      => Push'Access);
                  end;
               else

                  if Actual_List /= No_List then
                     Apply_Arguments (Actual_List);
                  end if;

                  for Item of Pending loop
                     Process (Item, Item = Last_Element);
                  end loop;
               end if;

               Pending.Clear;
            end if;
         end;
      end loop;

      declare
         use Ack.Features, Ack.Classes;
         Last_Entity : constant Constant_Entity_Type :=
                         Constant_Entity_Type
                           (Get_Entity (Last_Element));
         Current_Context : constant Constant_Class_Entity :=
                             Constant_Class_Entity
                               (Get_Context (Last_Element).Class_Context);
         Expanded        : constant Boolean :=
                             Previous_Context /= null
                                 and then Current_Context /= null
                                     and then Current_Context.Expanded;
         Has_Result      : constant Boolean :=
                             Ack.Features.Is_Feature (Last_Entity)
                               and then
                                 (Constant_Feature_Entity
                                    (Last_Entity).Has_Result
                                  or else Constant_Feature_Entity (Last_Entity)
                                  .Is_Property);
      begin
         --  Ada.Text_IO.Put_Line
         --    ("expanded no-result check"
         --     & ": "
         --     & (if Previous_Entity = null then ""
         --       else Previous_Entity.Link_Name & ",")
         --     & Last_Entity.Link_Name
         --     & "; expanded=" & (if Expanded then "yes" else "no")
         --     & "; update-expanded-value="
         --     & (if Current_Context.Update_Expanded_Value
         --       then "yes" else "no")
         --     & "; has-result="
         --     & (if Has_Result
         --       then "yes" else "no")
         --     & "; intrinsic="
         --     & (if Last_Entity.Intrinsic
         --       then "yes" else "no"));

         if Expanded
           and then Current_Context.Update_Expanded_Value
           and then not Has_Result
           and then not Last_Entity.Intrinsic
         then
            Unit.Push_Return (1);
            Previous_Entity.Pop_Entity
              (Get_Context (Precursor).Class_Context, Current_Context, Unit);
         end if;
      end;

   exception
      when others =>
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            Get_Program (Precursor).Show_Location
            & ": generate precursor failed");
         raise;

   end Generate_Precursor;

   --------------------
   -- Generate_Retry --
   --------------------

   procedure Generate_Retry
     (Unit  : in out Tagatha.Code.Instance'Class;
      Context : not null access constant Root_Entity_Type'Class;
      Retry   : Node_Id;
      Target  : Tagatha.Code.Label)
   is
      pragma Unreferenced (Context, Retry, Target);
   begin
      --  if Tagatha.Code.Has_Label (Target) then
      Unit.Retry_Routine;
      --  else
      --     raise Constraint_Error with
      --     Get_Program (Retry).Show_Location
      --       & ": expected to be in a rescue context";
      --  end if;
   end Generate_Retry;

   ------------------------
   -- Generate_Set_Value --
   ------------------------

   procedure Generate_Set_Value
     (Unit       : in out Tagatha.Code.Instance'Class;
      Context    : not null access constant Root_Entity_Type'Class;
      Value_Type : not null access constant Root_Entity_Type'Class;
      Node       : Node_Id)
   is
      pragma Unreferenced (Context);
      Entity : constant Entity_Type := Get_Entity (Node);
   begin
      if Value_Type.Standard_Name = "none" then
         Entity.Pop_Entity
           (Get_Context (Node).Class_Context,
            Entity.Get_Type.Class_Context, Unit);
      else
         Entity.Pop_Entity
           (Get_Context (Node).Class_Context,
            Value_Type, Unit);
      end if;

      Entity.Clear_Attached;

   exception
      when others =>
         Ada.Text_IO.Put_Line
           (Get_Program (Node).Show_Location
            & ": pop entity " & Entity.Qualified_Name & " failed");
         raise;
   end Generate_Set_Value;

   -------------------------------
   -- Generate_Tuple_Expression --
   -------------------------------

   procedure Generate_Tuple_Expression
     (Unit       : in out Tagatha.Code.Instance'Class;
      Context    : not null access constant Root_Entity_Type'Class;
      Expression : Node_Id)
   is
      Tuple_Type   : constant Entity_Type := Get_Type (Expression);
      Actual_Nodes : constant Array_Of_Nodes :=
                       To_Array
                         (Tuple_Expression_List (Expression));
      Arity_Image : constant String := Natural'Image (Actual_Nodes'Length);
      Make_Name    : constant String :=
                       Tuple_Type.Link_Name
                       & ".make_tuple"
                       & Arity_Image (2 .. Arity_Image'Last);
   begin
      for Arg of reverse Actual_Nodes loop
         Generate_Expression (Unit, Context, Arg);
      end loop;

      Unit.Call
        (Tuple_Type.Allocator_Name, 0, 1);
      Unit.Push_Return (1);
      Unit.Duplicate;
      Unit.Pop_Local
        (Tagatha.Local_Index (Context.Shelf ("tuple-expression")));

      Unit.Call (Make_Name, Actual_Nodes'Length + 1, 0);

      Unit.Push_Local
        (Tagatha.Local_Index (Context.Shelf ("tuple-expression")));

   end Generate_Tuple_Expression;

end Ack.Generate;
