with Ada.Exceptions;
with Ada.Text_IO;

with Ack.Variables;

with Ack.Semantic.Classes;
with Ack.Semantic.Work;

with Ack.Semantic.Analysis.Expressions;
with Ack.Semantic.Analysis.Types;

with Ack.Types;

package body Ack.Semantic.Analysis.Statements is

   Local_Iterable_Type         : Ack.Types.Type_Entity := null;
   Local_Iterable_Class        : Ack.Classes.Class_Entity := null;

   function Class_Iterable
     return Ack.Classes.Class_Entity;

   function Type_Iterable
     return Ack.Types.Type_Entity;

   ------------------------
   -- Analyse_Assignment --
   ------------------------

   procedure Analyse_Assignment
     (Class      : Ack.Classes.Class_Entity;
      Container  : not null access Root_Entity_Type'Class;
      Attachment : in out Ack.Attachment.Attachment_Context'Class;
      Assignment : Node_Id)
   is
      Target : constant String :=
                 To_Standard_String (Get_Name (Variable (Assignment)));
   begin
      if Container.Contains (Target) then
         declare
            Entity : constant Ack.Entity_Type :=
                       Container.Get (Target);
         begin
            if Ack.Features.Is_Feature (Entity) then
               Ack.Semantic.Work.Check_Work_Item
                 (Class        =>
                    Ack.Features.Feature_Entity (Entity).Definition_Class,
                  Feature_Name => Get_Name (Variable (Assignment)),
                  Category     => Ack.Semantic.Work.Feature_Header);
            end if;

            Ack.Semantic.Analysis.Expressions.Analyse_Expression
              (Class, Container, Attachment,
               Entity.Get_Type,
               Expression (Assignment));
            Set_Entity (Variable (Assignment), Entity);
            Set_Context (Variable (Assignment), Container);
            Attachment.Transfer_Current_Context_Attachment (Entity);
         end;
      else
         Error (Variable (Assignment), E_Undeclared_Name);
      end if;

   end Analyse_Assignment;

   ----------------------
   -- Analyse_Compound --
   ----------------------

   procedure Analyse_Compound
     (Class      : Ack.Classes.Class_Entity;
      Container  : not null access Root_Entity_Type'Class;
      Attachment : in out Ack.Attachment.Attachment_Context'Class;
      Compound   : Node_Id)
   is
      List : constant List_Id := Instructions (Compound);

      procedure Analyse (Node : Node_Id);

      -------------
      -- Analyse --
      -------------

      procedure Analyse (Node : Node_Id) is
      begin
         case N_Instruction (Kind (Node)) is
            when N_Assignment =>
               Analyse_Assignment (Class, Container, Attachment, Node);
            when N_Creation_Instruction =>
               Analyse_Creation (Class, Container, Attachment, Node);
            when N_Conditional =>
               Analyse_Conditional (Class, Container, Attachment, Node);
            when N_Loop =>
               Analyse_Loop (Class, Container, Attachment, Node);
            when N_Precursor =>
               Ack.Semantic.Analysis.Expressions.Analyse_Precursor
                 (Class           => Class,
                  Container       => Container,
                  Attachment      => Attachment,
                  Expression_Type => null,
                  Precursor       => Node);
            when N_Check =>
               Analyse_Check (Class, Container, Attachment, Node);
            when N_Retry =>
               Analyse_Retry (Class, Container, Attachment, Node);
         end case;
      exception
         when E : others =>
            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Standard_Error,
               Get_Program (Node).Show_Location
               & ": "
               & Ada.Exceptions.Exception_Message (E));
            raise;
      end Analyse;

   begin
      Scan (List, Analyse'Access);
   end Analyse_Compound;

   -------------------------
   -- Analyse_Conditional --
   -------------------------

   procedure Analyse_Conditional
     (Class       : Ack.Classes.Class_Entity;
      Container   : not null access Root_Entity_Type'Class;
      Attachment  : in out Ack.Attachment.Attachment_Context'Class;
      Conditional : Node_Id)
   is
      procedure Analyse_Element (Element : Node_Id);

      ---------------------
      -- Analyse_Element --
      ---------------------

      procedure Analyse_Element (Element : Node_Id) is
         Condition : constant Node_Id := Field_1 (Element);
         Compound  : constant Node_Id := Field_2 (Element);
      begin
         if Condition /= No_Node then
            Ack.Semantic.Analysis.Expressions.Analyse_Boolean_Expression
              (Class, Container, Attachment,
               Condition);
            if Implicit_Entity (Condition) then
               Container.Add_Implicit (Get_Entity (Condition));
            end if;
         end if;
         Analyse_Compound (Class, Container, Attachment, Compound);
         if Condition /= No_Node and then Implicit_Entity (Condition) then
            Container.Remove_Implicit;
         end if;
      end Analyse_Element;

   begin
      Scan (Node_Table.Element (Conditional).List,
            Analyse_Element'Access);
   end Analyse_Conditional;

   ----------------------
   -- Analyse_Creation --
   ----------------------

   procedure Analyse_Creation
     (Class      : Ack.Classes.Class_Entity;
      Container  : not null access Root_Entity_Type'Class;
      Attachment : in out Ack.Attachment.Attachment_Context'Class;
      Creation   : Node_Id)
   is
      Explicit_Type_Node : constant Node_Id :=
                             Explicit_Creation_Type (Creation);
      Call_Node          : constant Node_Id := Creation_Call (Creation);
      Explicit_Call_Node : constant Node_Id :=
                             Explicit_Creation_Call (Call_Node);
      Actual_List_Node   : constant Node_Id :=
                             (if Explicit_Call_Node = No_Node then No_Node
                              else Actual_List (Explicit_Call_Node));
      Variable_Node      : constant Node_Id := Variable (Call_Node);
      Name               : constant Name_Id := Get_Name (Variable_Node);
      Created_Entity     : constant Entity_Type :=
                             (if Container.Contains (Name)
                              then Container.Get (Name) else null);
      Created_Type       : Entity_Type;
      Creator_Name       : Name_Id;
   begin

      if Created_Entity = null then
         Error (Variable_Node, E_Undeclared_Name);
         return;
      end if;

      if Ack.Features.Is_Feature (Created_Entity) then
         Ack.Semantic.Work.Check_Work_Item
           (Ack.Features.Feature_Entity (Created_Entity).Active_Class,
            Created_Entity.Entity_Name_Id,
            Ack.Semantic.Work.Feature_Header);
      end if;

      Set_Context (Creation, Container);
      Set_Entity (Creation, Created_Entity);

      Created_Type := Created_Entity.Get_Type;

      if Created_Type = null then
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            Get_Program (Variable_Node).Show_Location
            & ": no type for entity '"
            & Created_Entity.Declared_Name
            & "'");
      end if;

      if Explicit_Type_Node in Real_Node_Id then
         Ack.Semantic.Analysis.Types.Analyse_Type (Class, Explicit_Type_Node);
         if Get_Entity (Explicit_Type_Node) /= null then
            Created_Type := Get_Entity (Explicit_Type_Node);
            if Created_Type.Deferred then
               Error (Explicit_Type_Node, E_Create_Deferred_Class);
            end if;
         end if;
      elsif Created_Type.Deferred then
         Error (Creation, E_Create_Deferred_Class);
      end if;

      if not Created_Type.Conforms_To (Created_Entity.Get_Type) then
         Error (Creation, E_Creation_Type_Error, Created_Type);
      end if;

      if Explicit_Call_Node not in Real_Node_Id then
         Ack.Semantic.Work.Check_Work_Item
           (Ack.Types.Type_Entity (Created_Type).Class,
            No_Name, Ack.Semantic.Work.Class_Binding);

         if not Created_Type.Has_Default_Creation_Routine then
            Error (Creation, E_No_Default_Create_Routine);
         end if;
      end if;

      if Explicit_Call_Node in Real_Node_Id then

         Creator_Name := Get_Name (Explicit_Call_Node);

         Ack.Semantic.Work.Check_Work_Item
           (Ack.Types.Type_Entity (Created_Type).Class,
            Creator_Name, Ack.Semantic.Work.Feature_Header);

         if Created_Type.Contains (Creator_Name) then
            declare
               Creator : constant Entity_Type :=
                           Created_Type.Get (Creator_Name);
            begin
               Set_Entity (Explicit_Call_Node, Creator);
               if Creator.all not in Ack.Features.Feature_Entity_Record'Class
                 or else not Ack.Features.Feature_Entity (Creator).Is_Creator
               then
                  Error (Explicit_Call_Node, E_Not_A_Create_Feature,
                         Created_Type);
               else
                  if Actual_List_Node /= No_Node then
                     declare
                        Actuals : constant Array_Of_Nodes :=
                                    To_Array
                                      (Node_Table.Element
                                         (Actual_List_Node).List);
                     begin
                        if Creator.Argument_Count = 0 then
                           Error (Actual_List_Node,
                                  E_Does_Not_Accept_Arguments,
                                  Creator);
                        elsif Actuals'Length > Creator.Argument_Count then
                           Error (Actual_List_Node, E_Too_Many_Arguments);
                        elsif Actuals'Length < Creator.Argument_Count then
                           Error (Actual_List_Node, E_Insufficient_Arguments);
                        else
                           for I in Actuals'Range loop
                              Attachment.Save_State;
                              Ack.Semantic.Analysis.Expressions
                                .Analyse_Expression
                                  (Class           => Class,
                                   Container       => Container,
                                   Attachment      => Attachment,
                                   Expression_Type =>
                                     Creator.Argument (I).Get_Type,
                                   Expression      => Actuals (I));
                              Set_Destination_Type
                                (Actuals (I), Creator.Argument (I).Get_Type);
                              Attachment.Restore_State;
                           end loop;
                        end if;

                        Attachment.Detach_Current_Context;

                     end;
                  end if;
               end if;
            end;
         else
            Error (Explicit_Call_Node, E_Not_Defined_In,
                   Created_Type);
         end if;
      end if;

      Attachment.Attach (Created_Entity);

   end Analyse_Creation;

   ------------------
   -- Analyse_Loop --
   ------------------

   procedure Analyse_Loop
     (Class      : Ack.Classes.Class_Entity;
      Container  : not null access Root_Entity_Type'Class;
      Attachment : in out Ack.Attachment.Attachment_Context'Class;
      Loop_Node  : Node_Id)
   is
      Iteration_Node      : constant Node_Id := Loop_Iteration (Loop_Node);
      Initialization_Node : constant Node_Id :=
                              Loop_Initialization (Loop_Node);
      Exit_Condition_Node : constant Node_Id :=
                              Loop_Exit_Condition (Loop_Node);
      Loop_Body_Node      : constant Node_Id := Loop_Body (Loop_Node);
   begin

      if Iteration_Node /= No_Node then
         declare
            use type Ack.Types.Type_Entity;
            Expression_Node : constant Node_Id := Expression (Iteration_Node);
            Expression_Type : constant Ack.Types.Type_Entity :=
                                Type_Iterable;
            Iterable_Type   : Ack.Types.Type_Entity;
         begin

            Ack.Semantic.Analysis.Expressions.Analyse_Expression
              (Class, Container, Attachment,
               Expression_Type, Expression_Node);

            Iterable_Type :=
              Ack.Types.Type_Entity
                (Get_Type (Expression_Node));

            if Iterable_Type = null then
               return;
            end if;

            declare
               use Ack.Types;
               Inherited_Type           : constant access constant
                 Type_Entity_Record'Class :=
                   Iterable_Type.Get_Ancestor_Type
                     (Class_Iterable);
            begin
               if Inherited_Type = null then
                  Error (Iteration_Node, E_Iterator_Type_Error,
                         Class_Iterable);
                  return;
               end if;
            end;

            Ack.Semantic.Work.Check_Work_Item
              (Class        => Iterable_Type.Class,
               Feature_Name => Get_Name_Id ("new_cursor"),
               Category     => Ack.Semantic.Work.Feature_Header);

            declare
               use Ack.Types;

               Inherited_Type           : constant access constant
                 Type_Entity_Record'Class :=
                                      Iterable_Type.Get_Ancestor_Type
                                        (Class_Iterable);
               New_Cursor_Feature : constant Ack.Features.Feature_Entity :=
                                      Iterable_Type.Feature
                                        (Get_Name_Id ("new_cursor"));
               Iterator_Type      : constant Ack.Types.Type_Entity :=
                                      Ack.Types.Type_Entity
                                        (New_Cursor_Feature.Get_Type);
               Generic_Bindings   : constant Natural :=
                                      Inherited_Type.Generic_Binding_Count;
               pragma Assert (Generic_Bindings = 1);
               Implicit_Type      : constant Ack.Types.Type_Entity :=
                                      Inherited_Type.Generic_Binding (1);
               Implicit           : constant Ack.Variables.Variable_Entity :=
                                      Ack.Variables.New_Iterator_Entity
                                        (Name       =>
                                           Get_Name (Iteration_Node),
                                         Node       => Iteration_Node,
                                         Iteration_Type =>
                                           Iterator_Type,
                                         Local_Type     => Implicit_Type);
            begin
               Implicit.Set_Attached;
               Set_Entity (Iteration_Node, Implicit);
               Set_Context (Iteration_Node, Container);
               Set_Implicit_Entity (Iteration_Node);
               Container.Add_Implicit (Implicit);
            end;
         end;
      else
         if Exit_Condition_Node = No_Node then
            Error (Loop_Node, E_Missing_Exit_Condition);
         end if;
      end if;

      if Initialization_Node /= No_Node then
         Analyse_Compound (Class, Container, Attachment,
                           Compound (Initialization_Node));
      end if;

      if Exit_Condition_Node /= No_Node then
         Ack.Semantic.Analysis.Expressions.Analyse_Boolean_Expression
           (Class, Container, Attachment,
            Expression (Exit_Condition_Node));
      end if;

      Analyse_Compound (Class, Container, Attachment,
                        Compound (Loop_Body_Node));

      if Iteration_Node /= No_Node then
         Container.Remove_Implicit;
      end if;

   end Analyse_Loop;

   -------------------
   -- Analyse_Retry --
   -------------------

   procedure Analyse_Retry
     (Class      : Ack.Classes.Class_Entity;
      Container  : not null access Root_Entity_Type'Class;
      Attachment : in out Ack.Attachment.Attachment_Context'Class;
      Retry      : Node_Id)
   is
   begin
      null;
   end Analyse_Retry;

   --------------------
   -- Class_Iterable --
   --------------------

   function Class_Iterable
      return Ack.Classes.Class_Entity
   is
      use type Ack.Classes.Class_Entity;
   begin
      if Local_Iterable_Class = null then
         declare
            Aqua_Entity     : constant Ack.Classes.Class_Entity :=
                                Ack.Classes.Get_Top_Level_Class ("aqua");
            Iterable_Name   : constant String := "iterable";
         begin
            if Aqua_Entity.Contains (Iterable_Name) then
               Local_Iterable_Class :=
                 Ack.Classes.Class_Entity
                   (Aqua_Entity.Get (Iterable_Name));
            else
               Local_Iterable_Class :=
                 Ack.Semantic.Classes.Load_Class
                   (null, Aqua_Entity,
                    Get_Name_Id (Iterable_Name));
            end if;
         end;
      end if;

      return Local_Iterable_Class;
   end Class_Iterable;

   -------------------
   -- Type_Iterable --
   -------------------

   function Type_Iterable
      return Ack.Types.Type_Entity
   is
      use type Ack.Types.Type_Entity;
   begin
      if Local_Iterable_Type = null then
         Local_Iterable_Type :=
           Ack.Types.New_Class_Type (No_Node, Class_Iterable, False);
      end if;

      return Local_Iterable_Type;
   end Type_Iterable;

end Ack.Semantic.Analysis.Statements;
