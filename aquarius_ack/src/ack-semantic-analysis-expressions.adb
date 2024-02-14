with Ack.Semantic.Tuples;
with Ack.Semantic.Types;
with Ack.Semantic.Work;

with Ack.Types;
with Ack.Variables;

package body Ack.Semantic.Analysis.Expressions is

   ------------------------------
   -- Analyse_Actual_Arguments --
   ------------------------------

   procedure Analyse_Actual_Arguments
     (Class            : Ack.Classes.Class_Entity;
      Container        : not null access Root_Entity_Type'Class;
      Attachment       : in out Ack.Attachment.Attachment_Context'Class;
      Entity           : Entity_Type;
      Actual_List_Node : Node_Id)
   is
   begin
      if Actual_List_Node /= No_Node then
         declare
            Actuals : constant Array_Of_Nodes :=
                        To_Array
                          (Node_Table.Element
                             (Actual_List_Node).List);
         begin
            if Entity.Argument_Count = 0 then
               Error (Actual_List_Node, E_Does_Not_Accept_Arguments, Entity);
            elsif Actuals'Length > Entity.Argument_Count then
               Error (Actual_List_Node, E_Too_Many_Arguments);
            elsif Actuals'Length < Entity.Argument_Count then
               Error (Actual_List_Node, E_Insufficient_Arguments);
            else
               for I in Actuals'Range loop
                  Analyse_Expression
                    (Class           => Class,
                     Container       => Container,
                     Attachment      => Attachment,
                     Expression_Type => Entity.Argument (I).Get_Type,
                     Expression      => Actuals (I));
               end loop;
            end if;
         end;
      end if;
   end Analyse_Actual_Arguments;

   --------------------------------
   -- Analyse_Boolean_Expression --
   --------------------------------

   procedure Analyse_Boolean_Expression
     (Class           : Ack.Classes.Class_Entity;
      Container       : not null access Root_Entity_Type'Class;
      Attachment      : in out Ack.Attachment.Attachment_Context'Class;
      Expression      : Node_Id)
   is
   begin
      Analyse_Expression
        (Class, Container, Attachment,
         Types.Type_Boolean,
         Expression);
   end Analyse_Boolean_Expression;

   ------------------------
   -- Analyse_Expression --
   ------------------------

   procedure Analyse_Expression
     (Class           : Ack.Classes.Class_Entity;
      Container       : not null access Root_Entity_Type'Class;
      Attachment      : in out Ack.Attachment.Attachment_Context'Class;
      Expression_Type : not null access Root_Entity_Type'Class;
      Expression      : Node_Id)
   is
      K : constant Node_Kind := Kind (Expression);
   begin
      case N_Expression_Node (K) is
         when N_Operator =>
            Analyse_Operator
              (Class, Container, Attachment, Expression_Type, Expression);
         when N_Precursor =>
            Analyse_Precursor
              (Class, Container, Attachment, Expression_Type, Expression);
         when N_Old =>
            Analyse_Expression (Class, Container, Attachment,
                                Expression_Type,
                                Ack.Expression (Expression));
            Container.Save_Old_Value (Ack.Expression (Expression));
         when N_Tuple =>
            Analyse_Tuple_Expression
              (Class, Container, Attachment, Expression_Type, Expression);
         when N_Attachment_Test =>
            Analyse_Expression (Class, Container, Attachment,
                                Types.Type_Any,
                                Expression => Field_1 (Expression));
            if Get_Name (Expression) /= No_Name
              and then Get_Type (Field_1 (Expression)) /= null
            then
               declare
                  Implicit : constant Ack.Variables.Variable_Entity :=
                               Ack.Variables.New_Local_Entity
                                 (Name       => Get_Name (Expression),
                                  Node       => Expression,
                                  Local_Type =>
                                    Get_Type (Field_1 (Expression)));
               begin
                  Implicit.Set_Attached;
                  Attachment.Attach (Implicit);
                  Set_Type (Expression,
                            Ack.Types.Get_Top_Level_Type ("boolean"));
                  Set_Entity (Expression, Implicit);
                  Set_Implicit_Entity (Expression);
               end;
            end if;

         when N_Constant =>
            declare
               Value     : constant Node_Id := Constant_Value (Expression);
               Value_Type : constant Ack.Types.Type_Entity :=
                              (case N_Constant_Value (Kind (Value)) is
                                  when N_String_Constant  =>
                                     Types.Type_String,
                                  when N_Character_Constant =>
                                     Types.Type_Character,
                                  when N_Integer_Constant =>
                                     Types.Type_Integral (Value),
                                  when N_Boolean_Constant =>
                                     Types.Type_Boolean);
            begin
               if Kind (Value) = N_Integer_Constant then
                  Set_Type (Expression, Expression_Type);
                  Set_Entity (Expression, Expression_Type);
                  if not Expression_Type.Conforms_To (Value_Type) then
                     Error (Expression, E_Type_Error, Value_Type);
                  end if;
               else
                  Set_Type (Expression, Value_Type);
                  Set_Entity (Expression, Value_Type);
                  if not  Value_Type.Conforms_To (Expression_Type) then
                     Error (Expression, E_Type_Error,
                            Entity_Type (Expression_Type));
                  end if;
               end if;
            end;
      end case;

   end Analyse_Expression;

   ----------------------
   -- Analyse_Operator --
   ----------------------

   procedure Analyse_Operator
     (Class           : Ack.Classes.Class_Entity;
      Container       : not null access Root_Entity_Type'Class;
      Attachment      : in out Ack.Attachment.Attachment_Context'Class;
      Expression_Type : access Root_Entity_Type'Class;
      Operator_Node   : Node_Id)
   is
      use type Ack.Types.Type_Entity;
      Operator  : constant Name_Id := Get_Name (Operator_Node);
      Left      : constant Node_Id := Field_1 (Operator_Node);
      Right     : constant Node_Id := Field_2 (Operator_Node);
      Left_Type : Ack.Types.Type_Entity;
   begin
      Analyse_Expression
        (Class           => Class,
         Container       => Container,
         Attachment      => Attachment,
         Expression_Type => Types.Type_Any,
         Expression      => Left);
      Left_Type := Ack.Types.Type_Entity (Get_Type (Left));

      if Left_Type /= null
        and then not Left_Type.Is_Generic_Formal_Type
      then
         Ack.Semantic.Work.Check_Work_Item
           (Left_Type.Class, No_Name, Ack.Semantic.Work.Class_Binding);
      end if;

      if Left_Type = null then
         null;
      elsif not Left_Type.Has_Aliased_Feature (Operator, Right /= No_Node) then
         Error (Operator_Node, E_Undeclared_Name);
      else
         declare
            Feature : constant Ack.Features.Feature_Entity :=
                        Left_Type.Aliased_Feature
                          (Alias => Operator,
                           Infix => Right /= No_Node);
         begin

            Ack.Semantic.Work.Check_Work_Item
              (Feature.Definition_Class, Feature.Entity_Name_Id,
               Ack.Semantic.Work.Feature_Header);

            pragma Assert (Feature.Argument_Count in 0 .. 1);
            if not Feature.Has_Type then
               raise Constraint_Error with
                 "attempted to treat non-value feature "
                 & Feature.Qualified_Name
                 & " as an operator";
            end if;

            if Feature.Argument_Count = 0 then
               if Right /= No_Node then
                  Error (Right, E_Too_Many_Arguments);
               end if;
            else
               if Right = No_Node then
                  Error (Operator_Node, E_Insufficient_Arguments);
               else
                  declare
                     Argument_Type : constant Ack.Types.Type_Entity :=
                                       Ack.Types.Type_Entity
                                         (Feature.Argument (1).Get_Type);
                     Expected_Type : constant Ack.Types.Type_Entity :=
                                       Ack.Types.Get_Concrete_Type
                                         (Of_Type => Argument_Type,
                                          Current => Left_Type,
                                          Feature => Feature);
                  begin
                     Analyse_Expression
                       (Class, Container, Attachment, Expected_Type, Right);
                  end;
               end if;
            end if;

            declare
               Result_Type : constant Ack.Types.Type_Entity :=
                               Ack.Types.Get_Concrete_Type
                                 (Of_Type => Ack.Types.Type_Entity
                                    (Feature.Get_Type),
                                  Current => Left_Type,
                                  Feature => Feature);
            begin
               Set_Type (Operator_Node, Result_Type);
               Set_Entity (Operator_Node, Feature);
               if not Result_Type.Conforms_To (Expression_Type) then
                  Error (Operator_Node, E_Type_Error,
                         Entity_Type (Expression_Type));
               end if;
            end;
         end;
      end if;

   end Analyse_Operator;

   -----------------------
   -- Analyse_Precursor --
   -----------------------

   procedure Analyse_Precursor
     (Class           : Ack.Classes.Class_Entity;
      Container       : not null access Root_Entity_Type'Class;
      Attachment      : in out Ack.Attachment.Attachment_Context'Class;
      Expression_Type : access Root_Entity_Type'Class;
      Precursor       : Node_Id)
   is
      List   : constant List_Id :=
                 Node_Table (Precursor).List;

      Local_Table  : Entity_Type := Entity_Type (Container);
      Value_Entity : Entity_Type := null;
      Value_Type   : Entity_Type := null;

      Stop         : Boolean := False;

      procedure Process (Precursor_Element : Node_Id);

      -------------
      -- Process --
      -------------

      procedure Process (Precursor_Element : Node_Id) is
         Name : constant Name_Id := Get_Name (Precursor_Element);
      begin
         if Stop then
            return;
         end if;

         if Local_Table = null then
            Error (Precursor_Element, E_No_Component);
            Stop := True;
            return;
         end if;

         Local_Table.Check_Bound;

         if not Local_Table.Contains (Name) then
            Error (Precursor_Element, E_Undeclared_Name, Local_Table);
            Stop := True;
            return;
         end if;

         declare
            Entity : constant Entity_Type := Local_Table.Get (Name);
         begin
            if Ack.Features.Is_Feature (Entity) then
               Ack.Semantic.Work.Check_Work_Item
                 (Ack.Classes.Constant_Class_Entity
                    (Local_Table.Class_Context),
                  Name,
                  Ack.Semantic.Work.Feature_Header);
            end if;
         end;

         declare
            use type Ack.Types.Constant_Type_Entity;
            Entity           : constant Entity_Type :=
                                 Local_Table.Get (Name);
            E_Type           : constant Ack.Types.Constant_Type_Entity :=
                                 Ack.Types.Constant_Type_Entity
                                   (Entity.Get_Type);
            Actual_List_Node : constant Node_Id :=
                                 Actual_List (Precursor_Element);
         begin

            Node_Table (Precursor_Element).Attached :=
              Attachment.Is_Attached (Entity);

            if Local_Warn_No_Default_Create
              and then E_Type /= null
              and then Entity.Standard_Name /= "void"
              and then not E_Type.Expanded
              and then Entity.Can_Update
              and then not Entity.Attached
              and then not E_Type.Detachable
              and then not E_Type.Deferred
              and then not E_Type.Is_Generic_Formal_Type
            then
               if not Attachment.Is_Attached (Entity) then
                  if not E_Type.Has_Default_Creation_Routine then
                     Warning (Precursor_Element,
                              E_Value_Might_Be_Void,
                              Entity);
                  end if;
               end if;
            end if;

            if Actual_List_Node /= No_Node then
               declare
                  Actuals : constant Array_Of_Nodes :=
                              To_Array
                                (Node_Table.Element
                                   (Actual_List_Node).List);
               begin
                  if Entity.Argument_Count = 0 then
                     Error (Actual_List_Node, E_Does_Not_Accept_Arguments,
                            Entity);
                  elsif Actuals'Length > Entity.Argument_Count then
                     Error (Actual_List_Node, E_Too_Many_Arguments);
                  elsif Actuals'Length < Entity.Argument_Count then
                     Error (Actual_List_Node, E_Insufficient_Arguments);
                  else
                     for I in Actuals'Range loop
                        Attachment.Save_State;
                        Analyse_Expression
                          (Class           => Class,
                           Container       => Container,
                           Attachment      => Attachment,
                           Expression_Type => Entity.Argument (I).Get_Type,
                           Expression      => Actuals (I));
                        Set_Destination_Type
                          (Actuals (I), Entity.Argument (I).Get_Type);
                        Attachment.Restore_State;
                     end loop;
                  end if;

                  Attachment.Detach_Current_Context;

               end;

            else
               Attachment.Set_Current_Context_Attachment (Entity);
            end if;

            Set_Entity (Precursor_Element, Entity);
            Set_Context (Precursor_Element, Local_Table.Class_Context);

            Value_Entity := Entity;
            Value_Type := Value_Entity.Get_Type;
            Local_Table := Value_Type;
         end;

      exception
         when others =>
            raise Constraint_Error with
            Get_Program (Precursor_Element).Show_Location
              & ": exception while processing precursor";

      end Process;

   begin
      Scan (List, Process'Access);

      if not Stop then
         if Value_Entity /= null then
            Set_Entity (Precursor, Value_Entity);

            if Expression_Type = null then
               if Value_Type /= null then
                  Error (Precursor, E_Ignored_Return_Value);
               end if;
            elsif Value_Type /= null
              and then not Value_Type.Conforms_To (Expression_Type)
            then
               Error (Precursor, E_Type_Error,
                      Entity_Type (Expression_Type));
            end if;
         end if;

         if Value_Type /= null then
            Set_Type (Precursor, Value_Type);
         end if;

      end if;

      Set_Context (Precursor, Container);

   end Analyse_Precursor;

   ------------------------------
   -- Analyse_Tuple_Expression --
   ------------------------------

   procedure Analyse_Tuple_Expression
     (Class           : Ack.Classes.Class_Entity;
      Container       : not null access Root_Entity_Type'Class;
      Attachment      : in out Ack.Attachment.Attachment_Context'Class;
      Expression_Type : access Root_Entity_Type'Class;
      Expression      : Node_Id)
   is
      use type Ack.Classes.Class_Entity;
      use type Ack.Types.Type_Entity;
      Expr_Nodes : constant Array_Of_Nodes :=
                     To_Array
                       (Tuple_Expression_List (Expression));
      Expr_Count : constant Natural :=
                     Expr_Nodes'Length;
      Expr_Type    : constant Ack.Types.Type_Entity :=
                       Ack.Types.Type_Entity (Expression_Type);
      Tuple_Arity  : constant Tuple_Arity_Range :=
                       Tuple_Arity_Range (Expr_Count);
      Expr_Type_Entity : constant Ack.Types.Type_Entity :=
                           Ack.Types.Type_Entity (Expression_Type);
      Actual_Types : Ack.Types.Array_Of_Types (Expr_Nodes'Range);
      Tuple_Class  : constant Ack.Classes.Class_Entity :=
        Tuples.Tuple_Class (Tuple_Arity);
      Type_Entity   : Ack.Types.Type_Entity := null;
   begin

      Container.Add_Shelf ("tuple-expression");

      if Expr_Type.Class /= null
        and then not Expr_Type.Class.Conforms_To (Tuple_Class)
      then
         Error (Expression, E_Type_Error, Tuple_Class);
         return;
      end if;

      if Expr_Type_Entity.Generic_Binding_Count = 0 then
         Error (Expression, E_Type_Error, Expr_Type_Entity);
         return;
      end if;

      if Expr_Type_Entity.Generic_Binding_Count /= Expr_Nodes'Length then
         Error (Expression, E_Type_Error, Expr_Type_Entity);
         return;
      end if;

      for I in Expr_Nodes'Range loop
         Analyse_Expression
           (Class, Container, Attachment,
            Expr_Type_Entity.Generic_Binding (I), Expr_Nodes (I));
         Actual_Types (I) :=
           Ack.Types.Type_Entity (Get_Type (Expr_Nodes (I)));
      end loop;

      Type_Entity :=
        Ack.Types.Instantiate_Generic_Class
          (Node            => Expression,
           Generic_Class   => Tuple_Class,
           Generic_Actuals => Actual_Types,
           Detachable      => False);

      if Type_Entity /= null then
         Set_Type (Expression, Type_Entity);
      end if;

      Attachment.Attach_Current_Context;

   end Analyse_Tuple_Expression;

end Ack.Semantic.Analysis.Expressions;
