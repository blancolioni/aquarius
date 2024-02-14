with Ack.Semantic.Work;

with Ack.Semantic.Analysis.Statements;
with Ack.Semantic.Analysis.Types;

with Ack.Types;

package body Ack.Semantic.Analysis.Features is

   --------------------------
   -- Analyse_Feature_Body --
   --------------------------

   procedure Analyse_Feature_Body
     (Class   : Ack.Classes.Class_Entity;
      Feature : Node_Id)
   is
      Names           : constant List_Id := New_Feature_List (Feature);
      Dec_Body        : constant Node_Id := Declaration_Body (Feature);
      Value_Node      : constant Node_Id := Value (Dec_Body);
      Value_Feature   : constant Boolean :=
                          Value_Node /= No_Node
                              and then Kind (Value_Node) = N_Explicit_Value;
      Routine_Feature : constant Boolean :=
                          Value_Node /= No_Node
                              and then Kind (Value_Node) = N_Routine;
      Deferred        : constant Boolean :=
                          Routine_Feature
                              and then Node_Table.Element
                                (Value_Node).Deferred;
      Effective       : constant Boolean :=
                          Routine_Feature and then not Deferred
                                and then Kind (Value_Node) = N_Routine;
      Precondition_Node : constant Node_Id :=
                            (if Routine_Feature
                             then Precondition (Value_Node)
                             else No_Node);
      Postcondition_Node : constant Node_Id :=
                             (if Routine_Feature
                              then Postcondition (Value_Node)
                              else No_Node);
      Rescue_Node        : constant Node_Id :=
                             (if Routine_Feature
                              then Rescue (Value_Node)
                              else No_Node);
      Locals_Node        : constant Node_Id :=
                             (if Routine_Feature
                              then Local_Declarations (Value_Node)
                              else No_Node);
      Effective_Node     : constant Node_Id :=
                             (if Effective then Effective_Routine (Value_Node)
                           else No_Node);
      Internal           : constant Boolean :=
                             Effective
                                 and then Kind (Effective_Node) = N_Internal;
      External           : constant Boolean :=
                          Effective
                                 and then Kind (Effective_Node) = N_External;

      Attachment         : Ack.Attachment.Attachment_Context;

   begin

      if not Property_Feature_Node (Feature)
        and then not Value_Feature
        and then not Routine_Feature
      then
         Error (Feature, E_Requires_Body);
      end if;

      for Node of List_Table.Element (Names).List loop
         declare
            Entity : constant Ack.Features.Feature_Entity :=
                       Ack.Features.Get_Feature_Entity (Node);
         begin

            if Locals_Node /= No_Node then
               Analyse_Entity_Declaration_Groups
                 (Class      => Class,
                  Feature    => Entity,
                  Group_List => Entity_Declaration_Group_List (Locals_Node),
                  Local      => True);
            end if;

            if Entity.Standard_Name = "void" then
               Entity.Set_Explicit_Value (No_Node);
            elsif Value_Feature then
               Entity.Set_Explicit_Value
                 (Constant_Value (Value_Node));
            elsif Deferred then
               null;  --  already analysed by feature header
            elsif Internal then
               Entity.Set_Routine (Effective_Node);
            elsif External then
               Entity.Set_External
                 (External_Type  =>
                    To_Standard_String (Get_Name (Effective_Node)),
                  External_Alias =>
                    (if Feature_Alias (Effective_Node) = No_Node
                     then Entity.Standard_Name
                     else To_String
                       (Get_Name (Feature_Alias (Effective_Node)))));
            end if;

            Entity.Bind;

            if Precondition_Node /= No_Node then
               declare
                  procedure Add_Precondition
                    (Tag        : Name_Id;
                     Expression : Node_Id);

                  ----------------------
                  -- Add_Precondition --
                  ----------------------

                  procedure Add_Precondition
                    (Tag        : Name_Id;
                     Expression : Node_Id)
                  is
                  begin
                     Entity.Add_Precondition (Tag, Expression);
                  end Add_Precondition;

               begin
                  Analyse_Assertion (Class, Entity, Attachment,
                                     Assertion (Precondition_Node),
                                     Add_Precondition'Access);
               end;
            end if;

            if Rescue_Node /= No_Node then
               declare
                  Rescue_Attachment : Ack.Attachment.Attachment_Context;
               begin
                  Statements.Analyse_Compound
                    (Class, Entity, Rescue_Attachment, Compound (Rescue_Node));
                  Entity.Set_Rescue_Node (Rescue_Node);
               end;
            end if;

            if Internal then
               for I in 1 .. Entity.Argument_Count loop
                  Attachment.Attach (Entity.Argument (I));
               end loop;

               Analyse_Effective_Routine
                 (Class, Entity, Attachment, Effective_Node);
            end if;

            if Postcondition_Node /= No_Node then
               declare
                  procedure Add_Postcondition
                    (Tag        : Name_Id;
                     Expression : Node_Id);

                  -----------------------
                  -- Add_Postcondition --
                  -----------------------

                  procedure Add_Postcondition
                    (Tag        : Name_Id;
                     Expression : Node_Id)
                  is
                  begin
                     Entity.Add_Postcondition (Tag, Expression);
                  end Add_Postcondition;

               begin
                  Analyse_Assertion (Class, Entity, Attachment,
                                     Assertion (Postcondition_Node),
                                     Add_Postcondition'Access);
               end;
            end if;

         end;
      end loop;
   end Analyse_Feature_Body;

   ----------------------------
   -- Analyse_Feature_Header --
   ----------------------------

   procedure Analyse_Feature_Header
     (Class   : Ack.Classes.Class_Entity;
      Feature : Node_Id)
   is
      Names        : constant List_Id := New_Feature_List (Feature);
      Dec_Body     : constant Node_Id := Declaration_Body (Feature);
      Arg_Node     : constant Node_Id := Formal_Arguments (Dec_Body);
      Type_Node    : constant Node_Id := Value_Type (Dec_Body);

      procedure Analyse_Ancestor_Feature
        (Ancestor_Class    : Ack.Classes.Class_Entity;
         Redefinition_Node : Node_Id;
         Ancestor_Feature  : Name_Id);

      ------------------------------
      -- Analyse_Ancestor_Feature --
      ------------------------------

      procedure Analyse_Ancestor_Feature
        (Ancestor_Class    : Ack.Classes.Class_Entity;
         Redefinition_Node : Node_Id;
         Ancestor_Feature  : Name_Id)
      is
         pragma Unreferenced (Redefinition_Node);
      begin
         Ack.Semantic.Work.Check_Work_Item
           (Class        => Ancestor_Class,
            Feature_Name => Ancestor_Feature,
            Category     => Ack.Semantic.Work.Feature_Header);
      end Analyse_Ancestor_Feature;

   begin
      if Type_Node /= No_Node then
         Types.Analyse_Type (Class, Type_Node);
      end if;

      for Node of List_Table.Element (Names).List loop
         if not Ack.Features.Has_Feature_Entity (Node) then
            raise Constraint_Error with
            Get_Program (Node).Show_Location
              & ": expected an entity in "
              & To_String (Get_Name (Feature_Name (Node)));
         end if;

         Class.Scan_Redefinitions
           (Get_Entity (Node).Entity_Name_Id,
            Analyse_Ancestor_Feature'Access);

         declare
            Entity    : constant Ack.Features.Feature_Entity :=
                       Ack.Features.Get_Feature_Entity (Node);
         begin
            if Arg_Node /= No_Node then
               Analyse_Entity_Declaration_Groups
                 (Class, Entity,
                  Entity_Declaration_Group_List (Arg_Node),
                  Local => False);
            end if;

            if Type_Node /= No_Node
              and then Has_Entity (Type_Node)
            then
               Entity.Set_Result_Type
                 (Ack.Types.Get_Type_Entity (Type_Node));
            end if;
         end;

      end loop;
   end Analyse_Feature_Header;

end Ack.Semantic.Analysis.Features;
