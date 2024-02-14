with Ack.Semantic.Analysis.Class_Names;
with Ack.Semantic.Tuples;
with Ack.Semantic.Types;

with Ack.Types;

package body Ack.Semantic.Analysis.Types is

   ---------------------------
   -- Analyse_Anchored_Type --
   ---------------------------

   procedure Analyse_Anchored_Type
     (Class     : Ack.Classes.Class_Entity;
      Type_Node : Node_Id)
   is
      Feature_Name : constant Name_Id :=
                       Get_Name (Type_Node);
      Type_Entity  : constant Ack.Types.Type_Entity :=
                       Ack.Types.New_Anchored_Type
                         (Type_Node, Class, Feature_Name);
   begin
      Set_Entity (Type_Node, Type_Entity);
   end Analyse_Anchored_Type;

   ------------------------
   -- Analyse_Class_Type --
   ------------------------

   procedure Analyse_Class_Type
     (Class     : Ack.Classes.Class_Entity;
      Type_Node : Node_Id)
   is
      use type Ack.Types.Type_Entity;
      Name_Node     : constant Node_Id := Class_Name (Type_Node);
      Generics_Node : constant Node_Id := Actual_Generics (Type_Node);
      Class_Entity  : Ack.Classes.Class_Entity := null;
      Type_Entity   : Ack.Types.Type_Entity := null;

   begin
      Class_Names.Analyse_Class_Name (Class, Name_Node, False);

      if not Has_Entity (Name_Node) then
         return;
      end if;

      if Ack.Types.Has_Type_Entity (Name_Node) then
         Type_Entity := Ack.Types.Get_Type_Entity (Name_Node);
      elsif Ack.Classes.Has_Class_Entity (Name_Node) then
         Class_Entity := Ack.Classes.Get_Class_Entity (Name_Node);

         if Generics_Node = No_Node then
            Type_Entity :=
              Ack.Types.New_Class_Type
                (Type_Node, Class_Entity,
                 Detachable => Node_Table.Element (Type_Node).Detachable);
         else
            declare
               Actual_Nodes : constant Array_Of_Nodes :=
                                To_Array
                                  (Actual_Generics_List (Generics_Node));
               Actual_Count : constant Natural :=
                                Actual_Nodes'Length;
               Actual_Types : Ack.Types.Array_Of_Types (Actual_Nodes'Range);
            begin
               if Actual_Count < Class_Entity.Generic_Formal_Count then
                  Error (Type_Node, E_Insufficient_Arguments);
               elsif Actual_Count > Class_Entity.Generic_Formal_Count then
                  Error (Type_Node, E_Too_Many_Arguments);
               else
                  for I in Actual_Nodes'Range loop
                     Analyse_Type (Class, Actual_Nodes (I));
                     Actual_Types (I) :=
                       Ack.Types.Get_Type_Entity (Actual_Nodes (I));
                  end loop;

                  Type_Entity :=
                    Ack.Types.Instantiate_Generic_Class
                      (Node            => Type_Node,
                       Generic_Class   => Class_Entity,
                       Generic_Actuals => Actual_Types,
                       Detachable      =>
                         Node_Table.Element (Type_Node).Detachable);
               end if;
            end;
         end if;
      end if;

      if Type_Entity /= null then
         Set_Entity (Type_Node, Type_Entity);
      else
         Set_Entity (Type_Node, Ack.Semantic.Types.Type_Any);
      end if;

   end Analyse_Class_Type;

   ------------------------
   -- Analyse_Tuple_Type --
   ------------------------

   procedure Analyse_Tuple_Type
     (Class     : Ack.Classes.Class_Entity;
      Type_Node : Node_Id)
   is
      use type Ack.Types.Type_Entity;
      Actual_Nodes : constant Array_Of_Nodes :=
                       To_Array
                         (Tuple_Argument_List (Type_Node));
      Actual_Count : constant Natural :=
                       Actual_Nodes'Length;
      Tuple_Arity  : constant Tuple_Arity_Range :=
                       Tuple_Arity_Range (Actual_Count);
      Actual_Types : Ack.Types.Array_Of_Types (Actual_Nodes'Range);
      Tuple_Class  : constant Ack.Classes.Class_Entity :=
        Ack.Semantic.Tuples.Tuple_Class (Tuple_Arity);
      Type_Entity   : Ack.Types.Type_Entity := null;
   begin
      for I in Actual_Nodes'Range loop
         Analyse_Type (Class, Actual_Nodes (I));
         Actual_Types (I) := Ack.Types.Get_Type_Entity (Actual_Nodes (I));
      end loop;

      Type_Entity :=
        Ack.Types.Instantiate_Generic_Class
          (Node            => Type_Node,
           Generic_Class   => Tuple_Class,
           Generic_Actuals => Actual_Types,
           Detachable      => False);

      if Type_Entity /= null then
         Set_Entity (Type_Node, Type_Entity);
      else
         Set_Entity (Type_Node, Ack.Semantic.Types.Type_Any);
      end if;

   end Analyse_Tuple_Type;

   ------------------
   -- Analyse_Type --
   ------------------

   procedure Analyse_Type
     (Class     : Ack.Classes.Class_Entity;
      Type_Node : Node_Id)
   is
   begin
      case N_Type (Kind (Type_Node)) is
         when N_Class_Type =>
            Analyse_Class_Type (Class, Type_Node);
         when N_Anchored_Type =>
            Analyse_Anchored_Type (Class, Type_Node);
         when N_Tuple_Type =>
            Analyse_Tuple_Type (Class, Type_Node);
      end case;
   end Analyse_Type;

end Ack.Semantic.Analysis.Types;
