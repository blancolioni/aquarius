with Ada.Text_IO;

with Ack.Semantic.Analysis;
with Ack.Semantic.Classes;

with Ack.Environment;

package body Ack.Semantic is

   -------------------------------
   -- Analyse_Class_Declaration --
   -------------------------------

   procedure Analyse_Class_Declaration
     (Node : Node_Id)
   is
   begin
      Analysis.Analyse_Class_Declaration (Node);
   end Analyse_Class_Declaration;

   ---------------
   -- Get_Class --
   ---------------

   function Get_Class
     (Qualified_Name : String)
      return Ack.Classes.Constant_Class_Entity
   is
      Q_Name : constant String := Qualified_Name & ".";
      Start  : Positive := Q_Name'First;
      Class  : Ack.Classes.Class_Entity;
   begin
      for I in Q_Name'Range loop
         if Q_Name (I) = '.' then
            declare
               use type Ack.Classes.Class_Entity;
               Name : constant String :=
                        Qualified_Name (Start .. I - 1);
            begin
               if Start = Q_Name'First then
                  Class :=
                    Classes.Load_Class (null, Ack.Environment.Top_Level,
                                        Get_Name_Id (Name));
                  if Class = null then
                     Ada.Text_IO.Put_Line
                       (Ada.Text_IO.Standard_Error,
                        "failed to load top level class '"
                        & Name & "'");
                     return null;
                  end if;
               elsif Class.Contains (Name) then
                  Class :=
                    Ack.Classes.Class_Entity
                      (Class.Get (Name));
               else
                  Class :=
                    Classes.Load_Class (null, Class, Get_Name_Id (Name));
                  if Class = null then
                     Ada.Text_IO.Put_Line
                       (Ada.Text_IO.Standard_Error,
                        "failed to load child class '"
                        & Name & "' in " & Qualified_Name);
                     return null;
                  end if;

               end if;
               Start := I + 1;
            exception
               when others =>
                  Ada.Text_IO.Put_Line
                    ("failed to get classes for '" & Qualified_Name & "'");
                  raise;
            end;
         end if;
      end loop;
      return Ack.Classes.Constant_Class_Entity (Class);
   end Get_Class;

   ---------------------------
   -- Property_Feature_Node --
   ---------------------------

   function Property_Feature_Node
     (Node : Node_Id)
      return Boolean
   is
      Dec_Body        : constant Node_Id := Declaration_Body (Node);
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
      Arg_Node        : constant Node_Id := Formal_Arguments (Dec_Body);
      Type_Node       : constant Node_Id := Value_Type (Dec_Body);
   begin
      return not Deferred
        and then not Routine_Feature
        and then not Value_Feature
        and then Arg_Node = No_Node and then Type_Node /= No_Node;
   end Property_Feature_Node;

end Ack.Semantic;
