with Ack.Environment;
with Ack.Semantic.Classes;

package body Ack.Semantic.Types is

   Local_Integral_Type         : Ack.Types.Type_Entity := null;

   function Get_Top_Level_Type
     (Name : String)
      return Ack.Types.Type_Entity;

   function Type_Any return Ack.Types.Type_Entity
   is (Get_Top_Level_Type ("any"));

   function Type_Boolean return Ack.Types.Type_Entity
   is (Get_Top_Level_Type ("boolean"));

   function Type_Character return Ack.Types.Type_Entity
   is (Get_Top_Level_Type ("character"));

   function Type_String return Ack.Types.Type_Entity
   is (Get_Top_Level_Type ("string"));

   ------------------------
   -- Get_Top_Level_Type --
   ------------------------

   function Get_Top_Level_Type
     (Name : String)
      return Ack.Types.Type_Entity
   is
      use type Ack.Classes.Class_Entity;
      Class : Ack.Classes.Class_Entity :=
                Ack.Classes.Get_Top_Level_Class (Name);
   begin
      if Class = null then
         Class := Classes.Load_Class (null, Ack.Environment.Top_Level,
                              Get_Name_Id (Name));
      end if;

      if Class = null then
         raise Constraint_Error with
           "unknown top-level class: " & Name;
      end if;

      return Ack.Types.Get_Top_Level_Type (Name);
   end Get_Top_Level_Type;

   -------------------
   -- Type_Integral --
   -------------------

   function Type_Integral
     (Node : Node_Id)
      return Ack.Types.Type_Entity
   is
      use type Ack.Types.Type_Entity;
   begin
      if Local_Integral_Type = null then
         declare
            Aqua_Entity     : constant Ack.Classes.Class_Entity :=
                                Ack.Classes.Get_Top_Level_Class ("aqua");
            Integral_Name   : constant String := "integral";
            Integral_Entity : constant Ack.Classes.Class_Entity :=
                                (if Aqua_Entity.Contains (Integral_Name)
                                 then Ack.Classes.Class_Entity
                                   (Aqua_Entity.Get (Integral_Name))
                                   else Classes.Load_Class
                                   (Get_Program (Aqua_Entity.Declaration_Node),
                                    Aqua_Entity, Get_Name_Id (Integral_Name)));
         begin
            Local_Integral_Type :=
              Ack.Types.New_Class_Type (Node, Integral_Entity, False);
         end;
      end if;

      return Local_Integral_Type;
   end Type_Integral;

end Ack.Semantic.Types;
