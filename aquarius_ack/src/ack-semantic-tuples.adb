with Ack.Environment;
with Ack.Semantic.Classes;

package body Ack.Semantic.Tuples is

   Tuple_Classes : array (Tuple_Arity_Range) of Ack.Classes.Class_Entity :=
                     [others => null];

   function Load_Tuple_Class
     (Arity : Tuple_Arity_Range)
      return Ack.Classes.Class_Entity;

   ----------------------
   -- Load_Tuple_Class --
   ----------------------

   function Load_Tuple_Class
     (Arity : Tuple_Arity_Range)
      return Ack.Classes.Class_Entity
   is
      Aqua_Class   : constant Ack.Classes.Class_Entity :=
                       Classes.Load_Class (null,
                                   Ack.Environment.Top_Level,
                                   Get_Name_Id ("Aqua"));
      Arity_Image  : constant String := Tuple_Arity_Range'Image (Arity);
      Tuple_Name   : constant String :=
                       "Tuple" & Arity_Image (2 .. Arity_Image'Last);
   begin
      return Classes.Load_Class (null, Aqua_Class,
                         Get_Name_Id (Tuple_Name));
   end Load_Tuple_Class;

   -----------------
   -- Tuple_Class --
   -----------------

   function Tuple_Class
     (Arity : Tuple_Arity_Range)
      return Ack.Classes.Class_Entity
   is
      use type Ack.Classes.Class_Entity;
      Tuple_Class  : Ack.Classes.Class_Entity renames
        Tuple_Classes (Arity);
   begin
      if Tuple_Class = null then
         Tuple_Class := Load_Tuple_Class (Arity);
      end if;
      return Tuple_Class;
   end Tuple_Class;

end Ack.Semantic.Tuples;
