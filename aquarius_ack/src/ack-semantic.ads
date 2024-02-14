with Ack.Classes;

package Ack.Semantic is

   procedure Analyse_Class_Declaration
     (Node : Node_Id);

   function Get_Class
     (Qualified_Name : String)
      return Ack.Classes.Constant_Class_Entity;

private

   type Tuple_Arity_Range is range 2 .. 20;

   function Property_Feature_Node
     (Node : Node_Id)
     return Boolean
     with Pre => Kind (Node) = N_Feature_Declaration;

end Ack.Semantic;
