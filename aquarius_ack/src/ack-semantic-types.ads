with Ack.Types;

private package Ack.Semantic.Types is

   function Type_Any return Ack.Types.Type_Entity;

   function Type_Integral (Node : Node_Id) return Ack.Types.Type_Entity;

   function Type_String return Ack.Types.Type_Entity;

   function Type_Character return Ack.Types.Type_Entity;

   function Type_Boolean return Ack.Types.Type_Entity;

end Ack.Semantic.Types;
