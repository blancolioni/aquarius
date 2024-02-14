package Ack.Semantic.Analysis.Types is

   procedure Analyse_Type
     (Class     : Ack.Classes.Class_Entity;
      Type_Node : Node_Id)
     with Pre => Kind (Type_Node) in N_Type;

   procedure Analyse_Class_Type
     (Class     : Ack.Classes.Class_Entity;
      Type_Node : Node_Id)
     with Pre => Kind (Type_Node) = N_Class_Type;

   procedure Analyse_Anchored_Type
     (Class     : Ack.Classes.Class_Entity;
      Type_Node : Node_Id)
     with Pre => Kind (Type_Node) = N_Anchored_Type;

   procedure Analyse_Tuple_Type
     (Class     : Ack.Classes.Class_Entity;
      Type_Node : Node_Id)
     with Pre => Kind (Type_Node) = N_Tuple_Type;

end Ack.Semantic.Analysis.Types;
