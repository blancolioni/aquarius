package Ack.Semantic.Analysis.Features is

   procedure Analyse_Feature_Header
     (Class   : Ack.Classes.Class_Entity;
      Feature : Node_Id)
     with Pre => Kind (Feature) = N_Feature_Declaration;

   procedure Analyse_Feature_Body
     (Class   : Ack.Classes.Class_Entity;
      Feature : Node_Id)
     with Pre => Kind (Feature) = N_Feature_Declaration;

end Ack.Semantic.Analysis.Features;
