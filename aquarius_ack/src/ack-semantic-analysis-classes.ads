package Ack.Semantic.Analysis.Classes is

   procedure Analyse_Class_Declaration
     (Node : Node_Id);

   function Analyse_Class_Header
     (Class  : Node_Id;
      Header : Node_Id)
     return Ack.Classes.Class_Entity;

   procedure Analyse_Formal_Generics
     (Class           : Ack.Classes.Class_Entity;
      Formal_Generics : Node_Id);

   procedure Analyse_Notes
     (Class : Ack.Classes.Class_Entity;
      Notes : Node_Id);

   procedure Analyse_Feature_Name
     (Class    : Ack.Classes.Class_Entity;
      Exports  : Node_Id;
      Feature  : Node_Id)
     with Pre => Kind (Feature) = N_Feature_Declaration;

   procedure Analyse_Features
     (Class   : Ack.Classes.Class_Entity;
      Node    : Node_Id;
      Analyse : not null access
        procedure (Class : Ack.Classes.Class_Entity;
                   Exports  : Node_Id;
                   Node  : Node_Id))
     with Pre => Kind (Node) = N_Features;

   procedure Analyse_Inheritance
     (Class       : Ack.Classes.Class_Entity;
      Inheritance : Node_Id);

   procedure Analyse_Inherit
     (Class   : Ack.Classes.Class_Entity;
      Inherit : Node_Id);

end Ack.Semantic.Analysis.Classes;
