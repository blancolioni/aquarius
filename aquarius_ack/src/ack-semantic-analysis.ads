with Ack.Features;

with Ack.Attachment;

private package Ack.Semantic.Analysis is

   procedure Analyse_Class_Declaration
     (Node : Node_Id);

   procedure Analyse_Entity_Declaration_Groups
     (Class      : Ack.Classes.Class_Entity;
      Feature    : Ack.Features.Feature_Entity;
      Group_List : Node_Id;
      Local      : Boolean);

   procedure Analyse_Assertion
     (Class      : Ack.Classes.Class_Entity;
      Container  : not null access Root_Entity_Type'Class;
      Attachment : in out Ack.Attachment.Attachment_Context'Class;
      Assertion  : Node_Id;
      Process    : not null access
        procedure (Tag : Name_Id;
                   Condition : Node_Id));

   procedure Analyse_Effective_Routine
     (Class      : Ack.Classes.Class_Entity;
      Container  : not null access Root_Entity_Type'Class;
      Attachment : in out Ack.Attachment.Attachment_Context'Class;
      Routine    : Node_Id);

end Ack.Semantic.Analysis;
