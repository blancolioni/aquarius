package Ack.Semantic.Analysis.Statements is

   procedure Analyse_Compound
     (Class      : Ack.Classes.Class_Entity;
      Container  : not null access Root_Entity_Type'Class;
      Attachment : in out Ack.Attachment.Attachment_Context'Class;
      Compound   : Node_Id);

   procedure Analyse_Assignment
     (Class      : Ack.Classes.Class_Entity;
      Container  : not null access Root_Entity_Type'Class;
      Attachment : in out Ack.Attachment.Attachment_Context'Class;
      Assignment : Node_Id);

   procedure Analyse_Conditional
     (Class       : Ack.Classes.Class_Entity;
      Container   : not null access Root_Entity_Type'Class;
      Attachment  : in out Ack.Attachment.Attachment_Context'Class;
      Conditional : Node_Id);

   procedure Analyse_Creation
     (Class      : Ack.Classes.Class_Entity;
      Container  : not null access Root_Entity_Type'Class;
      Attachment : in out Ack.Attachment.Attachment_Context'Class;
      Creation   : Node_Id);

   procedure Analyse_Loop
     (Class      : Ack.Classes.Class_Entity;
      Container  : not null access Root_Entity_Type'Class;
      Attachment : in out Ack.Attachment.Attachment_Context'Class;
      Loop_Node  : Node_Id);

   procedure Analyse_Check
     (Class      : Ack.Classes.Class_Entity;
      Container  : not null access Root_Entity_Type'Class;
      Attachment : in out Ack.Attachment.Attachment_Context'Class;
      Check      : Node_Id)
   is null;

   procedure Analyse_Retry
     (Class      : Ack.Classes.Class_Entity;
      Container  : not null access Root_Entity_Type'Class;
      Attachment : in out Ack.Attachment.Attachment_Context'Class;
      Retry      : Node_Id);

end Ack.Semantic.Analysis.Statements;
