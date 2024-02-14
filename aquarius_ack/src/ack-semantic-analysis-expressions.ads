package Ack.Semantic.Analysis.Expressions is

   procedure Analyse_Boolean_Expression
     (Class           : Ack.Classes.Class_Entity;
      Container       : not null access Root_Entity_Type'Class;
      Attachment      : in out Ack.Attachment.Attachment_Context'Class;
      Expression      : Node_Id);

   procedure Analyse_Expression
     (Class           : Ack.Classes.Class_Entity;
      Container       : not null access Root_Entity_Type'Class;
      Attachment      : in out Ack.Attachment.Attachment_Context'Class;
      Expression_Type : not null access Root_Entity_Type'Class;
      Expression      : Node_Id);

   procedure Analyse_Operator
     (Class           : Ack.Classes.Class_Entity;
      Container       : not null access Root_Entity_Type'Class;
      Attachment      : in out Ack.Attachment.Attachment_Context'Class;
      Expression_Type : access Root_Entity_Type'Class;
      Operator_Node   : Node_Id);

   procedure Analyse_Precursor
     (Class           : Ack.Classes.Class_Entity;
      Container       : not null access Root_Entity_Type'Class;
      Attachment      : in out Ack.Attachment.Attachment_Context'Class;
      Expression_Type : access Root_Entity_Type'Class;
      Precursor       : Node_Id);

   procedure Analyse_Tuple_Expression
     (Class           : Ack.Classes.Class_Entity;
      Container       : not null access Root_Entity_Type'Class;
      Attachment      : in out Ack.Attachment.Attachment_Context'Class;
      Expression_Type : access Root_Entity_Type'Class;
      Expression      : Node_Id);

   procedure Analyse_Actual_Arguments
     (Class            : Ack.Classes.Class_Entity;
      Container        : not null access Root_Entity_Type'Class;
      Attachment       : in out Ack.Attachment.Attachment_Context'Class;
      Entity           : Entity_Type;
      Actual_List_Node : Node_Id);

end Ack.Semantic.Analysis.Expressions;
