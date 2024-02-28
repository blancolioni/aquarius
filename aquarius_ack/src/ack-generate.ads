with Tagatha.Code;

package Ack.Generate is

   procedure Generate_Class_Declaration
     (Node : Node_Id;
      Root : Boolean);

   procedure Generate_Compound
     (Unit         : in out Tagatha.Code.Instance'Class;
      Context      : not null access constant Root_Entity_Type'Class;
      Node         : Node_Id;
      Retry_Target : Tagatha.Code.Label := Tagatha.Code.No_Label);

   procedure Generate_Expression
     (Unit       : in out Tagatha.Code.Instance'Class;
      Context    : not null access constant Root_Entity_Type'Class;
      Expression : Node_Id);

   procedure Generate_Exit
     (Unit    : in out Tagatha.Code.Instance'Class);

end Ack.Generate;
