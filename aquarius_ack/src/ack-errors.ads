package Ack.Errors is

   procedure Report_Errors (Node : Node_Id);
   procedure Record_Errors (Node : Node_Id);

   function Has_Errors return Boolean;
   procedure Clear_Errors;

end Ack.Errors;
