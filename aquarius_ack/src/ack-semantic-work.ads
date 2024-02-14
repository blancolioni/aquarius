with Ack.Classes;

package Ack.Semantic.Work is

   type Work_Item_Category is
     (Feature_Header,
      Feature_Body,
      Class_Binding,
      Class_Layout,
      Error_Report);

   type Feature_Work_Handler is access
     procedure (Class    : Ack.Classes.Class_Entity;
                Feature  : Node_Id);

   procedure Add_Work_Item
     (Category  : Work_Item_Category;
      Class     : Ack.Classes.Class_Entity;
      Feature   : Node_Id);

   function Have_Work return Boolean;
   procedure Execute_Work;

   procedure Check_Work_Item
     (Class        : not null access constant
        Ack.Classes.Class_Entity_Record'Class;
      Feature_Name : Name_Id;
      Category     : Work_Item_Category);

end Ack.Semantic.Work;
