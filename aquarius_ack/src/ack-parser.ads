package Ack.Parser is

   function Import
     (Program : Aquarius.Programs.Program_Tree)
     return Node_Id;

private

   function Import_Class_Name
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
     with Pre => From.Name = "class_name";

   function Import_Optional_Child
     (Parent     : Aquarius.Programs.Program_Tree;
      Child_Name : String;
      Import     : not null access
        function (Child : Aquarius.Programs.Program_Tree)
      return Node_Id)
      return Node_Id;

   function Import_List
     (From         : Aquarius.Programs.Program_Tree;
      Child_Name   : String;
      Import_Child : not null access
        function (Child : Aquarius.Programs.Program_Tree)
      return Node_Id)
      return List_Id;

   function Import_String_Constant
     (Raw_Text : String)
      return String;

   function Import_Character_Constant
     (Raw_Text : String)
      return String;

end Ack.Parser;
