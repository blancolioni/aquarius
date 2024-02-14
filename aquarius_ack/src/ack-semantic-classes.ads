private package Ack.Semantic.Classes is

   function Load_Class
     (Referrer : Aquarius.Programs.Program_Tree;
      Parent   : not null access Root_Entity_Type'Class;
      Name     : Name_Id)
      return Ack.Classes.Class_Entity;

end Ack.Semantic.Classes;
