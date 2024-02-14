package Ack.Files is

   function Find_Class_File
     (Referrer : Aquarius.Programs.Program_Tree;
      Parent   : not null access constant Root_Entity_Type'Class;
      Name     : Name_Id)
      return String;

end Ack.Files;
