package Ack.Loader is

   type Ack_Loader is access
     function (Path : String) return Aquarius.Programs.Program_Tree;

   function Load_Class_File
     (Path : String)
      return Aquarius.Programs.Program_Tree;

   procedure Set_Loader
     (Fn : Ack_Loader);

end Ack.Loader;
