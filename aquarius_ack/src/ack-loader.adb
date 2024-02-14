package body Ack.Loader is

   Local_Loader : Ack_Loader;

   ---------------------
   -- Load_Class_File --
   ---------------------

   function Load_Class_File
     (Path : String)
      return Aquarius.Programs.Program_Tree
   is
   begin
      pragma Assert (Local_Loader /= null, "no ack loader configured");
      return Local_Loader (Path);
   end Load_Class_File;

   ----------------
   -- Set_Loader --
   ----------------

   procedure Set_Loader
     (Fn : Ack_Loader)
   is
   begin
      pragma Assert (Local_Loader = null, "conflicting ack loaders");
      Local_Loader := Fn;
   end Set_Loader;

end Ack.Loader;
