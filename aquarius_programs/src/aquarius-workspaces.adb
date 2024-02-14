package body Aquarius.Workspaces is

   -------------------------
   -- New_Empty_Workspace --
   -------------------------

   function New_Empty_Workspace
     (Path : String)
      return Reference
   is
   begin
      return new Instance'
        (Name => Aquarius.Names.To_Aquarius_Name ("Untitled Workspace"),
         Path => Aquarius.Names.To_Aquarius_Name (Path),
         Map  => <>);
   end New_Empty_Workspace;

   overriding function Get_Program
     (This      : Instance;
      File_Name : String)
      return Aquarius.Programs.Program_Tree
   is
   begin
      if not This.Map.Contains (File_Name) then
         return null;
      end if;
      return This.Map (File_Name);
   end Get_Program;

end Aquarius.Workspaces;
