with Ada.Directories;
with Ada.Environment_Variables;
with Ada.Text_IO;
with Ack.Loader;
with Aquarius.Grammars.Manager;
with Aquarius.Loader;
with Aquarius.Programs;
with Kosei.Json;

package body Aquarius.Library is

   function Find_Configuration return String;

   function Load_Aqua_Class
     (Path : String)
      return Aquarius.Programs.Program_Tree
   is (Aquarius.Loader.Load_From_File
       (Grammar => Aquarius.Grammars.Manager.Get_Grammar ("aqua"),
        Path    => Path));

   ------------------------
   -- Find_Configuration --
   ------------------------

   function Find_Configuration return String is

      function Try (Path : String) return Boolean;

      ---------
      -- Try --
      ---------

      function Try (Path : String) return Boolean is
      begin
         return Ada.Directories.Exists
           (Ada.Directories.Compose
              (Path, "aquarius.json"));
      end Try;

   begin
      declare
         Path : constant String := ".aquarius";
      begin
         if Try (Path) then
            return Path;
         end if;
      end;

      declare
         Home : constant String :=
                  Ada.Environment_Variables.Value
                    ("HOME", "");
         Path : constant String :=
                  Ada.Directories.Compose (Home, ".aquarius");
      begin
         if Try (Path) then
            return Path;
         end if;
      end;

      return "";
   end Find_Configuration;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
      Config_Path : constant String := Find_Configuration;
   begin
      if Config_Path = "" then
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            "Unable to locate Aquarius configuration files");
         raise Configuration_Error;
      end if;

      Ada.Text_IO.Put_Line
        ("Reading config from " & Config_Path);

      Kosei.Json.Add_Json_Config
        (Ada.Directories.Compose
           (Config_Path, "aquarius.json"));

      Ack.Loader.Set_Loader (Load_Aqua_Class'Access);

   end Initialize;

end Aquarius.Library;
