with Ada.Directories;                  use Ada.Directories;

with Kosei;

package body Aquarius.Configuration is

   function Get_Path
     (Config_Name : String)
      return String
   is (Kosei.Get ("/path")
       & "/"
       & Kosei.Get (Config_Name));

   -----------------------------
   -- Aqua_Configuration_Path --
   -----------------------------

   function Aqua_Configuration_Path return String is
   begin
      return Kosei.Get ("/path") & "/aqua_vm/aqua.config";
   end Aqua_Configuration_Path;

   ---------------------------------
   -- Aqua_Standard_Assembly_Path --
   ---------------------------------

   function Aqua_Standard_Assembly_Path return String is
   begin
      return Get_Path ("/install/paths/standard") & "/asm";
   end Aqua_Standard_Assembly_Path;

   --------------------------------
   -- Aqua_Standard_Library_Path --
   --------------------------------

   function Aqua_Standard_Library_Path return String is
   begin
      return Get_Path ("/install/paths/standard") & "/standard";
   end Aqua_Standard_Library_Path;

   ------------------------
   -- Assembly_File_Path --
   ------------------------

   function Assembly_File_Path
     (Base_Name : String)
      return String
   is
   begin
      return Compose (Assembly_Path, Base_Name, "s");
   end Assembly_File_Path;

   -------------------
   -- Assembly_Path --
   -------------------

   function Assembly_Path return String is
   begin
      return Get_Path ("/install/paths/assembly");
   end Assembly_Path;

   --------------------
   -- Directory_Path --
   --------------------

   function Directory_Path
     (Directory_Name : String)
      return String
   is
   begin
      return Kosei.Get ("/filesystem/" & Directory_Name & "/path");
   end Directory_Path;

   -------------------------
   -- Generated_File_Path --
   -------------------------

   function Generated_File_Path
     (Name : String)
      return String
   is
   begin
      return Compose (Generated_Path, Name);
   end Generated_File_Path;

   --------------------
   -- Generated_Path --
   --------------------

   function Generated_Path return String is
   begin
      return Get_Path ("/install/paths/generated");
   end Generated_Path;

   ------------------
   -- Grammar_Path --
   ------------------

   function Grammar_Path
     (Grammar_Name : String)
      return String
   is
   begin
      return Compose
        (Get_Path ("/install/paths/grammar"), Grammar_Name);
   end Grammar_Path;

   ----------------------
   -- Object_File_Path --
   ----------------------

   function Object_File_Path
     (Base_Name : String)
      return String
   is
   begin
      return Compose (Object_Path, Base_Name, "o");
   end Object_File_Path;

   -----------------
   -- Object_Path --
   -----------------

   function Object_Path return String is
   begin
      return Get_Path ("/install/paths/object");
   end Object_Path;

   --------------------
   -- Temporary_Path --
   --------------------

   function Temporary_Path return String is
   begin
      return Get_Path ("/install/paths/temporary");
   end Temporary_Path;

   ---------------
   -- Tool_Path --
   ---------------

   function Tool_Path (Name : String) return String is
   begin
      return Kosei.Get ("/install/tools/" & Name);
   end Tool_Path;

end Aquarius.Configuration;
