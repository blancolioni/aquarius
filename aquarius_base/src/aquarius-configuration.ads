package Aquarius.Configuration is

   function Grammar_Path
     (Grammar_Name : String)
      return String;

   function Generated_Path return String;

   function Generated_File_Path
     (Name : String)
      return String;

   function Object_Path return String;

   function Object_File_Path
     (Base_Name : String)
     return String;

   function Assembly_Path return String;

   function Assembly_File_Path
     (Base_Name : String)
      return String;

   function Tool_Path (Name : String) return String;

   function Aqua_Standard_Library_Path return String;

   function Aqua_Standard_Assembly_Path return String;

end Aquarius.Configuration;
