with Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

package Aquarius.Paths is

   function Scratch_Path return String;

   function Scratch_File (Name : String;
                          Extension : String := "")
                          return String;

   function Is_Newer
     (File_1, File_2 : String)
      return Boolean;

   type Aquarius_Path is private;

   function To_OS_Path (Path : Aquarius_Path) return String;
--     function To_OS_Path (Path      : Aquarius_Path;
--                          File_Name : String)
--                          return String;

   function To_Aquarius_Path (From_OS_Path : String) return Aquarius_Path;

   function Join_Paths (Left, Right : Aquarius_Path) return Aquarius_Path;

private

   use Ada.Strings.Unbounded;

   package String_Vectors is
      new Ada.Containers.Vectors (Positive, Unbounded_String);

   type Aquarius_Path is
      record
         Absolute  : Boolean;
         Prefix    : Unbounded_String;
         Elements  : String_Vectors.Vector;
      end record;

end Aquarius.Paths;
