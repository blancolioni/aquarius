with Ada.Directories;

package body Aquarius.Sources.Files is

   type Instance (Length : Natural) is new Source_Interface with
      record
         Path : String (1 .. Length);
      end record;

   overriding function Short_Name (This : Instance) return String
   is (Ada.Directories.Simple_Name (This.Path));

   overriding function Full_Name (This : Instance) return String
   is (This.Path);

   overriding function Grammar_Tag (This : Instance) return String
   is (Ada.Directories.Extension (This.Path));

   -----------------
   -- File_Source --
   -----------------

   function File_Source (Path : String) return Source_Reference is
   begin
      return new Instance'(Path'Length, Path);
   end File_Source;

end Aquarius.Sources.Files;
