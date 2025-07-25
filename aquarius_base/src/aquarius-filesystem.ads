package Aquarius.Filesystem is

   function Create (Path : String) return Boolean;
   function Delete (Path : String) return Boolean;
   function Exists (Path : String) return Boolean;
   function Append (Path : String; Text : String) return Boolean;

end Aquarius.Filesystem;
