package Aquarius.Grammars.Manager is

   function Get_Grammar_For_File (File_Name : String)
                                 return Aquarius_Grammar;

   function Get_Grammar (Name : String) return Aquarius_Grammar;

end Aquarius.Grammars.Manager;
