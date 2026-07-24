with Aquarius.Sources;

package Aquarius.Grammars.Manager is

   function Get_Grammar_For_File (File_Name : String)
                                 return Aquarius_Grammar;

   function Get_Grammar
     (Source : Aquarius.Sources.Source_Reference)
      return Aquarius_Grammar;

   function Get_Grammar (Name : String) return Aquarius_Grammar;

   function Load_Grammar_From_File (Name : String;
                                    Path : String)
                                    return Aquarius_Grammar;
   --  Load and analyse the EBNF grammar at Path, reporting any errors
   --  to the console. Returns null if the grammar has errors.

end Aquarius.Grammars.Manager;
