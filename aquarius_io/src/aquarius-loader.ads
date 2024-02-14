with Aquarius.Grammars;
with Aquarius.Interaction;
with Aquarius.Programs;

package Aquarius.Loader is

   function Load_From_File
     (Grammar    : Aquarius.Grammars.Aquarius_Grammar;
      Store      : not null access Programs.Root_Program_Tree_Store'Class;
      Interactor : Aquarius.Interaction.Interactor_Access;
      Path       : String)
      return Aquarius.Programs.Program_Tree;

   function Load_From_File
     (Grammar    : Aquarius.Grammars.Aquarius_Grammar;
      Path       : String)
     return Aquarius.Programs.Program_Tree;
   --  Load file using default store, interactor.
   --     interactor - console

   function Load_From_File
     (Grammar    : Aquarius.Grammars.Aquarius_Grammar;
      Store      : not null access Programs.Root_Program_Tree_Store'Class;
      Path       : String)
     return Aquarius.Programs.Program_Tree;
   --  Load file using default interactor (console)

end Aquarius.Loader;
