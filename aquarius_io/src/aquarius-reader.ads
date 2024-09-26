with Aquarius.Grammars;
with Aquarius.Interaction;
with Aquarius.Programs;
with Aquarius.Sources;
with Aquarius.Streams;

package Aquarius.Reader is

   function Read
     (Grammar    : Aquarius.Grammars.Aquarius_Grammar;
      Store      : not null access Programs.Root_Program_Tree_Store'Class;
      Interactor : Aquarius.Interaction.Interactor_Access;
      Source     : Aquarius.Sources.Source_Reference;
      Stream     : Aquarius.Streams.Reader_Reference)
      return Aquarius.Programs.Program_Tree;

   function Read
     (Grammar    : Aquarius.Grammars.Aquarius_Grammar;
      Source     : Aquarius.Sources.Source_Reference;
      Stream     : Aquarius.Streams.Reader_Reference)
     return Aquarius.Programs.Program_Tree;
   --  Load file using default store, interactor.
   --     interactor - console

   function Read
     (Grammar    : Aquarius.Grammars.Aquarius_Grammar;
      Store      : not null access Programs.Root_Program_Tree_Store'Class;
      Source     : Aquarius.Sources.Source_Reference;
      Stream     : Aquarius.Streams.Reader_Reference)
      return Aquarius.Programs.Program_Tree;
   --  Load file using default interactor (console)

end Aquarius.Reader;
