package body Aquarius.Programs.Models is

   ------------
   -- Create --
   ------------

   function Create
     (Tree    : not null Program_Tree;
      Grammar : Aquarius.Grammars.Aquarius_Grammar)
      return Program_Tree_Model_Access
   is
   begin
      return new Program_Tree_Model'
        (Aquarius.Observable.Publisher_Base with
           Tree    => Tree,
           Grammar => Grammar);
   end Create;

   ----------
   -- Kind --
   ----------

   overriding function Kind (Model : Program_Tree_Model) return String is
      pragma Unreferenced (Model);
   begin
      return "tree";
   end Kind;

   --------------------
   -- Default_Viewer --
   --------------------

   overriding function Default_Viewer
     (Model : Program_Tree_Model) return String
   is
      pragma Unreferenced (Model);
   begin
      return "source";
   end Default_Viewer;

   -------------
   -- Program --
   -------------

   overriding function Program
     (Model : Program_Tree_Model) return Program_Tree
   is
   begin
      return Model.Tree;
   end Program;

   -------------
   -- Grammar --
   -------------

   overriding function Grammar
     (Model : Program_Tree_Model)
      return Aquarius.Grammars.Aquarius_Grammar
   is
   begin
      return Model.Grammar;
   end Grammar;

end Aquarius.Programs.Models;
