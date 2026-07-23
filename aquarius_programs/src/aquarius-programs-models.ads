with Aquarius.Grammars;
with Aquarius.Models;
with Aquarius.Observable;

--  Tree-shaped models: a parsed program tree together with the grammar it was
--  parsed against. A viewer that wants to display a program tree (the source
--  editor) requires a Tree_Model_Interface.

package Aquarius.Programs.Models is

   type Tree_Model_Interface is
     interface and Aquarius.Models.Model_Interface;

   function Program
     (Model : Tree_Model_Interface) return Program_Tree is abstract;

   function Grammar
     (Model : Tree_Model_Interface)
      return Aquarius.Grammars.Aquarius_Grammar is abstract;

   --  A ready-to-use concrete tree model wrapping an already-parsed program
   --  tree. Read-only for now (editing / reparse is a later phase), so it has
   --  no mutators; it is observable so a future editable model can share the
   --  same viewer.
   type Program_Tree_Model is
     new Aquarius.Observable.Publisher_Base and Tree_Model_Interface
   with private;

   type Program_Tree_Model_Access is access all Program_Tree_Model'Class;

   function Create
     (Tree    : not null Program_Tree;
      Grammar : Aquarius.Grammars.Aquarius_Grammar)
      return Program_Tree_Model_Access;

private

   type Program_Tree_Model is
     new Aquarius.Observable.Publisher_Base and Tree_Model_Interface
   with record
      Tree    : Program_Tree;
      Grammar : Aquarius.Grammars.Aquarius_Grammar;
   end record;

   overriding function Kind (Model : Program_Tree_Model) return String;
   overriding function Default_Viewer
     (Model : Program_Tree_Model) return String;
   overriding function Program
     (Model : Program_Tree_Model) return Program_Tree;
   overriding function Grammar
     (Model : Program_Tree_Model)
      return Aquarius.Grammars.Aquarius_Grammar;

end Aquarius.Programs.Models;
