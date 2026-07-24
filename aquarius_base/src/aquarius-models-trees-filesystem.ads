--  A tree model backed by a directory on the filesystem. Children are the
--  directory's entries (subdirectories first, then files, alphabetically;
--  hidden dot-entries skipped). Files carry their path as the open Target;
--  directories only expand.

package Aquarius.Models.Trees.Filesystem is

   type Filesystem_Tree_Model is
     new Aquarius.Observable.Publisher_Base and Tree_Model_Interface
   with private;

   type Filesystem_Tree_Model_Access is
     access all Filesystem_Tree_Model'Class;

   function Create (Path : String) return Filesystem_Tree_Model_Access;
   --  A model rooted at the directory (or file) Path.

private

   type Filesystem_Tree_Model is
     new Aquarius.Observable.Publisher_Base and Tree_Model_Interface
   with record
      Root_Node : Tree_Node_Reference;
   end record;

   overriding function Kind (Model : Filesystem_Tree_Model) return String;
   overriding function Default_Viewer
     (Model : Filesystem_Tree_Model) return String;
   overriding function Root
     (Model : Filesystem_Tree_Model) return Tree_Node_Reference;

end Aquarius.Models.Trees.Filesystem;
