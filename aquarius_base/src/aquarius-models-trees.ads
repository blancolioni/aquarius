--  Tree-shaped models: a hierarchy of labelled nodes, shown in a tree view
--  (e.g. a directory listing, an outline). A viewer that wants to display a
--  hierarchy requires a Tree_Model_Interface.
--
--  The model is node-centric so a view can populate lazily: it can show that a
--  node is expandable (Has_Children) without enumerating its children, and
--  materialise each child only when needed (Child).

package Aquarius.Models.Trees is

   type Tree_Node_Interface is interface;
   type Tree_Node_Reference is access all Tree_Node_Interface'Class;

   function Label
     (Node : Tree_Node_Interface) return String is abstract;

   function Icon_Name
     (Node : Tree_Node_Interface) return String is abstract;
   --  Name of a themed icon to show beside the label (e.g. "folder",
   --  "text-x-generic"), or "" for none.

   function Has_Children
     (Node : Tree_Node_Interface) return Boolean is abstract;
   --  Whether the node can be expanded. May be True without Child_Count having
   --  been computed, so a view can show an expander without enumerating.

   function Child_Count
     (Node : Tree_Node_Interface) return Natural is abstract;

   function Child
     (Node  : Tree_Node_Interface;
      Index : Positive) return Tree_Node_Reference is abstract;
   --  The Index'th child (1-based), materialised on demand.

   function Target
     (Node : Tree_Node_Interface) return String is abstract;
   --  A locator (path/URI) the environment can open when the node is
   --  activated, or "" if the node is not openable (e.g. an interior node
   --  that only expands).

   type Tree_Model_Interface is
     interface and Aquarius.Models.Model_Interface;

   function Root
     (Model : Tree_Model_Interface) return Tree_Node_Reference is abstract;

end Aquarius.Models.Trees;
