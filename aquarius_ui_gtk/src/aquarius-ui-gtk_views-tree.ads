private with Ada.Containers.Vectors;
private with Gtk.Scrolled_Window;
private with Gtk.Tree_Model;
private with Gtk.Tree_Store;
private with Gtk.Tree_View;
private with Aquarius.Models.Trees;

with Gtk.Widget;

with Aquarius.Models;
with Aquarius.Observable;

--  A tree view: displays an Aquarius.Models.Trees tree model in a GtkTreeView.
--  Rows are populated lazily (a node's children are inserted when its row is
--  expanded). Activating a node whose Target is non-empty asks the frontend to
--  open it (see Gtk_Views.Open_Target); interior nodes just expand.

package Aquarius.UI.Gtk_Views.Tree is

   type Tree_View is new Gtk_View_Base with private;
   type Tree_View_Access is access all Tree_View'Class;

   function Create return Tree_View_Access;

   overriding function Id (View : Tree_View) return String;

   overriding function Accepts
     (View  : Tree_View;
      Model : Aquarius.Models.Model_Interface'Class) return Boolean;

   overriding function Widget
     (View : Tree_View) return Gtk.Widget.Gtk_Widget;

   overriding procedure Update
     (View : in out Tree_View;
      Data : Aquarius.Observable.Update_Data_Interface'Class);

private

   package Node_Vectors is new Ada.Containers.Vectors
     (Positive, Aquarius.Models.Trees.Tree_Node_Reference,
      Aquarius.Models.Trees."=");

   type Tree_View is new Gtk_View_Base with record
      Tree_Widget : Gtk.Tree_View.Gtk_Tree_View;
      Store       : Gtk.Tree_Store.Gtk_Tree_Store;
      Store_Model : Gtk.Tree_Model.Gtk_Tree_Model;
      Scroller    : Gtk.Scrolled_Window.Gtk_Scrolled_Window;
      Nodes       : Node_Vectors.Vector;
      --  Row column 2 holds a 1-based index into Nodes (or -1 for an
      --  unpopulated placeholder child), mapping each row back to its model
      --  node. Widget returns Scroller so the view clips/scrolls in a bubble.
   end record;

end Aquarius.UI.Gtk_Views.Tree;
