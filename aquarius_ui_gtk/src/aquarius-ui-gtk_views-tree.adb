with Glib;                       use Glib;
with Glib.Object;

with Gtk.Cell_Renderer_Pixbuf;    use Gtk.Cell_Renderer_Pixbuf;
with Gtk.Cell_Renderer_Text;      use Gtk.Cell_Renderer_Text;
with Gtk.Enums;                   use Gtk.Enums;
with Gtk.Scrolled_Window;         use Gtk.Scrolled_Window;
with Gtk.Tree_Model;              use Gtk.Tree_Model;
with Gtk.Tree_Store;              use Gtk.Tree_Store;
with Gtk.Tree_View;               use Gtk.Tree_View;
with Gtk.Tree_View_Column;        use Gtk.Tree_View_Column;

package body Aquarius.UI.Gtk_Views.Tree is

   use type Aquarius.Models.Model_Reference;
   use type Aquarius.Models.Trees.Tree_Node_Reference;

   package Trees renames Aquarius.Models.Trees;

   Col_Icon  : constant Gint := 0;
   Col_Label : constant Gint := 1;
   Col_Index : constant Gint := 2;

   package View_Data is new Glib.Object.User_Data (Tree_View_Access);

   procedure Add_Node
     (View   : Tree_View_Access;
      Parent : Gtk_Tree_Iter;
      Node   : Trees.Tree_Node_Reference);

   procedure Populate (View : Tree_View_Access; Iter : Gtk_Tree_Iter);

   function Node_Of
     (View : Tree_View_Access; Iter : Gtk_Tree_Iter)
      return Trees.Tree_Node_Reference;

   procedure On_Expanded
     (Self : access Gtk_Tree_View_Record'Class;
      Iter : Gtk_Tree_Iter;
      Path : Gtk_Tree_Path);

   procedure On_Activated
     (Self   : access Gtk_Tree_View_Record'Class;
      Path   : Gtk_Tree_Path;
      Column : not null access Gtk_Tree_View_Column_Record'Class);

   --------------
   -- Add_Node --
   --------------

   procedure Add_Node
     (View   : Tree_View_Access;
      Parent : Gtk_Tree_Iter;
      Node   : Trees.Tree_Node_Reference)
   is
      Iter : Gtk_Tree_Iter;
   begin
      View.Nodes.Append (Node);
      View.Store.Append (Iter, Parent);
      View.Store.Set (Iter, Col_Icon, Node.Icon_Name);
      View.Store.Set (Iter, Col_Label, Node.Label);
      View.Store.Set (Iter, Col_Index, Gint (View.Nodes.Last_Index));

      if Node.Has_Children then
         --  Placeholder child (index -1) so the expander shows; it is
         --  replaced by the real children when the row is first expanded.
         declare
            Placeholder : Gtk_Tree_Iter;
         begin
            View.Store.Append (Placeholder, Iter);
            View.Store.Set (Placeholder, Col_Index, Gint (-1));
            View.Store.Set (Placeholder, Col_Label, "");
         end;
      end if;
   end Add_Node;

   -------------
   -- Node_Of --
   -------------

   function Node_Of
     (View : Tree_View_Access; Iter : Gtk_Tree_Iter)
      return Trees.Tree_Node_Reference
   is
      Idx : constant Gint := Get_Int (View.Store_Model, Iter, Col_Index);
   begin
      if Idx >= 1 then
         return View.Nodes (Positive (Idx));
      else
         return null;
      end if;
   end Node_Of;

   --------------
   -- Populate --
   --------------

   procedure Populate (View : Tree_View_Access; Iter : Gtk_Tree_Iter) is
      First : constant Gtk_Tree_Iter := Nth_Child (View.Store_Model, Iter, 0);
      Node  : constant Trees.Tree_Node_Reference := Node_Of (View, Iter);
   begin
      --  Only populate once: the placeholder child carries index -1.
      if First = Null_Iter
        or else Get_Int (View.Store_Model, First, Col_Index) /= -1
        or else Node = null
      then
         return;
      end if;

      --  Drop the placeholder (and anything else), then add real children.
      declare
         Child : Gtk_Tree_Iter := Nth_Child (View.Store_Model, Iter, 0);
      begin
         while Child /= Null_Iter loop
            View.Store.Remove (Child);
         end loop;
      end;

      for I in 1 .. Node.Child_Count loop
         Add_Node (View, Iter, Node.Child (I));
      end loop;
   end Populate;

   ----------------
   -- On_Expanded --
   ----------------

   procedure On_Expanded
     (Self : access Gtk_Tree_View_Record'Class;
      Iter : Gtk_Tree_Iter;
      Path : Gtk_Tree_Path)
   is
      pragma Unreferenced (Path);
   begin
      Populate (View_Data.Get (Self), Iter);
   end On_Expanded;

   -----------------
   -- On_Activated --
   -----------------

   procedure On_Activated
     (Self   : access Gtk_Tree_View_Record'Class;
      Path   : Gtk_Tree_Path;
      Column : not null access Gtk_Tree_View_Column_Record'Class)
   is
      pragma Unreferenced (Column);
      View : constant Tree_View_Access := View_Data.Get (Self);
      Iter : constant Gtk_Tree_Iter := Get_Iter (View.Store_Model, Path);
   begin
      if Iter = Null_Iter then
         return;
      end if;
      declare
         Node : constant Trees.Tree_Node_Reference := Node_Of (View, Iter);
         Cb   : constant Open_Target_Callback := Open_Target;
      begin
         if Node /= null and then Node.Target /= "" and then Cb /= null then
            Cb (Node.Target);
         end if;
      end;
   end On_Activated;

   ------------
   -- Create --
   ------------

   function Create return Tree_View_Access is
      Result : constant Tree_View_Access := new Tree_View;
      Col    : Gtk_Tree_View_Column;
      Pixbuf : Gtk_Cell_Renderer_Pixbuf;
      Text   : Gtk_Cell_Renderer_Text;
      Dummy  : Gint;
   begin
      Gtk.Tree_Store.Gtk_New
        (Result.Store, [GType_String, GType_String, GType_Int]);
      Result.Store_Model := +Result.Store;

      Gtk.Tree_View.Gtk_New (Result.Tree_Widget);
      Result.Tree_Widget.Set_Model (Result.Store_Model);
      Result.Tree_Widget.Set_Headers_Visible (False);

      --  Single column: themed icon then label.
      Gtk.Tree_View_Column.Gtk_New (Col);
      Gtk.Cell_Renderer_Pixbuf.Gtk_New (Pixbuf);
      Col.Pack_Start (Pixbuf, Expand => False);
      Col.Add_Attribute (Pixbuf, "icon-name", Col_Icon);
      Gtk.Cell_Renderer_Text.Gtk_New (Text);
      Col.Pack_Start (Text, Expand => True);
      Col.Add_Attribute (Text, "text", Col_Label);
      Dummy := Result.Tree_Widget.Append_Column (Col);

      Result.Tree_Widget.On_Row_Expanded (On_Expanded'Access);
      Result.Tree_Widget.On_Row_Activated (On_Activated'Access);
      View_Data.Set (Result.Tree_Widget, Result);

      Gtk.Scrolled_Window.Gtk_New (Result.Scroller);
      Result.Scroller.Set_Policy (Policy_Automatic, Policy_Automatic);
      Result.Scroller.Add (Result.Tree_Widget);
      return Result;
   end Create;

   --------
   -- Id --
   --------

   overriding function Id (View : Tree_View) return String is
      pragma Unreferenced (View);
   begin
      return "tree";
   end Id;

   -------------
   -- Accepts --
   -------------

   overriding function Accepts
     (View  : Tree_View;
      Model : Aquarius.Models.Model_Interface'Class) return Boolean
   is
      pragma Unreferenced (View);
   begin
      return Model in Trees.Tree_Model_Interface'Class;
   end Accepts;

   ------------
   -- Widget --
   ------------

   overriding function Widget
     (View : Tree_View) return Gtk.Widget.Gtk_Widget
   is (Gtk.Widget.Gtk_Widget (View.Scroller));

   ------------
   -- Update --
   ------------

   overriding procedure Update
     (View : in out Tree_View;
      Data : Aquarius.Observable.Update_Data_Interface'Class)
   is
      pragma Unreferenced (Data);
      M    : constant Aquarius.Models.Model_Reference := View.Model;
      Self : constant Tree_View_Access :=
               View_Data.Get (View.Tree_Widget);
   begin
      if M = null
        or else M.all not in Trees.Tree_Model_Interface'Class
      then
         return;
      end if;

      View.Store.Clear;
      View.Nodes.Clear;

      declare
         Root : constant Trees.Tree_Node_Reference :=
                  Trees.Tree_Model_Interface'Class (M.all).Root;
      begin
         if Root = null then
            return;
         end if;
         --  Show the root's contents as the top-level rows (its own name is
         --  the bubble title). This makes the first level visible without an
         --  expansion; deeper levels expand lazily. A leaf root is shown as a
         --  single row.
         if Root.Has_Children then
            for I in 1 .. Root.Child_Count loop
               Add_Node (Self, Null_Iter, Root.Child (I));
            end loop;
         else
            Add_Node (Self, Null_Iter, Root);
         end if;
      end;
   end Update;

end Aquarius.UI.Gtk_Views.Tree;
