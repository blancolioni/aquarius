with Gtk.Widget;

with Aquarius.Models;
with Aquarius.UI.Views;

--  GtkAda view layer. This is the ONLY layer that knows about concrete
--  widgets, so an unbound widget (e.g. GtkSourceView) can be introduced here
--  without touching aquarius_ui or aquarius_base.

package Aquarius.UI.Gtk_Views is

   type Gtk_View_Interface is
     interface and Aquarius.UI.Views.View_Interface;

   function Widget
     (View : Gtk_View_Interface) return Gtk.Widget.Gtk_Widget is abstract;

   type Gtk_View_Reference is access all Gtk_View_Interface'Class;

   --  Hook so a view can ask the frontend to open a locator (path/URI) in a
   --  new bubble, without depending on the frontend package (which would be a
   --  cycle: the frontend withs the views). The frontend installs this at
   --  startup; views invoke it, e.g. the tree view when a file node is
   --  activated.
   type Open_Target_Callback is access procedure (Target : String);

   procedure Set_Open_Target (Callback : Open_Target_Callback);
   function Open_Target return Open_Target_Callback;

   --  Base implementing model storage + subscription. Concrete views add
   --  Id, Accepts, Widget and Update.
   type Gtk_View_Base is abstract new Gtk_View_Interface with private;

   overriding procedure Set_Model
     (View  : not null access Gtk_View_Base;
      Model : Aquarius.Models.Model_Reference);

   overriding function Model
     (View : Gtk_View_Base) return Aquarius.Models.Model_Reference;

private

   type Gtk_View_Base is abstract new Gtk_View_Interface with record
      Model_Ref : Aquarius.Models.Model_Reference;
   end record;

end Aquarius.UI.Gtk_Views;
