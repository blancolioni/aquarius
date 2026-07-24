with Gtk.Scrolled_Window;
with Gtk.Text_View;
with Gtk.Widget;

with Aquarius.Models;
with Aquarius.Observable;

--  A source view: displays a program tree (an Aquarius.Programs.Models tree
--  model) as arranged, syntax-styled text in a GtkTextView. Layout comes from
--  the grammar's format rules (Aquarius.Programs.Arrangements); colours come
--  from a Styler via the GtkTextBuffer renderer. Read-only for now.

package Aquarius.UI.Gtk_Views.Source is

   type Source_View is new Gtk_View_Base with private;
   type Source_View_Access is access all Source_View'Class;

   function Create return Source_View_Access;

   overriding function Id (View : Source_View) return String;

   overriding function Accepts
     (View  : Source_View;
      Model : Aquarius.Models.Model_Interface'Class) return Boolean;

   overriding function Widget
     (View : Source_View) return Gtk.Widget.Gtk_Widget;

   overriding procedure Update
     (View : in out Source_View;
      Data : Aquarius.Observable.Update_Data_Interface'Class);

private

   type Source_View is new Gtk_View_Base with record
      Text_Widget : Gtk.Text_View.Gtk_Text_View;
      Scroller    : Gtk.Scrolled_Window.Gtk_Scrolled_Window;
   end record;
   --  Widget returns Scroller so the view is clipped to the bubble's content
   --  area and scrolls, like the plain-text view.

end Aquarius.UI.Gtk_Views.Source;
