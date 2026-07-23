with Gtk.Scrolled_Window;
with Gtk.Text_View;
with Gtk.Widget;

with Aquarius.Models;
with Aquarius.Observable;

--  A plain-text view: displays a Text_Model in a GtkTextView. GtkSourceView is
--  not bound in GtkAda, so the source editor will later extend/replace this;
--  the abstraction hides the concrete widget from callers.

package Aquarius.UI.Gtk_Views.Text is

   type Text_View is new Gtk_View_Base with private;
   type Text_View_Access is access all Text_View'Class;

   function Create return Text_View_Access;

   overriding function Id (View : Text_View) return String;

   overriding function Accepts
     (View  : Text_View;
      Model : Aquarius.Models.Model_Interface'Class) return Boolean;

   overriding function Widget
     (View : Text_View) return Gtk.Widget.Gtk_Widget;

   overriding procedure Update
     (View : in out Text_View;
      Data : Aquarius.Observable.Update_Data_Interface'Class);

private

   type Text_View is new Gtk_View_Base with record
      Text_Widget : Gtk.Text_View.Gtk_Text_View;
      Scroller    : Gtk.Scrolled_Window.Gtk_Scrolled_Window;
   end record;
   --  Widget returns Scroller (which contains Text_Widget) so the view is
   --  clipped to the bubble's content area and scrolls when the text is
   --  larger, rather than growing the bubble to fit the whole file.

end Aquarius.UI.Gtk_Views.Text;
