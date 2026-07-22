with Gtk.Text_View;    use Gtk.Text_View;
with Gtk.Text_Buffer;  use Gtk.Text_Buffer;

with Aquarius.Models.Text;

package body Aquarius.UI.Gtk_Views.Text is

   use type Aquarius.Models.Model_Reference;

   package Text_Models renames Aquarius.Models.Text;

   ------------
   -- Create --
   ------------

   function Create return Text_View_Access is
      Result : constant Text_View_Access := new Text_View;
   begin
      Gtk.Text_View.Gtk_New (Result.Text_Widget);
      Result.Text_Widget.Set_Editable (False);
      Result.Text_Widget.Set_Cursor_Visible (False);
      return Result;
   end Create;

   --------
   -- Id --
   --------

   overriding function Id (View : Text_View) return String is
      pragma Unreferenced (View);
   begin
      return "text";
   end Id;

   -------------
   -- Accepts --
   -------------

   overriding function Accepts
     (View  : Text_View;
      Model : Aquarius.Models.Model_Interface'Class) return Boolean
   is
      pragma Unreferenced (View);
   begin
      return Model in Text_Models.Text_Model_Interface'Class;
   end Accepts;

   ------------
   -- Widget --
   ------------

   overriding function Widget
     (View : Text_View) return Gtk.Widget.Gtk_Widget
   is (Gtk.Widget.Gtk_Widget (View.Text_Widget));

   ------------
   -- Update --
   ------------

   overriding procedure Update
     (View : in out Text_View;
      Data : Aquarius.Observable.Update_Data_Interface'Class)
   is
      pragma Unreferenced (Data);
      M : constant Aquarius.Models.Model_Reference := View.Model;
   begin
      if M /= null
        and then M.all in Text_Models.Text_Model_Interface'Class
      then
         View.Text_Widget.Get_Buffer.Set_Text
           (Text_Models.Text_Model_Interface'Class (M.all).Text);
      end if;
   end Update;

end Aquarius.UI.Gtk_Views.Text;
