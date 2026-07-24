with Gtk.Enums;           use Gtk.Enums;
with Gtk.Scrolled_Window;  use Gtk.Scrolled_Window;
with Gtk.Text_View;        use Gtk.Text_View;
with Gtk.Widget;           use Gtk.Widget;
with Pango.Font;

with Aquarius.Messages;
with Aquarius.Programs;
with Aquarius.Programs.Arrangements;
with Aquarius.Programs.Models;
with Aquarius.Rendering.Styles;

with Aquarius.UI.Gtk_Views.Tree_Render;

package body Aquarius.UI.Gtk_Views.Source is

   use type Aquarius.Models.Model_Reference;

   package Tree_Models renames Aquarius.Programs.Models;

   use type Aquarius.Programs.Program_Tree;

   ------------
   -- Create --
   ------------

   function Create return Source_View_Access is
      Result : constant Source_View_Access := new Source_View;
   begin
      Gtk.Text_View.Gtk_New (Result.Text_Widget);
      Result.Text_Widget.Set_Editable (False);
      Result.Text_Widget.Set_Cursor_Visible (False);
      --  Column padding assumes a fixed-width font. Override_Font is
      --  obsolescent but the supported CSS route is far heavier for one font.
      pragma Warnings (Off);
      Result.Text_Widget.Override_Font
        (Pango.Font.From_String ("monospace 10"));
      pragma Warnings (On);

      Gtk.Scrolled_Window.Gtk_New (Result.Scroller);
      Result.Scroller.Set_Policy (Policy_Automatic, Policy_Automatic);
      Result.Scroller.Add (Result.Text_Widget);
      return Result;
   end Create;

   --------
   -- Id --
   --------

   overriding function Id (View : Source_View) return String is
      pragma Unreferenced (View);
   begin
      return "source";
   end Id;

   -------------
   -- Accepts --
   -------------

   overriding function Accepts
     (View  : Source_View;
      Model : Aquarius.Models.Model_Interface'Class) return Boolean
   is
      pragma Unreferenced (View);
   begin
      return Model in Tree_Models.Tree_Model_Interface'Class;
   end Accepts;

   ------------
   -- Widget --
   ------------

   overriding function Widget
     (View : Source_View) return Gtk.Widget.Gtk_Widget
   is (Gtk.Widget.Gtk_Widget (View.Scroller));

   ------------
   -- Update --
   ------------

   overriding procedure Update
     (View : in out Source_View;
      Data : Aquarius.Observable.Update_Data_Interface'Class)
   is
      pragma Unreferenced (Data);
      M : constant Aquarius.Models.Model_Reference := View.Model;
   begin
      if M = null
        or else M.all not in Tree_Models.Tree_Model_Interface'Class
      then
         return;
      end if;

      declare
         Model    : Tree_Models.Tree_Model_Interface'Class renames
                      Tree_Models.Tree_Model_Interface'Class (M.all);
         Program  : constant Aquarius.Programs.Program_Tree := Model.Program;
         Messages : Aquarius.Messages.Message_List;
         Renderer : Aquarius.UI.Gtk_Views.Tree_Render.Buffer_Renderer :=
                      Aquarius.UI.Gtk_Views.Tree_Render.Create
                        (View.Text_Widget.Get_Buffer,
                         Aquarius.Rendering.Styles.Default_Styler);
      begin
         if Program = null then
            return;
         end if;

         Aquarius.Programs.Arrangements.Arrange
           (Program, Messages, Line_Length => 72);
         Aquarius.Programs.Arrangements.Render (Program, Renderer);
      end;
   end Update;

end Aquarius.UI.Gtk_Views.Source;
