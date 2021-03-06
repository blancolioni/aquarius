with Gtk.Object;

package Gtk.Builder is

   type Gtk_Builder_Record is new Gtk.Object.Gtk_Object_Record with private;
   type Gtk_Builder is access all Gtk_Builder_Record'Class;

   procedure Gtk_New
     (Builder      : out Gtk_Builder);
   --  Create a new GtkBuilder object

   procedure Initialize
     (Builder  : access Gtk_Builder_Record'Class);

   procedure Add_From_File
     (Builder   : access Gtk_Builder_Record;
      File_Name : in     UTF8_String);

   function Get_Object
     (Builder   : access Gtk_Builder_Record;
      Name      : in     UTF8_String)
      return Gtk.Object.Gtk_Object;

private

   type Gtk_Builder_Record is new Gtk.Object.Gtk_Object_Record with
     null record;

end Gtk.Builder;
