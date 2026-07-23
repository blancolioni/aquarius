with Aquarius.Models;
with Aquarius.Models.Text;
with Aquarius.Programs.Models;

with Aquarius.UI.Views;
with Aquarius.UI.Views.Factories;
with Aquarius.UI.Views.Registry;

with Aquarius.UI.Gtk_Views.Text;
with Aquarius.UI.Gtk_Views.Source;

package body Aquarius.UI.Gtk_Views.Register is

   package Views renames Aquarius.UI.Views;

   type Text_Factory is
     new Views.Factories.View_Factory_Interface with null record;

   overriding function Id (Factory : Text_Factory) return String;
   overriding function Can_View
     (Factory : Text_Factory;
      Model   : Aquarius.Models.Model_Interface'Class) return Boolean;
   overriding function Create
     (Factory : Text_Factory) return Views.View_Reference;

   type Source_Factory is
     new Views.Factories.View_Factory_Interface with null record;

   overriding function Id (Factory : Source_Factory) return String;
   overriding function Can_View
     (Factory : Source_Factory;
      Model   : Aquarius.Models.Model_Interface'Class) return Boolean;
   overriding function Create
     (Factory : Source_Factory) return Views.View_Reference;

   --------
   -- Id --
   --------

   overriding function Id (Factory : Text_Factory) return String is
      pragma Unreferenced (Factory);
   begin
      return "text";
   end Id;

   --------------
   -- Can_View --
   --------------

   overriding function Can_View
     (Factory : Text_Factory;
      Model   : Aquarius.Models.Model_Interface'Class) return Boolean
   is
      pragma Unreferenced (Factory);
   begin
      return Model in Aquarius.Models.Text.Text_Model_Interface'Class;
   end Can_View;

   ------------
   -- Create --
   ------------

   overriding function Create
     (Factory : Text_Factory) return Views.View_Reference
   is
      pragma Unreferenced (Factory);
   begin
      return Views.View_Reference (Gtk_Views.Text.Create);
   end Create;

   --------
   -- Id --
   --------

   overriding function Id (Factory : Source_Factory) return String is
      pragma Unreferenced (Factory);
   begin
      return "source";
   end Id;

   --------------
   -- Can_View --
   --------------

   overriding function Can_View
     (Factory : Source_Factory;
      Model   : Aquarius.Models.Model_Interface'Class) return Boolean
   is
      pragma Unreferenced (Factory);
   begin
      return Model in Aquarius.Programs.Models.Tree_Model_Interface'Class;
   end Can_View;

   ------------
   -- Create --
   ------------

   overriding function Create
     (Factory : Source_Factory) return Views.View_Reference
   is
      pragma Unreferenced (Factory);
   begin
      return Views.View_Reference (Gtk_Views.Source.Create);
   end Create;

   ------------------
   -- Register_All --
   ------------------

   procedure Register_All is
   begin
      Views.Registry.Register (new Text_Factory);
      Views.Registry.Register (new Source_Factory);
   end Register_All;

end Aquarius.UI.Gtk_Views.Register;
