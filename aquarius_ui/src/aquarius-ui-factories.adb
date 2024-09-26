with Aquarius.Programs;
with Aquarius.UI.Models;

package body Aquarius.UI.Factories is

   type Instance is new Aquarius.UI.Editor.Model_Factory_Interface with
      record
         null;
      end record;

   overriding function Create
     (This    : Instance;
      Program : Aquarius.Programs.Program_Tree)
      return Aquarius.UI.Editor.Model_Reference;

   function Editor_Model_Factory
     return Aquarius.UI.Editor.Model_Factory_Interface'Class
   is (Instance'(null record));

   ------------
   -- Create --
   ------------

   overriding function Create
     (This    : Instance;
      Program : Aquarius.Programs.Program_Tree)
      return Aquarius.UI.Editor.Model_Reference
   is
   begin
      return Aquarius.UI.Models.Create_Model (Program);
   end Create;

end Aquarius.UI.Factories;
