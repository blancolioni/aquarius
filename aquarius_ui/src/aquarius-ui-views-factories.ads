with Aquarius.Models;

--  A factory produces empty views of one kind. The registry uses Can_View to
--  test whether a factory's views can display a given model.

package Aquarius.UI.Views.Factories is

   type View_Factory_Interface is interface;

   function Id (Factory : View_Factory_Interface) return String is abstract;
   --  Viewer id of the views this factory creates (matched against a model's
   --  Default_Viewer).

   function Can_View
     (Factory : View_Factory_Interface;
      Model   : Aquarius.Models.Model_Interface'Class) return Boolean
      is abstract;

   function Create
     (Factory : View_Factory_Interface) return View_Reference is abstract;

   type View_Factory_Reference is access all View_Factory_Interface'Class;

end Aquarius.UI.Views.Factories;
