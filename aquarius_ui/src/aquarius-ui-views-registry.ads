with Aquarius.Models;
with Aquarius.UI.Views.Factories;

--  Global registry of view factories and the model->view negotiation.

package Aquarius.UI.Views.Registry is

   procedure Register (Factory : Factories.View_Factory_Reference);

   function Resolve
     (Model : Aquarius.Models.Model_Reference) return View_Reference;
   --  Choose and create a view for Model (hybrid negotiation):
   --    1. if Model.Default_Viewer names a registered factory that Can_View
   --       the model, use it;
   --    2. otherwise the first registered factory that Can_View the model;
   --    3. otherwise null.
   --  The created view is bound via Set_Model. Returns null if nothing fits.

end Aquarius.UI.Views.Registry;
