with Ada.Containers.Vectors;

package body Aquarius.UI.Views.Registry is

   use type Aquarius.Models.Model_Reference;

   package Factory_Vectors is new Ada.Containers.Vectors
     (Positive, Factories.View_Factory_Reference, Factories."=");

   Known : Factory_Vectors.Vector;

   --------------
   -- Register --
   --------------

   procedure Register (Factory : Factories.View_Factory_Reference) is
   begin
      Known.Append (Factory);
   end Register;

   -------------
   -- Resolve --
   -------------

   function Resolve
     (Model : Aquarius.Models.Model_Reference) return View_Reference
   is
      View : View_Reference := null;
   begin
      if Model = null then
         return null;
      end if;

      --  1. honour the model's suggested viewer
      declare
         Suggested : constant String := Model.Default_Viewer;
      begin
         if Suggested /= "" then
            for Factory of Known loop
               if Factory.Id = Suggested
                 and then Factory.Can_View (Model.all)
               then
                  View := Factory.Create;
                  exit;
               end if;
            end loop;
         end if;
      end;

      --  2. otherwise, first factory that can view it
      if View = null then
         for Factory of Known loop
            if Factory.Can_View (Model.all) then
               View := Factory.Create;
               exit;
            end if;
         end loop;
      end if;

      --  3. bind the model
      if View /= null then
         View.Set_Model (Model);
      end if;

      return View;
   end Resolve;

end Aquarius.UI.Views.Registry;
