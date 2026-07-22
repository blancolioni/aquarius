with Aquarius.Observable;
use Aquarius.Observable;

--  Root of the bubble data-model hierarchy. A model is the data shown in a
--  bubble; it is observable so viewers can refresh when it changes. Concrete
--  shape interfaces (text, table, image, ...) extend Model_Interface.

package Aquarius.Models is

   type Model_Interface is interface and Publisher_Interface;

   function Kind (Model : Model_Interface) return String is abstract;
   --  Soft capability tag used for viewer discovery, e.g. "text", "table",
   --  "tree", "image".

   function Default_Viewer (Model : Model_Interface) return String is abstract;
   --  Id of the viewer this model would prefer, or "" to let the registry
   --  choose. Only a suggestion: a viewer still confirms it can display the
   --  model (see Aquarius.UI.Views).

   type Model_Reference is access all Model_Interface'Class;

end Aquarius.Models;
