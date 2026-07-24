with Aquarius.Models;
with Aquarius.Observable;
use Aquarius.Observable;

--  A view displays a model inside a bubble. Views are UI-toolkit-agnostic at
--  this level; concrete toolkit views (e.g. GtkAda) implement this interface
--  and add their own way of producing a widget.
--
--  A view is a subscriber: Set_Model registers it with the model, and the
--  model's Notify drives the view's Update (refresh).

package Aquarius.UI.Views is

   type View_Interface is interface and Subscriber_Interface;

   function Id (View : View_Interface) return String is abstract;
   --  Viewer id, e.g. "text", "source", "table".

   function Accepts
     (View  : View_Interface;
      Model : Aquarius.Models.Model_Interface'Class) return Boolean
      is abstract;
   --  The view's hard requirement on the model shape (nominal check, e.g.
   --  Model in Text_Model_Interface'Class).

   procedure Set_Model
     (View  : not null access View_Interface;
      Model : Aquarius.Models.Model_Reference) is abstract;
   --  Adopt Model and subscribe to its updates. Takes an access so the view
   --  can register itself as the model's subscriber.

   function Model
     (View : View_Interface) return Aquarius.Models.Model_Reference
      is abstract;

   type View_Reference is access all View_Interface'Class;

end Aquarius.UI.Views;
