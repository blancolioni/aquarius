private with Aqua.Server;

package Aquarius.Plugins.Aqua_Plugin is

   subtype Parent is Aquarius.Plugins.Instance;
   type Instance is new Parent with private;

private

   type Instance is new Parent with
      record
         Server : Aqua.Server.Reference;
      end record;

   overriding procedure Load
     (This : in out Instance;
      Name : String);

   procedure Bind_Action
     (This        : in out Instance;
      Group       : Aquarius.Actions.Action_Group;
      Position    : Rule_Position;
      Parent_Name : String;
      Child_Name  : String;
      Address     : Aqua.Address_Type);

end Aquarius.Plugins.Aqua_Plugin;
