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

end Aquarius.Plugins.Aqua_Plugin;
