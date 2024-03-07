private with Aquarius.Properties;

package Aquarius.Plugins.EBNF is

   subtype Parent is Aquarius.Plugins.Instance;
   type Instance is new Parent with private;

private

   type Instance is new Parent with
      record
         Separator : Aquarius.Properties.Property_Type;
      end record;

   overriding procedure Load
     (This : in out Instance;
      Name : String);

   Global_EBNF_Plugin : aliased Instance;

   function Global_Plugin return access Instance'Class
   is (Global_EBNF_Plugin'Access);

end Aquarius.Plugins.EBNF;
