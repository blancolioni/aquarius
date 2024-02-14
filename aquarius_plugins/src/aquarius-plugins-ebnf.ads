with Aquarius.Properties;

package Aquarius.Plugins.EBNF is

   type EBNF_Plugin is new Aquarius_Plugin_Type with private;

   overriding
   function Name (Plugin : EBNF_Plugin) return String;

   overriding
   function Version (Plugin : EBNF_Plugin) return String;

   overriding
   procedure Load (Plugin  : not null access EBNF_Plugin;
                   Grammar : Aquarius.Grammars.Aquarius_Grammar);

private

   type EBNF_Plugin is new Aquarius_Plugin_Type with
      record
         Separator : Aquarius.Properties.Property_Type;
      end record;

   function Global_Plugin return access EBNF_Plugin'Class;

end Aquarius.Plugins.EBNF;
