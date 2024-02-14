private with Aquarius.Programs.Parser;

package Aquarius.Plugins.Script_Plugin is

   type Script_Plugin_Type is new Aquarius_Plugin_Type with private;

   overriding
   function Name (Plugin : Script_Plugin_Type) return String;

   overriding
   function Version (Plugin : Script_Plugin_Type) return String;

   overriding
   procedure Load (Plugin  : not null access Script_Plugin_Type;
                   Grammar : Aquarius.Grammars.Aquarius_Grammar);

   function Get_Plugin (From_Tree : Aquarius.Programs.Program_Tree)
                       return Aquarius_Plugin;

private

   type Script_Plugin_Type is new Aquarius_Plugin_Type with
      record
         Ada_Grammar : Aquarius.Grammars.Aquarius_Grammar;
         Current     : Aquarius.Programs.Parser.Parse_Context;

         Property_Action_Time    : Aquarius.Properties.Property_Type;
         Property_Action_Context : Aquarius.Properties.Property_Type;
         Property_Parent_Node    : Aquarius.Properties.Property_Type;
         Property_Child_Node     : Aquarius.Properties.Property_Type;
         Property_Action_Script  : Aquarius.Properties.Property_Type;
         Property_Expression     : Aquarius.Properties.Property_Type;
         Property_Binding_Name   : Aquarius.Properties.Property_Type;
         Property_Binding_Value  : Aquarius.Properties.Property_Type;
         Property_Plugin         : Aquarius.Properties.Property_Type;
         Property_Group_Name     : Aquarius.Properties.Property_Type;
      end record;

   type Script_Plugin_Access is access all Script_Plugin_Type'Class;

   function Plugin return Script_Plugin_Access;

end Aquarius.Plugins.Script_Plugin;
