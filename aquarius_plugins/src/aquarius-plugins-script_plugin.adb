with Aquarius.Plugins.Script_Plugin.Generated;

package body Aquarius.Plugins.Script_Plugin is

   procedure Create_Properties
     (Plugin  : not null access Script_Plugin_Type'Class;
      Grammar : in out     Aquarius.Grammars.Aquarius_Grammar_Record'Class);

   Global_Script_Plugin : Script_Plugin_Access;

   -----------------------
   -- Create_Properties --
   -----------------------

   procedure Create_Properties
     (Plugin  : not null access Script_Plugin_Type'Class;
      Grammar : in out     Aquarius.Grammars.Aquarius_Grammar_Record'Class)
   is
   begin
      Aquarius.Properties.Create_Property
        (Grammar, Plugin.Property_Action_Time,
         "script-action-time",
         Inherited => False,
         Has_Value => True);
      Aquarius.Properties.Create_Property
        (Grammar, Plugin.Property_Action_Context,
         "script-action-context",
         Inherited => False,
         Has_Value => True);
      Aquarius.Properties.Create_Property
        (Grammar, Plugin.Property_Parent_Node,
         "script-parent-node",
         Inherited => False,
         Has_Value => True);
      Aquarius.Properties.Create_Property
        (Grammar, Plugin.Property_Child_Node,
         "script-child-node",
         Inherited => False,
         Has_Value => True);
      Aquarius.Properties.Create_Property
        (Grammar, Plugin.Property_Action_Script,
         "script-action-script",
         Inherited => True,
         Has_Value => True);
      Aquarius.Properties.Create_Property
        (Grammar, Plugin.Property_Group_Name,
         "script-group-name",
         Inherited => True,
         Has_Value => True);
      Aquarius.Properties.Create_Property
        (Grammar, Plugin.Property_Binding_Name,
         "script-binding-name",
         Inherited => False,
         Has_Value => True);
      Aquarius.Properties.Create_Property
        (Grammar, Plugin.Property_Binding_Value,
         "script-binding-value",
         Inherited => False,
         Has_Value => True);
      Aquarius.Properties.Create_Property
        (Grammar, Plugin.Property_Expression,
         "script-expression",
         Inherited => False,
         Has_Value => True);
      Aquarius.Properties.Create_Property
        (Grammar, Plugin.Property_Plugin,
         "script-new-plugin",
         Inherited => True,
         Has_Value => True);
   end Create_Properties;

   ----------------
   -- Get_Plugin --
   ----------------

   function Get_Plugin (From_Tree : Aquarius.Programs.Program_Tree)
                       return Aquarius_Plugin
   is
   begin
      return Aquarius_Plugin (From_Tree.Property (Plugin.Property_Plugin));
   end Get_Plugin;

   ----------
   -- Load --
   ----------

   overriding
   procedure Load (Plugin  : not null access Script_Plugin_Type;
                   Grammar : Aquarius.Grammars.Aquarius_Grammar)
   is
   begin
      Load (Aquarius_Plugin_Type (Plugin.all)'Access, Grammar);

      Create_Properties (Plugin, Grammar.all);

      Aquarius.Plugins.Script_Plugin.Generated.Bind_Actions
        (Plugin.all, Grammar);

      Global_Script_Plugin := Script_Plugin_Access (Plugin);
   end Load;

   ----------
   -- Name --
   ----------

   overriding
   function Name (Plugin : Script_Plugin_Type) return String is
      pragma Unreferenced (Plugin);
   begin
      return "script";
   end Name;

   ------------
   -- Plugin --
   ------------

   function Plugin return Script_Plugin_Access is
   begin
      return Global_Script_Plugin;
   end Plugin;

   -------------
   -- Version --
   -------------

   overriding
   function Version (Plugin : Script_Plugin_Type) return String is
      pragma Unreferenced (Plugin);
   begin
      return "0.1";
   end Version;

end Aquarius.Plugins.Script_Plugin;
