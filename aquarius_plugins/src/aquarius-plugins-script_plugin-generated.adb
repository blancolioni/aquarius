with Aquarius.Actions;
with Aquarius.Programs;
with Aquarius.Plugins.Script_Plugin.Bindings;
package body Aquarius.Plugins.Script_Plugin.Generated is
   package Bindings is
      pragma Style_Checks (Off);
      procedure Actionable_plugin_declaration_Before_list_of_declarations (Target_Actionable : not null access Aquarius.Actions.Actionable'Class;
                                                                           Child_Actionable : not null access Aquarius.Actions.Actionable'Class);
      procedure Actionable_Plugin_Declaration_After_List_Of_Declarations (Target_Actionable : not null access Aquarius.Actions.Actionable'Class;
                                                                           Child_Actionable : not null access Aquarius.Actions.Actionable'Class);
      procedure Actionable_property_declaration_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class);
      procedure Actionable_group_declaration_Before_list_of_actions (Target_Actionable : not null access Aquarius.Actions.Actionable'Class;
                                                                     Child_Actionable : not null access Aquarius.Actions.Actionable'Class);
      procedure Actionable_group_declaration_After_list_of_actions (Target_Actionable : not null access Aquarius.Actions.Actionable'Class;
                                                                    Child_Actionable : not null access Aquarius.Actions.Actionable'Class);
      procedure Actionable_group_declaration_Before
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class);
      procedure Actionable_group_declaration_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class);
      procedure Actionable_After_File_Reference (Target_Actionable : not null access Aquarius.Actions.Actionable'Class);
      procedure Actionable_After_Value_Declaration (Target_Actionable : not null access Aquarius.Actions.Actionable'Class);
   end Bindings;
   package body Bindings is
      procedure Actionable_plugin_declaration_Before_list_of_declarations (Target_Actionable : not null access Aquarius.Actions.Actionable'Class;
                                                                           Child_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Aquarius.Plugins.Script_Plugin.Bindings.plugin_declaration_Before_list_of_declarations (Aquarius.Programs.Program_Tree
           (Target_Actionable), Aquarius.Programs.Program_Tree
           (Child_Actionable));
      end Actionable_plugin_declaration_Before_list_of_declarations;
      procedure Actionable_Plugin_Declaration_after_List_Of_Declarations (Target_Actionable : not null access Aquarius.Actions.Actionable'Class;
                                                                           Child_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Aquarius.Plugins.Script_Plugin.Bindings.Plugin_Declaration_After_List_Of_Declarations (Aquarius.Programs.Program_Tree
           (Target_Actionable), Aquarius.Programs.Program_Tree
           (Child_Actionable));
      end Actionable_Plugin_Declaration_After_List_Of_Declarations;
      procedure Actionable_property_declaration_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Aquarius.Plugins.Script_Plugin.Bindings.property_declaration_After (Aquarius.Programs.Program_Tree
           (Target_Actionable));
      end Actionable_property_declaration_After;
      procedure Actionable_group_declaration_Before_list_of_actions (Target_Actionable : not null access Aquarius.Actions.Actionable'Class;
                                                                     Child_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Aquarius.Plugins.Script_Plugin.Bindings.Group_Declaration_After_Group_Header (Aquarius.Programs.Program_Tree
           (Target_Actionable), Aquarius.Programs.Program_Tree
           (Child_Actionable));
      end Actionable_group_declaration_Before_list_of_actions;
      procedure Actionable_group_declaration_After_list_of_actions (Target_Actionable : not null access Aquarius.Actions.Actionable'Class;
                                                                    Child_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Aquarius.Plugins.Script_Plugin.Bindings.group_declaration_After_list_of_actions (Aquarius.Programs.Program_Tree
           (Target_Actionable), Aquarius.Programs.Program_Tree
           (Child_Actionable));
      end Actionable_group_declaration_After_list_of_actions;
      procedure Actionable_group_declaration_Before
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Aquarius.Plugins.Script_Plugin.Bindings.group_declaration_Before (Aquarius.Programs.Program_Tree
           (Target_Actionable));
      end Actionable_group_declaration_Before;
      procedure Actionable_group_declaration_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Aquarius.Plugins.Script_Plugin.Bindings.group_declaration_After (Aquarius.Programs.Program_Tree
           (Target_Actionable));
      end Actionable_group_declaration_After;
      procedure Actionable_After_File_Reference
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Aquarius.Plugins.Script_Plugin.Bindings.After_Action_File_Reference (Aquarius.Programs.Program_Tree
                                                                              (Target_Actionable));
      end Actionable_After_File_Reference;
      procedure Actionable_After_Value_Declaration
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Aquarius.Plugins.Script_Plugin.Bindings.After_Value_Declaration (Aquarius.Programs.Program_Tree
                                                                              (Target_Actionable));
      end Actionable_After_Value_Declaration;
   end Bindings;
   procedure Bind_Actions (Plugin : in out Script_Plugin_Type;
                           Grammar : in Aquarius.Grammars.Aquarius_Grammar) is
      Parser : Aquarius.Actions.Action_Group;
   begin
      Grammar.Add_Action_Group ("parser",
                                Aquarius.Actions.Parse_Trigger, Parser);
      Plugin.Register_Action ("plugin_declaration",
                              "list_of_declarations", Parser,
                              Aquarius.Before,
                              Bindings.Actionable_plugin_declaration_Before_list_of_declarations'access);
      Plugin.Register_Action ("plugin_declaration",
                              "list_of_declarations", Parser,
                              Aquarius.After,
                              Bindings.Actionable_Plugin_Declaration_After_List_Of_Declarations'Access);
      Plugin.Register_Action ("property_declaration", Parser,
                              Aquarius.After,
                              Bindings.Actionable_property_declaration_After'access);
      Plugin.Register_Action ("group_declaration", "group_header",
                              Parser, Aquarius.After,
                              Bindings.Actionable_group_declaration_Before_list_of_actions'access);
      Plugin.Register_Action ("group_body", "list_of_actions",
                              Parser, Aquarius.After,
                              Bindings.Actionable_group_declaration_After_list_of_actions'access);
      Plugin.Register_Action ("group_declaration", Parser,
                              Aquarius.Before,
                              Bindings.Actionable_group_declaration_Before'access);
      Plugin.Register_Action ("group_declaration", Parser,
                              Aquarius.After,
                              Bindings.Actionable_group_declaration_After'access);
      Plugin.Register_Action ("action_file_reference", Parser,
                              Aquarius.After,
                              Bindings.Actionable_After_File_Reference'Access);
      Plugin.Register_Action ("value_declaration", Parser,
                              Aquarius.After,
                              Bindings.Actionable_After_Value_Declaration'Access);
   end Bind_Actions;
end Aquarius.Plugins.Script_Plugin.Generated;
