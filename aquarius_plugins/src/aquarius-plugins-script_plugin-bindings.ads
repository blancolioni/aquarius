with Aquarius.Programs;

package Aquarius.Plugins.Script_Plugin.Bindings is

   procedure Plugin_Declaration_Before_List_Of_Declarations
     (Parent : Aquarius.Programs.Program_Tree;
      Child  : Aquarius.Programs.Program_Tree);

   procedure Plugin_Declaration_After_List_Of_Declarations
     (Parent : Aquarius.Programs.Program_Tree;
      Child  : Aquarius.Programs.Program_Tree);

   procedure Property_Declaration_After
     (Item : Aquarius.Programs.Program_Tree);

   procedure Group_Declaration_Before
     (Item : Aquarius.Programs.Program_Tree);

   procedure Group_Declaration_After
     (Item : Aquarius.Programs.Program_Tree);

   procedure Group_Declaration_After_Group_Header
     (Parent : Aquarius.Programs.Program_Tree;
      Child  : Aquarius.Programs.Program_Tree);

   procedure Group_Declaration_After_List_Of_Actions
     (Parent : Aquarius.Programs.Program_Tree;
      Child  : Aquarius.Programs.Program_Tree);

   procedure After_Action_File_Reference
     (Item : Aquarius.Programs.Program_Tree);

   procedure After_Value_Declaration
     (Item : Aquarius.Programs.Program_Tree);

end Aquarius.Plugins.Script_Plugin.Bindings;
