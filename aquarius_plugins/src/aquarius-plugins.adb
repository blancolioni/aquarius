with Aquarius.Syntax;

with Aquarius.Interaction;
with Aquarius.Loader;

package body Aquarius.Plugins is

   function To_Plugin_Name (Name : String) return Plugin_Map_Name;
   function To_Plugin_Group_Name (Name : String) return Plugin_Group_Name;

   ----------------------
   -- Add_Action_Group --
   ----------------------

   procedure Add_Action_Group (Plugin : in out Aquarius_Plugin_Type;
                               Group  : Aquarius.Actions.Action_Group)
   is
   begin
      Plugin.Action_Groups.Insert
        (To_Plugin_Group_Name (Aquarius.Actions.Action_Group_Name (Group)),
         Group);
   end Add_Action_Group;

   ---------------------
   -- Add_Search_Path --
   ---------------------

   procedure Add_Search_Path
     (Plugin : in out Aquarius_Plugin_Type'Class;
      Path   : String)
   is
   begin
      Plugin.Standard_Paths.Append (Path);
   end Add_Search_Path;

   ------------------------
   -- Add_Standard_Entry --
   ------------------------

   procedure Add_Standard_Entry
     (Plugin : not null access Aquarius_Plugin_Type;
      Item   : Aquarius.Entries.Table_Entry)
   is
   begin
      Plugin.Standard.Insert (Item);
   end Add_Standard_Entry;

   -------------------------
   -- Create_Action_Group --
   -------------------------

   procedure Create_Action_Group
     (Plugin     : in out Aquarius_Plugin_Type;
      Group_Name : String;
      Trigger    : Aquarius.Actions.Action_Execution_Trigger;
      Group      :    out Aquarius.Actions.Action_Group)
   is
   begin
      Aquarius.Actions.Create_Action_Group
        (Plugin.Group_List, Group_Name, Trigger, Group);
   end Create_Action_Group;

   ----------------------
   -- Get_Action_Group --
   ----------------------

   function Get_Action_Group (Plugin : Aquarius_Plugin_Type;
                              Name   : String)
                             return Aquarius.Actions.Action_Group
   is
   begin
      return Plugin.Action_Groups.Element (To_Plugin_Group_Name (Name));
   end Get_Action_Group;

   ------------------------
   -- Get_Standard_Entry --
   ------------------------

   function Get_Standard_Entry (Plugin : access Aquarius_Plugin_Type;
                                Name   : String)
                               return Aquarius.Entries.Table_Entry
   is
   begin
      return Plugin.Standard.Retrieve (Name);
   end Get_Standard_Entry;

   -----------------------
   -- Get_Standard_Type --
   -----------------------

   function Get_Standard_Type (Plugin : access Aquarius_Plugin_Type;
                               Name   : String)
                              return Aquarius.Types.Aquarius_Type
   is
      pragma Unreferenced (Plugin);
      pragma Unreferenced (Name);
   begin
      return null;
   end Get_Standard_Type;

   -------------
   -- Grammar --
   -------------

   function Grammar
     (Plugin : Aquarius_Plugin_Type'Class)
      return Aquarius.Grammars.Aquarius_Grammar
   is
   begin
      return Plugin.Grammar;
   end Grammar;

   -----------------------
   -- Have_Action_Group --
   -----------------------

   function Have_Action_Group (Plugin : Aquarius_Plugin_Type;
                               Name   : String)
                              return Boolean
   is
   begin
      return Plugin.Action_Groups.Contains (To_Plugin_Group_Name (Name));
   end Have_Action_Group;

   ----------
   -- Load --
   ----------

   procedure Load (Plugin  : not null access Aquarius_Plugin_Type;
                   Grammar : Aquarius.Grammars.Aquarius_Grammar)
   is
   begin
      Plugin.Grammar  := Grammar;
      Plugin.Standard := Aquarius.Entries.New_Symbol_Table ("standard");
      Aquarius.Properties.Create_Property
        (Grammar.all, Plugin.Change_Flag, "plugin-change-flag",
         Inherited => False,
         Has_Value => False);
   end Load;

   -----------------------
   -- Load_Program_Tree --
   -----------------------

   function Load_Program_Tree
     (Plugin : Aquarius_Plugin_Type'Class;
      Path   : String)
      return Aquarius.Programs.Program_Tree
   is
   begin
      return Aquarius.Loader.Load_From_File
        (Plugin.Grammar, Path);
   end Load_Program_Tree;

   --------------------
   -- Object_Changed --
   --------------------

   overriding
   procedure Object_Changed
     (W       : in out Aquarius_Plugin_Type;
      Item    : not null access Aquarius.Root_Aquarius_Object'Class;
      Context : not null access Aquarius.Root_Aquarius_Object'Class)
   is
      Tree : constant Aquarius.Programs.Program_Tree :=
        Aquarius.Programs.Program_Tree (Context);
      Name : constant Plugin_Map_Name := To_Plugin_Name (Tree.Name);
   begin
      if W.Change_Handlers.Contains (Name) and then
        not Tree.Has_Property (W.Change_Flag)
      then
         Tree.Set_Property (W.Change_Flag);
         declare
            Handler : constant Change_Handler :=
                        W.Change_Handlers.Element (Name);
            Interactor : constant Aquarius.Interaction.Interactor_Access :=
                           Aquarius.Interaction.Interactor_Access
                             (Tree.Property
                                (Aquarius.Properties.Interactor_Property));
         begin
            Handler (Tree, Item);
            Interactor.Update (Tree);
         end;
         Tree.Clear_Property (W.Change_Flag);
      end if;
   end Object_Changed;

   ---------------------
   -- Register_Action --
   ---------------------

   procedure Register_Action
     (Plugin      : not null access Aquarius_Plugin_Type;
      Syntax_Name : String;
      Group       : Aquarius.Actions.Action_Group;
      Position    : Rule_Position;
      Action      : Aquarius.Actions.Node_Action)
   is
      Defn : constant Aquarius.Syntax.Syntax_Tree :=
        Plugin.Grammar.Get_Definition (Syntax_Name);
   begin
      Aquarius.Actions.Set_Action
        (Defn, Group, Position,
         Aquarius.Actions.Create_Action_Execution (Action));
   exception
      when Constraint_Error =>
         raise Constraint_Error with
           "register action: " & Plugin.Grammar.Name &
           ": no such syntax rule: " &
           Syntax_Name;
   end Register_Action;

   ---------------------
   -- Register_Action --
   ---------------------

   procedure Register_Action
     (Plugin      : not null access Aquarius_Plugin_Type;
      Parent_Name : String;
      Child_Name  : String;
      Group       : Aquarius.Actions.Action_Group;
      Position    : Rule_Position;
      Action      : Aquarius.Actions.Parent_Action)
   is
   begin
      Aquarius.Actions.Set_Action
        (Source   => Plugin.Grammar.Get_Definition (Parent_Name),
         Child    => Plugin.Grammar.Get_Definition (Child_Name),
         Group    => Group,
         Position => Position,
         Action   => Aquarius.Actions.Create_Action_Execution (Action));
   exception
      when Constraint_Error =>
         raise Constraint_Error with
           "register parent/child action: " & Plugin.Grammar.Name &
           ": no such syntax rule: " &
           Parent_Name;
   end Register_Action;

   -----------------------------
   -- Register_Change_Handler --
   -----------------------------

   procedure Register_Change_Handler
     (Plugin      : not null access Aquarius_Plugin_Type'Class;
      Syntax_Name : String;
      Handler     : Change_Handler)
   is
   begin

      Plugin.Change_Handlers.Insert (To_Plugin_Name (Syntax_Name),
                                     Handler);

   exception
      when Constraint_Error =>
         raise Constraint_Error with
           "grammar " & Plugin.Grammar.Name & ": no such syntax rule: " &
           Syntax_Name;
   end Register_Change_Handler;

   -------------------
   -- Register_Rule --
   -------------------

   procedure Register_Rule
     (Plugin      : not null access Aquarius_Plugin_Type;
      Syntax_Name : String;
      Rule        : Aquarius.Formats.Aquarius_Format)
   is
   begin

      Plugin.Grammar.Get_Definition (Syntax_Name).Set_Format (Rule);
   exception
      when Constraint_Error =>
         raise Constraint_Error with
           "grammar " & Plugin.Grammar.Name & ": no such syntax rule: " &
           Syntax_Name;
   end Register_Rule;

   -------------------
   -- Register_Rule --
   -------------------

   procedure Register_Rule
     (Plugin      : not null access Aquarius_Plugin_Type;
      Syntax_Name : String;
      Rule        : Aquarius.Formats.Format_Rule)
   is
   begin
      Plugin.Register_Rule (Syntax_Name, Aquarius.Formats.Make_Format (Rule));
   end Register_Rule;

   -------------------
   -- Register_Rule --
   -------------------

   procedure Register_Rule
     (Plugin      : not null access Aquarius_Plugin_Type;
      Syntax_Name : String;
      Rule_1      : Aquarius.Formats.Format_Rule;
      Rule_2      : Aquarius.Formats.Format_Rule)
   is
   begin
      Plugin.Register_Rule (Syntax_Name,
                            Aquarius.Formats.Make_Format ([Rule_1, Rule_2]));
   end Register_Rule;

   --------------------------
   -- To_Plugin_Group_Name --
   --------------------------

   function To_Plugin_Group_Name (Name : String) return Plugin_Group_Name is
      Result : Plugin_Group_Name;
   begin
      Ada.Strings.Fixed.Move (Name, Result,
                              Drop => Ada.Strings.Right);
      return Result;
   end To_Plugin_Group_Name;

   --------------------
   -- To_Plugin_Name --
   --------------------

   function To_Plugin_Name (Name : String) return Plugin_Map_Name is
      Result : Plugin_Map_Name;
   begin
      Ada.Strings.Fixed.Move (Name, Result,
                              Drop => Ada.Strings.Right);
      return Result;
   end To_Plugin_Name;

end Aquarius.Plugins;
