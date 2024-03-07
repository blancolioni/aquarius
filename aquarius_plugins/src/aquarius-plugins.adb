package body Aquarius.Plugins is

   ----------------------
   -- Add_Action_Group --
   ----------------------

   procedure Add_Action_Group
     (This   : in out Instance;
      Group  : Aquarius.Actions.Action_Group)
   is
   begin
      This.Action_Groups.Insert
        (Aquarius.Actions.Action_Group_Name (Group), Group);
   end Add_Action_Group;

   ---------------------
   -- Add_Search_Path --
   ---------------------

   procedure Add_Search_Path
     (This : in out Instance;
      Path : String)
   is
   begin
      This.Standard_Paths.Append (Path);
   end Add_Search_Path;

   -------------------------
   -- Create_Action_Group --
   -------------------------

   function New_Action_Group
     (This    : in out Instance;
      Name    : String;
      Trigger : Aquarius.Actions.Action_Execution_Trigger)
      return Aquarius.Actions.Action_Group
   is
      Group : Aquarius.Actions.Action_Group;
   begin
      Aquarius.Actions.Create_Action_Group
        (This.Group_List, Name, Trigger, Group);
      This.Grammar.Add_Action_Group (Group);
      This.Add_Action_Group (Group);
      return Group;
   end New_Action_Group;

   ----------------------
   -- Get_Action_Group --
   ----------------------

   function Get_Action_Group
     (This   : Instance;
      Name   : String)
      return Aquarius.Actions.Action_Group
   is
   begin
      return This.Action_Groups.Element (Name);
   end Get_Action_Group;

   -------------
   -- Grammar --
   -------------

   function Grammar
     (This : Instance)
      return Aquarius.Grammars.Aquarius_Grammar
   is
   begin
      return This.Grammar;
   end Grammar;

   -----------------------
   -- Have_Action_Group --
   -----------------------

   function Have_Action_Group
     (This : Instance;
      Name   : String)
      return Boolean
   is
   begin
      return This.Action_Groups.Contains (Name);
   end Have_Action_Group;

   ---------------------
   -- Register_Action --
   ---------------------

   procedure Register_Action
     (This        : in out Instance;
      Syntax_Name : String;
      Group       : Aquarius.Actions.Action_Group;
      Position    : Rule_Position;
      Action      : Aquarius.Actions.Node_Action)
   is
   begin
      Aquarius.Actions.Set_Action
        (Source   => This.Grammar.Get_Definition (Syntax_Name),
         Group    => Group,
         Position => Position,
         Action   => Aquarius.Actions.Create_Action_Execution (Action));
   exception
      when Constraint_Error =>
         raise Constraint_Error with
           "register action: " & This.Grammar.Name &
           ": no such syntax rule: " &
           Syntax_Name;
   end Register_Action;

   ---------------------
   -- Register_Action --
   ---------------------

   procedure Register_Action
     (This        : in out Instance;
      Parent_Name : String;
      Child_Name  : String;
      Group       : Aquarius.Actions.Action_Group;
      Position    : Rule_Position;
      Action      : Aquarius.Actions.Parent_Action)
   is
   begin
      Aquarius.Actions.Set_Action
        (Source   => This.Grammar.Get_Definition (Parent_Name),
         Child    => This.Grammar.Get_Definition (Child_Name),
         Group    => Group,
         Position => Position,
         Action   => Aquarius.Actions.Create_Action_Execution (Action));
   exception
      when Constraint_Error =>
         raise Constraint_Error with
           "register parent/child action: " & This.Grammar.Name &
           ": no such syntax rule: " &
           Parent_Name;
   end Register_Action;

   -------------------
   -- Register_Rule --
   -------------------

   procedure Register_Rule
     (This        : in out Instance;
      Syntax_Name : String;
      Rule        : Aquarius.Formats.Aquarius_Format)
   is
   begin
      This.Grammar.Get_Definition (Syntax_Name).Set_Format (Rule);
   exception
      when Constraint_Error =>
         raise Constraint_Error with
           "grammar " & This.Grammar.Name & ": no such syntax rule: " &
           Syntax_Name;
   end Register_Rule;

   -------------------
   -- Register_Rule --
   -------------------

   procedure Register_Rule
     (This        : in out Instance;
      Syntax_Name : String;
      Rule        : Aquarius.Formats.Format_Rule)
   is
   begin
      This.Register_Rule (Syntax_Name, Aquarius.Formats.Make_Format (Rule));
   end Register_Rule;

   -------------------
   -- Register_Rule --
   -------------------

   procedure Register_Rule
     (This        : in out Instance;
      Syntax_Name : String;
      Rule_1      : Aquarius.Formats.Format_Rule;
      Rule_2      : Aquarius.Formats.Format_Rule)
   is
   begin
      This.Register_Rule
        (Syntax_Name,
         Aquarius.Formats.Make_Format ([Rule_1, Rule_2]));
   end Register_Rule;

end Aquarius.Plugins;
