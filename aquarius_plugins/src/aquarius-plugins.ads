private with Ada.Containers.Indefinite_Doubly_Linked_Lists;
private with Aquarius.Names;
private with WL.String_Maps;

with Aquarius.Actions;
with Aquarius.Formats;
with Aquarius.Grammars;

package Aquarius.Plugins is

   subtype Parent is Root_Aquarius_Object;
   type Instance is abstract new Parent with private;
   type Reference is access all Instance'Class;

   function Version (This : Instance) return String;
   function Description (This : Instance) return String;

   procedure Load
     (This : in out Instance;
      Name : String)
   is abstract;

   procedure Report_State
     (This : Instance)
   is null;

   procedure Add_Search_Path
     (This : in out Instance;
      Path : String);

   function Grammar
     (This : Instance)
      return Aquarius.Grammars.Aquarius_Grammar;

   function New_Action_Group
     (This    : in out Instance;
      Name    : String;
      Trigger : Aquarius.Actions.Action_Execution_Trigger)
      return Aquarius.Actions.Action_Group;

   procedure Add_Action_Group
     (This   : in out Instance;
      Group  : Aquarius.Actions.Action_Group)
     with Pre => not This.Have_Action_Group
       (Aquarius.Actions.Action_Group_Name (Group));

   function Have_Action_Group
     (This : Instance;
      Name   : String)
      return Boolean;

   function Get_Action_Group
     (This   : Instance;
      Name   : String)
      return Aquarius.Actions.Action_Group
     with Pre => This.Have_Action_Group (Name);

   procedure Register_Rule
     (This        : in out Instance;
      Syntax_Name : String;
      Rule        : Aquarius.Formats.Aquarius_Format);

   procedure Register_Rule
     (This        : in out Instance;
      Syntax_Name : String;
      Rule        : Aquarius.Formats.Format_Rule);

   procedure Register_Rule
     (This        : in out Instance;
      Syntax_Name : String;
      Rule_1      : Aquarius.Formats.Format_Rule;
      Rule_2      : Aquarius.Formats.Format_Rule);

   procedure Register_Action
     (This        : in out Instance;
      Syntax_Name : String;
      Group       : Aquarius.Actions.Action_Group;
      Position    : Rule_Position;
      Action      : Aquarius.Actions.Node_Action);

   procedure Register_Action
     (This        : in out Instance;
      Parent_Name : String;
      Child_Name  : String;
      Group       : Aquarius.Actions.Action_Group;
      Position    : Rule_Position;
      Action      : Aquarius.Actions.Parent_Action);

private

   subtype Plugin_Group_Name is String (1 .. 20);

   package Group_Maps is
     new WL.String_Maps (Aquarius.Actions.Action_Group,
                         Aquarius.Actions."=");

   package String_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists (String);

   type Instance is abstract new Parent with
      record
         Name            : Aquarius.Names.Aquarius_Name;
         Description     : Aquarius.Names.Aquarius_Name;
         Version         : Aquarius.Names.Aquarius_Name;
         Grammar         : Aquarius.Grammars.Aquarius_Grammar;
         Action_Groups   : Group_Maps.Map;
         Group_List      : Aquarius.Actions.Action_Group_List;
         Standard_Paths  : String_Lists.List;
      end record;

   overriding function Name (This : Instance) return String
   is (Aquarius.Names.To_String (This.Name));

   function Version (This : Instance) return String
   is (Aquarius.Names.To_String (This.Version));

   function Description (This : Instance) return String
   is (Aquarius.Names.To_String (This.Description));

end Aquarius.Plugins;
