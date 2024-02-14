with Aquarius.Actions;
with Aquarius.Formats;
with Aquarius.Grammars;
with Aquarius.Programs;
with Aquarius.Properties;

with Aquarius.Entries;
with Aquarius.Types;

private with Ada.Containers.Hashed_Maps;
private with Ada.Containers.Indefinite_Doubly_Linked_Lists;
private with Ada.Containers.Indefinite_Hashed_Maps;
private with Ada.Strings.Fixed.Hash;

package Aquarius.Plugins is

   type Aquarius_Plugin_Type is
     abstract new Root_Aquarius_Object
--     and Aquarius.Programs.Root_Program_Tree_Store
     and Watcher
     with private;

   type Aquarius_Plugin is access all Aquarius_Plugin_Type'Class;

   not overriding
   function Version (Plugin : Aquarius_Plugin_Type)
                    return String
      is abstract;

   --  overriding
   --  function Environment_Name
   --    (Plugin : Aquarius_Plugin_Type)
   --     return String;

   procedure Load (Plugin  : not null access Aquarius_Plugin_Type;
                   Grammar : Aquarius.Grammars.Aquarius_Grammar);

   procedure Report_State
     (Plugin : Aquarius_Plugin_Type)
   is null;

   procedure Add_Search_Path
     (Plugin : in out Aquarius_Plugin_Type'Class;
      Path   : String);

   --  overriding function Get_Program
   --    (Plugin    : Aquarius_Plugin_Type;
   --     File_Name : String)
   --     return Aquarius.Programs.Program_Tree;

   function Get_Standard_Entry (Plugin : access Aquarius_Plugin_Type;
                                Name   : String)
                               return Aquarius.Entries.Table_Entry;

   function Get_Standard_Type (Plugin : access Aquarius_Plugin_Type;
                               Name   : String)
                              return Aquarius.Types.Aquarius_Type;

   function Grammar
     (Plugin : Aquarius_Plugin_Type'Class)
      return Aquarius.Grammars.Aquarius_Grammar;

   procedure Create_Action_Group
     (Plugin     : in out Aquarius_Plugin_Type;
      Group_Name : String;
      Trigger    : Aquarius.Actions.Action_Execution_Trigger;
      Group      :    out Aquarius.Actions.Action_Group);

   procedure Add_Action_Group (Plugin : in out Aquarius_Plugin_Type;
                               Group  : Aquarius.Actions.Action_Group)
   with Pre => not Plugin.Have_Action_Group
     (Aquarius.Actions.Action_Group_Name (Group));

   function Have_Action_Group (Plugin : Aquarius_Plugin_Type;
                               Name   : String)
                              return Boolean;
   function Get_Action_Group (Plugin : Aquarius_Plugin_Type;
                              Name   : String)
                             return Aquarius.Actions.Action_Group
     with Pre => Plugin.Have_Action_Group (Name);

   procedure Register_Rule
     (Plugin      : not null access Aquarius_Plugin_Type;
      Syntax_Name : String;
      Rule        : Aquarius.Formats.Aquarius_Format);

   procedure Register_Rule
     (Plugin      : not null access Aquarius_Plugin_Type;
      Syntax_Name : String;
      Rule        : Aquarius.Formats.Format_Rule);

   procedure Register_Rule
     (Plugin      : not null access Aquarius_Plugin_Type;
      Syntax_Name : String;
      Rule_1      : Aquarius.Formats.Format_Rule;
      Rule_2      : Aquarius.Formats.Format_Rule);

   procedure Register_Action
     (Plugin      : not null access Aquarius_Plugin_Type;
      Syntax_Name : String;
      Group       : Aquarius.Actions.Action_Group;
      Position    : Rule_Position;
      Action      : Aquarius.Actions.Node_Action);

   procedure Register_Action
     (Plugin      : not null access Aquarius_Plugin_Type;
      Parent_Name : String;
      Child_Name  : String;
      Group       : Aquarius.Actions.Action_Group;
      Position    : Rule_Position;
      Action      : Aquarius.Actions.Parent_Action);

   type Change_Handler is access
     procedure (Item   : not null access
                  Aquarius.Programs.Program_Tree_Type'Class;
                Object : not null access Root_Aquarius_Object'Class);

   procedure Register_Change_Handler
     (Plugin      : not null access Aquarius_Plugin_Type'Class;
      Syntax_Name : String;
      Handler     : Change_Handler);

   procedure On_Entry_Change
     (Plugin   : not null access Aquarius_Plugin_Type;
      Node     : Aquarius.Programs.Program_Tree)
   is null;

   procedure Add_Standard_Entry
     (Plugin : not null access Aquarius_Plugin_Type;
      Item   : Aquarius.Entries.Table_Entry);

   procedure Add_Standard_Type
     (Plugin : not null access Aquarius_Plugin_Type;
      Name   : String;
      Item   : Aquarius.Types.Aquarius_Type)
   is null;

private

   subtype Plugin_Map_Name is String (1 .. 20);

   package Change_Handler_Map is
      new Ada.Containers.Hashed_Maps
     (Key_Type        => Plugin_Map_Name,
      Element_Type    => Change_Handler,
      Hash            => Ada.Strings.Fixed.Hash,
      Equivalent_Keys => "=");

   subtype Plugin_Group_Name is String (1 .. 20);

   package Group_Map is
      new Ada.Containers.Hashed_Maps
     (Key_Type        => Plugin_Group_Name,
      Element_Type    => Aquarius.Actions.Action_Group,
      Hash            => Ada.Strings.Fixed.Hash,
      Equivalent_Keys => "=",
      "="             => Aquarius.Actions."=");

   package Program_Tree_Maps is
     new Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => String,
        Element_Type    => Aquarius.Programs.Program_Tree,
        Hash            => Ada.Strings.Fixed.Hash,
        Equivalent_Keys => "=",
        "="             => Aquarius.Programs."=");

   package String_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists (String);

   type Aquarius_Plugin_Type is
     abstract new Root_Aquarius_Object and Watcher with
     --  and Aquarius.Programs.Root_Program_Tree_Store with
      record
         Grammar         : Aquarius.Grammars.Aquarius_Grammar;
         Standard        : Aquarius.Entries.Symbol_Table;
         Change_Handlers : Change_Handler_Map.Map;
         Action_Groups   : Group_Map.Map;
         Group_List      : Aquarius.Actions.Action_Group_List;
         Change_Flag     : Aquarius.Properties.Property_Type;
         Loaded_Programs : Program_Tree_Maps.Map;
         Have_Menu       : Boolean := False;
         Standard_Paths  : String_Lists.List;
      end record;

   overriding
   procedure Object_Changed
     (W       : in out Aquarius_Plugin_Type;
      Item    : not null access Aquarius.Root_Aquarius_Object'Class;
      Context : not null access Aquarius.Root_Aquarius_Object'Class);

   function Load_Program_Tree
     (Plugin : Aquarius_Plugin_Type'Class;
      Path : String)
     return Aquarius.Programs.Program_Tree;

   --  overriding
   --  function Environment_Name
   --    (Plugin : Aquarius_Plugin_Type)
   --     return String
   --  is (Aquarius_Plugin_Type'Class (Plugin).Name);

end Aquarius.Plugins;
