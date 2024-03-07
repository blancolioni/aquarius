private with Ada.Containers.Indefinite_Hashed_Maps;
private with Ada.Strings.Fixed.Hash;
private with Aqua;

private with WL.String_Maps;

with Aquarius.Actions;
with Aquarius.Entries;
with Aquarius.Formats;
with Aquarius.Layout;
with Aquarius.Messages;
with Aquarius.Names;
with Aquarius.Source;
with Aquarius.Syntax;
with Aquarius.Tokens;
pragma Elaborate_All (Aquarius.Tokens);
with Aquarius.Types;

with Aquarius.Trees;

package Aquarius.Programs is

   type Local_Environment_Interface is interface;

   function Environment_Name
     (Local : Local_Environment_Interface)
      return String
      is abstract;

   type Program_Tree_Type is
     new Aquarius.Trees.Root_Tree_Type
     and Aquarius.Actions.Actionable
     and Aquarius.Entries.Entry_Property_Interface
     and Aquarius.Types.Type_Property_Interface
     --  and Aquarius.Source.Has_Source_Position_Interface
     with private;

   type Program_Tree is access all Program_Tree_Type'Class;

   type Array_Of_Program_Trees is array (Positive range <>) of Program_Tree;
   Empty_Program_Tree_Array : Array_Of_Program_Trees (1 .. 0);

   function New_Program_Tree
     (Syntax   : Aquarius.Syntax.Syntax_Tree)
      return Program_Tree;

   function New_Program
     (Syntax   : Aquarius.Syntax.Syntax_Tree;
      Source   : Aquarius.Source.Source_File)
      return Program_Tree;

   function New_Program_Root
     (Syntax   : Aquarius.Syntax.Syntax_Tree;
      Source   : Aquarius.Source.Source_File;
      Environment : not null access Local_Environment_Interface'Class)
      return Program_Tree;

   function New_Error_Tree
     (Position : Aquarius.Source.Source_Position;
      Syntax   : Aquarius.Syntax.Syntax_Tree;
      Message  : String)
      return Program_Tree;

   --  Syntax_Index: return the child number of the tree, excluding
   --  repeated nodes.  I.e. the child number of the syntax of the
   --  tree.  Zero is never returned, except for the root node.
   function Syntax_Index (Tree : not null access Program_Tree_Type)
                         return Natural;

   procedure Set_Source_Position
     (Item : in out Program_Tree_Type;
      Pos  : Aquarius.Source.Source_Position);

   procedure Expand (Item : not null access Program_Tree_Type);
   --  Expand: creates all required children of a tree

   procedure Expand_All (Item : in out Program_Tree_Type);
   --  Expand_All: creates all children of a tree

   function First_Program_Child (Item : Program_Tree_Type)
                                return Program_Tree;

   procedure Create_Symbol_Table (Tree : in out Program_Tree_Type);
   function Symbol_Table (Tree : Program_Tree_Type)
                         return Aquarius.Entries.Symbol_Table;
   procedure Set_Symbol_Table (Tree  : in out Program_Tree_Type;
                               Table : Aquarius.Entries.Symbol_Table);

   function Program_Root
     (Item : not null access Program_Tree_Type'Class)
      return Program_Tree;

   function Program_Root_Node
     (Item : Program_Tree_Type'Class)
      return Program_Tree_Type'Class;

   function Program_Parent (Item : Program_Tree_Type'Class)
                           return Program_Tree;
   function Program_Left (Item : Program_Tree_Type'Class)
                         return Program_Tree;
   function Program_Right (Item : Program_Tree_Type'Class)
                          return Program_Tree;

   function Program_Child (Item  : Program_Tree_Type;
                           Index : Positive)
                           return Program_Tree
   is (Program_Tree (Item.Child (Index)));

   function Program_Child (Item  : Program_Tree_Type;
                           Name  : String;
                           Index : Positive := 1)
                          return Program_Tree;

   --  Direct_Children: return an array of the children of
   --  Item that for which Child.Name = Name
   function Direct_Children (Item : Program_Tree_Type;
                             Name : String)
                            return Array_Of_Program_Trees;

   --  Direct_Children (2)
   --  Return an array of all named children of Item
   --  Setting Skip_Separators to False includes separator children
   function Direct_Children (Item            : Program_Tree_Type;
                             Skip_Separators : Boolean := True)
                            return Array_Of_Program_Trees;

   function Concatenate_Children (Item : Program_Tree_Type) return String;
   --  Finds all named children of Item and concatenates their text
   --  representations

   function Chosen_Tree (Item : not null access Program_Tree_Type)
                        return Program_Tree;
   --  Chosen_Tree: Item must refer to a choice tree
   --  (not counting single-item sequence children).
   --  Return the first named child of the choice that
   --  was made.

   function Internal_Tree (Item : Program_Tree_Type'Class) return Boolean;
   --  an internal tree is not mentioned directly in the EBNF

   overriding
   function Has_Named_Property (Item : Program_Tree_Type;
                                Name : String)
                               return Boolean;

--     function Has_Property
--       (Item : Program_Tree_Type;
--        Name : String)
--        return Boolean;
--
--     function Property
--       (Item : Program_Tree_Type;
--        Name : String)
--        return access Root_Aquarius_Object'Class;
--
--     procedure Set_Property
--       (Item : in out Program_Tree_Type;
--        Name : String;
--        Value : access Root_Aquarius_Object'Class);

   overriding
   function Text (Item : Program_Tree_Type) return String;

   overriding
   function Standard_Text (Item : Program_Tree_Type) return String;

   function Local_Environment
     (Item : in out Program_Tree_Type'Class)
      return access Local_Environment_Interface'Class;

   function Local_Environment_Name
     (Item : in out Program_Tree_Type'Class)
      return String
   is (Item.Local_Environment.Environment_Name);

   function Layout_Start_Position (Item : Program_Tree_Type)
                                  return Aquarius.Layout.Position;
   function Layout_End_Position (Item : Program_Tree_Type)
                                return Aquarius.Layout.Position;
   function Layout_Start_Column (Item : Program_Tree_Type)
                                  return Aquarius.Layout.Column_Number;
   function Layout_End_Column (Item : Program_Tree_Type)
                               return Aquarius.Layout.Column_Number;

   function Layout_Line (Item : Program_Tree_Type)
                         return Aquarius.Layout.Line_Number;

   function Contains_Position (Item : Program_Tree_Type;
                               Position : Aquarius.Layout.Position)
                               return Boolean;

   procedure Set_Layout_Position
     (Item : in out Program_Tree_Type;
      Pos  : Aquarius.Layout.Position;
      Line : Aquarius.Layout.Line_Number;
      Col  : Aquarius.Layout.Column_Number);

   function Layout_Length (Item : Program_Tree_Type)
                          return Aquarius.Layout.Count;

   function Source (Item : Program_Tree_Type'Class)
                   return Aquarius.Source.Source_File;

   function Source_File_Name
     (Item : Program_Tree_Type'Class)
      return String;
   --  return the name of the file that produced this tree,
   --  after stripping the path

   function Source_Directory
     (Item : Program_Tree_Type'Class)
      return String;
   --  return the full path to the directory in which the source file
   --  for this program tree was found

   function Find_Node_At
     (Top      : not null access Program_Tree_Type'Class;
      Location : Aquarius.Layout.Position)
      return Program_Tree;
   --  Search for a node at Location within the original source file

   function Find_Node_At
     (Parent   : not null access Program_Tree_Type'Class;
      Location : Aquarius.Source.Source_Position)
      return Program_Tree;

   function Find_Local_Node_At
     (Top      : not null access Program_Tree_Type'Class;
      Location : Aquarius.Layout.Position)
      return Program_Tree;
   --  Search for a node placed at Location in the local rendered context

   function Find_Local_First_Node_At_Line
     (Top  : not null access Program_Tree_Type'Class;
      Line : Aquarius.Layout.Line_Number)
      return Program_Tree;

   function Find_Node_Containing
     (Top      : not null access Program_Tree_Type'Class;
      Location : Aquarius.Layout.Position)
      return Program_Tree;
   --  Find a child of Top which contains the given Location and return it.
   --  Return null if no such child exists.

   procedure Set_Vertical_Gap_Before
     (Item  : in out Program_Tree_Type'Class;
      Gap   : Aquarius.Layout.Count);

   function Vertical_Gap_Before
     (Item : Program_Tree_Type'Class)
      return Aquarius.Layout.Count;

   function Separator_New_Line (Item : Program_Tree_Type'Class)
                               return Boolean;
   procedure Set_Separator_New_Line (Item : in out Program_Tree_Type'Class);

   function Has_Soft_New_Line_Rule_Before
     (Item : Program_Tree_Type'Class)
      return Boolean;

   function Has_Soft_New_Line_Rule_After
     (Item : Program_Tree_Type'Class)
      return Boolean;

   function Rules
     (Item : Program_Tree_Type'Class)
      return Aquarius.Formats.Immediate_Rules;

   function Soft_New_Line (Item : Program_Tree_Type'Class)
                          return Boolean;

   procedure Set_Soft_New_Line (Item : in out Program_Tree_Type'Class);

   procedure Set_New_Line_Before
     (Item    : in out Program_Tree_Type'Class;
      Enabled : Boolean);

   function New_Line_Before
     (Item : Program_Tree_Type'Class)
      return Boolean;

   overriding function Image (Item : Program_Tree_Type) return String;

   overriding function Name (Item : Program_Tree_Type) return String;

   function Path_Image (Item : Program_Tree_Type) return String;

--     overriding
--     function Location_Line (Location : Program_Tree_Type)
--                            return Positive;
--
--     overriding
--     function Location_Column (Location : Program_Tree_Type)
--                              return Positive;

   function Get_Token (Item : Program_Tree_Type'Class)
                      return Aquarius.Tokens.Token;

   function Is_Choice
     (Item : Program_Tree_Type)
     return Boolean;

   function Is_Comment
     (Item : Program_Tree_Type)
     return Boolean;

   function Is_Filled
     (Item : Program_Tree_Type)
     return Boolean;

   function Is_Terminal
     (Item : Program_Tree_Type)
     return Boolean;

   function Is_Reserved_Terminal
     (Item : Program_Tree_Type)
     return Boolean;

   function Is_Separator
     (Item : Program_Tree_Type)
     return Boolean;

   function Is_Declaration
     (Item : Program_Tree_Type)
      return Boolean;

   procedure Set_Declaration
     (Item : in out Program_Tree_Type);

   function Declaration_Parent
     (Item : not null access Program_Tree_Type'Class)
      return Program_Tree;

   function Scan_Terminal
     (Start : not null access Program_Tree_Type'Class;
      Count : Integer)
      return Program_Tree
   with Pre => Start.Is_Terminal;
   --  Scan /Count/ terminals forward (if Count > 0) or backward
   --  (if Count < 0).  If Count = 0 then Start will be returned
   --  Start must be a terminal

   function Start_Of_Line
     (Tree   : not null access Program_Tree_Type'Class)
      return Aquarius.Programs.Program_Tree;
   --  Return the first terminal on the same line as Tree

   function End_Of_Line
     (Tree   : not null access Program_Tree_Type'Class)
      return Aquarius.Programs.Program_Tree;
   --  Return the last terminal on the same line as Tree

   function Has_Space_After
     (Item : Program_Tree_Type)
      return Boolean;
   --  Return True if the format for this tree allows a space after;
   --  i.e. the space after format is not "never"

   function Has_Symbol_Table (Item : Program_Tree_Type) return Boolean;

   function Render_Class (Item : Program_Tree_Type'Class) return String;

   procedure Fill (Item : in out Program_Tree_Type;
                   Text : String);

   procedure Fill (Item : in out Program_Tree_Type);

   function Frame (Item : Program_Tree_Type)
                  return Aquarius.Tokens.Token_Frame;

   function Minimum_Indent (Item : Program_Tree_Type)
                           return Aquarius.Source.Column_Number;

   function Has_Cross_Reference
     (Item : Program_Tree_Type'Class)
      return Boolean;

   function Cross_Reference_Name
     (Item : Program_Tree_Type'Class)
      return Program_Tree;

   procedure Set_Error (Item  : in out Program_Tree_Type;
                        Value : Boolean);

   function Has_Error (Item : Program_Tree_Type)
                      return Boolean;

   procedure Set_Inherited_Message_Level
     (Item  : in out  Program_Tree_Type;
      Level : Aquarius.Messages.Message_Level);

   function Get_Inherited_Message_Level
     (Item  : Program_Tree_Type)
     return Aquarius.Messages.Message_Level;

   overriding
   function Parent_Actionable
     (Child    : not null access Program_Tree_Type;
      Parent   : not null access Aquarius.Actions.Action_Source'Class)
     return access Aquarius.Actions.Actionable'Class;

   overriding
   function Actionable_Source
     (Item : Program_Tree_Type)
     return access Aquarius.Actions.Action_Source'Class;

   procedure Run_Actions
     (Start        : in out Program_Tree_Type;
      Action_Group : Aquarius.Actions.Action_Group;
      Stop_After   : Program_Tree := null);

   procedure Run_Actions
     (Start        : in out Program_Tree_Type;
      Stop         : not null access Program_Tree_Type'Class;
      Action_Group : Aquarius.Actions.Action_Group);

   procedure Execute_Single_Action
     (Tree         : not null access Program_Tree_Type'Class;
      Action_Group : Aquarius.Actions.Action_Group;
      Position     : Rule_Position);

   function Sequence_Number
     (Tree : Program_Tree_Type'Class)
      return Natural;

   type Root_Program_Tree_Store is interface
     and Local_Environment_Interface;

   function Get_Program
     (From      : Root_Program_Tree_Store;
      File_Name : String)
      return Program_Tree
      is abstract;

   type Program_Tree_Store is access all Root_Program_Tree_Store'Class;

   procedure Get_Allocation_Info
     (Allocated_Tree_Count : out Natural;
      Free_Tree_Count      : out Natural);

   procedure Free (Item : in out Program_Tree);

private

   type Aquarius_Object_Access is access all Root_Aquarius_Object'Class;

   package Aquarius_Object_Maps is
     new Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => String,
        Element_Type    => Aquarius_Object_Access,
        Hash            => Ada.Strings.Fixed.Hash,
        Equivalent_Keys => "=");

   package String_Property_Maps is
     new WL.String_Maps (String);

   Name_Property         : constant String := "name";

   package Aqua_Property_Maps is
     new WL.String_Maps (Aqua.Word, Aqua."=");

   type Local_Environment_Access is
     access all Local_Environment_Interface'Class;

   type Program_Tree_Type is
     new Aquarius.Trees.Root_Tree_Type
     and Aquarius.Actions.Actionable
     and Aquarius.Entries.Entry_Property_Interface
     and Aquarius.Types.Type_Property_Interface
     --  and Aquarius.Source.Has_Source_Position_Interface
   with
      record
         Free              : Boolean;
         Temporary         : Boolean;
         Comment           : Boolean;
         Error_Node        : Boolean;
         Error_Tree        : Boolean;
         Filled            : Boolean;
         Separator_Node    : Boolean;
         Separator_NL      : Boolean;
         Soft_NL           : Boolean;
         NL                : Boolean;
         Overflow_Checked  : Boolean;
         Have_Symbol_Table : Boolean;
         Is_Declaration    : Boolean;
         Has_Position      : Boolean;
         Has_Environment   : Boolean;
         Indent_Rule       : Boolean;
         Self              : Program_Tree;
         Sequence          : Natural := 0;
         Tree_Name         : Aquarius.Names.Aquarius_Name;
         Source_File       : Aquarius.Source.Source_File;
         Source_File_Name  : Aquarius.Names.Aquarius_Name;
         Msg_Level         : Aquarius.Messages.Message_Level;
         Vertical_Gap      : Aquarius.Layout.Count;
         Syntax            : Aquarius.Syntax.Syntax_Tree;
         Fill_Text         : Aquarius.Tokens.Token_Text;
         Local_Start       : Aquarius.Layout.Position;
         Start_Position    : Aquarius.Layout.Position;
         End_Position      : Aquarius.Layout.Position;
         Start_Line        : Aquarius.Layout.Line_Number;
         End_Line          : Aquarius.Layout.Line_Number;
         Start_Column      : Aquarius.Layout.Column_Number;
         End_Column        : Aquarius.Layout.Column_Number;
         Offset_Rule       : Aquarius.Source.Source_Position;
         Render_Class      : Aquarius.Syntax.Syntax_Tree;
         Local_Env         : Local_Environment_Access;
         String_Props      : String_Property_Maps.Map;
         Aqua_Props        : Aqua_Property_Maps.Map;
      end record;

   function Class_Name
     (It : Program_Tree_Type)
      return String
   is ("aquarius__trees__program_tree");

   --  override of Add_Child so that properties get copied from
   --  parent to child.
   --  NB: Always add the child to the tree BEFORE doing anything
   --  else to it
   overriding
   procedure Add_Child
     (Item      : not null access Program_Tree_Type;
      New_Child : not null access Aquarius.Trees.Root_Tree_Type'Class);

   overriding
   function Has_Entry (Item : Program_Tree_Type) return Boolean;

   overriding
   function Get_Entry (Item : Program_Tree_Type)
                       return Aquarius.Entries.Table_Entry;

   overriding
   procedure Set_Entry
     (Item : in out Program_Tree_Type;
      Ent  : not null access Entries.Table_Entry_Record'Class);

   overriding
   function Has_Type (Program : Program_Tree_Type) return Boolean;

   overriding
   function Get_Type (Program : Program_Tree_Type)
                       return Aquarius.Types.Aquarius_Type;

   overriding
   procedure Set_Type
     (Program  : in out Program_Tree_Type;
      Property : not null access Types.Root_Aquarius_Type'Class);

   procedure Set_Property
     (Program  : in out Program_Tree_Type;
      Name     : String;
      Value    : String);

   function Get_Property
     (Program  : in out Program_Tree_Type;
      Name     : String)
      return String;

   function Has_Property
     (Program  : Program_Tree_Type;
      Name     : String)
      return Boolean;

   overriding function Name (Item : Program_Tree_Type) return String
   is (Aquarius.Names.To_String (Item.Tree_Name));

   function Get_Tree_From_Sequence
     (Sequence : Positive)
      return Program_Tree;

   function Sequence_Number
     (Tree : Program_Tree_Type'Class)
      return Natural
   is (Tree.Sequence);

end Aquarius.Programs;
