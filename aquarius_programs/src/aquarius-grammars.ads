private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Containers.Indefinite_Hashed_Maps;
private with Ada.Strings.Hash_Case_Insensitive;
private with Ada.Strings.Equal_Case_Insensitive;

with Aquarius.Actions;
with Aquarius.Lexers;
with Aquarius.Names;
with Aquarius.Programs;
with Aquarius.Properties;
with Aquarius.Source;
with Aquarius.Syntax;
with Aquarius.Tokens;
with Aquarius.Trees;

package Aquarius.Grammars is

   Grammar_Error : exception;

   type Aquarius_Grammar_Record is
     new Root_Aquarius_Object and
     Aquarius.Properties.Property_Pool
     with private;

   type Aquarius_Grammar is access all Aquarius_Grammar_Record'Class;

   not overriding
   function New_Grammar (Name : String;
                         EBNF : Aquarius.Programs.Program_Tree)
                         return Aquarius_Grammar;

   overriding
   function Name (Grammar : Aquarius_Grammar_Record)
                 return String;

   not overriding
   procedure Add_Class_Terminal
     (Grammar     : in out Aquarius_Grammar_Record;
      Declaration : not null access Aquarius.Trees.Root_Tree_Type'Class;
      Name        : String;
      Lex         : Aquarius.Lexers.Lexer;
      Delimiter   : Boolean                   := False);

   procedure Line_Comment
     (Grammar : in out Aquarius_Grammar_Record;
      Text    : String)
     with Pre => not Grammar.Have_Line_Comment,
     Post => Grammar.Have_Line_Comment;

   procedure Block_Comment_Start
     (Grammar : in out Aquarius_Grammar_Record;
      Text    : String)
     with Pre => not Grammar.Have_Block_Comment,
     Post => Grammar.Have_Block_Comment;

   procedure Block_Comment_End
     (Grammar : in out Aquarius_Grammar_Record;
      Text    : String)
     with Pre => Grammar.Have_Block_Comment;

   not overriding
   procedure Add_Value (Grammar     : in out Aquarius_Grammar_Record;
                        Declaration : Aquarius.Trees.Tree;
                        Name        : String;
                        Value       : String);

   not overriding
   function Add_Non_Terminal
     (Grammar       : in out Aquarius_Grammar_Record;
      Declaration   : not null access Aquarius.Trees.Root_Tree_Type'Class;
      Name          : String)
     return Aquarius.Syntax.Syntax_Tree;

   --  Reference_Name: create a syntax rule that references a named
   --  entity, which may or may not have been defined yet.  Return
   --  a syntax tree which represents the reference (and indirectly,
   --  the named entity).

   --     Grammar      : current grammar
   --     Name         : name of the rule
   --     Indent_Rule  : True if the referenced rule is bounded by
   --                    a non-zero indent (a token is only considered
   --                    as a potential continuation of the rule if it
   --                    is a non-zero number of characters from the
   --                    left-most column).
   --     Offset_Rule  : True if the referenced rule is bounded by
   --                    by an offset rule (a token is only considered
   --                    as a continuation of the rule if it is as least
   --                    as far to the right as the first element of the
   --                    rule)  (not currently implemented).

   not overriding
   function Reference_Name
     (Grammar      : in out Aquarius_Grammar_Record;
      Reference    : not null access Aquarius.Trees.Root_Tree_Type'Class;
      Name         : String;
      Indent_Rule  : Boolean       := False;
      Offset_Rule  : Boolean       := False)
     return Aquarius.Syntax.Syntax_Tree;

   --  Reference_Token: report a reference to a reserved token.
   --  If it doesn't already exist, it will be added with the
   --  appropriate class

   --  Text: the literal text of this terminal
   --  Non_Terminal_Name: the name of the associated non-terminal

   not overriding
   function Reference_Terminal
     (Grammar           : in out Aquarius_Grammar_Record;
      Reference         : not null access Aquarius.Trees.Root_Tree_Type'Class;
      Text              : String;
      Indent_Rule       : Boolean       := False;
      Offset_Rule       : Boolean       := False)
     return Aquarius.Syntax.Syntax_Tree;

   not overriding
   function Get_Definition (Grammar : Aquarius_Grammar_Record;
                            Name    : String)
                           return Aquarius.Syntax.Syntax_Tree;

   not overriding
   function Get_Top_Level_Syntax
     (Grammar : Aquarius_Grammar_Record)
     return Aquarius.Syntax.Syntax_Tree;

   not overriding
   procedure Check_Grammar (Grammar : in out Aquarius_Grammar_Record);

   procedure Add_Action_Group
     (Grammar : in out Aquarius_Grammar_Record;
      Name    : String;
      Trigger : Aquarius.Actions.Action_Execution_Trigger;
      Group   :    out Aquarius.Actions.Action_Group);
   --  Add_Action_Group: add an action group to the grammar.  If more
   --  than one action group has the same trigger, (see
   --  Aquarius.Actions spec for details on triggers) they are always
   --  executed in the same order they were added.  Furthermore, an
   --  action group always runs in isolation and to completion on the
   --  entire tree before the next one the in trigger set starts.

   procedure Scan_Action_Groups
     (Grammar : Aquarius_Grammar_Record'Class;
      Process : not null access
        procedure (Group : Aquarius.Actions.Action_Group));

   procedure Add_Action_Program
     (Grammar : in out Aquarius_Grammar_Record'Class;
      Group   : Aquarius.Actions.Action_Group;
      Program : Aquarius.Programs.Program_Tree);

   procedure Scan_Action_Programs
     (Grammar : Aquarius_Grammar_Record'Class;
      Process : not null access
        procedure (Group : Aquarius.Actions.Action_Group;
                   Program : Aquarius.Programs.Program_Tree));

   function Significant_End_Of_Line
     (Grammar : Aquarius_Grammar_Record)
      return Boolean;
   --  Return True if end of line is syntactically significant.
   --  This is detected automatically if the grammar has a rule which
   --  matches the end of line.

   function Line_Continues
     (Grammar : Aquarius_Grammar_Record;
      Line    : String)
      return Natural;
   --  Return 0 if Significant_End_Of_Line returns false,
   --  or if the grammar does not define a continuation character.
   --  Otherwise, return the index at which the following line should
   --  start (i.e. the following line should replace the contents of
   --  Line beginning at the index returned)

   function Group
     (Grammar : Aquarius_Grammar_Record'Class;
      Name    : String)
      return Aquarius.Actions.Action_Group;

   not overriding
   procedure Run_Actions
     (Grammar      : Aquarius_Grammar_Record;
      Group_Name   : String;
      Start        : Aquarius.Programs.Program_Tree);

   --  Run_Action_Trigger: run action groups belonging to
   --  the specified trigger.  Perform them via a depth
   --  first search.  Process action groups in order of
   --  creation, finishing the first action group completely
   --  before moving to the next.  If Stop_After is not
   --  null, the run stops after the given tree has been
   --  processed.

   not overriding
   procedure Run_Action_Trigger
     (Grammar    : Aquarius_Grammar_Record;
      Start      : Aquarius.Programs.Program_Tree;
      Trigger    : Aquarius.Actions.Action_Execution_Trigger;
      Stop_After : Aquarius.Programs.Program_Tree := null);

   not overriding
   procedure Run_Action_Trigger
     (Grammar    : Aquarius_Grammar_Record;
      Start      : Aquarius.Programs.Program_Tree;
      Stop       : Aquarius.Programs.Program_Tree;
      Trigger    : Aquarius.Actions.Action_Execution_Trigger);

   --  Run_Parse_Actions: run actions at the specified Tree and Position
   --  that are triggered by parsing.

   procedure Run_Parse_Actions
     (Tree     : in out Aquarius.Programs.Program_Tree_Type'Class;
      Position : Rule_Position);

   not overriding
   function Frame (Grammar : Aquarius_Grammar_Record)
                  return Aquarius.Tokens.Token_Frame;

   not overriding
   function Has_Errors (Grammar : Aquarius_Grammar_Record)
                       return Boolean;

   function Have_Line_Comment
     (Grammar : Aquarius_Grammar_Record)
      return Boolean;

   function Line_Comment
     (Grammar : Aquarius_Grammar_Record)
      return String
     with Pre => Grammar.Have_Line_Comment;

   function Have_Block_Comment
     (Grammar : Aquarius_Grammar_Record)
      return Boolean;

   function Block_Comment_Start
     (Grammar : Aquarius_Grammar_Record)
      return String
     with Pre => Grammar.Have_Block_Comment;

   function Block_Comment_End
     (Grammar : Aquarius_Grammar_Record)
      return String
     with Pre => Grammar.Have_Block_Comment;

   function Have_Syntax
     (Grammar : Aquarius_Grammar_Record'Class;
      Name    : String)
      return Boolean;

   function Get_Syntax
     (Grammar : Aquarius_Grammar_Record'Class;
      Name    : String)
      return Aquarius.Syntax.Syntax_Tree
     with Pre => Grammar.Have_Syntax (Name);

   not overriding
   function Make_Program_Tree
     (Grammar : not null access Aquarius_Grammar_Record;
      Name    : String)
      return Aquarius.Programs.Program_Tree;

   not overriding
   function Make_Error_Tree
     (Grammar  : Aquarius_Grammar_Record;
      Position : Aquarius.Source.Source_Position;
      Message  : String)
     return Aquarius.Programs.Program_Tree;

   function New_Property
     (Grammar   : not null access Aquarius_Grammar_Record'Class;
      Name      : String;
      Inherited : Boolean;
      Has_Value : Boolean)
      return Aquarius.Properties.Property_Type;

   function Get_EBNF_Tree
     (Grammar : Aquarius_Grammar_Record'Class)
      return Aquarius.Programs.Program_Tree;

   function Get_Grammar
     (Program : Aquarius.Programs.Program_Tree_Type'Class)
      return Aquarius_Grammar;

private

   package Syntax_Map is
     new Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => String,
        Element_Type    => Aquarius.Syntax.Syntax_Tree,
        Hash            => Ada.Strings.Hash_Case_Insensitive,
        Equivalent_Keys => Ada.Strings.Equal_Case_Insensitive,
        "="             => Aquarius.Syntax."=");

   function Action_Entity_Key
     (Group_Name    : String;
      Position_Name : String;
      Parent_Name   : String;
      Child_Name    : String)
      return String
   is (Group_Name & "-" & Position_Name & "-"
       & Parent_Name & (if Child_Name = "" then "" else "/" & Child_Name));

   type Action_Program_Entry is
      record
         Group   : Aquarius.Actions.Action_Group;
         Program : Aquarius.Programs.Program_Tree;
      end record;

   package Action_Program_Lists is
      new Ada.Containers.Doubly_Linked_Lists (Action_Program_Entry);

   type Aquarius_Grammar_Record is
     new Root_Aquarius_Object and
     Aquarius.Properties.Property_Pool with
      record
         Grammar_Name        : Aquarius.Tokens.Token_Text;
         Frame               : Aquarius.Tokens.Token_Frame;
         Definition          : Aquarius.Programs.Program_Tree;
         Top_Level_Syntax    : Aquarius.Syntax.Syntax_Tree;
         Action_Groups       : Aquarius.Actions.Action_Group_List;
         Action_Programs     : Action_Program_Lists.List;
         Case_Sensitive      : Boolean;
         Match_EOL           : Boolean := False;
         Continuation        : Character := Character'Val (0);
         Non_Terminals       : Syntax_Map.Map;
         Terminals           : Syntax_Map.Map;
         Line_Comment        : Aquarius.Names.Aquarius_Name :=
                                 Aquarius.Names.Null_Aquarius_Name;
         Block_Comment_Start : Aquarius.Names.Aquarius_Name :=
                                 Aquarius.Names.Null_Aquarius_Name;
         Block_Comment_End   : Aquarius.Names.Aquarius_Name :=
                                 Aquarius.Names.Null_Aquarius_Name;
         Error_Token         : Aquarius.Tokens.Token;
         Error_Syntax        : Aquarius.Syntax.Syntax_Tree;
         Error               : Boolean;
         Properties          : Aquarius.Properties.Property_Type_List;
      end record;

   overriding
   function Get_Property_Types (From : Aquarius_Grammar_Record)
                               return Aquarius.Properties.Property_Type_List;

   --  private subprograms for child packages

   --  The following definitions of Add_Non_Terminal are only
   --  be used for bootstrapping Aquarius (i.e. when creating the
   --  the syntax for EBNF

   not overriding
   procedure Add_Non_Terminal
     (Grammar       : not null access Aquarius_Grammar_Record;
      Name          : String;
      Definition    : Aquarius.Syntax.Syntax_Tree;
      Child         : Aquarius.Syntax.Syntax_Tree);

   type Array_Of_Syntax_Trees is
     array (Positive range <>) of Aquarius.Syntax.Syntax_Tree;

   not overriding
   procedure Add_Non_Terminal
     (Grammar       : not null access Aquarius_Grammar_Record;
      Name          : String;
      Definition    : Aquarius.Syntax.Syntax_Tree;
      Children      : Array_Of_Syntax_Trees);

   function Have_Syntax
     (Grammar : Aquarius_Grammar_Record'Class;
      Name    : String)
      return Boolean
   is (Grammar.Non_Terminals.Contains (Name)
       or else Grammar.Terminals.Contains (Name));

   function Get_Syntax
     (Grammar : Aquarius_Grammar_Record'Class;
      Name    : String)
      return Aquarius.Syntax.Syntax_Tree
   is (if Grammar.Non_Terminals.Contains (Name)
       then Grammar.Non_Terminals.Element (Name)
       else Grammar.Terminals.Element (Name));

end Aquarius.Grammars;
