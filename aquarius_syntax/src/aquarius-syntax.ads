with Aquarius.Actions;
with Aquarius.Formats;
with Aquarius.Layout;
with Aquarius.Names;
with Aquarius.Tokens;
with Aquarius.Trees;

private with Aquarius.Names.Sets;

package Aquarius.Syntax is

   pragma Elaborate_Body;

   Syntax_Tree_Error : exception;

   type Syntax_Tree_Record is
     new Aquarius.Trees.Root_Tree_Type and
     Aquarius.Actions.Action_Source
     with private;

   type Syntax_Tree is access all Syntax_Tree_Record'Class;
   type Array_Of_Syntax_Trees is array (Positive range <>) of Syntax_Tree;

   function Same (Left, Right : Syntax_Tree) return Boolean;

   Empty_Tree : constant Syntax_Tree;

   type Node_Class is (Terminal, Choice, Non_Terminal);

   function Is_Empty (Item : Syntax_Tree) return Boolean;

   function Syntax_Class
     (Item : not null access Syntax_Tree_Record)
     return Node_Class;

   function Syntax_Child
     (Item  : not null access Syntax_Tree_Record;
      Index : Positive)
      return Syntax_Tree
   is (Syntax_Tree (Item.Child (Index)));

   overriding
     function Has_Named_Property
     (Item : Syntax_Tree_Record;
      Name : String)
     return Boolean;

   overriding
   function Text (Item : Syntax_Tree_Record) return String;

   function Has_Token (Item : Syntax_Tree_Record)
                      return Boolean;

   function Token (Item : Syntax_Tree_Record)
                  return Aquarius.Tokens.Token;

   not overriding
   function Frame (Item : Syntax_Tree_Record)
                  return Aquarius.Tokens.Token_Frame;

   function Separator (Item : Syntax_Tree_Record)
                       return Syntax_Tree;

   function Is_Separator
     (Item : Syntax_Tree_Record)
      return Boolean;

   procedure Set_Separator (Item      : in out Syntax_Tree_Record;
                            Separator : Syntax_Tree);

   function Has_Separator
     (Item : not null access Syntax_Tree_Record'Class)
     return Boolean;

   function Optional (Item : Syntax_Tree_Record) return Boolean;
   function Repeatable (Item : Syntax_Tree_Record) return Boolean;

   function New_Terminal
     (Frame       : Aquarius.Tokens.Token_Frame;
      Declaration : not null access Aquarius.Trees.Root_Tree_Type'Class;
      Tree_Token  : Aquarius.Tokens.Token)
     return Syntax_Tree;

   function New_Choice
     (Frame       : Aquarius.Tokens.Token_Frame;
      Declaration : not null access Aquarius.Trees.Root_Tree_Type'Class)
     return Syntax_Tree;

   function New_Optional
     (Frame       : Aquarius.Tokens.Token_Frame;
      Declaration : not null access Aquarius.Trees.Root_Tree_Type'Class)
     return Syntax_Tree;

   function New_Repeat
     (Frame       : Aquarius.Tokens.Token_Frame;
      Declaration : not null access Aquarius.Trees.Root_Tree_Type'Class;
      Optional    : Boolean;
      Separator   : Syntax_Tree)
     return Syntax_Tree;

   function New_Sequence
     (Frame       : Aquarius.Tokens.Token_Frame;
      Declaration : not null access Aquarius.Trees.Root_Tree_Type'Class)
     return Syntax_Tree;

   function Is_Sequence
     (Item : not null access Syntax_Tree_Record'Class)
     return Boolean;

   function Is_Repeat
     (Item : not null access Syntax_Tree_Record'Class)
     return Boolean;

   procedure Set_Non_Terminal_Name
     (Item         : not null access Syntax_Tree_Record;
      Non_Terminal : String);

   function Non_Terminal_Name
     (Item         : not null access Syntax_Tree_Record)
     return String;

   procedure Add_Precondition_Property
     (Item          : not null access Syntax_Tree_Record;
      Property_Name : String);

   function Check_Precondition
     (For_Syntax : not null access Syntax_Tree_Record;
      Test_Tree  : not null access Aquarius.Trees.Root_Tree_Type'Class)
     return Boolean;

   --  Set_Render_Class: sub-trees can have a rendering class
   --  associated with them, which can change how it is displayed.
   --  For example, a sub-tree with the class 'function_name' might be
   --  shown in a different colour

   procedure Set_Render_Class (Item       : in out Syntax_Tree_Record;
                               Class_Name : String);

   function Has_Render_Class (Item : Syntax_Tree_Record)
                             return Boolean;

   function Render_Class (Item : Syntax_Tree_Record)
                         return String;

   --  Has_Indent_Rule: True if the syntax is bounded by the
   --  first non-indented token that follows it (non-inclusive).
   --  For example, in the EBNF grammar, definition bodies are
   --  subject to the indent rule, meaning that the following
   --  sequence is parsed as two rules rather than one followed
   --  by a syntax error (because the defining identifier 'term'
   --  is not indented).
   --    expression ::= term operator term
   --    term ::= identifier | integer
   --  Not to be confused with a layout rule that requires
   --  tokens to be indented when rendering them ...
   function Has_Indent_Rule (Item : not null access Syntax_Tree_Record)
                            return Boolean;

   --  Enable_Indent_Rule: enables the above rule
   procedure Enable_Indent_Rule (Item : not null access Syntax_Tree_Record);

   overriding
   function Image (Tree : Syntax_Tree_Record) return String;

   overriding
   function Name (Tree : Syntax_Tree_Record) return String;

   overriding
   procedure Add_Child
     (Item      : not null access Syntax_Tree_Record;
      New_Child : not null access Aquarius.Trees.Root_Tree_Type'Class);

   --  Like Add_Child, but we assert that the child will be
   --  the only child of Item

   procedure Add_Single_Child
     (Item      : not null access Syntax_Tree_Record;
      New_Child : not null access Syntax_Tree_Record);

--     procedure Indent_Child
--       (Item       : not null access Syntax_Tree_Record;
--        Child_Name : in     String;
--        Level      : in     Layout.Positive_Count := 3);

   function Indented (Tree : Syntax_Tree_Record) return Boolean;
   function Indent (Tree : Syntax_Tree_Record)
                   return Aquarius.Layout.Positive_Count;

--     procedure Indent_Child
--       (Item        : not null access Syntax_Tree_Record;
--        Format_Name : in     String;
--        Child_Name  : in     String;
--        Level       : in     Aquarius.Layout.Positive_Count := 3);

--     function Indented (Tree        : not null access Syntax_Tree_Record;
--                        Format_Name : String)
--                       return Boolean;

--     function Get_Indent (Tree        : not null access Syntax_Tree_Record;
--                          Format_Name : String)
--                         return Aquarius.Layout.Positive_Count;

--     procedure Indent
--       (Tree        : not null access Syntax_Tree_Record;
--        Format_Name : in     String;
--        Level       : in     Aquarius.Layout.Positive_Count := 3);

--     procedure Indent
--       (Tree        : not null access Syntax_Tree_Record;
--        Level       : in     Aquarius.Layout.Positive_Count := 3);

   --  Find_Child: find the first child of Current with the given
   --  name.  Do not search beyond named subtrees.

   function Find_Child (Current    : not null access Syntax_Tree_Record;
                        Child_Name : String)
                       return Syntax_Tree;
   --  Find_Child: find the Index'th child of Current with the name
   --  Child_Name.  Do not search beyond named subtrees.
   function Find_Child (Current    : not null access Syntax_Tree_Record;
                        Child_Name : String;
                        Index      : Positive)
                       return Syntax_Tree;

   --     procedure Add_Table_Property
   --  (Tree : not null access Syntax_Tree_Record);
   --     function Has_Table_Property
   --       (Tree : not null access Syntax_Tree_Record)
   --       return Boolean;

   --  Align all instances of Item under Top vertically
   procedure Vertical_Align (Item : in out Syntax_Tree_Record;
                             Top  : Syntax_Tree);

   function Vertically_Aligned
     (Item : Syntax_Tree_Record)
      return Boolean;

   function Align_Parent
     (Item : not null access Syntax_Tree_Record)
      return Syntax_Tree;

   not overriding
   function Referenced (Item : not null access Syntax_Tree_Record)
                       return Boolean;

   not overriding
   procedure Set_Referenced (Item : not null access Syntax_Tree_Record);

   --  Leaving blank lines

   --  Set_Vertical_Gap: Set the vertical gap following the tree
   --  (which currently must be a terminal).
--     procedure Set_Vertical_Gap (Item : not null access Syntax_Tree_Record;
--                                 Gap  : Aquarius.Layout.Positive_Count);

--     function Vertical_Gap (Item : not null access Syntax_Tree_Record)
--                           return Aquarius.Layout.Count;

   --  Special case formats

   --  Has_Format: return True if the syntax tree has some kind of
   --  format associated with it.
   function Has_Format (Item : Syntax_Tree_Record) return Boolean;

   --  Set_Format: give the syntax tree a format.  This overrides any format
   --  that the symbol of a syntax tree might have.
   procedure Set_Format (Item : in out Syntax_Tree_Record;
                         To   : Aquarius.Formats.Aquarius_Format);

   function Get_Format (Item : Syntax_Tree_Record)
                       return Aquarius.Formats.Aquarius_Format;

   type Special_Format_Function is access
     function (Tree : not null access Aquarius.Trees.Root_Tree_Type'Class)
              return Aquarius.Formats.Aquarius_Format;

   --  Set_Format: give the syntax tree a special format.  This function
   --  overrides any format the tree might already have.
   procedure Set_Format (Item : in out Syntax_Tree_Record;
                         To   : Special_Format_Function);

   function Get_Special_Format
     (Item : Syntax_Tree_Record)
     return Special_Format_Function;

   --  Has_Simple_Format: return true if the syntax tree has a format
   --  which can be stored in an Aquarius.Formats.Format type
   --  (otherwise the format must be a function of type
   --  Special_Format_Function)

   --  If a format is named, then it only applies in a context where
   --  the name is active.  For example, a set of formats that create
   --  a space-saving formal argument list could be called "narrow",
   --  and they would be activated by the indent package if there was
   --  no room for the normal format.

   function Has_Format (Item : not null access Syntax_Tree_Record;
                        Name : String)
                       return Boolean;
   procedure Set_Format (Item : not null access Syntax_Tree_Record;
                         Name : String;
                         To   : Aquarius.Formats.Aquarius_Format);
   function Format (Item : not null access Syntax_Tree_Record;
                    Name : String)
                   return Aquarius.Formats.Aquarius_Format;

   --  Optional format names
   --  Used to set a name for an alternative set of formats
   --  in the tree's children.  This named will be passed to
   --  the named format and indent functions.
   procedure Add_Format_Option (Item  : not null access Syntax_Tree_Record;
                                Name  : String);

   function Has_Format_Option
     (Item : Syntax_Tree_Record)
     return Boolean;
   function Format_Option (Item : Syntax_Tree_Record) return String;

   --  Superclass flag
   --  Set if this syntax node is a display superclass; i.e. that its
   --  name should be given to the formatter

   function Is_Superclass (Item : Syntax_Tree_Record) return Boolean;
   procedure Set_Superclass (Item : in out Syntax_Tree_Record);

   function Has_Plugin_Mark (Item : Syntax_Tree_Record)
                             return Boolean;

   procedure Plugin_Mark (Item : in out Syntax_Tree_Record);

   procedure Set_Cross_Reference
     (Item       : in out Syntax_Tree_Record'Class;
      Name_Child : Syntax_Tree);
   --  The syntax given by Item can be the target of a cross-reference,
   --  named by its child Name_Child

   function Has_Cross_Reference
     (Item : Syntax_Tree_Record'Class)
      return Boolean;

   function Cross_Reference_Name
     (Item : Syntax_Tree_Record'Class)
      return Syntax_Tree;

   not overriding
   function Declaration (Item : Syntax_Tree_Record)
                        return Aquarius.Trees.Tree;

   not overriding
   procedure Set_Declaration
     (Item : in out Syntax_Tree_Record;
      Dec  : not null access Aquarius.Trees.Root_Tree_Type'Class);

private

   type Syntax_Tree_Node_Record;
   type Syntax_Tree_Node is access Syntax_Tree_Node_Record;

   --  Syntax_Tree: anything stored directly in the tree is
   --  particular to the instatiation of the node (e.g. an identifer
   --  has both properties associated with being an identifier, and
   --  other properties associated with being, say, a variable
   --  declaration).
   type Syntax_Tree_Record is
     new Aquarius.Trees.Root_Tree_Type and
     Aquarius.Actions.Action_Source with
      record
         Node             : Syntax_Tree_Node;
         Indent           : Aquarius.Layout.Count := 0;
         Vertical_Gap     : Aquarius.Layout.Count := 0;
         Render_Class     : Aquarius.Names.Aquarius_Name;
         When_Properties  : Aquarius.Names.Sets.Name_Set;
         Has_Indent_Rule  : Boolean               := False;
         Has_Plugin_Mark  : Boolean               := False;
         Has_Render_Class : Boolean               := False;
         Referenced_Flag  : Boolean               := False;
         Pristine         : Boolean               := True;
      end record;

   Empty_Tree : constant Syntax_Tree := null;

   overriding
   function Get_Action_List (Source : not null access Syntax_Tree_Record)
                            return Aquarius.Actions.Action_Instance_List;

   overriding procedure Append_Action
     (Source : in out Syntax_Tree_Record;
      Action : Aquarius.Actions.Action_Instance);

   -------------------------------
   --  Caching the Begins check --
   -------------------------------

   --  Return true if the truth of Sym beginning Tree is known
   function Is_Begins_Cached (Tree : not null access Syntax_Tree_Record;
                              Tok  : Aquarius.Tokens.Token)
                             return Boolean;

   --  Return the cached truth value of Sym beginning Tree
   --  Requires Is_Begins_Cached to be true
   function Cached_Begins (Tree : not null access Syntax_Tree_Record;
                           Tok  : Aquarius.Tokens.Token)
                          return Boolean;

   --  Cache the truth value of Sym beginning Tree.
   --  Precondition: not Is_Begins_Cached (Tree, Sym);
   --  Postconditions: Is_Begins_Cached (Tree, Sym)
   --                  Cached_Begins (Tree, Syn) = Begins
   procedure Set_Begins (Tree   : not null access Syntax_Tree_Record;
                         Tok    : Aquarius.Tokens.Token;
                         Begins : Boolean);

   pragma Inline (Syntax_Class);

end Aquarius.Syntax;
