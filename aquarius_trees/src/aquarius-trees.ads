private with Ada.Containers.Vectors;

with Aquarius.Locations;
with Aquarius.Messages;
with Aquarius.Names;
with Aquarius.Properties;
with Aquarius.Sources;

package Aquarius.Trees is

   pragma Elaborate_Body;

   Tree_Error : exception;

   type Aquarius_Object is access all Root_Aquarius_Object'Class;

   type Array_Of_Objects is array (Positive range <>) of Aquarius_Object;

   type Root_Tree_Type is
     abstract new Aquarius.Messages.Message_Location
     and Aquarius.Locations.Updateable_Location_Interface
     and Root_Aquarius_Object
   with private;

   type Tree is access all Root_Tree_Type'Class;

   overriding
   function "=" (Left : Root_Tree_Type;
                 Right : Root_Tree_Type)
                 return Boolean;

   function Same_Node (Item   : Root_Tree_Type;
                       Other  : Root_Tree_Type'Class)
                       return Boolean;

   procedure Initialise_Tree
     (Item          : in out Root_Tree_Type;
      Source        : Aquarius.Sources.Source_Reference;
      Location      : Aquarius.Locations.Location_Interface'Class;
      Keep_Parent   : Boolean;
      Keep_Siblings : Boolean;
      Temporary     : Boolean := False);
   --  Keep_Siblings: true if left/right siblings of tree nodes
   --  should be remembered by the node (or only maintained by the parent)
   --  Keep_Parent: as Keep_Siblings, but for the parent of a tree node

   --  with Post => Aquarius.Source.Show (Item.Get_Location) =
   --    Aquarius.Source.Show (Location);

   overriding function Offset
     (This : Root_Tree_Type)
      return Aquarius.Locations.Location_Offset;

   overriding function Line
     (This : Root_Tree_Type)
      return Aquarius.Locations.Line_Index;

   overriding function Column
     (This : Root_Tree_Type)
      return Aquarius.Locations.Column_Index;

   overriding procedure Update_Location
     (This : in out Root_Tree_Type;
      From : Aquarius.Locations.Location_Interface'Class);

   overriding function Location_Name
     (This      : Root_Tree_Type;
      Show_Path : Boolean := False)
      return String;

   overriding
   procedure Attach_Message
     (To    : in out Root_Tree_Type;
      Item  : Aquarius.Messages.Message);

   overriding procedure Get_Messages
     (From  : Root_Tree_Type;
      List  : in out Aquarius.Messages.Message_List);

   overriding procedure Clear_Messages
     (Item : in out Root_Tree_Type);

   --  Has_Messages: returns true if this node only has messages attached
   function Has_Messages
     (Item : Root_Tree_Type)
      return Boolean;

   --  Get_Message_Level: if Has_Messages is True, returns the
   --  highest message level of the messages in this node.
   --  Otherwise returns No_Message
   function Get_Message_Level
     (Item : Root_Tree_Type)
      return Aquarius.Messages.Message_Level;

   function Source
     (This : Root_Tree_Type)
      return Aquarius.Sources.Source_Reference;

   procedure Set_Property
     (Item  : in out Root_Tree_Type;
      Prop  : Aquarius.Properties.Property_Type;
      Value : not null access Root_Aquarius_Object'Class);

   --  This version of Set_Property sets a property whose only value is
   --  existence
   procedure Set_Property
     (Item  : in out Root_Tree_Type;
      Prop  : Aquarius.Properties.Property_Type);

   procedure Clear_Property
     (Item  : not null access Root_Tree_Type;
      Prop  : Aquarius.Properties.Property_Type);

   function Has_Property
     (Item : Root_Tree_Type;
      Prop : Aquarius.Properties.Property_Type)
      return Boolean;

   function Has_Named_Property (Item : Root_Tree_Type;
                                Name : String)
                               return Boolean;

   function Has_Named_Property (Item : Root_Tree_Type;
                                Name : Aquarius.Names.Aquarius_Name)
                               return Boolean;

   function Show_Set_Properties (Item : not null access Root_Tree_Type'Class)
                                return String;

   function Property (Item : Root_Tree_Type;
                      Prop : Aquarius.Properties.Property_Type)
                     return Aquarius_Object;

   --  All_Properties: recursively find all properties of the indicated
   --  type in the entire tree
   function All_Properties (Top   : not null access Root_Tree_Type;
                            Prop  : Aquarius.Properties.Property_Type)
                           return Array_Of_Objects;

   function Is_Null (T : Tree) return Boolean;

   function Has_Children (Item : Root_Tree_Type) return Boolean;

   function Child_Count (Item : Root_Tree_Type) return Natural;

   function Child (Item  : Root_Tree_Type;
                   Index :        Positive)
                  return Tree;

   function First_Child (Item : Root_Tree_Type) return Tree;
   function Last_Child (Item : Root_Tree_Type) return Tree;

   function Right_Sibling (Item : Root_Tree_Type'Class) return Tree;
   function Left_Sibling (Item : Root_Tree_Type'Class) return Tree;
   function Parent (Item : Root_Tree_Type'Class) return Tree;
   procedure Add_Right_Sibling
     (To_Tree     : not null access Root_Tree_Type;
      New_Sibling : not null access Root_Tree_Type'Class);

   procedure Add_Left_Sibling
     (To_Tree     : not null access Root_Tree_Type;
      New_Sibling : not null access Root_Tree_Type'Class);

   procedure Add_Child (Item      : not null access Root_Tree_Type;
                        New_Child : not null access Root_Tree_Type'Class);

   procedure Remove_Child (Item      : not null access Root_Tree_Type;
                           Index     :        Positive);

   procedure Remove_Child (Item      : not null access Root_Tree_Type;
                           Child     : not null access Root_Tree_Type);

   procedure Replace_Child (Item      : not null access Root_Tree_Type;
                            Old_Child : not null access Root_Tree_Type;
                            New_Child : not null access Root_Tree_Type);
   --  Equivalent to Old_Child.Add_Left_Sibling (New_Child) followed
   --  by Item.Remove_Child (Old_Child);

   procedure Set_Foster_Parent (Item    : not null access Root_Tree_Type;
                                Parent  : not null access Root_Tree_Type);

   --  First_Leaf: return the left-most leaf of Item; equivalent
   --  to calling First_Child until a leaf is found.

   function First_Leaf (Item : not null access Root_Tree_Type) return Tree;
   function Last_Leaf (Item : not null access Root_Tree_Type) return Tree;

   function Next_Leaf (Current : not null access Root_Tree_Type'Class)
                      return Tree;

   function Previous_Leaf (Current : not null access Root_Tree_Type'Class)
                          return Tree;

   --  Leaf: equivalent to First_Leaf (Breadth_First_Search (Item, Leaf_Name)
   function Leaf (Item      : not null access Root_Tree_Type;
                  Leaf_Name : String)
                 return Tree;

   function Breadth_First_Search
     (Top         : Root_Tree_Type;
      Match       : not null access function (Item : Root_Tree_Type'Class)
      return Boolean;
      Match_Index   : Positive := 1;
      Stop_At_Named : Boolean := False)
     return Tree;
   --  Breadth_First_Search
   --  Perform a bfs on the tree, stopping when the number
   --  of times the Match function returns True is equal
   --  to Match_Index.  Note that when a match is found, its
   --  sub-tree is not searched.  If Stop_At_Named is True, the
   --  search will not continue past named sub-trees

   function Breadth_First_Search
     (Top           : Root_Tree_Type;
      Child_Name    : String;
      Match_Index   : Positive := 1;
      Stop_At_Named : Boolean := False)
      return Tree;
   --  Breadth_First_Search (2)
   --  Same as above, but specialised for finding subtrees with
   --  the given name.  If Stop_At_Named is True, the search will
   --  not continue past named sub-trees.

   procedure Breadth_First_Scan
     (Top : Root_Tree_Type;
      Process : not null access
        procedure (Item : Tree));

   function Has_Name (Item : Tree) return Boolean;

   type Array_Of_Trees is array (Positive range <>) of Tree;

   --  Search
   --  return all nodes which match the condition, in depth-first order

   function Search
     (Top         : not null access Root_Tree_Type'Class;
      Match       : not null access function (Item : Tree) return Boolean)
     return Array_Of_Trees;

   function Get_Matching_Children
     (Top   : Root_Tree_Type;
      Match : not null access function (Item : Tree) return Boolean)
     return Array_Of_Trees;

   function Get_Matching_Children
     (Top   : Root_Tree_Type;
      Name  : String)
     return Array_Of_Trees;

   function Get_Named_Children
     (Top   : Root_Tree_Type)
     return Array_Of_Trees;

   function Children
     (Top  : not null access Root_Tree_Type'Class;
      Path : String)
      return Array_Of_Trees;

   function Child
     (Top  : not null access Root_Tree_Type'Class;
      Path : String)
      return Tree;

   --     overriding
--     function Parent_Actionable
--       (Child    : not null access Root_Tree_Type;
--        Parent   : not null access Aquarius.Actions.Action_Source'Class)
--       return access Aquarius.Actions.Actionable'Class;

   --  Common_Ancestor: find the common ancestor of Left and Right.
   --  Left_Ancestor is set to the child of Ancestor that leads to
   --  Left, similarly for Right.
   --  If Keep_Parent is False, Constraint_Error is raised.
   --  If there is no common ancestor, all out arguments are
   --  set to null.
   --  If Left is an ancestor of Right, Ancestor is set to Left,
   --  Right_Ancestor is set to the child of Left that leads to
   --  Right, and Left is set to null (similarly if Right is an
   --  ancestor of Left).  If Left = Right, Ancestor is set to Left,
   --  while Left_Ancestor and Right_Ancestor are both set to null.
   procedure Common_Ancestor
     (Left, Right    : not null access Root_Tree_Type'Class;
      Ancestor       : out Tree;
      Left_Ancestor  : out Tree;
      Right_Ancestor : out Tree);

   type Scan_Direction is (Backwards, Forewards);

   function Search_Leaves (Start     : not null access Root_Tree_Type;
                           Direction : Scan_Direction;
                           Match     : not null access
                             function (Leaf : not null access constant
                                         Root_Tree_Type'Class)
                           return Boolean)
                          return Tree;

   function Temporary (Item : Root_Tree_Type) return Boolean;

   function Image (Item : Root_Tree_Type) return String
      is abstract;

   function Text
     (Item : Root_Tree_Type)
     return String;

   function Standard_Text
     (Item : Root_Tree_Type)
     return String;

   function Internal_Declaration return Tree;
   --  Return a tree that represents an internal declaration; i.e.
   --  something that is not declared in any source file.
   --  For example, a token representing an error.

private

   package Tree_Vectors is
      new Ada.Containers.Vectors (Positive, Tree);

   type Root_Tree_Type is
     abstract new Aquarius.Messages.Message_Location
     and Aquarius.Locations.Updateable_Location_Interface
     and Root_Aquarius_Object with
      record
         Identity            : Positive;
         Source              : Aquarius.Sources.Source_Reference;
         Offset              : Aquarius.Locations.Location_Offset := 0;
         Line                : Aquarius.Locations.Line_Index      := 1;
         Column              : Aquarius.Locations.Column_Index    := 1;
         Temporary           : Boolean;
         Keep_Parent         : Boolean := False;
         Keep_Siblings       : Boolean := False;
         Messages            : Aquarius.Messages.Message_List;
         Properties          : Aquarius.Properties.Property_List;
         Left, Right, Parent : Tree;
         Foster_Parent       : Tree;
         Children            : Tree_Vectors.Vector;
      end record;

   pragma Inline_Always (Left_Sibling);
   pragma Inline_Always (Right_Sibling);
   pragma Inline_Always (Parent);

   function Source
     (This : Root_Tree_Type)
      return Aquarius.Sources.Source_Reference
   is (This.Source);

   overriding function Offset
     (This : Root_Tree_Type)
      return Aquarius.Locations.Location_Offset
   is (This.Offset);

   overriding function Line
     (This : Root_Tree_Type)
      return Aquarius.Locations.Line_Index
   is (This.Line);

   overriding function Column
     (This : Root_Tree_Type)
      return Aquarius.Locations.Column_Index
   is (This.Column);

end Aquarius.Trees;
