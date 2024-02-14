private with Ada.Characters.Handling;
private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Containers.Vectors;
private with Ada.Containers.Indefinite_Vectors;
private with Ada.Strings.Unbounded;

private with WL.String_Maps;

with Tagatha.Code;

with Aquarius.Programs;

package Ack is

   type Node_Kind is
     (N_Uninitialized_Node,
      N_Error_Node,
      N_Class_Declaration,
      N_Class_Header,
      N_Class_Name,
      N_Formal_Generics,
      N_Formal_Generic,
      N_Formal_Generic_Name,
      N_Actual_Generics,
      N_Notes,
      N_Note_Entry,
      N_Note_Name,
      N_Note_Value,
      N_Note_Item,
      N_Inheritance,
      N_Inherited,
      N_New_Exports,
      N_Undefine,
      N_Redefine,
      N_Rename,
      N_Select,
      N_Creators,
      N_Creation_Clause,
      N_Features,
      N_Feature_Clause,
      N_Feature_Declaration,
      N_New_Feature,
      N_Declaration_Body,
      N_Formal_Arguments,
      N_Entity_Declaration_Group_List,
      N_Entity_Declaration_Group,
      N_Identifier_List,
      N_Assertion,
      N_Assertion_Clause,
      N_Class_Type,
      N_Anchored_Type,
      N_Tuple_Type,
      N_Extended_Feature_Name,
      N_Feature_Name,
      N_Feature_Alias,
      N_Feature_Value,
      N_Explicit_Value,
      N_Routine,
      N_Precondition,
      N_Local_Declarations,
      N_Postcondition,
      N_Rescue,
      N_Internal,
      N_External,
      N_Explicit_Creation_Type,
      N_Explicit_Creation_Call,
      N_Creation_Call,
      N_Expression,
      N_If_Then,
      N_Iteration,
      N_Initialization,
      N_Invariant,
      N_Exit_Condition,
      N_Loop_Body,
      N_Variant,
      N_Compound,
      N_Assignment,
      N_Creation_Instruction,
      N_Conditional,
      N_Loop,
      N_Check,
      N_Retry,
      N_Precursor,
      N_Operator,
      N_Attachment_Test,
      N_Old,
      N_Tuple,
      N_Constant,
      N_String_Constant,
      N_Character_Constant,
      N_Integer_Constant,
      N_Boolean_Constant,
      N_Variable,
      N_Precursor_Element,
      N_Actual_List,
      N_Identifier,
      N_Get_Property);

   subtype N_Type is Node_Kind range
     N_Class_Type .. N_Tuple_Type;

   subtype N_Instruction is Node_Kind range
     N_Assignment .. N_Precursor;

   subtype N_Expression_Node is Node_Kind range
     N_Precursor .. N_Constant;

   subtype N_Constant_Value is Node_Kind range
     N_String_Constant .. N_Boolean_Constant;

   subtype N_Effective_Routine is Node_Kind range N_Internal .. N_External;

   type Error_Kind is
     (E_No_Error,
      E_Undeclared_Name,
      E_Redefined_Name,
      E_Not_Defined_In,
      E_Not_A_Create_Feature,
      E_Create_Deferred_Class,
      E_Inherited_Expanded_Class,
      E_Requires_Body,
      E_Unnecessary_Redefine,
      E_Missing_Redefine,
      E_Missing_Redefinition,
      E_No_Component,
      E_No_Child,
      E_Missing_Exit_Condition,
      E_Id_List_With_Arguments,
      E_Id_List_With_No_Type,
      E_Id_List_With_Routine,
      E_Type_Error,
      E_Creation_Type_Error,
      E_Iterator_Type_Error,
      E_No_Default_Create_Routine,
      E_Insufficient_Arguments,
      E_Too_Many_Arguments,
      E_Does_Not_Accept_Arguments,
      E_Ignored_Return_Value,
      E_Requires_Value,
      E_Requires_Definition,
      E_Illegal_Redefinition,
      E_Not_An_Iterator,
      E_Value_Might_Be_Void
     );

   type Assertion_Monitoring_Level is
     (Monitor_None,
      Monitor_Preconditions,
      Monitor_Postconditions,
      Monitor_Class_Invariants,
      Monitor_All);

   procedure Set_Default_Monitoring_Level
     (Level : Assertion_Monitoring_Level);

   function Default_Monitoring_Level return Assertion_Monitoring_Level;

   procedure Set_Write_Tables
     (Write_Tables : Boolean);

   procedure Set_Trace
     (Class_Analysis : Boolean);

   procedure Set_Stack_Check
     (Stack_Check : Boolean);

   type Node_Id is private;

   No_Node : constant Node_Id;

   function Kind (Node : Node_Id) return Node_Kind
     with Pre => Node /= No_Node;

   function Copy (Node : Node_Id) return Node_Id;

   type List_Id is private;

   No_List : constant List_Id;

   function New_List return List_Id;
   procedure Append (List : List_Id;
                     Node : Node_Id);

   function Length
     (List : List_Id)
      return Natural;

   procedure Scan
     (List : List_Id;
      Process : not null access
        procedure (Node : Node_Id));

   type Array_Of_Nodes is array (Positive range <>) of Node_Id;

   function To_Array
     (List : List_Id)
      return Array_Of_Nodes;

   type Name_Id is private;

   No_Name : constant Name_Id;

   function Find_Name_Id
     (Name : String)
      return Name_Id;

   function Get_Name_Id
     (Name : String)
      return Name_Id;

   function To_String
     (Name : Name_Id)
      return String;

   function To_Standard_String
     (Name : Name_Id)
      return String;

   type Text_Id is private;

   No_Text : constant Text_Id;

   function Make_Text_Id
     (Text : String)
      return Text_Id;

   function To_String (Id : Text_Id) return String;

   type Byte_Offset is new Natural;

   type Word_Offset is new Natural;

   function Image (Offset : Word_Offset) return String;

   type Bit_Offset is new Natural;

   procedure Push_Offset
     (Unit   : in out Tagatha.Code.Instance'Class;
      Offset : Word_Offset);

   type Root_Entity_Type is abstract tagged private;

   function Deferred
     (Entity : Root_Entity_Type)
      return Boolean
   is abstract;

   function Intrinsic
     (Entity : Root_Entity_Type)
      return Boolean
   is (False);

   type Entity_Type is access all Root_Entity_Type'Class;
   type Constant_Entity_Type is access constant Root_Entity_Type'Class;

   function Class_Context
     (Entity : not null access constant Root_Entity_Type)
      return Constant_Entity_Type
      is abstract;

   function Entity_Name_Id (Entity : Root_Entity_Type'Class) return Name_Id;
   function Standard_Name (Entity : Root_Entity_Type'Class) return String;
   function Declared_Name (Entity : Root_Entity_Type'Class) return String;
   function Qualified_Name (Entity : Root_Entity_Type'Class) return String;
   function Allocator_Name (Entity : Root_Entity_Type'Class) return String;
   function Link_Name (Entity : Root_Entity_Type) return String;
   function Link_Name_Id (Entity : Root_Entity_Type) return Name_Id;
   function Base_File_Name (Entity : Root_Entity_Type'Class) return String;
   function Base_Child_File_Name
     (Entity     : Root_Entity_Type;
      Child_Name : Name_Id)
      return String;

   function Declaration_Context
     (Entity : Root_Entity_Type)
      return Entity_Type;

   function Has_Default_Creation_Routine
     (Entity : Root_Entity_Type)
      return Boolean
   is (False);

   function Default_Creation_Routine
     (Entity : Root_Entity_Type)
      return Entity_Type
   is (null)
     with Pre => Root_Entity_Type'Class (Entity).Has_Default_Creation_Routine;

   function Full_Name (Entity : Root_Entity_Type) return String
   is (Root_Entity_Type'Class (Entity).Qualified_Name);

   function Description (Entity : Root_Entity_Type) return String
   is (Root_Entity_Type'Class (Entity).Qualified_Name);

   function Assertion_Monitoring
     (Entity : Root_Entity_Type'Class)
      return Assertion_Monitoring_Level;

   function Monitor
     (Entity    : Root_Entity_Type'Class;
      Assertion : Assertion_Monitoring_Level)
      return Boolean
   is (Entity.Assertion_Monitoring >= Assertion);

   function Monitor_Preconditions
     (Entity : Root_Entity_Type'Class)
      return Boolean
   is (Entity.Monitor (Monitor_Preconditions));

   function Monitor_Postconditions
     (Entity : Root_Entity_Type'Class)
      return Boolean
   is (Entity.Monitor (Monitor_Postconditions));

   function Monitor_Class_Invariants
     (Entity : Root_Entity_Type'Class)
      return Boolean
   is (Entity.Monitor (Monitor_Class_Invariants));

   procedure Set_Assertion_Monitoring
     (Entity : in out Root_Entity_Type'Class;
      Level  : Assertion_Monitoring_Level);

   function Concrete_Entity
     (Entity : not null access Root_Entity_Type)
      return Entity_Type
   is (Entity_Type (Entity));

   function Expanded
     (Entity : Root_Entity_Type)
      return Boolean
      is abstract;

   function Conforms_To
     (Class : not null access constant Root_Entity_Type;
      Other : not null access constant Root_Entity_Type'Class)
      return Boolean;

   function Proper_Ancestor_Of
     (Ancestor   : not null access constant Root_Entity_Type;
      Descendent : not null access constant Root_Entity_Type'Class)
      return Boolean;

   function Can_Update
     (Entity : Root_Entity_Type)
      return Boolean
   is (False);

   function Attached
     (Entity : not null access constant Root_Entity_Type'Class)
      return Boolean;

   procedure Set_Attached
     (Entity : in out Root_Entity_Type'Class);

   procedure Clear_Attached
     (Entity : in out Root_Entity_Type'Class);

   function Instantiate
     (Entity             : not null access Root_Entity_Type;
      Type_Instantiation : not null access
        function (Generic_Type : Entity_Type) return Entity_Type)
      return Entity_Type
      is abstract;

   function Argument_Count (Entity : Root_Entity_Type) return Natural
   is (0);

   function Argument
     (Entity : Root_Entity_Type;
      Index  : Positive)
      return Entity_Type
   is (null)
     with Pre'Class =>
       Entity.Argument_Count >= Index;

   procedure Add_Shelf
     (Entity : in out Root_Entity_Type;
      Name   : String)
   is null;

   function Shelf
     (Entity : Root_Entity_Type;
      Name   : String)
      return Positive;

   function Declaration_Node
     (Entity : Root_Entity_Type'Class)
      return Node_Id;

   function Has_Type
     (Entity : Root_Entity_Type'Class)
      return Boolean;

   function Get_Type
     (Entity : Root_Entity_Type'Class)
      return Entity_Type;

   function Detachable
     (Entity : Root_Entity_Type)
      return Boolean
   is (False);

   procedure Save_Old_Value
     (Entity : in out Root_Entity_Type;
      Node   : Node_Id)
   is null;

   procedure Push_Old_Value
     (Entity : in out Root_Entity_Type;
      Unit   : in out Tagatha.Code.Instance'Class;
      Node   : Node_Id)
   is null;

   procedure Check_Bound (Entity : not null access Root_Entity_Type) is null;
   procedure Bind (Entity : not null access Root_Entity_Type) is null;

   procedure Push_Entity
     (Entity        : Root_Entity_Type;
      Have_Current  : Boolean;
      Context       : not null access constant Root_Entity_Type'Class;
      Unit          : in out Tagatha.Code.Instance'Class)
   is null;

   procedure Push_Entity_Address
     (Entity        : Root_Entity_Type;
      Have_Current  : Boolean;
      Context       : not null access constant Root_Entity_Type'Class;
      Unit          : in out Tagatha.Code.Instance'Class)
   is null;

   procedure Pop_Entity
     (Entity     : Root_Entity_Type;
      Context    : not null access constant Root_Entity_Type'Class;
      Value_Type : not null access constant Root_Entity_Type'Class;
      Unit       : in out Tagatha.Code.Instance'Class)
   is null;

   procedure Allocate
     (Entity : Root_Entity_Type;
      Unit   : in out Tagatha.Code.Instance'Class)
   is null;

   function Contains
     (Table_Entity : Root_Entity_Type;
      Name         : String;
      Recursive    : Boolean := True)
      return Boolean;

   function Get
     (Table_Entity : not null access constant Root_Entity_Type;
      Name         : String)
      return Entity_Type;
   --  with Pre'Class => Table_Entity.Contains (Name);

   function Contains
     (Table_Entity : Root_Entity_Type'Class;
      Name         : Name_Id)
      return Boolean
   is (Table_Entity.Contains (To_Standard_String (Name)));

   function Get
     (Table_Entity : not null access constant Root_Entity_Type'Class;
      Name         : Name_Id)
      return Entity_Type
   is (Table_Entity.Get (To_Standard_String (Name)));

   procedure Insert
     (Table_Entity : Root_Entity_Type'Class;
      Entity       : not null access Root_Entity_Type'Class)
     with Pre => not Table_Entity.Contains (Entity.Standard_Name, False),
     Post => Table_Entity.Contains (Entity.Standard_Name, False);

   procedure Add_Implicit
     (Table_Entity    : in out Root_Entity_Type;
      Implicit_Entity : not null access Root_Entity_Type'Class);

   procedure Remove_Implicit
     (Table_Entity    : in out Root_Entity_Type);

   function Has_Error
     (Node : Node_Id)
      return Boolean;

   function Get_Error
     (Node : Node_Id)
      return Error_Kind;

   function Get_Error_Entity
     (Node : Node_Id)
      return Constant_Entity_Type;

   function Get_Error_Context
     (Node : Node_Id)
      return Constant_Entity_Type;

   procedure Scan_Errors
     (Top     : Node_Id;
      Process : not null access
        procedure (Node : Node_Id;
                   Error : Error_Kind;
                   Warning : Boolean));

   procedure Error
     (Node    : Node_Id;
      Kind    : Error_Kind;
      Entity  : access constant Root_Entity_Type'Class := null;
      Context : access constant Root_Entity_Type'Class := null)
     with Pre => Kind /= E_Undeclared_Name or else Has_Name (Node);

   procedure Warning
     (Node    : Node_Id;
      Kind    : Error_Kind;
      Entity  : access constant Root_Entity_Type'Class := null;
      Context : access constant Root_Entity_Type'Class := null)
     with Pre => Kind /= E_Undeclared_Name or else Has_Name (Node);

   function Get_Program
     (N : Node_Id)
      return Aquarius.Programs.Program_Tree;

   function Implicit_Entity
     (N : Node_Id)
      return Boolean;

   procedure Set_Implicit_Entity
     (N : Node_Id);

   function Class_Header (N : Node_Id) return Node_Id
     with Pre => Kind (N) = N_Class_Declaration;

   function Class_Name (N : Node_Id) return Node_Id
     with Pre => Kind (N) in N_Class_Header | N_Class_Type;

   function Actual_Generics (N : Node_Id) return Node_Id
     with Pre => Kind (N) = N_Class_Type;

   function Formal_Generics (N : Node_Id) return Node_Id
     with Pre => Kind (N) = N_Class_Header;

   function Formal_Generics_List (N : Node_Id) return List_Id
     with Pre => Kind (N) = N_Formal_Generics;

   function Formal_Generic_Name (N : Node_Id) return Node_Id
     with Pre => Kind (N) = N_Formal_Generic;

   function Notes (N : Node_Id) return Node_Id
     with Pre => Kind (N) = N_Class_Declaration;

   function Note_Name (N : Node_Id) return Node_Id
     with Pre => Kind (N) = N_Note_Entry;

   function Note_Value (N : Node_Id) return Node_Id
     with Pre => Kind (N) = N_Note_Entry;

   function Inheritance (N : Node_Id) return Node_Id
     with Pre => Kind (N) = N_Class_Declaration;

   function Inherits (N : Node_Id) return List_Id
     with Pre => Kind (N) = N_Inheritance;

   function Inherit_Class_Type (N : Node_Id) return Node_Id
     with Pre => Kind (N) = N_Inherited;

   function Redefine (N : Node_Id) return List_Id
     with Pre => Kind (N) = N_Inherited;

   function Identifiers (N : Node_Id) return List_Id
     with Pre => Kind (N) in N_Class_Name | N_Entity_Declaration_Group;

   function Has_Name (N : Node_Id) return Boolean
   is (Kind (N) in N_Identifier | N_Feature_Name | N_Feature_Alias
         | N_Variable | N_Integer_Constant
         | N_Boolean_Constant | N_Character_Constant
         | N_Effective_Routine | N_Precursor_Element
         | N_Explicit_Creation_Call
         | N_Anchored_Type
         | N_Formal_Generic_Name | N_Get_Property
         | N_Attachment_Test | N_Iteration | N_Operator
         | N_Note_Name | N_Note_Item | N_Assertion_Clause);

   function Get_Name (N : Node_Id) return Name_Id
     with Pre => Has_Name (N);

   function Has_Text (N : Node_Id) return Boolean
   is (Kind (N) = N_String_Constant);

   function Get_Text (N : Node_Id) return Text_Id
     with Pre => Has_Text (N);

   function Get_Entity (N : Node_Id) return Entity_Type;
   function Has_Entity (N : Node_Id) return Boolean;

   function Get_Context (N : Node_Id) return Constant_Entity_Type;
   function Has_Context (N : Node_Id) return Boolean;

   function Get_Type (N : Node_Id) return Entity_Type;
   function Has_Type (N : Node_Id) return Boolean;

   function Has_Destination_Type (N : Node_Id) return Boolean;
   function Get_Destination_Type (N : Node_Id) return Constant_Entity_Type
     with Pre => Has_Destination_Type (N);

   function Class_Creators (N : Node_Id) return Node_Id
     with Pre => Kind (N) = N_Class_Declaration;

   function Creator_Clauses (N : Node_Id) return List_Id
     with Pre => Kind (N) = N_Creators;

   function Creator_List (N : Node_Id) return List_Id
     with Pre => Kind (N) = N_Creation_Clause;

   function Class_Features (N : Node_Id) return Node_Id
     with Pre => Kind (N) = N_Class_Declaration;

   function Feature_Clauses (N : Node_Id) return List_Id
     with Pre => Kind (N) = N_Features;

   function Feature_Declarations (N : Node_Id) return List_Id
     with Pre => Kind (N) = N_Feature_Clause;

   function New_Feature_List (N : Node_Id) return List_Id
     with Pre => Kind (N) = N_Feature_Declaration;

   function Extended_Feature_Name (N : Node_Id) return Node_Id
     with Pre => Kind (N) = N_New_Feature;

   function Feature_Name (N : Node_Id) return Node_Id
     with Pre => Kind (N) = N_Extended_Feature_Name;

   function Declaration_Body (N : Node_Id) return Node_Id
     with Pre => Kind (N) = N_Feature_Declaration;

   function Formal_Arguments (N : Node_Id) return Node_Id
     with Pre => Kind (N) = N_Declaration_Body;

   function Declaration_Count (N : Node_Id) return Natural
     with Pre => Kind (N) = N_Entity_Declaration_Group_List;

   function Group_Type (N : Node_Id) return Node_Id
     with Pre => Kind (N) = N_Entity_Declaration_Group;

   function Value_Type (N : Node_Id) return Node_Id
     with Pre => Kind (N) = N_Declaration_Body;

   function Value (N : Node_Id) return Node_Id
     with Pre => Kind (N) = N_Declaration_Body;

   function Entity_Declaration_Group_List (N : Node_Id) return Node_Id
     with Pre => Kind (N) in N_Formal_Arguments | N_Local_Declarations;

   procedure Scan_Entity_Declarations
     (Group   : Node_Id;
      Process : not null access
        procedure (Declaration_Node : Node_Id))
     with Pre => Kind (Group) = N_Entity_Declaration_Group_List;

   function Feature_Alias (N : Node_Id) return Node_Id
     with Pre => Kind (N) in N_External | N_Extended_Feature_Name;

   function Precondition (N : Node_Id) return Node_Id
     with Pre => Kind (N) = N_Routine;

   function Postcondition (N : Node_Id) return Node_Id
     with Pre => Kind (N) = N_Routine;

   function Assertion (N : Node_Id) return Node_Id
     with Pre => Kind (N) in N_Precondition | N_Postcondition;

   function Assertion_Clauses (N : Node_Id) return List_Id
     with Pre => Kind (N) = N_Assertion;

   function Rescue (N : Node_Id) return Node_Id
     with Pre => Kind (N) = N_Routine;

   function Local_Declarations (N : Node_Id) return Node_Id
     with Pre => Kind (N) = N_Routine,
     Post => Local_Declarations'Result = No_Node
       or else Kind (Local_Declarations'Result) = N_Local_Declarations;

   function Effective_Routine (N : Node_Id) return Node_Id
     with Pre => Kind (N) = N_Routine;

   function Once_Routine (N : Node_Id) return Boolean
     with Pre => Kind (N) = N_Internal;

   function Compound (N : Node_Id) return Node_Id
     with Pre => Kind (N) in
     N_Internal | N_Loop_Body | N_Initialization | N_Rescue;

   function Instructions (N : Node_Id) return List_Id
     with Pre => Kind (N) = N_Compound;

   function Variable (N : Node_Id) return Node_Id
     with Pre => Kind (N) in N_Assignment | N_Creation_Call;

   function Expression (N : Node_Id) return Node_Id
     with Pre => Kind (N) in N_Assignment | N_Iteration | N_Exit_Condition
     | N_Assertion_Clause | N_Old;

   function Creation_Call (N : Node_Id) return Node_Id
     with Pre => Kind (N) = N_Creation_Instruction;

   function Explicit_Creation_Call (N : Node_Id) return Node_Id
     with Pre => Kind (N) = N_Creation_Call,
     Post => Explicit_Creation_Call'Result = No_Node
     or else Kind (Explicit_Creation_Call'Result)
     = N_Explicit_Creation_Call;

   function Explicit_Creation_Type (N : Node_Id) return Node_Id
     with Pre => Kind (N) = N_Creation_Instruction,
     Post => Explicit_Creation_Type'Result = No_Node
     or else Kind (Explicit_Creation_Type'Result)
     in N_Type;

   procedure Set_Explicit_Creation_Call
     (N    : Node_Id;
      Name : Name_Id)
     with Pre => Kind (N) = N_Creation_Call
     and then Explicit_Creation_Call (N) = No_Node,
     Post => Explicit_Creation_Call (N) /= No_Node
     and then Get_Name (Explicit_Creation_Call (N)) = Name;

   function Actual_List (N : Node_Id) return Node_Id
     with Pre => Kind (N) in N_Precursor_Element | N_Explicit_Creation_Call;

   function Tuple_Expression_List (N : Node_Id) return List_Id
     with Pre => Kind (N) = N_Tuple;

   function Constant_Value (N : Node_Id) return Node_Id
     with Pre => Kind (N) = N_Constant
     or else Kind (N) = N_Explicit_Value;

   function Boolean_Value (N : Node_Id) return Boolean
     with Pre => Kind (N) = N_Boolean_Constant;

   function Loop_Iteration (N : Node_Id) return Node_Id
     with Pre => Kind (N) = N_Loop,
     Post => Loop_Iteration'Result = No_Node
       or else Kind (Loop_Iteration'Result) = N_Iteration;

   function Loop_Initialization (N : Node_Id) return Node_Id
     with Pre => Kind (N) = N_Loop,
     Post => Loop_Initialization'Result = No_Node
     or else Kind (Loop_Initialization'Result) = N_Initialization;

   function Loop_Invariant (N : Node_Id) return Node_Id
     with Pre => Kind (N) = N_Loop,
     Post => Loop_Invariant'Result = No_Node
     or else Kind (Loop_Invariant'Result) = N_Invariant;

   function Loop_Exit_Condition (N : Node_Id) return Node_Id
     with Pre => Kind (N) = N_Loop,
     Post => Loop_Exit_Condition'Result = No_Node
     or else Kind (Loop_Exit_Condition'Result) = N_Exit_Condition;

   function Loop_Body (N : Node_Id) return Node_Id
     with Pre => Kind (N) = N_Loop,
     Post => Kind (Loop_Body'Result) = N_Loop_Body;

   function Loop_Variant (N : Node_Id) return Node_Id
     with Pre => Kind (N) = N_Loop,
     Post => Loop_Variant'Result = No_Node
     or else Kind (Loop_Variant'Result) = N_Variant;

   function Get_Property (N : Node_Id) return Node_Id
     with Pre => Kind (N) in N_Expression_Node;

private

   type Node_Id is range 0 .. 99_999_999;

   No_Node : constant Node_Id := 0;
   Error_Node : constant Node_Id := 1;

   subtype Real_Node_Id is Node_Id range 2 .. Node_Id'Last;

   type List_Id is range 0 .. 99_999_999;

   No_List : constant List_Id := 0;

   subtype Real_List_Id is List_Id range 1 .. List_Id'Last;

   type Name_Id is range 0 .. 99_999_999;

   No_Name : constant Name_Id := 0;
   Error_Name : constant Name_Id := 1;

   subtype Real_Name_Id is Name_Id range 2 .. Name_Id'Last;

   type Text_Id is range 0 .. 99_999_999;

   No_Text : constant Text_Id := 0;

   subtype Real_Text_Id is Text_Id range 1 .. Text_Id'Last;

   type Program_Id is range 0 .. 99_999_999;

   No_Program : constant Program_Id := 0;
   Error_Program : constant Program_Id := 1;

   subtype Real_Program_Id is Program_Id range 2 .. Program_Id'Last;

   type Field_Index is range 1 .. 6;

   type Node_Field_Array is array (Field_Index) of Node_Id;

   type Node_Record is
      record
         Kind            : Node_Kind  := N_Uninitialized_Node;
         From            : Aquarius.Programs.Program_Tree := null;
         Deferred        : Boolean    := False;
         Expanded        : Boolean    := False;
         Frozen          : Boolean    := False;
         Defining        : Boolean    := False;
         Single          : Boolean    := False;
         Once            : Boolean    := False;
         Detachable      : Boolean    := False;
         Inherited       : Boolean    := False;
         Implicit_Entity : Boolean    := False;
         Attached        : Boolean    := False;
         Field           : Node_Field_Array := [others => No_Node];
         List            : List_Id    := No_List;
         Name            : Name_Id    := No_Name;
         Text            : Text_Id := No_Text;
         Entity          : Entity_Type := null;
         Context         : Constant_Entity_Type := null;
         Node_Type       : Entity_Type := null;
         Dest_Type       : Constant_Entity_Type := null;
         Error           : Error_Kind := E_No_Error;
         Warning         : Boolean := False;
         Error_Entity    : Constant_Entity_Type := null;
         Error_Context   : Constant_Entity_Type := null;
         Integer_Value   : Integer;
         Label           : Natural := 0;
      end record;

   package List_Of_Nodes is
     new Ada.Containers.Doubly_Linked_Lists (Node_Id);

   type List_Record is
      record
         List : List_Of_Nodes.List;
      end record;

   function Copy (List : List_Id) return List_Id;

   package Node_Vectors is
     new Ada.Containers.Vectors (Real_Node_Id, Node_Record);

   package List_Vectors is
     new Ada.Containers.Vectors (Real_List_Id, List_Record);

   package Text_Vectors is
     new Ada.Containers.Indefinite_Vectors (Real_Text_Id, String);

   package Name_Vectors is
     new Ada.Containers.Indefinite_Vectors (Real_Name_Id, String);

   package Name_Maps is
     new WL.String_Maps (Name_Id);

   Node_Table   : Node_Vectors.Vector;
   List_Table   : List_Vectors.Vector;
   Text_Table   : Text_Vectors.Vector;
   Name_Table   : Name_Vectors.Vector;
   Name_Map     : Name_Maps.Map;

   function To_String
     (Name : Name_Id)
      return String
   is (if Name = No_Name then "" else Name_Table (Name));

   function To_Standard_String
     (Name : Name_Id)
      return String
   is (Ada.Characters.Handling.To_Lower (To_String (Name)));

   function To_String (Id : Text_Id) return String
   is (if Id = No_Text then "" else Text_Table (Id));

   function Length
     (List : List_Id)
      return Natural
   is (if List = No_List then 0
       else Natural (List_Table (List).List.Length));

   function Get_Program
     (N : Node_Id)
      return Aquarius.Programs.Program_Tree
   is (Node_Table (N).From);

   function Implicit_Entity
     (N : Node_Id)
      return Boolean
   is (Node_Table.Element (N).Implicit_Entity);

   function Field_1 (Node : Node_Id) return Node_Id
   is (Node_Table (Node).Field (1));

   function Field_2 (Node : Node_Id) return Node_Id
   is (Node_Table (Node).Field (2));

   function Field_3 (Node : Node_Id) return Node_Id
   is (Node_Table (Node).Field (3));

   function Field_4 (Node : Node_Id) return Node_Id
   is (Node_Table (Node).Field (4));

   function Field_5 (Node : Node_Id) return Node_Id
   is (Node_Table (Node).Field (5));

   function Field_6 (Node : Node_Id) return Node_Id
   is (Node_Table (Node).Field (6));

   function Kind (Node : Node_Id) return Node_Kind
   is (Node_Table.Element (Node).Kind);

   function Label (Node : Node_Id) return Natural
   is (Node_Table.Element (Node).Label);

   function Class_Header (N : Node_Id) return Node_Id
   is (Node_Table.Element (N).Field (2));

   function Class_Name (N : Node_Id) return Node_Id
   is (Field_2 (N));

   function Actual_Generics (N : Node_Id) return Node_Id
   is (Field_3 (N));

   function Actual_Generics_List (N : Node_Id) return List_Id
   is (Node_Table (N).List)
   with Pre => Kind (N) = N_Actual_Generics;

   function Tuple_Argument_List (N : Node_Id) return List_Id
   is (Node_Table.Element (N).List)
   with Pre => Kind (N) = N_Tuple_Type;

   function Formal_Generics (N : Node_Id) return Node_Id
   is (Field_3 (N));

   function Formal_Generics_List (N : Node_Id) return List_Id
   is (Node_Table (N).List);

   function Formal_Generic_Name (N : Node_Id) return Node_Id
   is (Field_1 (N));

   function Notes (N : Node_Id) return Node_Id
   is (Field_1 (N));

   function Note_Name (N : Node_Id) return Node_Id
   is (Field_1 (N));

   function Note_Value (N : Node_Id) return Node_Id
   is (Field_2 (N));

   function Inheritance (N : Node_Id) return Node_Id
   is (Field_3 (N));

   function Inherits (N : Node_Id) return List_Id
   is (Node_Table (N).List);

   function Inherit_Class_Type (N : Node_Id) return Node_Id
   is (Field_1 (N));

   function Redefine (N : Node_Id) return List_Id
   is (if Field_4 (N) = No_Node then No_List
       else Node_Table (Field_4 (N)).List);

   function Identifiers (N : Node_Id) return List_Id
   is (Node_Table.Element (N).List);

   function Feature_Clauses (N : Node_Id) return List_Id
   is (Node_Table.Element (N).List);

   function Feature_Declarations (N : Node_Id) return List_Id
   is (Node_Table.Element (N).List);

   function New_Feature_List (N : Node_Id) return List_Id
   is (Node_Table.Element (N).List);

   function Extended_Feature_Name (N : Node_Id) return Node_Id
   is (Node_Table.Element (N).Field (1));

   function Feature_Name (N : Node_Id) return Node_Id
   is (Node_Table.Element (N).Field (1));

   function Declaration_Body (N : Node_Id) return Node_Id
   is (Node_Table.Element (N).Field (1));

   function Formal_Arguments (N : Node_Id) return Node_Id
   is (Node_Table.Element (N).Field (1));

   function Declaration_Count (N : Node_Id) return Natural
   is (Node_Table.Element (N).Integer_Value);

   procedure Set_Declaration_Count
     (N : Node_Id;
      Count : Natural)
     with Pre => Kind (N) = N_Entity_Declaration_Group;

   function Group_Type (N : Node_Id) return Node_Id
   is (Field_1 (N));

   function Value_Type (N : Node_Id) return Node_Id
   is (Node_Table.Element (N).Field (2));

   function Assertion (N : Node_Id) return Node_Id
   is (Field_1 (N));

   function Assertion_Clauses (N : Node_Id) return List_Id
   is (Node_Table.Element (N).List);

   function Precondition (N : Node_Id) return Node_Id
   is (Field_1 (N));

   function Local_Declarations (N : Node_Id) return Node_Id
   is (Field_2 (N));

   function Postcondition (N : Node_Id) return Node_Id
   is (Field_4 (N));

   function Rescue (N : Node_Id) return Node_Id
   is (Field_5 (N));

   function Value (N : Node_Id) return Node_Id
   is (Node_Table.Element (N).Field (3));

   function Entity_Declaration_Group_List (N : Node_Id) return Node_Id
   is (Field_1 (N));

   function Get_Name (N : Node_Id) return Name_Id
   is (Node_Table.Element (N).Name);

   function Get_Text (N : Node_Id) return Text_Id
   is (Node_Table.Element (N).Text);

   function Has_Entity (N : Node_Id) return Boolean
   is (Node_Table.Element (N).Entity /= null);

   function Get_Entity (N : Node_Id) return Entity_Type
   is (Node_Table.Element (N).Entity);

   function Has_Context (N : Node_Id) return Boolean
   is (Node_Table.Element (N).Context /= null);

   function Get_Context (N : Node_Id) return Constant_Entity_Type
   is (Node_Table.Element (N).Context);

   function Has_Type (N : Node_Id) return Boolean
   is (Node_Table.Element (N).Node_Type /= null);

   function Get_Type (N : Node_Id) return Entity_Type
   is (Node_Table.Element (N).Node_Type);

   function Has_Destination_Type (N : Node_Id) return Boolean
   is (Node_Table.Element (N).Dest_Type /= null);

   function Get_Destination_Type (N : Node_Id) return Constant_Entity_Type
   is (Node_Table.Element (N).Dest_Type);

   procedure Set_Destination_Type
     (N       : Node_Id;
      To_Type : not null access constant Root_Entity_Type'Class);

   function Class_Creators (N : Node_Id) return Node_Id
   is (Node_Table.Element (N).Field (4));

   function Creator_Clauses (N : Node_Id) return List_Id
   is (Node_Table.Element (N).List);

   function Creator_List (N : Node_Id) return List_Id
   is (Node_Table.Element (N).List);

   function Class_Features (N : Node_Id) return Node_Id
   is (Node_Table.Element (N).Field (5));

   function Feature_Alias (N : Node_Id) return Node_Id
   is (if Kind (N) = N_External then Field_1 (N) else Field_2 (N));

   function Effective_Routine (N : Node_Id) return Node_Id
   is (Field_3 (N));

   function Once_Routine (N : Node_Id) return Boolean
   is (Node_Table.Element (N).Once);

   function Compound (N : Node_Id) return Node_Id
   is (Field_1 (N));

   function Instructions (N : Node_Id) return List_Id
   is (Node_Table.Element (N).List);

   function Variable (N : Node_Id) return Node_Id
   is (Field_1 (N));

   function Explicit_Creation_Type (N : Node_Id) return Node_Id
   is (Field_2 (N));

   function Creation_Call (N : Node_Id) return Node_Id
   is (Field_1 (N));

   function Explicit_Creation_Call (N : Node_Id) return Node_Id
   is (Field_2 (N));

   function Expression (N : Node_Id) return Node_Id
   is (if Kind (N) = N_Assignment
       then Field_2 (N) else Field_1 (N));

   function Actual_List (N : Node_Id) return Node_Id
   is (Field_2 (N));

   function Tuple_Expression_List (N : Node_Id) return List_Id
   is (Node_Table.Element (N).List);

   function Constant_Value (N : Node_Id) return Node_Id
   is (Field_2 (N));

   function Boolean_Value (N : Node_Id) return Boolean
   is (To_Standard_String (Get_Name (N)) = "true");

   function Loop_Iteration (N : Node_Id) return Node_Id
   is (Field_1 (N));

   function Loop_Initialization (N : Node_Id) return Node_Id
   is (Field_2 (N));

   function Loop_Invariant (N : Node_Id) return Node_Id
   is (Field_3 (N));

   function Loop_Exit_Condition (N : Node_Id) return Node_Id
   is (Field_4 (N));

   function Loop_Body (N : Node_Id) return Node_Id
   is (Field_5 (N));

   function Loop_Variant (N : Node_Id) return Node_Id
   is (Field_6 (N));

   function Get_Property (N : Node_Id) return Node_Id
   is (Field_6 (N));

   function Has_Error
     (Node : Node_Id)
      return Boolean
   is (Node_Table.Element (Node).Error /= E_No_Error);

   function Get_Error
     (Node : Node_Id)
      return Error_Kind
   is (Node_Table.Element (Node).Error);

   function Get_Error_Entity
     (Node : Node_Id)
      return Constant_Entity_Type
   is (Node_Table.Element (Node).Error_Entity);

   function Get_Error_Context
     (Node : Node_Id)
      return Constant_Entity_Type
   is (Node_Table.Element (Node).Error_Context);

   function Require_Else (Node : Node_Id) return Boolean
   is (Node_Table.Element (Node).Inherited);

   function Ensure_Then (Node : Node_Id) return Boolean
   is (Node_Table.Element (Node).Inherited);

   function New_Node
     (Kind       : Node_Kind;
      From       : Aquarius.Programs.Program_Tree;
      Deferred   : Boolean     := False;
      Expanded   : Boolean     := False;
      Frozen     : Boolean     := False;
      Defining   : Boolean     := False;
      Once       : Boolean     := False;
      Detachable : Boolean     := False;
      Inherited  : Boolean     := False;
      Field_1    : Node_Id     := No_Node;
      Field_2    : Node_Id     := No_Node;
      Field_3    : Node_Id     := No_Node;
      Field_4    : Node_Id     := No_Node;
      Field_5    : Node_Id     := No_Node;
      Field_6    : Node_Id     := No_Node;
      List       : List_Id     := No_List;
      Name       : Name_Id     := No_Name;
      Text       : Text_Id     := No_Text;
      Entity     : Entity_Type := null)
    return Node_Id;

   procedure Depth_First_Scan
     (Top : Node_Id;
      Process : not null access
        procedure (Node : Node_Id));

   procedure Set_Context
     (Node    : Real_Node_Id;
      Context : not null access constant Root_Entity_Type'Class)
     with Pre => Get_Context (Node) = null;

   procedure Set_Entity
     (Node : Real_Node_Id;
      Entity : not null access Root_Entity_Type'Class)
     with Pre => Get_Entity (Node) = null;

   procedure Set_Type
     (Node   : Real_Node_Id;
      Entity : not null access Root_Entity_Type'Class)
     with Pre => Get_Type (Node) = null;

   procedure Set_Label
     (Node  : Real_Node_Id;
      Value : Positive)
     with Pre => Label (Node) = 0;

   function Contains_Name
     (List : List_Id;
      Name : Name_Id)
      return Boolean;

   package Compiled_Class_Maps is
     new WL.String_Maps (String);

   Class_Object_Paths : Compiled_Class_Maps.Map;

   package Class_Node_Maps is
     new WL.String_Maps (Node_Id);

   Loaded_Classes : Class_Node_Maps.Map;

   package Class_Node_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Node_Id);

   Partial_Class_List : Class_Node_Lists.List;

   function "+" (S : String) return Ada.Strings.Unbounded.Unbounded_String
                 renames Ada.Strings.Unbounded.To_Unbounded_String;

   function "-" (S : Ada.Strings.Unbounded.Unbounded_String) return String
                 renames Ada.Strings.Unbounded.To_String;

   package Entity_Maps is
     new WL.String_Maps (Entity_Type);

   package List_Of_Entities is
     new Ada.Containers.Doubly_Linked_Lists (Entity_Type);

   package List_Of_Constant_Entities is
     new Ada.Containers.Doubly_Linked_Lists (Constant_Entity_Type);

   type Entity_Table_Record is
      record
         Map       : Entity_Maps.Map;
         List      : List_Of_Entities.List;
         Implicits : List_Of_Entities.List;
      end record;

   type Entity_Table is access Entity_Table_Record;

   package Shelf_Maps is
     new WL.String_Maps (Positive);

   type Root_Entity_Type is abstract tagged
      record
         Name                 : Ada.Strings.Unbounded.Unbounded_String;
         Source_Name          : Ada.Strings.Unbounded.Unbounded_String;
         Declaration_Node     : Node_Id;
         Declaration_Context  : Entity_Type;
         Value_Type           : Entity_Type;
         Destination_Type     : Constant_Entity_Type;
         Children             : Entity_Table;
         Parent_Environment   : Entity_Type;
         Sequence_Number      : Natural := 0;
         Created              : Boolean := False;
         Attached             : Boolean := False;
         Has_Monitoring_Level : Boolean := False;
         Monitoring_Level     : Assertion_Monitoring_Level;
         Shelves              : Shelf_Maps.Map;
      end record;

   function Identity
     (Entity : Root_Entity_Type'Class)
      return String
   is ("[e" & Integer'Image (-Entity.Sequence_Number) & "/node"
       & Integer'Image (-(Integer (Entity.Declaration_Node)))
       & " " & Entity.Qualified_Name
       & "]");

   function Has_Context
     (Entity : Root_Entity_Type)
      return Boolean
   is (Entity.Declaration_Context /= null);

   function Declaration_Context
     (Entity : Root_Entity_Type)
      return Entity_Type
   is (Entity.Declaration_Context);

   function Standard_Name (Entity : Root_Entity_Type'Class) return String
   is (-Entity.Name);

   function Entity_Name_Id (Entity : Root_Entity_Type'Class) return Name_Id
   is (Get_Name_Id (Entity.Standard_Name));

   function Declared_Name (Entity : Root_Entity_Type'Class) return String
   is (-Entity.Source_Name);

   function Context_Name
     (Entity : Root_Entity_Type'Class;
      Separator : String;
      Standard_Names : Boolean)
      return String
   is ((if Entity.Has_Context
        then Entity.Declaration_Context.Context_Name
          (Separator, Standard_Names) & Separator else "")
       & (if Standard_Names
          then Entity.Standard_Name else Entity.Declared_Name));

   function Allocator_Name (Entity : Root_Entity_Type'Class) return String
   is (Entity.Link_Name & ".$create");

   function Qualified_Name (Entity : Root_Entity_Type'Class) return String
   is (Entity.Context_Name (".", False));

   function Link_Name (Entity : Root_Entity_Type) return String
   is (Entity.Context_Name (".", True));

   function Link_Name_Id (Entity : Root_Entity_Type) return Name_Id
   is (Get_Name_Id (Entity.Context_Name (".", True)));

   function Base_File_Name (Entity : Root_Entity_Type'Class) return String
   is (Entity.Context_Name ("-", True));

   function Base_Child_File_Name
     (Entity     : Root_Entity_Type;
      Child_Name : Name_Id)
      return String
   is (Entity.Base_File_Name & "-" & To_Standard_String (Child_Name));

   function Assertion_Monitoring
     (Entity : Root_Entity_Type'Class)
      return Assertion_Monitoring_Level
   is (if Entity.Has_Monitoring_Level
       then Entity.Monitoring_Level
       elsif Entity.Has_Context
       then Entity.Declaration_Context.Assertion_Monitoring
       else Default_Monitoring_Level);

   function Has_Type
     (Entity : Root_Entity_Type'Class)
      return Boolean
   is (Entity.Value_Type /= null);

   function Get_Type
     (Entity : Root_Entity_Type'Class)
      return Entity_Type
   is (Entity.Value_Type);

   function Declaration_Node
     (Entity : Root_Entity_Type'Class)
      return Node_Id
   is (Entity.Declaration_Node);

   function Attached
     (Entity : not null access constant Root_Entity_Type'Class)
      return Boolean
   is (Entity.Attached);

   function Shelf
     (Entity : Root_Entity_Type;
      Name   : String)
      return Positive
   is (Entity.Shelves.Element (Name));

   function Proper_Ancestor_Of
     (Ancestor   : not null access constant Root_Entity_Type;
      Descendent : not null access constant Root_Entity_Type'Class)
      return Boolean
   is (False);

   procedure Create
     (Entity             : in out Root_Entity_Type'Class;
      Name               : Name_Id;
      Node               : Node_Id;
      Table              : Boolean;
      Parent_Environment : access Root_Entity_Type'Class := null;
      Context            : access Root_Entity_Type'Class := null)
     with Pre => not Entity.Created,
     Post => Entity.Created;

   procedure Instantiated
     (Entity : in out Root_Entity_Type'Class);

   function Next_String_Label
     (Base_Name : String)
      return String;

   procedure Push_String_Constant
     (Code : in out Tagatha.Code.Instance'Class;
      Text : String);

   Local_Default_Monitoring_Level : Assertion_Monitoring_Level :=
                                      Monitor_All;

   function Default_Monitoring_Level return Assertion_Monitoring_Level
   is (Local_Default_Monitoring_Level);

   Local_Write_Tables           : Boolean := True;
   Trace_Class_Analysis         : Boolean := False;
   Stack_Check_Enabled          : Boolean := False;
   Local_Warn_No_Default_Create : Boolean := False;

end Ack;
