with Ada.Strings.Unbounded;
with Ada.Tags;

private with Ada.Containers.Vectors;
private with Ada.Containers.Hashed_Maps;
private with Ada.Strings.Unbounded.Hash;

with Aquarius.Trees;

package Aquarius.Entries is

   pragma Elaborate_Body;

   type Symbol_Table_Record is new Root_Aquarius_Object with private;
   type Symbol_Table is access all Symbol_Table_Record'Class;

   No_Symbol_Table : constant Symbol_Table := null;

   function New_Symbol_Table (Name     : String;
                              Parent   : Symbol_Table := No_Symbol_Table)
                             return Symbol_Table;

   procedure Insert_Table (Child_Table : Symbol_Table;
                           Into_Table  : Symbol_Table);

   overriding
   function Name (Item : Symbol_Table_Record)
                 return String;

   type Table_Entry_Record is abstract new Root_Aquarius_Object with private;

   procedure Create_Entry
     (Item          : in out Table_Entry_Record;
      Name          : String;
      Declaration   : access Aquarius.Trees.Root_Tree_Type'Class);

   procedure Set_Display_Name (Item         : access Table_Entry_Record;
                               Display_Name : String);

   overriding
   function Name (Item : Table_Entry_Record) return String;

   type Table_Entry is access all Table_Entry_Record'Class;

   function Is_Null (Item : access Table_Entry_Record'Class) return Boolean;
   function Display_Name (Item : access Table_Entry_Record) return String;
   function Owner (Item : access Table_Entry_Record) return Symbol_Table;
   function Entry_Owner (Item : access Table_Entry_Record)
                        return Table_Entry;
   function Declaration (Item : access Table_Entry_Record)
                        return Aquarius.Trees.Tree;
   function Implementation (Item : access Table_Entry_Record)
                            return Aquarius.Trees.Tree;
   --  See note for Set_Implementation below.  May return same tree
   --  as Declaration

   function Incomplete (Item : access Table_Entry_Record) return Boolean;

   procedure Set_Complete (Item     : access Table_Entry_Record;
                           Complete : Boolean);

   procedure Rename (Item     : access Table_Entry_Record;
                     New_Name : String);

   procedure Set_Declaration
     (Item        : access Table_Entry_Record;
      Declaration : access Aquarius.Trees.Root_Tree_Type'Class);

   procedure Set_Implementation
     (Item           : access Table_Entry_Record;
      Implementation : access Aquarius.Trees.Root_Tree_Type'Class);
   --  Provide the program tree which implements this entry, as opposed
   --  to the one which declares it.  In many languages, this will be
   --  the same, and Set_Implementation need never be called.

   type Entry_Constraint is abstract tagged private;
   function Match (Constraint : Entry_Constraint;
                   Item       : access Table_Entry_Record'Class)
                  return Boolean is abstract;

   type Name_Constraint is new Entry_Constraint with private;

   overriding
   function Match (Constraint : Name_Constraint;
                   Item       : access Table_Entry_Record'Class)
                  return Boolean;

   procedure Initialise_Constraint (Constraint : in out Name_Constraint;
                                    Name       : String);

   type Class_Constraint is new Entry_Constraint with private;
   overriding
   function Match (Constraint : Class_Constraint;
                   Item       : access Table_Entry_Record'Class)
                  return Boolean;

   function Create_Class_Constraint (Class : Ada.Tags.Tag)
                                    return Entry_Constraint'Class;

   procedure Initialise_Constraint (Constraint : in out Class_Constraint;
                                    Class      : Ada.Tags.Tag);

   type Proposition_Constraint is new Entry_Constraint with private;

   overriding
   function Match (Constraint : Proposition_Constraint;
                   Item       : access Table_Entry_Record'Class)
                  return Boolean;

   type Proposition_Evaluator is
     access function (Item : access Table_Entry_Record'Class) return Boolean;

   function Create_Proposition_Constraint
     (Proposition   : Proposition_Evaluator;
      Search_Parent : Boolean := True)
     return Entry_Constraint'Class;

   type Named_Proposition_Constraint is new Name_Constraint with private;

   overriding
   function Match (Constraint : Named_Proposition_Constraint;
                   Item       : access Table_Entry_Record'Class)
                  return Boolean;

   function Create_Named_Proposition_Constraint
     (Name        : String;
      Proposition : Proposition_Evaluator)
     return Entry_Constraint'Class;

   type Array_Of_Entries is array (Positive range <>) of Table_Entry;

   function Search (Table : access Symbol_Table_Record;
                    Name  : String)
                   return Array_Of_Entries;

   function Search (Table      : access Symbol_Table_Record;
                    Constraint : Entry_Constraint'Class)
                   return Array_Of_Entries;

   --  Retrieve is a version of Search that either returns a single result,
   --  no result, or raises an exception.
   function Retrieve (Table : access Symbol_Table_Record;
                      Name  : String)
                     return Table_Entry;

   function Exists (Table : access Symbol_Table_Record;
                    Name  : String)
                   return Boolean;

   --  Get_Contents: return an array containing everything in the table
   function Get_Contents (Table : access Symbol_Table_Record)
                         return Array_Of_Entries;

   procedure Insert (Table     : access Symbol_Table_Record;
                     Item      : Table_Entry);

   procedure Remove (Table     : access Symbol_Table_Record;
                     Item      : access Table_Entry_Record'Class);

   --  Set_Value_Size: set the room taken up by things with values
   procedure Set_Value_Size (Table : access Symbol_Table_Record;
                             Size  : Natural);

   function Value_Size (Table : access Symbol_Table_Record)
                       return Natural;

   type Entry_Property_Interface is interface;
   function Has_Entry (Item : Entry_Property_Interface) return Boolean
                       is abstract;
   function Get_Entry (Item : Entry_Property_Interface) return Table_Entry
                       is abstract;
   procedure Set_Entry (Item : in out Entry_Property_Interface;
                        Ent  : not null access Table_Entry_Record'Class)
   is abstract;

   procedure Transfer_Entry (From : Entry_Property_Interface'Class;
                             To   : in out Entry_Property_Interface'Class);

private

   package Table_Entry_Vector is
     new Ada.Containers.Vectors (Positive, Table_Entry);

   package Hash_Map is
      new Ada.Containers.Hashed_Maps
     (Key_Type        => Ada.Strings.Unbounded.Unbounded_String,
      Element_Type    => Table_Entry_Vector.Vector,
      Hash            => Ada.Strings.Unbounded.Hash,
      Equivalent_Keys => Ada.Strings.Unbounded."=",
      "="             => Table_Entry_Vector."=");

   type Entry_Watcher is access all Watcher'Class;

   package Watcher_Vector is new
     Ada.Containers.Vectors (Positive, Entry_Watcher);

   type Symbol_Table_Record is new Root_Aquarius_Object with
      record
         Table_Name : Ada.Strings.Unbounded.Unbounded_String;
         Parent     : Symbol_Table;
         Map        : Hash_Map.Map;
         Value_Size : Natural;
      end record;

   type Table_Entry_Record is abstract new Root_Aquarius_Object with
      record
         Name           : Ada.Strings.Unbounded.Unbounded_String;
         Display_Name   : Ada.Strings.Unbounded.Unbounded_String;
         Declaration    : Aquarius.Trees.Tree;
         Implementation : Aquarius.Trees.Tree;
         Owner          : Symbol_Table;
         Entry_Owner    : Table_Entry;
         Complete       : Boolean;
      end record;

   type Entry_Constraint is abstract tagged
      record
         Search_Parent : Boolean := True;
      end record;

   type Name_Constraint is new Entry_Constraint with
      record
         Name : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   type Class_Constraint is new Entry_Constraint with
      record
         Class : Ada.Tags.Tag;
      end record;

   type Proposition_Constraint is new Entry_Constraint with
      record
         Proposition : Proposition_Evaluator;
      end record;

   type Named_Proposition_Constraint is new Name_Constraint with
      record
         Proposition : Proposition_Evaluator;
      end record;

end Aquarius.Entries;
