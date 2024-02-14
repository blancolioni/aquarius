private with Ada.Containers.Vectors;
limited with Aquarius.Entries;

package Aquarius.Types is

   pragma Elaborate_Body;

   --  Root_Aquarius_Type: base class of all types.  Derived classes are
   --  defined in children of this package; however it's not required
   --  to use them (see the Klein plugin for an example of defining
   --  plugin-specific types).  The derived types are oriented toward
   --  an Ada plugin, but are general enough to be used for other
   --  languages.

   type Root_Aquarius_Type is abstract new Root_Aquarius_Object with private;
   type Aquarius_Type is access all Root_Aquarius_Type'Class;

   type Array_Of_Types is array (Positive range <>) of Aquarius_Type;

   --  Default version of unify requires that the types access
   --  exactly the same object.  However, if With_Type is Universal
   --  and Item is not, Unify is tried again with swapped arguments
   function Unify (Item      : not null access Root_Aquarius_Type;
                   With_Type : not null access Root_Aquarius_Type'Class)
                  return Aquarius_Type;

   overriding
   function Name (Item : Root_Aquarius_Type)
                 return String;

   --  Description: returns a string describing the type, for use in
   --  error messages.
   function Description (Item : Root_Aquarius_Type)
                        return String
      is abstract;

   --  Create_Derived_Type: implements derived types; that is types
   --  which are based on other types.  Normally, this creates an
   --  identical copy of the type.  For tagged types, it implements
   --  subclassing.
   function Create_Derived_Type (From : Root_Aquarius_Type)
                                return Aquarius_Type
      is abstract;

   --  The size in bits of the type.  This is usually calculated from
   --  the properties of the type.
   function Size_Bits (Item : Root_Aquarius_Type) return Natural;

   --  Set_Entry: if a type is named by an entry, this sets it.
   procedure Set_Entry
     (Item  : in out Root_Aquarius_Type;
      To    : not null access Aquarius.Entries.Table_Entry_Record'Class);

   procedure Set_Default_Size (Item : in out Root_Aquarius_Type;
                               Size : Natural);

   procedure Set_Universal (Item : in out Root_Aquarius_Type);

   --  Set_Constrained: a constrained type has a definite size.
   procedure Set_Constrained
     (Item        : in out Root_Aquarius_Type'Class;
      Constrained : Boolean);

   function New_Named_Type (Name      : String;
                            Base_Type : Aquarius.Types.Aquarius_Type)
                           return Aquarius.Types.Aquarius_Type;

   function Is_Named_Type (Item : not null access Root_Aquarius_Type'Class)
                          return Boolean;

   function Get_Named_Type_Base
     (Item : not null access Root_Aquarius_Type'Class)
     return Aquarius_Type;

   type Possible_Type_Record is new Root_Aquarius_Object with private;
   type Possible_Types is access all Possible_Type_Record'Class;

   procedure Add_Type (To         : in out Possible_Type_Record;
                       Added_Type : Aquarius_Type);

   overriding
   function Name (Item : Possible_Type_Record) return String;

   function Unify (Item       : access Possible_Type_Record;
                   Candidates : access Possible_Type_Record'Class)
                  return Possible_Types;

   function Unifies (Item      : access Possible_Type_Record;
                     Candidate : Aquarius_Type)
                    return Boolean;

   function Count (Item : Possible_Type_Record)
                  return Natural;

   function Get_Type (Item : Possible_Type_Record)
                     return Aquarius_Type;

   function Single_Possible_Type (New_Type : Aquarius_Type)
                              return Possible_Types;

   type Type_Property_Interface is interface;
   function Has_Type (Item : Type_Property_Interface) return Boolean
                       is abstract;
   function Get_Type (Item : Type_Property_Interface) return Aquarius_Type
                       is abstract;
   procedure Set_Type (Item : in out Type_Property_Interface;
                        Typ  : not null access Root_Aquarius_Type'Class)
   is abstract;

   procedure Transfer_Type (From : Type_Property_Interface'Class;
                            To   : in out Type_Property_Interface'Class);

private

   type Root_Aquarius_Type is abstract new Root_Aquarius_Object with
      record
         Size        : Natural;
         Universal   : Boolean := False;
         Constrained : Boolean := True;
         Type_Entry  : access Aquarius.Entries.Table_Entry_Record'Class;
      end record;

   package Type_Vector is
      new Ada.Containers.Vectors (Positive, Aquarius_Type);

   type Possible_Type_Record is new Root_Aquarius_Object with
      record
         Types : Type_Vector.Vector;
      end record;

end Aquarius.Types;
