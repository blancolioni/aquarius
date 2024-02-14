with Ada.Containers.Vectors;

with Aquarius.Names;

package Aquarius.Properties is

   pragma Elaborate_Body;

   Property_Error : exception;

   type Property_Type is private;

   type Property_Type_List is private;

   type Property_Pool is
     interface and Root_Aquarius_Object;

   function Get_Property_Types (From : Property_Pool)
                               return Property_Type_List
      is abstract;

   procedure Create_Property
     (Pool      : in out Property_Pool'Class;
      Prop      : in out Property_Type;
      Name      : String;
      Inherited : Boolean;
      Has_Value : Boolean);

   function Have_Property_Type
     (Pool : Property_Pool'Class;
      Name : String)
     return Boolean;

   function Get_Property_Type
     (Pool : Property_Pool'Class;
      Name : String)
     return Property_Type;

   function Empty_Pool return Property_Type_List;

   function Is_Inherited (Property  : Property_Type) return Boolean;
   function Get_Name (Property : Property_Type) return String;

   type Property_List is private;

   function Get (Item : Property_List;
                 Prop : Property_Type)
                return access Root_Aquarius_Object'Class;

   procedure Set (Item  : in out          Property_List;
                  Prop  : Property_Type;
                  Value : not null access Root_Aquarius_Object'Class);

   procedure Set (Item  : in out Property_List;
                  Prop  : Property_Type);

   procedure Clear (Item  : in out Property_List;
                    Prop  : Property_Type);

   function Exists (Item : Property_List;
                    Prop : Property_Type)
                   return Boolean;

   --  Predefined properties
   function Grammar_Property return Property_Type;
   function Interactor_Property return Property_Type;
   function Project_Property return Property_Type;
   function Syntax_Property return Property_Type;
   function Entry_Property return Property_Type;
   function Symbol_Table_Property return Property_Type;
   function Pool_Property return Property_Type;

   function Type_Property return Property_Type;
   function Type_Error_Property return Property_Type;
   function Inferred_Types_Property return Property_Type;
   function Possible_Types_Property return Property_Type;

   function UI_Property return Property_Type;

   function Tagatha_Property return Property_Type;

   --  show properties that have been set in a list
   function Show_Set_Properties (List : Property_List) return String;

private

   type Property_Type is
      record
         Name      : Aquarius.Names.Aquarius_Name;
         Inherited : Boolean;
         Has_Value : Boolean;
         Index     : Positive;
      end record;

   type Property_Type_List_Record;
   type Property_Type_List is access Property_Type_List_Record;

   type Property_Value is
     record
        Index  : Positive;
        Value  : access Root_Aquarius_Object'Class;
     end record;

   package Property_Vector is
      new Ada.Containers.Vectors (Positive, Property_Value);

   Max_Property_Types : constant := 64;
   type Property_Present_Flags is
     array (1 .. Max_Property_Types) of Boolean;
   pragma Pack (Property_Present_Flags);

   type Property_List is
      record
         Present    : Property_Present_Flags := [others => False];
         Properties : Property_Vector.Vector;
      end record;

end Aquarius.Properties;
