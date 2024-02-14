private with Ada.Containers.Indefinite_Ordered_Maps;
private with Ada.Strings.Fixed.Less_Case_Insensitive;

private package Ack.Bindings.Actions is

   type Ack_Binding_Table is private;

   procedure Scan_Trees
     (Table : Ack_Binding_Table;
      Process : not null access
        procedure (Tree_Name : String));

   procedure Add_Tree
     (Table     : in out Ack_Binding_Table;
      Tree_Name : String);

   procedure Create_Binding
     (Table             : in out Ack_Binding_Table;
      Parent_Tree_Name  : String;
      Child_Tree_Name   : String;
      Position          : Binding_Position);

   function Have_Binding
     (Table             : Ack_Binding_Table;
      Parent_Tree_Name  : String;
      Child_Tree_Name   : String;
      Position          : Binding_Position)
      return Boolean;

private

   type Child_Binding_Record is
      record
         Child_Name : Name_Id;
         Position   : Binding_Position;
      end record;

   package Child_Binding_Maps is
     new Ada.Containers.Indefinite_Ordered_Maps
       (Key_Type     => String,
        Element_Type => Child_Binding_Record,
        "<"          => Ada.Strings.Fixed.Less_Case_Insensitive);

   package Binding_Maps is
     new Ada.Containers.Indefinite_Ordered_Maps
       (Key_Type     => String,
        Element_Type => Child_Binding_Maps.Map,
        "<"          => Ada.Strings.Fixed.Less_Case_Insensitive,
        "="          => Child_Binding_Maps."=");

   type Ack_Binding_Table is
      record
         Map : Binding_Maps.Map;
      end record;

end Ack.Bindings.Actions;
