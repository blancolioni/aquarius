private with Ada.Containers.Doubly_Linked_Lists;

with Ack.Classes;
with Ack.Features;

package Ack.Types is

   type Type_Entity_Record is
     new Root_Entity_Type with private;

   function Has_Feature
     (Typ   : not null access constant Type_Entity_Record'Class;
      Name  : Name_Id)
      return Boolean;

   function Is_Generic_Formal_Type
     (Typ : Type_Entity_Record'Class)
      return Boolean;

   function Feature
     (Typ   : not null access constant Type_Entity_Record'Class;
      Name  : Name_Id)
      return Ack.Features.Feature_Entity
     with Pre => Typ.Has_Feature (Name);

   function Has_Aliased_Feature
     (Typ   : Type_Entity_Record'Class;
      Alias : Name_Id;
      Infix : Boolean)
      return Boolean;

   function Aliased_Feature
     (Typ   : Type_Entity_Record'Class;
      Alias : Name_Id;
      Infix : Boolean)
      return Ack.Features.Feature_Entity
     with Pre => Typ.Has_Aliased_Feature (Alias, Infix);

   function Class
     (Typ : Type_Entity_Record'Class)
      return Ack.Classes.Class_Entity;

   overriding function Expanded
     (Typ : Type_Entity_Record)
      return Boolean;

   overriding function Deferred
     (Typ : Type_Entity_Record)
      return Boolean;

   overriding function Has_Default_Creation_Routine
     (Typ : Type_Entity_Record)
      return Boolean;

   overriding function Default_Creation_Routine
     (Typ : Type_Entity_Record)
      return Entity_Type;

   type Type_Entity is access all Type_Entity_Record'Class;
   type Constant_Type_Entity is access constant Type_Entity_Record'Class;

   function Generic_Binding_Count
     (Typ : Type_Entity_Record'Class)
      return Natural;

   function Generic_Binding
     (Typ   : Type_Entity_Record'Class;
      Index : Positive)
      return Type_Entity;

   function Get_Ancestor_Type
     (Typ   : not null access constant Type_Entity_Record'Class;
      Ancestor : not null access constant
        Ack.Classes.Class_Entity_Record'Class)
      return access constant Type_Entity_Record'Class;

   function Has_Type_Entity
     (Node : Node_Id)
      return Boolean;

   function Get_Type_Entity
     (Node : Node_Id)
      return Type_Entity
     with Pre => Kind (Node) in
     N_Class_Name | N_Class_Type | N_Tuple_Type | N_Anchored_Type
     and then Has_Type_Entity (Node);

   type Array_Of_Types is array (Positive range <>) of Type_Entity;

   Empty_Type_Array : Array_Of_Types (1 .. 0);

   function New_Class_Type
     (Node       : Node_Id;
      Class      : not null access
        Ack.Classes.Class_Entity_Record'Class;
      Detachable : Boolean)
      return Type_Entity;

   function New_Anchored_Type
     (Node       : Node_Id;
      Class      : not null access Ack.Classes.Class_Entity_Record'Class;
      Anchor     : Name_Id)
      return Type_Entity;

   function Instantiate_Generic_Class
     (Node            : Node_Id;
      Generic_Class   : Ack.Classes.Class_Entity;
      Generic_Actuals : Array_Of_Types;
      Detachable      : Boolean)
      return Type_Entity;

   function Update_Type_Instantiation
     (Instantiated_Type : not null access constant Type_Entity_Record'Class;
      Type_With_Bindings : not null access constant
        Type_Entity_Record'Class)
      return Constant_Type_Entity;

   function New_Generic_Formal_Type
     (Name          : Name_Id;
      Node          : Node_Id;
      Generic_Class : Ack.Classes.Class_Entity;
      Constraints   : Array_Of_Types := Empty_Type_Array)
      return Type_Entity;

   function Get_Concrete_Type
     (Of_Type : not null access Type_Entity_Record'Class;
      Current : not null access Type_Entity_Record'Class;
      Feature : not null access constant
        Ack.Features.Feature_Entity_Record'Class)
      return Type_Entity;

   function Get_Class_Type
     (Class : not null access Ack.Classes.Class_Entity_Record'Class)
      return Type_Entity;

   function Get_Top_Level_Type
     (Name : String)
      return Type_Entity;

private

   type Generic_Argument_Binding is
      record
         Formal : Constant_Type_Entity;
         Actual : Type_Entity;
      end record;

   package List_Of_Generic_Bindings is
     new Ada.Containers.Doubly_Linked_Lists (Generic_Argument_Binding);

   package List_Of_Constraints is
     new Ada.Containers.Doubly_Linked_Lists (Type_Entity);

   type Type_Entity_Record is
     new Root_Entity_Type with
      record
         Class            : Ack.Classes.Class_Entity;
         Generic_Bindings : List_Of_Generic_Bindings.List;
         Constraints      : List_Of_Constraints.List;
         Anchored_Name    : Name_Id := No_Name;
         Anchored_Entity  : Entity_Type;
         Generic_Formal   : Boolean := False;
         Detachable       : Boolean := False;
         Anchored         : Boolean := False;
      end record;

   overriding function Class_Context
     (Typ : not null access constant Type_Entity_Record)
      return Constant_Entity_Type
   is (Constant_Entity_Type (Typ.Class));

   overriding function Instantiate
     (Entity             : not null access Type_Entity_Record;
      Type_Instantiation : not null access
        function (Generic_Type : Entity_Type) return Entity_Type)
      return Entity_Type;

   overriding function Concrete_Entity
     (Entity : not null access Type_Entity_Record)
      return Entity_Type;

   overriding function Conforms_To
     (Conformer : not null access constant Type_Entity_Record;
      Other     : not null access constant Root_Entity_Type'Class)
      return Boolean;

   overriding function Proper_Ancestor_Of
     (Ancestor : not null access constant Type_Entity_Record;
      Other     : not null access constant Root_Entity_Type'Class)
      return Boolean;

   overriding function Description
     (Typ       : Type_Entity_Record)
      return String;

   overriding function Full_Name
     (Typ       : Type_Entity_Record)
      return String;

   overriding function Link_Name
     (Typ       : Type_Entity_Record)
      return String;

   overriding function Detachable
     (Typ : Type_Entity_Record)
      return Boolean
   is (Typ.Detachable);

   overriding function Contains
     (Typ       : Type_Entity_Record;
      Name      : String;
      Recursive : Boolean := True)
      return Boolean;

   overriding function Get
     (Typ  : not null access constant Type_Entity_Record;
      Name : String)
      return Entity_Type;

   overriding procedure Allocate
     (Typ  : Type_Entity_Record;
      Unit : in out Tagatha.Code.Instance'Class);

   overriding procedure Check_Bound
     (Typ : not null access Type_Entity_Record);

   function Class
     (Typ : Type_Entity_Record'Class)
      return Ack.Classes.Class_Entity
   is (if Typ.Generic_Formal then null else Typ.Class);

   overriding function Has_Default_Creation_Routine
     (Typ : Type_Entity_Record)
      return Boolean
   is (Typ.Class.Has_Default_Creation_Routine);

   overriding function Default_Creation_Routine
     (Typ : Type_Entity_Record)
      return Entity_Type
   is (Typ.Class.Default_Creation_Routine);

   function Has_Type_Entity
     (Node : Node_Id)
      return Boolean
   is (Has_Entity (Node)
       and then Get_Entity (Node).all in Type_Entity_Record'Class);

   function Generic_Binding_Count
     (Typ : Type_Entity_Record'Class)
      return Natural
   is (if Typ.Generic_Bindings.Is_Empty
       then Typ.Class.Generic_Formal_Count
       else Natural (Typ.Generic_Bindings.Length));

   function Is_Generic_Formal_Type
     (Typ : Type_Entity_Record'Class)
      return Boolean
   is (Typ.Generic_Formal);

end Ack.Types;
