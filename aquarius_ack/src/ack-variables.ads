package Ack.Variables is

   type Variable_Entity_Record is
     new Root_Entity_Type with private;

   overriding function Deferred
     (Entity : Variable_Entity_Record)
      return Boolean
   is (False);

   overriding function Expanded
     (Entity : Variable_Entity_Record)
      return Boolean
   is (Entity.Get_Type.Expanded);

   procedure Set_Offset
     (Variable : in out Variable_Entity_Record'Class;
      Offset   : Positive);

   type Variable_Entity is access all Variable_Entity_Record'Class;

   function New_Argument_Entity
     (Name          : Name_Id;
      Node          : Node_Id;
      Argument_Type : not null access Root_Entity_Type'Class)
      return Variable_Entity;

   function New_Local_Entity
     (Name       : Name_Id;
      Node       : Node_Id;
      Local_Type : not null access Root_Entity_Type'Class)
      return Variable_Entity;

   function New_Iterator_Entity
     (Name           : Name_Id;
      Node           : Node_Id;
      Iteration_Type : not null access Root_Entity_Type'Class;
      Local_Type     : not null access Root_Entity_Type'Class)
      return Variable_Entity;

   function Is_Variable
     (Entity : not null access Root_Entity_Type'Class)
      return Boolean;

private

   type Variable_Kind is (Argument, Local);

   type Variable_Entity_Record is
     new Root_Entity_Type with
      record
         Kind      : Variable_Kind;
         Offset    : Positive;
         Iterator  : Boolean := False;
         Iteration : Entity_Type;
      end record;

   overriding function Class_Context
     (Variable : not null access constant Variable_Entity_Record)
      return Constant_Entity_Type
   is (raise Constraint_Error
         with "variable " & Variable.Declared_Name
       & " does not have a class context");

   overriding function Instantiate
     (Entity             : not null access Variable_Entity_Record;
      Type_Instantiation : not null access
        function (Generic_Type : Entity_Type) return Entity_Type)
      return Entity_Type;

   overriding function Concrete_Entity
     (Variable : not null access Variable_Entity_Record)
      return Entity_Type
   is (Entity_Type (Variable));

   overriding procedure Push_Entity
     (Variable      : Variable_Entity_Record;
      Have_Current  : Boolean;
      Context       : not null access constant Root_Entity_Type'Class;
      Unit          : in out Tagatha.Code.Instance'Class)
     with Pre => not Have_Current;

   overriding procedure Push_Entity_Address
     (Variable      : Variable_Entity_Record;
      Have_Current  : Boolean;
      Context       : not null access constant Root_Entity_Type'Class;
      Unit          : in out Tagatha.Code.Instance'Class)
     with Pre => not Have_Current;

   overriding procedure Pop_Entity
     (Variable   : Variable_Entity_Record;
      Context    : not null access constant Root_Entity_Type'Class;
      Value_Type : not null access constant Root_Entity_Type'Class;
      Unit       : in out Tagatha.Code.Instance'Class);

   overriding function Can_Update
     (Entity : Variable_Entity_Record)
      return Boolean
   is (Entity.Kind = Local and then not Entity.Iterator);

   function Is_Variable
     (Entity : not null access Root_Entity_Type'Class)
      return Boolean
   is (Entity.all in Variable_Entity_Record'Class);

end Ack.Variables;
