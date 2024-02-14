with WL.String_Maps;

with Ack.Semantic.Work;

package body Ack.Types is

   package Class_Type_Maps is
     new WL.String_Maps (Type_Entity);

   Class_Type_Map : Class_Type_Maps.Map;

   ---------------------
   -- Aliased_Feature --
   ---------------------

   function Aliased_Feature
     (Typ   : Type_Entity_Record'Class;
      Alias : Name_Id;
      Infix : Boolean)
      return Ack.Features.Feature_Entity
   is
   begin
      return Typ.Class.Aliased_Feature (Alias, Infix);
   end Aliased_Feature;

   --------------
   -- Allocate --
   --------------

   overriding procedure Allocate
     (Typ  : Type_Entity_Record;
      Unit : in out Tagatha.Code.Instance'Class)
   is
   begin
      Typ.Class.Allocate (Unit);
   end Allocate;

   -----------------
   -- Check_Bound --
   -----------------

   overriding procedure Check_Bound
     (Typ : not null access Type_Entity_Record)
   is
   begin
      if not Typ.Generic_Formal then
         Typ.Class.Check_Bound;
      end if;
   end Check_Bound;

   ---------------------
   -- Concrete_Entity --
   ---------------------

   overriding function Concrete_Entity
     (Entity : not null access Type_Entity_Record)
      return Entity_Type
   is
   begin
      return Entity_Type (Entity);
   end Concrete_Entity;

   -----------------
   -- Conforms_To --
   -----------------

   overriding function Conforms_To
     (Conformer : not null access constant Type_Entity_Record;
      Other     : not null access constant Root_Entity_Type'Class)
      return Boolean
   is
   begin
      if Constant_Type_Entity (Conformer) = Constant_Type_Entity (Other) then
         return True;
      elsif Conformer.Generic_Formal then
         if Conformer.Constraints.Is_Empty then
            return True;
         else
            for Constraint of Conformer.Constraints loop
               if Constraint.Conforms_To (Other) then
                  return True;
               end if;
            end loop;
            return False;
         end if;
      else
         return Conformer.Class.Conforms_To (Other);
      end if;
   end Conforms_To;

   --------------
   -- Contains --
   --------------

   overriding function Contains
     (Typ       : Type_Entity_Record;
      Name      : String;
      Recursive : Boolean := True)
      return Boolean
   is
   begin
      return Root_Entity_Type (Typ).Contains (Name, Recursive)
        or else (Recursive and then Typ.Class.Contains (Name));
   end Contains;

   --------------
   -- Deferred --
   --------------

   overriding function Deferred
     (Typ : Type_Entity_Record)
      return Boolean
   is
      use type Ack.Classes.Class_Entity;
   begin
      return Typ.Class /= null
        and then Typ.Class.Deferred;
   end Deferred;

   -----------------
   -- Description --
   -----------------

   overriding function Description
     (Typ       : Type_Entity_Record)
      return String
   is
   begin
      if Typ.Generic_Formal then
         return Typ.Declared_Name & " (a generic argument of "
           & Typ.Class.Description & ")";
      elsif Typ.Anchored then
         return "like " & To_String (Typ.Anchored_Name);
      else
         return Typ.Full_Name & " (" & Typ.Class.Description & ")";
      end if;
   end Description;

   --------------
   -- Expanded --
   --------------

   overriding function Expanded
     (Typ : Type_Entity_Record)
      return Boolean
   is
      use type Ack.Classes.Class_Entity;
   begin
      return Typ.Class /= null
        and then Typ.Class.Expanded;
   end Expanded;

   -------------
   -- Feature --
   -------------

   function Feature
     (Typ   : not null access constant Type_Entity_Record'Class;
      Name  : Name_Id)
      return Ack.Features.Feature_Entity
   is
   begin
      return Ack.Features.Feature_Entity (Typ.Get (Name));
   end Feature;

   ---------------
   -- Full_Name --
   ---------------

   overriding function Full_Name
     (Typ       : Type_Entity_Record)
      return String
   is
      function Generic_Bindings_Image return String;

      ----------------------------
      -- Generic_Bindings_Image --
      ----------------------------

      function Generic_Bindings_Image return String is
         use Ada.Strings.Unbounded;
         Result : Unbounded_String;
      begin
         for Binding of Typ.Generic_Bindings loop
            if Result /= Null_Unbounded_String then
               Result := Result & ",";
            end if;
            Result := Result & Binding.Actual.Qualified_Name;
         end loop;
         return To_String (Result);
      end Generic_Bindings_Image;

      Detachable : constant String :=
                     (if Typ.Detachable
                      then "detachable " else "");

   begin
      if Typ.Generic_Bindings.Is_Empty then
         return Detachable
           & Type_Entity_Record'Class (Typ).Qualified_Name;
      else
         return Detachable
           & Type_Entity_Record'Class (Typ).Qualified_Name
           & "[" & Generic_Bindings_Image & "]";
      end if;
   end Full_Name;

   ---------------------
   -- Generic_Binding --
   ---------------------

   function Generic_Binding
     (Typ   : Type_Entity_Record'Class;
      Index : Positive)
      return Type_Entity
   is
      Count : Natural := 0;
   begin
      if Typ.Generic_Bindings.Is_Empty then
         return Typ.Class.Generic_Formal (Index);
      end if;

      for Binding of Typ.Generic_Bindings loop
         Count := Count + 1;
         if Count = Index then
            return Binding.Actual;
         end if;
      end loop;
      raise Constraint_Error with
        "generic_binding: index"
        & Index'Img & " too large : " & Typ.Description;
   end Generic_Binding;

   ---------
   -- Get --
   ---------

   overriding function Get
     (Typ  : not null access constant Type_Entity_Record;
      Name : String)
      return Entity_Type
   is
   begin
      if Root_Entity_Type (Typ.all).Contains (Name) then
         return Root_Entity_Type (Typ.all).Get (Name);
      else
         declare
            use type Ack.Classes.Constant_Class_Entity;
            Feature        : constant Ack.Features.Feature_Entity :=
                                 Typ.Class.Feature
                                   (Get_Name_Id (Name));
            Ancestor_Type : Ack.Classes.Class_Type_Access :=
                              Ack.Classes.Class_Type_Access (Typ);
            Type_Class : constant Ack.Classes.Constant_Class_Entity :=
                           Ack.Classes.Constant_Class_Entity (Typ.Class);
         begin

            Ack.Semantic.Work.Check_Work_Item
              (Typ.Class,
               Feature.Entity_Name_Id,
               Ack.Semantic.Work.Feature_Header);

            if not Feature.Deferred
              and then Feature.Effective_Class /= Type_Class
            then
               Ancestor_Type :=
                 Get_Ancestor_Type
                   (Typ      => Typ,
                    Ancestor => Feature.Effective_Class);
            end if;

            if Ancestor_Type.Generic_Bindings.Is_Empty then
               Typ.Insert (Feature);
               return Entity_Type (Feature);
            else
               declare
                  function Instantiate_Entity
                    (Entity : Entity_Type)
                     return Entity_Type;

                  ------------------------
                  -- Instantiate_Entity --
                  ------------------------

                  function Instantiate_Entity
                    (Entity : Entity_Type)
                     return Entity_Type
                  is
                  begin
                     for Binding of Ancestor_Type.Generic_Bindings loop
                        if Constant_Type_Entity (Entity) = Binding.Formal then
                           return Entity_Type (Binding.Actual);
                        end if;
                     end loop;

                     return Entity;
                  end Instantiate_Entity;

                  Instantiated_Entity : constant Entity_Type :=
                                          Feature.Instantiate
                                            (Instantiate_Entity'Access);
               begin
                  Typ.Insert (Instantiated_Entity);
                  return Instantiated_Entity;
               end;
            end if;
         end;
      end if;
   end Get;

   -----------------------
   -- Get_Ancestor_Type --
   -----------------------

   function Get_Ancestor_Type
     (Typ      : not null access constant Type_Entity_Record'Class;
      Ancestor : not null access constant
        Ack.Classes.Class_Entity_Record'Class)
      return access constant Type_Entity_Record'Class
   is
      use Ack.Classes;
   begin
      if Constant_Class_Entity (Typ.Class)
        = Constant_Class_Entity (Ancestor)
      then
         return Typ;
      end if;
      return Typ.Class.Get_Ancestor_Type (Typ, Ancestor);
   end Get_Ancestor_Type;

   --------------------
   -- Get_Class_Type --
   --------------------

   function Get_Class_Type
     (Class : not null access Ack.Classes.Class_Entity_Record'Class)
      return Type_Entity
   is
      Key : constant String := Class.Full_Name;
   begin
      if not Class_Type_Map.Contains (Key) then
         Class_Type_Map.Insert
           (Key, New_Class_Type (Class.Declaration_Node,
            Ack.Classes.Class_Entity (Class), False));
      end if;
      return Class_Type_Map.Element (Key);
   end Get_Class_Type;

   -----------------------
   -- Get_Concrete_Type --
   -----------------------

   function Get_Concrete_Type
     (Of_Type : not null access Type_Entity_Record'Class;
      Current : not null access Type_Entity_Record'Class;
      Feature : not null access constant
        Ack.Features.Feature_Entity_Record'Class)
      return Type_Entity
   is
   begin
      if not Of_Type.Anchored then
         return Type_Entity (Of_Type);
      end if;

      declare
         Anchor_Name : constant String :=
                         To_Standard_String (Of_Type.Anchored_Name);
      begin
         if Anchor_Name = "current" then
            return Type_Entity (Current);
         else
            raise Constraint_Error with
            Get_Program (Feature.Declaration_Node).Show_Location
              & ": like " & To_String (Of_Type.Anchored_Name)
              & ": non-Current anchors not implemented";
         end if;
      end;
   end Get_Concrete_Type;

   ------------------------
   -- Get_Top_Level_Type --
   ------------------------

   function Get_Top_Level_Type
     (Name : String)
      return Type_Entity
   is
      use Ack.Classes;
      Class : constant Class_Entity := Get_Top_Level_Class (Name);
   begin
      if Class = null then
         raise Constraint_Error with
           "predefined class '" & Name & "' not found";
      end if;
      return Get_Class_Type (Class);
   end Get_Top_Level_Type;

   ---------------------
   -- Get_Type_Entity --
   ---------------------

   function Get_Type_Entity
     (Node : Node_Id)
      return Type_Entity
   is
   begin
      return Type_Entity (Get_Entity (Node));
   end Get_Type_Entity;

   -------------------------
   -- Has_Aliased_Feature --
   -------------------------

   function Has_Aliased_Feature
     (Typ   : Type_Entity_Record'Class;
      Alias : Name_Id;
      Infix : Boolean)
      return Boolean
   is
   begin
      return Typ.Class.Has_Aliased_Feature (Alias, Infix);
   end Has_Aliased_Feature;

   -----------------
   -- Has_Feature --
   -----------------

   function Has_Feature
     (Typ   : not null access constant Type_Entity_Record'Class;
      Name  : Name_Id)
      return Boolean
   is
   begin
      return Typ.Class.Has_Feature (Name);
   end Has_Feature;

   -----------------
   -- Instantiate --
   -----------------

   overriding function Instantiate
     (Entity             : not null access Type_Entity_Record;
      Type_Instantiation : not null access
        function (Generic_Type : Entity_Type) return Entity_Type)
      return Entity_Type
   is
   begin
      return Result : Entity_Type do
         if Entity.Generic_Formal then
            Result := Type_Instantiation (Entity_Type (Entity));
         elsif Entity.Class.Generic_Formal_Count > 0 then
            declare
               Actuals : Array_Of_Types
                 (1 .. Entity.Class.Generic_Formal_Count);
            begin
               for I in Actuals'Range loop
                  Actuals (I) :=
                    Type_Entity
                      (Entity.Generic_Binding (I)
                       .Instantiate (Type_Instantiation));
               end loop;
               Result :=
                 Entity_Type
                   (Instantiate_Generic_Class
                      (Node            => Entity.Declaration_Node,
                       Generic_Class   => Entity.Class,
                       Generic_Actuals => Actuals,
                       Detachable      => Entity.Detachable));
            end;
         else
            Result := Entity_Type (Entity);
         end if;
      end return;
   end Instantiate;

   ------------------------------
   -- Instantiate_Generic_Type --
   ------------------------------

   function Instantiate_Generic_Class
     (Node            : Node_Id;
      Generic_Class   : Ack.Classes.Class_Entity;
      Generic_Actuals : Array_Of_Types;
      Detachable      : Boolean)
      return Type_Entity
   is
   begin
      return Result : constant Type_Entity := new Type_Entity_Record'
        (Root_Entity_Type with
         Class               => Generic_Class,
         Generic_Bindings    => <>,
         Constraints         => <>,
         Anchored_Name       => No_Name,
         Anchored_Entity     => null,
         Generic_Formal      => False,
         Detachable          => Detachable,
         Anchored            => False)
      do
         Result.Create
           (Get_Name_Id (Generic_Class.Declared_Name), Node, Table => True);
         for I in Generic_Actuals'Range loop
            Result.Generic_Bindings.Append
              (Generic_Argument_Binding'
                 (Formal =>
                      Constant_Type_Entity (Generic_Class.Generic_Formal (I)),
                  Actual => Generic_Actuals (I)));
         end loop;

      end return;

   end Instantiate_Generic_Class;

   ---------------
   -- Link_Name --
   ---------------

   overriding function Link_Name
     (Typ       : Type_Entity_Record)
      return String
   is
   begin
      return Typ.Class.Link_Name;
   end Link_Name;

   function New_Anchored_Type
     (Node       : Node_Id;
      Class      : not null access Ack.Classes.Class_Entity_Record'Class;
      Anchor     : Name_Id)
      return Type_Entity
   is
   begin
      return Result : constant Type_Entity := new Type_Entity_Record'
        (Root_Entity_Type with
           Class               => Ack.Classes.Class_Entity (Class),
         Generic_Bindings    => <>,
         Constraints         => <>,
         Anchored_Name       => Anchor,
         Anchored_Entity     => null,
         Generic_Formal      => False,
         Detachable          => False,
         Anchored            => True)
      do
         Result.Create (Get_Name_Id (Class.Declared_Name), Node,
                        Table => True);
      end return;
   end New_Anchored_Type;

   --------------------
   -- New_Class_Type --
   --------------------

   function New_Class_Type
     (Node       : Node_Id;
      Class      : not null access Ack.Classes.Class_Entity_Record'Class;
      Detachable : Boolean)
      return Type_Entity
   is
   begin
      return Result : constant Type_Entity := new Type_Entity_Record'
        (Root_Entity_Type with
         Class               => Ack.Classes.Class_Entity (Class),
         Generic_Bindings    => <>,
         Constraints         => <>,
         Anchored_Name       => <>,
         Anchored_Entity     => <>,
         Generic_Formal      => False,
         Detachable          => Detachable,
         Anchored            => False)
      do
         Result.Create (Get_Name_Id (Class.Declared_Name), Node,
                        Table => True);
      end return;
   end New_Class_Type;

   -----------------------------
   -- New_Generic_Formal_Type --
   -----------------------------

   function New_Generic_Formal_Type
     (Name          : Name_Id;
      Node          : Node_Id;
      Generic_Class : Ack.Classes.Class_Entity;
      Constraints   : Array_Of_Types := Empty_Type_Array)
      return Type_Entity
   is
   begin
      return Result : constant Type_Entity := new Type_Entity_Record'
        (Root_Entity_Type with
         Class               => null,
         Generic_Bindings    => <>,
         Constraints         => <>,
         Anchored_Name       => <>,
         Anchored_Entity     => <>,
         Generic_Formal      => False,
         Detachable          => False,
         Anchored            => False)
      do
         Result.Create (Name, Node, Table => True);
         Result.Class := Generic_Class;
         Result.Generic_Formal := True;

         for Constraint of Constraints loop
            Result.Constraints.Append (Constraint);
         end loop;

      end return;
   end New_Generic_Formal_Type;

   ------------------------
   -- Proper_Ancestor_Of --
   ------------------------

   overriding function Proper_Ancestor_Of
     (Ancestor  : not null access constant Type_Entity_Record;
      Other     : not null access constant Root_Entity_Type'Class)
      return Boolean
   is
   begin
      if Other.all not in Type_Entity_Record'Class
        or else Ancestor.Qualified_Name = Other.Qualified_Name
      then
         return False;
      end if;

      return Other.Conforms_To (Ancestor);
   end Proper_Ancestor_Of;

   -------------------------------
   -- Update_Type_Instantiation --
   -------------------------------

   function Update_Type_Instantiation
     (Instantiated_Type  : not null access constant Type_Entity_Record'Class;
      Type_With_Bindings : not null access constant
        Type_Entity_Record'Class)
      return Constant_Type_Entity
   is
      Bindings : List_Of_Generic_Bindings.List :=
                   Instantiated_Type.Generic_Bindings;
      Changed  : Boolean := False;
   begin
      for Inner_Binding of Bindings loop
         for Outer_Binding of Type_With_Bindings.Generic_Bindings loop
            if Constant_Type_Entity (Inner_Binding.Actual)
              = Outer_Binding.Formal
            then
               Inner_Binding.Actual := Outer_Binding.Actual;
               Changed := True;
               exit;
            end if;
         end loop;
      end loop;

      if Changed then
         declare
            Result : constant Type_Entity :=
                       new Type_Entity_Record'
                         (Type_Entity_Record (Instantiated_Type.all));
         begin
            Result.Generic_Bindings := Bindings;
            return Constant_Type_Entity (Result);
         end;
      else
         return Constant_Type_Entity (Instantiated_Type);
      end if;
   end Update_Type_Instantiation;

end Ack.Types;
