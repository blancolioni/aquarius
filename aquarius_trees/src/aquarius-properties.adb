package body Aquarius.Properties is

   type Aquarius_Object_Access is
     access all Root_Aquarius_Object'Class;

   package Property_Type_Vector is
      new Ada.Containers.Vectors (Positive, Property_Type);

   type Property_Type_List_Record is
      record
         Properties : Property_Type_Vector.Vector;
      end record;

   Prop_Grammar : constant Property_Type :=
     (Aquarius.Names.To_Aquarius_Name ("properties-grammar"),
      True, True, 1);
   Prop_Syntax : constant Property_Type :=
     (Aquarius.Names.To_Aquarius_Name ("properties-syntax"),
      False, True, 2);
   Prop_Symbol_Table : constant Property_Type :=
     (Aquarius.Names.To_Aquarius_Name ("properties-symbol-table"),
      True, True, 3);
   Prop_Entry : constant Property_Type :=
     (Aquarius.Names.To_Aquarius_Name ("properties-entry"),
      False, True, 4);
   Prop_Type : constant Property_Type :=
     (Aquarius.Names.To_Aquarius_Name ("properties-type"),
      False, True, 5);
   Prop_Type_Error : constant Property_Type :=
     (Aquarius.Names.To_Aquarius_Name ("properties-type-error"),
      False, False, 6);
   Prop_Inferred_Types : constant Property_Type :=
     (Aquarius.Names.To_Aquarius_Name ("properties-inferred-types"),
      False, True, 7);
   Prop_Possible_Types : constant Property_Type :=
     (Aquarius.Names.To_Aquarius_Name ("properties-possible-types"),
      False, True, 8);
   Prop_Pool : constant Property_Type :=
     (Aquarius.Names.To_Aquarius_Name ("properties-property-pool"),
      True, True, 9);
   Prop_Project : constant Property_Type :=
     (Aquarius.Names.To_Aquarius_Name ("properties-project"),
      True, True, 10);
   Prop_UI : constant Property_Type :=
     (Aquarius.Names.To_Aquarius_Name ("properties-UI"),
      True, True, 11);
   Prop_Interactor : constant Property_Type :=
     (Aquarius.Names.To_Aquarius_Name ("properties-UI"),
      True, True, 12);
   Prop_Tagatha : constant Property_Type :=
     (Aquarius.Names.To_Aquarius_Name ("properties-tagatha"),
      True, True, 13);

   -----------
   -- Clear --
   -----------

   procedure Clear (Item  : in out Property_List;
                    Prop  : Property_Type)
   is
   begin
      if not Item.Present (Prop.Index) then
         raise Constraint_Error with
           "attempt to clear " & Get_Name (Prop) &
           " but it is not set";
      end if;

      Item.Present (Prop.Index) := False;
      if Prop.Has_Value then
         for I in 1 .. Item.Properties.Last_Index loop
            if Item.Properties.Element (I).Index = Prop.Index then
               Item.Properties.Delete (I);
               exit;
            end if;
         end loop;
      end if;
   end Clear;

   ---------------------
   -- Create_Property --
   ---------------------

   procedure Create_Property
     (Pool      : in out Property_Pool'Class;
      Prop      : in out Property_Type;
      Name      : String;
      Inherited : Boolean;
      Has_Value : Boolean)
   is
      From_Pool : constant Property_Type_List := Pool.Get_Property_Types;
   begin
      Prop := (Aquarius.Names.To_Aquarius_Name (Name),
               Inherited, Has_Value,
               From_Pool.Properties.Last_Index + 1);
      From_Pool.Properties.Append (Prop);
   end Create_Property;

   ----------------
   -- Empty_Pool --
   ----------------

   function Empty_Pool return Property_Type_List is
      Result : constant Property_Type_List :=
        new Property_Type_List_Record;
   begin
      Result.Properties.Append (Prop_Grammar);
      Result.Properties.Append (Prop_Syntax);
      Result.Properties.Append (Prop_Symbol_Table);
      Result.Properties.Append (Prop_Entry);
      Result.Properties.Append (Prop_Type);
      Result.Properties.Append (Prop_Type_Error);
      Result.Properties.Append (Prop_Inferred_Types);
      Result.Properties.Append (Prop_Possible_Types);
      Result.Properties.Append (Prop_Pool);
      Result.Properties.Append (Prop_Project);
      Result.Properties.Append (Prop_UI);
      Result.Properties.Append (Prop_Tagatha);
      return Result;
   end Empty_Pool;

   --------------------
   -- Entry_Property --
   --------------------

   function Entry_Property
     return Aquarius.Properties.Property_Type
   is
   begin
      return Prop_Entry;
   end Entry_Property;

   ------------
   -- Exists --
   ------------

   function Exists (Item : Property_List;
                    Prop : Property_Type)
                   return Boolean
   is
   begin
      return Item.Present (Prop.Index);
   end Exists;

   ---------
   -- Get --
   ---------

   function Get (Item : Property_List;
                 Prop : Property_Type)
                return access Root_Aquarius_Object'Class
   is
   begin
      if not Prop.Has_Value then
         raise Constraint_Error with
           "attempt to treat flag property '" &
           Get_Name (Prop) & "' as a value";
      end if;
      if Item.Present (Prop.Index) then
         for I in 1 .. Item.Properties.Last_Index loop
            if Item.Properties.Element (I).Index = Prop.Index then
               return Item.Properties.Element (I).Value;
            end if;
         end loop;
         raise Constraint_Error with
           "expected to find a property called " & Get_Name (Prop);
      else
         raise Constraint_Error with
           "attempted to get a non-existent property";
      end if;
   end Get;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (Property : Property_Type) return String is
   begin
      return Aquarius.Names.To_String (Property.Name);
   end Get_Name;

   -----------------------
   -- Get_Property_Type --
   -----------------------

   function Get_Property_Type
     (Pool : Property_Pool'Class;
      Name : String)
     return Property_Type
   is
      use type Aquarius.Names.Aquarius_Name;
      P : constant Property_Type_List := Pool.Get_Property_Types;
   begin
      for I in 1 .. P.Properties.Last_Index loop
         if P.Properties.Element (I).Name = Name then
            return P.Properties.Element (I);
         end if;
      end loop;
      raise Constraint_Error with
        Name & ": no such property";
   end Get_Property_Type;

   ----------------------
   -- Grammar_Property --
   ----------------------

   function Grammar_Property
     return Aquarius.Properties.Property_Type
   is
   begin
      return Prop_Grammar;
   end Grammar_Property;

   ------------------------
   -- Have_Property_Type --
   ------------------------

   function Have_Property_Type
     (Pool : Property_Pool'Class;
      Name : String)
     return Boolean
   is
      use type Aquarius.Names.Aquarius_Name;
      P : constant Property_Type_List := Pool.Get_Property_Types;
   begin
      for I in 1 .. P.Properties.Last_Index loop
         if P.Properties.Element (I).Name = Name then
            return True;
         end if;
      end loop;
      return False;
   end Have_Property_Type;

   -----------------------------
   -- Inferred_Types_Property --
   -----------------------------

   function Inferred_Types_Property
     return Aquarius.Properties.Property_Type
   is
   begin
      return Prop_Inferred_Types;
   end Inferred_Types_Property;

   -------------------------
   -- Interactor_Property --
   -------------------------

   function Interactor_Property
     return Aquarius.Properties.Property_Type
   is
   begin
      return Prop_Interactor;
   end Interactor_Property;

   ------------------
   -- Is_Inherited --
   ------------------

   function Is_Inherited (Property  : Property_Type) return Boolean is
   begin
      return Property.Inherited;
   end Is_Inherited;

   -------------------
   -- Pool_Property --
   -------------------

   function Pool_Property
     return Aquarius.Properties.Property_Type
   is
   begin
      return Prop_Pool;
   end Pool_Property;

   -----------------------------
   -- Possible_Types_Property --
   -----------------------------

   function Possible_Types_Property
     return Aquarius.Properties.Property_Type
   is
   begin
      return Prop_Possible_Types;
   end Possible_Types_Property;

   ----------------------
   -- Project_Property --
   ----------------------

   function Project_Property
     return Aquarius.Properties.Property_Type
   is
   begin
      return Prop_Project;
   end Project_Property;

   ---------
   -- Set --
   ---------

   procedure Set (Item  : in out          Property_List;
                  Prop  : Property_Type;
                  Value : not null access Root_Aquarius_Object'Class)
   is
      Prop_Value : constant Aquarius_Object_Access :=
                     Aquarius_Object_Access (Value);
   begin
      if not Prop.Has_Value then
         raise Constraint_Error with
           "attempt to set value of a flag property " &
           Aquarius.Names.To_String (Prop.Name);
      elsif Item.Present (Prop.Index) then
         for I in 1 .. Item.Properties.Last_Index loop
            if Item.Properties.Element (I).Index = Prop.Index then
               Item.Properties.Replace_Element (I, (Prop.Index, Value));
               return;
            end if;
         end loop;
         raise Constraint_Error with
           "expected to find a property called " & Get_Name (Prop);
      else
         Item.Present (Prop.Index) := True;
         Item.Properties.Append (Property_Value'(Prop.Index, Prop_Value));
      end if;
   end Set;

   ---------
   -- Set --
   ---------

   procedure Set (Item  : in out Property_List;
                  Prop  : Property_Type)
   is
   begin
      if Prop.Has_Value then
         raise Constraint_Error with
           "attempt to treat value property '" &
           Get_Name (Prop) & "' as a flag";
      elsif Item.Present (Prop.Index) then
         raise Constraint_Error with
           "property " & Get_Name (Prop) & " is already set";
      else
         Item.Present (Prop.Index) := True;
      end if;
   end Set;

   -------------------------
   -- Show_Set_Properties --
   -------------------------

   function Show_Set_Properties (List : Property_List) return String is
      Result : String (1 .. Max_Property_Types) := (others => '0');
   begin
      for I in Result'Range loop
         if List.Present (I) then
            Result (I) := '1';
         end if;
      end loop;
      return Result;
   end Show_Set_Properties;

   ---------------------------
   -- Symbol_Table_Property --
   ---------------------------

   function Symbol_Table_Property
     return Aquarius.Properties.Property_Type
   is
   begin
      return Prop_Symbol_Table;
   end Symbol_Table_Property;

   ---------------------
   -- Syntax_Property --
   ---------------------

   function Syntax_Property
     return Aquarius.Properties.Property_Type
   is
   begin
      return Prop_Syntax;
   end Syntax_Property;

   ----------------------
   -- Tagatha_Property --
   ----------------------

   function Tagatha_Property
     return Aquarius.Properties.Property_Type
   is
   begin
      return Prop_Tagatha;
   end Tagatha_Property;

   -------------------------
   -- Type_Error_Property --
   -------------------------

   function Type_Error_Property
     return Aquarius.Properties.Property_Type
   is
   begin
      return Prop_Type_Error;
   end Type_Error_Property;

   -------------------
   -- Type_Property --
   -------------------

   function Type_Property
     return Aquarius.Properties.Property_Type
   is
   begin
      return Prop_Type;
   end Type_Property;

   -----------------
   -- UI_Property --
   -----------------

   function UI_Property
     return Aquarius.Properties.Property_Type
   is
   begin
      return Prop_UI;
   end UI_Property;

end Aquarius.Properties;
