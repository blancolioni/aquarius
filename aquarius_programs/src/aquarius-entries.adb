package body Aquarius.Entries is

   function Search_General (Cursor : Hash_Map.Cursor;
                            Constraint : Entry_Constraint'Class)
                           return Array_Of_Entries;

   function Select_Matches (V : Table_Entry_Vector.Vector;
                            Constraint : Entry_Constraint'Class)
                           return Array_Of_Entries;

   -----------------------------
   -- Create_Class_Constraint --
   -----------------------------

   function Create_Class_Constraint (Class : Ada.Tags.Tag)
                                    return Entry_Constraint'Class
   is
      Result : Class_Constraint;
   begin
      Initialise_Constraint (Result, Class);
      return Entry_Constraint'Class (Result);
   end Create_Class_Constraint;

   ------------------
   -- Create_Entry --
   ------------------

   procedure Create_Entry
     (Item        : in out Table_Entry_Record;
      Name        : String;
      Declaration : access Aquarius.Trees.Root_Tree_Type'Class)
   is
   begin
      Item.Name           := Ada.Strings.Unbounded.To_Unbounded_String (Name);
      Item.Declaration    := Aquarius.Trees.Tree (Declaration);
      Item.Implementation := Item.Declaration;
      Item.Owner          := null;
      Item.Complete       := True;
   end Create_Entry;

   -----------------------------------------
   -- Create_Named_Proposition_Constraint --
   -----------------------------------------

   function Create_Named_Proposition_Constraint
     (Name        : String;
      Proposition : Proposition_Evaluator)
     return Entry_Constraint'Class
   is
      Result : Named_Proposition_Constraint;
   begin
      Initialise_Constraint (Result, Name);
      Result.Proposition := Proposition;
      return Result;
   end Create_Named_Proposition_Constraint;

   -----------------------------------
   -- Create_Proposition_Constraint --
   -----------------------------------

   function Create_Proposition_Constraint
     (Proposition   : Proposition_Evaluator;
      Search_Parent : Boolean := True)
     return Entry_Constraint'Class
   is
      Result : Proposition_Constraint;
   begin
      Result.Search_Parent := Search_Parent;
      Result.Proposition   := Proposition;
      return Result;
   end Create_Proposition_Constraint;

   -----------------
   -- Declaration --
   -----------------

   function Declaration (Item : access Table_Entry_Record)
                        return Aquarius.Trees.Tree
   is
   begin
      return Item.Declaration;
   end Declaration;

   ------------------
   -- Display_Name --
   ------------------

   function Display_Name (Item : access Table_Entry_Record) return String is
      use Ada.Strings.Unbounded;
   begin
      if Item.Display_Name = Null_Unbounded_String then
         return To_String (Item.Name);
      else
         return To_String (Item.Display_Name);
      end if;
   end Display_Name;

   -----------------
   -- Entry_Owner --
   -----------------

   function Entry_Owner (Item : access Table_Entry_Record)
                        return Table_Entry
   is
   begin
      return Item.Entry_Owner;
   end Entry_Owner;

   ------------
   -- Exists --
   ------------

   function Exists (Table : access Symbol_Table_Record;
                    Name  : String)
                   return Boolean
   is
      use Ada.Strings.Unbounded;
      use Hash_Map;
      C : constant Cursor := Find (Table.Map,
                                   To_Unbounded_String (Name));
   begin
      return Has_Element (C) or else
        (Table.Parent /= null and then Exists (Table.Parent, Name));
   end Exists;

   ------------------
   -- Get_Contents --
   ------------------

   function Get_Contents (Table : access Symbol_Table_Record)
                         return Array_Of_Entries
   is
      function Get (Cursor : Hash_Map.Cursor) return Array_Of_Entries;

      ---------
      -- Get --
      ---------

      function Get (Cursor : Hash_Map.Cursor) return Array_Of_Entries is
      begin
         if not Hash_Map.Has_Element (Cursor) then
            declare
               Result : Array_Of_Entries (1 .. 0);
            begin
               return Result;
            end;
         else
            declare
               V      : constant Table_Entry_Vector.Vector :=
                 Hash_Map.Element (Cursor);
               Result : Array_Of_Entries (1 .. V.Last_Index);
            begin
               for I in Result'Range loop
                  Result (I) := V.Element (I);
               end loop;
               return Result & Get (Hash_Map.Next (Cursor));
            end;
         end if;
      end Get;

   begin
      return Get (Table.Map.First);
   end Get_Contents;

   --------------------
   -- Implementation --
   --------------------

   function Implementation (Item : access Table_Entry_Record)
                            return Aquarius.Trees.Tree
   is
   begin
      return Item.Implementation;
   end Implementation;

   ----------------
   -- Incomplete --
   ----------------

   function Incomplete (Item : access Table_Entry_Record) return Boolean is
   begin
      return not Item.Complete;
   end Incomplete;

   ---------------------------
   -- Initialise_Constraint --
   ---------------------------

   procedure Initialise_Constraint (Constraint : in out Name_Constraint;
                                    Name       : String)
   is
   begin
      Constraint.Name := Ada.Strings.Unbounded.To_Unbounded_String (Name);
   end Initialise_Constraint;

   ---------------------------
   -- Initialise_Constraint --
   ---------------------------

   procedure Initialise_Constraint (Constraint : in out Class_Constraint;
                                    Class      : Ada.Tags.Tag)
   is
   begin
      Constraint.Class := Class;
   end Initialise_Constraint;

   ------------
   -- Insert --
   ------------

   procedure Insert (Table     : access Symbol_Table_Record;
                     Item      : Table_Entry)
   is
   begin

      if Table.Map.Contains (Item.Name) then
         declare
            V : Table_Entry_Vector.Vector := Table.Map.Element (Item.Name);
         begin
            V.Append (Item);
            Table.Map.Replace (Item.Name, V);
         end;
      else
         Table.Map.Insert (Item.Name, Table_Entry_Vector.To_Vector (Item, 1));
      end if;
      if Item.Owner = null then
         Item.Owner := Symbol_Table (Table);
      end if;
   end Insert;

   ------------------
   -- Insert_Table --
   ------------------

   procedure Insert_Table (Child_Table : Symbol_Table;
                           Into_Table  : Symbol_Table)
   is
      procedure Insert_Vector (Position : Hash_Map.Cursor);
      procedure Insert_Item (Position : Table_Entry_Vector.Cursor);

      -----------------
      -- Insert_Item --
      -----------------

      procedure Insert_Item (Position : Table_Entry_Vector.Cursor) is
      begin
         Into_Table.Insert (Table_Entry_Vector.Element (Position));
      end Insert_Item;

      -------------------
      -- Insert_Vector --
      -------------------

      procedure Insert_Vector (Position : Hash_Map.Cursor) is
      begin
         Hash_Map.Element (Position).Iterate (Insert_Item'Access);
      end Insert_Vector;

   begin
      Child_Table.Map.Iterate (Insert_Vector'Access);
   end Insert_Table;

   -------------
   -- Is_Null --
   -------------

   function Is_Null (Item : access Table_Entry_Record'Class) return Boolean is
   begin
      return Item = null;
   end Is_Null;

   -----------
   -- Match --
   -----------

   overriding
   function Match (Constraint : Name_Constraint;
                   Item       : access Table_Entry_Record'Class)
                  return Boolean
   is
      use type Ada.Strings.Unbounded.Unbounded_String;
   begin
      return Constraint.Name = Item.Name;
   end Match;

   -----------
   -- Match --
   -----------

   overriding
   function Match (Constraint : Class_Constraint;
                   Item       : access Table_Entry_Record'Class)
                  return Boolean
   is
      use Ada.Tags;
   begin
      return Item.all'Tag = Constraint.Class;
   end Match;

   -----------
   -- Match --
   -----------

   overriding
   function Match (Constraint : Named_Proposition_Constraint;
                   Item       : access Table_Entry_Record'Class)
                  return Boolean
   is
   begin
      return Match (Name_Constraint (Constraint), Item) and then
        Constraint.Proposition (Item);
   end Match;

   -----------
   -- Match --
   -----------

   overriding
   function Match (Constraint : Proposition_Constraint;
                   Item       : access Table_Entry_Record'Class)
                  return Boolean
   is
   begin
      return Constraint.Proposition (Item);
   end Match;

   ----------
   -- Name --
   ----------

   overriding
   function Name (Item : Table_Entry_Record)
                 return String
   is
   begin
      return Ada.Strings.Unbounded.To_String (Item.Name);
   end Name;

   ----------
   -- Name --
   ----------

   overriding
   function Name
     (Item : Symbol_Table_Record)
     return String
   is
   begin
      return Ada.Strings.Unbounded.To_String (Item.Table_Name);
   end Name;

   ----------------------
   -- New_Symbol_Table --
   ----------------------

   function New_Symbol_Table (Name     : String;
                              Parent   : Symbol_Table := No_Symbol_Table)
                             return Symbol_Table
   is
      Result : constant Symbol_Table := new Symbol_Table_Record;
   begin
      Result.Table_Name := Ada.Strings.Unbounded.To_Unbounded_String (Name);
      Result.Parent     := Parent;
      return Result;
   end New_Symbol_Table;

   -----------
   -- Owner --
   -----------

   function Owner (Item : access Table_Entry_Record) return Symbol_Table is
   begin
      return Item.Owner;
   end Owner;

   ------------
   -- Remove --
   ------------

   procedure Remove (Table     : access Symbol_Table_Record;
                     Item      : access Table_Entry_Record'Class)
   is
      V : Table_Entry_Vector.Vector := Table.Map.Element (Item.Name);
   begin
      for I in 1 .. V.Last_Index loop
         if V.Element (I) = Table_Entry (Item) then
            V.Delete (I);
            return;
         end if;
      end loop;
      raise Constraint_Error with
        "attempt to remove non-existent element '" & Name (Item.all) & "'";
   end Remove;

   ------------
   -- Rename --
   ------------

   procedure Rename (Item     : access Table_Entry_Record;
                     New_Name : String)
   is
   begin
      if New_Name /= Name (Item.all) then
         Item.Owner.Remove (Item);
         Item.Name := Ada.Strings.Unbounded.To_Unbounded_String (New_Name);
         Item.Owner.Insert (Table_Entry (Item));
      end if;
   end Rename;

   --------------
   -- Retrieve --
   --------------

   function Retrieve (Table : access Symbol_Table_Record;
                      Name  : String)
                     return Table_Entry
   is
      --  we specialise this for speed
      use Ada.Strings.Unbounded;
      use Hash_Map;
      C : constant Cursor := Find (Table.Map,
                                   To_Unbounded_String (Name));
   begin
      if C = No_Element then
         if Table.Parent /= null then
            return Retrieve (Table.Parent, Name);
         else
            return null;
         end if;
      else
         declare
            use Ada.Containers;
            Matches : constant Table_Entry_Vector.Vector :=
              Element (C);
         begin
            if Matches.Length > 1 then
               raise Constraint_Error with
                 "unexpected multiple matches for '" & Name & "' in table " &
                 Table.Name;
            else
               return Matches.Element (1);
            end if;
         end;
      end if;
   end Retrieve;

   ------------
   -- Search --
   ------------

   function Search (Table : access Symbol_Table_Record;
                    Name  : String)
                   return Array_Of_Entries
   is
      --  we specialise this for speed
      use Ada.Strings.Unbounded;
      use Hash_Map;
      C : constant Cursor := Find (Table.Map,
                                   To_Unbounded_String (Name));
      No_Result : Array_Of_Entries (1 .. 0);
   begin
      if C = No_Element then
         if Table.Parent = null then
            return No_Result;
         else
            return Table.Parent.Search (Name);
         end if;
      else
         declare
            Matches : constant Table_Entry_Vector.Vector :=
              Element (C);
            Result  : Array_Of_Entries (1 .. Matches.Last_Index);
         begin
            for I in Result'Range loop
               Result (I) := Matches.Element (I);
            end loop;
            if Table.Parent = null then
               return Result;
            else
               return Result & Table.Parent.Search (Name);
            end if;
         end;
      end if;
   end Search;

   ------------
   -- Search --
   ------------

   function Search (Table      : access Symbol_Table_Record;
                    Constraint : Entry_Constraint'Class)
                   return Array_Of_Entries
   is
      No_Result : Array_Of_Entries (1 .. 0);
   begin
      if Constraint in Name_Constraint'Class then
         declare
            use Hash_Map;
            C : constant Cursor := Find (Table.Map,
                                         Name_Constraint (Constraint).Name);
         begin
            if C = No_Element then
               if Constraint.Search_Parent and then Table.Parent /= null then
                  return Table.Parent.Search (Constraint);
               else
                  return No_Result;
               end if;
            else
               declare
                  Matches : constant Array_Of_Entries :=
                    Select_Matches (Element (C), Constraint);
               begin
                  if Constraint.Search_Parent
                    and then Table.Parent /= null
                  then
                     return Matches & Search (Table.Parent, Constraint);
                  else
                     return Matches;
                  end if;
               end;
            end if;
         end;
      else
         return Search_General (Hash_Map.First (Table.Map), Constraint);
      end if;
   end Search;

   --------------------
   -- Search_General --
   --------------------

   function Search_General (Cursor     : Hash_Map.Cursor;
                            Constraint : Entry_Constraint'Class)
                           return Array_Of_Entries
   is
   begin
      if not Hash_Map.Has_Element (Cursor) then
         declare
            Empty_Result : Array_Of_Entries (1 .. 0);
         begin
            return Empty_Result;
         end;
      else
         declare
            Partial : constant Array_Of_Entries :=
              Select_Matches (Hash_Map.Element (Cursor), Constraint);
         begin
            return Partial & Search_General (Hash_Map.Next (Cursor),
                                             Constraint);
         end;
      end if;
   end Search_General;

   --------------------
   -- Select_Matches --
   --------------------

   function Select_Matches (V : Table_Entry_Vector.Vector;
                            Constraint : Entry_Constraint'Class)
                           return Array_Of_Entries
   is
      Count  : Natural := 0;
      Result : Array_Of_Entries (1 .. V.Last_Index);
   begin
      for I in 1 .. V.Last_Index loop
         if Match (Constraint, V.Element (I)) then
            Count := Count + 1;
            Result (Count) := V.Element (I);
         end if;
      end loop;
      return Result (1 .. Count);
   end Select_Matches;

   ------------------
   -- Set_Complete --
   ------------------

   procedure Set_Complete (Item     : access Table_Entry_Record;
                           Complete : Boolean)
   is
   begin
      Item.Complete := Complete;
   end Set_Complete;

   ---------------------
   -- Set_Declaration --
   ---------------------

   procedure Set_Declaration
     (Item        : access Table_Entry_Record;
      Declaration : access Aquarius.Trees.Root_Tree_Type'Class)
   is
   begin
      Item.Declaration := Aquarius.Trees.Tree (Declaration);
      Item.Implementation := Item.Declaration;
   end Set_Declaration;

   ----------------------
   -- Set_Display_Name --
   ----------------------

   procedure Set_Display_Name (Item      : access Table_Entry_Record;
                            Display_Name : String)
   is
      use Ada.Strings.Unbounded;
   begin
      Item.Display_Name := To_Unbounded_String (Display_Name);
   end Set_Display_Name;

   ------------------------
   -- Set_Implementation --
   ------------------------

   procedure Set_Implementation
     (Item           : access Table_Entry_Record;
      Implementation : access Aquarius.Trees.Root_Tree_Type'Class)
   is
   begin
      Item.Implementation := Aquarius.Trees.Tree (Implementation);
   end Set_Implementation;

   --------------------
   -- Set_Value_Size --
   --------------------

   procedure Set_Value_Size (Table : access Symbol_Table_Record;
                             Size  : Natural)
   is
   begin
      Table.Value_Size := Size;
   end Set_Value_Size;

   --------------------
   -- Transfer_Entry --
   --------------------

   procedure Transfer_Entry (From : Entry_Property_Interface'Class;
                             To   : in out Entry_Property_Interface'Class)
   is
   begin
      if From.Has_Entry then
         To.Set_Entry (From.Get_Entry);
      end if;
   end Transfer_Entry;

   ----------------
   -- Value_Size --
   ----------------

   function Value_Size (Table : access Symbol_Table_Record)
                       return Natural
   is
   begin
      return Table.Value_Size;
   end Value_Size;

end Aquarius.Entries;
