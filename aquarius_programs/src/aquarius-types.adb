with Ada.Strings.Unbounded;
with Ada.Tags;

with Aquarius.Entries;

package body Aquarius.Types is

   type Named_Type_Record is new Aquarius.Types.Root_Aquarius_Type with
      record
         Base_Type : Aquarius.Types.Aquarius_Type;
         Name      : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   overriding
   function Name (Item : Named_Type_Record)
                  return String;

   overriding
   function Create_Derived_Type (Item : Named_Type_Record)
                                 return Aquarius_Type;

   overriding
   function Description (Item : Named_Type_Record)
                         return String;

   overriding
   function Unify (Item      : not null access Named_Type_Record;
                   With_Type : not null access Root_Aquarius_Type'Class)
                   return Aquarius_Type;

   --------------
   -- Add_Type --
   --------------

   procedure Add_Type (To         : in out Possible_Type_Record;
                       Added_Type : Aquarius_Type)
   is
   begin
      if Added_Type = null then
         raise Constraint_Error with "attempting to add a null type";
      end if;
      for I in 1 .. To.Types.Last_Index loop
         if To.Types.Element (I) = Added_Type then
            return;
         end if;
      end loop;
      To.Types.Append (Added_Type);
   end Add_Type;

   -----------
   -- Count --
   -----------

   function Count (Item : Possible_Type_Record)
                  return Natural
   is
   begin
      return Item.Types.Last_Index;
   end Count;

   -------------------------
   -- Create_Derived_Type --
   -------------------------

   overriding
   function Create_Derived_Type (Item : Named_Type_Record)
                                return Aquarius_Type
   is
   begin
      return new Named_Type_Record'(Item);
   end Create_Derived_Type;

   -----------------
   -- Description --
   -----------------

   overriding
   function Description (Item : Named_Type_Record)
                        return String
   is
      Root_Item : Root_Aquarius_Type'Class renames
        Root_Aquarius_Type'Class (Item);
   begin
      return Ada.Tags.External_Tag (Root_Item'Tag);
      --  Root_Aquarius_Type'Class (Item)'Tag);
   end Description;

   -------------------------
   -- Get_Named_Type_Base --
   -------------------------

   function Get_Named_Type_Base
     (Item : not null access Root_Aquarius_Type'Class)
     return Aquarius_Type
   is
   begin
      return Named_Type_Record (Item.all).Base_Type;
   end Get_Named_Type_Base;

   --------------
   -- Get_Type --
   --------------

   function Get_Type (Item : Possible_Type_Record)
                     return Aquarius_Type
   is
   begin
      pragma Assert (Item.Count = 1);
      return Item.Types.Element (1);
   end Get_Type;

   -------------------
   -- Is_Named_Type --
   -------------------

   function Is_Named_Type (Item : not null access Root_Aquarius_Type'Class)
                          return Boolean
   is
   begin
      return Item.all in Named_Type_Record'Class;
   end Is_Named_Type;

   ----------
   -- Name --
   ----------

   overriding
   function Name (Item : Root_Aquarius_Type)
                 return String
   is
   begin
      if not Aquarius.Entries.Is_Null (Item.Type_Entry) then
         return Item.Type_Entry.Name;
      else
         return Root_Aquarius_Type'Class (Item).Description;
      end if;
   end Name;

   ----------
   -- Name --
   ----------

   overriding
   function Name (Item : Named_Type_Record)
                 return String
   is
      use Ada.Strings.Unbounded;
   begin
      return To_String (Item.Name);
   end Name;

   ----------
   -- Name --
   ----------

   overriding
   function Name (Item : Possible_Type_Record)
                 return String
   is
      function Name_Rest (Index : Positive) return String;

      ---------------
      -- Name_Rest --
      ---------------

      function Name_Rest (Index : Positive) return String is
      begin
         if Index > Item.Count then
            return "";
         else
            return "," & Item.Types.Element (Index).Name &
              Name_Rest (Index + 1);
         end if;
      end Name_Rest;

   begin
      if Item.Count = 0 then
         return "nothing";
      elsif Item.Count = 1 then
         return Item.Types.Element (1).Name;
      else
         return "[" & Item.Types.Element (1).Name & Name_Rest (2) & "]";
      end if;
   end Name;

   --------------------
   -- New_Named_Type --
   --------------------

   function New_Named_Type (Name      : String;
                            Base_Type : Aquarius.Types.Aquarius_Type)
                           return Aquarius.Types.Aquarius_Type
   is
      Named_Type : Named_Type_Record;
      Result     : Aquarius.Types.Aquarius_Type;
   begin
      Named_Type.Base_Type := Base_Type;
      Named_Type.Name      :=
        Ada.Strings.Unbounded.To_Unbounded_String (Name);
      Result := new Named_Type_Record'(Named_Type);
      Result.Set_Default_Size (Base_Type.Size_Bits);
      return Result;
   end New_Named_Type;

   ---------------------
   -- Set_Constrained --
   ---------------------

   procedure Set_Constrained
     (Item        : in out Root_Aquarius_Type'Class;
      Constrained : Boolean)
   is
   begin
      Item.Constrained := Constrained;
   end Set_Constrained;

   ----------------------
   -- Set_Default_Size --
   ----------------------

   procedure Set_Default_Size (Item : in out Root_Aquarius_Type;
                               Size : Natural)
   is
   begin
      Item.Size := Size;
   end Set_Default_Size;

   ---------------
   -- Set_Entry --
   ---------------

   procedure Set_Entry
     (Item  : in out Root_Aquarius_Type;
      To    : not null access Aquarius.Entries.Table_Entry_Record'Class)
   is
   begin
      Item.Type_Entry := To;
   end Set_Entry;

   -------------------
   -- Set_Universal --
   -------------------

   procedure Set_Universal (Item : in out Root_Aquarius_Type) is
   begin
      Item.Universal := True;
   end Set_Universal;

   --------------------------
   -- Single_Possible_Type --
   --------------------------

   function Single_Possible_Type (New_Type : Aquarius_Type)
                                 return Possible_Types
   is
      Result : constant Possible_Types := new Possible_Type_Record;
   begin
      Result.Add_Type (New_Type);
      return Result;
   end Single_Possible_Type;

   ---------------
   -- Size_Bits --
   ---------------

   function Size_Bits (Item : Root_Aquarius_Type) return Natural is
   begin
      return Item.Size;
   end Size_Bits;

   -------------------
   -- Transfer_Type --
   -------------------

   procedure Transfer_Type (From : Type_Property_Interface'Class;
                            To   : in out Type_Property_Interface'Class)
   is
   begin
      if From.Has_Type then
         To.Set_Type (From.Get_Type);
      end if;
   end Transfer_Type;

   -------------
   -- Unifies --
   -------------

   function Unifies (Item      : access Possible_Type_Record;
                     Candidate : Aquarius_Type)
                    return Boolean
   is
   begin
      for I in 1 .. Item.Types.Last_Index loop
         if Unify (Item.Types.Element (I), Candidate) /= null then
            return True;
         end if;
      end loop;
      return False;
   end Unifies;

   -----------
   -- Unify --
   -----------

   function Unify (Item      : not null access Root_Aquarius_Type;
                   With_Type : not null access Root_Aquarius_Type'Class)
                  return Aquarius_Type
   is
   begin
      if Aquarius_Type (Item) = Aquarius_Type (With_Type) then
         return Aquarius_Type (Item);
      elsif not Item.Universal and then With_Type.Universal then
         return Unify (With_Type, Aquarius_Type (Item));
      else
         return null;
      end if;
   end Unify;

   -----------
   -- Unify --
   -----------

   overriding
   function Unify (Item      : not null access Named_Type_Record;
                   With_Type : not null access Root_Aquarius_Type'Class)
                  return Aquarius_Type
   is
   begin
      if Aquarius_Type (Item) = Aquarius_Type (With_Type) then
         return Aquarius_Type (Item);
      elsif Unify (Item.Base_Type, With_Type) /= null then
         return Aquarius_Type (Item);
      else
         return null;
      end if;
   end Unify;

   -----------
   -- Unify --
   -----------

   function Unify (Item       : access Possible_Type_Record;
                   Candidates : access Possible_Type_Record'Class)
                  return Possible_Types
   is
      Result : constant Possible_Types := new Possible_Type_Record;
   begin
      for I in 1 .. Item.Types.Last_Index loop
         for J in 1 .. Candidates.Types.Last_Index loop
            declare
               Candidate : constant Aquarius_Type :=
                 Unify (Item.Types.Element (I),
                        Candidates.Types.Element (J));
            begin
               if Candidate /= null then
                  Result.Add_Type (Candidate);
                  exit;
               end if;
            end;
         end loop;
      end loop;
      return Result;
   end Unify;

end Aquarius.Types;
