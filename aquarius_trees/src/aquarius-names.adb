with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Fixed.Hash;

package body Aquarius.Names is

   package Name_Vectors is
     new Ada.Containers.Indefinite_Vectors (Real_Aquarius_Name, String);

   package Name_Maps is
     new Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => String,
        Element_Type    => Real_Aquarius_Name,
        Hash            => Ada.Strings.Fixed.Hash,
        Equivalent_Keys => "=");

   protected Aquarius_Name_Container is
      function To_String (Name : Aquarius_Name) return String;
      function Get_Allocated_Name_Count return Natural;
      procedure Get_Aquarius_Name (Text : String;
                                   Name : out Aquarius_Name);
   private
      Vector : Name_Vectors.Vector;
      Map    : Name_Maps.Map;
   end Aquarius_Name_Container;

   ---------
   -- "=" --
   ---------

   function "="
     (Left  : String;
      Right : Aquarius_Name)
      return Boolean
   is
   begin
      return Left = To_String (Right);
   end "=";

   ---------
   -- "=" --
   ---------

   function "=" (Left  : Aquarius_Name;
                 Right : String)
                 return Boolean
   is
   begin
      return To_String (Left) = Right;
   end "=";

   --------------------------
   -- Allocated_Name_Count --
   --------------------------

   function Allocated_Name_Count
     return Natural
   is
   begin
      return Aquarius_Name_Container.Get_Allocated_Name_Count;
   end Allocated_Name_Count;

   -----------------------------
   -- Aquarius_Name_Container --
   -----------------------------

   protected body Aquarius_Name_Container is

      ------------------------------
      -- Get_Allocated_Name_Count --
      ------------------------------

      function Get_Allocated_Name_Count return Natural is
      begin
         return Natural (Vector.Length);
      end Get_Allocated_Name_Count;

      -----------------------
      -- Get_Aquarius_Name --
      -----------------------

      procedure Get_Aquarius_Name
        (Text : String;
         Name : out Aquarius_Name)
      is
      begin
         if Map.Contains (Text) then
            Name := Map (Text);
         else
            Vector.Append (Text);
            Map.Insert (Text, Vector.Last_Index);
            Name := Vector.Last_Index;
         end if;
      end Get_Aquarius_Name;

      ---------------
      -- To_String --
      ---------------

      function To_String (Name : Aquarius_Name) return String is
      begin
         return Vector (Name);
      end To_String;

   end Aquarius_Name_Container;

   ----------
   -- Name --
   ----------

   overriding function Name
     (Item : Aquarius_Name_Value)
      return String
   is
   begin
      return To_String (Item.Value);
   end Name;

   ----------------
   -- Name_Value --
   ----------------

   function Name_Value
     (Name : String)
      return Aquarius_Name_Value_Access
   is
   begin
      return new Aquarius_Name_Value'(Value => To_Aquarius_Name (Name));
   end Name_Value;

   ----------------------
   -- To_Aquarius_Name --
   ----------------------

   function To_Aquarius_Name (Item : String) return Aquarius_Name is
   begin
      return Result : Aquarius_Name do
         Aquarius_Name_Container.Get_Aquarius_Name (Item, Result);
      end return;
   end To_Aquarius_Name;

   ---------------
   -- To_String --
   ---------------

   function To_String
     (Item : Aquarius_Name)
      return String
   is
   begin
      if Item = 0 then
         return "";
      else
         return Aquarius_Name_Container.To_String (Item);
      end if;
   end To_String;

end Aquarius.Names;
