with Ada.Exceptions;
with Ada.Text_IO;

package body Aquarius.VM.Maps is

   --------------
   -- Contains --
   --------------

   overriding function Contains
     (Map : Map_Property_Type;
      Key : String)
      return Boolean
   is
   begin
      return Map.Map.Contains
        (Ada.Strings.Unbounded.To_Unbounded_String (Key));
   end Contains;

   -------------
   -- Element --
   -------------

   function Element
     (Property : Map_Property_Type'Class;
      Key      : String)
      return VM_Value
   is
      Position : constant VM_Value_Lists.Cursor :=
                   Property.Map.Element
                     (Ada.Strings.Unbounded.To_Unbounded_String (Key));
   begin
      return VM_Value_Lists.Element (Position);
   end Element;

   ---------
   -- Get --
   ---------

   overriding function Get
     (Map : Map_Property_Type;
      Key : String)
      return VM_Value
   is
   begin
      if Map_Property_Type'Class (Map).Contains (Key) then
         return Map_Property_Type'Class (Map).Element (Key);
      else
         return Null_Value;
      end if;
   end Get;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Property : in out Map_Property_Type'Class;
      Key      : in     String;
      Value    : in     VM_Value)
   is
   begin
      Property.List.Append (Value);
      Property.Map.Insert
        (Ada.Strings.Unbounded.To_Unbounded_String (Key), Property.List.Last);
   exception
      when E : Constraint_Error =>
         Ada.Text_IO.Put_Line
           ("Error inserting [" & Key & "]: "
            & Ada.Exceptions.Exception_Message (E));
         raise;
   end Insert;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (Property : Map_Property_Type'Class;
      Process  : not null access
        procedure (Key : String;
                   Value : VM_Value))
   is
      use VM_Value_Maps;
   begin
      for Position in Property.Map.Iterate loop
         Process (Ada.Strings.Unbounded.To_String (Key (Position)),
                  VM_Value_Lists.Element (Element (Position)));
      end loop;
   end Iterate;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (Property : Map_Property_Type'Class;
      Process  : not null access
        procedure (Value : VM_Value))
   is
   begin
      for V of Property.List loop
         Process (V);
      end loop;
   end Iterate;

   ----------
   -- Name --
   ----------

   overriding function Name
     (Item : Map_Property_Type)
      return String
   is
      pragma Unreferenced (Item);
   begin
      return "value-map";
   end Name;

   -------------
   -- Replace --
   -------------

   procedure Replace
     (Property : in out Map_Property_Type'Class;
      Key      : in     String;
      Value    : in     VM_Value)
   is
      Position : constant VM_Value_Lists.Cursor :=
                   Property.Map.Element
                     (Ada.Strings.Unbounded.To_Unbounded_String (Key));
   begin
      Property.List.Replace_Element (Position, Value);
   end Replace;

   ---------
   -- Set --
   ---------

   overriding procedure Set
     (Map   : in out Map_Property_Type;
      Key   : in     String;
      Value : in     VM_Value)
   is
   begin
      if Map_Property_Type'Class (Map).Contains (Key) then
         Map_Property_Type'Class (Map).Replace (Key, Value);
      else
         Map_Property_Type'Class (Map).Insert (Key, Value);
      end if;
   end Set;

end Aquarius.VM.Maps;
