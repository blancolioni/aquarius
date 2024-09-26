with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Unbounded;            use Ada.Strings.Unbounded;

package body Aquarius.Streams.Strings is

   type Reader_Instance is new Stream_Reader_Interface with
      record
         Index : Natural := 1;
         S     : Unbounded_String := Null_Unbounded_String;
      end record;

   type Reader_Instance_Reference is access all Reader_Instance'Class;

   overriding function Offset
     (This : Reader_Instance)
      return Aquarius.Locations.Location_Offset
   is (Aquarius.Locations.Location_Offset (This.Index));

   overriding function Line
     (This : Reader_Instance)
      return Aquarius.Locations.Line_Index
   is (1);

   overriding function Column
     (This : Reader_Instance)
      return Aquarius.Locations.Column_Index
   is (Aquarius.Locations.Column_Index (This.Index));

   overriding procedure Close (This : not null access Reader_Instance);

   overriding function Current_Offset
     (This : Reader_Instance)
      return Stream_Offset
   is (Stream_Offset (This.Index - 1));

   overriding function End_Of_Stream
     (This : Reader_Instance)
      return Boolean;

   overriding procedure Get_Line
     (This        : in out Reader_Instance;
      Line        : out String;
      Last        : out Natural);

   type Writer_Instance is new Stream_Writer_Interface with
      record
         S : Unbounded_String;
      end record;

   type Writer_Instance_Reference is access all Writer_Instance'Class;

   overriding procedure Close (This : not null access Writer_Instance);

   overriding function To_String (This : Writer_Instance) return String;

   overriding procedure Put
     (This : in out Writer_Instance;
      Text : String);

   overriding procedure New_Line
     (This : in out Writer_Instance);

   package Reader_Reference_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Reader_Instance_Reference);

   Free_Readers : Reader_Reference_Lists.List;

   package Writer_Reference_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Writer_Instance_Reference);

   Free_Writers : Writer_Reference_Lists.List;

   -----------
   -- Close --
   -----------

   overriding procedure Close (This : not null access Reader_Instance) is
   begin
      This.all := (others => <>);
      Free_Readers.Append (Reader_Instance_Reference (This));
   end Close;

   -----------
   -- Close --
   -----------

   overriding procedure Close (This : not null access Writer_Instance) is
   begin
      This.S := Null_Unbounded_String;
      Free_Writers.Append (Writer_Instance_Reference (This));
   end Close;

   -------------------
   -- End_Of_Stream --
   -------------------

   overriding function End_Of_Stream
     (This : Reader_Instance)
      return Boolean
   is
   begin
      return This.Index > Length (This.S);
   end End_Of_Stream;

   --------------
   -- Get_Line --
   --------------

   overriding procedure Get_Line
     (This        : in out Reader_Instance;
      Line        : out String;
      Last        : out Natural)
   is
      NL : constant String := [1 => Character'Val (10)];
      Line_Last  : Natural :=
                     Index (This.S, NL, This.Index);
      Last_Index : Natural;
   begin
      if Line_Last = 0 then
         Line_Last := Length (This.S) + 1;
      end if;
      Last := Natural'Min (Line_Last - This.Index, Line'Last);
      Last_Index := This.Index + Last - Line'First;
      Line (Line'First .. Last) := Slice (This.S, This.Index, Last_Index);
      This.Index := Line_Last + 1;
   end Get_Line;

   --------------
   -- New_Line --
   --------------

   overriding procedure New_Line
     (This : in out Writer_Instance)
   is
   begin
      Append (This.S, Character'Val (10));
   end New_Line;

   ---------
   -- Put --
   ---------

   overriding procedure Put
     (This : in out Writer_Instance;
      Text : String)
   is
   begin
      Append (This.S, Text);
   end Put;

   -------------------
   -- String_Reader --
   -------------------

   function String_Reader (S : String) return Reader_Reference is
   begin
      if Free_Readers.Is_Empty then
         return new Reader_Instance'(Index => 1, S => To_Unbounded_String (S));
      else
         declare
            R : constant Reader_Instance_Reference :=
                  Free_Readers.First_Element;
         begin
            R.S := To_Unbounded_String (S);
            Free_Readers.Delete_First;
            return Reader_Reference (R);
         end;
      end if;
   end String_Reader;

   -------------------
   -- String_Writer --
   -------------------

   function String_Writer return Writer_Reference is
   begin
      if Free_Writers.Is_Empty then
         return new Writer_Instance;
      else
         return S : constant Writer_Reference :=
           Writer_Reference (Free_Writers.First_Element)
         do
            Free_Writers.Delete_First;
         end return;
      end if;
   end String_Writer;

   ---------------
   -- To_String --
   ---------------

   overriding function To_String (This : Writer_Instance) return String is
   begin
      return To_String (This.S);
   end To_String;

end Aquarius.Streams.Strings;
