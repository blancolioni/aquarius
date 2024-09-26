with Ada.Containers.Indefinite_Vectors;
with Ada.Text_IO;

with Aquarius.Free_Lists;

package body Aquarius.Streams.Files is

   type Line_Type (Length : Natural) is
      record
         Line_Start : Stream_Offset;
         Line       : String (1 .. Length);
      end record;

   package String_Vectors is
     new Ada.Containers.Indefinite_Vectors (Positive, Line_Type);

   type Reader_Instance is new Stream_Reader_Interface with
      record
         Current_Line : Positive := 1;
         Line_First   : Positive := 1;
         Lines        : String_Vectors.Vector;
      end record;

   type Reference is access all Reader_Instance;

   overriding function Offset
     (This : Reader_Instance)
      return Aquarius.Locations.Location_Offset
   is (Aquarius.Locations.Location_Offset
       (This.Lines.Element (This.Current_Line).Line_Start));

   overriding function Line
     (This : Reader_Instance)
      return Aquarius.Locations.Line_Index
   is (Aquarius.Locations.Line_Index (This.Current_Line));

   overriding function Column
     (This : Reader_Instance)
      return Aquarius.Locations.Column_Index
   is (1);

   overriding procedure Close
     (This : not null access Reader_Instance);

   overriding function End_Of_Stream
     (This : Reader_Instance)
      return Boolean;

   overriding procedure Get_Line
     (This        : in out Reader_Instance;
      Line        : out String;
      Last        : out Natural);

   overriding function Current_Offset
     (This : Reader_Instance)
      return Stream_Offset;

   package Free_Readers is
     new Aquarius.Free_Lists (Reader_Instance, Reference);

   -----------
   -- Close --
   -----------

   overriding procedure Close
     (This : not null access Reader_Instance)
   is
   begin
      This.all := (others => <>);
      Free_Readers.Release (Reference (This));
   end Close;

   --------------------
   -- Current_Offset --
   --------------------

   overriding function Current_Offset
     (This : Reader_Instance)
      return Stream_Offset
   is
   begin
      return This.Lines.Element (This.Current_Line).Line_Start
        + Stream_Offset (This.Line_First) - 1;
   end Current_Offset;

   -------------------
   -- End_Of_Stream --
   -------------------

   overriding function End_Of_Stream
     (This : Reader_Instance)
      return Boolean
   is
   begin
      return This.Current_Line > This.Lines.Last_Index;
   end End_Of_Stream;

   --------------
   -- Get_Line --
   --------------

   overriding procedure Get_Line
     (This        : in out Reader_Instance;
      Line        : out String;
      Last        : out Natural)
   is
      Current   : constant String := This.Lines (This.Current_Line).Line;
      Remaining : constant Natural := Current'Last - This.Line_First + 1;
   begin
      if Remaining <= Line'Length then
         Last := Line'First + Remaining - 1;
         Line (Line'First .. Last) := Current;
         This.Line_First := 1;
         This.Current_Line := This.Current_Line + 1;
      else
         Last := Line'Last;
         Line := Current
           (This.Line_First .. This.Line_First + Line'Length - 1);
         This.Line_First := This.Line_First + Line'Length - 1;
      end if;
   end Get_Line;

   -----------------
   -- File_Reader --
   -----------------

   function File_Reader (Path : String) return Reader_Reference is
      use Ada.Text_IO;
      R : constant Reference := Free_Readers.Claim;
      File : File_Type;
      Offset : Stream_Offset := 0;
   begin
      Open (File, In_File, Path);
      while not End_Of_File (File) loop
         declare
            Line : constant String := Get_Line (File);
         begin
            R.Lines.Append (Line_Type'(Line'Length, Offset, Line));
            Offset := Offset + Stream_Offset (Line'Length) + 1;
         end;
      end loop;
      Close (File);
      return Reader_Reference (R);
   end File_Reader;

end Aquarius.Streams.Files;
