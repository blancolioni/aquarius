with Ada.Characters.Handling;
with Ada.Containers.Indefinite_Multiway_Trees;
with Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

package body Kosei.Json is

   type Json_Element_Class is
     (Null_Value, Boolean_Value, Integer_Value, Float_Value, String_Value,
      Object_Value, Array_Value);

   type Json_Element_Record (Class : Json_Element_Class) is
      record
         Name : Ada.Strings.Unbounded.Unbounded_String;
         case Class is
            when Null_Value =>
               null;
            when Boolean_Value =>
               Bool_Value : Boolean;
            when Integer_Value =>
               Int_Value  : Integer;
            when Float_Value =>
               Flt_Value  : Float;
            when String_Value =>
               Str_Value : Ada.Strings.Unbounded.Unbounded_String;
            when Object_Value =>
               null;
            when Array_Value =>
               null;
         end case;
      end record;

   package Json_Trees is
     new Ada.Containers.Indefinite_Multiway_Trees
       (Json_Element_Record);

   type Json_Cursor is new Cursor_Interface with
      record
         Position : Json_Trees.Cursor;
      end record;

   overriding function Element
     (Position : Json_Cursor;
      Name     : String)
      return Cursor_Interface'Class;

   overriding function Value
     (Position : Json_Cursor;
      Name     : String)
      return String;

   overriding procedure Iterate_Children
     (This    : Json_Cursor;
      Process : not null access
        procedure (Position : Cursor_Interface'Class));

   type Json_Config is new Configuration_Interface with
      record
         Tree : Json_Trees.Tree;
      end record;

   overriding function Root
     (This : Json_Config)
      return Cursor_Interface'Class
   is (Json_Cursor'(Position => Json_Trees.First_Child (This.Tree.Root)));

   overriding procedure Put (This : Json_Config);

   function Read_Json_File
     (Path : String)
     return Json_Trees.Tree;

   function Find
     (Parent : Json_Trees.Cursor;
      Name   : String)
      return Json_Trees.Cursor;

   function To_String (Rec : Json_Element_Record) return String;

   ---------------------
   -- Add_Json_Config --
   ---------------------

   procedure Add_Json_Config
     (File_Path          : String;
      Configuration_Path : String := "")
   is
      Tree : constant Json_Trees.Tree := Read_Json_File (File_Path);
      Config : constant Json_Config :=
                 Json_Config'
                   (Tree => Tree);
   begin
      Config.Add_Configuration (Configuration_Path);
   end Add_Json_Config;

   -------------
   -- Element --
   -------------

   overriding function Element
     (Position : Json_Cursor;
      Name     : String)
      return Cursor_Interface'Class
   is
   begin
      return Json_Cursor'(Position => Find (Position.Position, Name));
   end Element;

   ----------
   -- Find --
   ----------

   function Find
     (Parent : Json_Trees.Cursor;
      Name   : String)
      return Json_Trees.Cursor
   is
      use Ada.Strings.Unbounded;
      use Json_Trees;
      Child : Cursor := First_Child (Parent);
   begin
      while Has_Element (Child) loop
         if Element (Child).Name = Name then
            return Child;
         end if;
         Next_Sibling (Child);
      end loop;
      return Json_Trees.No_Element;
   end Find;

   ----------------------
   -- Iterate_Children --
   ----------------------

   overriding procedure Iterate_Children
     (This    : Json_Cursor;
      Process : not null access
        procedure (Position : Cursor_Interface'Class))
   is
      procedure Local_Process (Position : Json_Trees.Cursor);

      -------------------
      -- Local_Process --
      -------------------

      procedure Local_Process (Position : Json_Trees.Cursor) is
      begin
         Process (Json_Cursor'(Position => Position));
      end Local_Process;

      Value : constant Json_Element_Record :=
                Json_Trees.Element (This.Position);
   begin

      if Value.Class = Array_Value then
         Json_Trees.Iterate_Children (This.Position, Local_Process'Access);
      end if;
   end Iterate_Children;

   ---------
   -- Put --
   ---------

   overriding procedure Put (This : Json_Config) is
      use Ada.Strings.Unbounded;
      use Ada.Text_IO;

      procedure Put_Position (Position : Json_Trees.Cursor);

      ------------------
      -- Put_Position --
      ------------------

      procedure Put_Position (Position : Json_Trees.Cursor) is
         Start_Col : constant Positive_Count := Col;
         Rec       : constant Json_Element_Record :=
                       Json_Trees.Element (Position);

         First_Child : Boolean := True;

         procedure Put_Child (Position : Json_Trees.Cursor);

         ---------------
         -- Put_Child --
         ---------------

         procedure Put_Child (Position : Json_Trees.Cursor) is
         begin
            if First_Child then
               First_Child := False;
            else
               Put_Line (",");
            end if;
            Set_Col (Start_Col + 2);
            Put_Position (Position);
         end Put_Child;

      begin
         if Rec.Name /= "" then
            Put ('"' & To_String (Rec.Name) & '"' & ": ");
         end if;
         case Rec.Class is
            when Null_Value =>
               Put ("null");
            when Boolean_Value =>
               Put (if Rec.Bool_Value then "true" else "false");
            when Integer_Value =>
               Put
                 (Ada.Strings.Fixed.Trim
                    (Rec.Int_Value'Image, Ada.Strings.Left));
            when Float_Value =>
               Put
                 (Ada.Strings.Fixed.Trim
                    (Rec.Flt_Value'Image, Ada.Strings.Left));
            when String_Value =>
               Put ('"' & To_String (Rec.Str_Value) & '"');
            when Array_Value =>
               Put_Line ("[");
               Set_Col (Start_Col + 2);
               Json_Trees.Iterate_Children
                 (Position, Put_Child'Access);
               New_Line;
               Set_Col (Start_Col + 2);
               Put ("]");
            when Object_Value =>
               Put_Line ("{");
               Set_Col (Start_Col + 2);
               Json_Trees.Iterate_Children
                 (Position, Put_Child'Access);
               New_Line;
               Set_Col (Start_Col + 2);
               Put ("}");
         end case;
      end Put_Position;
   begin
      Put_Position (Json_Trees.First_Child (This.Tree.Root));
   end Put;

   --------------------
   -- Read_Json_File --
   --------------------

   function Read_Json_File
     (Path : String)
      return Json_Trees.Tree
   is
      use Ada.Strings.Unbounded;
      Tree               : Json_Trees.Tree;

      Current_Line       : Ada.Strings.Unbounded.Unbounded_String;
      Current_Line_Index : Natural := 0;
      Current_Last       : Natural := 0;
      Current_Index      : Natural := 0;
      Current_Character  : Character := ' ';
      Done               : Boolean := False;
      File_Name          : constant String :=
                             Ada.Directories.Simple_Name (Path);
      File               : Ada.Text_IO.File_Type;

      procedure Start;
      procedure Finish;

      procedure Next_Character;

      procedure Skip_Whitespace;

      procedure Error (Message : String);

      procedure Parse_Json_Element
        (Name     : String;
         Position : Json_Trees.Cursor);
      procedure Parse_Json_Object
        (Name     : String;
         Position : Json_Trees.Cursor);
      procedure Parse_Json_Array
        (Name     : String;
         Position : Json_Trees.Cursor);

      function Parse_Terminal return String;
      function Parse_Rest_Of_String return String;

      -----------
      -- Error --
      -----------

      procedure Error (Message : String) is
      begin
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            Ada.Strings.Unbounded.To_String (Current_Line));

         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            File_Name
            & ":" & Current_Line_Index'Image
            & ": " & Message);

         raise Constraint_Error with Message;
      end Error;

      ------------
      -- Finish --
      ------------

      procedure Finish is
      begin
         Skip_Whitespace;
         if not Done then
            Error ("extra ignored");
         end if;
         Ada.Text_IO.Close (File);
      end Finish;

      --------------------
      -- Next_Character --
      --------------------

      procedure Next_Character is
      begin
         if Current_Index >= Current_Last then
            begin
               Current_Line_Index := Current_Line_Index + 1;
               if Ada.Text_IO.End_Of_File (File) then
                  Done := True;
                  return;
               end if;

               Current_Line :=
                 Ada.Strings.Unbounded.To_Unbounded_String
                   (Ada.Text_IO.Get_Line (File));
               Current_Last :=
                 Ada.Strings.Unbounded.Length (Current_Line);

            end;
            Current_Index := 0;
            Next_Character;
            return;
         end if;

         Current_Index := Current_Index + 1;
         Current_Character :=
           Ada.Strings.Unbounded.Element (Current_Line, Current_Index);
      end Next_Character;

      ----------------------
      -- Parse_Json_Array --
      ----------------------

      procedure Parse_Json_Array
        (Name     : String;
         Position : Json_Trees.Cursor)
      is
         Child_Pos : Json_Trees.Cursor;
      begin
         Tree.Append_Child
           (Position,
            Json_Element_Record'(Name  => To_Unbounded_String (Name),
                                 Class => Array_Value));
         Child_Pos := Json_Trees.Last_Child (Position);

         while not Done
           and then Current_Character /= ']'
         loop

            Parse_Json_Element ("", Child_Pos);
            Skip_Whitespace;

            if Current_Character = ',' then
               Next_Character;
               Skip_Whitespace;
            elsif Current_Character /= ']' then
               Error ("missing ',' in array");
            end if;

         end loop;

         if Current_Character = ']' then
            Next_Character;
         else
            Error ("missing ']'");
         end if;

      end Parse_Json_Array;

      ------------------------
      -- Parse_Json_Element --
      ------------------------

      procedure Parse_Json_Element
        (Name     : String;
         Position : Json_Trees.Cursor)
      is
      begin
         Skip_Whitespace;

         if Done then
            return;
         end if;

         case Current_Character is
            when '{' =>
               Next_Character;
               Skip_Whitespace;
               Parse_Json_Object (Name, Position);
            when '[' =>
               Next_Character;
               Skip_Whitespace;
               Parse_Json_Array (Name, Position);
            when others =>
               declare
                  Id : constant String := Parse_Terminal;
                  N  : constant Unbounded_String :=
                         To_Unbounded_String (Name);
                  Rec : constant Json_Element_Record :=
                          (if Id = "null" then (Null_Value, N)
                           elsif Id = "true" or else Id = "false"
                           then (Boolean_Value, N, Boolean'Value (Id))
                           elsif (for all Ch of Id => Ch in '0' .. '9')
                           then (Integer_Value, N, Integer'Value (Id))
                           else (String_Value, N, To_Unbounded_String (Id)));

               begin
                  Tree.Append_Child (Position, Rec);
               end;
         end case;
      end Parse_Json_Element;

      -----------------------
      -- Parse_Json_Object --
      -----------------------

      procedure Parse_Json_Object
        (Name     : String;
         Position : Json_Trees.Cursor)
      is
         Child_Pos : Json_Trees.Cursor;
      begin
         Tree.Append_Child
           (Position,
            Json_Element_Record'(Name  => To_Unbounded_String (Name),
                                 Class => Object_Value));
         Child_Pos := Json_Trees.Last_Child (Position);

         while not Done
           and then Current_Character /= '}'
         loop
            case Current_Character is
               when '"' =>
                  declare
                     Id : constant String := Parse_Terminal;
                  begin
                     Skip_Whitespace;
                     if Current_Character = ':' then
                        Next_Character;
                     else
                        Error ("missing value");
                     end if;

                     Parse_Json_Element (Id, Child_Pos);
                  end;
               when others =>
                  Error ("missing field name");
            end case;

            Skip_Whitespace;

            if Current_Character = ',' then
               Next_Character;
            elsif Current_Character = '}' then
               null;
            else
               Error ("missing ','");
            end if;

            Skip_Whitespace;

         end loop;

         if Current_Character = '}' then
            Next_Character;
         else
            Error ("missing close brace");
         end if;

      end Parse_Json_Object;

      --------------------------
      -- Parse_Rest_Of_String --
      --------------------------

      function Parse_Rest_Of_String return String is
         Result : Unbounded_String;

         procedure Save;

         ----------
         -- Save --
         ----------

         procedure Save is
         begin
            Append (Result, Current_Character);
            Next_Character;
         end Save;

      begin
         while not Done and then Current_Character /= '"' loop
            if Current_Character = '\' then
               Next_Character;
            end if;
            Save;
         end loop;

         if Done then
            Error ("missing close quote");
         end if;

         Next_Character;
         return To_String (Result);
      end Parse_Rest_Of_String;

      --------------------
      -- Parse_Terminal --
      --------------------

      function Parse_Terminal return String is
         Result : Unbounded_String;

         procedure Save;

         ----------
         -- Save --
         ----------

         procedure Save is
         begin
            Append (Result, Current_Character);
            Next_Character;
         end Save;

      begin
         if Current_Character = '"' then
            Next_Character;
            return Parse_Rest_Of_String;
         elsif Current_Character in '0' .. '9' | '+' | '-' then
            if Current_Character in '-' | '+' then
               Save;
            end if;

            while not Done
              and then Current_Character in
                '0' .. '9' | '.' | 'e' | 'E' | '+' | '-'
            loop
               Save;
            end loop;

         elsif Ada.Characters.Handling.Is_Letter (Current_Character) then
            while not Done
              and then Ada.Characters.Handling.Is_Alphanumeric
                (Current_Character)
            loop
               Save;
            end loop;
         else
            Error ("expected a name");
            return "";
         end if;

         return To_String (Result);

      end Parse_Terminal;

      ---------------------
      -- Skip_Whitespace --
      ---------------------

      procedure Skip_Whitespace is
      begin
         while not Done
           and then (Ada.Characters.Handling.Is_Space (Current_Character)
                     or else Current_Character = Character'Val (9))
         loop
            Next_Character;
         end loop;
      end Skip_Whitespace;

      -----------
      -- Start --
      -----------

      procedure Start is
      begin
         Ada.Text_IO.Open (File, Ada.Text_IO.In_File, Path);
         Next_Character;
      end Start;

   begin
      Start;
      Parse_Json_Element ("", Tree.Root);
      Tree.Append_Child
        (Json_Trees.First_Child (Tree.Root),
         Json_Element_Record'
           (String_Value, To_Unbounded_String ("path"),
            To_Unbounded_String
              (Ada.Directories.Containing_Directory (Path))));
      Finish;
      return Tree;
   end Read_Json_File;

   ---------------
   -- To_String --
   ---------------

   function To_String (Rec : Json_Element_Record) return String is
   begin
      case Rec.Class is
         when Null_Value =>
            return "null";
         when Boolean_Value =>
            return (if Rec.Bool_Value then "true" else "false");
         when Integer_Value =>
            return Ada.Strings.Fixed.Trim
              (Rec.Int_Value'Image, Ada.Strings.Left);
         when Float_Value =>
            return Ada.Strings.Fixed.Trim
              (Rec.Flt_Value'Image, Ada.Strings.Left);
         when String_Value =>
            return Ada.Strings.Unbounded.To_String (Rec.Str_Value);
         when Array_Value =>
            return "[array]";
         when Object_Value =>
            return "[object]";
      end case;
   end To_String;

   -----------
   -- Value --
   -----------

   overriding function Value
     (Position : Json_Cursor;
      Name     : String)
      return String
   is
      Child : constant Json_Trees.Cursor := Find (Position.Position, Name);
   begin
      if Json_Trees.Has_Element (Child) then
         return To_String (Json_Trees.Element (Child));
      else
         return "";
      end if;
   end Value;

end Kosei.Json;
