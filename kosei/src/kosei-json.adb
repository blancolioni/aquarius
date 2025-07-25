with Ada.Characters.Handling;
with Ada.Containers.Indefinite_Holders;
with Ada.Directories;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with WL.Json;

package body Kosei.Json is

   package Json_Holders is
     new Ada.Containers.Indefinite_Holders
       (WL.Json.Json_Value'Class, WL.Json."=");

   type Json_Cursor is new Cursor_Interface with
      record
         Top : Json_Holders.Holder;
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
         Root : Json_Cursor;
      end record;

   overriding function Root
     (This : Json_Config)
      return Cursor_Interface'Class
   is (This.Root);

   function Read_Json_File
     (Path : String)
     return WL.Json.Json_Value'Class;

   ---------------------
   -- Add_Json_Config --
   ---------------------

   procedure Add_Json_Config
     (File_Path          : String;
      Configuration_Path : String := "")
   is
      Root_Value : constant WL.Json.Json_Value'Class :=
                     Read_Json_File (File_Path);
      Root       : constant Json_Cursor :=
                     Json_Cursor'
                       (Top => Json_Holders.To_Holder (Root_Value));
      Config : constant Json_Config :=
                 Json_Config'
                   (Root => Root);
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
      Result : constant WL.Json.Json_Value'Class :=
                 Position.Top.Element.Get_Property (Name);
   begin
      return Json_Cursor'
        (Top => Json_Holders.To_Holder (Result));
   end Element;

   ----------------------
   -- Iterate_Children --
   ----------------------

   overriding procedure Iterate_Children
     (This    : Json_Cursor;
      Process : not null access
        procedure (Position : Cursor_Interface'Class))
   is
      Value : constant WL.Json.Json_Value'Class :=
                This.Top.Element;
   begin
      if Value in WL.Json.Json_Array'Class then
         declare
            Arr : WL.Json.Json_Array'Class
            renames WL.Json.Json_Array'Class (Value);
         begin
            for I in 1 .. Arr.Length loop
               Process (Json_Cursor'
                          (Top => Json_Holders.To_Holder (Arr.Element (I))));
            end loop;
         end;
      end if;
   end Iterate_Children;

   --------------------
   -- Read_Json_File --
   --------------------

   function Read_Json_File
     (Path : String)
      return WL.Json.Json_Value'Class
   is
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

      function Parse_Json_Element return WL.Json.Json_Value'Class;
      function Parse_Json_Object return WL.Json.Json_Value'Class;
      function Parse_Json_Array return WL.Json.Json_Value'Class;

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

               declare
                  Next_Line : constant String :=
                                Ada.Text_IO.Get_Line (File);
                  Trimmed_Line : constant String :=
                                   (if Next_Line'Length > 0
                                    and then Next_Line (Next_Line'Last)
                                    = Character'Val (13)
                                    then Next_Line
                                      (Next_Line'First ..
                                         Next_Line'Last - 1)
                                    else Next_Line);
               begin

                  Current_Line :=
                    Ada.Strings.Unbounded.To_Unbounded_String
                      (Trimmed_Line);
                  Current_Last :=
                    Ada.Strings.Unbounded.Length (Current_Line);
               end;
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

      function Parse_Json_Array return WL.Json.Json_Value'Class is
         This : WL.Json.Json_Array;
      begin
         while not Done
           and then Current_Character /= ']'
         loop
            This.Append (Parse_Json_Element);
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

         return This;

      end Parse_Json_Array;

      ------------------------
      -- Parse_Json_Element --
      ------------------------

      function Parse_Json_Element return WL.Json.Json_Value'Class is
      begin
         Skip_Whitespace;

         if Done then
            return WL.Json.Null_Value;
         end if;

         case Current_Character is
            when '{' =>
               Next_Character;
               Skip_Whitespace;
               return Parse_Json_Object;
            when '[' =>
               Next_Character;
               Skip_Whitespace;
               return Parse_Json_Array;
            when others =>
               declare
                  Id : constant String := Parse_Terminal;
               begin
                  if Id = "null" then
                     return WL.Json.Null_Value;
                  elsif Id = "true" or else Id = "false" then
                     return WL.Json.Boolean_Value
                       (Boolean'Value (Id));
                  elsif (for all Ch of Id => Ch in '0' .. '9') then
                     return WL.Json.Integer_Value
                       (Integer'Value (Id));
                  else
                     return WL.Json.String_Value (Id);
                  end if;
               end;
         end case;
      end Parse_Json_Element;

      -----------------------
      -- Parse_Json_Object --
      -----------------------

      function Parse_Json_Object return WL.Json.Json_Value'Class is
         This : WL.Json.Json_Object;
      begin
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

                     This.Set_Property (Id, Parse_Json_Element);
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

         return This;

      end Parse_Json_Object;

      --------------------------
      -- Parse_Rest_Of_String --
      --------------------------

      function Parse_Rest_Of_String return String is
         use Ada.Strings.Unbounded;
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
         use Ada.Strings.Unbounded;
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
      return Result : WL.Json.Json_Value'Class :=
        Parse_Json_Element
      do
         if Result in WL.Json.Json_Object'Class then
            WL.Json.Json_Object'Class (Result)
              .Set_Property ("path",
                             Ada.Directories.Containing_Directory (Path));
         end if;
         Finish;
      end return;
   end Read_Json_File;

   -----------
   -- Value --
   -----------

   overriding function Value
     (Position : Json_Cursor;
      Name     : String)
      return String
   is
      Result : constant String :=
                 Position.Top.Element.Get_Property (Name);
   begin
      return (if Result = "null" then "" else Result);
   end Value;

end Kosei.Json;
