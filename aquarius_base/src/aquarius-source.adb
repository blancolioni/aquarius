with Ada.Characters.Latin_1;
with Ada.Directories;
with Ada.Strings.Fixed;

package body Aquarius.Source is

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : Source_Position) return Boolean is
   begin
      if Left.File = Right.File then
         if Left.Line = Right.Line then
            if Left.Column = Right.Column then
               return False;
            else
               return Left.Column < Right.Column;
            end if;
         else
            return Left.Line < Right.Line;
         end if;
      else
         return Left.File.Get_File_Name < Right.File.Get_File_Name;
      end if;
   end "<";

   --------------------------
   -- Containing_Directory --
   --------------------------

   function Containing_Directory (From : Source_File) return String is
   begin
      return Ada.Directories.Containing_Directory
        (From.Get_Full_Path);
   end Containing_Directory;

   -----------------
   -- End_Of_File --
   -----------------

   function End_Of_File (Position : Source_Position) return Boolean is
   begin
      return Position.End_Of_File;
   end End_Of_File;

   -----------------
   -- End_Of_Line --
   -----------------

   function End_Of_Line (Position : Source_Position) return Boolean is
   begin
      return Position.End_Of_Line;
   end End_Of_Line;

   -------------------
   -- Get_Character --
   -------------------

   function Get_Character (Position : Source_Position) return Character is
   begin
      if Position.End_Of_File then
         raise Constraint_Error with
           "tried to read past the end of the file at " &
           Show (Position);
      elsif Position.End_Of_Line then
         raise Constraint_Error with
           "tried to read past the end of the line at " &
           Show (Position);
      else
         return Position.File.Current_Line
           (Positive (Position.Column));
      end if;
   end Get_Character;

   ----------------
   -- Get_Column --
   ----------------

   function Get_Column (From : Source_Position) return Column_Number is
   begin
      return From.Column;
   end Get_Column;

   -------------------------
   -- Get_Column_Position --
   -------------------------

   function Get_Column_Position (From   : Source_Position;
                                 Column : Column_Number)
                                return Source_Position
   is
      Result : Source_Position := From;
   begin
      Result.Column := Column;
      return Result;
   end Get_Column_Position;

   -------------------
   -- Get_File_Name --
   -------------------

   function Get_File_Name (From : Source_File) return String is
   begin
      if From = null then
         return "(input)";
      else
         return From.Get_File_Name;
      end if;
   end Get_File_Name;

   -------------------
   -- Get_Full_Path --
   -------------------

   function Get_Full_Path (From : Source_File) return String is
   begin
      return From.Get_Full_Path;
   end Get_Full_Path;

   --------------
   -- Get_Line --
   --------------

   function Get_Line (From : Source_Position) return Line_Number is
   begin
      return From.Line;
   end Get_Line;

   --------------
   -- Get_Line --
   --------------

   procedure Get_Line
     (Position    : Source_Position;
      Include_EOL : Boolean;
      Line        :    out String;
      Last        :    out Natural)
   is
   begin
      if Position.End_Of_File then
         Last := 0;
         return;
      end if;

      Line (Line'First ..
              Line'First + Position.File.Current_Line_Length - 1) :=
        Position.File.Current_Line (1 .. Position.File.Current_Line_Length);
      Last := Position.File.Current_Line_Length + Line'First - 1;
      if Include_EOL then
         Last := Last + 1;
         Line (Last) := Ada.Characters.Latin_1.LF;
      end if;
   end Get_Line;

   -----------------------
   -- Get_Relative_Path --
   -----------------------

   function Get_Relative_Path (From : Source_File) return String is
      Result  : constant String :=
                  (if From = null
                   then "(input)"
                   else From.Get_Full_Path);
      Current : constant String := Ada.Directories.Current_Directory;
      Match   : constant String :=
                  (if Result'Length >= Current'Length
                   then Result (1 .. Current'Length)
                   else "");
   begin
      if Match /= "" and then Match = Current then
         return "." & Result (Match'Length + 1 .. Result'Last);
      else
         return Result;
      end if;
   end Get_Relative_Path;

   ---------------------
   -- Get_Source_File --
   ---------------------

   function Get_Source_File (From : Source_Position) return Source_File is
   begin
      return From.File;
   end Get_Source_File;

   ---------------
   -- Get_Start --
   ---------------

   function Get_Start (From : Source_File) return Source_Position is
   begin
      return From.Start_Position;
   end Get_Start;

   -------------
   -- Go_Back --
   -------------

   procedure Go_Back (Position   : in out Source_Position;
                      Characters : Natural)
   is
   begin
      Position.Column := Position.Column - Column_Number (Characters);
   end Go_Back;

   ------------------
   -- Set_Position --
   ------------------

   procedure Set_Position (Position : in out Source_Position;
                           Line     : Line_Number;
                           Column   : Column_Number)
   is
   begin
      Position.Line := Line;
      Position.Column := Column;
   end Set_Position;

   ----------
   -- Show --
   ----------

   function Show
     (Position  : Source_Position;
      Full_Path : Boolean := False)
      return String
   is
   begin
      if Position.File = null then
         return "<null source file>";
      else
         declare
            Line_Image : constant String :=
              Ada.Strings.Fixed.Trim (Line_Number'Image (Position.Line),
                                      Ada.Strings.Left);
            Col_Image  : constant String :=
              Ada.Strings.Fixed.Trim (Column_Number'Image (Position.Column),
                                      Ada.Strings.Left);
            File_Image : constant String :=
                           (if Full_Path
                            then Position.File.Get_Full_Path
                            else Position.File.Get_File_Name);
         begin
            if Position = No_Source_Position then
               return "no source position";
            else
               return File_Image & ":" &
                 Line_Image & ":" &
                 Col_Image;
            end if;
         end;
      end if;

   end Show;

   --------------------
   -- Skip_Character --
   --------------------

   procedure Skip_Character (Position : in out Source_Position) is
   begin
      if Position.End_Of_Line then
         if Position.File.End_Of_File then
            Position.End_Of_File := True;
            Position.File.Close;
            return;
         else
            Position.File.Next_Line;
            Position.Column := 1;
            Position.Line := Position.Line + 1;
         end if;
      else
         Position.Column := Position.Column + 1;
      end if;
      Position.End_Of_Line :=
        Natural (Position.Column) > Position.File.Current_Line_Length;
   end Skip_Character;

   ---------------
   -- Skip_Line --
   ---------------

   procedure Skip_Line (Position : in out Source_Position) is
   begin
      Position.End_Of_Line := True;
      Skip_Character (Position);
   end Skip_Line;

end Aquarius.Source;
