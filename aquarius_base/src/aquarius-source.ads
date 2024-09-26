with Aquarius.Sources;

package Aquarius.Source is

   pragma Elaborate_Body (Aquarius.Source);

   File_Not_Found : exception;

   type Line_Number is new Natural;
   type Column_Number is new Natural;

   type Source_Position is private;
   type Source_File is private;

   function "<" (Left, Right : Source_Position) return Boolean;

   function To_Source_Reference
     (File : Source_File)
      return Aquarius.Sources.Source_Reference;

   --  If we encounter a tab character, it gets translated to spaces
   --  Aquarius itself never uses tab characters, so files that are
   --  read in need to be scrubbed.  This variable tells Aquarius
   --  which columns tab characters jump to
   Tab_Stop : Positive := 8;

   No_Source_Position : constant Source_Position;
   No_Source_File     : constant Source_File;

   function Get_Start (From : Source_File) return Source_Position;

   function Get_Line (From : Source_Position) return Line_Number;
   function Get_Column (From : Source_Position) return Column_Number;

   function Get_Column_Position (From   : Source_Position;
                                 Column : Column_Number)
                                return Source_Position;

   function Get_Source_File (From : Source_Position) return Source_File;

   function Get_File_Name (From : Source_File) return String;
   function Get_Full_Path (From : Source_File) return String;
   function Get_Relative_Path (From : Source_File) return String;
   function Containing_Directory (From : Source_File) return String;

   function Get_Character (Position : Source_Position) return Character;
   procedure Skip_Character (Position : in out Source_Position);

   procedure Skip_Line (Position : in out Source_Position);

   procedure Get_Line
     (Position : Source_Position;
      Include_EOL : Boolean;
      Line        :    out String;
      Last        :    out Natural);

   function End_Of_Line (Position : Source_Position) return Boolean;
   function End_Of_File (Position : Source_Position) return Boolean;

   function Show
     (Position  : Source_Position;
      Full_Path : Boolean := False)
      return String;

   procedure Go_Back (Position   : in out Source_Position;
                      Characters : Natural);

   procedure Set_Position (Position : in out Source_Position;
                           Line     : Line_Number;
                           Column   : Column_Number);

private

   Max_Line_Length : constant := 2000;

   type Source_File_Record is abstract tagged limited
      record
         Start_Position      : Source_Position;
         Current_Position    : Source_Position;
         Current_Line        : String (1 .. Max_Line_Length);
         Current_Line_Length : Natural;
         Reference           : Aquarius.Sources.Source_Reference;
      end record;

   function Get_File_Name (File : access Source_File_Record) return String
      is abstract;

   function Get_Full_Path (File : access Source_File_Record) return String
      is abstract;

   procedure Next_Line (File    : access Source_File_Record)
      is abstract;

   --  Fills in Current_Line
   --  Must not insert tabs
   function End_Of_File (File : access Source_File_Record) return Boolean
      is abstract;

   procedure Close (File : access Source_File_Record) is abstract;

   type Source_File is access all Source_File_Record'Class;

   No_Source_File : constant Source_File := null;

   type Source_Position is
      record
         File        : Source_File;
         Line        : Line_Number;
         Column      : Column_Number;
         End_Of_Line : Boolean;
         End_Of_File : Boolean;
      end record;

   No_Source_Position : constant Source_Position :=
     (null, 0, 0, False, True);

   function To_Source_Reference
     (File : Source_File)
      return Aquarius.Sources.Source_Reference
   is (File.Reference);

end Aquarius.Source;
