with Ada.Characters.Latin_1;
with Ada.Directories;
with Ada.Strings.UTF_Encoding;

package body Aquarius.Source.File_System is

   type File_System_File_Access is access all File_System_File'Class;

   --------------
   -- Add_Path --
   --------------

   procedure Add_Path (List : in out Search_Path_List;
                       Path : String)
   is
      New_Path : constant Aquarius.Paths.Aquarius_Path :=
                   Aquarius.Paths.To_Aquarius_Path (Path);
   begin
      List.Paths.Append (New_Path);
   end Add_Path;

   -----------
   -- Close --
   -----------

   overriding
   procedure Close (Item : access File_System_File) is
   begin
      Ada.Text_IO.Close (Item.File);
   end Close;

   -----------------------
   -- Current_Directory --
   -----------------------

   function Current_Directory return Search_Path_List is
      Result : Search_Path_List;
   begin
      Result.Paths.Append
        (Aquarius.Paths.To_Aquarius_Path
           (Ada.Directories.Current_Directory));
      return Result;
   end Current_Directory;

   -----------------
   -- End_Of_File --
   -----------------

   overriding
   function End_Of_File (File : access File_System_File) return Boolean is
   begin
      return Ada.Text_IO.End_Of_File (File.File);
   end End_Of_File;

   ---------------
   -- Find_File --
   ---------------

   function Find_File (Search_Path : Search_Path_List;
                       File_Name   : String)
                      return String
   is

      function Check_Path (OS_Path : String) return String;

      ----------------
      -- Check_Path --
      ----------------

      function Check_Path (OS_Path : String) return String is
         File_Path : constant String :=
           Ada.Directories.Compose (OS_Path,
                                    File_Name);
      begin
         if Ada.Directories.Exists (File_Path) then
            return File_Path;
         else
            return "";
         end if;
      end Check_Path;

   begin

      if Search_Path.Paths.Is_Empty then
         return Check_Path (Aquarius.Paths.To_OS_Path (Search_Path.Base_Path));
      end if;

      for Path of Search_Path.Paths loop
         declare
            use Aquarius.Paths;
            Full_Path : constant Aquarius_Path :=
                          Join_Paths (Search_Path.Base_Path, Path);
            OS_Path   : constant String := To_OS_Path (Full_Path);
            Result    : constant String := Check_Path (OS_Path);
         begin
            if Result /= "" then
               return Result;
            end if;
         end;
      end loop;

      return "";

   end Find_File;

   -------------------
   -- Get_File_Name --
   -------------------

   overriding
   function Get_File_Name (File : access File_System_File) return String is
   begin
      return Ada.Strings.Unbounded.To_String (File.File_Name);
   end Get_File_Name;

   -------------------
   -- Get_Full_Path --
   -------------------

   overriding
   function Get_Full_Path (File : access File_System_File) return String is
   begin
      return Ada.Strings.Unbounded.To_String (File.Full_Path);
   end Get_Full_Path;

   ---------------
   -- Next_Line --
   ---------------

   overriding
   procedure Next_Line (File : access File_System_File) is
      Ch     : Character;
      Length : Natural := 0;
   begin
      while not Ada.Text_IO.End_Of_Line (File.File) loop
         Ada.Text_IO.Get (File.File, Ch);
         if Ch = Ada.Characters.Latin_1.HT then
            loop
               Length := Length + 1;
               File.Current_Line (Length) := ' ';
               exit when Length mod Tab_Stop = 0;
            end loop;
         else
            Length := Length + 1;
            File.Current_Line (Length) := Ch;
         end if;
      end loop;

      --  check for BOM
      declare
         use Ada.Text_IO, Ada.Strings.UTF_Encoding;
      begin
         if Line (File.File) = 1
           and then Length >= 3
           and then File.Current_Line (1 .. 3) = BOM_8
         then
            File.Current_Line (1 .. Length - 3) :=
              File.Current_Line (4 .. Length);
            Length := Length - 3;
         end if;
      end;

      --  check for carriage return as last character
      if Length > 0
        and then File.Current_Line (Length) = Character'Val (13)
      then
         Length := Length - 1;
      end if;

      if not Ada.Text_IO.End_Of_File (File.File) then
         Ada.Text_IO.Skip_Line (File.File);
      end if;

      File.Current_Line_Length := Length;
   end Next_Line;

   ---------------
   -- Read_File --
   ---------------

   function Read_File (File_Path   : String)
                      return Source_File
   is
      File : constant File_System_File_Access := new File_System_File;
   begin
      Ada.Text_IO.Open (File.File,
                        Ada.Text_IO.In_File, File_Path);
      File.File_Name :=
        Ada.Strings.Unbounded.To_Unbounded_String
        (Ada.Directories.Simple_Name (File_Path));
      File.Full_Path :=
        Ada.Strings.Unbounded.To_Unbounded_String (File_Path);
      File.Current_Position := (Source_File (File), 0, 0, True,
                                Ada.Text_IO.End_Of_File (File.File));
      File.Current_Line_Length := 0;
      if not End_Of_File (File) then
         Skip_Character (File.Current_Position);
      end if;
      File.Start_Position := File.Current_Position;
      return Source_File (File);
   end Read_File;

   ---------------
   -- Read_File --
   ---------------

   function Read_File (File_Path   : String;
                       Search_Path : Search_Path_List;
                       Extension   : String            := "")
                      return Source_File
   is
   begin
      for Path of Search_Path.Paths loop
         declare
            use Aquarius.Paths;
            Full_Path : constant Aquarius_Path :=
                          Join_Paths (Search_Path.Base_Path, Path);
            File_Name : constant String :=
                          Ada.Directories.Compose (To_OS_Path (Full_Path),
                                                   File_Path, Extension);
         begin
            if Ada.Directories.Exists (File_Name) then
               return Read_File (File_Name);
            end if;
         end;
      end loop;

      raise File_Not_Found;

   end Read_File;

   -------------------
   -- Set_Base_Path --
   -------------------

   procedure Set_Base_Path (List : in out Search_Path_List;
                            Path : String)
   is
   begin
      List.Base_Path := Aquarius.Paths.To_Aquarius_Path (Path);
   end Set_Base_Path;

end Aquarius.Source.File_System;
