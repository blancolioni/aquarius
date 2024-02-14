with Ada.Calendar;
with Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;

with Kosei;

package body Aquarius.Paths is

   Always_Newer : constant Boolean := False;

   Unix_Paths : constant Boolean := False;
   Separator  : constant Character := '\';

   Standard_Separator : constant Character := '/';

   Standardise_Path : constant Ada.Strings.Maps.Character_Mapping :=
                        Ada.Strings.Maps.To_Mapping
                          ("\", [Standard_Separator]);

   --------------
   -- Is_Newer --
   --------------

   function Is_Newer
     (File_1, File_2 : String)
      return Boolean
   is
      use type Ada.Calendar.Time;
   begin
      return Always_Newer
        or else not Ada.Directories.Exists (File_2)
        or else (Ada.Directories.Exists (File_1)
                 and then Ada.Directories.Modification_Time (File_1)
                 > Ada.Directories.Modification_Time (File_2));
   end Is_Newer;

   ----------------
   -- Join_Paths --
   ----------------

   function Join_Paths (Left, Right : Aquarius_Path) return Aquarius_Path is
      use type Ada.Containers.Count_Type;
      Result : Aquarius_Path := Left;
   begin
      if Right.Absolute then
         return Right;
      end if;

      for I in 1 .. Right.Elements.Last_Index loop
         declare
            Elem : constant Unbounded_String := Right.Elements.Element (I);
         begin
            if Elem = "." then
               null;
            elsif Elem = ".." then
               if Result.Elements.Length > 0 then
                  Result.Elements.Delete_Last;
               else
                  Result.Elements.Append (Elem);
               end if;
            else
               Result.Elements.Append (Right.Elements.Element (I));
            end if;
         end;
      end loop;
      return Result;
   end Join_Paths;

   ------------------
   -- Scratch_File --
   ------------------

   function Scratch_File (Name : String;
                          Extension : String := "")
                          return String
   is
   begin
      return Ada.Directories.Compose
        (Scratch_Path, Name, Extension);
   end Scratch_File;

   ------------------
   -- Scratch_Path --
   ------------------

   function Scratch_Path return String is
   begin
      return Kosei.Get ("/install/paths/scratch");
   end Scratch_Path;

   ----------------------
   -- To_Aquarius_Path --
   ----------------------

   function To_Aquarius_Path (From_OS_Path : String) return Aquarius_Path is
      Result  : Aquarius_Path;
      Standard_Path : constant String :=
                  Ada.Strings.Fixed.Translate
                    (Source  => From_OS_Path,
                     Mapping => Standardise_Path);
      Start  : Positive       := Standard_Path'First;
      Index  : Positive;
   begin
      if not Unix_Paths and then
        Standard_Path'Length > 1 and then
        Standard_Path (Standard_Path'First + 1) = ':'
      then
         declare
            Prefix : constant String :=
                       Standard_Path
                         (Standard_Path'First .. Standard_Path'First + 1);
         begin
            Result.Prefix := To_Unbounded_String (Prefix);
            Start := Standard_Path'First + 2;
         end;
      end if;

      if Standard_Path'Length >= 2
        and then
          Standard_Path (Standard_Path'First .. Standard_Path'First + 1)
        = ".."
      then
         Result.Absolute := False;
      end if;

      Result.Absolute := Standard_Path (Start) = Standard_Separator;

      if Result.Absolute then
         Start := Start + 1;
      end if;

      while Start < Standard_Path'Last loop

         while Start <= Standard_Path'Last
           and then Standard_Path (Start) = Standard_Separator
         loop
            Start := Start + 1;
         end loop;

         exit when Start > Standard_Path'Last;

         Index := Start;
         if False
           and then Index < Standard_Path'Last
           and then Standard_Path (Index .. Index + 1) = ".."
           and then (Index = Standard_Path'Last - 1
                     or else Standard_Path (Index + 2) = Standard_Separator)
           and then Result.Elements.Last_Index > 0
         then
            Result.Elements.Delete_Last;
            Start := Index + 2;
         elsif False
           and then Index < Standard_Path'Last
           and then Standard_Path (Index) = '.'
           and then Standard_Path (Index + 1) = Standard_Separator
         then
            Start := Index + 1;
         else
            while Index <= Standard_Path'Last
              and then Standard_Path (Index) /= Standard_Separator
            loop
               Index := Index + 1;
            end loop;

            if Index > Start then
               Result.Elements.Append
                 (To_Unbounded_String (Standard_Path (Start .. Index - 1)));
               Start := Index;
            else
               exit;
            end if;
         end if;
      end loop;

      return Result;

   end To_Aquarius_Path;

   ----------------
   -- To_OS_Path --
   ----------------

   function To_OS_Path (Path : Aquarius_Path) return String is
      Result : Unbounded_String;
   begin
      if Path.Absolute then
         Result := Path.Prefix & Separator;
      end if;

      for I in 1 .. Path.Elements.Last_Index loop
         Result := Result & Path.Elements.Element (I);
         if I < Path.Elements.Last_Index then
            Result := Result & Separator;
         end if;
      end loop;

      return To_String (Result);

   end To_OS_Path;

end Aquarius.Paths;
