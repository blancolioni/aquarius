with Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Text_IO;
with Aquarius.Configuration;

package body Aquarius.Filesystem is

   function To_File_Path (Sandboxed_Path : String) return String;

   ------------
   -- Append --
   ------------

   function Append (Path : String; Text : String) return Boolean is
      use Ada.Text_IO;
      File : File_Type;
   begin
      if Path = "stdout" then
         Put (Text);
      elsif Path = "stderr" then
         Put (Standard_Error, Text);
      else
         Open (File, Append_File, Path);
         Put (File, Text);
         Close (File);
      end if;
      return True;
   exception
      when others =>
         return False;
   end Append;

   ------------
   -- Create --
   ------------

   function Create (Path : String) return Boolean is
      use Ada.Text_IO;
      File : File_Type;
   begin
      Create (File, Out_File, To_File_Path (Path));
      Close (File);
      return True;
   exception
      when others =>
         return False;
   end Create;

   ------------
   -- Delete --
   ------------

   function Delete (Path : String) return Boolean is
   begin
      Ada.Directories.Delete_File (To_File_Path (Path));
      return True;
   exception
      when others =>
         return False;
   end Delete;

   ------------
   -- Exists --
   ------------

   function Exists (Path : String) return Boolean is
   begin
      return Ada.Directories.Exists (To_File_Path (Path));
   end Exists;

   ------------------
   -- To_File_Path --
   ------------------

   function To_File_Path (Sandboxed_Path : String) return String is
      procedure Validate;

      --------------
      -- Validate --
      --------------

      procedure Validate is
         Found : Boolean := False;
      begin
         for Ch of Sandboxed_Path loop
            if Ch = '/' then
               if Found then
                  raise Constraint_Error
                    with "invalid path: too many elements";
               end if;
               Found := True;
            end if;
         end loop;
         if not Found then
            raise Constraint_Error with "invalid path: no directory";
         end if;
      end Validate;

   begin
      Validate;
      declare
         Slash_Index : constant Positive :=
                         Ada.Strings.Fixed.Index (Sandboxed_Path, "/");
         Directory : constant String :=
                         Sandboxed_Path (Sandboxed_Path'First ..
                                                         Slash_Index - 1);
         File        : constant String :=
                         Sandboxed_Path (Slash_Index + 1 ..
                                                         Sandboxed_Path'Last);
      begin
         return Ada.Directories.Compose
           (Aquarius.Configuration.Directory_Path (Directory), File);
      end;
   end To_File_Path;

end Aquarius.Filesystem;
