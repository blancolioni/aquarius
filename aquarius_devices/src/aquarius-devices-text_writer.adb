with Ada.Containers.Indefinite_Vectors;
with Ada.Text_IO;
with Aquarius.Filesystem;

package body Aquarius.Devices.Text_Writer is

   use Aqua;

   Register_Count : constant := 1024;
   type Register_Index is range 0 .. Register_Count - 1;

   R_Command       : constant Register_Index := 0;
   R_File          : constant Register_Index := 1;
   R_Buffer_Length : constant Register_Index := 2;
   R_Buffer_Start  : constant Register_Index := 3;

   type Register_Array is array (Register_Index) of Word_32;

   Command_Mask : constant := 16#00FF#;

   Command_Open_File  : constant := 1;
   Command_Close_File : constant := 2;
   Command_Write      : constant := 3;
   Command_Read       : constant := 4;

   Flag_Create        : constant := 16#0100#;
   Flag_Write         : constant := 16#0200#;
   Flag_Replace       : constant := 16#0400#;

   E_No_Error            : constant := 0;
   E_Bad_Command         : constant := 1;
   E_File_Not_Found      : constant := 2;
   E_File_Not_Open       : constant := 3;
   E_File_Exists         : constant := 4;
   E_Operation_Failed    : constant := 5;
   E_Not_Implemented     : constant := 6;

   package String_Vectors is
     new Ada.Containers.Indefinite_Vectors (Natural, String);

   subtype Parent is Aquarius.Devices.Instance;

   type Instance is new Parent with
      record
         Rs     : Register_Array := [others => 0];
         Files  : String_Vectors.Vector;
      end record;

   type Instance_Reference is access all Instance'Class;

   overriding function Name (This : Instance) return String
   is ("aqua-text-writer");

   overriding function Word_Count (This : Instance) return Natural
   is (Natural (Register_Count));

   overriding procedure Get_Word_32
     (This    : in out Instance;
      Address : Address_Type;
      Value   : out Word_32);

   overriding procedure Set_Word_32
     (This    : in out Instance;
      Address : Address_Type;
      Value   : Word_32);

   procedure Run_Command
     (This    : in out Instance'Class;
      Command : Word_32);

   ------------
   -- Create --
   ------------

   function Create
     return Aquarius.Devices.Reference
   is
      This : constant Instance_Reference := new Instance;
   begin
      This.Files.Append ("stdin");
      This.Files.Append ("stdout");
      This.Files.Append ("stderr");
      return Aquarius.Devices.Reference (This);
   end Create;

   -----------------
   -- Get_Word_32 --
   -----------------

   overriding procedure Get_Word_32
     (This    : in out Instance;
      Address : Address_Type;
      Value   : out Word_32)
   is
   begin
      Value := This.Rs (Register_Index (Address / 4));
   end Get_Word_32;

   -----------------
   -- Run_Command --
   -----------------

   procedure Run_Command
     (This    : in out Instance'Class;
      Command : Word_32)
   is
      Create  : constant Boolean := (Command and Flag_Create) = Flag_Create;
      Write   : constant Boolean := (Command and Flag_Write) = Flag_Write;
      Replace : constant Boolean := (Command and Flag_Replace) = Flag_Replace;
      Exists  : Boolean;
   begin
      case Command and Command_Mask is
         when Command_Open_File =>
            declare
               Count : Natural := 0;
               Path  : String (1 .. Natural (This.Rs (R_Buffer_Length)));
               First : constant Register_Index := R_Buffer_Start + 1;
               Last  : constant Register_Index :=
                         First
                           + Register_Index (This.Rs (R_Buffer_Length)) - 1;
            begin
               for X of This.Rs (First .. Last) loop
                  Count := Count + 1;
                  Path (Count) := Character'Val (X);
               end loop;

               Exists := Filesystem.Exists (Path);

               if Exists
                 and then (Write or else Create)
                 and then not Replace
               then
                  Ada.Text_IO.Put_Line ("file exists replace not set");
                  This.Rs (R_Command) := E_File_Exists;
                  return;
               end if;

               if not Exists
                 and then not Create
               then
                  Ada.Text_IO.Put_Line
                    ("file does not exist but create not set");
                  This.Rs (R_Command) := E_File_Not_Found;
                  return;
               end if;

               if Exists and then Replace then
                  if not Filesystem.Delete (Path) then
                     Ada.Text_IO.Put_Line
                       ("cannot delete " & Path);
                     This.Rs (R_Command) := E_Operation_Failed;
                     return;
                  end if;
               end if;

               if not Filesystem.Create (Path) then
                  Ada.Text_IO.Put_Line
                    ("cannot create " & Path);
                  This.Rs (R_Command) := E_Operation_Failed;
                  return;
               end if;

               declare
                  File_Index : Natural := 0;
               begin
                  while File_Index <= This.Files.Last_Index
                    and then This.Files (File_Index) /= ""
                  loop
                     File_Index := File_Index + 1;
                  end loop;

                  if File_Index > This.Files.Last_Index then
                     This.Files.Append (Path);
                  else
                     This.Files.Replace_Element (File_Index, Path);
                  end if;

                  This.Rs (R_Command) := E_No_Error;
                  This.Rs (R_File) := Word_32 (File_Index);
               end;
            end;

         when Command_Close_File =>
            declare
               File_Index : constant Natural :=
                              Natural (This.Rs (R_File));
            begin
               if File_Index > This.Files.Last_Index
                 or else This.Files (File_Index) = ""
               then
                  This.Rs (R_Command) := E_File_Not_Open;
                  return;
               end if;
               This.Files.Replace_Element (File_Index, "");
            end;

         when Command_Read =>
            This.Rs (R_Command) := E_Not_Implemented;

         when Command_Write =>
            declare
               File_Index : constant Natural :=
                              Natural (This.Rs (R_File));
            begin
               if File_Index > This.Files.Last_Index
                 or else This.Files (File_Index) = ""
               then
                  Ada.Text_IO.Put_Line
                    ("File" & File_Index'Image & " not open");

                  This.Rs (R_Command) := E_File_Not_Open;
                  return;
               end if;

               declare
                  Count   : Natural := 0;
                  Buffer  : String (1 .. Natural (This.Rs (R_Buffer_Length)));
                  First   : constant Register_Index := R_Buffer_Start;
                  Last    : constant Register_Index :=
                              First
                                + Register_Index (This.Rs (R_Buffer_Length))
                              - 1;
               begin
                  for X of This.Rs (First .. Last) loop
                     Count := Count + 1;
                     Buffer (Count) := Character'Val (X);
                  end loop;
                  This.Rs (R_Command) := E_No_Error;
                  if not Filesystem.Append
                    (This.Files (File_Index), Buffer)
                  then
                     This.Rs (R_Command) := E_Operation_Failed;
                  end if;
               end;
            end;
         when others =>
            This.Rs (R_Command) := E_Bad_Command;
      end case;
   end Run_Command;

   -----------------
   -- Set_Word_32 --
   -----------------

   overriding procedure Set_Word_32
     (This    : in out Instance;
      Address : Address_Type;
      Value   : Word_32)
   is
      R : constant Register_Index := Register_Index (Address / 4);
   begin
      This.Rs (R) := Value;
      case R is
         when R_Command =>
            This.Run_Command (Value);
         when R_File =>
            null;
         when R_Buffer_Length =>
            null;
         when others =>
            null;
      end case;
   end Set_Word_32;

end Aquarius.Devices.Text_Writer;
