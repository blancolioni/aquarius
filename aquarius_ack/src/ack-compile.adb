with Ada.Directories;
--  with Ada.Strings.Fixed;
with Ada.Text_IO;

with Ack.Parser;
with Ack.Semantic;
with Ack.Generate;
with Ack.Loader;

with Ack.Features;

with Ack.Errors;

with Aquarius.Configuration;

with Ack.Semantic.Work;

with GNAT.OS_Lib;

package body Ack.Compile is

   procedure Compile_Class
     (Source_Path : String;
      Result      : in out Compilation_Result'Class;
      Root        : Boolean := False);

   procedure Generate_Object_Code
     (Base_Name   : String;
      Main        : Boolean;
      Success     : out Boolean);

   procedure Compile_Class
     (Source_Path      : String;
      Result           : in out Compilation_Result'Class;
      Root_Class       : Boolean;
      Feature_Callback : access
        procedure (Class        : not null access constant
                     Ack.Classes.Class_Entity_Record'Class;
                   Feature      : not null access constant
                     Root_Entity_Type'Class));

   ----------------------------
   -- Check_Assembly_Package --
   ----------------------------

   procedure Check_Assembly_Package
     (Package_Name : String;
      Assembled    : out Boolean)
   is
      use Ada.Directories;
      use type Ada.Calendar.Time;
      Source_Path : constant String :=
                      Ada.Directories.Compose
                        (Aquarius.Configuration.Aqua_Standard_Assembly_Path,
                         Package_Name,
                         "s");
      Source_Modification_Time : constant Ada.Calendar.Time :=
                                   Ada.Directories.Modification_Time
                                     (Source_Path);
      Object_Path              : constant String :=
                      Aquarius.Configuration.Object_File_Path (Package_Name);
      Ignore_Success           : Boolean;
   begin
      if not Ada.Directories.Exists (Source_Path) then
         raise Constraint_Error
           with "no such standard assembly: " & Package_Name;
      end if;

      Assembled := False;

      if not Exists (Object_Path)
        or else Modification_Time (Object_Path)
        < Source_Modification_Time
      then
         Ada.Directories.Copy_File
           (Source_Path,
            Aquarius.Configuration.Assembly_File_Path (Package_Name));
         Generate_Object_Code (Package_Name, False, Ignore_Success);
         Assembled := True;
      end if;

   end Check_Assembly_Package;

   -------------------
   -- Compile_Class --
   -------------------

   procedure Compile_Class
     (Source_Path : String;
      Result      : in out Compilation_Result'Class;
      Root_Class  : Boolean;
      Feature_Callback : access
        procedure (Class        : not null access constant
                     Ack.Classes.Class_Entity_Record'Class;
                   Feature      : not null access constant
                     Root_Entity_Type'Class))
   is
      Base_Name : constant String :=
                    Ada.Directories.Base_Name (Source_Path);
   begin
      if not Class_Object_Paths.Contains (Base_Name) then
         Compile_Class (Source_Path, Result, Root_Class);

         if not Ack.Errors.Has_Errors then

            for Partial_Class of Partial_Class_List loop
               exit when Ack.Errors.Has_Errors;
               declare
                  Key : constant String :=
                          Get_Entity (Partial_Class).Base_File_Name;
               begin
                  if not Class_Object_Paths.Contains (Key) then
                     declare
                        use Aquarius.Programs;
                        Program     : constant Program_Tree :=
                                        Get_Program (Partial_Class);
                        Source_Path : constant String :=
                                        Program.Source_Directory
                                        & "/" & Program.Source_File_Name;
                     begin
                        Compile_Class (Source_Path, Result);
                     end;
                  end if;
               end;
            end loop;
         end if;

         Partial_Class_List.Clear;
      end if;

      if Feature_Callback /= null
        and then Loaded_Classes.Contains (Base_Name)
      then
         declare
            Class : constant Ack.Classes.Class_Entity :=
                      Ack.Classes.Get_Class_Entity
                        (Loaded_Classes.Element (Base_Name));

            procedure Call (Feature : not null access
                              Ack.Features.Feature_Entity_Record'Class);

            ----------
            -- Call --
            ----------

            procedure Call (Feature : not null access
                              Ack.Features.Feature_Entity_Record'Class) is
            begin
               Feature_Callback (Class, Feature);
            end Call;

         begin
            Class.Scan_Features (Call'Access);
         end;
      end if;

   end Compile_Class;

   -------------------
   -- Compile_Class --
   -------------------

   procedure Compile_Class
     (Source_Path : String;
      Result      : in out Compilation_Result'Class;
      Feature_Callback : access
        procedure (Class        : not null access constant
                     Ack.Classes.Class_Entity_Record'Class;
                   Feature      : not null access constant
                     Root_Entity_Type'Class))
   is
   begin
      Compile_Class (Source_Path, Result, False, Feature_Callback);
   end Compile_Class;

   --------------------------
   -- Generate_Object_Code --
   --------------------------

   procedure Generate_Object_Code
     (Base_Name   : String;
      Main        : Boolean;
      Success     : out Boolean)
   is
      use type GNAT.OS_Lib.Argument_List;
      Object_Path : constant String :=
                      Aquarius.Configuration.Object_File_Path
                        (Base_Name);
      Source_Path : constant String :=
                      Aquarius.Configuration.Assembly_File_Path
                        (Base_Name);
      Assembler   : constant String :=
                      Aquarius.Configuration.Tool_Path ("as");
      Standard_Asm : constant String :=
                       Aquarius.Configuration.Aqua_Standard_Assembly_Path;
      Args        : GNAT.OS_Lib.Argument_List :=
                      [new String'("-o"),
                       new String'(Object_Path),
                       new String'("--config-path"),
                       new String'("./aqua_as/share/aqua_as"),
                       new String'("--write-listing")]
                      & (if Main
                         then [new String'(Standard_Asm & "/artl.s")]
                         else [])
                      & [new String'(Source_Path)];
      Exit_Code   : constant Integer :=
                      GNAT.OS_Lib.Spawn
                        (Program_Name => Assembler,
                         Args         => Args);
   begin
      for Arg of Args loop
         GNAT.OS_Lib.Free (Arg);
      end loop;

      Success := Exit_Code = 0;

      if Exit_Code /= 0 then
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            "Error assembling " & Base_Name);
         Ada.Text_IO.Put
           (Ada.Text_IO.Standard_Error,
            Assembler);
         for Arg of Args loop
            Ada.Text_IO.Put
              (Ada.Text_IO.Standard_Error,
               " " & Arg.all);
         end loop;
         Ada.Text_IO.New_Line (Ada.Text_IO.Standard_Error);

      end if;
   end Generate_Object_Code;

   -------------------
   -- Compile_Class --
   -------------------

   procedure Compile_Class
     (Source_Path : String;
      Result      : in out Compilation_Result'Class;
      Root        : Boolean := False)
   is
      use type Ada.Calendar.Time;
      Base_Name                : constant String :=
                                   Ada.Directories.Base_Name (Source_Path);
      Source_Modification_Time : constant Ada.Calendar.Time :=
                                   Ada.Directories.Modification_Time
                                     (Source_Path);
   begin
      if Result.Compilation_Count = 0
        or else Source_Modification_Time > Result.Newest_Class_Source
      then
         Result.Newest_Class_Source := Source_Modification_Time;
      end if;

      Result.Compilation_Count := Result.Compilation_Count + 1;

      if not Class_Object_Paths.Contains (Base_Name) then
         if not Loaded_Classes.Contains (Base_Name) then
            declare
               Source_Program : constant Aquarius.Programs.Program_Tree :=
                                  Ack.Loader.Load_Class_File (Source_Path);
               Node           : constant Ack.Node_Id :=
                                  Ack.Parser.Import
                                    (Source_Program);
            begin
               Ack.Semantic.Analyse_Class_Declaration (Node);
               Loaded_Classes.Insert (Base_Name, Node);
            end;

         end if;

         while Ack.Semantic.Work.Have_Work loop
            Ack.Semantic.Work.Execute_Work;
         end loop;

         if not Ack.Errors.Has_Errors then
            declare
               use Ada.Directories, Ada.Calendar;
               Object_Path : constant String :=
                               Aquarius.Configuration.Object_File_Path
                                 (Base_Name);
            begin
               if not Exists (Object_Path)
                 or else Modification_Time (Object_Path)
                 < Source_Modification_Time
               then
                  declare
                     Success : Boolean;
                  begin
                     Ada.Text_IO.Put_Line
                       ("generating " & Base_Name);
                     Ack.Generate.Generate_Class_Declaration
                       (Loaded_Classes.Element (Base_Name), Root);

                     Generate_Object_Code (Base_Name, Root, Success);

                     if not Success then
                        Result.Error := True;
                     end if;
                  end;
               end if;

               Class_Object_Paths.Insert (Base_Name, Object_Path);

            end;
         end if;

      end if;

   end Compile_Class;

   ---------------------
   -- Load_Root_Class --
   ---------------------

   procedure Load_Root_Class
     (Source_Path : String)
   is
      Result : Ack.Compile.Compilation_Result;
   begin
      Compile_Class (Source_Path, Result,
                     Root_Class => True,
                     Feature_Callback => null);

      Ada.Text_IO.Put_Line
        ("loaded"
         & Natural'Image (Result.Compiled_Classes_Count)
         & " classes");

      if Result.Has_Error then
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            "assembly failed");
      end if;
   end Load_Root_Class;

end Ack.Compile;
