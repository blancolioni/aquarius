with Ada.Characters.Handling;
with Ada.Containers.Vectors;
with Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Text_IO;

with Aquarius.Grammars.EBNF;
with Aquarius.Loader;
with Aquarius.Messages.Console;

with Kosei;

package body Aquarius.Grammars.Manager is

   Loaded_EBNF  : Boolean := False;
   EBNF_Grammar : Aquarius.Grammars.Aquarius_Grammar;

   package Grammar_Vectors is
      new Ada.Containers.Vectors (Positive, Aquarius_Grammar);

   Loaded_Grammars : Grammar_Vectors.Vector;

   function Load_Grammar (Name : String) return Aquarius_Grammar;

   function Load_Grammar_From_File (Name   : String;
                                    Path : String)
                                   return Aquarius_Grammar;

   procedure Check_EBNF;
   --  Check_EBNF: load EBNF grammar if it's not already loaded.

   ----------------
   -- Check_EBNF --
   ----------------

   procedure Check_EBNF is
   begin
      if not Loaded_EBNF then
         EBNF_Grammar := Aquarius.Grammars.EBNF.Create_EBNF_Grammar;
         Loaded_Grammars.Append (EBNF_Grammar);
         Loaded_EBNF  := True;
      end if;
   end Check_EBNF;

   -----------------
   -- Get_Grammar --
   -----------------

   function Get_Grammar (Name : String) return Aquarius_Grammar is
      Standard_Name : constant String :=
        Ada.Characters.Handling.To_Lower (Name);
   begin

      Check_EBNF;

      for I in 1 .. Loaded_Grammars.Last_Index loop
         if Loaded_Grammars.Element (I).Name = Standard_Name then
            return Loaded_Grammars.Element (I);
         end if;
      end loop;

      if True then
         Ada.Text_IO.Put_Line
           ("Loading grammar: " & Standard_Name);
      end if;

      --  Maybe we want to load a custom grammar.  Check to see if
      --  a file exists in the current directory, and that the name
      --  we supplied has an extension
      declare
         use Ada.Directories;
      begin
         if Exists (Standard_Name)
           and then Kind (Standard_Name) = Ordinary_File
           and then Ada.Strings.Fixed.Index (Standard_Name, ".") > 0
         then
            return Load_Grammar_From_File
              (Ada.Directories.Simple_Name (Standard_Name),
               Standard_Name);
         else
            return Load_Grammar (Standard_Name);
         end if;
      end;

   end Get_Grammar;

   --------------------------
   -- Get_Grammar_For_File --
   --------------------------

   function Get_Grammar_For_File (File_Name : String)
                                 return Aquarius_Grammar
   is
      Extension   : constant String :=
        Ada.Directories.Extension (File_Name);
   begin
      if Extension = "ebnf" then
         Check_EBNF;
         return EBNF_Grammar;
      end if;

      declare
         Grammar_Name : constant String :=
                          Kosei.Get
                            ("/extensions/" & Extension & "/plugin");
      begin
         if Grammar_Name = "" then
            raise Grammar_Error with
              "cannot find grammar for file extension ." & Extension;
         else
            return Get_Grammar (Grammar_Name);
         end if;
      end;
   end Get_Grammar_For_File;

   ------------------
   -- Load_Grammar --
   ------------------

   function Load_Grammar (Name : String) return Aquarius_Grammar is
      Path : constant String :=
               Kosei.Get ("/path")
             & "/"
             & Kosei.Get ("/install/paths/grammar")
               & "/" & Name;
   begin
      Check_EBNF;
      if Name = "ebnf" then
         return EBNF_Grammar;
      end if;

      return Load_Grammar_From_File
        (Name => Name,
         Path => Ada.Directories.Compose (Path, Name, "ebnf"));
   end Load_Grammar;

   ----------------------------
   -- Load_Grammar_From_File --
   ----------------------------

   function Load_Grammar_From_File (Name   : String;
                                    Path : String)
                                   return Aquarius_Grammar
   is

      Result : Aquarius_Grammar;
      EBNF   : Aquarius.Programs.Program_Tree;

   begin

      --  Turn off tracing while loading a grammar

      EBNF   := Aquarius.Loader.Load_From_File (EBNF_Grammar, Path);

      Result := New_Grammar (Name, EBNF);
      Loaded_Grammars.Append (Result);

      EBNF.Set_Property (Aquarius.Properties.Grammar_Property, Result);
      EBNF_Grammar.Run_Actions ("analyse", EBNF);

      declare
         use Aquarius.Messages;
         List : Message_List;
      begin
         EBNF.Get_Messages (List);
         if Highest_Level (List) > Warning then
            Aquarius.Messages.Console.Show_Messages (List);
            return null;
         end if;
      end;

      Result.Check_Grammar;

      declare
         use Aquarius.Messages;
         List : Message_List;
      begin
         EBNF.Get_Messages (List);
         Aquarius.Messages.Console.Show_Messages (List);
         if Highest_Level (List) > Warning then
            return null;
         end if;
      end;

      return Result;

   end Load_Grammar_From_File;

end Aquarius.Grammars.Manager;
