with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Directories;

with Aquarius.Configuration;

package body Ack.Files is

   package String_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists (String);

   ---------------------
   -- Find_Class_File --
   ---------------------

   function Find_Class_File
     (Referrer : Aquarius.Programs.Program_Tree;
      Parent   : not null access constant Root_Entity_Type'Class;
      Name     : Name_Id)
      return String
   is
      use type Aquarius.Programs.Program_Tree;

      Class_Path  : String_Lists.List;
      File_Name : constant String :=
                    Parent.Base_Child_File_Name (Name) & ".aqua";
   begin

      if Referrer /= null then
         Class_Path.Append (Referrer.Source_Directory);
      end if;

      if Parent.Declaration_Node /= No_Node then
         declare
            Parent_Path : constant String :=
                            Get_Program (Parent.Declaration_Node)
                            .Source_Directory;
         begin
            Class_Path.Append (Parent_Path);
         end;
      end if;

      Class_Path.Append (Ada.Directories.Current_Directory);
      Class_Path.Append (Aquarius.Configuration.Aqua_Standard_Library_Path);
      Class_Path.Append (Aquarius.Configuration.Generated_Path);

      for Path of Class_Path loop
         declare
            File_Path : constant String :=
                          Ada.Directories.Compose (Path, File_Name);
         begin
            if Ada.Directories.Exists (File_Path) then
               return File_Path;
            end if;
         end;
      end loop;

      return "";

   end Find_Class_File;

end Ack.Files;
