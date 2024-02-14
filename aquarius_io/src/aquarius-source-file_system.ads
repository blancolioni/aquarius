private with Ada.Text_IO;
private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Strings.Unbounded;
private with Aquarius.Paths;

package Aquarius.Source.File_System is

   type Search_Path_List is tagged private;

   function Current_Directory return Search_Path_List;
   procedure Add_Path (List : in out Search_Path_List;
                       Path : String);

   procedure Set_Base_Path (List : in out Search_Path_List;
                            Path : String);

--     procedure Add_Path (List  : in out Search_Path_List;
--                         Start : in     String;
--                         Path  : in     String);

   function Read_File (File_Path   : String;
                       Search_Path : Search_Path_List;
                       Extension   : String            := "")
                      return Source_File;

   function Find_File (Search_Path : Search_Path_List;
                       File_Name   : String)
                      return String;

   function Read_File (File_Path   : String)
                      return Source_File;

private

   type File_System_File is limited new Source_File_Record with
      record
         File_Name : Ada.Strings.Unbounded.Unbounded_String;
         Full_Path : Ada.Strings.Unbounded.Unbounded_String;
         File      : Ada.Text_IO.File_Type;
      end record;

   overriding
   function Get_File_Name (File : access File_System_File) return String;

   overriding
   function Get_Full_Path (File : access File_System_File) return String;

   overriding
   procedure Next_Line (File : access File_System_File);

   overriding
   function End_Of_File (File : access File_System_File) return Boolean;

   overriding
   procedure Close (Item : access File_System_File);

   package Path_Lists is
     new Ada.Containers.Doubly_Linked_Lists
       (Aquarius.Paths.Aquarius_Path,
        Aquarius.Paths."=");

   type Search_Path_List is tagged
      record
         Base_Path : Aquarius.Paths.Aquarius_Path;
         Paths     : Path_Lists.List;
      end record;

end Aquarius.Source.File_System;
