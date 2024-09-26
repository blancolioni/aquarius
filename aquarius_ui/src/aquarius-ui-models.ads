private with Ada.Strings.Unbounded;
private with Aquarius.Trees.Cursors;

with Aquarius.Programs;
with Aquarius.UI.Editor;

private package Aquarius.UI.Models is

   function Create_Model
     (Program : Aquarius.Programs.Program_Tree)
      return Aquarius.UI.Editor.Model_Reference;

private

   subtype Text is Ada.Strings.Unbounded.Unbounded_String;

   function "-" (U : Ada.Strings.Unbounded.Unbounded_String)
                 return String
                 renames Ada.Strings.Unbounded.To_String;

   type Instance is new Aquarius.UI.Editor.Model_Interface with
      record
         Program : Aquarius.Programs.Program_Tree;
         Point   : Aquarius.Trees.Cursors.Cursor;
         Partial : Text;
      end record;

   type Reference is access all Instance'Class;

   overriding function Insert_Character_At_Point
     (This : not null access Instance;
      Ch   : Character)
      return Command_Interface'Class;

   overriding function Contents
     (This : Instance)
      return String;

   type Model_Command_Instance is abstract new Command_Interface with
      record
         Model : Reference;
      end record;

end Aquarius.UI.Models;
