private with Ada.Strings.Unbounded;
private with Aquarius.Programs.Parser;
private with Aquarius.Trees.Cursors;

with Aquarius.Grammars;
with Aquarius.Programs;
with Aquarius.UI.Editor;

private package Aquarius.UI.Models is

   function Create_Model
     (Grammar : Aquarius.Grammars.Aquarius_Grammar;
      Program : Aquarius.Programs.Program_Tree)
      return Aquarius.UI.Editor.Model_Reference;

private

   subtype Text is Ada.Strings.Unbounded.Unbounded_String;

   function "-" (U : Ada.Strings.Unbounded.Unbounded_String)
                 return String
                 renames Ada.Strings.Unbounded.To_String;

   type Instance is new Aquarius.UI.Editor.Model_Interface with
      record
         Grammar : Aquarius.Grammars.Aquarius_Grammar;
         Program : Aquarius.Programs.Program_Tree;
         Point   : Aquarius.Trees.Cursors.Cursor;
         Partial : Text;
         Context : Aquarius.Programs.Parser.Parse_Context;
      end record;

   type Reference is access all Instance'Class;

   overriding function Insert_Character_At_Point
     (This : not null access Instance;
      Ch   : Character)
      return Command_Interface'Class;

   overriding function Contents
     (This : Instance)
      return String;

   procedure Read_Partial
     (This    : in out Instance'Class;
      Leaving : Boolean);

   type Model_Command_Instance is abstract new Command_Interface with
      record
         Model : Reference;
      end record;

end Aquarius.UI.Models;
