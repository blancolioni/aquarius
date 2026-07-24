private with Ada.Strings.Unbounded;

with Aquarius.Commands;

--  Text-shaped models: a sequence of lines of plain text. A viewer that wants
--  to display text requires a Text_Model_Interface.

package Aquarius.Models.Text is

   type Text_Model_Interface is interface and Model_Interface;

   function Line_Count
     (Model : Text_Model_Interface) return Natural is abstract;

   function Line
     (Model : Text_Model_Interface;
      Index : Positive)
      return String
      is abstract;

   function Text (Model : Text_Model_Interface) return String is abstract;
   --  Whole content as one string (lines joined by LF).

   --  A ready-to-use concrete text model whose only mutator is command-based,
   --  so all changes are undoable.
   type Simple_Text_Model is
     new Publisher_Base and Text_Model_Interface
   with private;

   type Simple_Text_Model_Access is access all Simple_Text_Model'Class;

   function Create (Initial : String := "") return Simple_Text_Model_Access;

   function Set_Text_Command
     (Model    : not null Simple_Text_Model_Access;
      New_Text : String)
      return Aquarius.Commands.Command_Reference;
   --  Command that replaces the whole text (Undo restores the previous text
   --  and notifies subscribers). Perform it on a Command_Manager.

private

   use Ada.Strings.Unbounded;

   type Simple_Text_Model is
     new Publisher_Base and Text_Model_Interface with record
      Content : Unbounded_String;
   end record;

   overriding function Kind (Model : Simple_Text_Model) return String;
   overriding function Default_Viewer
     (Model : Simple_Text_Model) return String;
   overriding function Line_Count (Model : Simple_Text_Model) return Natural;
   overriding function Line
     (Model : Simple_Text_Model; Index : Positive) return String;
   overriding function Text (Model : Simple_Text_Model) return String;

   type Set_Text_Command_Type is new Aquarius.Commands.Command_Interface with
      record
         Model    : Simple_Text_Model_Access;
         New_Text : Unbounded_String;
         Old_Text : Unbounded_String;
      end record;

   overriding function Description
     (Command : Set_Text_Command_Type) return String;
   overriding procedure Execute (Command : in out Set_Text_Command_Type);
   overriding procedure Undo (Command : in out Set_Text_Command_Type);

end Aquarius.Models.Text;
