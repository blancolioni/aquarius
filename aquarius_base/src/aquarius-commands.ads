private with Ada.Containers.Vectors;

--  Undo/redo command framework. Every mutation of a model must be expressed as
--  a Command and performed through a Command_Manager, so that it can be undone
--  and redone.

package Aquarius.Commands is

   type Command_Interface is interface;

   function Description
     (Command : Command_Interface) return String is abstract;
   --  Short human-readable description, e.g. for an undo menu ("Undo <desc>").

   procedure Execute (Command : in out Command_Interface) is abstract;
   procedure Undo    (Command : in out Command_Interface) is abstract;

   type Command_Reference is access all Command_Interface'Class;

   --  A per-session (or per-model) undo/redo stack.
   type Command_Manager is tagged limited private;

   procedure Perform
     (Manager : in out Command_Manager;
      Command : Command_Reference);
   --  Execute Command and push it onto the undo stack. Clears the redo stack.

   procedure Undo (Manager : in out Command_Manager);
   procedure Redo (Manager : in out Command_Manager);

   function Can_Undo (Manager : Command_Manager) return Boolean;
   function Can_Redo (Manager : Command_Manager) return Boolean;

   procedure Clear (Manager : in out Command_Manager);

private

   package Command_Vectors is
     new Ada.Containers.Vectors (Positive, Command_Reference);

   type Command_Manager is tagged limited record
      Done   : Command_Vectors.Vector;   --  undo stack (last = most recent)
      Undone : Command_Vectors.Vector;   --  redo stack
   end record;

end Aquarius.Commands;
