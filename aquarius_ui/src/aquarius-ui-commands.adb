with Ada.Containers.Indefinite_Doubly_Linked_Lists;

package body Aquarius.UI.Commands is

   package Command_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists
       (Command_Interface'Class);

   type Instance is new Command_Manager_Interface with
      record
         Command_List : Command_Lists.List   := [];
         Current      : Command_Lists.Cursor := Command_Lists.No_Element;
      end record;

   overriding procedure Append_Command
     (This    : in out Instance;
      Command : Command_Interface'Class);

   overriding procedure Next_Command
     (This    : in out Instance);

   overriding procedure Previous_Command
     (This    : in out Instance);

   function Create_Command_Manager return Command_Manager_Interface'Class
   is (Instance'(others => <>));

   --------------------
   -- Append_Command --
   --------------------

   overriding procedure Append_Command
     (This    : in out Instance;
      Command : Command_Interface'Class)
   is
   begin
      This.Command_List.Append (Command);
   end Append_Command;

   ------------------
   -- Next_Command --
   ------------------

   overriding procedure Next_Command
     (This    : in out Instance)
   is
   begin
      if not Command_Lists.Has_Element (This.Current) then
         This.Current := This.Command_List.First;
      else
         Command_Lists.Next (This.Current);
      end if;

      This.Command_List (This.Current).Execute;
   end Next_Command;

   ----------------------
   -- Previous_Command --
   ----------------------

   overriding procedure Previous_Command
     (This    : in out Instance)
   is
   begin
      This.Command_List (This.Current).Undo;
      Command_Lists.Previous (This.Current);
   end Previous_Command;

end Aquarius.UI.Commands;
