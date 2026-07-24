package body Aquarius.Commands is

   -------------
   -- Perform --
   -------------

   procedure Perform
     (Manager : in out Command_Manager;
      Command : Command_Reference)
   is
   begin
      Command.Execute;
      Manager.Done.Append (Command);
      Manager.Undone.Clear;
   end Perform;

   ----------
   -- Undo --
   ----------

   procedure Undo (Manager : in out Command_Manager) is
      Command : Command_Reference;
   begin
      if not Manager.Done.Is_Empty then
         Command := Manager.Done.Last_Element;
         Manager.Done.Delete_Last;
         Command.Undo;
         Manager.Undone.Append (Command);
      end if;
   end Undo;

   ----------
   -- Redo --
   ----------

   procedure Redo (Manager : in out Command_Manager) is
      Command : Command_Reference;
   begin
      if not Manager.Undone.Is_Empty then
         Command := Manager.Undone.Last_Element;
         Manager.Undone.Delete_Last;
         Command.Execute;
         Manager.Done.Append (Command);
      end if;
   end Redo;

   --------------
   -- Can_Undo --
   --------------

   function Can_Undo (Manager : Command_Manager) return Boolean is
     (not Manager.Done.Is_Empty);

   --------------
   -- Can_Redo --
   --------------

   function Can_Redo (Manager : Command_Manager) return Boolean is
     (not Manager.Undone.Is_Empty);

   -----------
   -- Clear --
   -----------

   procedure Clear (Manager : in out Command_Manager) is
   begin
      Manager.Done.Clear;
      Manager.Undone.Clear;
   end Clear;

end Aquarius.Commands;
