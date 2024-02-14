package body Aquarius.Interaction.Console is

   type Console_Interactor_Type is
     new Interactor with null record;

   type Console_Interactor_Access is access all Console_Interactor_Type'Class;

   overriding
   function Name
     (Item  : Console_Interactor_Type)
     return String;

   overriding
   procedure Update
     (Item  : in out Console_Interactor_Type;
      Start : not null access Aquarius.Trees.Root_Tree_Type'Class)
      is null;

   Local_Console_Interactor : Console_Interactor_Access := null;

   function Console_Interactor
     return access Interactor'Class
   is
   begin
      if Local_Console_Interactor = null then
         Local_Console_Interactor := new Console_Interactor_Type;
      end if;
      return Local_Console_Interactor;
   end Console_Interactor;

   ----------
   -- Name --
   ----------

   overriding
   function Name
     (Item  : Console_Interactor_Type)
      return String
   is
      pragma Unreferenced (Item);
   begin
      return "Console interactor";
   end Name;

end Aquarius.Interaction.Console;
