package Aquarius is

   pragma Pure (Aquarius);

   type Root_Aquarius_Object is interface;

   function Name (Item : Root_Aquarius_Object) return String
      is abstract;

   --  Whether to apply a rule before or after a node
   type Rule_Position is (Before, After);

   --  Support for updating dependent objects automatically
   type Watcher is interface;

   procedure Object_Changed
     (W       : in out Watcher;
      Item    : not null access Root_Aquarius_Object'Class;
      Context : not null access Root_Aquarius_Object'Class)
   is null;

   procedure Object_Removed
     (W       : in out Watcher;
      Item    : not null access Root_Aquarius_Object'Class;
      Context : not null access Root_Aquarius_Object'Class)
   is null;

end Aquarius;
