with Aquarius.Trees;

package Aquarius.Interaction is

   type Interactor is interface and Root_Aquarius_Object;

   procedure Update
     (Item  : in out Interactor;
      Start : not null access Aquarius.Trees.Root_Tree_Type'Class)
      is abstract;

   type Interactor_Access is access all Interactor'Class;

end Aquarius.Interaction;
