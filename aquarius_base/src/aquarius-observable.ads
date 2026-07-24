private with Ada.Containers.Vectors;

--  A minimal observer/observable (publish/subscribe) framework. Publishers
--  notify their subscribers of updates; a subscriber reacts in Update.

package Aquarius.Observable is

   type Update_Data_Interface is interface;

   function No_Update_Data return Update_Data_Interface'Class;

   type Subscriber_Interface is interface;

   procedure Update
     (This : in out Subscriber_Interface;
      Data : Update_Data_Interface'Class)
   is abstract;

   type Subscriber_Reference is access all Subscriber_Interface'Class;

   type Publisher_Interface is interface;

   procedure Add_Subscriber
     (This       : in out Publisher_Interface;
      Subscriber : not null Subscriber_Reference)
   is abstract;

   procedure Remove_Subscriber
     (This       : in out Publisher_Interface;
      Subscriber : not null Subscriber_Reference)
   is abstract;

   procedure Notify
     (This : Publisher_Interface;
      Data : Update_Data_Interface'Class)
   is abstract;

   --  A ready-to-use publisher that manages its own subscriber list. Concrete
   --  observable types can derive from this to avoid re-implementing the
   --  subscription machinery.
   type Publisher_Base is new Publisher_Interface with private;

   overriding procedure Add_Subscriber
     (This       : in out Publisher_Base;
      Subscriber : not null Subscriber_Reference);

   overriding procedure Remove_Subscriber
     (This       : in out Publisher_Base;
      Subscriber : not null Subscriber_Reference);

   overriding procedure Notify
     (This : Publisher_Base;
      Data : Update_Data_Interface'Class);

private

   package Subscriber_Vectors is
     new Ada.Containers.Vectors (Positive, Subscriber_Reference);

   type Publisher_Base is new Publisher_Interface with record
      Subscribers : Subscriber_Vectors.Vector;
   end record;

end Aquarius.Observable;
