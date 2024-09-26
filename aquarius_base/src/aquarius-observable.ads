package Aquarius.Observable is

   type Update_Data_Interface is interface;

   function No_Update_Data return Update_Data_Interface'Class;

   type Subscriber_Interface is interface;

   type Publisher_Interface is interface;

   procedure Update
     (This : in out Subscriber_Interface;
      Data : Update_Data_Interface'Class)
   is abstract;

   procedure Subscribe
     (This     : in out Subscriber_Interface;
      Observer : not null access Publisher_Interface'Class)
   is abstract;

   procedure Unsubscribe
     (This     : in out Subscriber_Interface;
      Observer : not null access Publisher_Interface'Class)
   is abstract;

   procedure Notify
     (This : Publisher_Interface;
      Data : Update_Data_Interface'Class)
   is abstract;

end Aquarius.Observable;
