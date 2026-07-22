package body Aquarius.Observable is

   type No_Update_Data_Instance is
     new Update_Data_Interface with null record;

   function No_Update_Data return Update_Data_Interface'Class
   is (No_Update_Data_Instance'(null record));

   --------------------
   -- Add_Subscriber --
   --------------------

   overriding procedure Add_Subscriber
     (This       : in out Publisher_Base;
      Subscriber : not null Subscriber_Reference)
   is
   begin
      This.Subscribers.Append (Subscriber);
   end Add_Subscriber;

   -----------------------
   -- Remove_Subscriber --
   -----------------------

   overriding procedure Remove_Subscriber
     (This       : in out Publisher_Base;
      Subscriber : not null Subscriber_Reference)
   is
      use Subscriber_Vectors;
      Position : Cursor := This.Subscribers.Find (Subscriber);
   begin
      if Has_Element (Position) then
         This.Subscribers.Delete (Position);
      end if;
   end Remove_Subscriber;

   ------------
   -- Notify --
   ------------

   overriding procedure Notify
     (This : Publisher_Base;
      Data : Update_Data_Interface'Class)
   is
   begin
      for Subscriber of This.Subscribers loop
         Subscriber.Update (Data);
      end loop;
   end Notify;

end Aquarius.Observable;
