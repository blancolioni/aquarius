with Aquarius.Observable;

package body Aquarius.UI.Gtk_Views is

   use type Aquarius.Models.Model_Reference;

   Open_Target_Handler : Open_Target_Callback := null;

   ---------------------
   -- Set_Open_Target --
   ---------------------

   procedure Set_Open_Target (Callback : Open_Target_Callback) is
   begin
      Open_Target_Handler := Callback;
   end Set_Open_Target;

   -----------------
   -- Open_Target --
   -----------------

   function Open_Target return Open_Target_Callback is
   begin
      return Open_Target_Handler;
   end Open_Target;

   ---------------
   -- Set_Model --
   ---------------

   overriding procedure Set_Model
     (View  : not null access Gtk_View_Base;
      Model : Aquarius.Models.Model_Reference)
   is
   begin
      View.Model_Ref := Model;
      if Model /= null then
         Model.Add_Subscriber
           (Aquarius.Observable.Subscriber_Reference (View));
         --  Initial render.
         Gtk_View_Base'Class (View.all).Update
           (Aquarius.Observable.No_Update_Data);
      end if;
   end Set_Model;

   -----------
   -- Model --
   -----------

   overriding function Model
     (View : Gtk_View_Base) return Aquarius.Models.Model_Reference
   is (View.Model_Ref);

end Aquarius.UI.Gtk_Views;
