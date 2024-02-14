with Ada.Text_IO;

package body Aquarius.Errors is

   procedure Error
     (Level       : Aquarius.Messages.Message_Level;
      Location    : access Aquarius.Messages.Message_Location'Class;
      Reference   : access Aquarius.Messages.Message_Location'Class;
      Message     : String;
      Ref_Message : String);

   -----------
   -- Error --
   -----------

   procedure Error
     (Location : access Aquarius.Messages.Message_Location'Class;
      Message  : String)
   is
   begin
      Error (Aquarius.Messages.Error, Location, null, Message, "");
   end Error;

   -----------
   -- Error --
   -----------

   procedure Error
     (Location    : access Aquarius.Messages.Message_Location'Class;
      Reference   : access Aquarius.Messages.Message_Location'Class;
      Message     : String;
      Ref_Message : String := "")
   is
   begin
      Error (Aquarius.Messages.Error,
             Location, Reference, Message, Ref_Message);
   end Error;

   -----------
   -- Error --
   -----------

   procedure Error
     (Level       : Aquarius.Messages.Message_Level;
      Location    : access Aquarius.Messages.Message_Location'Class;
      Reference   : access Aquarius.Messages.Message_Location'Class;
      Message     : String;
      Ref_Message : String)
   is
      use type Aquarius.Messages.Message_Level;
      Msg : constant Aquarius.Messages.Message :=
        Aquarius.Messages.New_Message (Level, Location, Message,
                                       Reference, Ref_Message);
   begin
      if Location = null then
         Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
                               Aquarius.Messages.Show (Msg));
         if Level > Aquarius.Messages.Warning then
            raise Internal_Error;
         end if;
      else
         Location.Attach_Message (Msg);
      end if;
   end Error;

   ------------
   -- Report --
   ------------

   procedure Report
     (Location : access Aquarius.Messages.Message_Location'Class;
      Message  : String)
   is
   begin
      Error (Aquarius.Messages.Informational,
             Location, null, Message, "");
   end Report;

   -------------
   -- Warning --
   -------------

   procedure Warning
     (Location : access Aquarius.Messages.Message_Location'Class;
      Message  : String)
   is
   begin
      Error (Aquarius.Messages.Warning, Location, null, Message, "");
   end Warning;

   -------------
   -- Warning --
   -------------

   procedure Warning
     (Location    : access Aquarius.Messages.Message_Location'Class;
      Reference   : access Aquarius.Messages.Message_Location'Class;
      Message     : String;
      Ref_Message : String := "")
   is
   begin
      Error (Aquarius.Messages.Warning,
             Location, Reference, Message, Ref_Message);
   end Warning;

end Aquarius.Errors;
