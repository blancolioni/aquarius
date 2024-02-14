with Aquarius.Messages;

package Aquarius.Errors is

   --  Error procedures in this package attach the error to the given
   --  location if it is not null, otherwise they write the message
   --  to Standard_Error and exit Aquarius.  This is to avoid cluttering
   --  the code with errors from the internal bootstrap execution, which
   --  should never produce any

   Internal_Error : exception;

   procedure Error
     (Location : access Aquarius.Messages.Message_Location'Class;
      Message  : String);

   procedure Error
     (Location    : access Aquarius.Messages.Message_Location'Class;
      Reference   : access Aquarius.Messages.Message_Location'Class;
      Message     : String;
      Ref_Message : String := "");

   procedure Warning
     (Location : access Aquarius.Messages.Message_Location'Class;
      Message  : String);

   procedure Warning
     (Location    : access Aquarius.Messages.Message_Location'Class;
      Reference   : access Aquarius.Messages.Message_Location'Class;
      Message     : String;
      Ref_Message : String := "");

   --  Report: an informational error report; not necessarily fatal
   procedure Report
     (Location : access Aquarius.Messages.Message_Location'Class;
      Message  : String);

end Aquarius.Errors;
