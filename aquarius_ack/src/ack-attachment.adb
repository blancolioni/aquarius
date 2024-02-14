with Ada.Text_IO;

package body Ack.Attachment is

   Trace_Attachment : constant Boolean := False;

   ------------
   -- Attach --
   ------------

   procedure Attach
     (Context : in out Attachment_Context'Class;
      Entity  : not null access constant Root_Entity_Type'Class)
   is
   begin
      if Trace_Attachment then
         if Context.Attached.Is_Empty then
            Ada.Text_IO.Put_Line
              ("empty attachment context");
         end if;
         Ada.Text_IO.Put_Line
           ("attaching: " & Entity.Qualified_Name);
      end if;

      Context.Attached.Include (Entity.Qualified_Name);
   end Attach;

   ----------------------------
   -- Attach_Current_Context --
   ----------------------------

   procedure Attach_Current_Context
     (Context : in out Attachment_Context'Class)
   is
   begin
      if Trace_Attachment then
         if Context.Attached.Is_Empty then
            Ada.Text_IO.Put_Line
              ("empty attachment context");
         end if;
         Ada.Text_IO.Put_Line
           ("attaching current context");
      end if;

      Context.Current := True;
   end Attach_Current_Context;

   -----------
   -- Clear --
   -----------

   procedure Clear (Context : in out Attachment_Context'Class) is
   begin
      if Trace_Attachment then
         declare
            procedure Trace_Detach (Name : String);

            ------------------
            -- Trace_Detach --
            ------------------

            procedure Trace_Detach (Name : String) is
            begin
               Ada.Text_IO.Put_Line ("clear: detaching: " & Name);
            end Trace_Detach;

         begin
            Context.Attached.Iterate (Trace_Detach'Access);
         end;
      end if;
      Context.Attached.Clear;
      Context.Current := False;
   end Clear;

   ------------
   -- Detach --
   ------------

   procedure Detach
     (Context : in out Attachment_Context'Class;
      Entity  : not null access constant Root_Entity_Type'Class)
   is
   begin
      if Trace_Attachment then
         Ada.Text_IO.Put_Line ("detaching: " & Entity.Qualified_Name);
      end if;

      Context.Attached.Delete (Entity.Qualified_Name);
   end Detach;

   ----------------------------
   -- Detach_Current_Context --
   ----------------------------

   procedure Detach_Current_Context
     (Context : in out Attachment_Context'Class)
   is
   begin
      if Trace_Attachment then
         Ada.Text_IO.Put_Line ("detaching current context");
      end if;
      Context.Current := False;
   end Detach_Current_Context;

   -----------------
   -- Is_Attached --
   -----------------

   function Is_Attached
     (Context : Attachment_Context'Class;
      Entity  : not null access constant Root_Entity_Type'Class)
      return Boolean
   is
   begin
      return Context.Attached.Contains (Entity.Qualified_Name);
   end Is_Attached;

   ---------------------------------
   -- Is_Current_Context_Attached --
   ---------------------------------

   function Is_Current_Context_Attached
     (Context : Attachment_Context'Class)
      return Boolean
   is
   begin
      return Context.Current;
   end Is_Current_Context_Attached;

   -------------------
   -- Restore_State --
   -------------------

   procedure Restore_State (Context : in out Attachment_Context'Class) is
   begin
      if Trace_Attachment then
         Ada.Text_IO.Put_Line ("restore state");
      end if;
      Context.Attached := Context.Stack.First_Element;
      Context.Stack.Delete_First;
   end Restore_State;

   ----------------
   -- Save_State --
   ----------------

   procedure Save_State (Context : in out Attachment_Context'Class) is
   begin
      if Trace_Attachment then
         Ada.Text_IO.Put_Line ("save state");
      end if;
      Context.Stack.Insert (Context.Stack.First, Context.Attached);
   end Save_State;

   ------------------------------------
   -- Set_Current_Context_Attachment --
   ------------------------------------

   procedure Set_Current_Context_Attachment
     (Context : in out Attachment_Context'Class;
      From    : not null access constant Root_Entity_Type'Class)
   is
   begin
      if Trace_Attachment then
         Ada.Text_IO.Put_Line
           ((if Context.Is_Attached (From)
            then "attaching " else "detaching ")
            & "current context from " & From.Qualified_Name);
      end if;
      Context.Current := Context.Is_Attached (From);
   end Set_Current_Context_Attachment;

   -------------------------
   -- Transfer_Attachment --
   -------------------------

   procedure Transfer_Attachment
     (Context : in out Attachment_Context'Class;
      From    : not null access constant Root_Entity_Type'Class;
      To      : not null access constant Root_Entity_Type'Class)
   is
   begin
      if Trace_Attachment then
         Ada.Text_IO.Put_Line
           ((if Context.Is_Attached (From)
            then "attaching " else "detaching ")
            & To.Qualified_Name & " from context from " & From.Qualified_Name);
      end if;
      if Context.Is_Attached (From) then
         Context.Attached.Include (To.Qualified_Name);
      else
         Context.Attached.Delete (To.Qualified_Name);
      end if;
   end Transfer_Attachment;

   -----------------------------------------
   -- Transfer_Current_Context_Attachment --
   -----------------------------------------

   procedure Transfer_Current_Context_Attachment
     (Context : in out Attachment_Context'Class;
      To      : not null access constant Root_Entity_Type'Class)
   is
   begin
      if Trace_Attachment then
         Ada.Text_IO.Put_Line
           ((if Context.Current
            then "attaching " else "detaching ")
            & To.Qualified_Name & " from current context");
      end if;
      if Context.Current then
         Context.Attached.Include (To.Qualified_Name);
      else
         Context.Attached.Delete (To.Qualified_Name);
      end if;
   end Transfer_Current_Context_Attachment;

end Ack.Attachment;
