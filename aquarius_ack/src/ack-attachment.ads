private with WL.String_Sets;

package Ack.Attachment is

   type Attachment_Context is tagged private;

   procedure Save_State (Context : in out Attachment_Context'Class);
   procedure Restore_State (Context : in out Attachment_Context'Class);

   procedure Attach
     (Context : in out Attachment_Context'Class;
      Entity  : not null access constant Root_Entity_Type'Class);

   procedure Detach
     (Context : in out Attachment_Context'Class;
      Entity  : not null access constant Root_Entity_Type'Class);

   procedure Attach_Current_Context
     (Context : in out Attachment_Context'Class);

   procedure Detach_Current_Context
     (Context : in out Attachment_Context'Class);

   procedure Transfer_Attachment
     (Context : in out Attachment_Context'Class;
      From    : not null access constant Root_Entity_Type'Class;
      To      : not null access constant Root_Entity_Type'Class);

   procedure Transfer_Current_Context_Attachment
     (Context : in out Attachment_Context'Class;
      To      : not null access constant Root_Entity_Type'Class);

   procedure Set_Current_Context_Attachment
     (Context : in out Attachment_Context'Class;
      From    : not null access constant Root_Entity_Type'Class);

   procedure Clear (Context : in out Attachment_Context'Class);

   function Is_Attached
     (Context : Attachment_Context'Class;
      Entity  : not null access constant Root_Entity_Type'Class)
      return Boolean;

   function Is_Current_Context_Attached
     (Context : Attachment_Context'Class)
      return Boolean;

private

   package List_Of_Sets is
     new Ada.Containers.Doubly_Linked_Lists (WL.String_Sets.Set,
                                             WL.String_Sets."=");

   type Attachment_Context is tagged
      record
         Attached : WL.String_Sets.Set;
         Current  : Boolean := False;
         Stack    : List_Of_Sets.List;
      end record;

end Ack.Attachment;
