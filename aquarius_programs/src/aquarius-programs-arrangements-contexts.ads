with Ada.Strings.Unbounded;

with Aquarius.Layout;                  use Aquarius.Layout;
with Aquarius.Formats;                 use Aquarius.Formats;

private package Aquarius.Programs.Arrangements.Contexts is

   type Arrangement_Context is
      record
         Need_New_Line      : Boolean             := False;
         Need_Soft_New_Line : Boolean             := False;
         New_Line_Priority  : Rule_Priority       := 1;
         Got_New_Line       : Boolean             := False;
         First_On_Line      : Boolean             := True;
         First_Terminal     : Program_Tree;
         Previous_Terminal  : Program_Tree;
         Need_Space         : Boolean             := False;
         No_Space           : Boolean             := False;
         Cancel_Indent      : Boolean             := False;
         Message_Level      : Aquarius.Messages.Message_Level :=
                                Aquarius.Messages.No_Message;
         Space_Priority     : Rule_Priority       := 1;
         Current_Line       : Line_Number         := 1;
         Current_Column     : Column_Number       := 1;
         Current_Position   : Position            := 0;
         Current_Indent     : Column_Number       := 1;
         Previous_Indent    : Column_Number       := 1;
         Soft_Indent        : Column_Offset       := 0;
         Right_Margin       : Column_Number       := 72;
         Rearranging        : Boolean             := False;
         Updating           : Boolean             := False;
         Skip_Terminal      : Boolean             := False;
         User_Text          : Ada.Strings.Unbounded.Unbounded_String;
         User_Text_Length   : Count               := 0;
         User_Cursor        : Aquarius.Trees.Cursors.Cursor;
         User_Text_Line     : Aquarius.Layout.Line_Number;
         User_Text_Column   : Aquarius.Layout.Column_Number;
         Logging            : Aquarius.Messages.Message_List;
         Stop_Tree          : Program_Tree        := null;
         Stopped            : Boolean             := False;
      end record;

end Aquarius.Programs.Arrangements.Contexts;
