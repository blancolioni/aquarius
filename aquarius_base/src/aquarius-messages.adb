with Ada.Strings.Fixed;

package body Aquarius.Messages is

   function Before (Left, Right : Message) return Boolean;

   package Message_Sorting is
      new Message_Vector.Generic_Sorting (Before);

   -----------------
   -- Add_Message --
   -----------------

   procedure Add_Message (To    : in out Message_List;
                          Item  :      Message)
   is
   begin
      To.Messages.Append (Item);
      if To.Sorted then
         Message_Sorting.Sort (To.Messages);
      end if;
   end Add_Message;

   -------------------
   -- Add_Reference --
   -------------------

   procedure Add_Reference (To         : Message;
                            Reference  : access Message_Location'Class;
                            Message    : String)
   is
      Ref : constant Message_Reference :=
              (Reference, Ada.Strings.Unbounded.To_Unbounded_String (Message));
   begin
      To.References.Append (Ref);
   end Add_Reference;

   ------------
   -- Before --
   ------------

   function Before (Left, Right : Message) return Boolean is
   begin
      return Left.Location.Before (Right.Location);
   end Before;

   ------------------------
   -- Clear_Message_List --
   ------------------------

   procedure Clear_Message_List (List : in out Message_List) is
   begin
      List.Messages.Clear;
   end Clear_Message_List;

   -----------------------
   -- Copy_Message_List --
   -----------------------

   procedure Copy_Message_List (From : Message_List;
                                To   : in out Message_List)
   is
   begin
      To.Messages.Append (From.Messages);
      if To.Sorted then
         Message_Sorting.Sort (To.Messages);
      end if;
   end Copy_Message_List;

   -------------------------
   -- Create_Message_List --
   -------------------------

   procedure Create_Message_List (List   : in out Message_List;
                                  Sorted : Boolean)
   is
   begin
      List.Messages.Clear;
      List.Sorted := Sorted;
   end Create_Message_List;

   ------------------------
   -- Empty_Message_List --
   ------------------------

   function Empty_Message_List (Sorted : Boolean) return Message_List is
      Result : Message_List;
   begin
      Result.Sorted := Sorted;
      return Result;
   end Empty_Message_List;

   ------------------
   -- Get_Location --
   ------------------

   function Get_Location (Item : Message)
                         return access Message_Location'Class
   is
   begin
      return Item.Location;
   end Get_Location;

   -----------------
   -- Get_Message --
   -----------------

   function Get_Message   (List  : Message_List;
                           Index : Positive)
                          return Message
   is
   begin
      return List.Messages.Element (Index);
   end Get_Message;

   ----------------------
   -- Get_Message_Text --
   ----------------------

   function Get_Message_Text
     (Item : Message)
      return String
   is
   begin
      return Ada.Strings.Unbounded.To_String (Item.Text);
   end Get_Message_Text;

   -------------------
   -- Highest_Level --
   -------------------

   function Highest_Level (List : Message_List) return Message_Level is
      Highest : Message_Level := Message_Level'First;
   begin
      for I in 1 .. List.Messages.Last_Index loop
         if List.Messages.Element (I).Level > Highest then
            Highest := List.Messages.Element (I).Level;
            exit when Highest = Message_Level'Last;
         end if;
      end loop;
      return Highest;
   end Highest_Level;

   ----------------
   -- Level_Name --
   ----------------

   function Level_Name (Level : Message_Level) return String is
   begin
      case Level is
         when No_Message =>
            return "";
         when Informational =>
            return "information";
         when Style =>
            return "style";
         when Warning =>
            return "warning";
         when Error =>
            return "error";
         when Fatal_Error =>
            return "fatal error";
         when Internal_Error =>
            return "internal error";
      end case;
   end Level_Name;

   ------------------
   -- Level_Prefix --
   ------------------

   function Level_Prefix (Level : Message_Level) return String is
   begin
      case Level is
         when No_Message =>
            return "";
         when Informational =>
            return "";
         when Style =>
            return "style: ";
         when Warning =>
            return "warning: ";
         when Error =>
            return "";
         when Fatal_Error =>
            return "fatal error: ";
         when Internal_Error =>
            return "internal error: ";
      end case;
   end Level_Prefix;

   -------------------
   -- Message_Count --
   -------------------

   function Message_Count (List : Message_List) return Natural is
   begin
      return List.Messages.Last_Index;
   end Message_Count;

   -----------------
   -- New_Message --
   -----------------

   function New_Message (Level    : Message_Level;
                         Location : access Message_Location'Class;
                         Text     : String)
                        return Message
   is
      Result : constant Message := new Message_Record'
        (Level      => Level,
         Location   => Location,
         Text       => Ada.Strings.Unbounded.To_Unbounded_String (Text),
         References => Message_Reference_Vectors.Empty_Vector);
   begin
      return Result;
   end New_Message;

   -----------------
   -- New_Message --
   -----------------

   function New_Message (Level         : Message_Level;
                         Location      : access Message_Location'Class;
                         Text          : String;
                         Reference     : access Message_Location'Class;
                         Reference_Msg : String)
                        return Message
   is
      Result : constant Message := New_Message (Level, Location, Text);
   begin
      if Reference /= null then
         Add_Reference (Result, Reference, Reference_Msg);
      end if;
      return Result;
   end New_Message;

   ---------------------
   -- Reference_Count --
   ---------------------

   function Reference_Count (Item : Message) return Natural is
   begin
      return Item.References.Last_Index;
   end Reference_Count;

   ----------
   -- Show --
   ----------

   function Show (Item : Message) return String is
      Text : constant String :=
               Ada.Strings.Unbounded.To_String (Item.Text);
   begin
      if Item.Location /= null then
         return Item.Location.Show_Location & ": " & Text;
      else
         return Level_Prefix (Item.Level) & Text;
      end if;
   end Show;

   -------------------
   -- Show_Location --
   -------------------

   function Show_Location (Location : Message_Location'Class)
                          return String
   is
      Line_Img : constant String :=
        Ada.Strings.Fixed.Trim (Positive'Image (Location.Location_Line),
                                Ada.Strings.Left);
      Col_Img : constant String :=
        Ada.Strings.Fixed.Trim (Positive'Image (Location.Location_Column),
                                Ada.Strings.Left);
   begin
      return Location_Name (Location) & ":" & Line_Img & ":" & Col_Img;
   end Show_Location;

   --------------------
   -- Show_Reference --
   --------------------

   function Show_Reference
     (Item      : Message;
      Ref_Index : Positive)
      return String
   is
      Ref : constant Message_Reference := Item.References.Element (Ref_Index);
      Text : constant String :=
               Ada.Strings.Unbounded.To_String (Ref.Text);
      Loc  : constant String :=
               (if Ref.Reference = null
                then "(built-in)"
                else Ref.Reference.Show_Location);
   begin
      return Loc
        & ": " & Level_Prefix (Item.Level)
        & Text;
   end Show_Reference;

end Aquarius.Messages;
