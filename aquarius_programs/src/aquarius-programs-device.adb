with Ada.Text_IO;
with Aqua.Images;

package body Aquarius.Programs.Device is

   Trace_Properties : constant Boolean := False;

   type Driver_Command is
     (No_Command,
      Get_Name,
      Get_Text,
      Get_Standard_Text,
      Concatenated_Image,
      Error,
      Get_Property,
      Has_Property,
      Set_Property,
      Full_File_Name,
      File_Name,
      Start_Line,
      End_Line,
      Start_Column,
      End_Column,
      Start_Position,
      End_Position,
      Create_Entity,
      Find_Entity,
      Cross_Reference);

   function Is_Command (Value : Natural) return Boolean
   is (Value <= Driver_Command'Pos (Driver_Command'Last));

   --------------------------
   -- Aquarius_Tree_Driver --
   --------------------------

   function Aquarius_Tree_Driver
     return Aquarius.Devices.Reference
   is
   begin
      return new Aquarius_Tree_Driver_Record;
   end Aquarius_Tree_Driver;

   -----------------
   -- Read_String --
   -----------------

   function Read_String
     (Driver : Aquarius_Tree_Driver_Record'Class)
      return String
   is
      Length : constant Natural := Natural (Driver.Rs (R_String_Len));
      Result : String (1 .. Length);
      Index  : Register_Index := R_String;
   begin
      for Ch of Result loop
         Ch := Character'Val (Driver.Rs (Index));
         Index := Index + 1;
      end loop;
      return Result;
   end Read_String;

   -----------------
   -- Get_Word_32 --
   -----------------

   overriding procedure Get_Word_32
     (This    : in out Aquarius_Tree_Driver_Record;
      Address : Aqua.Address_Type;
      Value   : out Aqua.Word_32)
   is
      use type Aqua.Word_32;
      R : constant Register_Index := Register_Index (Address / 4);
   begin
      Value := This.Rs (R);
   end Get_Word_32;

   -----------------
   -- Set_Word_32 --
   -----------------

   overriding procedure Set_Word_32
     (This    : in out Aquarius_Tree_Driver_Record;
      Address : Aqua.Address_Type;
      Value   : Aqua.Word_32)
   is
      use type Aqua.Word_32;
      R : constant Register_Index := Register_Index (Address / 4);
   begin
      case R is
         when R_Current =>
            if Value /= 0 and then
              (Value >= 2 ** 31 or else
                 not Aquarius.Programs.Is_Valid_Sequence (Positive (Value)))
            then
               raise Constraint_Error with
                 "invalid sequence number: "
                 & Aqua.Images.Hex_Image (Value);
            end if;
            if Value = 0 then
               This.Current := null;
            else
               This.Current :=
                 Aquarius.Programs.Get_Tree_From_Sequence (Positive (Value));
            end if;

         when R_Command =>
            if Value > Driver_Command'Pos (Driver_Command'Last) then
               raise Constraint_Error with
                 "invalid command: " & Aqua.Images.Hex_Image (Value);
            end if;

         when others =>
            null;
      end case;

      This.Rs (R) := Value;

      if R = R_Command then
         This.Execute_Command;
      end if;
   end Set_Word_32;

   ---------------------
   -- Execute_Command --
   ---------------------

   procedure Execute_Command
     (This : in out Aquarius_Tree_Driver_Record'Class)
   is
      Current_Sequence : constant Natural :=
                           Natural (This.Rs (R_Current));
      Command          : constant Natural :=
                           Natural (This.Rs (R_Command));
   begin

      if Current_Sequence = 0 then
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            "execute-command: no current tree");
         return;
      end if;

      if This.Current = null
        or else This.Current.Sequence /= Current_Sequence
      then
         This.Current := Get_Tree_From_Sequence (Current_Sequence);
      end if;

      if not Is_Command (Command) then
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            "execute-command:"
            & Command'Image
            & ": not a valid command");
         return;
      end if;

      case Driver_Command'Val (Command) is
         when No_Command =>
            null;
         when Get_Name =>
            This.Write_String (This.Current.Name);
         when Get_Text =>
            This.Write_String (This.Current.Text);
         when Get_Standard_Text =>
            This.Write_String (This.Current.Standard_Text);
         when Concatenated_Image =>
            This.Write_String (This.Current.Concatenate_Children);
         when File_Name =>
            This.Write_String
              (This.Current.Source.Short_Name);
         when Full_File_Name =>
            This.Write_String
              (This.Current.Source.Full_Name);
         when Start_Line =>
            This.Rs (R_Transfer) := Aqua.Word (This.Current.Start_Line);
         when End_Line =>
            This.Rs (R_Transfer) := Aqua.Word (This.Current.End_Line);
         when Start_Column =>
            This.Rs (R_Transfer) := Aqua.Word (This.Current.Start_Column);
         when End_Column =>
            This.Rs (R_Transfer) := Aqua.Word (This.Current.End_Column);
         when Start_Position =>
            This.Rs (R_Transfer) := Aqua.Word (This.Current.Start_Offset);
         when End_Position =>
            This.Rs (R_Transfer) := Aqua.Word (This.Current.End_Offset);
         when Error =>
            This.Current.Attach_Message
              (Aquarius.Messages.New_Message
                 (Level    => Aquarius.Messages.Error,
                  Location => This.Current,
                  Text     => This.Read_String));
         when Get_Property =>
            declare
               Name  : constant String := This.Read_String;
               Props : Aqua_Property_Maps.Map renames
                         This.Current.Aqua_Props;
               Value : constant Aqua.Address_Type :=
                         Props.Element (Name);
            begin
               if Trace_Properties then
                  Ada.Text_IO.Put_Line
                    (This.Current.Show_Location
                     & ": " & Aqua.Images.Hex_Image (This.Rs (R_Current))
                     & ": get_property "
                     & Name & " = " & Aqua.Images.Hex_Image (Value));
               end if;

               This.Rs (R_Transfer) := Value;
            end;
         when Has_Property =>
            declare
               Name  : constant String := This.Read_String;
               Props : Aqua_Property_Maps.Map renames
                         This.Current.Aqua_Props;
               Value : constant Boolean :=
                         Props.Contains (Name);
            begin
               if Trace_Properties then
                  Ada.Text_IO.Put_Line
                    (This.Current.Show_Location
                     & ": " & Aqua.Images.Hex_Image (This.Rs (R_Current))
                     & ": has_property "
                     & Name & " = " & Boolean'Image (Value));
               end if;

               This.Rs (R_Transfer) := Boolean'Pos (Value);
            end;
         when Set_Property =>
            declare
               Name  : constant String := This.Read_String;
               Props : Aqua_Property_Maps.Map renames
                         This.Current.Aqua_Props;
               Value : constant Aqua.Address_Type := This.Rs (R_Transfer);
            begin
               if Trace_Properties then
                  Ada.Text_IO.Put_Line
                    (This.Current.Show_Location
                     & ": " & Aqua.Images.Hex_Image (This.Rs (R_Transfer))
                     & ": set_property "
                     & Name & " := " & Aqua.Images.Hex_Image (Value));
               end if;

               if Props.Contains (Name) then
                  Props.Replace (Name, Value);
               else
                  Props.Insert (Name, Value);
               end if;
            end;

         when Create_Entity =>

            null;

            --  declare
            --     use type Aqua.Word;
            --
            --     Specification : constant String :=
            --                       Driver.Read_String;
            --     Name_Left     : constant Natural :=
            --                       Specification'First - 1;
            --     Q_Name_Left   : constant Natural :=
            --                       Ada.Strings.Fixed.Index
            --                         (Specification, "/", Name_Left + 1);
            --     Class_Name_Left : constant Natural :=
            --                         Ada.Strings.Fixed.Index
            --                           (Specification, "/", Q_Name_Left + 1);
            --     Location_Left   : constant Natural :=
            --                         Ada.Strings.Fixed.Index
            --                     (Specification, "/", Class_Name_Left + 1);
            --     Name            : constant String :=
            --                         Specification
            --                           (Name_Left + 1 .. Q_Name_Left - 1);
            --     Qualified_Name  : constant String :=
            --                         Specification
            --                     (Q_Name_Left + 1 .. Class_Name_Left - 1);
            --     Class_Name      : constant String :=
            --                         Specification
            --                           (Class_Name_Left + 1
            --                            .. Location_Left - 1);
            --     Top_Level       : constant Boolean :=
            --                         Driver.Get_Word (8) /= 0;
            --  begin
            --
            --     declare
            --        use Aquarius.Programs.Komnenos_Entities;
            --     begin
            --        Create_Aquarius_Source_Entity
            --          (Table     => Komnenos.Entities.Tables.Active_Table,
            --           Name             => Name,
            --           Qualified_Name   => Qualified_Name,
            --           Class_Name       => Class_Name,
            --           Top_Level        => Top_Level,
            --           Compilation_Unit => Driver.Current.Program_Root,
            --           Defining_Name    => Driver.Current,
            --           Entity_Spec      => Driver.Current,
            --           Entity_Body      => Driver.Current);
            --     end;
            --
            --  end;

         when Find_Entity =>
            null;

         when Cross_Reference =>
            null;

      end case;
   end Execute_Command;

   ------------------
   -- Write_String --
   ------------------

   procedure Write_String
     (Driver : in out Aquarius_Tree_Driver_Record'Class;
      S      : String)
   is
      R : Register_Index := R_String;
   begin
      Driver.Rs (R_String_Len) := Aqua.Word_32 (S'Length);
      for Ch of S loop
         Driver.Rs (R) := Character'Pos (Ch);
         R := R + 1;
      end loop;
   end Write_String;

end Aquarius.Programs.Device;
