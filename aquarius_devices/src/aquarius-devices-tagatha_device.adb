with Ada.Containers.Vectors;
with Ada.Exceptions;
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with Tagatha.Arch;
with Tagatha.Code;

package body Aquarius.Devices.Tagatha_Device is

   No_Command          : constant := 0;
   New_Code            : constant := 1;
   Dispose_Code        : constant := 2;
   Next_Label          : constant := 3;
   Named_Label         : constant := 4;
   Set_Label           : constant := 5;
   Set_Source_Location : constant := 6;
   Push_Constant       : constant := 7;
   Push_Argument       : constant := 8;
   Push_Local          : constant := 9;
   Pop_Argument        : constant := 10;
   Pop_Local           : constant := 11;
   Begin_Routine       : constant := 12;
   End_Routine         : constant := 13;
   Drop                : constant := 14;
   Pop                 : constant := 15;
   Duplicate           : constant := 16;
   Swap                : constant := 17;
   Operate             : constant := 18;
   Branch_Cond         : constant := 19;
   Branch_Always       : constant := 20;
   Call                : constant := 21;
   Indirect_Call       : constant := 22;
   Jump                : constant := 23;
   Pop_Result          : constant := 24;
   Push_Return         : constant := 25;
   Exit_Routine        : constant := 26;
   Fail_Routine        : constant := 27;
   Retry_Routine       : constant := 28;
   Raise_Exception     : constant := 29;
   Add_Local           : constant := 30;
   Remove_Local        : constant := 31;
   Begin_Block         : constant := 32;
   End_Block           : constant := 33;

   Register_Count : constant := 1024;
   type Register_Index is range 0 .. Register_Count - 1;

   R_Current      : constant Register_Index := 0;
   R_Command      : constant Register_Index := 1;
   R_Transfer     : constant Register_Index := 2;
   R_Transfer_2   : constant Register_Index := 3;
   R_Content      : constant Register_Index := 4;
   R_String_Len   : constant Register_Index := 5;
   R_String       : constant Register_Index := 6;

   type Register_Array is array (Register_Index) of Aqua.Word_32;

   subtype Parent is Aquarius.Devices.Instance;

   type Code_Reference is access all Tagatha.Code.Instance;

   package Code_Vectors is
     new Ada.Containers.Vectors (Positive, Code_Reference);

   type Instance is new Parent with
      record
         Rs     : Register_Array := [others => 0];
         Code   : Code_Vectors.Vector;
      end record;

   type Instance_Reference is access all Instance'Class;

   overriding function Name (This : Instance) return String
   is ("tagatha");

   overriding function Word_Count (This : Instance) return Natural
   is (Natural (Register_Count));

   overriding procedure Get_Word_32
     (This    : in out Instance;
      Address : Aqua.Address_Type;
      Value   : out Aqua.Word_32);

   overriding procedure Set_Word_32
     (This    : in out Instance;
      Address : Aqua.Address_Type;
      Value   : Aqua.Word_32);

   function Read_String
     (Driver : Instance'Class)
      return String;

   procedure Write_String
     (Driver : in out Instance'Class;
      S      : String);

   procedure Execute_Command
     (This    : in out Instance'Class;
      Command : Aqua.Word_32);

   ------------
   -- Create --
   ------------

   function Create
     return Aquarius.Devices.Reference
   is
      This : constant Instance_Reference := new Instance;
   begin
      return Aquarius.Devices.Reference (This);
   end Create;

   ---------------------
   -- Execute_Command --
   ---------------------

   procedure Execute_Command
     (This    : in out Instance'Class;
      Command : Aqua.Word_32)
   is
      function Current return Code_Reference
      is (This.Code (Positive (This.Rs (R_Current))));

      function Get_Content return Tagatha.Operand_Content is
         use type Aqua.Word_32;
         Result : constant Tagatha.Operand_Content :=
                    (if This.Rs (R_Content) = 1
                     then Tagatha.Floating_Point_Content
                     else Tagatha.General_Content);
      begin
         This.Rs (R_Content) := 0;
         return Result;
      end Get_Content;

   begin
      This.Rs (R_Command) := 0;
      case Command is
         when No_Command =>
            null;
         when New_Code =>
            declare
               Code : constant Code_Reference :=
                        new Tagatha.Code.Instance;
               Index : Natural := 0;
            begin
               for I in 1 .. This.Code.Last_Index loop
                  if This.Code (I) = null then
                     This.Code (I) := Code;
                     Index := I;
                     exit;
                  end if;
               end loop;
               if Index = 0 then
                  This.Code.Append (Code);
                  Index := This.Code.Last_Index;
               end if;
               This.Rs (R_Current) := Aqua.Word_32 (Index);
            end;
         when Dispose_Code =>
            declare
               procedure Free is
                 new Ada.Unchecked_Deallocation
                   (Tagatha.Code.Instance, Code_Reference);
               Index : constant Positive :=
                         Positive (This.Rs (R_Current));
               Code : Code_Reference :=
                         This.Code (Index);
               Gen   : Tagatha.Arch.Instance'Class :=
                         Tagatha.Arch.Get ("pdp11");
            begin
               Code.Generate (Gen);
               Gen.Save ("tagatha.pdp11");
               This.Code (Index) := null;
               Free (Code);
               This.Rs (R_Current) := 0;
            end;

         when Next_Label =>
            declare
               L : constant Tagatha.Code.Label :=
                     Current.Next_Label;
            begin
               This.Rs (R_Transfer) :=
                 Aqua.Word_32 (Tagatha.Code.Get_Label_Index (L));
            end;

         when Named_Label =>
            declare
               S : constant String := This.Read_String;
               L : constant Tagatha.Code.Label :=
                     Current.Named_Label (S);
            begin
               This.Rs (R_Transfer) :=
                 Aqua.Word_32 (Tagatha.Code.Get_Label_Index (L));
            end;

         when Set_Label =>
            declare
               L : constant Tagatha.Code.Label :=
                     Tagatha.Code.From_Label_Index
                       (Natural (This.Rs (R_Transfer)));
            begin
               Current.Set_Label (L);
            end;

         when Set_Source_Location =>
            declare
               Line : constant Natural := Natural (This.Rs (R_Transfer));
               Col  : constant Natural := Natural (This.Rs (R_Transfer_2));
            begin
               Current.Source_Location (Line, Col);
            end;

         when Push_Constant =>
            declare
               use Tagatha;
               K : constant Word_64 :=
                     Word_64 (This.Rs (R_Transfer))
                     + 2 ** 32 * Word_64 (This.Rs (R_Transfer_2));
            begin
               Current.Push_Constant (K);
            end;

         when Push_Argument =>
            Current.Push_Argument
              (Index   => Tagatha.Argument_Index (This.Rs (R_Transfer)),
               Content => Get_Content);

         when Push_Local =>
            declare
               use type Aqua.Word_32;
               Reference : constant Boolean :=
                             (This.Rs (R_Transfer_2) and 1) = 1;
            begin
               Current.Push_Local
                 (Index     => Tagatha.Local_Index (This.Rs (R_Transfer)),
                  Content   => Get_Content,
                  Reference => Reference);
            end;

         when Pop_Argument =>
            Current.Pop_Argument
              (Index   => Tagatha.Argument_Index (This.Rs (R_Transfer)),
               Content => Get_Content);

         when Pop_Local =>
            Current.Pop_Local
              (Index   => Tagatha.Local_Index (This.Rs (R_Transfer)),
               Content => Get_Content);

         when Begin_Routine =>
            declare
               use type Aqua.Word_32;
               Options : constant Tagatha.Code.Routine_Options'Class :=
                           Tagatha.Code.Set_Argument_Count
                             (Tagatha.Argument_Count (This.Rs (R_Transfer)));
               Is_Public : constant Boolean :=
                              (This.Rs (R_Transfer_2) and 1) = 1;
            begin
               Current.Begin_Routine
                 (Name => This.Read_String,
                  Options => (if Is_Public
                              then Options
                              else  Options.Set_No_Linkage));
            end;

         when End_Routine =>
            Current.End_Routine;

         when Drop =>
            Current.Drop;

         when Pop =>
            Current.Pop;

         when Duplicate =>
            Current.Duplicate;

         when Swap =>
            Current.Swap;

         when Operate =>
            Current.Operate
              (Tagatha.Operator'Val (This.Rs (R_Transfer)));

         when Branch_Cond =>
            Current.Branch
              (Condition   =>
                 Tagatha.Branch_Condition'Val (This.Rs (R_Transfer)),
               Destination =>
                 Tagatha.Code.From_Label_Index
                   (Natural (This.Rs (R_Transfer_2))));

         when Branch_Always =>
            Current.Branch
              (Destination =>
                 Tagatha.Code.From_Label_Index
                   (Natural (This.Rs (R_Transfer))));

         when Call =>
            Current.Call
              (Name           => This.Read_String,
               Argument_Count => Natural (This.Rs (R_Transfer)),
               Result_Count   => Natural (This.Rs (R_Transfer_2)));

         when Indirect_Call =>
            Current.Indirect_Call
              (Argument_Count => Natural (This.Rs (R_Transfer)),
               Result_Count   => Natural (This.Rs (R_Transfer_2)));

         when Jump =>
            Current.Jump (This.Read_String);

         when Pop_Result =>
            Current.Pop_Result
              (Index   => Tagatha.Result_Index (This.Rs (R_Transfer)),
               Content => Get_Content);

         when Push_Return =>
            Current.Push_Return
              (Index   => Tagatha.Return_Index (This.Rs (R_Transfer)),
               Content => Get_Content);

         when Exit_Routine =>
            Current.Exit_Routine;

         when Fail_Routine =>
            Current.Fail_Routine;

         when Retry_Routine =>
            Current.Retry_Routine;

         when Raise_Exception =>
            Current.Raise_Exception;

         when Add_Local =>
            This.Rs (R_Transfer) :=
              Aqua.Word_32 (Current.Add_Local);

         when Remove_Local =>
            Current.Remove_Local;

         when Begin_Block =>
            Current.Begin_Block;

         when End_Block =>
            Current.End_Block;

         when others =>
            This.Rs (R_Command) := Command;
            This.Write_String ("invalid command");

      end case;
   end Execute_Command;

   -----------------
   -- Get_Word_32 --
   -----------------

   overriding procedure Get_Word_32
     (This    : in out Instance;
      Address : Aqua.Address_Type;
      Value   : out Aqua.Word_32)
   is
      use type Aqua.Word_32;
   begin
      Value := This.Rs (Register_Index (Address / 4));
   end Get_Word_32;

   -----------------
   -- Read_String --
   -----------------

   function Read_String
     (Driver : Instance'Class)
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
   -- Set_Word_32 --
   -----------------

   overriding procedure Set_Word_32
     (This    : in out Instance;
      Address : Aqua.Address_Type;
      Value   : Aqua.Word_32)
   is
      use type Aqua.Word_32;
      R : constant Register_Index := Register_Index (Address / 4);
   begin
      This.Rs (R) := Value;
      if R = R_Command then
         begin
            This.Execute_Command (Value);
         exception
            when E : others =>
               This.Rs (R_Command) := 1;
               This.Write_String ("unspecified error");
               Ada.Text_IO.Put_Line
                 (Ada.Text_IO.Standard_Error,
                  Ada.Exceptions.Exception_Message (E));

         end;
      end if;
   end Set_Word_32;

   ------------------
   -- Write_String --
   ------------------

   procedure Write_String
     (Driver : in out Instance'Class;
      S      : String)
   is
      Index  : Register_Index := R_String;
   begin
      Driver.Rs (R_String_Len) := Aqua.Word_32 (S'Length);
      for Ch of S loop
         Driver.Rs (Index) := Character'Pos (Ch);
         Index := Index + 1;
      end loop;
   end Write_String;

end Aquarius.Devices.Tagatha_Device;
