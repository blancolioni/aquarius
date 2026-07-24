with Ada.Characters.Latin_1;

package body Aquarius.Models.Text is

   LF : Character renames Ada.Characters.Latin_1.LF;

   ------------
   -- Create --
   ------------

   function Create (Initial : String := "") return Simple_Text_Model_Access is
   begin
      return new Simple_Text_Model'
        (Aquarius.Observable.Publisher_Base with
           Content => To_Unbounded_String (Initial));
   end Create;

   ----------
   -- Kind --
   ----------

   overriding function Kind (Model : Simple_Text_Model) return String is
      pragma Unreferenced (Model);
   begin
      return "text";
   end Kind;

   --------------------
   -- Default_Viewer --
   --------------------

   overriding function Default_Viewer (Model : Simple_Text_Model) return String
   is
      pragma Unreferenced (Model);
   begin
      return "text";
   end Default_Viewer;

   ----------------
   -- Line_Count --
   ----------------

   overriding function Line_Count (Model : Simple_Text_Model) return Natural is
      S     : constant String := To_String (Model.Content);
      Count : Natural := 0;
   begin
      if S'Length = 0 then
         return 0;
      end if;
      Count := 1;
      for C of S loop
         if C = LF then
            Count := Count + 1;
         end if;
      end loop;
      return Count;
   end Line_Count;

   ----------
   -- Line --
   ----------

   overriding function Line
     (Model : Simple_Text_Model; Index : Positive) return String
   is
      S     : constant String := To_String (Model.Content);
      Line_No : Positive := 1;
      Start   : Positive := S'First;
   begin
      if S'Length = 0 then
         return "";
      end if;
      for I in S'Range loop
         if Line_No = Index and then (I = S'Last or else S (I) = LF) then
            if S (I) = LF then
               return S (Start .. I - 1);
            else
               return S (Start .. I);
            end if;
         elsif S (I) = LF then
            Line_No := Line_No + 1;
            Start := I + 1;
         end if;
      end loop;
      return "";
   end Line;

   ----------
   -- Text --
   ----------

   overriding function Text (Model : Simple_Text_Model) return String is
   begin
      return To_String (Model.Content);
   end Text;

   ---------------------
   -- Set_Text_Command --
   ---------------------

   function Set_Text_Command
     (Model    : not null Simple_Text_Model_Access;
      New_Text : String)
      return Aquarius.Commands.Command_Reference
   is
   begin
      return Aquarius.Commands.Command_Reference'
        (new Set_Text_Command_Type'
           (Model    => Model,
            New_Text => To_Unbounded_String (New_Text),
            Old_Text => Null_Unbounded_String));
   end Set_Text_Command;

   -----------------
   -- Description --
   -----------------

   overriding function Description
     (Command : Set_Text_Command_Type) return String
   is
      pragma Unreferenced (Command);
   begin
      return "set text";
   end Description;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute (Command : in out Set_Text_Command_Type) is
   begin
      Command.Old_Text := Command.Model.Content;
      Command.Model.Content := Command.New_Text;
      Command.Model.Notify (Aquarius.Observable.No_Update_Data);
   end Execute;

   ----------
   -- Undo --
   ----------

   overriding procedure Undo (Command : in out Set_Text_Command_Type) is
   begin
      Command.Model.Content := Command.Old_Text;
      Command.Model.Notify (Aquarius.Observable.No_Update_Data);
   end Undo;

end Aquarius.Models.Text;
