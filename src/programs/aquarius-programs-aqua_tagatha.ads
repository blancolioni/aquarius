with Aqua.Execution;

package Aquarius.Programs.Aqua_Tagatha is

   procedure Add_Handlers;

   function Tagatha_Allocate
     (Context : in out Aqua.Execution.Execution_Interface'Class;
      Arguments : Aqua.Array_Of_Words)
      return Aqua.Word;

   function Tagatha_Apply_Fragment
     (Context : in out Aqua.Execution.Execution_Interface'Class;
      Arguments : Aqua.Array_Of_Words)
      return Aqua.Word;

   function Tagatha_Begin_Unit
     (Context : in out Aqua.Execution.Execution_Interface'Class;
      Arguments : Aqua.Array_Of_Words)
      return Aqua.Word;

   function Tagatha_Code_Segment
     (Context : in out Aqua.Execution.Execution_Interface'Class;
      Arguments : Aqua.Array_Of_Words)
      return Aqua.Word;

   function Tagatha_Data
     (Context : in out Aqua.Execution.Execution_Interface'Class;
      Arguments : Aqua.Array_Of_Words)
      return Aqua.Word;

   function Tagatha_End_Unit
     (Context : in out Aqua.Execution.Execution_Interface'Class;
      Arguments : Aqua.Array_Of_Words)
      return Aqua.Word;

   function Tagatha_Code_Unit
     (Context : in out Aqua.Execution.Execution_Interface'Class;
      Arguments : Aqua.Array_Of_Words)
      return Aqua.Word;

   function Tagatha_Label
     (Context : in out Aqua.Execution.Execution_Interface'Class;
      Arguments : Aqua.Array_Of_Words)
      return Aqua.Word;

   function Tagatha_Branch
     (Context : in out Aqua.Execution.Execution_Interface'Class;
      Arguments : Aqua.Array_Of_Words)
      return Aqua.Word;

   function Tagatha_Begin_Procedure
     (Context : in out Aqua.Execution.Execution_Interface'Class;
      Arguments : Aqua.Array_Of_Words)
      return Aqua.Word;

   function Tagatha_End_Procedure
     (Context : in out Aqua.Execution.Execution_Interface'Class;
      Arguments : Aqua.Array_Of_Words)
      return Aqua.Word;

   function Tagatha_Return
     (Context : in out Aqua.Execution.Execution_Interface'Class;
      Arguments : Aqua.Array_Of_Words)
      return Aqua.Word;

   function Tagatha_Text_Segment
     (Context : in out Aqua.Execution.Execution_Interface'Class;
      Arguments : Aqua.Array_Of_Words)
      return Aqua.Word;

end Aquarius.Programs.Aqua_Tagatha;
