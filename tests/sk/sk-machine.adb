with Ada.Text_IO;

with SK.Compiler;
with SK.Environments;
with SK.Evaluator;
with SK.Functions.Primitives;
with SK.Images;
with SK.Memory;
with SK.Parser;
with SK.Stack;

package body SK.Machine is

   Debug_Machine : constant Boolean := False;

   type SK_Machine_Record is
      record
         Cells  : SK.Cells.Managed_Cells;
         Env    : SK.Environments.Environment;
         Start  : Object;
      end record;

   procedure Define_Primitive (M     : SK_Machine;
                               Name  : String;
                               Code  : String);

   ----------
   -- Bind --
   ----------

   procedure Bind (M : SK_Machine;
                   Name : String)
   is
      E : Object := SK.Stack.Top (M.Cells);
   begin
      if Debug_Machine then
         Ada.Text_IO.Put_Line ("Bind: " & SK.Images.Image (M.Cells, E));
      end if;
      E := SK.Compiler.Compile (M.Cells, E);
      if Debug_Machine then
         Ada.Text_IO.Put_Line ("Compiles to: " &
                                 SK.Images.Image (M.Cells, E));
      end if;
--        E := SK.Compiler.Link (M.Cells, M.Env, E);
--        Ada.Text_IO.Put_Line ("Links to: " & SK.Images.Image (M.Cells, E));
      SK.Stack.Push (M.Cells, E);
      SK.Cells.Save_Definition (M.Cells, E);
      SK.Stack.Drop (M.Cells, 2);

      if Debug_Machine then
         Ada.Text_IO.Put_Line (Name & " = " & SK.Images.Image (M.Cells, E));
      end if;

      SK.Environments.Define (M.Env, Name, E);
   end Bind;

   -------------
   -- Compile --
   -------------

   function Compile (M : SK_Machine;
                     E : Object)
                     return Object
   is
      Result : constant Object := SK.Compiler.Compile (M.Cells, E);
   begin
      return SK.Compiler.Link (M.Cells, M.Env, Result);
   end Compile;

   --------------------
   -- Create_Machine --
   --------------------

   function Create_Machine (Size : Natural) return SK_Machine is
      Mem    : constant SK.Memory.Memory_Type :=
        SK.Memory.Create_Managed_Memory (SK.Memory.Cell_Address (Size), 0);
      Rom    : constant SK.Memory.Memory_Type :=
        SK.Memory.Create_Extensible_Memory (SK.Memory.Cell_Address (Size));
      Result : constant SK_Machine := new SK_Machine_Record'
        (Cells => SK.Cells.Create_Managed_Cells (Mem, Rom),
         Env   => SK.Environments.New_Environment
           (SK.Environments.Top_Level_Environment),
         Start => 0);
   begin
      SK.Functions.Primitives.Add_Primitives (Result.Env);

      Define_Primitive (Result, "true", "\x.\y.y");
      Define_Primitive (Result, "false", "\x.\y.x");
      Define_Primitive (Result, "if", "\p.\f.\t.p f t");
      Define_Primitive (Result, "Y", "\f.(\x.f (x x)) (\x.f (x x))");
      Define_Primitive (Result, "cons", "\h.\t.\x.\y.y h t");
      Define_Primitive (Result, "Y'",
                        "S (K (S I I)) (S (S (K S) K) (K (S I I)))");
      return Result;
   end Create_Machine;

   ----------------------
   -- Define_Primitive --
   ----------------------

   procedure Define_Primitive (M     : SK_Machine;
                               Name  : String;
                               Code  : String)
   is
      E : Object;
   begin
      E := SK.Parser.Parse_String (M.Cells, Code);
      SK.Stack.Push (M.Cells, E);
      E := SK.Compiler.Compile (M.Cells, E);
      SK.Stack.Push (M.Cells, E);
      E := SK.Compiler.Link (M.Cells, M.Env, E);
      SK.Stack.Push (M.Cells, E);
      SK.Cells.Save_Definition (M.Cells, E);
      SK.Stack.Drop (M.Cells, 3);

      if Debug_Machine then
         Ada.Text_IO.Put_Line (SK.Images.Image (M.Cells, E));
      end if;

      SK.Environments.Define (M.Env, Name, E);
   end Define_Primitive;

   --------------
   -- Evaluate --
   --------------

   function Evaluate
     (M : SK_Machine;
      E : Object)
      return Object
   is
      Value : Object := E;
   begin
      SK.Evaluator.Evaluate (M.Cells, Value);
      return Value;
   end Evaluate;

   --------------
   -- Evaluate --
   --------------

   procedure Evaluate (M : SK_Machine) is
      V : Object := SK.Stack.Top (M.Cells);
   begin
      if Debug_Machine then
         Ada.Text_IO.Put_Line ("Evaluating: " &
                                 SK.Images.Image (M.Cells, V));
      end if;
      V := Compile (M, V);
      SK.Evaluator.Evaluate (M.Cells, V);
      SK.Stack.Drop (M.Cells, 1);
      SK.Stack.Push (M.Cells, V);
   end Evaluate;

   ---------------
   -- Get_Cells --
   ---------------

   function Get_Cells (M : SK_Machine) return SK.Cells.Managed_Cells is
   begin
      return M.Cells;
   end Get_Cells;

   ------------------
   -- Load_Library --
   ------------------

   procedure Load_Library
     (M    : SK_Machine;
      Path : String)
   is
   begin
      --  Generated stub: replace with real body!
      raise Program_Error;
   end Load_Library;

   ------------------
   -- Load_Machine --
   ------------------

   function Load_Machine (Path : String) return SK_Machine is
   begin
      --  Generated stub: replace with real body!
      raise Program_Error;
      return Load_Machine (Path);
   end Load_Machine;

   --------------------
   -- Low_Level_Show --
   --------------------

   function Low_Level_Show (M    : SK_Machine;
                            Item : Object)
                            return String
   is
   begin
      return SK.Images.Low_Level_Image (M.Cells, Item);
   end Low_Level_Show;

   ------------------
   -- Parse_String --
   ------------------

   function Parse_String (M    : SK_Machine;
                          Text : String)
                          return Object
   is
   begin
      return SK.Parser.Parse_String (M.Cells, Text);
   end Parse_String;

   ----------
   -- Show --
   ----------

   function Show (M    : SK_Machine;
                  Item : Object)
                  return String
   is
   begin
      return SK.Images.Image (M.Cells, Item);
   end Show;

   --------------------
   -- Show_Stack_Top --
   --------------------

   function Show_Stack_Top (M    : SK_Machine)
                           return String
   is
   begin
      return Show (M, SK.Stack.Top (M.Cells));
   end Show_Stack_Top;

   -----------
   -- Start --
   -----------

   function Start (M : SK_Machine) return Object is
   begin
      return M.Start;
   end Start;

end SK.Machine;
