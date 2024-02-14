with WL.String_Maps;
with Ack.Generate.Primitives;

package body Ack.Generate.Intrinsics is

   package Generator_Maps is
     new WL.String_Maps (Intrinsic_Generator);

   Generator_Map : Generator_Maps.Map;

   procedure Create_Builtin_Generators;

   function System_Memory_Block_32_Get
     (Unit       : in out Tagatha.Code.Instance'Class;
      Push       : not null access
        procedure (Argument_Index : Positive))
      return Boolean;

   function System_Memory_Block_32_Put
     (Unit       : in out Tagatha.Code.Instance'Class;
      Push       : not null access
        procedure (Argument_Index : Positive))
      return Boolean;

   function System_Memory_Mem_Allocate
     (Unit       : in out Tagatha.Code.Instance'Class;
      Push       : not null access
        procedure (Argument_Index : Positive))
      return Boolean;

   function Put_Relative_Word_32
     (Unit       : in out Tagatha.Code.Instance'Class;
      Push       : not null access
        procedure (Argument_Index : Positive))
      return Boolean;

   function Offset_Words
     (Unit       : in out Tagatha.Code.Instance'Class;
      Push       : not null access
        procedure (Argument_Index : Positive))
      return Boolean;

   function Return_Argument
     (Unit       : in out Tagatha.Code.Instance'Class;
      Push       : not null access
        procedure (Argument_Index : Positive))
      return Boolean;

   -------------------
   -- Add_Intrinsic --
   -------------------

   procedure Add_Intrinsic
     (Name      : String;
      Generator : Intrinsic_Generator)
   is
   begin
      Generator_Map.Insert (Name, Generator);
   end Add_Intrinsic;

   -------------------------------
   -- Create_Builtin_Generators --
   -------------------------------

   procedure Create_Builtin_Generators is
   begin
      Add_Intrinsic
        ("offset_words", Offset_Words'Access);
      Add_Intrinsic
        ("return-argument", Return_Argument'Access);
      Add_Intrinsic
        ("put_relative_word_32", Put_Relative_Word_32'Access);
      Add_Intrinsic
        ("system-memory-block_32-get", System_Memory_Block_32_Get'Access);
      Add_Intrinsic
        ("system-memory-block_32-put", System_Memory_Block_32_Put'Access);
      Add_Intrinsic
        ("mm_allocate", System_Memory_Mem_Allocate'Access);
   end Create_Builtin_Generators;

   ------------------------
   -- Generate_Intrinsic --
   ------------------------

   procedure Generate_Intrinsic
     (Unit      : in out Tagatha.Code.Instance'Class;
      Name      : String;
      Arg_Count : Natural;
      Push      : not null access
        procedure (Argument_Index : Positive))
   is
   begin
      if Generator_Map.Is_Empty then
         Create_Builtin_Generators;
      end if;

      if not Generator_Map.Contains (Name) then
         for I in 1 .. Arg_Count loop
            Push (I);
         end loop;
         Ack.Generate.Primitives.Generate_Intrinsic
           (Unit, Get_Name_Id (Name));
         return;
--           raise Constraint_Error with
--             "unknown intrinsic: " & Name;
      end if;

      if not Generator_Map.Element (Name)
        (Unit, Push)
      then
         raise Constraint_Error with
           "generating intrinsic '" & Name & "' failed";
      end if;

   end Generate_Intrinsic;

   ------------------
   -- Offset_Words --
   ------------------

   function Offset_Words
     (Unit       : in out Tagatha.Code.Instance'Class;
      Push       : not null access
        procedure (Argument_Index : Positive))
      return Boolean
   is
   begin
      Push (1);
      Unit.Push_Constant (Tagatha.Int_32'(4));
      Unit.Operate (Tagatha.Op_Multiply);
      Unit.Operate (Tagatha.Op_Add);
      return True;
   end Offset_Words;

   --------------------------
   -- Put_Relative_Word_32 --
   --------------------------

   function Put_Relative_Word_32
     (Unit       : in out Tagatha.Code.Instance'Class;
      Push       : not null access
        procedure (Argument_Index : Positive))
      return Boolean
   is
   begin
      Push (1);
      Unit.Push_Constant (Tagatha.Int_32'(4));
      Unit.Operate (Tagatha.Op_Multiply);
      Unit.Operate (Tagatha.Op_Add);
      Push (2);
      Unit.Swap;
      Unit.Pop_Indirect;
      return True;
   end Put_Relative_Word_32;

   ---------------------
   -- Return_Argument --
   ---------------------

   function Return_Argument
     (Unit       : in out Tagatha.Code.Instance'Class;
      Push       : not null access
        procedure (Argument_Index : Positive))
      return Boolean
   is
      pragma Unreferenced (Unit);
   begin
      Push (1);
      return True;
   end Return_Argument;

   --------------------------------
   -- System_Memory_Block_32_Get --
   --------------------------------

   function System_Memory_Block_32_Get
     (Unit       : in out Tagatha.Code.Instance'Class;
      Push       : not null access
        procedure (Argument_Index : Positive))
      return Boolean
   is
   begin
      Unit.Dereference (Tagatha.General_Content, 1);
      Push (1);
      Unit.Push_Constant (Tagatha.Int_32'(4));
      Unit.Operate (Tagatha.Op_Multiply);
      Unit.Operate (Tagatha.Op_Add);
      Unit.Dereference;
      return True;
   end System_Memory_Block_32_Get;

   --------------------------------
   -- System_Memory_Block_32_Put --
   --------------------------------

   function System_Memory_Block_32_Put
     (Unit       : in out Tagatha.Code.Instance'Class;
      Push       : not null access
        procedure (Argument_Index : Positive))
      return Boolean
   is
   begin
      Unit.Dereference (Tagatha.General_Content, 1);
      Push (1);
      Unit.Push_Constant (Tagatha.Int_32'(4));
      Unit.Operate (Tagatha.Op_Multiply);
      Unit.Operate (Tagatha.Op_Add);
      Push (2);
      Unit.Swap;
      Unit.Pop_Indirect;
      return True;
   end System_Memory_Block_32_Put;

   --------------------------------
   -- System_Memory_Mem_Allocate --
   --------------------------------

   function System_Memory_Mem_Allocate
     (Unit       : in out Tagatha.Code.Instance'Class;
      Push       : not null access
        procedure (Argument_Index : Positive))
      return Boolean
   is
   begin
      Push (1);
      Unit.Call ("mm.allocate", 1, 1);
      Unit.Push_Return (1);
      return True;
   end System_Memory_Mem_Allocate;

end Ack.Generate.Intrinsics;
