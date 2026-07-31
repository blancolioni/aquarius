package body Ack.Generate.Primitives is

   type Primitive_Operator_Generator is access
     procedure (Unit : in out Tagatha.Code.Instance'Class);

   type Primitive_Operator_Record is
      record
         Operator  : Name_Id;
         Left_Type : Ack.Types.Type_Entity;
         Generator : Primitive_Operator_Generator;
      end record;

   package Primitive_Operator_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Primitive_Operator_Record);

   Primitive_Operators : Primitive_Operator_Lists.List;

   type Intrinsic_Feature_Generator is access
     procedure (Unit : in out Tagatha.Code.Instance'Class);

   package Intrinsic_Feature_Maps is
     new WL.String_Maps (Intrinsic_Feature_Generator);

   Intrinsic_Features : Intrinsic_Feature_Maps.Map;

   Have_Primitives : Boolean := False;

   procedure Create_Primitives;

   procedure Generate_Equal (Unit : in out Tagatha.Code.Instance'Class);
   procedure Generate_Not_Equal (Unit : in out Tagatha.Code.Instance'Class);

   procedure Generate_GE (Unit : in out Tagatha.Code.Instance'Class);
   procedure Generate_GT (Unit : in out Tagatha.Code.Instance'Class);
   procedure Generate_LE (Unit : in out Tagatha.Code.Instance'Class);
   procedure Generate_LT (Unit : in out Tagatha.Code.Instance'Class);

   procedure Generate_Not (Unit : in out Tagatha.Code.Instance'Class);
   procedure Generate_And (Unit : in out Tagatha.Code.Instance'Class);
   procedure Generate_Or (Unit : in out Tagatha.Code.Instance'Class);
   procedure Generate_Xor (Unit : in out Tagatha.Code.Instance'Class);
   procedure Generate_Implies (Unit : in out Tagatha.Code.Instance'Class);

   procedure Generate_Add (Unit : in out Tagatha.Code.Instance'Class);
   procedure Generate_Subtract (Unit : in out Tagatha.Code.Instance'Class);
   procedure Generate_Negate (Unit : in out Tagatha.Code.Instance'Class);
   procedure Generate_Multiply (Unit : in out Tagatha.Code.Instance'Class);
   procedure Generate_Divide (Unit : in out Tagatha.Code.Instance'Class);
   procedure Generate_Mod (Unit : in out Tagatha.Code.Instance'Class);
   procedure Generate_Join (Unit : in out Tagatha.Code.Instance'Class);

   --  Floating point arithmetic has its own operators in the Tagatha IR, so
   --  REAL cannot share the integer intrinsics; it names these instead.  The
   --  comparisons and Op_Mod are not duplicated, because the Aqua backend
   --  already picks fcmp/feql/frem from the operand content.
   procedure Generate_Fadd (Unit : in out Tagatha.Code.Instance'Class);
   procedure Generate_Fsub (Unit : in out Tagatha.Code.Instance'Class);
   procedure Generate_Fmul (Unit : in out Tagatha.Code.Instance'Class);
   procedure Generate_Fdiv (Unit : in out Tagatha.Code.Instance'Class);

   procedure Generate_Intrinsic_Real_Zero
     (Unit : in out Tagatha.Code.Instance'Class);
   procedure Generate_Intrinsic_Real_One
     (Unit : in out Tagatha.Code.Instance'Class);

   procedure Generate_To_Real (Unit : in out Tagatha.Code.Instance'Class);
   procedure Generate_To_Integer (Unit : in out Tagatha.Code.Instance'Class);

   procedure Generate_Offset_Words (Unit : in out Tagatha.Code.Instance'Class);

   procedure Generate_Intrinsic_Nop
     (Unit : in out Tagatha.Code.Instance'Class)
   is null;

   procedure Generate_Intrinsic_Replace_Current
     (Unit : in out Tagatha.Code.Instance'Class);

   procedure Generate_Intrinsic_Mem_Get_Word_32
     (Unit : in out Tagatha.Code.Instance'Class);

   procedure Generate_Intrinsic_Mem_Put_Word_32
     (Unit : in out Tagatha.Code.Instance'Class);

   --  The same two operations for a double, which is two words: the content
   --  travels with the dereference and the indirect store, so the backend
   --  moves the whole pair.
   procedure Generate_Intrinsic_Mem_Get_Real_64
     (Unit : in out Tagatha.Code.Instance'Class);

   procedure Generate_Intrinsic_Mem_Put_Real_64
     (Unit : in out Tagatha.Code.Instance'Class);

   procedure Generate_Intrinsic_Zero
     (Unit : in out Tagatha.Code.Instance'Class);

   procedure Generate_Intrinsic_One
     (Unit : in out Tagatha.Code.Instance'Class);

   --------------------------------
   -- Create_Integral_Primitives --
   --------------------------------

   procedure Create_Integral_Primitives
     (For_Class : Ack.Classes.Class_Entity)
   is
      Integer_Type : constant Ack.Types.Type_Entity :=
                       Ack.Types.New_Class_Type
                         (No_Node, For_Class, False);

      procedure Add
        (Name      : String;
         Left_Type : Ack.Types.Type_Entity;
         Generator : Primitive_Operator_Generator);

      ---------
      -- Add --
      ---------

      procedure Add
        (Name      : String;
         Left_Type : Ack.Types.Type_Entity;
         Generator : Primitive_Operator_Generator)
      is
      begin
         Primitive_Operators.Append
           (Primitive_Operator_Record'
              (Operator  => Get_Name_Id (Name),
               Left_Type => Left_Type,
               Generator => Generator));
      end Add;

   begin
      Add (">", Integer_Type, Generate_GT'Access);
      Add ("<", Integer_Type, Generate_LT'Access);
      Add (">=", Integer_Type, Generate_GE'Access);
      Add ("<=", Integer_Type, Generate_LE'Access);

      Add ("+", Integer_Type, Generate_Add'Access);
      Add ("-", Integer_Type, Generate_Subtract'Access);
   end Create_Integral_Primitives;

   -----------------------
   -- Create_Primitives --
   -----------------------

   procedure Create_Primitives is

--        Any_Type : constant Ack.Types.Type_Entity :=
--                     Ack.Types.Get_Top_Level_Type ("any");
      Boolean_Type : constant Ack.Types.Type_Entity :=
                       Ack.Types.Get_Top_Level_Type ("boolean");
      String_Type : constant Ack.Types.Type_Entity :=
                   Ack.Types.Get_Top_Level_Type ("string");
      Integer_Type  : constant Ack.Types.Type_Entity :=
                        Ack.Types.Get_Top_Level_Type ("integer");

      procedure Add
        (Name      : String;
         Left_Type : Ack.Types.Type_Entity;
         Generator : Primitive_Operator_Generator);

      procedure Add
        (Name      : String;
         Generator : Intrinsic_Feature_Generator);

      ---------
      -- Add --
      ---------

      procedure Add
        (Name      : String;
         Left_Type : Ack.Types.Type_Entity;
         Generator : Primitive_Operator_Generator)
      is
      begin
         Primitive_Operators.Append
           (Primitive_Operator_Record'
              (Operator  => Get_Name_Id (Name),
               Left_Type => Left_Type,
               Generator => Generator));
      end Add;

      ---------
      -- Add --
      ---------

      procedure Add
        (Name      : String;
         Generator : Intrinsic_Feature_Generator)
      is
      begin
         Intrinsic_Features.Insert (Name, Generator);
      end Add;

   begin
      if False then

         Add (">", Integer_Type, Generate_GT'Access);
         Add ("<", Integer_Type, Generate_LT'Access);
         Add (">=", Integer_Type, Generate_GE'Access);
         Add ("<=", Integer_Type, Generate_LE'Access);
      end if;

      Add ("=", Boolean_Type, Generate_Equal'Access);
      Add ("/=", Boolean_Type, Generate_Not_Equal'Access);
      Add ("not", Boolean_Type, Generate_Not'Access);
      Add ("and", Boolean_Type, Generate_And'Access);
      Add ("or", Boolean_Type, Generate_Or'Access);
      Add ("xor", Boolean_Type, Generate_Xor'Access);
      Add ("implies", Boolean_Type, Generate_Implies'Access);

      Add ("&", String_Type, Generate_Join'Access);
      Add ("+", Integer_Type, Generate_Add'Access);

      Add ("nop", Generate_Intrinsic_Nop'Access);
      Add ("replace-current", Generate_Intrinsic_Replace_Current'Access);

      Add ("not", Generate_Not'Access);
      Add ("and", Generate_And'Access);
      Add ("or", Generate_Or'Access);
      Add ("xor", Generate_Xor'Access);
      Add ("implies", Generate_Implies'Access);

      Add ("zero", Generate_Intrinsic_Zero'Access);
      Add ("one", Generate_Intrinsic_One'Access);

      Add ("real_zero", Generate_Intrinsic_Real_Zero'Access);
      Add ("real_one", Generate_Intrinsic_Real_One'Access);
      Add ("fadd", Generate_Fadd'Access);
      Add ("fsubtract", Generate_Fsub'Access);
      Add ("fmultiply", Generate_Fmul'Access);
      Add ("fdivide", Generate_Fdiv'Access);
      Add ("to_real", Generate_To_Real'Access);
      Add ("to_integer", Generate_To_Integer'Access);

      Add ("add", Generate_Add'Access);
      Add ("subtract", Generate_Subtract'Access);
      Add ("negate", Generate_Negate'Access);
      Add ("multiply", Generate_Multiply'Access);
      Add ("divide", Generate_Divide'Access);
      Add ("modulus", Generate_Mod'Access);
      Add ("equal", Generate_Equal'Access);
      Add ("not_equal", Generate_Not_Equal'Access);
      Add ("le", Generate_LE'Access);
      Add ("lt", Generate_LT'Access);
      Add ("ge", Generate_GE'Access);
      Add ("gt", Generate_GT'Access);
      Add ("get_word_32", Generate_Intrinsic_Mem_Get_Word_32'Access);
      Add ("put_word_32", Generate_Intrinsic_Mem_Put_Word_32'Access);
      Add ("get_real_64", Generate_Intrinsic_Mem_Get_Real_64'Access);
      Add ("put_real_64", Generate_Intrinsic_Mem_Put_Real_64'Access);
      Add ("offset_words", Generate_Offset_Words'Access);

      Add ("exit", Generate_Exit'Access);

      Have_Primitives := True;

   end Create_Primitives;

   ------------------
   -- Generate_Add --
   ------------------

   procedure Generate_Add (Unit : in out Tagatha.Code.Instance'Class) is
   begin
      Unit.Operate (Tagatha.Op_Add);
   end Generate_Add;

   ------------------
   -- Generate_And --
   ------------------

   procedure Generate_And (Unit : in out Tagatha.Code.Instance'Class) is
   begin
      Unit.Operate (Tagatha.Op_And);
   end Generate_And;

   ---------------------
   -- Generate_Divide --
   ---------------------

   procedure Generate_Divide (Unit : in out Tagatha.Code.Instance'Class) is
   begin
      Unit.Operate (Tagatha.Op_Divide);
   end Generate_Divide;

   -------------------
   -- Generate_Fadd --
   -------------------

   procedure Generate_Fadd (Unit : in out Tagatha.Code.Instance'Class) is
   begin
      Unit.Operate (Tagatha.Op_Fadd);
   end Generate_Fadd;

   -------------------
   -- Generate_Fdiv --
   -------------------

   procedure Generate_Fdiv (Unit : in out Tagatha.Code.Instance'Class) is
   begin
      Unit.Operate (Tagatha.Op_Fdiv);
   end Generate_Fdiv;

   -------------------
   -- Generate_Fmul --
   -------------------

   procedure Generate_Fmul (Unit : in out Tagatha.Code.Instance'Class) is
   begin
      Unit.Operate (Tagatha.Op_Fmul);
   end Generate_Fmul;

   -------------------
   -- Generate_Fsub --
   -------------------

   procedure Generate_Fsub (Unit : in out Tagatha.Code.Instance'Class) is
   begin
      Unit.Operate (Tagatha.Op_Fsub);
   end Generate_Fsub;

   --------------------
   -- Generate_Equal --
   --------------------

   procedure Generate_Equal (Unit : in out Tagatha.Code.Instance'Class) is
   begin
      Unit.Operate (Tagatha.Op_EQ);
   end Generate_Equal;

   -----------------
   -- Generate_GE --
   -----------------

   procedure Generate_GE (Unit : in out Tagatha.Code.Instance'Class) is
   begin
      Unit.Operate (Tagatha.Op_GE);
   end Generate_GE;

   -----------------
   -- Generate_GT --
   -----------------

   procedure Generate_GT (Unit : in out Tagatha.Code.Instance'Class) is
   begin
      Unit.Operate (Tagatha.Op_GT);
   end Generate_GT;

   ----------------------
   -- Generate_Implies --
   ----------------------

   procedure Generate_Implies (Unit : in out Tagatha.Code.Instance'Class) is
      False_Label : constant Tagatha.Code.Label := Unit.Next_Label;
      Out_Label : constant Tagatha.Code.Label := Unit.Next_Label;
   begin
      Unit.Swap;
      Unit.Branch (Tagatha.Z, False_Label);
      Unit.Drop;
      Unit.Push_Constant (Tagatha.Int_32'(1));
      Unit.Branch (Out_Label);
      Unit.Set_Label (False_Label);
      Unit.Operate (Tagatha.Op_Not);
      Unit.Set_Label (Out_Label);
   end Generate_Implies;

   ------------------------
   -- Generate_Intrinsic --
   ------------------------

   procedure Generate_Intrinsic
     (Unit : in out Tagatha.Code.Instance'Class;
      Name : Name_Id)
   is
   begin
      if not Have_Primitives then
         Create_Primitives;
      end if;

      if Intrinsic_Features.Contains (To_Standard_String (Name)) then
         Intrinsic_Features.Element (To_Standard_String (Name)) (Unit);
      else
         raise Constraint_Error with
           "no such intrinsic: " & To_Standard_String (Name);
      end if;
   end Generate_Intrinsic;

   ----------------------------------------
   -- Generate_Intrinsic_Mem_Get_Real_64 --
   ----------------------------------------

   procedure Generate_Intrinsic_Mem_Get_Real_64
     (Unit : in out Tagatha.Code.Instance'Class)
   is
   begin
      Unit.Dereference (Tagatha.Floating_Point_Content, 0);
      Unit.Swap;
      Unit.Drop;
   end Generate_Intrinsic_Mem_Get_Real_64;

   ----------------------------------------
   -- Generate_Intrinsic_Mem_Get_Word_32 --
   ----------------------------------------

   procedure Generate_Intrinsic_Mem_Get_Word_32
     (Unit : in out Tagatha.Code.Instance'Class)
   is
   begin
      Unit.Dereference;
      Unit.Swap;
      Unit.Drop;
   end Generate_Intrinsic_Mem_Get_Word_32;

   ----------------------------------------
   -- Generate_Intrinsic_Mem_Put_Real_64 --
   ----------------------------------------

   procedure Generate_Intrinsic_Mem_Put_Real_64
     (Unit : in out Tagatha.Code.Instance'Class)
   is
   begin
      Unit.Swap;
      Unit.Pop_Indirect (Tagatha.Floating_Point_Content);
      Unit.Drop;
   end Generate_Intrinsic_Mem_Put_Real_64;

   ----------------------------------------
   -- Generate_Intrinsic_Mem_Put_Word_32 --
   ----------------------------------------

   procedure Generate_Intrinsic_Mem_Put_Word_32
     (Unit : in out Tagatha.Code.Instance'Class)
   is
   begin
      Unit.Swap;
      Unit.Pop_Indirect;
      Unit.Drop;
   end Generate_Intrinsic_Mem_Put_Word_32;

   ----------------------------
   -- Generate_Intrinsic_One --
   ----------------------------

   procedure Generate_Intrinsic_One
     (Unit : in out Tagatha.Code.Instance'Class)
   is
   begin
      Unit.Drop;
      Unit.Push_Constant (Tagatha.Int_32'(1));
   end Generate_Intrinsic_One;

   ----------------------------------------
   -- Generate_Intrinsic_Replace_Current --
   ----------------------------------------

   procedure Generate_Intrinsic_Replace_Current
     (Unit : in out Tagatha.Code.Instance'Class)
   is
   begin
      Unit.Swap;
      Unit.Pop;
   end Generate_Intrinsic_Replace_Current;

   ----------------------------------
   -- Generate_Intrinsic_Real_One --
   ----------------------------------

   procedure Generate_Intrinsic_Real_One
     (Unit : in out Tagatha.Code.Instance'Class)
   is
   begin
      Unit.Drop;
      Unit.Push_Constant (Tagatha.Floating_Point_Constant'(1.0));
   end Generate_Intrinsic_Real_One;

   ----------------------------------
   -- Generate_Intrinsic_Real_Zero --
   ----------------------------------

   procedure Generate_Intrinsic_Real_Zero
     (Unit : in out Tagatha.Code.Instance'Class)
   is
   begin
      Unit.Drop;
      Unit.Push_Constant (Tagatha.Floating_Point_Constant'(0.0));
   end Generate_Intrinsic_Real_Zero;

   -----------------------------
   -- Generate_Intrinsic_Zero --
   -----------------------------

   procedure Generate_Intrinsic_Zero
     (Unit : in out Tagatha.Code.Instance'Class)
   is
   begin
      Unit.Drop;
      Unit.Push_Constant (Tagatha.Int_32'(0));
   end Generate_Intrinsic_Zero;

   -------------------
   -- Generate_Join --
   -------------------

   procedure Generate_Join (Unit : in out Tagatha.Code.Instance'Class) is
   begin
      Unit.Operate (Tagatha.Op_Add);
   end Generate_Join;

   -----------------
   -- Generate_LE --
   -----------------

   procedure Generate_LE (Unit : in out Tagatha.Code.Instance'Class) is
   begin
      Unit.Operate (Tagatha.Op_LE);
   end Generate_LE;

   -----------------
   -- Generate_LT --
   -----------------

   procedure Generate_LT (Unit : in out Tagatha.Code.Instance'Class) is
   begin
      Unit.Operate (Tagatha.Op_LT);
   end Generate_LT;

   ------------------
   -- Generate_Mod --
   ------------------

   procedure Generate_Mod (Unit : in out Tagatha.Code.Instance'Class) is
   begin
      Unit.Operate (Tagatha.Op_Mod);
   end Generate_Mod;

   -----------------------
   -- Generate_Multiply --
   -----------------------

   procedure Generate_Multiply (Unit : in out Tagatha.Code.Instance'Class) is
   begin
      Unit.Operate (Tagatha.Op_Multiply);
   end Generate_Multiply;

   ---------------------
   -- Generate_Negate --
   ---------------------

   procedure Generate_Negate (Unit : in out Tagatha.Code.Instance'Class) is
   begin
      Unit.Operate (Tagatha.Op_Negate);
   end Generate_Negate;

   ------------------
   -- Generate_Not --
   ------------------

   procedure Generate_Not (Unit : in out Tagatha.Code.Instance'Class) is
   begin
      Unit.Operate (Tagatha.Op_Not);
   end Generate_Not;

   ------------------------
   -- Generate_Not_Equal --
   ------------------------

   procedure Generate_Not_Equal (Unit : in out Tagatha.Code.Instance'Class) is
   begin
      Unit.Operate (Tagatha.Op_NE);
   end Generate_Not_Equal;

   ---------------------------
   -- Generate_Offset_Words --
   ---------------------------

   procedure Generate_Offset_Words
     (Unit : in out Tagatha.Code.Instance'Class)
   is
   begin
      Unit.Push_Constant (Tagatha.Int_32'(4));
      Unit.Operate (Tagatha.Op_Multiply);
      Unit.Operate (Tagatha.Op_Add);
   end Generate_Offset_Words;

   -----------------------
   -- Generate_Operator --
   -----------------------

   function Generate_Operator
     (Unit      : in out Tagatha.Code.Instance'Class;
      Operator  : Name_Id;
      Left_Type : Ack.Types.Type_Entity)
      return Boolean
   is
   begin
      if not Have_Primitives then
         Create_Primitives;
      end if;

      for Rec of Primitive_Operators loop
         if Rec.Operator = Operator
           and then Left_Type.Conforms_To (Rec.Left_Type)
         then
            Rec.Generator (Unit);
            return True;
         elsif Rec.Operator = Operator
           and then Rec.Left_Type.Standard_Name = "any"
         then
            raise Constraint_Error with
              "type " & Left_Type.Description
                & " does not conform to Any";
         end if;
      end loop;
      return False;
   end Generate_Operator;

   -----------------
   -- Generate_Or --
   -----------------

   procedure Generate_Or (Unit : in out Tagatha.Code.Instance'Class) is
   begin
      Unit.Operate (Tagatha.Op_Or);
   end Generate_Or;

   -----------------------
   -- Generate_Subtract --
   -----------------------

   procedure Generate_Subtract (Unit : in out Tagatha.Code.Instance'Class) is
   begin
      Unit.Operate (Tagatha.Op_Subtract);
   end Generate_Subtract;

   -------------------------
   -- Generate_To_Integer --
   -------------------------

   procedure Generate_To_Integer
     (Unit : in out Tagatha.Code.Instance'Class)
   is
   begin
      --  Current is the only operand, and it is the double to convert.
      --  Rounds to nearest: the VM's fix has no other rounding mode, so
      --  truncation towards zero is synthesised in REAL itself.
      Unit.Convert (Tagatha.General_Content);
   end Generate_To_Integer;

   ----------------------
   -- Generate_To_Real --
   ----------------------

   procedure Generate_To_Real
     (Unit : in out Tagatha.Code.Instance'Class)
   is
   begin
      --  Stack is Current then the integer argument.  Convert the argument
      --  into a double of its own before the move, so that this does not
      --  depend on Current being an operand the backend can flot into.
      Unit.Convert (Tagatha.Floating_Point_Content);
      Unit.Swap;
      Unit.Pop;
   end Generate_To_Real;

   ------------------
   -- Generate_Xor --
   ------------------

   procedure Generate_Xor (Unit : in out Tagatha.Code.Instance'Class) is
   begin
      Unit.Operate (Tagatha.Op_Xor);
   end Generate_Xor;

end Ack.Generate.Primitives;
