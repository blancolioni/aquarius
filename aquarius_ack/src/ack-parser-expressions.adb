package body Ack.Parser.Expressions is

   generic
      Prefix_Operators : String;
      Sub_Expressions  : String;
      Infix_Operators  : String;
      with function Import_Sub_Expression
        (Program : Aquarius.Programs.Program_Tree)
         return Node_Id;
   function Generic_Import
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id;

   function Import_Attachment_Test
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id;

   function Import_Primary
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id;

   function Import_Precursor_Element
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id;

   function Import_Actual_List
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
   is (New_Node
       (N_Actual_List,
        From,
        List =>
           Import_List
          (From         => From,
           Child_Name   => "expression",
           Import_Child => Import_Expression'Access)));

   --------------------
   -- Generic_Import --
   --------------------

   function Generic_Import
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
   is
      use Aquarius.Programs;
      Children : constant Array_Of_Program_Trees := From.Direct_Children;
      Index    : Positive := Children'First;
      Prefix   : Program_Tree;
      Node     : Node_Id;
   begin
      if Children (Index).Name = Prefix_Operators then
         Prefix := Children (Index);
         Index := Index + 1;
      end if;

      if Children (Index).Name /= Sub_Expressions then
         raise Constraint_Error with
           "expected a child called '" & Sub_Expressions
           & "' but found '" & Children (Index).Name & "'";
      end if;

      pragma Assert (Index <= Children'Last);
      pragma Assert (Children (Index).Name = Sub_Expressions);

      Node := Import_Sub_Expression (Children (Index));
      Index := Index + 1;

      while Index <= Children'Last loop
         declare
            Operator : constant Program_Tree := Children (Index);
            Right    : constant Program_Tree := Children (Index + 1);
         begin
            pragma Assert (Operator.Name = Infix_Operators);
            pragma Assert (Right.Name = Sub_Expressions);

            Index := Index + 2;
            Node := New_Node (N_Operator, Operator,
                              Field_1 => Node,
                              Field_2 => Import_Sub_Expression (Right),
                              Name    =>
                                Get_Name_Id (Operator.Concatenate_Children));
         end;
      end loop;

      if Prefix /= null then
         Node := New_Node (N_Operator, From,
                           Field_1 => Node,
                           Name    =>
                             Get_Name_Id (Prefix.Concatenate_Children));
      end if;

      return Node;
   end Generic_Import;

   -----------------------------
   -- Import_Actual_Arguments --
   -----------------------------

   function Import_Actual_Arguments
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
   is
   begin
      return Import_Actual_List (From.Program_Child ("actual_list"));
   end Import_Actual_Arguments;

   ----------------------------
   -- Import_Attachment_Test --
   ----------------------------

   function Import_Attachment_Test
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
   is
      use Aquarius.Programs;
      Children : constant Array_Of_Program_Trees :=
                   From.Direct_Children;
      Expression : Node_Id := No_Node;
      Identifier : Name_Id := No_Name;
   begin
      for Child of Children loop
         if Child.Name = "expression" then
            Expression := Import_Expression (Child);
         elsif Child.Name = "identifier" then
            Identifier := Get_Name_Id (Child.Text);
         end if;
      end loop;
      return New_Node (N_Attachment_Test, From,
                       Name => Identifier,
                       Field_1 => Expression);
   end Import_Attachment_Test;

   -----------------------
   -- Import_Expression --
   -----------------------

   function Import_Expression
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
   is
      use Aquarius.Programs;

      function Import_Factor is
        new Generic_Import
          ("unary_primary_operator", "primary", "",
           Import_Primary);

      function Import_Term is
        new Generic_Import
          ("", "factor", "multiplying_operator",
           Import_Factor);

      function Import_Simple_Expression is
        new Generic_Import
          ("unary_adding_operator", "term",
           "binary_adding_operator",
           Import_Term);

      function Import_Range_Expression is
        new Generic_Import
          ("", "simple_expression",
           "range_operator",
           Import_Simple_Expression);

      function Import_Relation is
        new Generic_Import
          ("", "range_expression",
           "relational_operator",
           Import_Range_Expression);

      function Import_Top_Level is
        new Generic_Import
          ("", "relation",
           "boolean_operator",
           Import_Relation);

      Top_Choice : constant Program_Tree := From.Chosen_Tree;
   begin
      if Top_Choice.Name = "operator_expression" then
         return Import_Top_Level (Top_Choice);
      elsif Top_Choice.Name = "attachment_test" then
         return Import_Attachment_Test (Top_Choice);
      else
         raise Constraint_Error with
           "expected an expression but found '" & Top_Choice.Name & "'";
      end if;
   end Import_Expression;

   ------------------------------
   -- Import_Manifest_Constant --
   ------------------------------

   function Import_Manifest_Constant
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
   is
      use Aquarius.Programs;
      Value : constant Program_Tree := From.Program_Child ("manifest_value");
      Choice : constant Program_Tree := Value.Chosen_Tree;
      Kind   : Node_Kind;
      Text   : Text_Id := No_Text;
      Name   : Name_Id := No_Name;
   begin
      if Choice.Name = "string_constant" then
         Kind := N_String_Constant;
         Text :=
           Make_Text_Id
             (Import_String_Constant (Choice.Concatenate_Children));
      elsif Choice.Name = "character_constant" then
         Kind := N_Character_Constant;
         Name :=
           Get_Name_Id
             (Import_Character_Constant (Choice.Concatenate_Children));
      elsif Choice.Name = "integer_constant" then
         Kind := N_Integer_Constant;
         Name := Get_Name_Id (Choice.Concatenate_Children);
      elsif Choice.Name = "boolean_constant" then
         Kind := N_Boolean_Constant;
         Name := Get_Name_Id (Choice.Concatenate_Children);
      else
         raise Constraint_Error with
           "unhandled constant type: " & Choice.Name;
      end if;

      return New_Node
        (N_Constant, From,
         Field_2 =>
           New_Node
             (Kind, Choice,
              Name => Name,
              Text => Text));
   end Import_Manifest_Constant;

   ----------------------
   -- Import_Precursor --
   ----------------------

   function Import_Precursor
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
   is
      use Aquarius.Programs;
      Es : constant Array_Of_Program_Trees :=
             From.Direct_Children (Skip_Separators => True);
      List : constant List_Id := New_List;

   begin
      for E of Es loop
         Append (List, Import_Precursor_Element (E));
--                   New_Node
--                     (N_Identifier, E,
--                      Name =>
--                        Get_Name_Id (E.Program_Child ("identifier").Text)));
      end loop;
      return New_Node (N_Precursor, From, List => List);
   end Import_Precursor;

   ------------------------------
   -- Import_Precursor_Element --
   ------------------------------

   function Import_Precursor_Element
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
   is
      use Aquarius.Programs;

      Id : constant String := From.Program_Child ("identifier").Text;
      Actual_Node : constant Node_Id :=
                      Import_Optional_Child
                        (From, "actuals",
                         Import_Actual_Arguments'Access);
   begin
      return New_Node (N_Precursor_Element, From,
                       Name => Get_Name_Id (Id),
                       Field_1 => No_Node,
                       Field_2 => Actual_Node);
   end Import_Precursor_Element;

   --------------------
   -- Import_Primary --
   --------------------

   function Import_Primary
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
   is
      use Aquarius.Programs;
      Choice : constant Aquarius.Programs.Program_Tree :=
                 From.Chosen_Tree;
   begin
      if Choice.Name = "manifest_constant" then
         return Import_Manifest_Constant (Choice);
      elsif Choice.Name = "precursor" then
         return Import_Precursor (Choice);
      elsif Choice.Name = "old" then
         return New_Node (N_Old, From,
                          Field_1 =>
                            Import_Primary
                              (Choice.Program_Child ("primary")));
      elsif Choice.First_Child.Text = "(" then
         declare
            Exprs : constant Array_Of_Program_Trees :=
                      Choice.Direct_Children ("expression");
         begin
            if Exprs'Length = 1 then
               return Import_Expression (Exprs (Exprs'First));
            else
               return New_Node (N_Tuple, From,
                                List =>
                                  Import_List
                                    (From, "expression",
                                     Import_Expression'Access));
            end if;
         end;
      else
         return No_Node;
      end if;
   end Import_Primary;

end Ack.Parser.Expressions;
