with WL.String_Maps;

with Aquarius.Trees;

with Ack.Parser.Expressions;

package body Ack.Parser is

   type Import_Function is access
     function (From : Aquarius.Programs.Program_Tree) return Node_Id;

   package Import_Function_Maps is
     new WL.String_Maps (Import_Function);

   Instruction_Imports : Import_Function_Maps.Map;

   function Import_Choice
     (Parent : Aquarius.Programs.Program_Tree;
      Left_Name, Right_Name : String;
      Left_Importer, Right_Importer : not null access
        function (From : Aquarius.Programs.Program_Tree)
      return Node_Id)
      return Node_Id;

   function Import_Class_Declaration
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
     with Pre => From.Name = "class_declaration";

   function Import_Class_Header
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
     with Pre => From.Name = "class_header";

   function Import_Formal_Generic
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
     with Pre => From.Name = "formal_generic";

   function Import_Formal_Generic_Name
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
   is (New_Node (N_Formal_Generic_Name, From,
                 Name =>
                    Get_Name_Id (From.Program_Child ("identifier").Text)))
     with Pre => From.Name = "formal_generic_name";

   function Import_Formal_Generics
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
   is (New_Node (N_Formal_Generics, From,
                 List =>
                    Import_List
                   (From         =>
                       From.Program_Child ("formal_generic_list"),
                    Child_Name   => "formal_generic",
                    Import_Child =>
                       Import_Formal_Generic'Access)))
     with Pre => From.Name = "formal_generics";

   function Import_Note_Item
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
     with Pre => From.Name = "note_item";

   function Import_Note_Entry
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
     with Pre => From.Name = "note_entry";

   function Import_Notes
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
   is (New_Node (N_Notes, From,
                 List =>
                    Import_List (From.Program_Child ("note_list"),
                                 "note_entry",
                                 Import_Note_Entry'Access)))
   with Pre => From.Name = "notes";

   function Import_Inherited
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
     with Pre => From.Name = "inherited";

   function Import_Inheritance
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
   is (New_Node (N_Inheritance, From,
                 List =>
                    Import_List (From.Program_Child ("inherit_list"),
                                 "inherited",
                                 Import_Inherited'Access)))
   with Pre => From.Name = "inheritance";

   function Import_Feature_Name
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
     with Pre => From.Name = "feature_name";

   function Import_Creation_Procedure
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
   is (Import_Feature_Name (From.Program_Child ("feature_name")))
   with Pre => From.Name = "creation_procedure";

   function Import_Creation_Clause
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
   is (New_Node
       (N_Creation_Clause, From,
        List =>
           Import_List
          (From.Program_Child ("creation_procedure_list"),
           "creation_procedure",
           Import_Creation_Procedure'Access)))
   with Pre => From.Name = "creation_clause";

   function Import_Creators
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
   is (New_Node
       (N_Creators, From,
        List => Import_List
          (From, "creation_clause", Import_Creation_Clause'Access)))
   with Pre => From.Name = "creators";

   function Import_Features
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
     with Pre => From.Name = "features";

   function Import_Feature_Clause
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
     with Pre => From.Name = "feature_clause";

   function Import_Feature_Declaration_List
     (From : Aquarius.Programs.Program_Tree)
      return List_Id
     with Pre => From.Name = "feature_declaration_list";

   function Import_Feature_Declaration
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
     with Pre => From.Name = "feature_declaration";

   function Import_New_Feature
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
     with Pre => From.Name = "new_feature";

   function Import_Extended_Feature_Name
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id;

   function Import_Feature_Alias
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
   is (New_Node (N_Feature_Alias, From,
                 Name => Get_Name_Id
                   (Import_String_Constant
                      (From.Program_Child ("string_constant").Text))))
     with Pre => From.Name = "feature_alias";

   function Import_Redefine
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
   is (New_Node (N_Redefine, From,
                 List =>
                    Import_List (From.Program_Child ("feature_list"),
                                 "feature_name",
                                 Import_Feature_Name'Access)))
   with Pre => From.Name = "redefine";

   function Import_Declaration_Body
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
     with Pre => From.Name = "declaration_body";

   function Import_Precondition
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
     with Pre => From.Name = "precondition";

   function Import_Postcondition
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
     with Pre => From.Name = "postcondition";

   function Import_Rescue
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
     with Pre => From.Name = "rescue";

   function Import_Assertion_Clause
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
     with Pre => From.Name = "assertion_clause";

   function Import_Assertion
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
   is (New_Node (N_Assertion, From,
                 List =>
                    Import_List (From, "assertion_clause",
                                 Import_Assertion_Clause'Access)))
   with Pre => From.Name = "assertion";

   function Import_Local_Declarations
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
     with Pre => From.Name = "local_declarations";

   function Import_Formal_Arguments
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
     with Pre => From.Name = "formal_arguments";

   function Import_Entity_Declaration_List
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
     with Pre => From.Name = "entity_declaration_list";

   function Import_Entity_Declaration_Group
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
   with Pre => From.Name = "entity_declaration_group";

   function Import_Type
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
     with Pre => From.Name = "type";

   function Import_Type_Mark
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
   is (Import_Type (From.Program_Child ("type")))
   with Pre => From.Name = "type_mark";

   function Import_Class_Type
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
   with Pre => From.Name = "class_type";

   function Import_Anchored
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
   with Pre => From.Name = "anchored";

   function Import_Tuple_Type
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
   with Pre => From.Name = "tuple_type";

   function Import_Actual_Generics
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
   is (New_Node (N_Actual_Generics, From,
                 List =>
                    Import_List
                   (From         =>
                       From.Program_Child ("type_list"),
                    Child_Name   => "type",
                    Import_Child =>
                       Import_Type'Access)))
   with Pre => From.Name = "actual_generics";

   function Import_Explicit_Value
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id is
     (New_Node (N_Explicit_Value, From,
                Field_2 =>
                   Ack.Parser.Expressions.Import_Manifest_Constant
                  (From.Program_Child ("manifest_constant"))))
   with Pre => From.Name = "explicit_value";

   function Import_Routine
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
   with Pre => From.Name = "routine";

   function Import_Deferred
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id is (No_Node)
   with Pre => From.Name = "deferred";

   function Import_Internal
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
     with Pre => From.Name = "internal";

   function Import_External
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
   with Pre => From.Name = "external";

   function Import_Effective_Routine
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
   is (Import_Choice
       (From, "internal", "external",
        Import_Internal'Access, Import_External'Access))
   with Pre => From.Name = "effective_routine";

   function Import_Instruction
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
     with Pre => From.Name = "instruction";

   function Import_Compound
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
   is (New_Node (N_Compound, From,
                 List =>
                    Import_List
                      (From, "instruction", Import_Instruction'Access)))
     with Pre => From.Name = "compound";

   function Import_Assignment
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
     with Pre => From.Name = "assignment";

   function Import_Creation_Instruction
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
     with Pre => From.Name = "creation_instruction";

   function Import_Explicit_Creation_Call
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
     with Pre => From.Name = "explicit_creation_call";

   function Import_Explicit_Creation_Type
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
     with Pre => From.Name = "explicit_creation_type";

   function Import_Conditional
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
     with Pre => From.Name = "conditional";

   function Import_Loop
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
   with Pre => From.Name = "loop";

   function Import_Check
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
     with Pre => From.Name = "check";

   function Import_Retry
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
     is (New_Node (N_Retry, From))
     with Pre => From.Name = "retry";

   function Import_Variable
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
     with Pre => From.Name = "variable";

   function Import_Feature_Value
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
   is (Import_Choice
       (From, "explicit_value", "routine",
        Import_Explicit_Value'Access, Import_Routine'Access))
   with Pre => From.Name = "feature_value";

   function Import_Feature_Body
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
   is (Import_Choice
       (From, "deferred", "effective_routine",
        Import_Deferred'Access, Import_Effective_Routine'Access))
   with Pre => From.Name = "feature_body";

   ------------
   -- Import --
   ------------

   function Import
     (Program : Aquarius.Programs.Program_Tree)
      return Node_Id
   is
   begin
      return Import_Class_Declaration
        (Program.Program_Child ("class_declaration"));
   end Import;

   ---------------------
   -- Import_Anchored --
   ---------------------

   function Import_Anchored
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
   is
      use Aquarius.Programs;
      Feature_Name : constant String :=
                       From.Program_Child ("feature_name")
                       .Program_Child ("identifier")
                       .Text;
   begin
      return Node : constant Node_Id :=
        New_Node (N_Anchored_Type, From,
                  Name => Get_Name_Id (Feature_Name))
      do
         null;
      end return;
   end Import_Anchored;

   -----------------------------
   -- Import_Assertion_Clause --
   -----------------------------

   function Import_Assertion_Clause
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
   is
      use Aquarius.Programs;
      Tag_Mark : constant Program_Tree := From.Program_Child ("tag_mark");
      Tag      : constant Name_Id :=
                   (if Tag_Mark /= null
                    then Get_Name_Id
                      (Tag_Mark.Program_Child ("tag")
                       .Concatenate_Children)
                    else No_Name);
      Clause   : constant Program_Tree :=
                   From.Program_Child ("unlabeled_assertion_clause");
      Expr     : constant Program_Tree :=
                   Clause.Program_Child ("boolean_expression");
   begin
      return New_Node
        (Kind => N_Assertion_Clause, From => From, Name => Tag,
         Field_1 =>
           Expressions.Import_Expression
             (Expr.Program_Child ("expression")));
   end Import_Assertion_Clause;

   -----------------------
   -- Import_Assignment --
   -----------------------

   function Import_Assignment
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
   is
   begin
      return New_Node
        (N_Assignment, From,
         Field_1 => Import_Variable (From.Program_Child ("variable")),
         Field_2 =>
           Ack.Parser.Expressions.Import_Expression
             (From.Program_Child ("expression")));
   end Import_Assignment;

   -------------------------------
   -- Import_Character_Constant --
   -------------------------------

   function Import_Character_Constant
     (Raw_Text : String)
      return String
   is
   begin
      return [Raw_Text (Raw_Text'First + 1)];
   end Import_Character_Constant;

   ------------------
   -- Import_Check --
   ------------------

   function Import_Check
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
   is
   begin
      return New_Node
        (N_Check, From,
         Field_1 => Import_Assertion (From.Program_Child ("assertion")));
   end Import_Check;

   -------------------
   -- Import_Choice --
   -------------------

   function Import_Choice
     (Parent                        : Aquarius.Programs.Program_Tree;
      Left_Name, Right_Name         : String;
      Left_Importer, Right_Importer : not null access
        function (From : Aquarius.Programs.Program_Tree)
      return Node_Id)
      return Node_Id
   is
      Choice : constant Aquarius.Programs.Program_Tree :=
                 Parent.Chosen_Tree;
   begin
      if Choice.Name = Left_Name then
         return Left_Importer (Choice);
      elsif Choice.Name = Right_Name then
         return Right_Importer (Choice);
      else
         raise Constraint_Error with
         Parent.Name & ": expected choice of "
           & Left_Name & " or " & Right_Name
           & " but found " & Choice.Name;
      end if;
   end Import_Choice;

   ------------------------------
   -- Import_Class_Declaration --
   ------------------------------

   function Import_Class_Declaration
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
   is
      Header_Node : constant Node_Id :=
                      Import_Class_Header
                        (From.Program_Child ("class_header"));
      Notes_Node  : constant Node_Id :=
                      Import_Optional_Child
                        (From, "notes", Import_Notes'Access);
      Inheritance_Node : constant Node_Id :=
                           Import_Optional_Child
                             (From, "inheritance", Import_Inheritance'Access);
      Creators_Node : constant Node_Id :=
                           Import_Optional_Child
                             (From, "creators", Import_Creators'Access);
      Features_Node    : constant Node_Id :=
                        Import_Optional_Child
                             (From, "features", Import_Features'Access);
      Invariant_Node : constant Node_Id := No_Node;
   begin
      return New_Node
        (N_Class_Declaration, From,
         Field_1 => Notes_Node,
         Field_2 => Header_Node,
         Field_3 => Inheritance_Node,
         Field_4 => Creators_Node,
         Field_5 => Features_Node,
         Field_6 => Invariant_Node);
   end Import_Class_Declaration;

   -------------------------
   -- Import_Class_Header --
   -------------------------

   function Import_Class_Header
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
   is
      use Aquarius.Programs;
      Class_Name_Node : constant Node_Id :=
                          Import_Class_Name
                            (From.Program_Child ("class_name"));
      Generics_Node   : constant Node_Id :=
                          Import_Optional_Child
                            (From, "formal_generics",
                             Import_Formal_Generics'Access);
      Header_Mark_Tree : constant Program_Tree :=
                          From.Program_Child ("header_mark");
      Header_Mark      : constant String :=
                           (if Header_Mark_Tree = null
                            then ""
                            else Header_Mark_Tree.Concatenate_Children);
      Deferred        : constant Boolean := Header_Mark = "deferred";
      Expanded        : constant Boolean := Header_Mark = "expanded";
      Frozen          : constant Boolean := Header_Mark = "frozen";
   begin
      Node_Table (Class_Name_Node).Defining := True;
      return New_Node
        (N_Class_Header, From,
         Deferred => Deferred,
         Expanded => Expanded,
         Frozen   => Frozen,
         Field_2  => Class_Name_Node,
         Field_3  => Generics_Node);
   end Import_Class_Header;

   -----------------------
   -- Import_Class_Name --
   -----------------------

   function Import_Class_Name
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
   is
      use Aquarius.Programs;
      List : constant List_Id := New_List;
      Ids  : constant Array_Of_Program_Trees :=
               From.Direct_Children (Skip_Separators => True);
   begin
      for Id of Ids loop
         declare
            Name : constant Name_Id :=
                     Get_Name_Id (Id.Text);
            Node : constant Node_Id :=
                     New_Node (N_Identifier, Id,
                               Name => Name);
         begin
            Append (List, Node);
         end;
      end loop;

      return New_Node
        (N_Class_Name, From,
         List => List);
   end Import_Class_Name;

   -----------------------
   -- Import_Class_Type --
   -----------------------

   function Import_Class_Type
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
   is
      use Aquarius.Programs;
      Actual_Generics_Node : constant Node_Id :=
                               Import_Optional_Child
                                 (From, "actual_generics",
                                  Import_Actual_Generics'Access);
   begin
      return Node : constant Node_Id :=
        New_Node (N_Class_Type, From,
                  Detachable => From.Program_Child ("detachable") /= null,
                  Field_2 =>
                    Import_Class_Name
                      (From.Program_Child ("class_name")),
                  Field_3 => Actual_Generics_Node)
      do
         null;
      end return;
   end Import_Class_Type;

   ------------------------
   -- Import_Conditional --
   ------------------------

   function Import_Conditional
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
   is
      use Aquarius.Programs;

      List : constant List_Id := New_List;
      Children         : constant Array_Of_Program_Trees :=
                           From.Direct_Children;
      Expression       : Program_Tree := null;
   begin
      for T of Children loop
         if T.Name = "boolean_expression" then
            pragma Assert (Expression = null);
            Expression := T.Program_Child ("expression");
         elsif T.Name = "compound" then
            if Expression /= null then
               Append
                 (List,
                  New_Node
                    (N_If_Then, Expression,
                     Field_1 =>
                       Expressions.Import_Expression (Expression),
                     Field_2 => Import_Compound (T)));
               Expression := null;
            else
               Append (List,
                       New_Node
                         (N_If_Then, T,
                          Field_2 => Import_Compound (T)));
            end if;
         end if;
      end loop;
      return New_Node (N_Conditional, From, List => List);
   end Import_Conditional;

   ---------------------------------
   -- Import_Creation_Instruction --
   ---------------------------------

   function Import_Creation_Instruction
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
   is
      use Aquarius.Programs;
      Creation_Call_Tree : constant Program_Tree :=
                             From.Program_Child ("creation_call");
      Variable_Tree      : constant Program_Tree :=
                             Creation_Call_Tree.Program_Child
                               ("variable");
      Identifier_Tree    : constant Program_Tree :=
                             Variable_Tree.Program_Child ("identifier");
      Explicit_Type      : constant Node_Id :=
                             Import_Optional_Child
                               (From, "explicit_creation_type",
                                Import_Explicit_Creation_Type'Access);
      Explict_Call       : constant Node_Id :=
                             Import_Optional_Child
                               (Parent     => Creation_Call_Tree,
                                Child_Name => "explicit_creation_call",
                                Import     =>
                                  Import_Explicit_Creation_Call'Access);
   begin
      return New_Node (N_Creation_Instruction, From,
                       Field_1 =>
                         New_Node
                           (N_Creation_Call, Creation_Call_Tree,
                            Field_1 =>
                              New_Node
                                (N_Variable, Variable_Tree,
                                 Name => Get_Name_Id (Identifier_Tree.Text)),
                            Field_2 => Explict_Call),
                       Field_2 => Explicit_Type);
   end Import_Creation_Instruction;

   -----------------------------
   -- Import_Declaration_Body --
   -----------------------------

   function Import_Declaration_Body
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
   is
      Formal_Arguments_Node : constant Node_Id :=
                                Import_Optional_Child
                                  (From, "formal_arguments",
                                   Import_Formal_Arguments'Access);
      Type_Mark_Node        : constant Node_Id :=
                                Import_Optional_Child
                                  (From, "type_mark",
                                   Import_Type_Mark'Access);
      Feature_Value_Node    : constant Node_Id :=
                                Import_Optional_Child
                                  (From, "feature_value",
                                   Import_Feature_Value'Access);
   begin
      return New_Node (N_Declaration_Body, From,
                       Field_1 => Formal_Arguments_Node,
                       Field_2 => Type_Mark_Node,
                       Field_3 => Feature_Value_Node);
   end Import_Declaration_Body;

   -------------------------------------
   -- Import_Entity_Declaration_Group --
   -------------------------------------

   function Import_Entity_Declaration_Group
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
   is
      use Aquarius.Programs;
      List : constant List_Id := New_List;
      Ids  : constant Array_Of_Program_Trees :=
               From.Program_Child
                 ("identifier_list").Direct_Children ("identifier");
      Group_Type : constant Node_Id :=
                     Import_Type_Mark (From.Program_Child ("type_mark"));
   begin
      for Id of Ids loop
         declare
            Name : constant Name_Id :=
                     Get_Name_Id (Id.Text);
            Node : constant Node_Id :=
                     New_Node (N_Identifier, Id,
                               Name => Name);
         begin
            Append (List, Node);
         end;
      end loop;
      return New_Node (N_Entity_Declaration_Group, From,
                       List => List, Field_1 => Group_Type);
   end Import_Entity_Declaration_Group;

   ------------------------------------
   -- Import_Entity_Declaration_List --
   ------------------------------------

   function Import_Entity_Declaration_List
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
   is
      use Aquarius.Programs;
      List : constant List_Id := New_List;
      Fs   : constant Array_Of_Program_Trees :=
               From.Direct_Children ("entity_declaration_group");
   begin
      for F of Fs loop
         declare
            Node : constant Node_Id :=
                     Import_Entity_Declaration_Group (F);
         begin
            if Node /= No_Node then
               Append (List, Node);
            end if;
         end;
      end loop;

      return New_Node (N_Entity_Declaration_Group_List, From,
                       List => List);

   end Import_Entity_Declaration_List;

   -----------------------------------
   -- Import_Explicit_Creation_Call --
   -----------------------------------

   function Import_Explicit_Creation_Call
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
   is
      use Aquarius.Programs;
      Id : constant Program_Tree := From.Program_Child ("identifier");
   begin
      return New_Node (N_Explicit_Creation_Call, From,
                       Name => Get_Name_Id (Id.Text),
                       Field_2 =>
                         Import_Optional_Child
                           (From, "actuals",
                            Expressions.Import_Actual_Arguments'Access));
   end Import_Explicit_Creation_Call;

   -----------------------------------
   -- Import_Explicit_Creation_Type --
   -----------------------------------

   function Import_Explicit_Creation_Type
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
   is
   begin
      return Import_Type (From.Program_Child ("type"));
   end Import_Explicit_Creation_Type;

   ----------------------------------
   -- Import_Extended_Feature_Name --
   ----------------------------------

   function Import_Extended_Feature_Name
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
   is
      Feature_Name_Node  : constant Node_Id :=
                             Import_Feature_Name
                               (From.Program_Child ("feature_name"));
      Feature_Alias_Node : constant Node_Id :=
                             Import_Optional_Child
                               (From, "feature_alias",
                                Import_Feature_Alias'Access);
   begin
      return New_Node (N_Extended_Feature_Name, From,
                       Field_1 => Feature_Name_Node,
                       Field_2 => Feature_Alias_Node);
   end Import_Extended_Feature_Name;

   ---------------------
   -- Import_External --
   ---------------------

   function Import_External
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
   is
      use Aquarius.Programs;
      Alias_Tree : constant Program_Tree :=
                     From.Program_Child ("external_name");
      Alias_Node : constant Node_Id :=
                     (if Alias_Tree /= null
                      then New_Node
                        (N_Feature_Alias, Alias_Tree,
                         Name => Get_Name_Id
                           (Import_String_Constant
                              (Alias_Tree.Program_Child
                                 ("string_constant").Text)))
                      else No_Node);
   begin
      return New_Node (N_External, From,
                       Name =>
                         Get_Name_Id
                           (Import_String_Constant
                              (From.Program_Child
                                 ("external_language")
                               .Program_Child ("string_constant").Text)),
                       Field_1 => Alias_Node);
   end Import_External;

   ---------------------------
   -- Import_Feature_Clause --
   ---------------------------

   function Import_Feature_Clause
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
   is
      Declaration_List : constant List_Id :=
                           Import_Feature_Declaration_List
                             (From.Program_Child
                                ("feature_declaration_list"));
   begin
      return New_Node (N_Feature_Clause, From,
                       List => Declaration_List);
   end Import_Feature_Clause;

   --------------------------------
   -- Import_Feature_Declaration --
   --------------------------------

   function Import_Feature_Declaration
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
   is
      New_Feature_List : constant List_Id :=
                           Import_List
                             (From.Program_Child ("new_feature_list"),
                              "new_feature",
                              Import_New_Feature'Access);
   begin
      return New_Node (N_Feature_Declaration, From,
                       List => New_Feature_List,
                       Field_1 =>
                         Import_Declaration_Body
                           (From.Program_Child
                              ("declaration_body")));
   end Import_Feature_Declaration;

   -------------------------------------
   -- Import_Feature_Declaration_List --
   -------------------------------------

   function Import_Feature_Declaration_List
     (From : Aquarius.Programs.Program_Tree)
      return List_Id
   is
      use Aquarius.Programs;
      Decs : constant Array_Of_Program_Trees :=
               From.Direct_Children
                 ("feature_declaration");
      List : constant List_Id := New_List;
   begin
      for Dec of Decs loop
         declare
            Node : constant Node_Id :=
                     Import_Feature_Declaration (Dec);
         begin
            if Node in Real_Node_Id then
               Append (List, Node);
            end if;
         end;
      end loop;
      return List;
   end Import_Feature_Declaration_List;

   -------------------------
   -- Import_Feature_Name --
   -------------------------

   function Import_Feature_Name
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
   is
   begin
      return New_Node
        (Kind     => N_Feature_Name,
         From     => From,
         Name     =>
           Get_Name_Id
             (From.Program_Child ("identifier").Text));
   end Import_Feature_Name;

   ---------------------
   -- Import_Features --
   ---------------------

   function Import_Features
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
   is
      use Aquarius.Programs;
      List : constant List_Id := New_List;
      Fs   : constant Array_Of_Program_Trees :=
             From.Direct_Children;
   begin
      for F of Fs loop
         Append (List, Import_Feature_Clause (F));
      end loop;

      return New_Node (N_Features, From,
                       List => List);
   end Import_Features;

   -----------------------------
   -- Import_Formal_Arguments --
   -----------------------------

   function Import_Formal_Arguments
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
   is
      use Aquarius.Programs;
      Child : constant Program_Tree :=
                From.Program_Child ("entity_declaration_list");
      Node  : constant Node_Id :=
                (if Child = null then No_Node
                 else Import_Entity_Declaration_List (Child));
   begin
      if Node = No_Node then
         return Node;
      else
         return New_Node (N_Formal_Arguments, From, Field_1 => Node);
      end if;
   end Import_Formal_Arguments;

   ----------------------------
   -- Import_Formal_Generics --
   ----------------------------

   function Import_Formal_Generic
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
   is
   begin
      return New_Node (N_Formal_Generic, From,
                       Field_1 =>
                         Import_Formal_Generic_Name
                           (From.Program_Child ("formal_generic_name")));
   end Import_Formal_Generic;

   ----------------------
   -- Import_Inherited --
   ----------------------

   function Import_Inherited
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
   is
      use Aquarius.Programs;
      function Import_Redefine_Adaption
        (Feature_Adaption : Aquarius.Programs.Program_Tree)
         return Node_Id;

      ------------------------------
      -- Import_Redefine_Adaption --
      ------------------------------

      function Import_Redefine_Adaption
        (Feature_Adaption : Aquarius.Programs.Program_Tree)
         return Node_Id
      is
      begin
         return Import_Optional_Child
           (Feature_Adaption, "redefine", Import_Redefine'Access);
      end Import_Redefine_Adaption;

      Redefines : constant Node_Id :=
                    Import_Optional_Child
                      (From, "feature_adaptation",
                       Import_Redefine_Adaption'Access);
   begin
      return New_Node (N_Inherited, From,
                       Field_1 =>
                         Import_Class_Type
                           (From.Program_Child ("class_type")),
                       Field_4 => Redefines);
   end Import_Inherited;

   ------------------------
   -- Import_Instruction --
   ------------------------

   function Import_Instruction
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
   is
      Choice : constant Aquarius.Programs.Program_Tree :=
                 From.Chosen_Tree;
      Choice_Name : constant String := Choice.Name;
      Choice_Node : Node_Id;
   begin
      if Instruction_Imports.Is_Empty then
         declare
            procedure Insert
              (Name : String;
               F    : Import_Function);

            ------------
            -- Insert --
            ------------

            procedure Insert
              (Name : String;
               F    : Import_Function)
            is
            begin
               Instruction_Imports.Insert (Name, F);
            end Insert;
         begin
            Insert ("assignment", Import_Assignment'Access);
            Insert ("creation_instruction",
                    Import_Creation_Instruction'Access);
            Insert ("conditional", Import_Conditional'Access);
            Insert ("loop", Import_Loop'Access);
            Insert ("check", Import_Check'Access);
            Insert ("retry", Import_Retry'Access);
            Insert ("precursor", Expressions.Import_Precursor'Access);
         end;
      end if;

      if Instruction_Imports.Contains (Choice_Name) then
         Choice_Node := Instruction_Imports.Element (Choice_Name) (Choice);
      else
         raise Constraint_Error with
         From.Show_Location & ": " & From.Name
           & ": expected an instruction but found " & Choice_Name;
      end if;

      return Choice_Node;

   end Import_Instruction;

   ---------------------
   -- Import_Internal --
   ---------------------

   function Import_Internal
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
   is
      Once : constant Boolean :=
               From.Program_Child ("routine_mark").Chosen_Tree.Name = "once";
   begin
      return New_Node (N_Internal, From,
                       Once    => Once,
                       Field_1 =>
                         Import_Compound
                           (From.Program_Child ("compound")));
   end Import_Internal;

   -----------------
   -- Import_List --
   -----------------

   function Import_List
     (From         : Aquarius.Programs.Program_Tree;
      Child_Name   : String;
      Import_Child : not null access
        function (Child : Aquarius.Programs.Program_Tree)
      return Node_Id)
      return List_Id
   is
      use Aquarius.Programs;
      List     : constant List_Id := New_List;
      Children : constant Array_Of_Program_Trees :=
                   From.Direct_Children (Child_Name);
   begin
      for Child of Children loop
         declare
            Node : constant Node_Id :=
                     Import_Child (Child);
         begin
            Append (List, Node);
         end;
      end loop;
      return List;
   end Import_List;

   -------------------------------
   -- Import_Local_Declarations --
   -------------------------------

   function Import_Local_Declarations
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
   is
      use Aquarius.Programs;
      Child : constant Program_Tree :=
                From.Program_Child ("entity_declaration_list");
      Node  : constant Node_Id :=
                (if Child = null then No_Node
                 else Import_Entity_Declaration_List (Child));
   begin
      if Node = No_Node then
         return Node;
      else
         return New_Node (N_Local_Declarations, From, Field_1 => Node);
      end if;
   end Import_Local_Declarations;

   -----------------
   -- Import_Loop --
   -----------------

   function Import_Loop
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
   is

      function Import_Iteration
        (Tree : Aquarius.Programs.Program_Tree)
         return Node_Id
      is (New_Node (N_Iteration, Tree,
                    Name =>
                       Get_Name_Id (Tree.Program_Child ("identifier").Text),
                    Field_1 =>
                       Expressions.Import_Expression
                         (Tree.Program_Child ("expression"))));

      function Import_Initialization
        (Tree : Aquarius.Programs.Program_Tree)
         return Node_Id
      is (New_Node (N_Initialization, Tree,
                    Field_1 =>
                       Import_Compound (Tree.Program_Child ("compound"))));

      function Import_Invariant
        (Tree : Aquarius.Programs.Program_Tree)
         return Node_Id
      is (if Tree.Name = "" then No_Node else No_Node);

      function Import_Exit_Condition
        (Tree : Aquarius.Programs.Program_Tree)
         return Node_Id
      is (New_Node (N_Exit_Condition, Tree,
                    Field_1 =>
                       Expressions.Import_Expression
                      (Tree.Program_Child ("boolean_expression")
                       .Program_Child ("expression"))));

      function Import_Variant
        (Tree : Aquarius.Programs.Program_Tree)
         return Node_Id
      is (if Tree.Name = "" then No_Node else No_Node);

      Loop_Body : constant Aquarius.Programs.Program_Tree :=
                    From.Program_Child ("loop_body");
   begin
      return New_Node
        (N_Loop, From,
         Field_1 =>
           Import_Optional_Child (From, "iteration", Import_Iteration'Access),
         Field_2 =>
           Import_Optional_Child (From, "initialization",
             Import_Initialization'Access),
         Field_3 =>
           Import_Optional_Child (From, "invariant",
             Import_Invariant'Access),
         Field_4 =>
           Import_Optional_Child (From, "exit_condition",
             Import_Exit_Condition'Access),
         Field_5 =>
           New_Node
             (N_Loop_Body, Loop_Body,
              Field_1 =>
                Import_Compound (Loop_Body.Program_Child ("compound"))),
         Field_6 =>
           Import_Optional_Child (From, "variant",
             Import_Variant'Access));
   end Import_Loop;

   ------------------------
   -- Import_New_Feature --
   ------------------------

   function Import_New_Feature
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
   is
      use Aquarius.Programs;
      Name_Node : constant Node_Id :=
                    Import_Extended_Feature_Name
                      (From.Program_Child ("extended_feature_name"));
      Frozen_Tree : constant Program_Tree :=
                      From.Program_Child ("frozen");
   begin
      return New_Node
        (Kind     => N_New_Feature,
         From     => From,
         Frozen   => Frozen_Tree /= null and then Frozen_Tree.Is_Filled,
         Field_1  => Name_Node);
   end Import_New_Feature;

   -----------------------
   -- Import_Note_Entry --
   -----------------------

   function Import_Note_Entry
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
   is
      use Aquarius.Programs;
      Name_Tree : constant Program_Tree :=
                    From.Program_Child ("note_name");
      Value_Tree : constant Program_Tree :=
                     From.Program_Child ("note_value");
      Name_Node : constant Node_Id :=
                     New_Node (N_Note_Name, Name_Tree,
                               Name =>
                                 Get_Name_Id
                                   (Name_Tree.Program_Child
                                        ("identifier").Text));
      Value_Node : constant Node_Id :=
                     New_Node (N_Note_Value, Value_Tree,
                               List => Import_List
                                 (From         => Value_Tree,
                                  Child_Name   => "note_item",
                                  Import_Child => Import_Note_Item'Access));
   begin
      return New_Node (N_Note_Entry, From,
                       Field_1 => Name_Node,
                       Field_2 => Value_Node);
   end Import_Note_Entry;

   ----------------------
   -- Import_Note_Item --
   ----------------------

   function Import_Note_Item
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
   is
      function Import_Note_Identifier
        (Id : Aquarius.Programs.Program_Tree)
         return Node_Id
      is (New_Node (N_Identifier, Id,
                    Name => Get_Name_Id (Id.Text)));
   begin
      return New_Node (N_Note_Item, From,
                       Field_1 =>
                         Import_Choice
                           (Parent         => From,
                            Left_Name      => "identifier",
                            Right_Name     => "manifest_constant",
                            Left_Importer  => Import_Note_Identifier'Access,
                            Right_Importer =>
                              Expressions.Import_Manifest_Constant'Access));
   end Import_Note_Item;

   ---------------------------
   -- Import_Optional_Child --
   ---------------------------

   function Import_Optional_Child
     (Parent     : Aquarius.Programs.Program_Tree;
      Child_Name : String;
      Import     : not null access
        function (Child : Aquarius.Programs.Program_Tree)
      return Node_Id)
      return Node_Id
   is
      use Aquarius.Programs, Aquarius.Trees;
      Children : constant Array_Of_Trees := Parent.Get_Named_Children;
   begin
      for Child of Children loop
         if Child.Name = Child_Name then
            return Import (Program_Tree (Child));
         end if;
      end loop;

      return No_Node;
   end Import_Optional_Child;

   --------------------------
   -- Import_Postcondition --
   --------------------------

   function Import_Postcondition
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
   is
   begin
      return New_Node (N_Postcondition, From,
                       Field_1 =>
                         Import_Assertion (From.Program_Child ("assertion")));
   end Import_Postcondition;

   -------------------------
   -- Import_Precondition --
   -------------------------

   function Import_Precondition
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
   is
      use type Aquarius.Programs.Program_Tree;
      Require_Else : constant Aquarius.Programs.Program_Tree :=
                       From.Program_Child ("else");
   begin
      return New_Node (N_Precondition, From,
                       Inherited =>
                         Require_Else /= null
                       and then Require_Else.Is_Filled,
                       Field_1 =>
                         Import_Assertion (From.Program_Child ("assertion")));
   end Import_Precondition;

   -------------------
   -- Import_Rescue --
   -------------------

   function Import_Rescue
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
   is
   begin
      return New_Node (N_Rescue, From,
                       Field_1 =>
                         Import_Compound (From.Program_Child ("compound")));
   end Import_Rescue;

   --------------------
   -- Import_Routine --
   --------------------

   function Import_Routine
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
   is
      Precondition       : constant Node_Id :=
                             Import_Optional_Child
                               (From, "precondition",
                                Import_Precondition'Access);
      Local_Declarations : constant Node_Id :=
                             Import_Optional_Child
                               (From, "local_declarations",
                                Import_Local_Declarations'Access);
      Feature_Body       : constant Node_Id :=
                             Import_Feature_Body
                               (From.Program_Child ("feature_body"));
      Postcondition      : constant Node_Id :=
                             Import_Optional_Child
                               (From, "postcondition",
                                Import_Postcondition'Access);
      Rescue             : constant Node_Id :=
                             Import_Optional_Child
                               (From, "rescue",
                                Import_Rescue'Access);
   begin
      return New_Node (N_Routine, From,
                       Deferred => Feature_Body = No_Node,
                       Field_1  => Precondition,
                       Field_2  => Local_Declarations,
                       Field_3  => Feature_Body,
                       Field_4  => Postcondition,
                       Field_5  => Rescue);
   end Import_Routine;

   ----------------------------
   -- Import_String_Constant --
   ----------------------------

   function Import_String_Constant
     (Raw_Text : String)
      return String
   is
      Result : String (1 .. Raw_Text'Length) := Raw_Text;
      Count  : Natural := 0;
      Index  : Positive := 1;
   begin
      while Index <= Result'Last loop
         if (Index = 1 or else Index = Result'Last)
           and then Result (Index) = '"'
         then
            Index := Index + 1;
         elsif Index < Result'Last - 1
           and then Result (Index) = '"'
           and then Result (Index + 1) = '"'
         then
            Index := Index + 2;
            Count := Count + 1;
            Result (Count) := '"';
         else
            Count := Count + 1;
            Result (Count) := Result (Index);
            Index := Index + 1;
         end if;
      end loop;

      return Result (1 .. Count);
   end Import_String_Constant;

   -----------------------
   -- Import_Tuple_Type --
   -----------------------

   function Import_Tuple_Type
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
   is
   begin
      return Node : constant Node_Id :=
        New_Node (N_Tuple_Type, From,
                  List =>
                    Import_List
                      (From, "type", Import_Type'Access));
   end Import_Tuple_Type;

   -----------------
   -- Import_Type --
   -----------------

   function Import_Type
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
   is
      use Aquarius.Programs;
      Choice_Tree : constant Program_Tree := From.Chosen_Tree;
      Choice_Node : Node_Id;
   begin
      if Choice_Tree.Name = "class_type" then
         Choice_Node := Import_Class_Type (Choice_Tree);
      elsif Choice_Tree.Name = "anchored" then
         Choice_Node := Import_Anchored (Choice_Tree);
      elsif Choice_Tree.Name = "tuple_type" then
         Choice_Node := Import_Tuple_Type (Choice_Tree);
      else
         raise Constraint_Error with
           "invalid type choice: " & Choice_Tree.Name;
      end if;

      return Choice_Node;

   end Import_Type;

   ---------------------
   -- Import_Variable --
   ---------------------

   function Import_Variable
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
   is
   begin
      return New_Node (N_Variable, From,
                       Name =>
                         Get_Name_Id
                           (From.Program_Child ("identifier").Text));
   end Import_Variable;

end Ack.Parser;
