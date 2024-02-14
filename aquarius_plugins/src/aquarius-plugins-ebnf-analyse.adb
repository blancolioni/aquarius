with Ada.Exceptions;

with Aquarius.Errors;
with Aquarius.Grammars;
with Aquarius.Grammars.Builtin;
with Aquarius.Lexers;
with Aquarius.Lexers.Parser;
with Aquarius.Programs;                 use Aquarius.Programs;
with Aquarius.Syntax;
with Aquarius.Tokens;
with Aquarius.Trees;

package body Aquarius.Plugins.EBNF.Analyse is

   procedure Copy_Syntax
     (From : not null access Aquarius.Trees.Root_Tree_Type'Class;
      To   : not null access Aquarius.Trees.Root_Tree_Type'Class);

   function Get_Syntax
     (Tree : Aquarius.Trees.Root_Tree_Type'Class)
      return Aquarius.Syntax.Syntax_Tree
   is (Aquarius.Syntax.Syntax_Tree
       (Tree.Property (Aquarius.Properties.Syntax_Property)));

   function Get_Syntax
     (Tree : not null access constant Aquarius.Trees.Root_Tree_Type'Class)
     return Aquarius.Syntax.Syntax_Tree
   is (Aquarius.Syntax.Syntax_Tree
       (Tree.Property (Aquarius.Properties.Syntax_Property)));

   procedure Set_Syntax
     (Tree  : in out Aquarius.Trees.Root_Tree_Type'Class;
      Value : Aquarius.Syntax.Syntax_Tree);

   procedure Set_Syntax
     (Tree  : not null access Aquarius.Trees.Root_Tree_Type'Class;
      Value : Aquarius.Syntax.Syntax_Tree);

   function String_To_Value
     (Raw_String : String)
      return String;

   --------------------------------------
   -- After_Cross_Reference_Definition --
   --------------------------------------

   procedure After_Cross_Reference_Definition
     (Target : not null access Aquarius.Actions.Actionable'Class)
   is
      Tree           : constant Program_Tree := Program_Tree (Target);
      Grammar        : constant Aquarius.Grammars.Aquarius_Grammar :=
                         Aquarius.Grammars.Get_Grammar (Tree.all);
      Reference_Node : constant Program_Tree :=
                         Tree.Program_Child ("identifier");
      Rule              : constant Aquarius.Syntax.Syntax_Tree :=
                            Grammar.Reference_Name
                              (Reference_Node, Reference_Node.Text);
      Child_Item     : constant Program_Tree :=
                         Tree.Program_Child ("terminal-or-rule");
      Terminal      : constant Program_Tree :=
        Child_Item.Program_Child ("terminal");
      Non_Terminal  : constant Program_Tree :=
        Child_Item.Program_Child ("identifier");
   begin

      if Terminal /= null then
         declare
            Terminal_Name : constant String := Terminal.Text;
            Token         : constant String :=
              Terminal_Name (Terminal_Name'First + 1 ..
                               Terminal_Name'Last - 1);
            Name_Child    : constant Aquarius.Syntax.Syntax_Tree :=
                              Grammar.Reference_Terminal (Terminal, Token);
         begin
            Rule.Set_Cross_Reference (Name_Child);
         end;
      else
         declare
            Non_Terminal_Name : constant String := Non_Terminal.Text;
            Name_Child        : constant Aquarius.Syntax.Syntax_Tree :=
              Grammar.Reference_Name (Non_Terminal, Non_Terminal_Name);
         begin
            Rule.Set_Cross_Reference (Name_Child);
         end;
      end if;

   end After_Cross_Reference_Definition;

   -----------------------------
   -- After_Format_Definition --
   -----------------------------

   procedure After_Format_Definition
     (Target : not null access Aquarius.Actions.Actionable'Class)
   is
      Tree          : constant Program_Tree := Program_Tree (Target);
      Grammar       : constant Aquarius.Grammars.Aquarius_Grammar :=
        Aquarius.Grammars.Get_Grammar (Tree.all);
      Format_List   : constant Program_Tree :=
        Tree.Program_Child ("list-of-formats");
      Format_Rules  : constant Aquarius.Trees.Array_Of_Trees :=
        Format_List.Get_Matching_Children ("identifier");
      Format        : Aquarius.Formats.Aquarius_Format;
      Format_Item   : constant Program_Tree :=
        Tree.Program_Child ("terminal-or-rule");
      Terminal      : constant Program_Tree :=
        Format_Item.Program_Child ("terminal");
      Non_Terminal  : constant Program_Tree :=
        Format_Item.Program_Child ("identifier");
   begin

      if Format_Rules'Length = 1 then
         Format :=
           Aquarius.Formats.To_Format
           (Format_Rules (Format_Rules'First).Text);
      else
         for I in Format_Rules'Range loop
            declare
               Name : constant String := Format_Rules (I).Text;
               Rule : constant Aquarius.Formats.Format_Rule :=
              Aquarius.Formats.To_Rule (Name);
            begin
               Aquarius.Formats.Add_Rule (Format, Rule);
            end;
         end loop;
      end if;

      if Terminal /= null then
         declare
            Terminal_Name : constant String := Terminal.Text;
            Token         : constant String :=
              Terminal_Name (Terminal_Name'First + 1 ..
                               Terminal_Name'Last - 1);
            Rule          : constant Aquarius.Syntax.Syntax_Tree :=
              Grammar.Reference_Terminal (Terminal, Token);
         begin
            Rule.Set_Format (Format);
         end;
      else
         declare
            Non_Terminal_Name : constant String := Non_Terminal.Text;
            Rule              : constant Aquarius.Syntax.Syntax_Tree :=
              Grammar.Reference_Name (Non_Terminal, Non_Terminal_Name);
         begin
            Rule.Set_Format (Format);
         end;
      end if;

   end After_Format_Definition;

   -----------------------
   -- After_Nested_Rule --
   -----------------------

   procedure After_Nested_Rule
     (Target : not null access Aquarius.Actions.Actionable'Class)
   is
      Tree         : constant Aquarius.Trees.Tree :=
                       Aquarius.Trees.Tree (Target);
      S_Body       : constant Aquarius.Trees.Tree :=
        Tree.Breadth_First_Search ("syntax-body");
   begin
      Set_Syntax (Tree.all, Get_Syntax (S_Body.all));
   end After_Nested_Rule;

   -------------------------
   -- After_Optional_Rule --
   -------------------------

   procedure After_Optional_Rule
     (Target : not null access Aquarius.Actions.Actionable'Class)
   is
      use Aquarius.Syntax;
      Tree         : constant Program_Tree := Program_Tree (Target);
      Grammar      : constant Aquarius.Grammars.Aquarius_Grammar :=
                       Aquarius.Grammars.Get_Grammar (Tree.all);
      Frame        : constant Aquarius.Tokens.Token_Frame :=
        Grammar.Frame;
      Option : constant Syntax_Tree := New_Optional (Frame, Tree);
      Child  : constant Aquarius.Trees.Tree :=
        Tree.Breadth_First_Search ("sequence-of-rules");
   begin

      Option.Add_Child (Get_Syntax (Child));
      Set_Syntax (Tree, Option);
   end After_Optional_Rule;

   --------------------------------
   -- After_Repeat_Optional_Rule --
   --------------------------------

   procedure After_Repeat_Optional_Rule
     (Target : not null access Aquarius.Actions.Actionable'Class)
   is
      use Aquarius.Syntax;
      Tree         : constant Program_Tree := Program_Tree (Target);
      Grammar      : constant Aquarius.Grammars.Aquarius_Grammar :=
        Aquarius.Grammars.Get_Grammar (Tree.all);
      Frame        : constant Aquarius.Tokens.Token_Frame :=
        Grammar.Frame;
      Repeat : constant Syntax_Tree :=
        New_Repeat (Frame, Tree, True, null);
      Child  : constant Program_Tree :=
        Program_Tree (Tree.Breadth_First_Search ("repeater"));
   begin
      Repeat.Add_Child (Get_Syntax (Child));
      Set_Syntax (Tree, Repeat);

      if Child.Has_Property (Global_Plugin.Separator) then
         Repeat.Set_Separator
           (Syntax_Tree (Child.Property (Global_Plugin.Separator)));
      end if;
   end After_Repeat_Optional_Rule;

   --------------------------------
   -- After_Repeat_Required_Rule --
   --------------------------------

   procedure After_Repeat_Required_Rule
     (Target : not null access Aquarius.Actions.Actionable'Class)
   is
      use Aquarius.Syntax;
      Tree         : constant Program_Tree := Program_Tree (Target);
      Grammar      : constant Aquarius.Grammars.Aquarius_Grammar :=
        Aquarius.Grammars.Get_Grammar (Tree.all);
      Frame        : constant Aquarius.Tokens.Token_Frame :=
        Grammar.Frame;
      Repeat : constant Syntax_Tree :=
        New_Repeat (Frame, Tree, False, null);
      Child  : constant Program_Tree :=
        Program_Tree (Tree.Breadth_First_Search ("repeater"));
   begin
      Repeat.Add_Child (Get_Syntax (Child));
      Set_Syntax (Tree, Repeat);
      if Child.Has_Property (Global_Plugin.Separator) then
         Repeat.Set_Separator
           (Syntax_Tree (Child.Property (Global_Plugin.Separator)));
      end if;
   end After_Repeat_Required_Rule;

   --------------------
   -- After_Repeater --
   --------------------

   procedure After_Repeater
     (Target : not null access Aquarius.Actions.Actionable'Class)
   is
      Tree         : constant Program_Tree := Program_Tree (Target);
      Child  : constant Aquarius.Trees.Tree :=
        Tree.Breadth_First_Search ("sequence-of-rules");
      Separator_Child : constant Program_Tree :=
        Program_Tree (Tree.Breadth_First_Search ("separator"));
   begin
      Set_Syntax
        (Tree, Get_Syntax (Child));

      if Separator_Child /= null then
         Tree.Set_Property
           (Global_Plugin.Separator,
            Get_Syntax (Separator_Child));
      end if;
   end After_Repeater;

   ----------------
   -- After_Rule --
   ----------------

   procedure After_Rule
     (Target : not null access Aquarius.Actions.Actionable'Class)
   is
      Tree         : constant Program_Tree := Program_Tree (Target);
      Child : Aquarius.Trees.Tree := Tree.First_Child;
   begin
      while Child.Name = "" loop
         Child := Child.First_Child;
      end loop;
      Copy_Syntax (Child, Tree);
   end After_Rule;

   ---------------------------
   -- After_Rule_Definition --
   ---------------------------

   procedure After_Rule_Definition
     (Target : not null access Aquarius.Actions.Actionable'Class)
   is
      Tree         : constant Aquarius.Programs.Program_Tree :=
        Aquarius.Programs.Program_Tree (Target);
      Definition   : constant Program_Tree := Program_Tree (Target);
      Defined_Name : constant Program_Tree :=
        Program_Tree (Definition.Leaf ("identifier"));
      Name         : constant String := Defined_Name.Text;
      Grammar      : constant Aquarius.Grammars.Aquarius_Grammar :=
        Aquarius.Grammars.Get_Grammar (Tree.all);
      Syntax_Definition : constant Program_Tree :=
        Program_Tree (Tree.Breadth_First_Search ("syntax-body"));
      Standard_Definition : constant Program_Tree :=
        Program_Tree (Tree.Breadth_First_Search ("standard-body"));
      Delimiter_Definition : constant Program_Tree :=
                               Program_Tree
                                 (Tree.Breadth_First_Search
                                    ("delimiter-body"));
      Regexp_Definition : constant Program_Tree :=
                               Program_Tree
                                 (Tree.Breadth_First_Search
                                    ("regular-expression-body"));

   begin

      if Syntax_Definition /= null then

         declare
            Definition : constant Aquarius.Syntax.Syntax_Tree :=
              Get_Syntax (Syntax_Definition);
            Rule       : constant Aquarius.Syntax.Syntax_Tree :=
              Grammar.Get_Definition (Name);
         begin
            Rule.Add_Single_Child (Definition);
         end;
      elsif Standard_Definition /= null then
         declare
            use Aquarius.Lexers;
            Standard_Name : constant String :=
              Standard_Definition.Leaf ("identifier").Text;
            Lex : constant Lexer :=
              Aquarius.Grammars.Builtin.Standard_Lexer (Standard_Name);
         begin
            if Lex = Null_Lexer then
               Aquarius.Errors.Error (Standard_Definition,
                                      "unrecognised standard lexer");
            else
               Grammar.Add_Class_Terminal (Tree, Name, Lex, False);
            end if;
         end;
      elsif Delimiter_Definition /= null then
         declare
            use Aquarius.Lexers;
            String_Text : constant String :=
              Delimiter_Definition.Leaf ("string").Text;
            Text        : constant String :=
              String_Text (String_Text'First + 1 ..
                             String_Text'Last - 1);
         begin
            Grammar.Add_Class_Terminal (Tree, Name,
                                        One_Of (Text), True);
         end;
      elsif Regexp_Definition /= null then
         declare
            use Aquarius.Lexers;
            Text : constant String :=
                     Regexp_Definition.Leaf ("regex").Text;
            Regexp : constant String :=
                       Text (Text'First + 1 .. Text'Last - 1);
            Lex    : constant Lexer :=
                       Aquarius.Lexers.Parser.Parse_Lexer (Regexp);
         begin
            if Lex = Null_Lexer then
               Aquarius.Errors.Error (Standard_Definition,
                                      "invalid regular expression: "
                                      & Text);
            else
               Grammar.Add_Class_Terminal (Tree, Name, Lex, False);
            end if;
         end;
      end if;

   exception
      when E : Aquarius.Tokens.Token_Error =>
         Aquarius.Errors.Error
           (Tree, null,
            Ada.Exceptions.Exception_Message (E));
   end After_Rule_Definition;

   ---------------------
   -- After_Separator --
   ---------------------

   procedure After_Separator
     (Target : not null access Aquarius.Actions.Actionable'Class)
   is
      Tree         : constant Program_Tree := Program_Tree (Target);
      Child  : constant Aquarius.Trees.Tree :=
        Tree.Breadth_First_Search ("terminal-rule");
   begin
      Copy_Syntax (Child, Tree);
   end After_Separator;

   -----------------------------
   -- After_Sequence_Of_Rules --
   -----------------------------

   procedure After_Sequence_Of_Rules
     (Target : not null access Aquarius.Actions.Actionable'Class)
   is
      Tree         : constant Program_Tree := Program_Tree (Target);
      Grammar      : constant Aquarius.Grammars.Aquarius_Grammar :=
        Aquarius.Grammars.Get_Grammar (Tree.all);
      Frame        : constant Aquarius.Tokens.Token_Frame :=
        Grammar.Frame;
      Items  : constant Aquarius.Trees.Array_Of_Trees :=
        Tree.Get_Matching_Children ("rule");
      Result : constant Aquarius.Syntax.Syntax_Tree :=
        Aquarius.Syntax.New_Sequence (Frame, Tree);
      Have_Optional : constant Boolean :=
        Get_Syntax (Items (Items'First).all).Optional;
      Intermediate  : Aquarius.Syntax.Syntax_Tree := Result;
   begin

      if Have_Optional then
         --  For disambiguating optional syntax, it's useful
         --  to ensure that any sequence which contains an
         --  optional node is the child of a single-node
         --  sequence.
         Intermediate := Aquarius.Syntax.New_Sequence (Frame, Tree);
         Result.Add_Child (Intermediate);
      end if;

      for I in Items'Range loop
         declare
            Child : constant Aquarius.Syntax.Syntax_Tree :=
              Get_Syntax (Items (I).all);
         begin
            Intermediate.Add_Child (Child);
         end;

      end loop;

      Set_Syntax (Tree.all, Result);

   end After_Sequence_Of_Rules;

   -------------------------
   -- After_Sequence_Rule --
   -------------------------

   procedure After_Sequence_Rule
     (Target : not null access Aquarius.Actions.Actionable'Class)
   is
      Tree         : constant Program_Tree := Program_Tree (Target);
      Grammar      : constant Aquarius.Grammars.Aquarius_Grammar :=
        Aquarius.Grammars.Get_Grammar (Tree.all);
      Frame        : constant Aquarius.Tokens.Token_Frame :=
        Grammar.Frame;
      Items  : constant Aquarius.Trees.Array_Of_Trees :=
        Tree.Get_Matching_Children ("terminal-rule");
      Result : constant Aquarius.Syntax.Syntax_Tree :=
        Aquarius.Syntax.New_Sequence (Frame, Tree);
   begin
      for I in Items'Range loop
         Result.Add_Child (Get_Syntax (Items (I)));
      end loop;
      Set_Syntax (Tree, Result);
   end After_Sequence_Rule;

   -----------------------
   -- After_Syntax_Body --
   -----------------------

   procedure After_Syntax_Body
     (Target : not null access Aquarius.Actions.Actionable'Class)
   is
      Tree         : constant Program_Tree := Program_Tree (Target);
      Grammar      : constant Aquarius.Grammars.Aquarius_Grammar :=
        Aquarius.Grammars.Get_Grammar (Tree.all);
      Frame        : constant Aquarius.Tokens.Token_Frame :=
        Grammar.Frame;
      Choices : constant Aquarius.Trees.Array_Of_Trees :=
        Tree.Get_Matching_Children ("sequence-of-rules");
   begin
      if Choices'Length = 0 then
         Aquarius.Errors.Error (Tree, "missing rule");
      elsif Choices'Length = 1 then
         Copy_Syntax (Choices (1), Tree);
      else
         declare
            Choice : constant Aquarius.Syntax.Syntax_Tree :=
              Aquarius.Syntax.New_Choice (Frame, Tree);
         begin
            for I in Choices'Range loop
               Choice.Add_Child (Get_Syntax (Choices (I)));
            end loop;
            Set_Syntax (Tree, Choice);
         end;
      end if;
   end After_Syntax_Body;

   -------------------------
   -- After_Terminal_Rule --
   -------------------------

   procedure After_Terminal_Rule
     (Target : not null access Aquarius.Actions.Actionable'Class)
   is
      Tree         : constant Program_Tree := Program_Tree (Target);
      Name_Reference   : constant Program_Tree :=
        Program_Tree (Tree.Leaf ("identifier"));
      Class_Spec      : constant Program_Tree :=
        Program_Tree (Tree.Breadth_First_Search ("class_spec"));
      Leaf             : constant Program_Tree :=
        Program_Tree (Tree.First_Leaf);
      Name             : constant String := Leaf.Text;
      G                : constant Aquarius.Grammars.Aquarius_Grammar :=
        Aquarius.Grammars.Get_Grammar (Tree.all);
      Value            : Aquarius.Syntax.Syntax_Tree;
   begin
      if Name_Reference /= null then
         Value := G.Reference_Name (Tree, Name);
      else
         Value := G.Reference_Terminal (Tree, Name (Name'First + 1 ..
                                                      Name'Last - 1));
      end if;
      if Class_Spec /= null then
         declare
            Class_Value : constant Aquarius.Syntax.Syntax_Tree :=
              Aquarius.Syntax.New_Sequence (G.Frame, Tree);
         begin
            Class_Value.Set_Render_Class
              (Class_Spec.Leaf ("identifier").Text);
            Class_Value.Add_Child (Value);
            Value := Class_Value;
         end;
      end if;
      Set_Syntax (Tree, Value);
   end After_Terminal_Rule;

   ----------------------------
   -- After_Value_Definition --
   ----------------------------

   procedure After_Value_Definition
     (Target : not null access Aquarius.Actions.Actionable'Class)
   is
      Tree         : constant Aquarius.Programs.Program_Tree :=
        Aquarius.Programs.Program_Tree (Target);
      Definition   : constant Program_Tree := Program_Tree (Target);
      Defined_Name : constant Program_Tree :=
        Program_Tree (Definition.Leaf ("identifier"));
      Name         : constant String := Defined_Name.Text;
      Grammar      : constant Aquarius.Grammars.Aquarius_Grammar :=
        Aquarius.Grammars.Get_Grammar (Tree.all);
      Child        : constant Aquarius.Trees.Tree :=
                       Definition.Breadth_First_Search ("expression");
      Value        : constant Aquarius.Trees.Tree := Child.First_Leaf;
      Value_Text   : constant String := Value.Text;
   begin
      if Value_Text (Value_Text'First) = '"'
        and then Value_Text (Value_Text'Last) = '"'
        and then Value_Text'Length >= 2
      then
         Grammar.Add_Value (Child, Name,
                            String_To_Value (Value_Text));
      else
         Grammar.Add_Value (Child, Name, Value_Text);
      end if;
   end After_Value_Definition;

   ----------------
   -- After_When --
   ----------------

   procedure After_When
     (Target : not null access Aquarius.Actions.Actionable'Class)
   is
      Tree  : constant Program_Tree := Program_Tree (Target);
      Rule  : constant Program_Tree := Program_Tree (Tree.Left_Sibling);
      Rule_Syntax   : constant Aquarius.Syntax.Syntax_Tree :=
        Get_Syntax (Rule);
      Preconditions : constant Program_Tree :=
        Tree.Program_Child ("sequence_of_identifiers");

   begin
      if Preconditions /= null then
         declare
            Properties    : constant Array_Of_Program_Trees :=
              Preconditions.Direct_Children ("identifier");
         begin
            for I in Properties'Range loop
               Rule_Syntax.Add_Precondition_Property
                 (Properties (I).First_Leaf.Text);
            end loop;
         end;
      end if;

   end After_When;

   ------------------------------
   -- Before_Format_Definition --
   ------------------------------

   procedure Before_Format_Definition
     (Target : not null access Aquarius.Actions.Actionable'Class)
   is
      pragma Unreferenced (Target);
   begin
      null;
   end Before_Format_Definition;

   ----------------------------
   -- Before_Rule_Definition --
   ----------------------------

   procedure Before_Rule_Definition
     (Target : not null access Aquarius.Actions.Actionable'Class)
   is
      Tree         : constant Aquarius.Programs.Program_Tree :=
        Aquarius.Programs.Program_Tree (Target);
      Definition   : constant Program_Tree := Program_Tree (Target);
      Defined_Name : constant Program_Tree :=
        Program_Tree (Definition.Leaf ("identifier"));
      Name         : constant String := Defined_Name.Text;
      Grammar      : constant Aquarius.Grammars.Aquarius_Grammar :=
        Aquarius.Grammars.Get_Grammar (Tree.all);
      Syntax_Definition : constant Program_Tree :=
        Program_Tree (Tree.Breadth_First_Search ("syntax-body"));
   begin
      if Syntax_Definition /= null then
         declare
            Rule       : constant Aquarius.Syntax.Syntax_Tree :=
              Grammar.Add_Non_Terminal (Tree, Name);
         begin
            Set_Syntax (Syntax_Definition, Rule);
         end;
      end if;
   end Before_Rule_Definition;

   -----------------------------
   -- Before_Value_Definition --
   -----------------------------

   procedure Before_Value_Definition
     (Target : not null access Aquarius.Actions.Actionable'Class)
   is
      pragma Unreferenced (Target);
   begin
      null;
   end Before_Value_Definition;

   -----------------
   -- Copy_Syntax --
   -----------------

   procedure Copy_Syntax
     (From : not null access Aquarius.Trees.Root_Tree_Type'Class;
      To   : not null access Aquarius.Trees.Root_Tree_Type'Class)
   is
   begin
      Set_Syntax (To, Get_Syntax (From));
   end Copy_Syntax;

   ----------------
   -- Set_Syntax --
   ----------------

   procedure Set_Syntax
     (Tree  : in out Aquarius.Trees.Root_Tree_Type'Class;
      Value : Aquarius.Syntax.Syntax_Tree)
   is
   begin
      Tree.Set_Property (Aquarius.Properties.Syntax_Property, Value);
   end Set_Syntax;

   ----------------
   -- Set_Syntax --
   ----------------

   procedure Set_Syntax
     (Tree  : not null access Aquarius.Trees.Root_Tree_Type'Class;
      Value : Aquarius.Syntax.Syntax_Tree)
   is
   begin
      Set_Syntax (Tree.all, Value);
   end Set_Syntax;

   ---------------------
   -- String_To_Value --
   ---------------------

   function String_To_Value
     (Raw_String : String)
      return String
   is
      Result : String (Raw_String'Range);
      Index  : Positive := Raw_String'First;
      Last   : Natural  := Raw_String'Last;
      Target : Positive := Result'First;
   begin
      if Raw_String (Raw_String'First) = '"'
        and then Raw_String (Raw_String'Last) = '"'
        and then Raw_String'Length /= 1
      then
         Index := Index + 1;
         Last := Last - 1;
      end if;

      while Index <= Last loop
         if Raw_String (Index) = '"'
           and then Index < Last
           and then Raw_String (Index + 1) = '"'
         then
            Index := Index + 1;
         end if;
         Result (Target) := Raw_String (Index);
         Target := Target + 1;
         Index := Index + 1;
      end loop;

      return Result (Result'First .. Target - 1);
   end String_To_Value;

end Aquarius.Plugins.EBNF.Analyse;
