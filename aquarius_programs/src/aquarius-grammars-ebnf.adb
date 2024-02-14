with Aquarius.Grammars.Builtin;
with Aquarius.Syntax;
with Aquarius.Trees;

package body Aquarius.Grammars.EBNF is

   procedure Create_Terminals
     (Grammar : Aquarius.Grammars.Aquarius_Grammar);

   procedure Create_Non_Terminals
     (Grammar : Aquarius.Grammars.Aquarius_Grammar);

   -------------------------
   -- Create_EBNF_Grammar --
   -------------------------

   function Create_EBNF_Grammar return Aquarius.Grammars.Aquarius_Grammar is
      use Aquarius.Syntax;
      Grammar : constant Aquarius.Grammars.Aquarius_Grammar :=
        Aquarius.Grammars.New_Grammar
        (Name   => "ebnf", EBNF => null);
      Internal : constant Aquarius.Trees.Tree :=
        Aquarius.Trees.Internal_Declaration;
   begin
      Add_Non_Terminal (Grammar, "source-file",
                        New_Repeat (Grammar.Frame, Internal, True, null),
                        Grammar.Reference_Name (Internal, "definition"));

      Create_Terminals (Grammar);
      Create_Non_Terminals (Grammar);

      Grammar.Check_Grammar;

      return Grammar;
   end Create_EBNF_Grammar;

   --------------------------
   -- Create_Non_Terminals --
   --------------------------

   procedure Create_Non_Terminals
     (Grammar : Aquarius.Grammars.Aquarius_Grammar)
   is
      use Aquarius.Syntax;
      Internal : constant Aquarius.Trees.Tree :=
        Aquarius.Trees.Internal_Declaration;
   begin
      Add_Non_Terminal
        (Grammar, "definition",
         New_Choice (Grammar.Frame, Internal),
         [Grammar.Reference_Name (Internal, "value-definition"),
          Grammar.Reference_Name (Internal, "format-definition"),
          Grammar.Reference_Name (Internal, "xref-definition"),
          Grammar.Reference_Name (Internal, "rule-definition")]);
      Add_Non_Terminal
        (Grammar, "value-definition",
         New_Sequence (Grammar.Frame, Internal),
         [Grammar.Reference_Name (Internal, "identifier"),
          Grammar.Reference_Terminal (Internal, "="),
          Grammar.Reference_Name (Internal, "expression")]);
      Add_Non_Terminal
        (Grammar, "format-definition",
         New_Sequence (Grammar.Frame, Internal),
         [Grammar.Reference_Terminal (Internal, "format"),
          Grammar.Reference_Name (Internal, "terminal-or-rule"),
          Grammar.Reference_Name (Internal, "list-of-formats",
                          Indent_Rule => True)]);
      Add_Non_Terminal
        (Grammar, "xref-definition",
         New_Sequence (Grammar.Frame, Internal),
         [Grammar.Reference_Terminal (Internal, "xref"),
          Grammar.Reference_Name (Internal, "identifier"),
          Grammar.Reference_Name (Internal, "terminal-or-rule")]);

      Add_Non_Terminal (Grammar, "terminal-or-rule",
                        New_Choice (Grammar.Frame, Internal),
                        [Grammar.Reference_Name (Internal, "identifier"),
                         Grammar.Reference_Name (Internal, "terminal")]);
      Add_Non_Terminal
        (Grammar, "list-of-formats",
         New_Repeat (Grammar.Frame, Internal, False, null),
         Grammar.Reference_Name (Internal, "identifier"));

      Add_Non_Terminal (Grammar, "expression",
                        New_Choice (Grammar.Frame, Internal),
                        [Grammar.Reference_Name (Internal, "string"),
                         Grammar.Reference_Name (Internal, "identifier"),
                         Grammar.Reference_Name (Internal, "integer")]);
      Add_Non_Terminal (Grammar, "rule-definition",
                        New_Sequence (Grammar.Frame, Internal),
                        [Grammar.Reference_Name (Internal, "identifier"),
                         Grammar.Reference_Terminal (Internal, "::="),
                         Grammar.Reference_Name
                           (Internal, "definition-body",
                            Indent_Rule => True)]);
      Add_Non_Terminal (Grammar, "definition-body",
                        New_Choice (Grammar.Frame, Internal),
                        [Grammar.Reference_Name (Internal, "standard-body"),
                         Grammar.Reference_Name (Internal, "delimiter-body"),
                         Grammar.Reference_Name
                           (Internal, "regular-expression-body"),
                         Grammar.Reference_Name (Internal, "syntax-body")]);

      Add_Non_Terminal (Grammar, "standard-body",
                        New_Sequence (Grammar.Frame, Internal),
                        [Grammar.Reference_Terminal (Internal, "standard"),
                         Grammar.Reference_Name (Internal, "identifier")]);

      Add_Non_Terminal (Grammar, "regular-expression-body",
                        New_Sequence (Grammar.Frame, Internal),
                        [Grammar.Reference_Terminal (Internal, "regex")]);

      Add_Non_Terminal (Grammar, "delimiter-body",
                        New_Sequence (Grammar.Frame, Internal),
                        [Grammar.Reference_Terminal (Internal, "delimiters"),
                         Grammar.Reference_Name (Internal, "string")]);

      Add_Non_Terminal
        (Grammar, "syntax-body",
         New_Repeat (Grammar.Frame, Internal, True,
           Grammar.Reference_Terminal
             (Internal, "|")),
         Grammar.Reference_Name (Internal, "sequence-of-rules"));

      Add_Non_Terminal (Grammar, "sequence-of-rules",
                        New_Repeat (Grammar.Frame, Internal, False, null),
                        [Grammar.Reference_Name (Internal, "rule"),
                         Grammar.Reference_Name (Internal, "optional_when")]);

      Add_Non_Terminal (Grammar, "optional_when",
                        New_Optional (Grammar.Frame, Internal),
                        [Grammar.Reference_Terminal (Internal, "when"),
                         Grammar.Reference_Name (Internal,
                                         "sequence_of_identifiers")]);

      Add_Non_Terminal (Grammar, "sequence_of_identifiers",
                        New_Repeat (Grammar.Frame, Internal, False, null),
                        Grammar.Reference_Name (Internal, "identifier"));

      Add_Non_Terminal (Grammar, "rule",
                        New_Choice (Grammar.Frame, Internal),
                        [Grammar.Reference_Name (Internal,
                                         "repeat-optional-rule"),
                         Grammar.Reference_Name (Internal,
                                         "repeat-required-rule"),
                         Grammar.Reference_Name (Internal,
                                         "terminal-rule"),
                         Grammar.Reference_Name (Internal,
                                         "optional-rule"),
                         Grammar.Reference_Name (Internal,
                                         "nested-rule")]);

      Add_Non_Terminal (Grammar, "repeat-optional-rule",
                        New_Sequence (Grammar.Frame, Internal),
                        [Grammar.Reference_Terminal (Internal, "{"),
                         Grammar.Reference_Name (Internal, "repeater"),
                         Grammar.Reference_Terminal (Internal, "}")]);

      Add_Non_Terminal (Grammar, "repeat-required-rule",
                        New_Sequence (Grammar.Frame, Internal),
                        [Grammar.Reference_Terminal (Internal, "<"),
                         Grammar.Reference_Name (Internal, "repeater"),
                         Grammar.Reference_Terminal (Internal, ">")]);

      Add_Non_Terminal (Grammar, "nested-rule",
                        New_Sequence (Grammar.Frame, Internal),
                        [Grammar.Reference_Terminal (Internal, "("),
                         Grammar.Reference_Name (Internal, "syntax-body"),
                         Grammar.Reference_Terminal (Internal, ")")]);

      Add_Non_Terminal (Grammar, "sequence-rule",
                        New_Repeat (Grammar.Frame, Internal, False, null),
                        Grammar.Reference_Name (Internal, "terminal-rule"));

      Add_Non_Terminal
        (Grammar, "optional-rule",
         New_Sequence (Grammar.Frame, Internal),
         [Grammar.Reference_Terminal (Internal, "["),
          Grammar.Reference_Name (Internal, "sequence-of-rules"),
          Grammar.Reference_Terminal (Internal, "]")]);

      Add_Non_Terminal
        (Grammar, "repeater",
         New_Sequence (Grammar.Frame, Internal),
         [Grammar.Reference_Name (Internal, "sequence-of-rules"),
          Grammar.Reference_Name (Internal, "opt-separator")]);
      Add_Non_Terminal (Grammar, "opt-separator",
                        New_Optional (Grammar.Frame, Internal),
                        Grammar.Reference_Name (Internal, "separator"));

      Add_Non_Terminal (Grammar, "separator",
                        New_Sequence (Grammar.Frame, Internal),
                        [Grammar.Reference_Terminal (Internal, "/"),
                         Grammar.Reference_Name (Internal, "terminal-rule")]);

      Add_Non_Terminal (Grammar, "terminal-rule",
                        New_Choice (Grammar.Frame, Internal),
                        [Grammar.Reference_Name (Internal, "rule_name"),
                         Grammar.Reference_Name (Internal, "terminal")]);

--        Add_Non_Terminal (Grammar, "rule_name",
--                          New_Sequence (null),
--              [Grammar.Reference_Name (Internal, "identifier")]);

      Add_Non_Terminal
        (Grammar, "rule_name",
         New_Sequence (Grammar.Frame, Internal),
         [Grammar.Reference_Name (Internal, "identifier"),
          Grammar.Reference_Name (Internal, "opt_class_spec")]);

      Add_Non_Terminal (Grammar, "opt_class_spec",
                        New_Optional (Grammar.Frame, Internal),
                        Grammar.Reference_Name (Internal, "class_spec"));

      Add_Non_Terminal (Grammar, "class_spec",
                        New_Sequence (Grammar.Frame, Internal),
                        [Grammar.Reference_Terminal (Internal, ":"),
                         Grammar.Reference_Name (Internal, "identifier")]);

   end Create_Non_Terminals;

   ----------------------
   -- Create_Terminals --
   ----------------------

   procedure Create_Terminals
     (Grammar : Aquarius_Grammar)
   is
      use Aquarius.Lexers, Aquarius.Grammars.Builtin;
      Identifier_Lex : constant Lexer :=
        Standard_Lexer ("ada_identifier");
      String_Lex     : constant Lexer :=
        Standard_Lexer ("ada_string_literal");
      Apostrophe     : constant Lexer := Literal (''');
      Terminal_Lex     : constant Lexer :=
        (Apostrophe & Apostrophe & Apostrophe) or
        (Apostrophe & Repeat (not Apostrophe) & Apostrophe);
      Integer_Lex    : constant Lexer :=
        Standard_Lexer ("ada_numeric_literal");
      Symbol_Lex     : constant Lexer :=
                         Repeat (One_Of (":="));
      Regex          : constant Lexer :=
                         Literal ('!')
                         & Repeat (not Literal ('!'))
                         & Literal ('!');

      Internal : constant Aquarius.Trees.Tree :=
        Aquarius.Trees.Internal_Declaration;
      Line_Comment : constant Lexer :=
                         Standard_Lexer ("ada_comment");
   begin
      Grammar.Add_Class_Terminal (Internal, "identifier", Identifier_Lex);
      Grammar.Add_Class_Terminal (Internal, "string",     String_Lex);
      Grammar.Add_Class_Terminal (Internal, "terminal",   Terminal_Lex);
      Grammar.Add_Class_Terminal (Internal, "integer",    Integer_Lex);
      Grammar.Add_Class_Terminal (Internal, "symbol",     Symbol_Lex);
      Grammar.Add_Class_Terminal (Internal, "comment",    Line_Comment);
      Grammar.Add_Class_Terminal (Internal, "regex",      Regex);
      Grammar.Add_Class_Terminal (Internal, "delimiter",
                          One_Of ("{}[]<>|()/"));
   end Create_Terminals;

end Aquarius.Grammars.EBNF;
