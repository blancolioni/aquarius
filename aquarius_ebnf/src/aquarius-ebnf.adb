with Aquarius.Regexes;
with Aquarius.Regexes.Parser;
with Aquarius.Source;
with Aquarius.Syntax;

package body Aquarius.EBNF is

   procedure Create_Terminals
     (Grammar : Aquarius.Grammars.Aquarius_Grammar);

   procedure Create_Non_Terminals
     (Grammar : Aquarius.Grammars.Aquarius_Grammar);

   procedure Create_Reserved_Symbols
     (Grammar : Aquarius.Grammars.Aquarius_Grammar);

   -------------------------
   -- Create_EBNF_Grammar --
   -------------------------

   function Create_EBNF_Grammar return Aquarius.Grammars.Aquarius_Grammar is
      Grammar : constant Aquarius.Grammars.Aquarius_Grammar :=
        Aquarius.Grammars.New_Grammar ("ebnf-bootstrap");
   begin
      Create_Terminals (Grammar);
      Create_Reserved_Symbols (Grammar);
      Create_Non_Terminals (Grammar);
      return Grammar;
   end Create_EBNF_Grammar;

   --------------------------
   -- Create_Non_Terminals --
   --------------------------

   procedure Create_Non_Terminals
     (Grammar : Aquarius.Grammars.Aquarius_Grammar)
   is
      use Aquarius.Grammars;
      use Aquarius.Syntax;

      Source : constant Aquarius.Source.Source_Position :=
        Aquarius.Source.No_Source_Position;

   begin
      Add_Non_Terminal (Grammar, "source-file",
                        New_Repeat (Source, Zero, Unbounded, null),
                        Reference_Non_Terminal (Grammar, "definition"));
      Add_Non_Terminal
        (Grammar, "definition",
         New_Choice (Source),
         (Reference_Non_Terminal (Grammar, "value-definition"),
          Reference_Non_Terminal (Grammar, "rule-definition")));
      Add_Non_Terminal
        (Grammar, "value-definition",
         New_Sequence (Source),
         (Get_Definition (Grammar, "identifier"),
          Get_Definition (Grammar, "="),
          Reference_Non_Terminal (Grammar, "expression")));
      Add_Non_Terminal (Grammar, "expression",
                        New_Choice (Source),
                        (Get_Definition (Grammar, "string"),
                         Get_Definition (Grammar, "identifier")));
      Add_Non_Terminal (Grammar, "rule-definition",
                        New_Sequence (Source),
                        (Get_Definition (Grammar, "identifier"),
                         Get_Definition (Grammar, "::="),
                         Reference_Non_Terminal
                           (Grammar, "definition-body")));
      Add_Non_Terminal (Grammar, "declaration-body",
                        New_Choice (Source),
                        (Reference_Non_Terminal (Grammar, "regex-body"),
                         Reference_Non_Terminal (Grammar, "syntax-body")));

   end Create_Non_Terminals;

   -----------------------------
   -- Create_Reserved_Symbols --
   -----------------------------

   procedure Create_Reserved_Symbols
     (Grammar : Aquarius.Grammars.Aquarius_Grammar)
   is
      use Aquarius.Grammars;
   begin
      Add_Symbol (Grammar, "::=");
      Add_Symbol (Grammar, "=");
      Add_Symbol (Grammar, "regex");
      Add_Symbol (Grammar, "{");
      Add_Symbol (Grammar, "}");
      Add_Symbol (Grammar, "|");
      Add_Symbol (Grammar, "[");
      Add_Symbol (Grammar, "]");
      Add_Symbol (Grammar, "<");
      Add_Symbol (Grammar, ">");
      Add_Symbol (Grammar, "(");
      Add_Symbol (Grammar, ")");
      Add_Symbol (Grammar, "/");
   end Create_Reserved_Symbols;

   ----------------------
   -- Create_Terminals --
   ----------------------

   procedure Create_Terminals
     (Grammar : Aquarius.Grammars.Aquarius_Grammar)
   is
      use Aquarius.Regexes, Aquarius.Regexes.Parser;
      use Aquarius.Grammars;
      Identifier_Ex : constant Regex :=
        Parse_Regex ("[A-Za-z_][A-Za-z0-9_\-]*");
      String_Ex     : constant Regex :=
        Parse_Regex ("""([~""]*("""")*)*""");
      Terminal_Ex   : constant Regex :=
        Parse_Regex ("'[A-Za-z_\-]'");
      Integer_Ex    : constant Regex := Parse_Regex ("[0-9]+");
   begin
      Add_Terminal (Grammar, "identifier", Identifier_Ex, False);
      Add_Terminal (Grammar, "string",     String_Ex,     False);
      Add_Terminal (Grammar, "terminal",   Terminal_Ex,   False);
      Add_Terminal (Grammar, "integer",    Integer_Ex, False);
   end Create_Terminals;

end Aquarius.EBNF;

