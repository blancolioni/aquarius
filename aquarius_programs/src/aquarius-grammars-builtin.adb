package body Aquarius.Grammars.Builtin is

   use Aquarius.Lexers;

   Have_Lexers : Boolean := False;

   procedure Create_Standard_Lexers;

   Ada_Identifiers        : Lexer;
   Ada_Numeric_Literals   : Lexer;
   Ada_Character_Literals : Lexer;
   Ada_String_Literals    : Lexer;
   Ada_Comments           : Lexer;
   Ada_Delimiters         : Lexer;
   Ada_Apostrophe         : Lexer;
   Ada_Symbols            : Lexer;
   Symbol_Sequence        : Lexer;

   Haskell_Varid          : Lexer;
   Haskell_Conid          : Lexer;
   Haskell_Varsym         : Lexer;
   Haskell_Consym         : Lexer;

   Hash_Line_Comment      : Lexer;

   Backslash_Escaped_String : Lexer;

   ----------------------------
   -- Create_Standard_Lexers --
   ----------------------------

   procedure Create_Standard_Lexers is
      Identifier_Start  : constant Lexer := Letter;
      Identifier_Extend : constant Lexer := Digit or Literal ('_');
      Numeral        : constant Lexer :=
        Digit & Optional (Repeat (Literal ('_') or Digit));
      Exponent       : constant Lexer :=
        (Literal ('E') or Literal ('e')) &
        Optional (Literal ('+') or Literal ('-')) & Numeral;
      Decimal_Literal : constant Lexer :=
        Numeral & Optional (Literal ('.') & Numeral) & Optional (Exponent);
      Extended_Digit : constant Lexer := Hex_Digit;
      Based_Numeral  : constant Lexer :=
        Extended_Digit & Optional (Repeat (Literal ('_') or Extended_Digit));
      Based_Literal  : constant Lexer :=
        Numeral & Literal ('#') & Based_Numeral &
        Optional (Literal ('.') & Based_Numeral) &
        Literal ('#') & Optional (Exponent);
      String_Element : constant Lexer :=
        (Literal ('"') & Literal ('"')) or (not Literal ('"'));
      Backslash_Escaped_String_Element : constant Lexer :=
        (Literal ('\') & Any) or (not (Literal ('\') or Literal ('"')));
      Haskell_Symbol : constant Lexer :=
        One_Of ("!#$%&*+./<=>?@^|-~\");

   begin

      Ada_Identifiers :=
        Identifier_Start &
        Optional (Repeat (Identifier_Start or Identifier_Extend));
      Ada_Numeric_Literals :=
        Based_Literal or Decimal_Literal;

      Ada_Character_Literals :=
        Literal (''') & Graphic & Literal (''');
      Ada_String_Literals :=
        Literal ('"') & Optional (Repeat (String_Element)) & Literal ('"');
      Ada_Comments :=
        Literal ('-') & Literal ('-') & Repeat (Any);
      Ada_Symbols :=
        One_Of ("=>*:/.<-'") & Optional (One_Of ("=>*:/.<"));
      Ada_Delimiters := One_Of ("&()+,;|");
      Ada_Apostrophe := Literal (''');

      Ada_Comments := Literal ('-') & Literal ('-') & Repeat (Any);

      Symbol_Sequence := One_Of (":=") & Repeat (One_Of (":="));

      Haskell_Varid  := Lowercase & Repeat (Letter or Digit);
      Haskell_Conid  := Uppercase & Repeat (Letter or Digit);
      Haskell_Varsym := Haskell_Symbol & Repeat (Haskell_Symbol or
                                                   Literal (':'));
      Haskell_Consym := Literal (':') & Repeat (Haskell_Symbol or
                                                  Literal (':'));

      Backslash_Escaped_String :=
        Literal ('"')
        & Optional (Repeat (Backslash_Escaped_String_Element))
        & Literal ('"');

      Hash_Line_Comment :=
        Literal ('#') & Repeat (Any);
      Have_Lexers := True;
   end Create_Standard_Lexers;

   --------------------
   -- Standard_Lexer --
   --------------------

   function Standard_Lexer (Name : String) return Aquarius.Lexers.Lexer is
   begin
      if not Have_Lexers then
         Create_Standard_Lexers;
      end if;

      if Name = "ada_identifier" then
         return Ada_Identifiers;
      elsif Name = "ada_string_literal" then
         return Ada_String_Literals;
      elsif Name = "ada_character_literal" then
         return Ada_Character_Literals;
      elsif Name = "ada_numeric_literal" then
         return Ada_Numeric_Literals;
      elsif Name = "ada_delimiters" then
         return Ada_Delimiters;
      elsif Name = "ada_symbol" then
         return Ada_Symbols;
      elsif Name = "ada_apostrophe" then
         return Ada_Apostrophe;
      elsif Name = "ada_comment" then
         return Ada_Comments;
      elsif Name = "backslash_escaped_string" then
         return Backslash_Escaped_String;
      elsif Name = "symbol_sequence" then
         return Symbol_Sequence;
      elsif Name = "haskell_varid" then
         return Haskell_Varid;
      elsif Name = "haskell_conid" then
         return Haskell_Conid;
      elsif Name = "haskell_varsym" then
         return Haskell_Varsym;
      elsif Name = "haskell_consym" then
         return Haskell_Consym;
      elsif Name = "hash_line_comment" then
         return Hash_Line_Comment;
      else
         raise Constraint_Error with Name & ": unknown lexer";
      end if;
   end Standard_Lexer;

end Aquarius.Grammars.Builtin;
