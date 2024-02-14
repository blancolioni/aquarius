package Aquarius.Lexers.Parser is

   function Parse_Lexer (Text : String) return Lexer;
   --  Turn the regular expression in Text into a lexer
   --  Text should be a valid (restricted) regular expression.
   --  Examples:
   --    matching an Ada identifier
   --     [A-Za-z](_[A-Za-z0-9]*)*

end Aquarius.Lexers.Parser;
