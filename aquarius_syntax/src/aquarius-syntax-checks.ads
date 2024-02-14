package Aquarius.Syntax.Checks is

   function Begins (Tok  : Aquarius.Tokens.Token;
                    Tree : Syntax_Tree)
                   return Boolean;

   function Nullable (Tree  : Syntax_Tree) return Boolean;

   --  Satisfies: return True if the Test tree satisfies any preconditions
   --  that the syntax tree might have

   function Satisfies (Test  : access Aquarius.Trees.Root_Tree_Type'Class;
                       Tree  : Syntax_Tree)
                      return Boolean;

end Aquarius.Syntax.Checks;
