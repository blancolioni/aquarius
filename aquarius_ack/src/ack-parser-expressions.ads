private package Ack.Parser.Expressions is

   function Import_Expression
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
     with Pre => From.Name = "expression";

   function Import_Manifest_Constant
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
     with Pre => From.Name = "manifest_constant";

   function Import_Precursor
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
     with Pre => From.Name = "precursor";

   function Import_Actual_Arguments
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
     with Pre => From.Name = "actuals";

end Ack.Parser.Expressions;
