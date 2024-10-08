with Aquarius.Programs.Arrangements.Contexts;

private package Aquarius.Programs.Arrangements.Logging is

   procedure Log
     (Context  : in out Contexts.Arrangement_Context;
      Program  : not null Program_Tree;
      Text     : String);

   procedure Log
     (Context  : in out Contexts.Arrangement_Context;
      Program  : not null Program_Tree);

end Aquarius.Programs.Arrangements.Logging;
