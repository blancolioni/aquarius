private with Aquarius.Counters;

with Aquarius.Grammars;
with Aquarius.Locations;
with Aquarius.Tokens;
with Aquarius.Trees.Cursors;

private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Containers.Vectors;

package Aquarius.Programs.Parser is

   type Parse_Context is
     new Aquarius.Locations.Updateable_Location_Interface
   with private;

   procedure Initialise_Parse_Context
     (Context     : out Parse_Context;
      Grammar     : Aquarius.Grammars.Aquarius_Grammar;
      Root        : Program_Tree;
      Interactive : Boolean;
      Run_Actions : Boolean := True);

   function Get_Cursor (Context : Parse_Context)
                       return Aquarius.Trees.Cursors.Cursor;

   procedure Set_Cursor (Context : in out Parse_Context;
                         Cursor  : Aquarius.Trees.Cursors.Cursor);

   procedure Add_Comment
     (Context  : in out Parse_Context;
      Location : Aquarius.Locations.Location_Interface'Class;
      Comment  : Program_Tree);

   procedure Clear_Comments (Context  : in out Parse_Context);

   procedure Add_Error (Context : in out Parse_Context;
                        Error   : Program_Tree);

   procedure Finish_Parse (Context : in out Parse_Context);

   function Is_Ambiguous (Context : Parse_Context) return Boolean;

   function Token_OK
     (Tok      : Aquarius.Tokens.Token;
      Context  : Parse_Context)
     return Boolean;

   procedure Parse_Token
     (Tok      : Aquarius.Tokens.Token;
      Tok_Text : String;
      Context  : in out Parse_Context);

--     procedure Backtrack_Parser
--       (Tok            : in     Aquarius.Tokens.Token;
--        Tok_Pos        : in     Aquarius.Layout.Position;
--        Tok_Text       : in     String;
--        Context        : in out Parse_Context;
--        Recovered      :    out Boolean);

   procedure Set_Vertical_Space
     (Context  : in out Parse_Context;
      Space    : Aquarius.Locations.Line_Count);

   procedure Parse_Tree
     (Top  : Program_Tree;
      Code : String);

   procedure Parse_Tree
     (Top  : Program_Tree;
      Tree : Program_Tree);

   procedure Parse_Tree
     (Top    : Program_Tree;
      Before : String;
      Child  : Aquarius.Programs.Program_Tree;
      After  : String);

   procedure Repeat_Child
     (Top   : Program_Tree;
      Child : Array_Of_Program_Trees);

   function Tree_OK
     (Tree           : Program_Tree;
      Context        : Parse_Context)
     return Boolean;

   procedure Parse_Tree
     (Tree           : Program_Tree;
      Context        : in out Parse_Context);

   procedure Summarise_Context
     (Context : Parse_Context);

private

   type Ambiguity_Record;

   type Ambiguity is access Ambiguity_Record;

   package List_Of_Ambiguities is
      new Ada.Containers.Doubly_Linked_Lists (Ambiguity);

   type Ambiguity_Record is
      record
         Active     : Boolean;
         Identity   : Aquarius.Counters.Counter_Type;
         Parent     : Program_Tree;
         Right      : Program_Tree;
         Top        : Program_Tree;
         Last_Parse : Program_Tree;
         Location   : Aquarius.Trees.Cursors.Cursor;
         Previous   : List_Of_Ambiguities.Cursor;
      end record;

   package Program_Tree_Vector is
      new Ada.Containers.Vectors (Positive, Program_Tree);

   type Parse_Context is
     new Aquarius.Locations.Updateable_Location_Interface with
      record
         Grammar         : Aquarius.Grammars.Aquarius_Grammar;
         Ambiguities     : List_Of_Ambiguities.List;
         Comments        : Program_Tree_Vector.Vector;
         Errors          : Program_Tree_Vector.Vector;
         Interactive     : Boolean                         := True;
         Vertical_Space  : Aquarius.Locations.Line_Count   := 0;
         Run_Actions     : Boolean                         := True;
         Location        : Aquarius.Locations.Instance;
      end record;

   overriding function Offset
     (This : Parse_Context)
      return Aquarius.Locations.Location_Offset
   is (This.Location.Offset);

   overriding function Line
     (This : Parse_Context)
      return Aquarius.Locations.Line_Index
   is (This.Location.Line);

   overriding function Column
     (This : Parse_Context)
      return Aquarius.Locations.Column_Index
   is (This.Location.Column);

   overriding procedure Update_Location
     (This : in out Parse_Context;
      From : Aquarius.Locations.Location_Interface'Class);

end Aquarius.Programs.Parser;
