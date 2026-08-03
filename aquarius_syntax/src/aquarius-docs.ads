package Aquarius.Docs is

   --  Terminal_Node: everything Aquarius.Docs needs from a leaf node.
   --  Nothing here mentions Program_Tree or grammars, so this package
   --  has no dependency on Aquarius.Programs; it is the other way
   --  round, with the tree types implementing this interface.

   type Terminal_Node is interface;

   function Text (Item : Terminal_Node) return String
      is abstract;
   --  Literal text of this leaf; used to measure whether a Group's
   --  flat form fits on the current line.

   procedure Set_Position
     (Item   : in out Terminal_Node;
      Offset : Natural;
      Line   : Positive;
      Column : Positive)
      is abstract;
   --  Record where Layout placed this leaf: Offset is a character
   --  count from the start of the layout, Line/Column its row/column.

   type Terminal_Node_Access is access all Terminal_Node'Class;

   --  Doc: a Wadler-style layout document.  Built from the
   --  combinators below, then consumed exactly once by Layout.

   type Doc is private;

   function Nil return Doc;
   --  The empty document.

   function Leaf (Terminal : Terminal_Node_Access) return Doc;
   --  A single leaf; its text comes from Terminal.Text.

   function Line return Doc;
   --  A space when flat, a newline when broken.

   function Break return Doc;
   --  Always a newline.

   function "&" (Left, Right : Doc) return Doc;
   --  Concatenation.

   function Nest (Offset : Integer; D : Doc) return Doc;
   --  D, indented by Offset while broken.

   function Group (D : Doc) return Doc;
   --  Try to lay out D flat; break only if it does not fit.

   procedure Layout
     (D            : Doc;
      Width        : Positive;
      Start_Offset : Natural  := 0;
      Start_Line   : Positive := 1;
      Start_Column : Positive := 1);
   --  Decides each Group's flat-vs-broken form and calls Set_Position
   --  on every leaf visited. Consumes D: once Layout returns, D has
   --  been fully deallocated and must not be used again.

private

   type Doc_Kind is
     (Nil_Kind, Leaf_Kind, Line_Kind, Break_Kind,
      Concat_Kind, Nest_Kind, Group_Kind);

   type Doc_Node;
   type Doc is access Doc_Node;

   type Doc_Node (Kind : Doc_Kind := Nil_Kind) is record
      case Kind is
         when Nil_Kind | Line_Kind | Break_Kind =>
            null;
         when Leaf_Kind =>
            Terminal : Terminal_Node_Access;
         when Concat_Kind =>
            Left, Right : Doc;
         when Nest_Kind =>
            Offset : Integer;
            Nested : Doc;
         when Group_Kind =>
            Grouped : Doc;
      end case;
   end record;

end Aquarius.Docs;
