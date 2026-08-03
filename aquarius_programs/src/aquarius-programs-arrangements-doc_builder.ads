with Aquarius.Docs;

package Aquarius.Programs.Arrangements.Doc_Builder is

   function Build (Item : Program_Tree) return Aquarius.Docs.Doc;
   --  Recursively builds a Doc for Item from its Aquarius.Formats rules.
   --  Item, being a Program_Tree, already implements Terminal_Node, so
   --  every terminal becomes exactly one Leaf.

end Aquarius.Programs.Arrangements.Doc_Builder;
