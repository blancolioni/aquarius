with Ada.Containers.Vectors;
with Ada.Unchecked_Deallocation;

package body Aquarius.Docs is

   type Layout_Mode is (Flat, Break);

   type Work_Item is record
      Indent : Integer;
      Mode   : Layout_Mode;
      D      : Doc;
   end record;

   package Work_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Work_Item);

   procedure Free is
     new Ada.Unchecked_Deallocation (Doc_Node, Doc);

   function Fits
     (Available : Integer;
      Head      : Work_Item;
      Stack     : Work_Vectors.Vector;
      Stack_Top : Natural)
      return Boolean;
   --  Does Head, followed by whatever remains on Stack (read from
   --  Stack_Top downward), fit in Available columns before the next
   --  guaranteed line break? Never mutates or frees a Doc_Node --
   --  Emit walks (and frees) the same nodes for real afterwards.

   procedure Emit
     (D            : Doc;
      Width        : Positive;
      Start_Line   : Positive;
      Start_Column : Positive);
   --  The real walk: decides each Group's flat-vs-broken form via
   --  Fits, calls Set_Position on every Leaf, and frees every
   --  Doc_Node once it has read whatever it needed from it.

   -----------
   -- Break --
   -----------

   function Break return Doc is
   begin
      return new Doc_Node'(Kind => Break_Kind);
   end Break;

   ----------
   -- Emit --
   ----------

   procedure Emit
     (D            : Doc;
      Width        : Positive;
      Start_Line   : Positive;
      Start_Column : Positive)
   is
      Stack      : Work_Vectors.Vector;
      Cur_Line   : Positive := Start_Line;
      Cur_Column : Positive := Start_Column;
   begin
      Stack.Append (Work_Item'(Start_Column - 1, Break, D));

      while not Stack.Is_Empty loop
         declare
            Item : constant Work_Item := Stack.Last_Element;
            Node : Doc := Item.D;
         begin
            Stack.Delete_Last;

            case Node.Kind is
               when Nil_Kind =>
                  null;

               when Leaf_Kind =>
                  Node.Terminal.Set_Position (Cur_Line, Cur_Column);
                  Cur_Column := Cur_Column + Node.Terminal.Text'Length;

               when Line_Kind =>
                  case Item.Mode is
                     when Flat =>
                        Cur_Column := Cur_Column + 1;
                     when Break =>
                        Cur_Line := Cur_Line + 1;
                        Cur_Column := Item.Indent + 1;
                  end case;

               when Break_Kind =>
                  Cur_Line := Cur_Line + 1;
                  Cur_Column := Item.Indent + 1;

               when Concat_Kind =>
                  --  push Right first so Left ends up on top (the
                  --  stack is popped from the end, so the last thing
                  --  pushed is the first thing processed)
                  Stack.Append
                    (Work_Item'(Item.Indent, Item.Mode, Node.Right));
                  Stack.Append
                    (Work_Item'(Item.Indent, Item.Mode, Node.Left));

               when Nest_Kind =>
                  Stack.Append
                    (Work_Item'
                       (Item.Indent + Node.Offset, Item.Mode, Node.Nested));

               when Group_Kind =>
                  declare
                     Chosen : Layout_Mode;
                  begin
                     if Item.Mode = Flat then
                        --  already inside a flattened ancestor: stays
                        --  flat unconditionally, no need to re-check
                        Chosen := Flat;
                     elsif Fits
                       (Width - Cur_Column,
                        Work_Item'(Item.Indent, Flat, Node.Grouped),
                        Stack,
                        Natural (Stack.Length))
                     then
                        Chosen := Flat;
                     else
                        Chosen := Break;
                     end if;
                     Stack.Append
                       (Work_Item'(Item.Indent, Chosen, Node.Grouped));
                  end;
            end case;

            Free (Node);
         end;
      end loop;
   end Emit;

   ----------
   -- Fits --
   ----------

   function Fits
     (Available : Integer;
      Head      : Work_Item;
      Stack     : Work_Vectors.Vector;
      Stack_Top : Natural)
      return Boolean
   is
      Local     : Work_Vectors.Vector;
      Remaining : Integer := Available;
      Top       : Natural := Stack_Top;
   begin
      Local.Append (Head);

      loop
         if Remaining < 0 then
            return False;
         end if;

         declare
            Item : Work_Item;
         begin
            if not Local.Is_Empty then
               Item := Local.Last_Element;
               Local.Delete_Last;
            elsif Top > 0 then
               Item := Stack (Top);
               Top := Top - 1;
            else
               return True;
            end if;

            case Item.D.Kind is
               when Nil_Kind =>
                  null;

               when Leaf_Kind =>
                  Remaining := Remaining - Item.D.Terminal.Text'Length;

               when Line_Kind =>
                  case Item.Mode is
                     when Flat =>
                        Remaining := Remaining - 1;
                     when Break =>
                        return True;
                  end case;

               when Break_Kind =>
                  return True;

               when Concat_Kind =>
                  Local.Append
                    (Work_Item'(Item.Indent, Item.Mode, Item.D.Right));
                  Local.Append
                    (Work_Item'(Item.Indent, Item.Mode, Item.D.Left));

               when Nest_Kind =>
                  Local.Append
                    (Work_Item'
                       (Item.Indent + Item.D.Offset, Item.Mode,
                        Item.D.Nested));

               when Group_Kind =>
                  --  Fits always measures a Group as if it were flat,
                  --  regardless of the ambient mode: it is asking
                  --  "does everything up to the next guaranteed break
                  --  fit if printed flat", not deciding this group's
                  --  real layout.
                  Local.Append
                    (Work_Item'(Item.Indent, Flat, Item.D.Grouped));
            end case;
         end;
      end loop;
   end Fits;

   -----------
   -- Group --
   -----------

   function Group (D : Doc) return Doc is
   begin
      return new Doc_Node'(Kind => Group_Kind, Grouped => D);
   end Group;

   ----------
   -- Leaf --
   ----------

   function Leaf (Terminal : Terminal_Node_Access) return Doc is
   begin
      return new Doc_Node'(Kind => Leaf_Kind, Terminal => Terminal);
   end Leaf;

   ------------
   -- Layout --
   ------------

   procedure Layout
     (D            : Doc;
      Width        : Positive;
      Start_Line   : Positive := 1;
      Start_Column : Positive := 1)
   is
   begin
      Emit (D, Width, Start_Line, Start_Column);
   end Layout;

   ----------
   -- Line --
   ----------

   function Line return Doc is
   begin
      return new Doc_Node'(Kind => Line_Kind);
   end Line;

   ----------
   -- Nest --
   ----------

   function Nest (Offset : Integer; D : Doc) return Doc is
   begin
      return new Doc_Node'(Kind => Nest_Kind, Offset => Offset, Nested => D);
   end Nest;

   ---------
   -- Nil --
   ---------

   function Nil return Doc is
   begin
      return new Doc_Node'(Kind => Nil_Kind);
   end Nil;

   ---------
   -- "&" --
   ---------

   function "&" (Left, Right : Doc) return Doc is
   begin
      return new Doc_Node'(Kind => Concat_Kind, Left => Left, Right => Right);
   end "&";

end Aquarius.Docs;
