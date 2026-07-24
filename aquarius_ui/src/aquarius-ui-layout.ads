--  Toolkit-agnostic bubble layout geometry. Kept free of any GUI dependency
--  so the overlap-removal algorithm can be unit-tested on plain rectangles.

package Aquarius.UI.Layout is

   type Rectangle is record
      X, Y, W, H : Long_Float;
   end record;

   type Rectangle_Array is array (Positive range <>) of Rectangle;

   function Overlaps
     (A, B : Rectangle; Gap : Long_Float := 12.0) return Boolean;
   --  True if A and B are closer than Gap (i.e. would overlap once Gap is
   --  reserved between them).

   procedure Remove_Overlaps
     (Rects : in out Rectangle_Array;
      Seed  : Positive;
      Gap   : Long_Float := 12.0);
   --  Phase 1 "frozen wavefront": Seed stays fixed; every rectangle that
   --  overlaps it is pushed clear (leaving Gap) along its axis of least
   --  penetration, then itself becomes fixed and pushes its own overlaps,
   --  breadth-first. Each rectangle moves at most once, so the cascade never
   --  returns to an already-adjusted rectangle and always terminates.
   --
   --  Not yet handled (future work): a rectangle trapped between two already
   --  fixed rectangles can retain a residual overlap.

   procedure Normalize
     (Rects  : in out Rectangle_Array;
      Margin : Long_Float := 12.0);
   --  Translate every rectangle by the same amount so the minimum X and Y are
   --  at least Margin (rectangles may be pushed to negative coordinates by
   --  Remove_Overlaps, but GtkLayout cannot place children at negative
   --  positions). Only shifts in the positive direction; a no-op if already
   --  within bounds. Overlap-freedom is preserved because it is a rigid
   --  translation of the whole set.

end Aquarius.UI.Layout;
