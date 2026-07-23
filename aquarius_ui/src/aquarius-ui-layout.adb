package body Aquarius.UI.Layout is

   ---------------------
   -- Remove_Overlaps --
   ---------------------

   procedure Remove_Overlaps
     (Rects : in out Rectangle_Array;
      Seed  : Positive;
      Gap   : Long_Float := 12.0)
   is
      Frozen : array (Rects'Range) of Boolean := [others => False];
      Queue  : array (Rects'Range) of Positive;
      Head   : Natural := Rects'First - 1;
      Tail   : Natural := Rects'First - 1;

      procedure Enqueue (Index : Positive);
      function Overlaps (A, B : Rectangle) return Boolean;
      procedure Push_Apart (Anchor : Rectangle; Movable : in out Rectangle);

      -------------
      -- Enqueue --
      -------------

      procedure Enqueue (Index : Positive) is
      begin
         Tail := Tail + 1;
         Queue (Tail) := Index;
         Frozen (Index) := True;
      end Enqueue;

      --------------
      -- Overlaps --
      --------------

      function Overlaps (A, B : Rectangle) return Boolean is
         Dx : constant Long_Float :=
           abs ((A.X + A.W / 2.0) - (B.X + B.W / 2.0));
         Dy : constant Long_Float :=
           abs ((A.Y + A.H / 2.0) - (B.Y + B.H / 2.0));
      begin
         return (A.W + B.W) / 2.0 + Gap > Dx
           and then (A.H + B.H) / 2.0 + Gap > Dy;
      end Overlaps;

      --------------
      -- Push_Apart --
      --------------

      procedure Push_Apart (Anchor : Rectangle; Movable : in out Rectangle) is
         Cxa   : constant Long_Float := Anchor.X + Anchor.W / 2.0;
         Cya   : constant Long_Float := Anchor.Y + Anchor.H / 2.0;
         Cxm   : constant Long_Float := Movable.X + Movable.W / 2.0;
         Cym   : constant Long_Float := Movable.Y + Movable.H / 2.0;
         Pen_X : constant Long_Float :=
           (Anchor.W + Movable.W) / 2.0 + Gap - abs (Cxa - Cxm);
         Pen_Y : constant Long_Float :=
           (Anchor.H + Movable.H) / 2.0 + Gap - abs (Cya - Cym);
      begin
         --  Push along the axis of least penetration, away from the anchor.
         if Pen_X <= Pen_Y then
            if Cxm >= Cxa then
               Movable.X := Movable.X + Pen_X;
            else
               Movable.X := Movable.X - Pen_X;
            end if;
         else
            if Cym >= Cya then
               Movable.Y := Movable.Y + Pen_Y;
            else
               Movable.Y := Movable.Y - Pen_Y;
            end if;
         end if;
      end Push_Apart;

      Anchor : Positive;
   begin
      if Seed not in Rects'Range then
         return;
      end if;

      Enqueue (Seed);
      while Head < Tail loop
         Head := Head + 1;
         Anchor := Queue (Head);
         for X in Rects'Range loop
            if not Frozen (X)
              and then Overlaps (Rects (Anchor), Rects (X))
            then
               Push_Apart (Rects (Anchor), Rects (X));
               Enqueue (X);
            end if;
         end loop;
      end loop;
   end Remove_Overlaps;

   ---------------
   -- Normalize --
   ---------------

   procedure Normalize
     (Rects  : in out Rectangle_Array;
      Margin : Long_Float := 12.0)
   is
      Min_X, Min_Y : Long_Float;
   begin
      if Rects'Length = 0 then
         return;
      end if;

      Min_X := Rects (Rects'First).X;
      Min_Y := Rects (Rects'First).Y;
      for R of Rects loop
         Min_X := Long_Float'Min (Min_X, R.X);
         Min_Y := Long_Float'Min (Min_Y, R.Y);
      end loop;

      declare
         Dx : constant Long_Float :=
           (if Min_X < Margin then Margin - Min_X else 0.0);
         Dy : constant Long_Float :=
           (if Min_Y < Margin then Margin - Min_Y else 0.0);
      begin
         if Dx /= 0.0 or else Dy /= 0.0 then
            for R of Rects loop
               R.X := R.X + Dx;
               R.Y := R.Y + Dy;
            end loop;
         end if;
      end;
   end Normalize;

end Aquarius.UI.Layout;
