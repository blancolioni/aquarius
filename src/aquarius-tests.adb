with Ada.Text_IO;

with Aquarius.Tests.Editor;
with Aquarius.Tests.Grammar;
with Aquarius.Tests.Streams;

with WL.Unit;

package body Aquarius.Tests is

   -------------
   -- Compare --
   -------------

   function Compare (X, Y : String) return Natural is
      Y_Index : Positive := Y'First;
   begin
      for X_Index in X'Range loop
         if Y_Index > Y'Last
           or else X (X_Index) /= Y (Y_Index)
         then
            return X_Index - X'First + 1;
         end if;
         Y_Index := Y_Index + 1;
      end loop;
      if Y_Index = Y'Last + 1 then
         return 0;
      else
         return X'Length;
      end if;
   end Compare;

   ---------------
   -- Run_Tests --
   ---------------

   procedure Run_Tests is
      Suite : WL.Unit.Test_Suite;
      Success : Natural;
      Failure : Natural;
      Error   : Natural;
      Not_Run : Natural;
   begin
      Suite.Verbose_Errors (False);
      Editor.Load (Suite);
      Grammar.Load (Suite);
      Streams.Load (Suite);
      Suite.Run_Tests (Success, Failure, Error, Not_Run);
      Ada.Text_IO.Put_Line
        ("Tests: successful" & Success'Image
         & "; failed" & Failure'Image
         & "; error" & Error'Image
         & "; not run" & Not_Run'Image);
   end Run_Tests;

end Aquarius.Tests;
