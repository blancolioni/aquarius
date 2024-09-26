with Aquarius.Streams.Strings;

with WL.Unit.Compare_Test;

package body Aquarius.Tests.Streams is

   function Id (S : String) return String is (S);

   function Read_Single_Line_Stream return String;
   function Read_Stream return String;

   package Stream_Tests is
     new WL.Unit.Compare_Test (String, Id, Compare);

   ----------
   -- Load --
   ----------

   procedure Load (Suite : in out WL.Unit.Test_Suite) is
   begin
      Suite.Append
        (Stream_Tests.Test
           ("read-single-line-stream",
            Read_Single_Line_Stream'Access, "single line"));
      Suite.Append
        (Stream_Tests.Test
           ("read-stream",
            Read_Stream'Access, "first line||third line|"));
   end Load;

   -----------------------------
   -- Read_Single_Line_Stream --
   -----------------------------

   function Read_Single_Line_Stream return String is
      Stream : constant Aquarius.Streams.Reader_Reference :=
                 Aquarius.Streams.Strings.String_Reader
                   ("single line" & Character'Val (10));
   begin
      return Result : constant String := Stream.Get_Line do
         Stream.Close;
      end return;
   end Read_Single_Line_Stream;

   -----------------
   -- Read_Stream --
   -----------------

   function Read_Stream return String is
      Stream : constant Aquarius.Streams.Reader_Reference :=
                 Aquarius.Streams.Strings.String_Reader
                   ("first line" & Character'Val (10)
                    & Character'Val (10)
                    & "third line" & Character'Val (10));

      function Read return String;

      ----------
      -- Read --
      ----------

      function Read return String is
      begin
         if Stream.End_Of_Stream then
            return "";
         else
            declare
               Line : constant String := Stream.Get_Line;
            begin
               return Line & "|" & Read;
            end;
         end if;
      end Read;

   begin
      return Read;
   end Read_Stream;

end Aquarius.Tests.Streams;
