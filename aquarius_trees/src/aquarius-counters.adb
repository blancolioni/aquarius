package body Aquarius.Counters is

   ----------
   -- Next --
   ----------

   function Next (Source : in out Counter_Source) return Counter_Type is
      Result : constant Counter_Type := Source.Current;
   begin
      for I in reverse Source.Current'Range loop
         if Source.Current (I) = '9' then
            Source.Current (I) := '0';
         elsif Source.Current (I) = 'Z' then
            Source.Current (I) := 'A';
         else
            Source.Current (I) := Character'Succ (Source.Current (I));
            exit;
         end if;
      end loop;

      return Result;
   end Next;

   ----------
   -- Show --
   ----------

   function Show (Counter : Counter_Type) return String is
   begin
      return String (Counter);
   end Show;

end Aquarius.Counters;
