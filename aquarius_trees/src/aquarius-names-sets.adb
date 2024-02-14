package body Aquarius.Names.Sets is

   ---------
   -- Any --
   ---------

   function Any
     (Set : Name_Set;
      Test : not null access
                   function (Name : Aquarius_Name) return Boolean)
      return Boolean
   is
   begin
      for Item of Set loop
         if Test (Item) then
            return True;
         end if;
      end loop;
      return False;
   end Any;

   --------------
   -- Contains --
   --------------

   overriding function Contains
     (Set : Name_Set;
      Name : Aquarius_Name)
      return Boolean
   is
   begin
      return Name_Sets.Set (Set).Contains (Name);
   end Contains;

   --------------
   -- Contains --
   --------------

   function Contains
     (Set : Name_Set;
      Name : String)
      return Boolean
   is
   begin
      return Set.Contains (To_Aquarius_Name (Name));
   end Contains;

   -----------
   -- Empty --
   -----------

   function Empty_Set return Name_Set is
   begin
      return (Name_Sets.Empty_Set with null record);
   end Empty_Set;

   ------------
   -- Insert --
   ------------

   overriding procedure Insert
     (Set : in out Name_Set;
      Name : Aquarius_Name)
   is
   begin
      Name_Sets.Set (Set).Insert (Name);
   end Insert;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Set : in out Name_Set;
      Name : String)
   is
   begin
      Set.Insert (To_Aquarius_Name (Name));
   end Insert;

   --------------
   -- Is_Empty --
   --------------

   overriding function Is_Empty (Set : Name_Set) return Boolean is
   begin
      return Name_Sets.Set (Set).Is_Empty;
   end Is_Empty;

   ----------
   -- Scan --
   ----------

   procedure Scan
     (Set : Name_Set;
      Process : not null access procedure (Name : Aquarius_Name))
   is
   begin
      for Name of Set loop
         Process (Name);
      end loop;
   end Scan;

end Aquarius.Names.Sets;
