private with Ada.Containers.Ordered_Sets;

package Aquarius.Names.Sets is

   type Name_Set is tagged private;

   function Empty_Set return Name_Set;

   function Is_Empty (Set : Name_Set) return Boolean;

   procedure Insert (Set : in out Name_Set;
                     Name : Aquarius_Name);

   procedure Insert (Set : in out Name_Set;
                     Name : String);

   function Contains (Set : Name_Set;
                      Name : Aquarius_Name)
                      return Boolean;

   function Contains (Set : Name_Set;
                      Name : String)
                      return Boolean;

   function Any (Set : Name_Set;
                 Test : not null access
                   function (Name : Aquarius_Name) return Boolean)
                 return Boolean;

   procedure Scan
     (Set : Name_Set;
      Process : not null access procedure (Name : Aquarius_Name));

private

   package Name_Sets is
     new Ada.Containers.Ordered_Sets (Aquarius_Name);

   type Name_Set is new Name_Sets.Set with null record;

end Aquarius.Names.Sets;
