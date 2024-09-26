generic
   type Object is limited private;
   type Name is access Object;
package Aquarius.Free_Lists is

   function Claim return Name;
   procedure Release (X : Name);

end Aquarius.Free_Lists;
