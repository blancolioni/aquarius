package body Aquarius.Observable is

   type No_Update_Data_Instance is
     new Update_Data_Interface with null record;

   function No_Update_Data return Update_Data_Interface'Class
   is (No_Update_Data_Instance'(null record));

end Aquarius.Observable;
