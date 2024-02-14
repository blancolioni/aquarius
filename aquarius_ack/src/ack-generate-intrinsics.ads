with Tagatha.Code;

package Ack.Generate.Intrinsics is

   procedure Generate_Intrinsic
     (Unit      : in out Tagatha.Code.Instance'Class;
      Name      : String;
      Arg_Count : Natural;
      Push      : not null access
        procedure (Argument_Index : Positive));

   type Intrinsic_Generator is access
     function (Unit : in out Tagatha.Code.Instance'Class;
               Push : not null access
                 procedure (Argument_Index : Positive))
               return Boolean;

   procedure Add_Intrinsic
     (Name      : String;
      Generator : Intrinsic_Generator);

end Ack.Generate.Intrinsics;
