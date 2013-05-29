with XMLrpc.Types;
package body XMLrpc.Message.Response.Error is

   -----------
   -- Build --
   -----------

   function Build
     (Faultcode   : Error.Faultcode;
      Faultstring : String)
      return Object
   is
      use XMLrpc.Types;
      use type XMLrpc.Parameters.List;

      O : Object;
      P : XMLrpc.Parameters.List;
   begin
      --  Set Wrapper Name

      Set_Wrapper_Name (O, "fault");

      --  Set Faultcode and Faultstring

      P := P
        & S (Error.Faultcode'Image (Faultcode), "faultcode")
        & S (Faultstring, "faultstring");

      --  Set parameters for this error object

      Set_Parameters (O, P);

      return O;
   end Build;

end XMLrpc.Message.Response.Error;
