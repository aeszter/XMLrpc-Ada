package body XMLrpc.Message.Response.Error is

   -----------
   -- Build --
   -----------

   function Build
     (Faultcode   : Error.Faultcode;
      Faultstring : String)
      return Object
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Build unimplemented");
      raise Program_Error;
      return Build (Faultcode, Faultstring);
   end Build;

end XMLrpc.Message.Response.Error;
