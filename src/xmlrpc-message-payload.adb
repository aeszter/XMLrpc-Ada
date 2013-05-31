package body XMLrpc.Message.Payload is

   -----------
   -- Build --
   -----------

   function Build
     (Procedure_Name : String;
      P_Set          : XMLrpc.Parameters.List)
      return Object
   is
   begin
      return (Wrapper_Name => To_Unbounded_String (Procedure_Name),
              P            => P_Set);
   end Build;

end XMLrpc.Message.Payload;
