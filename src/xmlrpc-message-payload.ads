with SOAP.Message.Payload;

with XMLrpc.Parameters;

package XMLrpc.Message.Payload is
   type Object is new Message.Object with private;

   function Build
     (Procedure_Name : String;
      P_Set          : XMLrpc.Parameters.List)
      return Object;

private
   type Object is new Message.Object with null record;
end XMLrpc.Message.Payload;
