with SOAP.Message.Payload;
with SOAP.Message.Response;

package XMLrpc.Message.Response.Error is

   type Object is new Message.Response.Object with private;
   type Faultcode is new String;

   function Build
     (Faultcode   : Error.Faultcode;
      Faultstring : String)
      return Object;

   function Client (Subname : String := "") return Faultcode;

private

   type Object is new Message.Response.Object with null record;

end XMLrpc.Message.Response.Error;
