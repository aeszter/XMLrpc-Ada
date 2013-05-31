with SOAP.Message.Payload;
with SOAP.Message.Response;
with SOAP.Message.Response.Error;

package XMLrpc.Message.Response.Error is

   type Object is new Message.Response.Object with private;
   type Faultcode is new Integer;

   HTTP_Error     : constant Faultcode := -100;
   Internal_Error : constant Faultcode := -101;

   function Build
     (Faultcode   : Error.Faultcode;
      Faultstring : String)
      return Object;

private

   type Object is new Message.Response.Object with null record;

end XMLrpc.Message.Response.Error;
