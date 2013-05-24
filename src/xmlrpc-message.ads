with Ada.Strings.Unbounded;
with SOAP.Message;
with XMLrpc; use XMLrpc;


package XMLrpc.Message is

   use Ada.Strings.Unbounded;

   type Object is tagged private;

private
   type Object is new SOAP.Message.Object with null record;

end XMLrpc.Message;
