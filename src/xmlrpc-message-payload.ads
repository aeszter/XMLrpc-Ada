with SOAP.Message.Payload;

package XMLrpc.Message.Payload is
type Object is new SOAP.Message.Payload.Object with private;
private
type Object is new SOAP.Message.Payload.Object with null record;
end XMLrpc.Message.Payload;
