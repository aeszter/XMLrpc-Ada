with AWS.Client;

with XMLrpc.Message.Payload;
with XMLrpc.Message.Response;


package XMLrpc.Message.XML is

   function Image (O : Object'Class) return String;

   function Image (O : Object'Class) return Unbounded_String;
   function Load_Response (Connection : AWS.Client.HTTP_Connection)
      return Message.Response.Object'Class;
end;
