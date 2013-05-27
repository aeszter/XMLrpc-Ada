with Ada.Strings.Unbounded;
with SOAP.Message;
with XMLrpc; use XMLrpc;
with XMLrpc.Parameters;

package XMLrpc.Message is

   use Ada.Strings.Unbounded;

   type Object is tagged private;

   function XML_Image (M : Object) return Unbounded_String;

   function Parameters (M : Object'class) return XMLrpc.Parameters.List;

private
   --   type Object is new SOAP.Message.Object with null record;
   type Object is tagged record
      Wrapper_Name : Unbounded_String;
      P            : XMLrpc.Parameters.List;
   end record;

end XMLrpc.Message;
