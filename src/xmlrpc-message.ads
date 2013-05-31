with Ada.Strings.Unbounded;
with SOAP.Message;
with XMLrpc; use XMLrpc;
with XMLrpc.Parameters;

package XMLrpc.Message is

   use Ada.Strings.Unbounded;

   type Object is tagged private;

   function XML_Image (M : Object) return Unbounded_String;

   function Wrapper_Name (M : Object'class) return String;

   function Parameters (M : Object'class) return XMLrpc.Parameters.List;

   procedure Set_Wrapper_Name
     (M     : in out Object'Class;
      Name  : String);

   procedure Set_Parameters
     (M     : in out Object'Class;
      P_Set : XMLrpc.Parameters.List);

private
   --   type Object is new SOAP.Message.Object with null record;
   type Object is tagged record
      Wrapper_Name : Unbounded_String;
      P            : XMLrpc.Parameters.List;
   end record;

end XMLrpc.Message;
