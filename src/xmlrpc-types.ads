with Ada.Finalization;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with SOAP.Types;

package XMLrpc.Types is

----------
-- Null --
----------

   type Object is abstract tagged private;

   type Object_Access is access all Object'Class;
   type Object_Safe_Pointer is tagged private;
   type Object_Set is array (Positive range <>) of Object_Safe_Pointer;

   function Image (O : Object) return String;
   function XML_Image (O : Object) return String;
   function XML_Type (O : Object) return String;

   function Name (O : Object'Class) return String;
   function "+" (O : Object'Class) return Object_Safe_Pointer;
   function "-" (O : Object_Safe_Pointer) return Object'Class;
   type Scalar is abstract new Object with private;

   XML_Null : constant String := "null";
pragma Compile_Time_Warning (Standard.True, "Null is not part of XML-RPC.");
   type RPC_Null is new Scalar with private;

   overriding function XML_Image (O : RPC_Null) return String;
   overriding function XML_Type  (O : RPC_Null) return String;

   XML_String : aliased constant String := "string";
   type RPC_String is new Scalar with private;

   overriding function Image (O : RPC_String) return String;
   overriding function XML_Image (O : RPC_String) return String;
   overriding function XML_Type (O : RPC_String) return String;

   function N (Name : String  := "item") return RPC_Null;

--   function "+" (O : SOAP.Types.Object'Class) return SOAP.Types.Object_Safe_Pointer
--                 renames SOAP.Types."+";

private
   type Object is abstract new Ada.Finalization.Controlled with
      record
         Name : Unbounded_String;
      end record;

   type Object_Safe_Pointer is new Ada.Finalization.Controlled with record
      O : Object_Access;
   end record;

   overriding procedure Adjust   (O : in out Object_Safe_Pointer);
   pragma Inline (Adjust);

   overriding procedure Finalize (O : in out Object_Safe_Pointer);
   pragma Inline (Finalize);

   type Scalar is abstract new Object with null record;
   type RPC_Null is new Scalar with null record;
   type RPC_String is new Scalar with record
      V : Unbounded_String;
   end record;

end XMLrpc.Types;
