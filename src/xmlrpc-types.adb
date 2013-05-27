with Ada.Finalization; use Ada.Finalization;
with Ada.Unchecked_Deallocation;
with Ada.Task_Attributes;
with Ada.Strings.Fixed;

with  XMLrpc.Utils;

package body XMLrpc.Types is

   function "+" (O : Object'Class) return Object_Safe_Pointer is
   begin
      return (Ada.Finalization.Controlled with new Object'Class'(O));
   end "+";

   function "-" (O : Object_Safe_Pointer) return Object'Class is
   begin
      return O.O.all;
   end "-";

   overriding procedure Adjust   (O : in out Object_Safe_Pointer) is
   begin
      if O.O /= null then
         O.O := new Object'Class'(O.O.all);
      end if;
   end Adjust;

   overriding procedure Finalize (O : in out Object_Safe_Pointer) is
      procedure Unchecked_Free is
        new Ada.Unchecked_Deallocation (Object'Class, Object_Access);
   begin
      if O.O /= null then
         Unchecked_Free (O.O);
      end if;
   end Finalize;

   function Image (O : Object) return String is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Image unimplemented");
      raise Program_Error;
      return Image (O);
   end Image;

   overriding function Image (O : RPC_String) return String is
   begin
        return To_String (O.V);
   end Image;

   function N (Name : String  := "item") return RPC_Null is
   begin
      return (Ada.Finalization.Controlled with
                To_Unbounded_String (Name));
   end N;

   function Name (O : Object'Class) return String is
   begin
      return To_String (O.Name);
   end Name;

   function Spaces (N : Natural) return String;
   pragma Inline (Spaces);
   --  Returns N * 3 spaces

   package XML_Indent is new Ada.Task_Attributes (Natural, 0);

   function Spaces (N : Natural) return String is
      use Ada.Strings.Fixed;
   begin
      return (3 * N) * ' ';
   end Spaces;
   --  Thread safe Indentation counter

   -----------
   -- Image --
   -----------
   overriding function XML_Image (O : RPC_String) return String is
   begin
      return XML_Image (Object (O));
   end XML_Image;

   overriding function XML_Image (O : RPC_Null) return String is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "XML_Image unimplemented");
      raise Program_Error;
      return XML_Image (O);
   end XML_Image;

   function XML_Image (O : Object) return String is
      Indent : constant Natural      := XML_Indent.Value;
      OC     : constant Object'Class := Object'Class (O);
   begin
      if OC in RPC_String then
         return Spaces (Indent)
           & "<" & XML_Type (OC) & '>'
           & Utils.Encode (Image (OC))
           & "</" & XML_Type (OC) & '>';
      else
         return Spaces (Indent)
           & "<" & XML_Type (OC) & '>'
           & Image (OC)
           & "</" & XML_Type (OC) & '>';
      end if;
   end XML_Image;

   overriding function XML_Type (O : RPC_String) return String is
      pragma Unreferenced (O);
   begin
      return XML_String;
   end XML_Type;

   ---------------
   -- XML_Image --
   ---------------

   --------------
   -- XML_Type --
   --------------

   overriding function XML_Type (O : RPC_Null) return String is
      pragma Unreferenced (O);
   begin
      return XML_Null;
   end XML_Type;

   function XML_Type (O : Object) return String is
      pragma Warnings (Off, O);
   begin
      return "";
   end XML_Type;

end XMLrpc.Types;
