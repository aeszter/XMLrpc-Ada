with Ada.Calendar;
with Ada.Strings.Unbounded;

with SOAP.Parameters;
with SOAP.Types;

with XMLrpc.Types;

package XMLrpc.Parameters is

   use Ada.Strings.Unbounded;

   Data_Error : exception renames SOAP.Types.Data_Error;

   Max_Parameters : constant := 50;
   --  This is the maximum number of parameters supported by this
   --  implementation.

   type List is private;
   No_Parameter : constant List;

   function Argument_Count (P : List) return Natural;
   --  Returns the number of parameters in P

   function Argument (P : List; Name : String) return Types.Object'Class;
   --  Returns parameters named Name in P. Raises Types.Data_Error if not
   --  found.

   function Argument (P : List; N : Positive) return Types.Object'Class;
   --  Returns Nth parameters in P. Raises Types.Data_Error if not found

   function Exist (P : List; Name : String) return Boolean;

   function Get (P : List; Name : String) return String;
   function Get (P : List; Name : String) return Integer;

   ------------------
   -- Constructors --
   ------------------

   function "&" (P : List; O : Types.Object'Class) return List;
   function "+" (O : Types.Object'Class) return List;

private
   type List is record
      V : Types.Object_Set (1 .. Max_Parameters);
      N : Natural := 0;
   end record;

   No_Parameter : constant List := (N => 0, V => (others => <>));
end XMLrpc.Parameters;
