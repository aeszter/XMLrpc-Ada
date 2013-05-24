with Ada.Calendar;
with Ada.Strings.Unbounded;

with XMLrpc.Types;

package XMLrpc.Parameters is

   use Ada.Strings.Unbounded;

   Data_Error : exception renames Types.Data_Error;

   Max_Parameters : constant := 50;
   --  This is the maximum number of parameters supported by this
   --  implementation.

   type List is private;
end XMLrpc.Parameters;
