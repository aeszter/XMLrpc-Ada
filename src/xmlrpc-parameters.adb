package body XMLrpc.Parameters is

   function "&" (P : List; O : Types.Object'Class) return List is
      NP : List := P;
   begin
      NP.N := NP.N + 1;
      NP.V (NP.N) := Types."+" (O);
      return NP;
   end "&";

   function "+" (O : Types.Object'Class) return List is
      P : List;
   begin
      P.V (1) := Types."+" (O);
      P.N := 1;
      return P;
   end "+";

   function Argument (P : List; Name : String) return Types.Object'Class is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Argument unimplemented");
      raise Program_Error;
      return Argument (P, Name);
   end Argument;

   function Argument (P : List; N : Positive) return Types.Object'Class is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Argument unimplemented");
      raise Program_Error;
      return Argument (P, N);
   end Argument;

   --------------------
   -- Argument_Count --
   --------------------

   function Argument_Count (P : List) return Natural is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Argument_Count unimplemented");
      raise Program_Error;
      return Argument_Count (P);
   end Argument_Count;

end XMLrpc.Parameters;
