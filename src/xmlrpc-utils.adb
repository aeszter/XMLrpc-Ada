with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body XMLrpc.Utils is
   function Encode (Str : String) return String is
      Result : Unbounded_String;
   begin
      for K in Str'Range loop
         case Str (K) is
            when '<'    => Append (Result, "&lt;");
--            when '>'    => Append (Result, "&gt;");
            when '&'    => Append (Result, "&amp;");
--            when '''    => Append (Result, "&apos;");
--            when '"'    => Append (Result, "&quot;");
--            when Character'Val (0) .. Character'Val (31) =>
--               Append (Result, "&#");
--               Append
--                 (Result, AWS.Utils.Image (Natural (Character'Pos (Str (K)))));
--               Append (Result, ';');
            when others => Append (Result, Str (K));
         end case;
      end loop;

      return To_String (Result);
   end Encode;
end XMLrpc.Utils;
