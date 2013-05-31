with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Calendar;

package body XMLrpc.Utils is

   function Date_Time (TI, Name : String) return Types.RPC_Date_Time
   is
      use Ada.Calendar;
      subtype Year_Range is Positive range TI'First .. TI'First + 3;
      subtype Month_Range is Positive range TI'First + 5 .. TI'First + 6;
      subtype Day_Range is Positive range TI'First + 8 .. TI'First + 9;
      subtype Hour_Range is Positive range TI'First + 11 .. TI'First + 12;
      subtype Minute_Range is Positive range TI'First + 14 .. TI'First + 15;
      subtype Second_Range is Positive range TI'First + 17 .. TI'First + 18;
      T : Time;
   begin
      --  timeInstant format is CCYY-MM-DDThh:mm:ss

      T := Time_Of (Year    => Year_Number'Value (TI (Year_Range)),
                    Month   => Month_Number'Value (TI (Month_Range)),
                    Day     => Day_Number'Value (TI (Day_Range)),
                    Seconds => Duration
                      (Natural'Value (TI (Hour_Range)) * 3600
                       + Natural'Value (TI (Minute_Range)) * 60
                       + Natural'Value (TI (Second_Range))));

      return Types.T (T, Name);
   end Date_Time;
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
