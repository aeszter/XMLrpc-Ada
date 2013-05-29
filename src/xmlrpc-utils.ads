with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with SOAP.Utils;
with XMLrpc.Types;

package XMLrpc.Utils is
   function Encode (Str : String) return String;
   function Tag (Name : String; Start : Boolean) return String
                 renames SOAP.Utils.Tag;
   function From_Utf8 (Str : Unbounded_String) return Unbounded_String
     renames SOAP.Utils.From_Utf8;
   function From_Utf8 (Str : String) return String
     renames SOAP.Utils.From_Utf8;
   function From_Utf8 (Str : String) return String_Access
     renames SOAP.Utils.From_Utf8;
   function To_Utf8 (Str : String) return String
     renames SOAP.Utils.To_Utf8;
   function To_Utf8 (Str : Unbounded_String) return Unbounded_String
     renames SOAP.Utils.To_Utf8;

   function Date_Time (TI, Name : String) return Types.RPC_Date_Time;

end XMLrpc.Utils;
