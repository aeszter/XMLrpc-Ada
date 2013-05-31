with Ada.Finalization; use Ada.Finalization;
with Ada.Unchecked_Deallocation;
with Ada.Task_Attributes;
with Ada.Strings.Fixed;
with Ada.Long_Float_Text_IO;
with GNAT.Calendar.Time_IO;
with Ada.Tags;

with  XMLrpc.Utils;

package body XMLrpc.Types is

   use Ada;

   procedure Unchecked_Free is
      new Ada.Unchecked_Deallocation (Object_Set, Object_Set_Access);

   procedure Unchecked_Free is
      new Ada.Unchecked_Deallocation (Natural, Counter_Access);

   New_Line : constant String := ASCII.CR & ASCII.LF;

   function Spaces (N : Natural) return String;
   pragma Inline (Spaces);
   --  Returns N * 3 spaces

   function "+" (O : Object'Class) return Object_Safe_Pointer is
   begin
      return (Ada.Finalization.Controlled with new Object'Class'(O));
   end "+";

   function "-" (O : Object_Safe_Pointer) return Object'Class is
   begin
      return O.O.all;
   end "-";

   function A (V : Object_Set) return RPC_Array is
   begin
      return (Finalization.Controlled
              with To_Unbounded_String ("Name"), new Natural'(1), new Object_Set'(V));
   end A;

   overriding procedure Adjust   (O : in out Object_Safe_Pointer) is
   begin
      if O.O /= null then
         O.O := new Object'Class'(O.O.all);
      end if;
   end Adjust;

   overriding procedure Adjust (O : in out Composite) is
   begin
      O.Ref_Counter.all := O.Ref_Counter.all + 1;
   end Adjust;

   function B
     (V    : Boolean;
      Name : String  := "item") return RPC_Boolean is
   begin
      return
        (Finalization.Controlled
         with To_Unbounded_String (Name), V);
   end B;

   function B64
     (V      : String;
      Name   : String  := "item") return RPC_Base64 is
   begin
      return
        (Finalization.Controlled
         with To_Unbounded_String (Name),
         To_Unbounded_String (V));
   end B64;

   function D
     (V    : Long_Float;
      Name : String          := "item") return RPC_Double is
   begin
      return
        (Finalization.Controlled
         with To_Unbounded_String (Name), V);
   end D;

   overriding procedure Finalize (O : in out Object_Safe_Pointer) is
      procedure Unchecked_Free is
        new Ada.Unchecked_Deallocation (Object'Class, Object_Access);
   begin
      if O.O /= null then
         Unchecked_Free (O.O);
      end if;
   end Finalize;

   overriding procedure Finalize (O : in out Composite) is
      Ref_Counter : Counter_Access := O.Ref_Counter;
   begin
      --  Ensure call is idempotent

      O.Ref_Counter := null;

      if Ref_Counter /= null then
         Ref_Counter.all := Ref_Counter.all - 1;

         if Ref_Counter.all = 0 then
            Unchecked_Free (O.O);
            Unchecked_Free (Ref_Counter);
         end if;
      end if;
   end Finalize;

   ---------
   -- Get --
   ---------

   function Get (O : Object'Class) return String is
      use type Ada.Tags.Tag;
   begin
      if O'Tag = Types.RPC_String'Tag
--        or else O'Tag = Types.Untyped.Untyped'Tag
      then
         return V (RPC_String (O));

--      elsif O'Tag = Types.RPC_Any_Type'Tag
--        and then RPC_Any_Type (O).O.O'Tag = Types.RPC_String'Tag
--      then
--         return V (RPC_String (RPC_Any_Type (O).O.O.all));
--
      else
         Get_Error ("String", O);
      end if;
   end Get;

   function Get (O : Object'Class) return Integer is
      use type Ada.Tags.Tag;
   begin
      if O'Tag = Types.RPC_Integer'Tag then
         return V (RPC_Integer (O));

--      elsif O'Tag = Types.Untyped.Untyped'Tag then
--       begin
--            return Integer'Value (V (XSD_String (O)));
--         exception
--            when others =>
--               Get_Error ("Integer", O);
--         end;

--      elsif O'Tag = Types.XSD_Any_Type'Tag
--        and then XSD_Any_Type (O).O.O'Tag = Types.XSD_Integer'Tag
--      then
--         return V (XSD_Integer (XSD_Any_Type (O).O.O.all));

      else
         Get_Error ("Integer", O);
      end if;
   end Get;

   procedure Get_Error (Expected : String; O : Object'Class) is
      use type Ada.Tags.Tag;
   begin
--      if O'Tag = Types.RPC_Any_Type'Tag then
--         raise Data_Error
--           with Expected & " expected, found "
--             & Tags.Expanded_Name (RPC_Any_Type (O).O.O'Tag)
--             & " in an RPC_Any_Type object.";
--      else
         raise Data_Error
           with Expected & " expected, found " & Tags.Expanded_Name (O'Tag);
--      end if;
   end Get_Error;

   function I
     (V    : Integer;
      Name : String := "item") return RPC_Integer is
   begin
      return
        (Finalization.Controlled
         with To_Unbounded_String (Name), V);
   end I;

   function Image (O : Object) return String is
      pragma Unreferenced (O);
   begin
      return "";
   end Image;

   overriding function Image (O : RPC_String) return String is
   begin
        return To_String (O.V);
   end Image;

   overriding function Image (O : RPC_Array) return String is
      Result : Unbounded_String;
   begin
      Append (Result, '(');

      for K in O.O'Range loop
         Append (Result, Integer'Image (K));
         Append (Result, " => ");
         Append (Result, Image (O.O (K).O.all));

         if K /= O.O'Last then
            Append (Result, ", ");
         end if;
      end loop;

      Append (Result, ')');

      return To_String (Result);
   end Image;

   overriding function Image (O : RPC_Base64) return String is
   begin
      return To_String (O.V);
   end Image;

   overriding function Image (O : RPC_Boolean) return String is
   begin
      if O.V then
         return "1";
      else
         return "0";
      end if;
   end Image;

   overriding function Image (O : RPC_Double) return String is
      Result : String (1 .. Long_Long_Float'Width);
   begin
      Long_Float_Text_IO.Put (Result, O.V, Exp => 0);
      return Strings.Fixed.Trim (Result, Strings.Both);
   end Image;

   overriding function Image (O : RPC_Integer) return String is
      V : constant String := Integer'Image (O.V);
   begin
      if O.V >= 0 then
         return V (V'First + 1 .. V'Last);
      else
         return V;
      end if;
   end Image;

   overriding function Image (O : RPC_Record) return String is
      Result : Unbounded_String;
   begin
      Append (Result, '(');

      for K in O.O'Range loop
         Append (Result, Name (O.O (K).O.all));
         Append (Result, " => ");
         Append (Result, Image (O.O (K).O.all));

         if K /= O.O'Last then
            Append (Result, ", ");
         end if;
      end loop;

      Append (Result, ')');

      return To_String (Result);
   end Image;

   overriding function Image (O : RPC_Date_Time) return String is
   begin
      return GNAT.Calendar.Time_IO.Image (O.T, "%Y-%m-%dT%H:%M:%S");
   end Image;

   overriding procedure Initialize (O : in out Composite) is
   begin
      O.Ref_Counter := new Natural'(1);
   end Initialize;

   function N (Name : String  := "item") return RPC_Null is
   begin
      return (Ada.Finalization.Controlled with
                To_Unbounded_String (Name));
   end N;

   function Name (O : Object'Class) return String is
   begin
      return To_String (O.Name);
   end Name;

   function R
     (V         : Object_Set;
      Name      : String;
      Type_Name : String := "") return RPC_Record
   is
      function T_Name return String;
      pragma Inline (T_Name);

      ------------
      -- T_Name --
      ------------

      function T_Name return String is
      begin
         if Type_Name = "" then
            return Name;
         else
            return Type_Name;
         end if;
      end T_Name;

   begin
      return
        (Finalization.Controlled
         with To_Unbounded_String (Name),
         new Natural'(1), new Object_Set'(V));
   end R;

   function S
     (V    : String;
      Name : String := "item") return RPC_String
   is
      L_V : constant String := Utils.To_Utf8 (V);
   begin
      return
        (Finalization.Controlled
         with To_Unbounded_String (Name),
         To_Unbounded_String (L_V));
   end S;

   function S
     (V      : Unbounded_String;
      Name   : String  := "item") return RPC_String is
   begin
      return
        (Finalization.Controlled
         with To_Unbounded_String (Name), Utils.To_Utf8 (V));
   end S;

   function Size (O : RPC_Array) return Natural is
   begin
      return O.O'Length;
   end Size;

   function Spaces (N : Natural) return String is
      use Ada.Strings.Fixed;
   begin
      return (3 * N) * ' ';
   end Spaces;

   function T
     (V        : Ada.Calendar.Time;
      Name     : String        := "item") return RPC_Date_Time is
   begin
      return
        (Finalization.Controlled
         with To_Unbounded_String (Name), V);
   end T;

   package XML_Indent is new Ada.Task_Attributes (Natural, 0);

   --  Thread safe Indentation counter

   overriding function V (O : RPC_Array) return Object_Set is
   begin
      return O.O.all;
   end V;

   function V (O : RPC_Array; N : Positive) return Object'Class is
   begin
      return O.O (N).O.all;
   end V;

   function V (O : RPC_Double) return Long_Float is
   begin
      return O.V;
   end V;

   function V (O : RPC_String) return String is
   begin
      return Utils.From_Utf8 (To_String (O.V));
   end V;

   function V (O : RPC_String) return Unbounded_String is
   begin
      return Utils.From_Utf8 (O.V);
   end V;

   function V (O : RPC_Boolean) return Boolean is
   begin
      return O.V;
   end V;

   function V (O : RPC_Integer) return Integer is
   begin
      return O.V;
   end V;

   function V (O : RPC_Record; Name : String) return Object'Class is
   begin
      for K in O.O'Range loop
         if Types.Name (O.O (K).O.all) = Name then
            return O.O (K).O.all;
         end if;
      end loop;

      raise Types.Data_Error
        with "(V) Struct object " & Name & " not found";
   end V;

   overriding function V (O : RPC_Record) return Object_Set is
   begin
      return O.O.all;
   end V;

   function V (O : RPC_Base64) return String is
   begin
      return To_String (O.V);
   end V;

   function V (O : RPC_Date_Time) return Calendar.Time is
   begin
      return O.T;
   end V;

   overriding function XML_Image (O : RPC_Array) return String is
      Indent : constant Natural := XML_Indent.Value;
      Result : Unbounded_String;
   begin
      --  Open array element

      Append (Result, Spaces (Indent));
      Append (Result, "<array>" & New_Line);
      Append (Result, Spaces (Indent + 1));
      Append (Result, "<data>" & New_Line);

      --  Add all elements

      XML_Indent.Set_Value (Indent + 2);

      for K in O.O'Range loop
         Append (Result, XML_Image (O.O (K).O.all));
         Append (Result, New_Line);
      end loop;

      XML_Indent.Set_Value (Indent);

      --  End array element

      Append (Result, Spaces (Indent + 1));
      Append (Result, "</data>" & New_Line);
      Append (Result, Spaces (Indent));
      Append (Result, "</array>" & New_Line);

      return To_String (Result);
   end XML_Image;

   overriding function XML_Image (O : RPC_String) return String is
   begin
      return XML_Image (Object (O));
   end XML_Image;

   overriding function XML_Image (O : RPC_Null) return String is
   begin
      return XML_Image (Object (O));
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

   overriding function XML_Image (O : RPC_Base64) return String is
   begin
      return XML_Image (Object (O));
   end XML_Image;

   overriding function XML_Image (O : RPC_Boolean) return String is
   begin
      return XML_Image (Object (O));
   end XML_Image;

   overriding function XML_Image (O : RPC_Double) return String is
   begin
      return XML_Image (Object (O));
   end XML_Image;

   overriding function XML_Image (O : RPC_Integer) return String is
   begin
      return XML_Image (Object (O));
   end XML_Image;

   overriding function XML_Image (O : RPC_Record) return String is
      Indent : constant Natural := XML_Indent.Value;
      Result : Unbounded_String;
   begin
      Append (Result, Spaces (Indent));
      Append (Result, Utils.Tag (XML_Type (O), Start => True));
      Append (Result, New_Line);

      XML_Indent.Set_Value (Indent + 1);

      for K in O.O'Range loop
         Append (Result, "<member>" & New_Line);
         XML_Indent.Set_Value (Indent + 2);
         Append (Result, "<name>");
         Append (Result, Name (O.O (K).O.all));
         Append (Result, "</name>" & New_Line);
         Append (Result, "<value>");
         Append (Result, XML_Image (O.O (K).O.all));
         Append (Result, "</value>" & New_Line);
         XML_Indent.Set_Value (Indent + 1);
         Append (Result, "</member>" & New_Line);
      end loop;

      XML_Indent.Set_Value (Indent);

      Append (Result, Spaces (Indent));
      Append (Result, Utils.Tag (XML_Type (O), Start => False));

      return To_String (Result);
   end XML_Image;

   overriding function XML_Image (O : RPC_Date_Time) return String is
   begin
      return XML_Image (Object (O));
   end XML_Image;

   --------------
   -- XML_Type --
   --------------

   overriding function XML_Type (O : RPC_String) return String is
      pragma Unreferenced (O);
   begin
      return XML_String;
   end XML_Type;

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

   overriding function XML_Type (O : RPC_Array) return String is
      pragma Warnings (Off, O);
   begin
      return XML_Array;
   end XML_Type;

   overriding function XML_Type  (O : RPC_Base64) return String is
      pragma Unreferenced (O);
   begin
      return XML_Base64;
   end XML_Type;

   overriding function XML_Type  (O : RPC_Boolean) return String is
      pragma Unreferenced (O);
   begin
      return XML_Boolean;
   end XML_Type;

   overriding function XML_Type  (O : RPC_Double) return String is
      pragma Unreferenced (O);
   begin
      return XML_Double;
   end XML_Type;

   overriding function XML_Type  (O : RPC_Integer) return String is
      pragma Unreferenced (O);
   begin
      return XML_Int;
   end XML_Type;

   overriding function XML_Type  (O : RPC_Record) return String is
      pragma Unreferenced (O);
   begin
      return XML_Record;
   end XML_Type;

   overriding function XML_Type  (O : RPC_Date_Time) return String is
      pragma Unreferenced (O);
   begin
      return XML_Date_Time;
   end XML_Type;

end XMLrpc.Types;
