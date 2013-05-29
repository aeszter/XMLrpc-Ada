with Ada.Finalization;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with SOAP.Types;
with Ada.Calendar;

package XMLrpc.Types is

   Data_Error : exception;

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

   type Composite is abstract new Object with private;
   --  Composite types are using a by-reference semantic for efficiency
   --  reason. Not that these types are not thread safe.

   function V (C : Composite) return Object_Set is abstract;

   -----------
   -- Array --
   -----------

   XML_Array     : aliased constant String := "array";

   type RPC_Array is new Composite with private;

   overriding function Image     (O : RPC_Array) return String;
   overriding function XML_Image (O : RPC_Array) return String;
   overriding function XML_Type  (O : RPC_Array) return String;

   function A (V : Object_Set) return RPC_Array;

   function Size (O : RPC_Array) return Natural;

   function V (O : RPC_Array; N : Positive) return Object'Class;

   overriding function V (O : RPC_Array) return Object_Set;

   ------------
   -- Base64 --
   ------------

   XML_Base64        : aliased constant String := "base64";

   type RPC_Base64 is new Scalar with private;

   overriding function Image     (O : RPC_Base64) return String;
   overriding function XML_Image (O : RPC_Base64) return String;
   overriding function XML_Type  (O : RPC_Base64) return String;

   function B64
     (V    : String;
      Name : String := "item") return RPC_Base64;

   function V (O : RPC_Base64) return String;

   -------------
   -- Boolean --
   -------------

   XML_Boolean : aliased constant String := "boolean";

   type RPC_Boolean is new Scalar with private;

   overriding function Image     (O : RPC_Boolean) return String;
   overriding function XML_Image (O : RPC_Boolean) return String;
   overriding function XML_Type  (O : RPC_Boolean) return String;

   function B (V : Boolean; Name : String  := "item") return RPC_Boolean;
   function V (O : RPC_Boolean) return Boolean;

   ------------
   -- Double --
   ------------

   XML_Double : aliased constant String := "double";

   type RPC_Double is new Scalar with private;

   overriding function Image     (O : RPC_Double) return String;
   overriding function XML_Image (O : RPC_Double) return String;
   overriding function XML_Type  (O : RPC_Double) return String;

   function D
     (V    : Long_Float;
      Name : String          := "item") return RPC_Double;

   function V (O : RPC_Double) return Long_Float;

   -------------
   -- Integer --
   -------------

   XML_Int : aliased constant String := "i4";

   type RPC_Integer is new Scalar with private;

   overriding function Image     (O : RPC_Integer) return String;
   overriding function XML_Image (O : RPC_Integer) return String;
   overriding function XML_Type  (O : RPC_Integer) return String;

   function I (V : Integer; Name : String := "item") return RPC_Integer;
   function V (O : RPC_Integer) return Integer;

   ----------
   -- Null --
   ----------

   XML_Null : constant String := "null";
pragma Compile_Time_Warning (Standard.True, "Null is not part of XML-RPC.");
   type RPC_Null is new Scalar with private;

   overriding function XML_Image (O : RPC_Null) return String;
   overriding function XML_Type  (O : RPC_Null) return String;

   function N (Name : String  := "item") return RPC_Null;

   ------------
   -- Record --
   ------------

   XML_Record : aliased constant String := "struct";
   type RPC_Record is new Composite with private;

   overriding function Image     (O : RPC_Record) return String;
   overriding function XML_Image (O : RPC_Record) return String;
   overriding function XML_Type  (O : RPC_Record) return String;

   function R
     (V         : Object_Set;
      Name      : String;
      Type_Name : String := "") return RPC_Record;

   function V (O : RPC_Record; Name : String) return Object'Class;

   overriding function V (O : RPC_Record) return Object_Set;

   ------------
   -- String --
   ------------

   XML_String : aliased constant String := "string";
   type RPC_String is new Scalar with private;

   overriding function Image (O : RPC_String) return String;
   overriding function XML_Image (O : RPC_String) return String;
   overriding function XML_Type (O : RPC_String) return String;

   function S
     (V    : String;
      Name : String := "item") return RPC_String;

   function S
     (V    : Unbounded_String;
      Name : String  := "item") return RPC_String;

   function V (O : RPC_String) return String;

   function V (O : RPC_String) return Unbounded_String;

   -----------------
   -- TimeInstant --
   -----------------

   XML_Date_Time : aliased constant String := "dateTime.iso8601";

   type RPC_Date_Time is new Scalar with private;

   overriding function Image     (O : RPC_Date_Time) return String;
   overriding function XML_Image (O : RPC_Date_Time) return String;
   overriding function XML_Type  (O : RPC_Date_Time) return String;

   function T
     (V        : Ada.Calendar.Time;
      Name     : String        := "item") return RPC_Date_Time;

   function V (O : RPC_Date_Time) return Ada.Calendar.Time;

--   function "+" (O : SOAP.Types.Object'Class) return SOAP.Types.Object_Safe_Pointer
--                 renames SOAP.Types."+";

   function Get (O : Object'Class) return String;
   function Get (O : Object'Class) return Integer;

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

   procedure Get_Error (Expected : String; O : Object'Class);
   pragma No_Return (Get_Error);

   type Scalar is abstract new Object with null record;

      type Counter_Access is access Natural;

   --  Composite

   type Object_Set_Access is access Object_Set;

   type Composite is abstract new Object with record
      Ref_Counter : Counter_Access;
      O           : Object_Set_Access;
   end record;

   overriding procedure Initialize (O : in out Composite);
   pragma Inline (Initialize);

   overriding procedure Adjust     (O : in out Composite);
   pragma Inline (Adjust);

   overriding procedure Finalize   (O : in out Composite);
   pragma Inline (Finalize);

   type RPC_Null is new Scalar with null record;

   type RPC_String is new Scalar with record
      V : Unbounded_String;
   end record;

   type RPC_Integer is new Scalar with record
      V : Integer;
   end record;

   type RPC_Double is new Scalar with record
      V : Long_Float;
   end record;

   type RPC_Boolean is new Scalar with record
      V : Boolean;
   end record;

   type RPC_Date_Time is new Scalar with record
      T        : Ada.Calendar.Time;
   end record;

   type RPC_Base64 is new Scalar with record
      V : Unbounded_String;
   end record;

   type RPC_Array is new Composite with null record;
   type RPC_Record is new Composite with null record;

end XMLrpc.Types;
