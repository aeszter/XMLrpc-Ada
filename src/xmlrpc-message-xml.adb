with Ada.Characters.Handling;
with Ada.Strings.Fixed;
with Ada.Exceptions;

with AWS.Client.XML.Input_Sources;

with Input_Sources.Strings;
with Unicode.CES.Utf8;
with DOM.Core.Nodes;
with Sax.Readers;

with XMLrpc.XML;

with XMLrpc.Message.Reader;
with XMLrpc.Message.Response.Error;
--  with XMLrpc.Name_Space;
--  with XMLrpc.Types.Untyped;
with XMLrpc.Utils;
with XMLrpc.Types;
with Ada.Text_IO;

package body XMLrpc.Message.XML is

   use Ada;
   use DOM.Core.Nodes;
   use XMLrpc.Message.Reader;

   NL : constant String := ASCII.CR & ASCII.LF;

   Max_Object_Size : constant := 2_048;
   --  This is the maximum number of items in a record or an array supported
   --  by this implementation.

   XML_Header : constant String := "<?xml version=""1.0""?>";

   Start_Body : constant String := "<methodCall>";
   End_Body   : constant String := "</methodCall>";

   type Type_State is
     (Void,
      T_Int, T_Boolean, T_String, T_Double,
      T_Time_Instant, T_Base64,
      T_Struct, T_Array);

   type State is record
      Wrapper_Name : Unbounded_String;
      Parameters   : XMLrpc.Parameters.List;
      A_State      : Type_State := Void;
   end record;

   function To_Type
     (Type_Name : String)
      return Type_State;

   procedure Parse_Document
     (N : DOM.Core.Node;
      S : in out State);

   procedure Parse_Envelope
     (N : DOM.Core.Node;
      S : in out State);

   procedure Parse_Header
     (N : DOM.Core.Node;
      S : in out State);

   procedure Parse_Body
     (N : DOM.Core.Node;
      S : in out State);

   procedure Parse_Wrapper (N : DOM.Core.Node; S : in out State);
   procedure Parse_Fault (N : DOM.Core.Node; S : in out State);

   function Parse_Param
     (N : DOM.Core.Node) return XMLrpc.Types.Object'Class;
   function Parse_Name (N : DOM.Core.Node) return String;

   function Parse_Value
     (N : DOM.Core.Node; Name : String) return XMLrpc.Types.Object'Class;

   --  Parse routines for specific types

   function Parse_Any_Type
     (Name : String;
      N    : DOM.Core.Node) return Types.Object'Class;

   function Parse_Int
     (Name : String;
      N    : DOM.Core.Node) return Types.Object'Class;

   function Parse_Double
     (Name : String;
      N    : DOM.Core.Node) return Types.Object'Class;

   function Parse_String
     (Name : String;
      N    : DOM.Core.Node) return Types.Object'Class;

   function Parse_Boolean
     (Name : String;
      N    : DOM.Core.Node) return Types.Object'Class;

   function Parse_Base64
     (Name : String;
      N    : DOM.Core.Node) return Types.Object'Class;

   function Parse_Time_Instant
     (Name : String;
      N    : DOM.Core.Node) return Types.Object'Class;

   function Parse_Array
     (Name : String;
      N    : DOM.Core.Node) return Types.Object'Class;

   function Parse_Record
     (Name : String;
      N    : DOM.Core.Node) return Types.Object'Class;

   procedure Error (Node : DOM.Core.Node; Message : String);
   pragma No_Return (Error);

   type Parse_Type is access
     function (Name : String;
               N    : DOM.Core.Node) return Types.Object'Class;

   type Type_Handler is record
      Name    : access constant String;
      Handler : Parse_Type;
   end record;

   Handlers : constant array (Type_State) of Type_Handler :=
                (Void           =>
                   (null, null),
                 T_Int            =>
                   (Types.XML_Int'Access, Parse_Int'Access),
                 T_Double         =>
                   (Types.XML_Double'Access, Parse_Double'Access),
                 T_String         =>
                   (Types.XML_String'Access, Parse_String'Access),
                 T_Boolean        =>
                   (Types.XML_Boolean'Access, Parse_Boolean'Access),
                 T_Base64         =>
                   (Types.XML_Base64'Access, Parse_Base64'Access),
                 T_Time_Instant   =>
                   (Types.XML_Date_Time'Access, Parse_Time_Instant'Access),
                 T_Struct         =>
                   (Types.XML_Record'Access, Parse_Record'Access),
                 T_Array          =>
                   (Types.XML_Array'Access, Parse_Array'Access));

   -----------
   -- Error --
   -----------

   procedure Error (Node : DOM.Core.Node; Message : String) is
      Name : constant String := Local_Name (Node);
   begin
      raise XMLrpc_Error with Name & " - " & Message;
   end Error;

   -----------
   -- Image --
   -----------

   function Image (O : Object'Class) return String is
   begin
      return To_String (XML.Image (O));
   end Image;

   -----------
   -- Image --
   -----------

   function Image (O : Object'Class) return Unbounded_String is
      Message_Body : Unbounded_String;
   begin
      --  Header

      Append (Message_Body, XML_Header & NL);

      --  Body

      Append (Message_Body, Start_Body & NL);

      --  Wrapper

      Append (Message_Body, Message.XML_Image (O));

      --  End of Body and Envelope

      Append (Message_Body, End_Body & NL);

      return Message_Body;
   end Image;

   -------------------
   -- Load_Response --
   -------------------

   function Load_Response
     (Connection : AWS.Client.HTTP_Connection)
      return Message.Response.Object'Class
   is
      use AWS.Client.XML.Input_Sources;

      Source : HTTP_Input;
      Reader : Tree_Reader;
      S      : State;
      Doc    : DOM.Core.Document;

   begin
      Create (Connection, Source);

      --  If True, xmlns:* attributes will be reported in Start_Element
      Set_Feature (Reader, Sax.Readers.Namespace_Prefixes_Feature, True);
      Set_Feature (Reader, Sax.Readers.Validation_Feature, False);

      Parse (Reader, Source);
      Close (Source);

      Doc := Get_Tree (Reader);

      Parse_Document (Doc, S);

      Free (Doc);

      if XMLrpc.Parameters.Exist (S.Parameters, "faultcode") then
         return Message.Response.Error.Build
           (Faultcode   =>
              Message.Response.Error.Faultcode
               (Integer'(XMLrpc.Parameters.Get (S.Parameters, "faultcode"))),
            Faultstring => XMLrpc.Parameters.Get (S.Parameters, "faultstring"));
      else
         return Message.Response.Object'
           (Message.Object'(S.Wrapper_Name, S.Parameters)
            with null record);
      end if;

   exception
      when E : others =>
         return Message.Response.Error.Build
           (Faultcode   => Message.Response.Error.Internal_Error,
            Faultstring => Exceptions.Exception_Message (E));
   end Load_Response;

   function Parse_Any_Type
     (Name : String;
      N    : DOM.Core.Node) return Types.Object'Class is
   begin
      --  "If no type is indicated, the type is string."
      return Parse_String (Name, N);
   end Parse_Any_Type;

   function Parse_Array
     (Name : String;
      N    : DOM.Core.Node) return Types.Object'Class
   is
      pragma Unreferenced (Name);
      use type DOM.Core.Node;
      use XMLrpc.Types;

      OS    : Types.Object_Set (1 .. Max_Object_Size);
      K     : Natural := 0;

      Field : DOM.Core.Node;

   begin
      Field := XMLrpc.XML.First_Child (N);

      while Field /= null loop
         K := K + 1;

         OS (K) := +Parse_Param (Field);
         Field := Next_Sibling (Field);
      end loop;

      return Types.A (OS (1 .. K));
   end Parse_Array;

   function Parse_Base64
     (Name : String;
      N    : DOM.Core.Node) return Types.Object'Class
   is
      use type DOM.Core.Node;

      Value : DOM.Core.Node;
   begin
      Normalize (N);
      Value := First_Child (N);

      if Value = null then
         --  No node found, this is an empty Base64 content
         return Types.B64 ("", Name);

      else
         return Types.B64 (Node_Value (Value), Name);
      end if;
   end Parse_Base64;

   procedure Parse_Body (N : DOM.Core.Node; S : in out State) is
      Name : constant String := Ada.Characters.Handling.To_Lower (Local_Name (N));
      use type XMLrpc.Parameters.List;

   begin
      Ada.Text_IO.Put_Line ("Parse_Body");
      if Name = "params" then
         Parse_Wrapper (XMLrpc.XML.First_Child (N), S);
      elsif Name = "fault" then
         Parse_Fault (XMLrpc.XML.First_Child (N), S);
      else
         Error (N, "expected ""params"" or ""fault""");
      end if;
   end Parse_Body;

   function Parse_Boolean
     (Name : String;
      N    : DOM.Core.Node) return Types.Object'Class
   is
      Value : constant DOM.Core.Node := First_Child (N);
   begin
      if Node_Value (Value) = "1"
        or else Node_Value (Value) = "true"
        or else Node_Value (Value) = "TRUE"
      then
         return Types.B (True, Name);
      elsif Node_Value (Value) = "0"
        or else Node_Value (Value) = "false"
        or else Node_Value (Value) = "FALSE"
      then
         return Types.B (False, Name);
      else
         raise Types.Data_Error
            with "did not understand boolean value """ & Node_Value (Value) & """";
      end if;
   end Parse_Boolean;

   procedure Parse_Document (N : DOM.Core.Node; S : in out State) is
      NL : constant DOM.Core.Node_List := Child_Nodes (N);
   begin
      Ada.Text_IO.Put_Line ("Parse_Document");
      if Length (NL) = 1 then
         Parse_Envelope (XMLrpc.XML.First_Child (N), S);
      else
         Error (N, "Document must have a single node, found "
                & Natural'Image (Length (NL)));
      end if;
   end Parse_Document;

   function Parse_Double
     (Name : String;
      N    : DOM.Core.Node) return Types.Object'Class
   is
      Value : constant DOM.Core.Node := First_Child (N);
   begin
      return Types.D (Long_Float'Value (Node_Value (Value)), Name);
   end Parse_Double;

   procedure Parse_Envelope (N : DOM.Core.Node; S : in out State) is
      --  the Envelope is either a methodCall or a methodResponse
      NL : constant DOM.Core.Node_List := Child_Nodes (N);
      LS : State := S;
      Name : constant String := Ada.Characters.Handling.To_Lower (Local_Name (N));
   begin
      Ada.Text_IO.Put_Line ("Parse_Envelope");
      if Name /= "methodcall" and then
        Name /= "methodresponse" then
         Error (N, "Envelope expected");
      end if;
      if Length (NL) = 1 then
         --  This must be the body, i.e. a params or a fault
         Parse_Body (XMLrpc.XML.First_Child (N), LS);

      elsif Length (NL) = 2 then
         --  The first child must the header , i.e. a methodName
         Parse_Header (XMLrpc.XML.First_Child (N), LS);

         --  The second child must be the body
         Parse_Body (XMLrpc.XML.Next_Sibling (First_Child (N)), LS);
      else
         Error (N, "Envelope must have at most two nodes, found "
                & Natural'Image (Length (NL)));
      end if;

      S := LS;
   end Parse_Envelope;

   procedure Parse_Fault (N : DOM.Core.Node; S : in out State) is
      use type XMLrpc.Parameters.List;
   begin
      S.Parameters := S.Parameters & Parse_Value (N, "Fault");
   end Parse_Fault;

   procedure Parse_Header (N : DOM.Core.Node; S : in out State) is
      pragma Unreferenced (S);
      Name : constant String := Local_Name (N);
   begin
      Ada.Text_IO.Put_Line ("Parse_Header");
      if Ada.Characters.Handling.To_Lower (Name) /= "methodname" then
         Error (N, "methodName node expected, found " & Name);
      end if;
   end Parse_Header;

   function Parse_Int
     (Name : String;
      N    : DOM.Core.Node) return Types.Object'Class
   is
      Value : constant DOM.Core.Node := First_Child (N);
   begin
      Ada.Text_IO.Put_Line ("Parse_Int");
      return Types.I (Integer'Value (Node_Value (Value)), Name);
   end Parse_Int;

   function Parse_Name (N : DOM.Core.Node) return String is
      Node_Name : constant String := Ada.Characters.Handling.To_Lower (Local_Name (N));
   begin
      if Node_Name = "name" then
         return Node_Value (First_Child (N));
      else
         Error (N, """name"" expected");
      end if;
   end Parse_Name;

   function Parse_Param
     (N : DOM.Core.Node) return XMLrpc.Types.Object'Class
   is
      use type DOM.Core.Node;
      use type DOM.Core.Node_Types;
      Name   : constant String := Local_Name (N);

   begin
      Ada.Text_IO.Put_Line ("Parse_Param");
      if Ada.Characters.Handling.To_Lower (Name) = "value" then
         return Parse_Value (N, Name);
      elsif Ada.Characters.Handling.To_Lower (Name) = "member" then
         declare

           --  The first child must be the name
            Param_Name : constant String := Parse_Name (XMLrpc.XML.First_Child (N));

         begin
         --  The second child must be the value
            return Parse_Value (XMLrpc.XML.Next_Sibling (First_Child (N)), Param_Name);
         end;

      else
         Error (N, "value or member node expected, found " & Name);
      end if;
   end Parse_Param;

   function Parse_Record
     (Name : String;
      N    : DOM.Core.Node) return Types.Object'Class
   is
      use type DOM.Core.Node;
      use type DOM.Core.Node_Types;
      use XMLrpc.Types;

      OS : Types.Object_Set (1 .. Max_Object_Size);
      K  : Natural := 0;

      Field : DOM.Core.Node := XMLrpc.XML.Get_Ref (N);
   begin
      Ada.Text_IO.Put_Line ("Parse_Record");
      Field := XMLrpc.XML.First_Child (Field);

      while Field /= null loop
         K := K + 1;
         OS (K) := +Parse_Param (Field);

         Field := Next_Sibling (Field);
      end loop;

      return Types.R (OS (1 .. K), Name);
   end Parse_Record;

   function Parse_String
     (Name : String;
      N    : DOM.Core.Node) return Types.Object'Class
   is
      use type DOM.Core.Node;
      use type DOM.Core.Node_Types;

      L : constant DOM.Core.Node_List := Child_Nodes (N);
      S : Unbounded_String;
      P : DOM.Core.Node;
   begin
      Ada.Text_IO.Put_Line ("Parse_String");
      for I in 0 .. Length (L) - 1 loop
         P := Item (L, I);
         if P.Node_Type = DOM.Core.Text_Node then
            Append (S, Node_Value (P));
         end if;
      end loop;

      return Types.S (S, Name);
   end Parse_String;

   ------------------------
   -- Parse_Time_Instant --
   ------------------------

   function Parse_Time_Instant
     (Name : String;
      N    : DOM.Core.Node) return Types.Object'Class
   is
      Value : constant DOM.Core.Node := First_Child (N);
      TI    : constant String        := Node_Value (Value);
   begin
      return Utils.Date_Time (TI, Name);
   end Parse_Time_Instant;

   function Parse_Value
     (N : DOM.Core.Node; Name : String) return XMLrpc.Types.Object'Class
   is
      use type DOM.Core.Node;
      use type DOM.Core.Node_Types;
      S_Type : constant Type_State :=
                 To_Type (Local_Name (First_Child (N)));

   begin
      Ada.Text_IO.Put_Line ("Parse_Value");
      return Handlers (S_Type).Handler (Name, First_Child (N));
   end Parse_Value;

   procedure Parse_Wrapper (N : DOM.Core.Node; S : in out State) is
      use type XMLrpc.Parameters.List;
      use type DOM.Core.Node_Types;

      NL     : constant DOM.Core.Node_List      := Child_Nodes (N);
      Prefix : constant String                  := DOM.Core.Nodes.Prefix (N);
      Name   : constant String                  := Local_Name (N);
      Atts   : constant DOM.Core.Named_Node_Map := Attributes (N);

   begin
      Ada.Text_IO.Put_Line ("Parse_Wrapper");
      for K in 0 .. Length (NL) - 1 loop
         if Item (NL, K).Node_Type /= DOM.Core.Text_Node then
            S.Parameters := S.Parameters & Parse_Param (Item (NL, K));
         end if;
      end loop;
   end Parse_Wrapper;

   function To_Type
     (Type_Name : String) return Type_State
   is
   begin
      for K in Handlers'Range loop
         if Handlers (K).Name /= null
           and then
             Handlers (K).Name.all = Type_Name
         then
            return K;
         end if;
      end loop;

      raise XMLrpc_Error
        with "could not find handler for type " & Type_Name;
   end To_Type;

end XMLrpc.Message.XML;
