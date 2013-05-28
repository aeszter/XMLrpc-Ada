with Ada.Characters.Handling;
with Ada.Strings.Fixed;
with Ada.Exceptions;

with AWS.Client.XML.Input_Sources;

with Input_Sources.Strings;
with Unicode.CES.Utf8;
with DOM.Core.Nodes;
with Sax.Readers;

with SOAP.XML;

with XMLrpc.Message.Reader;
with XMLrpc.Message.Response.Error;
--  with XMLrpc.Name_Space;
--  with XMLrpc.Types.Untyped;
with XMLrpc.Utils;
with XMLrpc.Types;

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
     (Void, T_Undefined,
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

   procedure Parse_Wrapper
     (N : DOM.Core.Node;
      S : in out State);

   function Parse_Param
     (N : DOM.Core.Node;
      S : State) return XMLrpc.Types.Object'Class;

   function Parse_Value
     (N : DOM.Core.Node;
      S : State) return XMLrpc.Types.Object'Class;

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
                 T_Undefined    =>
                   (Types.XML_Undefined'Access, null),
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
                   (Types.XML_Time_Instant'Access, Parse_Time_Instant'Access),
                 T_Struct         =>
                   (Types.XML_Struct'Access, Parse_Struct'Access),
                 T_Array          =>
                   (Types.XML_Array'Access, Parse_Array'Access));

   procedure Error (Node : DOM.Core.Node; Message : String);

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
               (String'(XMLrpc.Parameters.Get (S.Parameters, "faultcode"))),
            Faultstring => XMLrpc.Parameters.Get (S.Parameters, "faultstring"));
      else
         return Message.Response.Object'
           (Message.Object'(S.Wrapper_Name, S.Parameters)
            with null record);
      end if;

   exception
      when E : others =>
         return Message.Response.Error.Build
           (Faultcode   => Message.Response.Error.Client,
            Faultstring => Exceptions.Exception_Message (E));
   end Load_Response;

   procedure Parse_Body (N : DOM.Core.Node; S : in out State) is
   begin
      Parse_Wrapper (SOAP.XML.First_Child (N), S);
   end Parse_Body;

   procedure Parse_Document (N : DOM.Core.Node; S : in out State) is
      NL : constant DOM.Core.Node_List := Child_Nodes (N);
   begin
      if Length (NL) = 1 then
         Parse_Envelope (SOAP.XML.First_Child (N), S);
      else
         Error (N, "Document must have a single node, found "
                & Natural'Image (Length (NL)));
      end if;
   end Parse_Document;

   procedure Parse_Envelope (N : DOM.Core.Node; S : in out State) is
      --  the Envelope is either a methodCall or a methodResponse
      NL : constant DOM.Core.Node_List := Child_Nodes (N);
      LS : State := S;
   begin
      if Length (NL) = 1 then
         --  This must be the body, i.e. a params or a fault
         Parse_Body (SOAP.XML.First_Child (N), LS);

      elsif Length (NL) = 2 then
         --  The first child must the header , i.e. a methodName
         Parse_Header (SOAP.XML.First_Child (N), LS);

         --  The second child must be the body
         Parse_Body (SOAP.XML.Next_Sibling (First_Child (N)), LS);
      else
         Error (N, "Envelope must have at most two nodes, found "
                & Natural'Image (Length (NL)));
      end if;

      S := LS;
   end Parse_Envelope;

   procedure Parse_Header (N : DOM.Core.Node; S : in out State) is
      pragma Unreferenced (S);
      Name : constant String := Local_Name (N);
   begin
      if Ada.Characters.Handling.To_Lower (Name) /= "methodname" then
         Error (N, "methodName node expected, found " & Name);
      end if;
   end Parse_Header;

   function Parse_Param
     (N : DOM.Core.Node;
      S : State) return Types.Object'Class
   is
      use type DOM.Core.Node;
      use type DOM.Core.Node_Types;
      Name   : constant String := Local_Name (N);

   begin
      if Ada.Characters.Handling.To_Lower (Name) = "value" then
         return Parse_Value (SOAP.XML.First_Child (N), S);
      else
         Error (N, "value node expected, found " & Name);
      end if;
   end Parse_Param;

   function Parse_Value
     (N : DOM.Core.Node;
      S : State) return Types.Object'Class
   is
      use type DOM.Core.Node;
      use type DOM.Core.Node_Types;
      Name   : constant String := Local_Name (N);
      S_Type : constant Type_State := To_Type (Name);

   begin
      return Handlers (S_Type).Handler (Name, N);
   end Parse_Value;

   procedure Parse_Wrapper (N : DOM.Core.Node; S : in out State) is
      use type XMLrpc.Parameters.List;
      use type DOM.Core.Node_Types;

      NL     : constant DOM.Core.Node_List      := Child_Nodes (N);
      Prefix : constant String                  := DOM.Core.Nodes.Prefix (N);
      Name   : constant String                  := Local_Name (N);
      Atts   : constant DOM.Core.Named_Node_Map := Attributes (N);
      LS     : State := S;

   begin
      S.Wrapper_Name := To_Unbounded_String (Name);

      for K in 0 .. Length (NL) - 1 loop
         if Item (NL, K).Node_Type /= DOM.Core.Text_Node then
            S.Parameters := S.Parameters & Parse_Param (Item (NL, K), LS);
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
             Handlers (K).Name = Type_Name
         then
            return K;
         end if;
      end loop;

      return T_Undefined;
   end To_Type;

end XMLrpc.Message.XML;
