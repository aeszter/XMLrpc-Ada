package body XMLrpc.Message.XML is

   NL : constant String := ASCII.CR & ASCII.LF;

   Max_Object_Size : constant := 2_048;
   --  This is the maximum number of items in a record or an array supported
   --  by this implementation.

   XML_Header : constant String := "<?xml version=""1.0""?>";

   Start_Body : constant String := "<methodCall>";
   End_Body   : constant String := "</methodCall>";

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
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Load_Response unimplemented");
      raise Program_Error;
      return Load_Response (Connection);
   end Load_Response;

end XMLrpc.Message.XML;
