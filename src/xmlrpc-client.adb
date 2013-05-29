with AWS.Messages;
with AWS.Response;
with AWS.URL;
with AWS.Translator;

with XMLrpc.Message.Payload; use XMLrpc;
with XMLrpc.Message.XML;
with XMLrpc.Message.Response.Error;
with AWS.MIME;

package body XMLrpc.Client is

   ----------
   -- Call --
   ----------

   function Call
     (URL        : String;
      P          : Message.Payload.Object;
      User       : String                     := Not_Specified;
      Pwd        : String                     := Not_Specified;
      Proxy      : String                     := Not_Specified;
      Proxy_User : String                     := Not_Specified;
      Proxy_Pwd  : String                     := Not_Specified;
      Timeouts   : AWS.Client.Timeouts_Values := AWS.Client.No_Timeout)
      return Message.Response.Object'Class
   is
      Connection : AWS.Client.HTTP_Connection;
   begin
      AWS.Client.Create
        (Connection,
         URL, User, Pwd, Proxy, Proxy_User, Proxy_Pwd,
         Persistent => False,
         Timeouts   => Timeouts);

      declare
         Result : constant Message.Response.Object'Class :=
                    Call (Connection, P);
      begin
         AWS.Client.Close (Connection);
         return Result;
      end;
   exception
      when others =>
         AWS.Client.Close (Connection);
         raise;
   end Call;

   ----------
   -- Call --
   ----------

   function Call
     (Connection : AWS.Client.HTTP_Connection;
      P          : Message.Payload.Object)
      return Message.Response.Object'Class
   is
      use type AWS.Messages.Status_Code;

      Response : AWS.Response.Data;
   begin
      Post (Connection, Response, Message.XML.Image (P), True);

      if AWS.Response.Status_Code (Response) in AWS.Messages.Success then
         return Message.XML.Load_Response (Connection);

      else
         return Message.Response.Error.Build
           (Faultcode   => Message.Response.Error.HTTP_Error,
            Faultstring => AWS.Response.Message_Body (Response));
      end if;
   end Call;

   procedure Post
     (Connection  : AWS.Client.HTTP_Connection;
      Result      : out AWS.Response.Data;
      Data        : String;
      Streaming   : Boolean         := False;
      Attachments : AWS.Client.Attachment_List := Empty_Attachment_List;
      Headers     : AWS.Client.Header_List     := Empty_Header_List)
   is
   begin
      AWS.Client.SOAP_Post
        (Connection   => Connection,
         Result       => Result,
         Data         => Data,
         SOAPAction   => No_Data,
         Streaming    => Streaming,
         Attachments  => Attachments,
         Headers      => Headers);
   end Post;

end XMLrpc.Client;
