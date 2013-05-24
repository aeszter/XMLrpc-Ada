with AWS.Client; use AWS.Client;
with XMLrpc.Message.Payload;
with XMLrpc.Message.Response;
with AWS.Response;

use XMLrpc;

package XMLrpc.Client is

   Not_Specified : String renames AWS.Client.No_Data;

   function Call
     (URL        : String;
      P          : Message.Payload.Object;
      User       : String                     := Not_Specified;
      Pwd        : String                     := Not_Specified;
      Proxy      : String                     := Not_Specified;
      Proxy_User : String                     := Not_Specified;
      Proxy_Pwd  : String                     := Not_Specified;
      Timeouts   : AWS.Client.Timeouts_Values := AWS.Client.No_Timeout)
      return Message.Response.Object'Class;

   function Call
     (Connection : AWS.Client.HTTP_Connection;
      P          : Message.Payload.Object)
      return Message.Response.Object'Class;

private
     procedure Post
     (Connection  : AWS.Client.HTTP_Connection;
      Result      : out AWS.Response.Data;
      Data        : String;
      Streaming   : Boolean         := False;
      Attachments : AWS.Client.Attachment_List := Empty_Attachment_List;
      Headers     : AWS.Client.Header_List     := Empty_Header_List);

end XMLrpc.Client;
