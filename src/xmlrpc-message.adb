with XMLrpc.Types;

package body XMLrpc.Message is

   function Parameters (M : Object'class) return XMLrpc.Parameters.List is
   begin
      return M.P;
   end Parameters;

   procedure Set_Parameters
     (M     : in out Object'Class;
      P_Set : XMLrpc.Parameters.List) is
   begin
      M.P := P_Set;
   end Set_Parameters;

   ----------------------
   -- Set_Wrapper_Name --
   ----------------------

   procedure Set_Wrapper_Name
     (M    : in out Object'Class;
      Name : String) is
   begin
      M.Wrapper_Name := To_Unbounded_String (Name);
   end Set_Wrapper_Name;

   function Wrapper_Name (M : Object'class) return String is
   begin
      return To_String (M.Wrapper_Name);
   end Wrapper_Name;

   function XML_Image (M : Object) return Unbounded_String is

      New_Line       : constant String := ASCII.CR & ASCII.LF;
      Message_Header : Unbounded_String;
      Message_Body   : Unbounded_String;

   begin
      --  Procedure

      Append (Message_Header, "<methodName>");
      Append (Message_Header, Wrapper_Name (M));
      Append (Message_Header, "</methodName>" & New_Line);

      --  Procedure's parameters

      declare
         P : constant XMLrpc.Parameters.List := Parameters (M);
      begin
         for K in 1 .. XMLrpc.Parameters.Argument_Count (P) loop
            Append
              (Message_Body,
               XMLrpc.Types.XML_Image (XMLrpc.Parameters.Argument (P, K)) & New_Line);
         end loop;
      end;

      --  Close payload objects
      null;
      return Message_Header & Message_Body;
   end XML_Image;

end XMLrpc.Message;
