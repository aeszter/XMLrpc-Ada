package body XMLrpc.Message is

   function XML_Image (M : Object) return Unbounded_String is

      procedure Add_Namespaces (O : Types.Object'Class);
      --  Add name space reference into Message_Header

      New_Line       : constant String                 := ASCII.CR & ASCII.LF;
      NS             : constant SOAP.Name_Space.Object := Name_Space (M);
      NS_Name        : constant String                 :=
                         SOAP.Name_Space.Name (NS);
      Message_Header : Unbounded_String;
      Message_Body   : Unbounded_String;

      --------------------
      -- Add_Namespaces --
      --------------------

      procedure Add_Namespaces (O : Types.Object'Class) is
         use SOAP.Name_Space;
         use SOAP.Types;
         NS : constant SOAP.Name_Space.Object := Types.Name_Space (O);
      begin
         if NS /= No_Name_Space
           and then NS /= SOAP.Name_Space.AWS
           and then Index
             (Message_Header, ':' & SOAP.Name_Space.Name (NS) & '=') = 0
         then
            Append (Message_Header, " " & Image (NS));
         end if;

         --  If this is a composite object, check components

         if O in Types.Composite'Class then
            declare
               OS : constant Object_Set := V (Composite'Class (O));
            begin
               for K in OS'Range loop
                  Add_Namespaces (-OS (K));
               end loop;
            end;
         end if;
      end Add_Namespaces;

   begin
      --  Procedure

      Append (Message_Header, '<');

      if NS_Name /= "" then
         Append (Message_Header, NS_Name & ':');
      end if;

      Append (Message_Header, Wrapper_Name (M));

      Append (Message_Body, " xmlns");

      if NS_Name /= "" then
         Append (Message_Body, ":" & NS_Name);
      end if;

      Append
        (Message_Body,
         "=""" & SOAP.Name_Space.Value (NS) & """>" & New_Line);

      --  Procedure's parameters

      declare
         P : constant SOAP.Parameters.List := Parameters (M);
      begin
         for K in 1 .. SOAP.Parameters.Argument_Count (P) loop
            Add_Namespaces (SOAP.Parameters.Argument (P, K));

            Append
              (Message_Body,
               Types.XML_Image (SOAP.Parameters.Argument (P, K)) & New_Line);
         end loop;
      end;

      --  Close payload objects

      Append (Message_Body, "</");

      if NS_Name /= "" then
         Append (Message_Body, NS_Name & ':');
      end if;

      Append (Message_Body, Wrapper_Name (M));
      Append (Message_Body, ">" & New_Line);

      return Message_Header & Message_Body;
   end XML_Image;

   ----------------
   -- Parameters --
   ----------------

   function Parameters (M : Object'class) return XMLrpc.Parameters.List is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Parameters unimplemented");
      raise Program_Error;
      return Parameters (M);
   end Parameters;

end XMLrpc.Message;
