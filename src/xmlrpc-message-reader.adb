with Sax.Attributes;       use Sax.Attributes;
with Unicode;              use Unicode;
with Unicode.CES;          use Unicode.CES;
with DOM.Core.Nodes;       use DOM.Core.Nodes;
with DOM.Core.Documents;   use DOM.Core.Documents;
with DOM.Core.Elements;    use DOM.Core.Elements;

with XMLrpc.Utils;

package body XMLrpc.Message.Reader is

   ----------------
   -- Characters --
   ----------------

   overriding procedure Characters
     (Handler : in out Tree_Reader;
      Ch      : Unicode.CES.Byte_Sequence)
   is
      Tmp : Node;
      pragma Unreferenced (Tmp);

   begin
      declare
         --  Ch comes from the SAX parser and is Utf8 encoded. We convert
         --  it back to Basic_8bit (standard Ada strings). Depending on
         --  Ch's value this could raise an exception. For example if
         --  Ch is Utf32 encoded and contains characters outside the
         --  Basic_8bit encoding.
         S : constant String_Access := Utils.From_Utf8 (Ch);
      begin
         Tmp := Append_Child
           (Handler.Current_Node,
            Create_Text_Node (Handler.Tree, DOM_String_Access (S)));
      end;
   exception
      when Unicode.CES.Invalid_Encoding =>
         --  Here we had a problem decoding the string, just keep Ch as-is
         Tmp := Append_Child
           (Handler.Current_Node, Create_Text_Node (Handler.Tree, Ch));
   end Characters;

   -----------------
   -- End_Element --
   -----------------

   overriding procedure End_Element
     (Handler       : in out Tree_Reader;
      Namespace_URI : Unicode.CES.Byte_Sequence := "";
      Local_Name    : Unicode.CES.Byte_Sequence := "";
      Qname         : Unicode.CES.Byte_Sequence := "")
   is
      pragma Unreferenced (Namespace_URI);
      pragma Unreferenced (Local_Name);
      pragma Unreferenced (Qname);
   begin
      Handler.Current_Node := Parent_Node (Handler.Current_Node);
   end End_Element;

   --------------
   -- Get_Tree --
   --------------

   function Get_Tree (Read : Tree_Reader) return Document is
   begin
      return Read.Tree;
   end Get_Tree;

   --------------------------
   -- Ignorable_Whitespace --
   --------------------------

   overriding procedure Ignorable_Whitespace
     (Handler : in out Tree_Reader;
      Ch      : Unicode.CES.Byte_Sequence)
   is
      Tmp : Node;
      pragma Unreferenced (Tmp);

   begin
      --  Ignore these white spaces at the toplevel
      if Ch'Length >= 1
        and then Ch (Ch'First) /= ASCII.LF
        and then Handler.Current_Node /= Handler.Tree
      then
         Tmp := Append_Child
           (Handler.Current_Node, Create_Text_Node (Handler.Tree, Ch));
      end if;
   end Ignorable_Whitespace;

   --------------------
   -- Start_Document --
   --------------------

   overriding procedure Start_Document (Handler : in out Tree_Reader) is
      Implementation : DOM_Implementation;
   begin
      Handler.Tree := Create_Document (Implementation);
      Handler.Current_Node := Handler.Tree;
   end Start_Document;

   -------------------
   -- Start_Element --
   -------------------

   overriding procedure Start_Element
     (Handler       : in out Tree_Reader;
      Namespace_URI : Unicode.CES.Byte_Sequence       := "";
      Local_Name    : Unicode.CES.Byte_Sequence       := "";
      Qname         : Unicode.CES.Byte_Sequence       := "";
      Atts          : Sax.Attributes.Attributes'Class)
   is
      pragma Unreferenced (Local_Name);
   begin
      Handler.Current_Node := Append_Child
        (Handler.Current_Node,
         Create_Element_NS (Handler.Tree,
                            Namespace_URI => Namespace_URI,
                            Qualified_Name => Qname));

      --  Insert the attributes in the right order
      for J in 0 .. Get_Length (Atts) - 1 loop
         Set_Attribute_NS
           (Handler.Current_Node,
            Get_URI (Atts, J),
            Get_Qname (Atts, J),
            Get_Value (Atts, J));
      end loop;
   end Start_Element;

begin
   DOM.Core.Set_Node_List_Growth_Factor (1.0);
end XMLrpc.Message.Reader;
