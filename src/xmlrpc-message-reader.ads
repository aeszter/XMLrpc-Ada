with Sax.Readers;    use Sax.Readers;
with Sax.Attributes;
with Unicode.CES;
with DOM.Core;       use DOM.Core;

private package XMLrpc.Message.Reader is

   type Tree_Reader is new Sax.Readers.Reader with private;

   function Get_Tree (Read : Tree_Reader) return Document;

private

   type Tree_Reader is new Sax.Readers.Reader with record
      Tree              : Document;
      Current_Node      : Node;
      Internal_Encoding : Unicode.CES.Encoding_Scheme;
   end record;

   overriding procedure Start_Document
     (Handler : in out Tree_Reader);

   overriding procedure Start_Element
     (Handler       : in out Tree_Reader;
      Namespace_URI : Unicode.CES.Byte_Sequence       := "";
      Local_Name    : Unicode.CES.Byte_Sequence       := "";
      Qname         : Unicode.CES.Byte_Sequence       := "";
      Atts          : Sax.Attributes.Attributes'Class);

   overriding procedure End_Element
     (Handler       : in out Tree_Reader;
      Namespace_URI : Unicode.CES.Byte_Sequence := "";
      Local_Name    : Unicode.CES.Byte_Sequence := "";
      Qname         : Unicode.CES.Byte_Sequence := "");

   overriding procedure Characters
     (Handler : in out Tree_Reader;
      Ch      : Unicode.CES.Byte_Sequence);

   overriding procedure Ignorable_Whitespace
     (Handler : in out Tree_Reader;
      Ch      : Unicode.CES.Byte_Sequence);

end XMLrpc.Message.Reader;
