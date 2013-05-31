with DOM.Core;
with SOAP.XML;

package XMLrpc.XML is

   function First_Child (Parent : DOM.Core.Node) return DOM.Core.Node
renames SOAP.XML.First_Child;
   function Get_Ref (N : DOM.Core.Node) return DOM.Core.Node
     renames SOAP.XML.Get_Ref;
   function Next_Sibling (N : DOM.Core.Node) return DOM.Core.Node
     renames SOAP.XML.Next_Sibling;

end XMLrpc.XML;
