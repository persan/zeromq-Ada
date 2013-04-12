generic
   type Element_Type is private;
   pragma Compile_Time_Error
     (Element_Type'Has_Access_Values, "No access values allowed in Element");
   type Socket is new ZMQ.Sockets.Socket with private;
package ZMQ.Sockets.Typed_Generic is
   type Typed_Socket is new Socket with private;

   not overriding
   procedure Send
     (This  : in out Typed_Socket;
      Msg   : Element_Type;
      Flags : Socket_Flags := No_Flags);

   not overriding
   function Recv
     (This       : in Typed_Socket;
      Flags      : Socket_Flags := No_Flags)
      return  Element_Type;

   not overriding
   procedure Recv
     (This       : in Typed_Socket;
      msg        : out Element_Type;
      Flags      : Socket_Flags := No_Flags);
private
   type Typed_Socket is new Socket with null record;
end  ZMQ.Sockets.Typed_Generic;
