generic
   type Element_Type (<>) is private;
   type Root_Socket is new ZMQ.Sockets.Socket with private;
package ZMQ.Sockets.Indefinite_Typed_Generic is
   type Socket is new Root_Socket with private;

   not overriding
   procedure Send
     (This  : in out Socket;
      Msg   : Element_Type);

   not overriding
   procedure Recv
     (This       : in Socket;
      Msg        : out Element_Type);
private
   type Socket is new Root_Socket with  record
      null;
   end record;
end ZMQ.Sockets.Indefinite_Typed_Generic;
