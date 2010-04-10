 with ZMQ.Sockets;
with ZMQ.Contexts;
with ZMQ.Messages;
with Ada.Text_IO;
procedure zmq.examples.Display is
   use Ada.Text_IO;
   Context  : aliased Contexts.Context;
   Socket   : Sockets.Socket;


begin

   Context.Initialize (1, 1);
   Socket.initialize (Context, Sockets.SUB);
   Socket.setsockopt (Sockets.SUBSCRIBE, "");
   Socket.connect ("tcp://localhost:5555");

   loop
      Ada.Text_Io.Put_Line (Socket.Recv);
   end loop;

end zmq.examples.Display;
