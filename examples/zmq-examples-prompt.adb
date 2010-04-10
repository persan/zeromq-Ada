with ZMQ.Sockets;
with ZMQ.Contexts;
with ZMQ.Messages;
with Ada.Command_Line;
with Ada.Text_IO;
with GNAT.Sockets;
procedure zmq.examples.prompt is

   ctx : aliased Contexts.Context;
   s   : Sockets.Socket;

begin
   Ctx.Initialize (1, 1);
   s.initialize (ctx, Sockets.PUB);
   s.Bind ("tcp://lo:5555");
   loop
      declare
         textbuf : constant String :=  Ada.Text_IO.Get_Line;
      begin
         exit when textbuf'Length = 0;
         S.Send (GNAT.Sockets.Host_Name & ":" & textbuf);
      end;
   end loop;
end zmq.examples.prompt;
