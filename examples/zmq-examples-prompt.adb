with ZMQ.Sockets;
with ZMQ.Contexts;
with ZMQ.Messages;
with Ada.Command_Line;
with Ada.Text_IO;

procedure zmq.examples.prompt is

   ctx : aliased Contexts.Context;
   s   : Sockets.Socket;

   Address  : constant String := Ada.Command_Line.Argument (1);
   username : constant String := Ada.Command_Line.Argument (2);

begin
   Ctx.Initialize (1, 1);
   s.initialize (ctx, Sockets.PUB);
   s.Connect (Address);
   loop
      declare
         textbuf : constant String := Username & Ada.Text_IO.Get_Line;
         msg     : Messages.Message;
      begin
         msg.Initialize(textbuf);
         exit when textbuf'Length = 0;
         S.Send (Msg);
         msg.Finalize;
      end;
   end loop;
end zmq.examples.prompt;
