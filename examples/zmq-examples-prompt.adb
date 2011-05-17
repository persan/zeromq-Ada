with ZMQ.Sockets;
with ZMQ.Contexts;
with ZMQ.Messages;
with Ada.Text_IO;
with GNAT.Sockets;
procedure ZMQ.examples.prompt is

   ctx : aliased Contexts.Context;
   s   : Sockets.Socket;

begin
   ctx.Initialize (1);
   s.Initialize (ctx, Sockets.PUB);
   s.Connect ("tcp://localhost:5555");

   Read_Loop : for i in 1..100 loop
      Ada.Text_IO.Put_Line (">");
      declare
         textbuf : constant String :=  "Test";
         message_to_send : constant String := "hej" & ASCII.NUL & GNAT.Sockets.Host_Name & ":" & textbuf;
         query        : ZMQ.Messages.Message;
      begin
         query.Initialize(message_to_send);
         s.Send (query);
         query.Finalize;
         delay 0.02;
      end;
   end loop Read_Loop;
   s.Send (END_MESSAGE);
end ZMQ.Examples.prompt;
