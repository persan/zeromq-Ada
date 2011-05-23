with ZMQ.Sockets;
with ZMQ.Contexts;
with ZMQ.Messages;
with Ada.Text_IO; use Ada.Text_IO;

procedure ZMQ.examples.pull is 
      ctx              : ZMQ.Contexts.Context;
      puller                : ZMQ.Sockets.Socket;
begin
         ctx.Initialize (1);
         puller.Initialize (ctx, Sockets.pull);
         puller.bind("tcp://*:5555");
   for i in  1 .. 10 loop
      declare
         query : ZMQ.Messages.Message;
      begin
         query.Initialize;
         --  Receive a message, blocks until one is available
         puller.recv (query);
         --  Process the query
         Put_Line (query.getData);
      end;

   end loop;
end ZMQ.examples.pull;
