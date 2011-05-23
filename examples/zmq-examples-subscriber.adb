with ZMQ.Sockets;
with ZMQ.Contexts;
with ZMQ.Messages;
with Ada.Text_IO; use Ada.Text_IO;

procedure ZMQ.examples.Subscriber is
   context          : ZMQ.Contexts.Context;
   subscriber       : ZMQ.Sockets.Socket;
   Test             : String          := "Hello";
begin
   --  Initialise 0MQ context, requesting a single application thread
   --  and a single I/O thread
   context.Initialize (1);

   --   Create a ZMQ_REP socket to subscribe for messages
   subscriber.Initialize (context, ZMQ.Sockets.SUB);
   subscriber.connect("tcp://localhost:5555");
   -- subscribe to all messages started with "Hello"
   subscriber.Establish_message_filter(Test);
   loop 
       declare
           query : ZMQ.Messages.Message;
       begin
           query.Initialize;
           --  Receive a message, blocks until one is available
           subscriber.recv (query);
           --  Process the query
           Put_Line (query.getData);
       end;
   end loop;
end  ZMQ.examples.Subscriber;
