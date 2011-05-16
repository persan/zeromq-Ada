with ZMQ.Sockets;
with ZMQ.Contexts;
with ZMQ.Messages;
with Ada.Text_IO; use Ada.Text_IO;

procedure ZMQ.examples.Publisher is
   context          : ZMQ.Contexts.Context;
   publisher        : ZMQ.Sockets.Socket;
   sync             : ZMQ.Sockets.Socket;
   resultset_string : constant String := "OK";
   Test             : String          := "Hello";
begin
   --  Initialise 0MQ context, requesting a single application thread
   --  and a single I/O thread
   context.Initialize (1);

   --   Create a ZMQ_REP socket to receive requests and send replies
   publisher.Initialize (context, ZMQ.Sockets.PUB);
   --sync.Initialize (context, ZMQ.Sockets.PULL);

   --   Bind to the TCP transport and port 5555 on the 'lo' interface
   -- sync.Bind ("tcp://lo:5554");
   publisher.Bind ("tcp://*:5555");
   publisher.Bind ("ipc://hello");
   -- publisher.recv(0);
   for I in 1..10 loop
       declare
           query_string : constant String := "Hello =>";
           query        : ZMQ.Messages.Message;
       begin
           query.Initialize (query_string & "(" & i'Img & ");");
           publisher.send(query);
           query.Finalize;
       end;
   end loop;
end ZMQ.Examples.Publisher;
