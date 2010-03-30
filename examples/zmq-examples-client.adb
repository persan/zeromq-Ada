with ZMQ.Sockets;
with ZMQ.Contexts;
with ZMQ.Messages;
with Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;

procedure zmq.examples.Client is
   ctx              : ZMQ.Contexts.context;
   s                : ZMQ.Sockets.Socket;
   resultset_string : constant String := "OK";
begin
   --  Initialise 0MQ context, requesting a single application thread
   --  and a single I/O thread
   ctx.initialize (1, 1);

   --   Create a ZMQ_REP socket to receive requests and send replies
   s.initialize (ctx, Sockets.REQ);

   --   Bind to the TCP transport and port 5555 on the 'lo' interface
   s.connect ("tcp://localhost:5555");
   for i in  1 .. 5 loop
      declare
         query_string : constant String := "SELECT * FROM mytable";
         query        : ZMQ.Messages.Message;
      begin
         query.Initialize (query_string & "(" & i'Img & ");");
         s.send (query);
         query.Finalize;
      end;

      declare
         resultset        : ZMQ.Messages.Message;
      begin
         resultset.Initialize;
         s.recv (resultset);
         Put_Line ('"' & resultset.getData & '"');
         resultset.Finalize;
      end;
   end loop;
end;
