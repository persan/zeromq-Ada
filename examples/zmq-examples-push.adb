with ZMQ.Sockets;
with ZMQ.Contexts;
with ZMQ.Messages;
with Ada.Text_IO; use Ada.Text_IO;

procedure ZMQ.examples.Push is 
    ctx              : ZMQ.Contexts.Context;
    pusher                : ZMQ.Sockets.Socket;
begin
    ctx.Initialize (1);
    pusher.Initialize (ctx, Sockets.PUSH);
    pusher.connect("tcp://127.0.0.1:5555");
    for i in  0 .. 10 loop
        declare
            query_string : constant String := "SELECT * FROM mytable";
            query        : ZMQ.Messages.Message;
        begin
            query.Initialize (query_string & "=>" & I'Img);
            pusher.Send (query);
            query.Finalize;
        end;
    end loop;
end ZMQ.examples.Push;
