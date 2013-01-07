-------------------------------------------------------------------------------
--                   Copyright (c) 2011 Per Sandberg                         --
--                                                                           --
--  Permission is hereby granted, free of charge, to any person obtaining a  --
--  copy of this software and associated documentation files                 --
--  (the "Software"), to deal in the Software without restriction, including --
--  without limitation the rights to use, copy, modify, merge, publish,      --
--  distribute, sublicense, and / or sell copies of the Software, and to     --
--  permit persons to whom the Software is furnished to do so, subject to    --
--  the following conditions :                                               --
--                                                                           --
--  The above copyright notice and this permission notice shall be included  --
--  in all copies or substantial portions of the Software.                   --
--                                                                           --
--  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS  --
--  OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF               --
--  MERCHANTABILITY,                                                         --
--  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL  --
--  THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR     --
--  OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,    --
--  ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR    --
--  OTHER DEALINGS IN THE SOFTWARE.                                          --
-------------------------------------------------------------------------------

with ZMQ.Sockets;
with ZMQ.Contexts;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with ZMQ.Proxys;
procedure ZMQ.examples.Multi_Thread_Server is

   task type server_task (ctx : not null access ZMQ.Contexts.Context;
                          id  : Integer) is
   end server_task;

   task body server_task is
      msg : Ada.Strings.Unbounded.Unbounded_String;
      s   : ZMQ.Sockets.Socket;
   begin
      s.Initialize (ctx.all, Sockets.REP);
      s.Connect ("inproc://workers");
      loop
         msg := s.Recv;
         Append (msg, "<Served by thread:" & id'Img & ">");
         s.Send (msg);
      end loop;
   end server_task;

   ctx              : aliased ZMQ.Contexts.Context;

   Number_Of_Servers : constant := 10;
   servers          : array (1 .. Number_Of_Servers) of access server_task;

   workers          : ZMQ.Sockets.Socket;
   clients          : ZMQ.Sockets.Socket;


begin
   --   Create a ZMQ_REP socket to receive requests and send replies
   workers.Initialize (ctx, Sockets.XREQ);
   workers.Bind ("inproc://workers");

   --   Bind to the TCP transport and port 5555 on the 'lo' interface
   clients.Initialize (ctx, Sockets.XREP);
   workers.Bind ("tcp://lo:5555");

   for i in servers'Range loop
      servers (i) := new server_task (ctx'Access, i);
   end loop;
   ZMQ.Proxys.Proxy (workers, workers);

end ZMQ.Examples.Multi_Thread_Server;
