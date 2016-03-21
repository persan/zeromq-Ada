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
procedure ZMQ.Examples.Multi_Thread_Server is

   task type Server_Task (Ctx : not null access ZMQ.Contexts.Context;
                          Id  : Integer) is
   end Server_Task;

   task body Server_Task is
      Msg : Ada.Strings.Unbounded.Unbounded_String;
      S   : ZMQ.Sockets.Socket;
   begin
      S.Initialize (Ctx.all, Sockets.REP);
      S.Connect ("inproc://workers");
      loop
         Msg := S.Recv;
         Append (Msg, "<Served by thread:" & Id'Img & ">");
         S.Send (Msg);
      end loop;
   end Server_Task;

   Ctx              : aliased ZMQ.Contexts.Context;

   Number_Of_Servers : constant := 10;
   Servers           : array (1 .. Number_Of_Servers) of access Server_Task;

   Workers          : aliased ZMQ.Sockets.Socket;
   Clients          : aliased ZMQ.Sockets.Socket;

begin
   --  Initialise 0MQ context, requesting a single application thread
   --  and a single I/O thread
   Ctx.Set_Number_Of_IO_Threads (Servers'Length + 1);

   --   Create a ZMQ_REP socket to receive requests and send replies
   Workers.Initialize (Ctx, Sockets.XREQ);
   Workers.Bind ("inproc://workers");

   --   Bind to the TCP transport and port 5555 on the 'lo' interface
   Clients.Initialize (Ctx, Sockets.XREP);
   Workers.Bind ("tcp://lo:5555");

   for I in Servers'Range loop
      Servers (I) := new Server_Task (Ctx'Access, I);
   end loop;
   ZMQ.Proxys.Proxy (Frontend => Workers'Access, Backend => Clients'Access);

end ZMQ.Examples.Multi_Thread_Server;
