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
with ZMQ.Messages;
with Ada.Text_IO; use Ada.Text_IO;

procedure ZMQ.Examples.Server is
   Ctx              : ZMQ.Contexts.Context;
   S                : ZMQ.Sockets.Socket;
   Resultset_String : constant String := "OK";
begin
   --  Initialise 0MQ context, requesting a single application thread
   --  and a single I/O thread
   Ctx.Set_Number_Of_IO_Threads (1);

   --   Create a ZMQ_REP socket to receive requests and send replies
   S.Initialize (Ctx, Sockets.REP);

   --   Bind to the TCP transport and port 5555 on the 'lo' interface
   S.Bind ("tcp://lo:5555");

   loop
      declare
         Query : ZMQ.Messages.Message;
      begin
         Query.Initialize(0);
         --  Receive a message, blocks until one is available
         S.Recv (Query);
         --  Process the query
         Put_Line (Query.GetData);
         declare
            --  Allocate a response message and fill in an example response
            Resultset : ZMQ.Messages.Message;
         begin
            Resultset.Initialize (Query.GetData & "->" & Resultset_String);
            --   Send back our canned response
            S.Send (Resultset);
         end;
      end;

   end loop;
end ZMQ.Examples.Server;
