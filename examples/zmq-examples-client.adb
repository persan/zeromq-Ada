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

procedure ZMQ.examples.Client is
   ctx              : ZMQ.Contexts.Context;
   s                : ZMQ.Sockets.Socket;
begin
   --  Initialise 0MQ context, requesting a single application thread
   --  and a single I/O thread
   ctx.Set_number_of_IO_threads (1);

   --   Create a ZMQ_REP socket to receive requests and send replies
   s.Initialize (ctx, Sockets.REQ);

   --   Bind to the TCP transport and port 5555 on the 'lo' interface
   s.Connect ("tcp://localhost:5555");
   for i in  1 .. 10 loop
      declare
         query_string : constant String := "SELECT * FROM mytable";
         query        : ZMQ.Messages.Message;
      begin
         query.Initialize (query_string & "(" & i'Img & ");");
         s.Send (query);
         query.Finalize;
      end;

      declare
         resultset        : ZMQ.Messages.Message;
      begin
         resultset.Initialize;
         s.Recv (resultset);
         Put_Line ('"' & resultset.GetData & '"');
      end;
   end loop;
end ZMQ.Examples.Client;
