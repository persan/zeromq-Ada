-------------------------------------------------------------------------------
--            Copyright (C) 2020-2030, per.s.sandberg@bahnhof.se             --
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

procedure ZMQ.Examples.Client is
   Ctx              : ZMQ.Contexts.Context;
   S                : ZMQ.Sockets.Socket;
begin
   --  Initialise 0MQ context, requesting a single application thread
   --  and a single I/O thread
   Ctx.Set_Number_Of_IO_Threads (1);

   --   Create a ZMQ_REP socket to receive requests and send replies
   S.Initialize (Ctx, Sockets.REQ);

   --   Bind to the TCP transport and port 5555 on the 'lo' interface
   S.Connect ("tcp://localhost:5555");
   for I in  1 .. 10 loop
      declare
         Query_String : constant String := "SELECT * FROM mytable";
         Query        : ZMQ.Messages.Message;
      begin
         Query.Initialize (Query_String & "(" & I'Img & ");");
         S.Send (Query);
      end;

      declare
         Resultset        : ZMQ.Messages.Message;
      begin
         Resultset.Initialize (0);
         S.Recv (Resultset);
         Put_Line ('"' & Resultset.GetData & '"');
      end;
   end loop;
end ZMQ.Examples.Client;
