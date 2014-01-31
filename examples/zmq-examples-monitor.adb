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

with Ada.Text_IO; use Ada.Text_IO;

with GNAT.Exception_Traces;
with GNAT.Sockets;
with GNAT.Traceback.Symbolic;

with System.Address_To_Access_Conversions;

with ZMQ.Contexts;
with ZMQ.Low_Level;
with ZMQ.Messages;
with ZMQ.Sockets;

procedure ZMQ.Examples.Monitor is
   use ZMQ.Sockets;
   --  Port : constant String := "inproc://Zmq.pp";
   --  Port : constant String := "tcp://127.0.0.1:61113";
   --  Port : constant String := "tcp://lo:61112";


   S_Port      : constant String := "epgm://192.168.0.101;239.168.1.1:5555";
   C_Port      : constant String := "epgm://192.168.0.101;239.168.1.1:5555";
   Monitor_URL : constant String := "inproc://monitor.req";

   task type Server_Type (Ctx : not null access ZMQ.Contexts.Context) is
      entry Done;
      entry Start;
   end Server_Type;


   task body Server_Type is
      S                : ZMQ.Sockets.Socket;
   begin
      accept Start;
      S.Initialize (Ctx.all, Sockets.SUB);
      S.Connect (S_Port);
      S.Set_Message_Filter ("hej");
      loop
         declare
            Query : ZMQ.Messages.Message;
         begin
            S.Recv (Query);
            Put_Line (Query.GetData);
            exit when Query.GetData = END_MESSAGE;
         end;
      end loop;
      S.DisConnect (S_Port);
      accept Done;
   end Server_Type;

   task type Client_Type (Ctx : not null access ZMQ.Contexts.Context) is
      entry Start;
      entry Started;
      entry Done;
   end Client_Type;

   task body Client_Type is
      S   : Sockets.Socket;
   begin
      accept Start;
      S.Initialize (Ctx.all, Sockets.PUB);
      S.Set_Monitor (Monitor_URL, Sockets.EVENT_ALL);
      accept Started;
      S.Connect (C_Port);
      for I in 1 .. 5 loop
         S.Send ("hej" & ASCII.NUL & ":" & GNAT.Sockets.Host_Name & ":" & I'Img);
         S.Send ("hj" & ASCII.NUL & ":" & GNAT.Sockets.Host_Name & ":" & I'Img);
         delay 0.5;
      end loop;
      S.Send (END_MESSAGE);
      S.DisConnect (C_Port);
      delay 0.2;
      accept Done;
   end Client_Type;

   task type Monitor_Type (Ctx : not null access ZMQ.Contexts.Context) is
      entry Done;
      entry Start;
   end Monitor_Type;

   task body Monitor_Type is
      S   : Sockets.Socket;
      E   : aliased Low_Level.zmq_event_t;
      package Conv is new
        System.Address_To_Access_Conversions (Low_Level.Zmq_Event_T);
   begin
      accept Start;
      S.Initialize (Ctx.all, Sockets.PAIR);
      S.Connect (Monitor_URL);
      loop
         declare
            Msg : ZMQ.Messages.Message;
         begin

            S.Recv (Msg);
            E := Conv.To_Pointer (Msg.GetData).all;
            Ada.Text_IO.Put_Line ("Monitor" & E.event'Img);
            exit when Event_Type (E.event) = Sockets.EVENT_DISCONNECTED;
         end;
      end loop;
      accept Done;
   end Monitor_Type;

   Ctx_1     : aliased ZMQ.Contexts.Context;
   Ctx_2     : aliased ZMQ.Contexts.Context;
   Monitor   : Monitor_Type (Ctx_1'Access);
   Server    : Server_Type (Ctx_2'Access);
   Client    : Client_Type (Ctx_1'Access);

begin
   GNAT.Exception_Traces.Trace_On (GNAT.Exception_Traces.Every_Raise);
   GNAT.Exception_Traces.Set_Trace_Decorator
     (GNAT.Traceback.Symbolic.Symbolic_Traceback'Access);
   Ctx_1.Set_Number_Of_IO_Threads (2);

   Client.Start; delay 0.01;
   Monitor.Start; delay 0.01;
   Server.Start; delay 0.01;
   Client.Started; delay 0.01;


   Client.Done; delay 0.1;
   select
      Server.Done;
   or delay 0.2;
      Ada.Text_IO.Put_Line ("abort Server");
      abort Server;
   end select;

   select
      Monitor.Done;
   or delay 0.2;
      Ada.Text_IO.Put_Line ("abort Monitor");
      abort Monitor;
   end select;
end ZMQ.Examples.Monitor;
