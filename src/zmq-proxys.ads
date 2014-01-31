-------------------------------------------------------------------------------
--                                                                           --
--                             0MQ Ada-binding                               --
--                                                                           --
--                           Z M Q . P R O X Y S                             --
--                                                                           --
--                                  S p e c                                  --
--                                                                           --
--            Copyright (C) 2013-2020, per.s.sandberg@bahnhof.se             --
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

-------------------------------------------------------------------------------
--  Proxys are building blocks intended to serve as intermediate nodes
--  in complex messaging topologies.
--
--  Example usage
--   Shared queue
--    When the frontend is a ROUTER socket, and the backend is a DEALER socket,
--    the proxy shall act as a shared queue that collects requests from a set
--    of clients, and distributes these fairly among a set ofservices.
--    Requests shall be fair-queued from frontend connections and distributed
--    evenly across backend connections.
--    Replies shall automatically return to the client that made the original
--    request.
--   Forwarder
--    When the frontend is a XSUB socket, and the backend is a XPUB socket,
--    the proxy shall act as a message forwarder that collects messages from a
--    set of publishers and forwards these to a set of subscribers. This may be
--    used to bridge networks transports, e.g.
--    read on tcp:// and forward on pgm://.
--   Streamer
--    When the frontend is a PULL socket, and the backend is a PUSH socket,
--    the proxy shall collect tasks from a set of clients and forwards these
--    to a set of workers using the pipeline pattern
--
--  Example:
--   Create frontend and backend sockets
--    declare
--       context  : ZMQ.Contexts.Context;
--       frontend : ZMQ.Sockets.Socket;
--       backend  : ZMQ.Sockets.Socket;
--    begin
--       frontend.Initialize(context, ZMQ.Sockets.Router);
--       frontend.bind("tcp://*:5555");
--       backend.Initialize(context, ZMQ.Sockets.Dealer);
--       backend.bind ("tcp://*:5556");
--       ZMQ.Proxys.Proxy(frontend,backend);
--    end;
-------------------------------------------------------------------------------
with ZMQ.Sockets;
package ZMQ.Proxys is


   procedure Proxy
     (Frontend  : not null access Sockets.Socket;
      Backend   : not null access Sockets.Socket;
      Capture   : access Sockets.Socket := null);
   --  starts the built-in ØMQ proxy in the current application thread.
   --  The proxy connects a frontend socket to a backend socket.S
   --  Conceptually, data flows from frontend to backend.
   --  Depending on the socket types, replies may flow in the opposite
   --  direction. The direction is conceptual only;
   --  the proxy is fully symmetric and there is no technical difference
   --  between frontend and backend.
   --  Before calling Initialize you must set any socket options,
   --  and connect or bind both frontend and backend sockets.
   --  The two conventional proxy models are:
   --    The proxy runs in the current thread and returns only if/when the
   --    current context is closed.
   --  If the capture socket is not NULL, the proxy shall send all messages,
   --   received on both frontend and backend, to the capture socket.
   --   The capture socket should be a PUB, DEALER, PUSH, or PAIR socket.

end  ZMQ.Proxys;
