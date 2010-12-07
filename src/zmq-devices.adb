-------------------------------------------------------------------------------
--                                                                           --
--                             0MQ Ada-binding                               --
--                                                                           --
--                          Z M Q . D E V I C E S                            --
--                                                                           --
--                                  B o d y                                  --
--                                                                           --
--            Copyright (C) 2010-2011, per.sandberg@bredband.net             --
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
------------------------------------------------------------------------------

with ZMQ.Low_Level;
with Interfaces.C; use Interfaces.C;

package body ZMQ.devices is

   ----------------
   -- initialize --
   ----------------

   map : constant array (Device_Kind) of int :=
           (Streamer  => Low_Level.Defs.ZMQ_STREAMER,
            Forwarder => Low_Level.Defs.ZMQ_FORWARDER,
            Queue     => Low_Level.Defs.ZMQ_QUEUE);
   procedure initialize
     (this : in out device;
      Kind      : Device_Kind;
      insocket  : ZMQ.Sockets.Socket;
      outsocket : ZMQ.Sockets.Socket)
   is
   begin
      this.impl :=
        Low_Level.zmq_device (map (Kind),
                              insocket.get_impl,
                              outsocket.get_impl);
   end initialize;

end ZMQ.devices;
