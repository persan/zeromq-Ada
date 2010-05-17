with ZMQ.Low_Level;
with Interfaces.C; use Interfaces.C;

package body ZMQ.devices is

   ----------------
   -- initialize --
   ----------------

   map : constant array (Device_Kind) of int :=
           (Streamer  => Low_Level.defs.ZMQ_STREAMER,
            forwarder => Low_Level.defs.ZMQ_FORWARDER,
            queue     => Low_Level.defs.ZMQ_QUEUE);
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
