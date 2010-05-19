private with Interfaces.C;
with ZMQ.Sockets;
package ZMQ.devices is
   type device is tagged private;
   type Device_Kind is (Streamer, Forwarder, Queue);
   procedure initialize (this : in out device;
                         Kind      : Device_Kind;
                         insocket  : ZMQ.Sockets.Socket;
                         outsocket : ZMQ.Sockets.Socket);
private
   type device is tagged record
      impl : Interfaces.C.int;
   end record;
end  ZMQ.devices;
