with ZMQ.Low_Level;
with Interfaces.C; use Interfaces.C;
package body ZMQ.Proxys is

   -----------
   -- Proxy --
   -----------

   procedure Proxy
     (Frontend  : not null access Sockets.Socket;
      Backend   : not null access Sockets.Socket;
      Capture   : access Sockets.Socket := null)
   is
      Dummy : int;
      pragma Unreferenced (Dummy);
   begin
      Dummy := ZMQ.Low_Level.zmq_proxy
        (Frontend.Get_Impl, Backend.Get_Impl, Capture.Get_Impl);
   end Proxy;

end ZMQ.Proxys;
