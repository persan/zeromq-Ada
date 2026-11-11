with Interfaces.C;
package ZMQ_Platform is
   pragma Preelaborate;

   --  Windows uses a pointer-sized SOCKET type (UINT_PTR) to store the socket fd.
   subtype zmq_fd_t is Interfaces.C.size_t;
end ZMQ_Platform;

