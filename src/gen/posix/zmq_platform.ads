with Interfaces.C;
package ZMQ_Platform is
   pragma Preelaborate;

   --  Non-Windows uses an int to store the socket fd.
   subtype zmq_fd_t is Interfaces.C.int;
end ZMQ_Platform;
