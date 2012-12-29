-------------------------------------------------------------------------------
--                                                                           --
--                             0MQ Ada-binding                               --
--                                                                           --
--                           Z M Q . S O C K E T S                           --
--                                                                           --
--                                  S p e c                                  --
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

with Ada.Streams;
with Ada.Finalization;
with Ada.Strings.Unbounded;
with ZMQ.Contexts;
with ZMQ.Messages;
with System;
with GNAT.OS_Lib;
private with Interfaces.C;
package ZMQ.Sockets is

   type Socket_Type is
     (PAIR,
      PUB,
      SUB,
      REQ,
      REP,
      XREQ,
      XREP,
      PULL,
      PUSH,
      XPUB,
      XSUB);

   type Socket
     (With_Context : Contexts.Any_Context;
      Kind         : Socket_Type) is
     new Ada.Finalization.Limited_Controlled  with private;




   type Socket_Flags is mod 2 ** 32;

   pragma Warnings (Off);
   function "+" (L, R : Socket_Flags) return Socket_Flags renames "or";
   pragma Warnings (On);
   No_Flags : constant Socket_Flags := 2#0000_0000_0000_0000#;
   More     : constant Socket_Flags := 2#0000_0000_0000_0001#;
   Shared   : constant Socket_Flags := 2#0000_0000_1000_0000#;


   not overriding
   procedure Bind
     (This    : in out Socket;
      Address : String);

   not overriding
   procedure Bind
     (This    : in out Socket;
      Address : Ada.Strings.Unbounded.Unbounded_String);


   not overriding
   procedure Set_IO_Thread_Affinity
     (This  : in out Socket;
      Value : Natural);
   --  Sets the I/O thread affinity for newly created connections on the
   --  specified socket.
   --  Affinity determines which threads from the 0MQ I/O thread pool

   --  created connections.
   --  A value of zero specifies no affinity, meaning that work shall be
   --  distributed fairly among all 0MQ I/O threads in the thread pool.
   --  For non-zero values,
   --  the lowest bit corresponds to thread 1, second lowest bit to thread 2
   --  and so on.
   --  For example, a value of 3 specifies that subsequent connections on
   --  socket shall behandled exclusively by I/O threads 1 and 2.
   --  See also zmq_init(3) for details on allocating the number
   --  of I/O threads for a specific context.

   not overriding
   procedure Set_Socket_Identity
     (This  : in out Socket;
      Value : String);
   not overriding
   procedure Set_Socket_Identity
     (This  : in out Socket;
      Value : Ada.Streams.Stream_Element_Array);
   --  Sets the identity of the specified socket.
   --  Socket identity determines if existing 0MQ infastructure
   --  (message queues, forwarding devices) shall be identified with a specific
   --  application and persist across multiple runs of the application.
   --  If the socket has no identity, each run of an application is completely
   --  separate from other runs. However, with identity set the socket shall
   --  re-use any existing 0MQ infrastructure configured by the
   --  previous run(s).
   --  Thus the application may receive messages that were sent in the
   --  meantime, message queue limits shall be shared with previous run(s)
   --  and so on.
   --  Identity should be at least one byte and at most 255 bytes long.
   --  Identities starting with binary zero are reserved for use
   --  by 0MQ infrastructure.

   not overriding
   procedure Establish_Message_Filter
     (This  : in out Socket;
      Value : String);
   not overriding
   procedure Establish_Message_Filter
     (This  : in out Socket;
      Value : Ada.Strings.Unbounded.Unbounded_String);
   not overriding
   procedure Establish_Message_Filter
     (This  : in out Socket;
      Value : Ada.Streams.Stream_Element_Array);
   --  Establishes a new message filter on a SUB socket.
   --  Newly created SUB sockets filters out all incoming messages,
   --  therefore you should call this option to establish an initial
   --  message filter.
   --  An empty option_value of length zero shall subscribe to all
   --  incoming messages.
   --  A non-empty option_value shall subscribe to all messages beginning
   --  with the specified prefix.
   --  Mutiple filters may be attached to a single SUB socket,
   --  in which case a message shall be accepted
   --  if it matches at least one filter.

   not overriding
   procedure Remove_Message_Filter
     (This  : in out Socket;
      Value : String);
   not overriding
   procedure Remove_Message_Filter
     (This  : in out Socket;
      Value : Ada.Strings.Unbounded.Unbounded_String);
   not overriding
   procedure Remove_Message_Filter
     (This  : in out Socket;
      Value : Ada.Streams.Stream_Element_Array);
   --  Remove an existing message filter on a SUB socket.
   --  The filter specified must match an existing filter previously
   --  established with "Establish_message_filter".
   --  If the socket has several instances of the same filter attached the
   --  Remove_message_filter removes only one instance,
   --  leaving the rest in place and functional.

   not overriding
   procedure Set_Multicast_Data_Rate
     (This  : in out Socket;
      Value : Natural);
   --  Sets the maximum send or receive data rate for multicast transports
   --  such as PGM using the specified socket.

   not overriding
   procedure Set_Multicast_Recovery_Interval
     (This  : in out Socket;
      Value : Duration);
   --  Sets the recovery interval in seconds for multicast transports using
   --  the specified socket.
   --  The recovery interval determines the maximum time in seconds that a
   --  receiver can be absent from a multicast group before unrecoverable
   --  data loss will occur.
   --  Caution:
   --   Excersize care when setting large recovery intervals as the data needed
   --   for recovery will be held in memory.
   --     For example, a 1 minute recovery interval at a data rate of
   --     1Gbps requires a 7GB in-memory buffer.


   not overriding
   procedure Set_Kernel_Transmit_Buffer_Size
     (This  : in out Socket;
      Value : Natural);
   --  Sets the underlying kernel transmit buffer size for the socket
   --  to the specified size in bytes.
   --  A value of zero means leave the OS default unchanged.
   --  For details please refer to your operating system documentation
   --  for the SO_SNDBUF socket option.

   not overriding
   procedure Set_Kernel_Receive_Buffer_Size
     (This  : in out Socket;
      Value : Natural);
   --  Sets the underlying kernel receive buffer size for the socket to
   --  the specified size in bytes.
   --  A value of zero means leave the OS default unchanged.
   --  For details refer to your operating system documentation for the
   --  SO_RCVBUF socket option.






   not overriding
   function More_Message_Parts_To_Follow (This : Socket) return Boolean;
   --  Returns True if the multi-part message currently being read from the
   --  specified socket has more message parts to follow.
   --  If there are no message parts to follow or if the message currently
   --  being read is not a multi-part message a value of True will be returned.
   --  Otherwise, False will be returned.



   type Thread_Bitmap is array (0 .. 63) of Boolean;
   pragma Pack (Thread_Bitmap);
   function Get_IO_Thread_Affinity (This : Socket) return Thread_Bitmap;
   --  Returns the I/O thread affinity for newly created connections
   --  on the specified socket.
   --  Affinity determines which threads from the ZMQ I/O thread pool

   --  created connections.
   --  A value of zero specifies no affinity, meaning that work shall be
   --  distributed fairly among all ZMQ I/O threads in the thread pool.
   --  For non-zero values, the lowest bit corresponds to thread 1,
   --  second lowest bit to thread 2 and so on. For example,
   --  a value of 3 specifies that subsequent connections on socket shall be
   --  handled exclusively by I/O threads 1 and 2.

   function Get_Socket_Identity
     (This : Socket)
      return Ada.Streams.Stream_Element_Array;
   --  Returns the identity of the specified socket.
   --  Socket identity determines if existing ZMQ infastructure
   --  (message queues, forwarding devices) shall be identified with a specific
   --  application and persist across multiple runs of the application.
   --  If the socket has no identity, each run of an application is completely
   --  separate from other runs. However, with identity set the socket shall
   --  re-use any existing ZMQ infrastructure configured by the
   --  previous run(s).
   --  Thus the application may receive messages that were sent
   --  in the meantime,
   --  message queue limits shall be shared with previous run(s) and so on.
   --  Identity can be at least one byte and at most 255 bytes long.
   --  Identities starting with binary zero are reserved for use by the
   --   ZMQ infrastructure.

   function Get_Multicast_Data_Rate (This : Socket) return Natural;
   --  Returns the maximum send or receive data rate for multicast transports
   --  using the specified socket.

   function Get_Multicast_Recovery_Interval (This : Socket) return Duration;
   --  Retrieves the recovery interval for multicast transports using the
   --  specified socket.
   --  The recovery interval determines the maximum time in seconds that
   --  a receiver can be absent from a multicast group before unrecoverable
   --  data loss will occur.

   function Get_Kernel_Transmit_Buffer_Size (This : Socket) return Natural;
   --  Returns the underlying kernel transmit buffer size for the
   --  specified socket.
   --  A value of zero means that the OS default is in effect.
   --  For details refer to your operating system documentation for
   --   the SO_SNDBUF socket option.

   function Get_Kernel_Receive_Buffer_Size (This : Socket) return Natural;
   --  Returns the underlying kernel receive buffer size for the
   --  specified socket.
   --  A value of zero means that the OS default is in effect.
   --  For details refer to your operating system documentation
   --  for the SO_RCVBUF socket option



   not overriding procedure Connect
     (This    : in out Socket;
      Address : String);

   procedure Connect
     (This    : in out Socket;
      Address : Ada.Strings.Unbounded.Unbounded_String);


   not overriding procedure Send
     (This  : in out Socket;
      Msg   : String;
      Flags : Socket_Flags := No_Flags);

   not overriding procedure Send
     (This  : in out Socket;
      Msg   : Ada.Strings.Unbounded.Unbounded_String;
      Flags : Socket_Flags := No_Flags);

   not overriding procedure Send
     (This  : in out Socket;
      Msg   : Ada.Streams.Stream_Element_Array;
      Flags : Socket_Flags := No_Flags);

   not overriding procedure Send
     (This       : in out Socket;
      Msg_Addres : System.Address;
      Msg_Length : Natural;
      Flags      : Socket_Flags := No_Flags);
   --  Queues the message referenced by the msg argument to be sent to socket
   --  The flags argument is a combination of the flags defined below:
   --   NOBLOCK
   --    Specifies that the operation should be performed in non-blocking mode.
   --    If the message cannot be queued on the socket,
   --    the send function shall fail with errno set to EAGAIN.
   --   SNDMORE
   --     Specifies that the message being sent is a multi-part message,
   --     and that further message parts are to follow.
   --     Refer to the section regarding multi-part messages
   --     below for a detailed description.
   --  Note!
   --    A successful invocation of send does not indicate that the message
   --    has been transmitted to the network,
   --   only that it has been queued on the socket and 0MQ has assumed
   --   responsibility for the message.
   --  Multi-part messages
   --    A 0MQ message is composed of 1 or more message parts;
   --    each message part is an independent zmq_msg_t in its own right.
   --    0MQ ensures atomic delivery of messages;
   --    peers shall receive either all message parts of
   --    a message or none at all.
   --  The total number of message parts is unlimited.
   --
   --  An application wishing to send a multi-part message does so by
   --  specifying the SNDMORE flag to send.
   --  The presence of this flag indicates to 0MQ that the message being sent
   --  is a multi-part message and that more message parts are to follow.
   --  When the application wishes to send the final message part it does so
   --  by calling zmq without the SNDMORE flag;
   --  this indicates that no more message parts are to follow.
   --  Creates a Message and sends it over the socket.

   generic
      type Element is private;
   procedure Send_Generic
     (This  : in out Socket;
      Msg   : Element;
      Flags : Socket_Flags := No_Flags);

   --     not overriding
   --     procedure flush (This    : in out Socket);


   procedure Recv
     (This  : in Socket;
      Msg   : out Ada.Strings.Unbounded.Unbounded_String;
      Flags : Socket_Flags := No_Flags);

   not overriding function Recv
     (This  : in Socket;
      Flags : Socket_Flags := No_Flags)
      return  String;

   not overriding function Recv
     (This  : in Socket;
      Flags : Socket_Flags := No_Flags)
      return  Ada.Strings.Unbounded.Unbounded_String;

   not overriding procedure Recv
     (This    : in Socket;
      Msg     : Messages.Message'Class;
      Flags   : Socket_Flags := No_Flags);

   procedure Recv (This : in Socket; Flags : Socket_Flags := No_Flags);



   procedure Close (This : in out Socket) renames Finalize;
   --


   type Socket_Monitor is limited interface;
   type Any_Socket_Monitor is access all Socket_Monitor'Class;
   procedure Connected
     (This    : Socket_Monitor;
      Address : String;
      Fd      : GNAT.OS_Lib.File_Descriptor) is null;
   procedure Connect_Delayed
     (This    : Socket_Monitor;
      Address : String;
      Err     : Integer) is null;
   procedure Connect_Retried
     (This    : Socket_Monitor;
      Address  : String;
      Interval : Duration) is null;
   procedure Listening
     (This    : Socket_Monitor;
      Address : String;
      Fd      : GNAT.OS_Lib.File_Descriptor) is null;
   procedure Bind_Failed
     (This    : Socket_Monitor;
      Address : String;
      Err     : Integer) is null;
   procedure Accepted
     (This    : Socket_Monitor;
      Address : String;
      Fd      : GNAT.OS_Lib.File_Descriptor) is null;
   procedure Accept_Failed
     (This    : Socket_Monitor;
      Address : String;
      Err     : Integer) is null;
   procedure Closed
     (This    : Socket_Monitor;
      Address : String;
      Fd      : GNAT.OS_Lib.File_Descriptor) is null;
   procedure Close_Failed
     (This    : Socket_Monitor;
      Address : String;
      Err     : Integer) is null;
   procedure Disconnected
     (This    : Socket_Monitor;
      Address : String;
      Fd      : GNAT.OS_Lib.File_Descriptor) is null;

   procedure Set_Monitor (This : Socket;
                           Monitor : Any_Socket_Monitor);


   --  function "=" (Left, Right : in Context) return Boolean;
   function Get_Impl (This : in Socket) return System.Address;
private
   type Socket
     (With_Context : Contexts.Any_Context;
      Kind         : Socket_Type)
     is new Ada.Finalization.Limited_Controlled with record
      C : System.Address := System.Null_Address;
   end record;
   function Img (Item : Ada.Streams.Stream_Element_Array) return String;
   overriding
   procedure Initialize
     (This         : in out Socket);
   overriding
   procedure Finalize (This : in out Socket);

   type Socket_Opt is (AFFINITY,
                       IDENTITY,
                       SUBSCRIBE,
                       UNSUBSCRIBE,
                       RATE,
                       RECOVERY_IVL,
                       SNDBUF,
                       RCVBUF,
                       RCVMORE,
                       FD,
                       EVENTS,
                       GET_TYPE,
                       LINGER,
                       RECONNECT_IVL,
                       BACKLOG, ---
                       RECONNECT_IVL_MAX,
                       MAXMSGSIZE,
                       SNDHWM,
                       RCVHWM,
                       MULTICAST_HOPS,
                       RCVTIMEO,
                       SNDTIMEO,
                       IPV4ONLY,
                       LAST_ENDPOINT,
                       ROUTER_BEHAVIOR,
                       TCP_KEEPALIVE,
                       TCP_KEEPALIVE_CNT,
                       TCP_KEEPALIVE_IDLE,
                       TCP_KEEPALIVE_INTVL,
                       TCP_ACCEPT_FILTER,
                       DELAY_ATTACH_ON_CONNECT,
                       XPUB_VERBOSE);




   not overriding procedure Setsockopt
     (This   : in out Socket;
      Option : Socket_Opt;
      Value  : String);
   not overriding procedure Setsockopt
     (This   : in out Socket;
      Option : Socket_Opt;
      Value  : Boolean);
   not overriding procedure Setsockopt
     (This   : in out Socket;
      Option : Socket_Opt;
      Value  : Natural);
   not overriding procedure Setsockopt
     (This   : in out Socket;
      Option : Socket_Opt;
      Value  : Ada.Streams.Stream_Element_Array);

   not overriding procedure Setsockopt
     (This       : in out Socket;
      Option     : Socket_Opt;
      Value      : System.Address;
      Value_Size : Natural);

   --------------------------------------------------------
   --------------------------------------------------------

   function Getsockopt
     (This   : in Socket;
      Option : Socket_Opt)
      return   String;
   not overriding function Getsockopt
     (This   : in Socket;
      Option : Socket_Opt)
      return   Boolean;
   not overriding function Getsockopt
     (This   : in Socket;
      Option : Socket_Opt)
      return   Natural;
   not overriding function Getsockopt
     (This   : in Socket;
      Option : Socket_Opt)
      return   Interfaces.C.unsigned_long;

   not overriding function Getsockopt
     (This   : in Socket;
      Option : Socket_Opt)
      return   Ada.Streams.Stream_Element_Array;

   not overriding procedure Getsockopt
     (This       : in Socket;
      Option     : Socket_Opt;
      Value      : System.Address;
      Value_Size : out Natural);

   MAX_OPTION_SIZE : constant := 256;

end ZMQ.Sockets;
