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
with Interfaces.C;
with Ada.Real_Time;
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

   type Socket is
     new Ada.Finalization.Limited_Controlled  with private;
   type Any_Socket is access all Socket'Class;

   Null_Socket : constant Socket;

   type Socket_Flags is mod 2 ** 32;

   pragma Warnings (Off);
   function "+" (L, R : Socket_Flags) return Socket_Flags renames "or";
   pragma Warnings (On);
   No_Flags : constant Socket_Flags := 2#0000_0000_0000_0000#;
   More     : constant Socket_Flags := 2#0000_0000_0000_0001#;
   Shared   : constant Socket_Flags := 2#0000_0000_1000_0000#;

   not overriding
   procedure Initialize
     (This         : in out Socket;
      With_Context : Contexts.Context'Class;
      Kind         : Socket_Type);

   not overriding
   procedure Bind
     (This    : in out Socket;
      Address : String);

   not overriding
   procedure Bind
     (This    : in out Socket;
      Address : Ada.Strings.Unbounded.Unbounded_String);

   not overriding
   procedure UnBind
     (This    : in out Socket;
      Address : String);

   not overriding
   procedure UnBind
     (This    : in out Socket;
      Address : Ada.Strings.Unbounded.Unbounded_String);

   type Thread_Bitmap is array (0 .. 63) of Boolean;
   pragma Pack (Thread_Bitmap);

   not overriding
   procedure Set_High_Water_Mark_For_Outbound_Messages
     (This  : in out Socket;
      Messages : Positive := 1000);
   --  Sets the high water mark for outbound messages on the specified socket.
   --  The high water mark is a hard limit on the maximum number of outstanding
   --  messages ØMQ shall queue in memory for any single peer that the
   --  specified socket is communicating with.
   --  If this limit has been reached the socket enters an exceptional state
   --  and depending on the socket type, ØMQ will take appropriate action
   --  such as blocking or dropping sent messages.

   not overriding
   procedure Set_High_Water_Mark_For_Inbound_Messages
     (This  : in out Socket;
      Messages : Positive := 1000);
   --  Sets the high water mark for inbound messages on the specified socket.
   --  The high water mark is a hard limit on the maximum number of outstanding
   --  messages ØMQ shall queue in memory for any single peer that the
   --  specified socket is communicating with.
   --  If this limit has been reached the socket enters an exceptional state
   --  and depending on the socket type, ØMQ will take appropriate action
   --  such as blocking or dropping sent messages.


   not overriding
   procedure Set_IO_Thread_Affinity
     (This  : in out Socket;
      Value : Thread_Bitmap);
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
      Value : Ada.Strings.Unbounded.Unbounded_String);
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
   procedure Set_Message_Filter
     (This  : in out Socket;
      Value : String);
   not overriding
   procedure Set_Message_Filter
     (This  : in out Socket;
      Value : Ada.Strings.Unbounded.Unbounded_String);
   not overriding
   procedure Set_Message_Filter
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
     (This                : in out Socket;
      Kilobits_Per_Second : Natural := 100);
   --  Sets the maximum send or receive data rate for multicast transports
   --  such as PGM using the specified socket.

   not overriding
   procedure Set_Multicast_Recovery_Interval
     (This     : in out Socket;
      Interval : Duration);
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
      Size  : Natural);
   --  Sets the underlying kernel transmit buffer size for the socket
   --  to the specified size in bytes.
   --  A value of zero means leave the OS default unchanged.
   --  For details please refer to your operating system documentation
   --  for the SO_SNDBUF socket option.

   not overriding
   procedure Set_Kernel_Receive_Buffer_Size
     (This  : in out Socket;
      Size  : Natural);
   --  Sets the underlying kernel receive buffer size for the socket to
   --  the specified size in bytes.
   --  A value of zero means leave the OS default unchanged.
   --  For details refer to your operating system documentation for the
   --  SO_RCVBUF socket option.

   not overriding
   procedure Set_Linger_Period_For_Socket_Shutdown
     (This   : in out Socket;
      Period : Duration);
   not overriding
   procedure Set_Linger_Period_For_Socket_Shutdown
     (This   : in out Socket;
      Period : Ada.Real_Time.Time_Span);
   not overriding
   procedure Set_Linger_Period_For_Socket_Shutdown
     (This        : in out Socket;
      Miliseconds : Integer);
   --  The linger period determines how long pending messages which have yet
   --    to be sent to a peer shall linger in memory after a socket is closed.
   --    and further affects the termination of the socket's context.
   --  The following outlines the different behaviours:
   --    The default value of 'first specifies an infinite linger period.
   --      Pending messages shall not be discarded after a call to close;
   --      attempting to terminate the socket's context blocks until all
   --      pending messages have been sent to a peer.
   --    The value of 0 specifies no linger period.
   --      Pending messages shall be discarded immediately when the
   --      socket is closed.
   --      Positive values specify an upper bound for the linger period.
   --      Pending messages shall not be discarded after a call to close();
   --      attempting to terminate the socket's context blocks until either
   --      all pending messages have been sent to a peer,
   --      or the linger period expires, after which any pending
   --      messages are discarded.


   not overriding
   procedure Set_Reconnection_Interval
     (This     : in out Socket;
      Interval : Duration);
   not overriding
   procedure Set_Reconnection_Interval
     (This     : in out Socket;
      Interval : Ada.Real_Time.Time_Span);
   not overriding
   procedure Set_Reconnection_Interval
     (This        : in out Socket;
      Miliseconds : Integer);
   --  Set the initial reconnection interval for the specified socket.
   --   The reconnection interval is the period ØMQ shall wait between attempts
   --   to reconnect disconnected peers when using connection-oriented
   --   transports.
   --  A negative value means no reconnection.

   not overriding
   procedure Set_Maximum_Reconnection_Interval
     (This     : in out Socket;
      Interval : Duration := 0.0);
   not overriding
   procedure Set_Maximum_Reconnection_Interval
     (This     : in out Socket;
      Interval : Ada.Real_Time.Time_Span := Ada.Real_Time.Time_Span_Zero);
   not overriding
   procedure Set_Maximum_Reconnection_Interval
     (This        : in out Socket;
      Miliseconds : Integer := 0);
   --  Set the maximum reconnection interval for the specified socket.
   --  This is the maximum period ØMQ shall wait between attempts to reconnect.
   --  On each reconnect attempt, the previous interval shall be doubled untill
   --   Maximum_Reconnection_Interval is reached.
   --  This allows for exponential backoff strategy.
   --  Default value means no exponential backoff is performed and reconnect
   --   interval calculations are only based on Reconnection_Interval.

   not overriding
   procedure Set_Maximum_Queue_Length_Of_Outstanding_Connections
     (This        : in out Socket;
      Connections : Positive);
   --  Set the maximum length of the queue of outstanding peer connections
   --  for the specified socket;
   --  this only applies to connection-oriented transports.
   --  For details refer to your operating system documentation for the
   --  listen function.

   not overriding
   procedure Set_Maximum_Acceptable_Inbound_Message_Size
     (This  : in out Socket;
      Size  : Integer);
   --  Limits the size of the inbound message.
   --  If a peer sends a message larger than SIZE it is disconnected.
   --  A negative means no limit.

   not overriding
   procedure Set_Maximum_Network_Hops_For_Multicast_Packets
     (This      : in out Socket;
      Max_Hops  : Positive := 1);
   --  Sets the time-to-live field in every multicast packet sent
   --  from this socket. The default is 1 which means that the
   --  multicast packets don't leave the local network.

   not overriding
   procedure Set_Recieve_Time_Out
     (This      : in out Socket;
      Time      : Duration);
   not overriding
   procedure Set_Recieve_Time_Out
     (This      : in out Socket;
      Time      : Ada.Real_Time.Time_Span);
   not overriding
   procedure Set_Recieve_Time_Out
     (This         : in out Socket;
      Milliseconds : Integer);
   --  Sets the timeout for receive operation on the socket.
   --  If the value is 0, recv will fail immediately,
   --  with a EAGAIN error if there is no message to receive.
   --  If the value is Negative, it will block until a message is available.
   --  For all other values, it will wait for a message for that amount of time
   --   before Failing with an EAGAIN error.

   not overriding
   procedure Set_Send_Time_Out
     (This      : in out Socket;
      Time      : Duration);
   not overriding
   procedure Set_Send_Time_Out
     (This      : in out Socket;
      Time      : Ada.Real_Time.Time_Span);
   not overriding
   procedure Set_Send_Time_Out
     (This         : in out Socket;
      Milliseconds : Integer);
   --  Sets the timeout for send operation on the socket.
   --  If the value is zero, send will fail immediately, with a EAGAIN error
   --  if the message cannot be sent.
   --  If the value is negative, it will block until the message is sent.
   --  For all other values, it will try to send the message for that amount
   --  of time before failing with an EAGAIN error.

   not overriding
   procedure Use_IPv4_Sockets_Only
     (This         : in out Socket;
      Value        : Boolean);
   --  Sets the underlying native socket type.
   --  If set to True will use IPv4 sockets, while the value of False
   --  will use IPv6 sockets.
   --  An IPv6 socket lets applications connect to and accept connections
   --  from both IPv4 and IPv6 hosts.

   not overriding
   procedure Accept_Messages_Only_When_Connections_Are_Made
     (This         : in out Socket;
      Value        : Boolean);
   --  If set , will delay the attachment of a pipe on connect until
   --  the underlying connection has completed.
   --  This will cause the socket to block if there are no other connections,
   --  but will prevent queues from filling on pipes awaiting connection.



   not overriding
   procedure Set_Accept_Only_Routable_Messages_On_ROUTER_Sockets
     (This         : in out Socket;
      Value        : Boolean);
   --  Sets the ROUTER socket behavior when an unroutable message is
   --  encountered. A value of False is the default and discards the
   --  message silently when it cannot be routed.
   --  A value of True Raises EHOSTUNREACH  if the message cannot be routed.


   not overriding
   procedure Provide_All_Subscription_Messages_On_XPUB_Sockets
     (This         : in out Socket;
      Value        : Boolean);
   --  Sets the XPUB socket behavior on new subscriptions and unsubscriptions.
   --  A value of False is the default and passes only new
   --  subscription messages to upstream.
   --  A value of True passes all subscription messages upstream.



   type SO_KEEPALIVE_Type is (OS_Default, Disable, Enable);
   not overriding
   procedure Override_SO_KEEPALIVE_Socket_Option
     (This : in out Socket;
      Value : SO_KEEPALIVE_Type);
   --  Override TCP_KEEPCNT(or TCP_KEEPALIVE on some OS) socket option
   --  (where supported by OS).

   not overriding
   procedure Override_TCP_KEEPCNT_IDLE_Socket_Option
     (This : in out Socket;
      Value : Integer := -1);
   --  Override TCP_KEEPCNT socket option(where supported by OS).
   --  The default value of -1 means to skip any overrides and leave it
   --  to OS default.

   not overriding
   procedure Override_TCP_KEEPCNT_CNT_Socket_Option
     (This : in out Socket;
      Value : Integer := -1);
   --  Override TCP_KEEPCNT(or TCP_KEEPALIVE on some OS)
   --  socket option(where supported by OS).
   --  The default value of -1 means to skip any overrides and leave it to
   --  OS default.

   not overriding
   procedure Override_TCP_KEEPINTVL_socket_option
     (This : in out Socket;
      Value : Integer := -1);
   --  Override TCP_KEEPINTVL socket option(where supported by OS).
   --  The default value of -1 means to skip any overrides and leave it to
   --  OS default.

   not overriding
   procedure  Assign_Filters_To_Allow_New_TCP_Connections
     (This : in out Socket;
      Filter : String);
   not overriding
   procedure  Assign_Filters_To_Allow_New_TCP_Connections
     (This : in out Socket;
      Filter : Ada.Strings.Unbounded.Unbounded_String);
   not overriding
   procedure  Assign_Filters_To_Allow_New_TCP_Connections
     (This : in out Socket;
      Filter : Ada.Streams.Stream_Element_Array);
   --  Assign arbitrary number of filters that will be applied for
   --  each new TCP transport connection on a listening socket.
   --  If no filters applied, then TCP transport allows connections from
   --  any ip.
   --  If at least one filter is applied then new connection source ip
   --  should be matched. To clear all filters call
   --  Assign_Filters_To_Allow_New_TCP_Connections(socket, "").
   --  Filter is a null-terminated string with ipv6 or ipv4 CIDR.


   --=======================================================================
   --=======================================================================

   not overriding
   function Retrieve_Socket_Type (This : Socket) return Socket_Type;
   --  Retrieve the socket type for the specified socket.
   --  The socket type is specified at socket creation time and
   --  cannot be modified afterwards.

   not overriding
   function More_Message_Parts_To_Follow (This : Socket) return Boolean;
   --  returns True if the message part last received from the socket was
   --  a data part with more parts to follow.


   not overriding
   function Get_High_Water_Mark_For_Outbound_Messages
     (This : Socket) return Natural;
   --  Returns the high water mark for outbound messages on the
   --  specified socket.

   not overriding
   function Get_High_Water_Mark_For_Inbound_Messages
     (This : Socket) return Natural;
   --  Return the high water mark for inbound messages on the specified socket.

   not overriding
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

   not overriding
   function Get_Socket_Identity
     (This : Socket)
      return String;
   not overriding
   function Get_Socket_Identity
     (This : Socket)
      return Ada.Strings.Unbounded.Unbounded_String;
   not overriding
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

   not overriding
   function Get_Multicast_Data_Rate (This : Socket) return Natural;
   --  Returns the maximum send or receive data rate for multicast transports
   --  using the specified socket.

   not overriding
   function Get_Multicast_Recovery_Interval (This : Socket) return Duration;
   --  Retrieves the recovery interval for multicast transports using the
   --  specified socket.
   --  The recovery interval determines the maximum time in seconds that
   --  a receiver can be absent from a multicast group before unrecoverable
   --  data loss will occur.

   not overriding
   function Get_Kernel_Transmit_Buffer_Size (This : Socket) return Natural;
   --  Returns the underlying kernel transmit buffer size for the
   --  specified socket.
   --  A value of zero means that the OS default is in effect.
   --  For details refer to your operating system documentation for
   --   the SO_SNDBUF socket option.

   not overriding
   function Get_Kernel_Receive_Buffer_Size (This : Socket) return Natural;
   --  Returns the underlying kernel receive buffer size for the
   --  specified socket.
   --  A value of zero means that the OS default is in effect.
   --  For details refer to your operating system documentation
   --  for the SO_RCVBUF socket option

   not overriding
   function Get_Linger_Period_For_Socket_Shutdown
     (This : Socket) return Duration;
   not overriding
   function Get_Linger_Period_For_Socket_Shutdown
     (This : Socket) return Ada.Real_Time.Time_Span;
   not overriding
   function Get_Linger_Period_For_Socket_Shutdown   -- Millisecond
     (This : Socket) return Natural;
   --  Retrieves the linger period for the specified socket.


   not overriding
   function Get_Reconnection_Interval
     (This : Socket) return Duration;
   not overriding
   function Get_Reconnection_Interval
     (This : Socket) return Ada.Real_Time.Time_Span;
   not overriding
   function Get_Reconnection_Interval  -- Millisecond
     (This : Socket) return Natural;
   --  Retrieves the initial reconnection interval for the specified socket.

   not overriding
   function Get_Maximum_Reconnection_Interval
     (This : Socket) return Duration;
   not overriding
   function Get_Maximum_Reconnection_Interval
     (This : Socket) return Ada.Real_Time.Time_Span;
   not overriding
   function Get_Maximum_Reconnection_Interval  -- Millisecond
     (This : Socket) return Natural;
   --  Retrieves the maximum reconnection interval for the specified socket.

   not overriding
   function Get_Maximum_Length_Of_The_Queue_Of_Outstanding_Connections
     (This : Socket) return Natural;
   --  Retrieve the maximum length of the queue of outstanding peer connections
   --  for the specified socket;
   --    this only applies to connection-oriented transports.

   not overriding
   function Get_Maximum_Acceptable_Inbound_Message_Size
     (This : Socket) return Integer;
   --  Retrieves limit for the inbound messages.

   not overriding
   function Get_Maximum_Network_Hops_For_Multicast_Packets
     (This : Socket) return Positive;
   --  Retrieves time-to-live used for outbound multicast packets.

   not overriding
   function Get_Recieve_Timeout
     (This : Socket) return Duration;
   not overriding
   function Get_Recieve_Timeout
     (This : Socket) return Ada.Real_Time.Time_Span;
   not overriding
   function Get_Recieve_Timeout  -- Millisecond
     (This : Socket) return Integer;
   --  Retrieves the timeout for recv operation on the socket.

   not overriding
   function Get_Send_Timeout
     (This : Socket) return Duration;
   not overriding
   function Get_Send_Timeout
     (This : Socket) return Ada.Real_Time.Time_Span;
   not overriding
   function Get_Send_Timeout  -- Millisecond
     (This : Socket) return Integer;
   --  Retrieves the timeout for send operation on the socket.

   not overriding
   function Get_IPv4_only_socket_override
     (This : Socket) return Boolean;
   --   Retrives the underlying native socket type.

   not overriding
   function Get_Attach_On_Connect
     (This : Socket) return Boolean;
   --  Retrieves the state of the attach on connect value.

   not overriding
   function Get_File_Descriptor
     (This : Socket) return GNAT.OS_Lib.File_Descriptor;
   --  Retrieves the file descriptor associated with the specified socket.
   --  The returned file descriptor can be used to integrate the socket
   --  into an existing event loop;
   --  the ØMQ library shall signal any pending events on the socket
   --  in an edge-triggered fashion by making the file descriptor become
   --  ready for reading..

   not overriding
   function Get_Last_Endpoint_Set
     (This : Socket) return String;
   not overriding
   function Get_Last_Endpoint_Set
     (This : Socket) return Ada.Strings.Unbounded.Unbounded_String;
   --  Retrieves the last endpoint bound for TCP and IPC transports.
   --  The returned value will be a string in the form of a ZMQ DSN.
   --  Note that if the TCP host is INADDR_ANY, indicated by a *,
   --  then the returned address will be 0.0.0.0 (for IPv4).

   not overriding
   function Get_SO_KEEPALIVE_Socket_Option
     (This : in Socket) return SO_KEEPALIVE_Type;

   not overriding
   function Get_TCP_KEEPCNT_IDLE_socket_option
     (This : in Socket) return Integer;

   not overriding
   function Get_TCP_KEEPCNT_CNT_socket_option
     (This : in Socket) return Integer;

   not overriding
   function Get_TCP_KEEPINTVL_Socket_Option
     (This : in Socket) return Integer;

   not overriding
   procedure Connect
     (This    : in out Socket;
      Address : String);

   not overriding
   procedure Connect
     (This    : in out Socket;
      Address : Ada.Strings.Unbounded.Unbounded_String);

   not overriding
   procedure DisConnect
     (This    : in out Socket;
      Address : String);

   not overriding
   procedure DisConnect
     (This    : in out Socket;
      Address : Ada.Strings.Unbounded.Unbounded_String);


   not overriding
   procedure Send
     (This  : in out Socket;
      Msg   : String;
      Flags : Socket_Flags := No_Flags);

   not overriding
   procedure Send
     (This  : in out Socket;
      Msg   : Ada.Strings.Unbounded.Unbounded_String;
      Flags : Socket_Flags := No_Flags);

   not overriding
   procedure Send
     (This  : in out Socket;
      Msg   : Ada.Streams.Stream_Element_Array;
      Flags : Socket_Flags := No_Flags);

   not overriding
   procedure Send
     (This     : in out Socket;
      Msg      : ZMQ.Messages.Message'Class;
      Flags    : Socket_Flags := No_Flags);

   not overriding
   procedure Send
     (This        : in out Socket;
      Msg_Address : System.Address;
      Msg_Length  : Natural;
      Flags       : Socket_Flags := No_Flags);
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
      pragma Compile_Time_Error
        (Element'Has_Access_Values, "No access values allowed in Element");
   procedure Send_Generic
     (This  : in out Socket;
      Msg   : Element;
      Flags : Socket_Flags := No_Flags);

   --  generic
   --     type Element is private;
   --     with procedure Write (S : access Ada.Streams.Root_Stream_Type'Class;
   --                           Data : Element);
   --  procedure Send_Indefinite_Generic
   --    (This  : in out Socket;
   --     Msg   : Element;
   --     Flags : Socket_Flags := No_Flags);
   --  not overriding
   --  procedure flush (This    : in out Socket);


   not overriding
   procedure Recv
     (This  : in Socket;
      Msg   : out Ada.Strings.Unbounded.Unbounded_String;
      Flags : Socket_Flags := No_Flags);

   not overriding
   function Recv
     (This       : in Socket;
      Max_Length : Natural := 1024;
      Flags      : Socket_Flags := No_Flags)
      return  String;

   not overriding
   function Recv
     (This  : in Socket;
      Flags : Socket_Flags := No_Flags)
      return  Ada.Strings.Unbounded.Unbounded_String;

   not overriding
   procedure Recv
     (This    : in Socket;
      Msg     : in out Messages.Message'Class;
      Flags   : Socket_Flags := No_Flags);

   not overriding
   procedure Recv
     (This  : in Socket;
      Flags : Socket_Flags := No_Flags);

   not overriding
   procedure Close (This : in out Socket) renames Finalize;


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
     (This     : Socket_Monitor;
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
   type Mask_Type is mod 2 ** 32;
   procedure Set_Monitor
     (This    : Socket;
      Address : String;
      Mask    : Mask_Type);


   function Get_Impl (This : in Socket) return System.Address;

   --
   --  Low level setopt getopt operations.
   --

   type Socket_Opt is (ZMQ_TYPE,
                       AFFINITY,
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
                       ROUTER_MANDATORY,
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


private
   type Socket
     is new Ada.Finalization.Limited_Controlled with record
      C : System.Address := System.Null_Address;
   end record;


   overriding
   procedure Finalize (This : in out Socket);


   MAX_OPTION_SIZE : constant := 256;
   Null_Socket     : constant Socket :=
                       (Ada.Finalization.Limited_Controlled with
                                            C => System.Null_Address);

end ZMQ.Sockets;
