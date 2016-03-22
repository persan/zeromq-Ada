-------------------------------------------------------------------------------
--                                                                           --
--                             0MQ Ada-binding                               --
--                                                                           --
--                           Z M Q . S O C K E T S                           --
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

with Ada.Streams;
with Ada.Finalization;
with Ada.Strings.Unbounded;
with ZMQ.Contexts;
with ZMQ.Messages;
with System;
with Interfaces.C;
package ZMQ.Sockets is
   use Ada.Streams;
   type Socket_Type is
     (PAIR,
      PUB,
      SUB,
      REQ,
      REP,
      DEALER,
      ROUTER,
      PULL,
      PUSH,
      XPUB,
      XSUB,
      STREAM);
   XREQ : constant Socket_Type := DEALER;
   pragma Obsolescent (XREQ, "use DEALER");
   XREP : constant Socket_Type := ROUTER;
   pragma Obsolescent (XREP, "use ROUTER");
   type Socket is
     new Ada.Finalization.Limited_Controlled  with private;
   type Any_Socket is access all Socket'Class;

   type Socket_Flags is mod 2 ** 32;

   pragma Warnings (Off);
   function "+" (L, R : Socket_Flags) return Socket_Flags renames "or";
   pragma Warnings (On);
   No_Flags : constant Socket_Flags := 2#0000_0000_0000_0000#;
   More     : constant Socket_Flags := 2#0000_0000_0000_0001#;
   Shared   : constant Socket_Flags := 2#0000_0000_1000_0000#;

   --  ========================================================================
   --  Socket setup
   --  ========================================================================
   not overriding
   procedure Initialize (This         : in out Socket;
                         With_Context : Contexts.Context;
                         Kind         : Socket_Type);
   not overriding
   procedure Bind (This    : in out Socket;
                   Address : String);

   not overriding
   procedure Bind (This    : in out Socket;
                   Address : Ada.Strings.Unbounded.Unbounded_String);


   not overriding
   procedure Unbind (This    : in out Socket;
                     Address : String);

   not overriding
   procedure Unbind (This    : in out Socket;
                     Address : Ada.Strings.Unbounded.Unbounded_String);

   not overriding function Retrieve_Socket_Type
     (This : in Socket)
      return Socket_Type;
   --  ========================================================================
   --  Socket control
   --  ========================================================================

   not overriding
   function Get_High_Water_Mark_For_Inbound_Messages
     (This : Socket) return Natural;

   not overriding
   procedure  Set_High_Water_Mark_For_Outbound_Messages
     (This     : in out Socket;
      Messages : Natural := 1_000);
   --  Sets the high water mark for outbound messages on the specified socket.
   --  The high water mark is a hard limit on the maximum number of outstanding
   --  messages ØMQ shall queue in memory for any single peer that the
   --  specified socket is communicating with.
   --  If this limit has been reached the socket shall enter an
   --  exceptional state and depending on the socket type,
   --  ØMQ shall take appropriate action such as blocking or dropping
   --  sent messages.
   --  Refer to the individual socket descriptions for details on the exact
   --  action taken for each socket type.
   --  The value of zero means "no limit".
   --  ØMQ does not guarantee that the socket will accept as many  messages,
   --  and the actual limit may be as much as 60-70% lower depending on the
   --  flow of messages on the socket.
   ----------------------------------------------------------------------------



   not overriding
   function Get_High_Water_Mark_For_Outbound_Messages
     (This : Socket) return Natural;

   not overriding
   procedure  Set_High_Water_Mark_For_Inbound_Messages
     (This     : in out Socket;
      Messages : Natural := 1_000);
   --  Sets the high water mark for inbound messages on the specified socket.
   --  The high water mark is a hard limit on the maximum number of
   --  outstanding messages ØMQ shall queue in memory for any single peer
   --  that the specified socket is communicating with.
   --  If this limit has been reached the socket shall enter an exceptional
   --  state and depending on the socket type,
   --  ØMQ shall take appropriate action such as blocking or dropping sent
   --  messages.
   --  Refer to the individual socket descriptions for details on the exact
   --  action taken for each socket type.
   ----------------------------------------------------------------------------



   not overriding
   procedure  Set_Disk_Offload_Size
     (This       : in out Socket;
      Value      : Natural);
   --  Sets the disk offload (swap) size < for the specified socket.
   --  A socket which has ZMQ_SWAP set to a non - zero value may exceed
   --  in this case outstanding messages shall be offloaded to storage on
   --  disk rather than held in memory.
   --  The value defines the maximum size of the swap space in bytes
   ----------------------------------------------------------------------------


   type Thread_Bitmap is array (0 .. 63) of Boolean;

   pragma Pack (Thread_Bitmap);
   function Get_IO_Thread_Affinity
     (This : Socket) return Thread_Bitmap;
   not overriding
   procedure  Set_IO_Thread_Affinity
     (This     : in out Socket;
      Threads  : Thread_Bitmap);
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
   ----------------------------------------------------------------------------

   not overriding
   procedure  Set_Socket_Identity
     (This       : in out Socket;
      Value      : String) with
     Pre => Value'Length < 256 and then
     Value'Length > 0 and then
     Value (Value'First) /= ASCII.NUL;
   not overriding
   procedure  Set_Socket_Identity
     (This       : in out Socket;
      Value      : Ada.Streams.Stream_Element_Array) with
     Pre => Value'Length < 256 and then
     Value'Length > 0 and then
     Value (Value'First) /= 0;
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
   ----------------------------------------------------------------------------

   not overriding
   procedure  Establish_Message_Filter (This       : in out Socket;
                                        Value      : String) with
     Pre => This.Retrieve_Socket_Type = SUB;
   not overriding
   procedure  Establish_Message_Filter
     (This       : in out Socket;
      Value      : Ada.Strings.Unbounded.Unbounded_String) with
     Pre => This.Retrieve_Socket_Type = SUB;
   procedure  Establish_Message_Filter
     (This       : in out Socket;
      Value      : Ada.Streams.Stream_Element_Array)  with
     Pre => This.Retrieve_Socket_Type = SUB;
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
   ----------------------------------------------------------------------------

   not overriding
   procedure  Remove_Message_Filter (This       : in out Socket;
                                     Value      : String)  with
     Pre => This.Retrieve_Socket_Type = SUB;

   not overriding
   procedure  Remove_Message_Filter
     (This       : in out Socket;
      Value      : Ada.Strings.Unbounded.Unbounded_String)  with
     Pre => This.Retrieve_Socket_Type = SUB;

   not overriding
   procedure  Remove_Message_Filter
     (This       : in out Socket;
      Value      : Ada.Streams.Stream_Element_Array)  with
     Pre => This.Retrieve_Socket_Type = SUB;
   --  Remove an existing message filter on a SUB socket.
   --  The filter specified must match an existing filter previously
   --  established with "Establish_message_filter".
   --  If the socket has several instances of the same filter attached the
   --  Remove_message_filter removes only one instance,
   --  leaving the rest in place and functional.
   ----------------------------------------------------------------------------


   function Get_Multicast_Data_Rate
     (This : Socket) return Natural;
   not overriding
   procedure  Set_Multicast_Data_Rate
     (This                     : in out Socket;
      Kilobits_Per_Second      : Natural := 100);
   --  Sets the maximum send or receive data rate for multicast transports
   --  such as PGM using the specified socket.


   not overriding
   procedure  Set_Multicast_Recovery_Interval
     (This : in out Socket;
      Time : Duration := 10.0) with
     Inline => True;
   not overriding
   function Get_Multicast_Recovery_Interval
     (This : Socket) return Duration;
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
   procedure  Set_Multicast_Loopback
     (This   : in out Socket;
      Enable : Boolean);
   --  Controls whether data sent via multicast transports using
   --  the specified socket can also be received by the sending host
   --  via loopback.
   --  A value of False disables the loopback functionality,
   --  while the default value of True enables the loopback functionality.
   --  Leaving multicast loopback enabled when it is not required can have
   --  a negative impact on performance.
   --  Where possible, disable multicast_loopback
   --  in production environments.

   not overriding
   function Get_Kernel_Transmit_Buffer_Size
     (This : Socket) return Natural;
   not overriding
   procedure  Set_Kernel_Transmit_Buffer_Size
     (This  : in out Socket;
      Bytes : Natural);
   --  Sets the underlying kernel transmit buffer size for the socket
   --  to the specified size in bytes.
   --  A value of zero means leave the OS default unchanged.
   --  For details please refer to your operating system documentation
   --  for the SO_SNDBUF socket option.


   not overriding
   function Get_Kernel_Receive_Buffer_Size
     (This : Socket) return Natural;
   not overriding
   procedure  Set_Kernel_Receive_Buffer_Size
     (This  : in out Socket;
      Bytes : Natural);
   --  Sets the underlying kernel receive buffer size for the socket to
   --  the specified size in bytes.
   --  A value of zero means leave the OS default unchanged.
   --  For details refer to your operating system documentation for the
   --  SO_RCVBUF socket option.


   not overriding
   function Get_Linger_Period_For_Socket_Shutdown
     (This : Socket) return Duration;
   not overriding
   procedure  Set_Linger_Period_For_Socket_Shutdown
     (This   : in out Socket;
      Period : Duration := Duration'Last);
   --  Sets the linger period for the specified socket.
   --  The linger period determines how long pending messages which have yet
   --  to be sent to a peer shall linger in memory after a socket is closed,
   --  and further affects the termination of the socket's context.
   --  The following outlines the different behaviours:
   --   The default value of Duration'last specifies an infinite linger period.
   --      Pending messages shall not be discarded after a call to close;
   --       attempting to terminate the socket's context shall block
   --       until all pending messages have been sent to a peer.
   --  The value of 0.0 specifies no linger period.
   --    Pending messages will be discarded immediately when
   --    the socket is closed.
   --  Positive values specify an upper bound for the linger period in.
   --  Pending messages shall not be discarded after a call to close;
   --     attempting to terminate the socket's will block until either
   --    all pending messages have been sent to a peer,
   --    or the linger period expires, after which any pending messages shall
   --    be discarded.



   not overriding
   function Get_Reconnection_Interval
     (This : Socket) return Duration;
   not overriding
   procedure  Set_Reconnection_Interval
     (This   : in out Socket;
      Period : Duration := 0.100);
   --  Sets the initial reconnection interval for the specified socket.
   --  The reconnection interval is the period ØMQ shall wait between attempts
   --  to reconnect disconnected peers when using connection-oriented
   --  transports. The negative value means no reconnection.
   --  The reconnection interval may be randomized by ØMQ to prevent
   --  reconnection storms in topologies with a large number of
   --  peers per socket.


   not overriding
   function Get_Maximum_Reconnection_Interval
     (This : Socket) return Duration;
   not overriding
   procedure Set_Maximum_Reconnection_Interval
     (This   : in out Socket;
      Period : Duration := 0.0) with
     Pre => Period = 0.0 or else This.Get_Reconnection_Interval < Period;
   --  Sets the maximum reconnection interval for the specified socket.
   --  This is the maximum period ØMQ shall wait between attempts to reconnect.
   --  On each reconnect attempt, the previous interval shall be doubled
   --  untill ZMQ_RECONNECT_IVL_MAX is reached.
   --  This allows for exponential backoff strategy.
   --  Default value means no exponential backoff is performed and reconnect
   --  interval calculations are only based on reconnection_interval.

   not overriding
   function Get_Maximum_Length_Of_The_Queue_Of_Outstanding_Connections
     (This : Socket) return Natural;
   not overriding
   procedure Set_Maximum_Length_Of_The_Queue_Of_Outstanding_Connections
     (This        : in out Socket;
      Connections : Natural  := 100);
   --  Set the maximum length of the queue of outstanding peer connections
   --  this only applies to connection-oriented transports.
   --  For details refer to your operating system documentation for the
   --  listen function


   not overriding
   function Get_Maximum_Acceptable_Inbound_Message_Size
     (This : Socket) return Long_Long_Integer;
   not overriding
   procedure Set_Maximum_Acceptable_Inbound_Message_Size
     (This   : in out Socket;
      Bytes  : Long_Long_Integer  := 0)  with
     Pre => Bytes >= -1;
   --  Limits the size of the inbound message.
   --  If a peer sends a message larger than Bytes it is disconnected.
   --  Value of -1 means no limit.


   not overriding
   function Get_Maximum_Network_Hops_For_Multicast_Packets
     (This : Socket) return Positive;
   not overriding
   procedure Set_Maximum_Network_Hops_For_Multicast_Packets
     (This         : in out Socket;
      Network_Hops : Positive  := 1);
   --  Sets the time-to-live field in every multicast packet sent from
   --  this socket.
   --  The default is 1 which means that the multicast packets don't leave
   --  the local network

   not overriding
   function Get_Recieve_Timeout
     (This : Socket) return Duration;
   not overriding
   procedure Set_Recieve_Timeout
     (This    : in out Socket;
      Timeout : Duration := Duration'Last)  with
     Pre => Timeout > 0.0;
   --  Sets the timeout for receive operation on the socket.
   --  If the value is 0, receive will return immediately,
   --  with a EAGAIN error if there is no message to receive.
   --  If the value is Duration'Last, it will block until a message
   --  is available.
   --  For all other values, it will wait for a message for that amount of
   --  time before returning with an EAGAIN error.

   not overriding
   function Get_Send_Timeout
     (This : Socket) return Duration;
   not overriding
   procedure Set_Send_Timeout
     (This    : in out Socket;
      Timeout : Duration := Duration'Last)  with
     Pre => Timeout >= 0.0;
   --  Sets the timeout for send operation on the socket.
   --  If the value is 0, send  will return immediately, with a EAGAIN error
   --  if the message cannot be sent.
   --  If the value is Duration'Last, it will block until the message is sent.
   --  For all other values, it will try to send the message for that amount
   --  of time before returning with an EAGAIN error.

   not overriding
   function Get_Use_IPv4_Only
     (This : Socket) return Boolean;
   not overriding
   procedure Set_Use_IPv4_Only
     (This   : in out Socket;
      IPv4   : Boolean := True);
   --  Sets the underlying native socket type.
   --  False will enable use IPv6 sockets.
   --  An IPv6 socket lets applications connect to and accept
   --  connections from both IPv4 and IPv6 hosts.


   not overriding
   function More_Message_Parts_To_Follow
     (This : Socket) return Boolean;
   --  Returns True if the multi-part message currently being read from the
   --  specified socket has more message parts to follow.
   --  If there are no message parts to follow or if the message currently
   --  being read is not a multi-part message a value of True will be returned.
   --  Otherwise, False will be returned.



   not overriding
   function Get_Socket_Identity
     (This : Socket) return Ada.Streams.Stream_Element_Array;
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
   function Get_Multicast_Loopback
     (This : Socket) return Boolean;
   --  Returns True if multicast transports shall be recievd bye the
   --  loopback interface.


   --  ========================================================================
   --  Send and Recieve
   --  ========================================================================

   not overriding
   procedure Connect
     (This    : in out Socket;
      Address : String);

   not overriding
   procedure Connect
     (This    : in out Socket;
      Address : Ada.Strings.Unbounded.Unbounded_String);



   not overriding
   procedure Send (This   : in out Socket;
                   Msg    : Messages.Message'Class;
                   Flags  : Socket_Flags := No_Flags);

   not overriding
   procedure Send (This   : in out Socket;
                   Msg    : String;
                   Flags  : Socket_Flags := No_Flags);

   not overriding
   procedure Send (This    : in out Socket;
                   Msg     : Ada.Strings.Unbounded.Unbounded_String;
                   Flags   : Socket_Flags := No_Flags);

   not overriding
   procedure Send (This    : in out Socket;
                   Msg     : Ada.Streams.Stream_Element_Array;
                   Flags   : Socket_Flags := No_Flags);

   not overriding
   procedure Send (This           : in out Socket;
                   Msg_Address    : System.Address;
                   Msg_Length     : Natural;
                   Flags          : Socket_Flags := No_Flags);
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
   procedure Send_Generic (This    : in out Socket;
                           Msg     : Element;
                           Flags   : Socket_Flags := No_Flags);


   not overriding
   procedure Recv
     (This  : in Socket;
      Msg   : out Ada.Strings.Unbounded.Unbounded_String;
      Flags : Socket_Flags := No_Flags);

   not overriding
   function Recv
     (This       : in Socket;
      Max_Length : Natural;
      Flags      : Socket_Flags := No_Flags)
      return  String;

   not overriding
   procedure Recv
     (This    : in Socket;
      Msg     : in out Messages.Message'Class;
      Flags   : Socket_Flags := No_Flags);


   not overriding
   function Recv
     (This    : in Socket;
      Flags   : Socket_Flags := No_Flags) return String;

   not overriding
   function Recv
     (This    : in Socket;
      Flags   : Socket_Flags := No_Flags)
      return Ada.Strings.Unbounded.Unbounded_String;

   not overriding
   procedure Recv
     (This    : in Socket;
      Flags   : Socket_Flags := No_Flags);



   not overriding
   procedure Proxy (Frontend  : not null access Socket;
                    Backend   : not null access Socket'Class;
                    Capture   : access Socket'Class);


   overriding
   procedure Finalize (This : in out Socket);
   not overriding
   procedure Close (This : in out Socket) renames Finalize;

   --


   not overriding
   function Get_Impl (This : in Socket) return System.Address;

   type Event_Type is mod 2 ** 32;
   EVENT_CONNECTED       : constant Event_Type := 2#0000_0000_0001#;
   EVENT_CONNECT_DELAYED : constant Event_Type := 2#0000_0000_0010#;
   EVENT_CONNECT_RETRIED : constant Event_Type := 2#0000_0000_0100#;
   EVENT_LISTENING       : constant Event_Type := 2#0000_0000_1000#;
   EVENT_BIND_FAILED     : constant Event_Type := 2#0000_0001_0000#;
   EVENT_ACCEPTED        : constant Event_Type := 2#0000_0010_0000#;
   EVENT_ACCEPT_FAILED   : constant Event_Type := 2#0000_0100_0000#;
   EVENT_CLOSED          : constant Event_Type := 2#0000_1000_0000#;
   EVENT_CLOSE_FAILED    : constant Event_Type := 2#0001_0000_0000#;
   EVENT_DISCONNECTED    : constant Event_Type := 2#0010_0000_0000#;
   EVENT_ALL             : constant Event_Type := 2#0011_1111_1111#;

   not overriding
   procedure  Setsockopt (This       : in out Socket;
                          Option     : Interfaces.C.int;
                          Value      : System.Address;
                          Value_Size : Natural);
   not overriding
   procedure  Setsockopt (This    : in out Socket;
                          Option  : Interfaces.C.int;
                          Value   : String);
   not overriding
   procedure  Setsockopt (This    : in out Socket;
                          Option  : Interfaces.C.int;
                          Value   : Boolean);
   not overriding
   procedure  Setsockopt (This    : in out Socket;
                          Option  : Interfaces.C.int;
                          Value   : Integer);
   not overriding
   procedure  Setsockopt (This    : in out Socket;
                          Option  : Interfaces.C.int;
                          Value   : Long_Long_Integer);

   not overriding
   procedure  Setsockopt (This    : in out Socket;
                          Option  : Interfaces.C.int;
                          Value   : Interfaces.C.unsigned_long);
   not overriding
   procedure  Setsockopt (This    : in out Socket;
                          Option  : Interfaces.C.int;
                          Value   : Duration);
   not overriding
   procedure Setsockopt (This    : in out Socket;
                         Option  : Interfaces.C.int;
                         Value   : Ada.Streams.Stream_Element_Array);

   --
   --  Low level setopt getopt operations.
   --

   not overriding
   function  Getsockopt
     (This    : in Socket;
      Option  : Interfaces.C.int) return String;

   not overriding
   function  Getsockopt
     (This    : in Socket;
      Option  : Interfaces.C.int) return Boolean;

   not overriding
   function  Getsockopt
     (This    : in Socket;
      Option  : Interfaces.C.int) return Integer;

   not overriding
   function  Getsockopt
     (This    : in Socket;
      Option  : Interfaces.C.int) return Long_Long_Integer;

   not overriding
   function  Getsockopt
     (This    : in Socket;
      Option  : Interfaces.C.int) return Interfaces.C.unsigned_long;

   not overriding
   function  Getsockopt
     (This    : in Socket;
      Option  : Interfaces.C.int) return Duration;

   not overriding
   function Getsockopt
     (This    : in Socket;
      Option  : Interfaces.C.int) return Ada.Streams.Stream_Element_Array;

   not overriding
   procedure  Getsockopt (This       : in Socket;
                          Option     : Interfaces.C.int;
                          Value      : System.Address;
                          Value_Size : in out Natural);


   function Stream
     (This : Socket)
      return not null access Ada.Streams.Root_Stream_Type'Class with
     Pre => This.Retrieve_Socket_Type = STREAM;

private

   type Socket_Stream (Self : not null access Socket'Class) is new
     Ada.Streams.Root_Stream_Type with null record;

   procedure Read
     (Stream : in out Socket_Stream;
      Item   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset);

   procedure Write
     (Stream : in out Socket_Stream;
      Item   : Ada.Streams.Stream_Element_Array);

   procedure Read_Socket
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      S      : out Socket);
   procedure Write_Socket
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      S      : Socket);

   type Socket is new Ada.Finalization.Limited_Controlled with record
      C    : System.Address := System.Null_Address;
      S    : aliased Socket_Stream (Socket'Access);
   end record;

   for Socket'Read use Read_Socket;
   for Socket'Write use Write_Socket;


   MAX_OPTION_SIZE : constant := 256;

end ZMQ.Sockets;
