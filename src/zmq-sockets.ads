------------------------------------------------------------------------------
--                                                                          --
--                             0MQ Ada-binding                              --
--                                                                          --
--                           Z M Q . S O C K E T S                          --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--            Copyright (C) 2010-2011, per.sandberg@bredband.net            --
--                                                                          --
-- 0MQ Ada-binding is free software;  you can  redistribute it  and/or      --
-- modify it under terms of the  GNU General Public License as published    --
-- by the Free Soft-ware  Foundation;                                       --
-- either version 2,  or (at your option) any later version.                --
-- 0MQ Ada-binding is distributed in the hope that it will be useful, but   --
-- WITH OUT ANY WARRANTY;                                                   --
-- without even the  implied warranty of MERCHANTABILITY or                 --
-- FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License    --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Streams;
with Ada.Finalization;
with Ada.Strings.Unbounded;
with System;
with ZMQ.Messages;
with ZMQ.Contexts;
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
      PUSH);

   type Socket is new Ada.Finalization.Limited_Controlled with private;


   type Socket_Flags is mod 2 ** 32;

   function "+" (L, R : Socket_Flags) return Socket_Flags renames "or";
   No_Flags : constant Socket_Flags := 2#0000_0000_0000_0000#;

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
   procedure  Set_high_water_mark
     (This       : in out Socket;
      Value      : Natural);
   --  Sets the high water mark for the specified socket.
   --  The high water mark is a hard limit on the maximum number of
   --  outstanding messages 0MQ shall queue in memory for any single peer
   --  that the specified socket is communicating with.
   --  If this limit has been reached the socket shall enter an exceptional
   --  state and depending on the socket type,
   --  0MQ shall take appropriate action such as blocking or dropping
   --  sent messages.
   --  Refer to the individual socket descriptions in zmq_socket(3)
   --  for details on the exact action taken for each socket type.
   --  The default ZMQ_HWM value of zero means "no limit".

   not overriding
   procedure  Set_disk_offload_size (This       : in out Socket;
                                     Value      : Boolean);
   --  Sets the disk offload (swap) size < for the specified socket.
   --  A socket which has ZMQ_SWAP set to a non - zero value may exceed

   --  in this case outstanding messages shall be offloaded to storage on
   --  disk rather than held in memory.
   --  The value defines the maximum size of the swap space in bytes


   not overriding
   procedure  Set_IO_thread_affinity (This       : in out Socket;
                                      Value      : Natural);
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
   procedure  Set_socket_identity
     (This       : in out Socket;
      Value      : Ada.Streams.Stream_Element_Array);
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
   procedure  Establish_message_filter (This       : in out Socket;
                                        Value      : String);
   not overriding
   procedure  Establish_message_filter
     (This       : in out Socket;
      Value      : Ada.Strings.Unbounded.Unbounded_String);
   procedure  Establish_message_filter
     (This       : in out Socket;
      Value      : Ada.Streams.Stream_Element_Array);
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
   procedure  Remove_message_filter (This       : in out Socket;
                                     Value      : String);
   not overriding
   procedure  Remove_message_filter
     (This       : in out Socket;
      Value      : Ada.Strings.Unbounded.Unbounded_String);
   not overriding
   procedure  Remove_message_filter
     (This       : in out Socket;
      Value      : Ada.Streams.Stream_Element_Array);
   --  Remove an existing message filter on a SUB socket.
   --  The filter specified must match an existing filter previously
   --  established with "Establish_message_filter".
   --  If the socket has several instances of the same filter attached the
   --  Remove_message_filter removes only one instance,
   --  leaving the rest in place and functional.

   not overriding
   procedure  Set_multicast_data_rate (This       : in out Socket;
                                       Value      : Natural);
   --  Sets the maximum send or receive data rate for multicast transports
   --  such as PGM using the specified socket.

   not overriding
   procedure  set_multicast_recovery_interval (This       : in out Socket;
                                               Value      : Duration);
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
   procedure  Set_multicast_loopback (This   : in out Socket;
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
   procedure  Set_kernel_transmit_buffer_size (This       : in out Socket;
                                               Value      : Natural);
   --  Sets the underlying kernel transmit buffer size for the socket
   --  to the specified size in bytes.
   --  A value of zero means leave the OS default unchanged.
   --  For details please refer to your operating system documentation
   --  for the SO_SNDBUF socket option.


   not overriding
   procedure  Set_kernel_receive_buffer_size (This       : in out Socket;
                                              Value      : Natural);
   --  Sets the underlying kernel receive buffer size for the socket to
   --  the specified size in bytes.
   --  A value of zero means leave the OS default unchanged.
   --  For details refer to your operating system documentation for the
   --  SO_RCVBUF socket option.


   function More_message_parts_to_follow (This : Socket) return Boolean;
   --  Returns True if the multi-part message currently being read from the
   --  specified socket has more message parts to follow.
   --  If there are no message parts to follow or if the message currently
   --  being read is not a multi-part message a value of True will be returned.
   --  Otherwise, False will be returned.

   function Get_high_water_mark (This : Socket) return Natural;
   --  Returns the high water mark for the specified socket.
   --  The high water mark is a hard limit on the maximum number of outstanding
   --  messages ZMQ shall queue in memory for any single peer that
   --  the specified socket is communicating with.
   --  If this limit has been reached the socket shall enter an exceptional
   --  state and depending on the socket type, ZMQ shall take appropriate
   --  action such as blocking or dropping sent messages.
   --  The default high_water_mark value of zero means "no limit".

   function Get_disk_offload_size (This : Socket) return Natural;
   --  Returns the disk offload (swap) size for the specified socket.
   --  A socket which has SWAP set to a non-zero value may exceed

   --   in this case outstanding messages shall be offloaded to storage on disk
   --   rather than held in memory.
   --  The value of defines the maximum size of the swap space in bytes.

   function Get_IO_thread_affinity (This : Socket) return Natural;
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

   function Get_socket_identity
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

   function Get_multicast_data_rate (This : Socket) return Natural;
   --  Returns the maximum send or receive data rate for multicast transports
   --  using the specified socket.

   function Get_multicast_recovery_interval (This : Socket) return Duration;
   --  Retrieves the recovery interval for multicast transports using the
   --  specified socket.
   --  The recovery interval determines the maximum time in seconds that
   --  a receiver can be absent from a multicast group before unrecoverable
   --  data loss will occur.

   function Get_multicast_loopback (This : Socket) return Boolean;
   --  Returns True if multicast transports shall be recievd bye the
   --  loopback interface.
   function Get_kernel_transmit_buffer_size (This : Socket) return Natural;
   --  Returns the underlying kernel transmit buffer size for the
   --  specified socket.
   --  A value of zero means that the OS default is in effect.
   --  For details refer to your operating system documentation for
   --   the SO_SNDBUF socket option.

   function Get_kernel_receive_buffer_size (This : Socket) return Natural;
   --  Returns the underlying kernel receive buffer size for the
   --  specified socket.
   --  A value of zero means that the OS default is in effect.
   --  For details refer to your operating system documentation
   --  for the SO_RCVBUF socket option

   not overriding
   procedure Connect (This    : in out Socket;
                      Address : String);

   procedure Connect (This    : in out Socket;
                      Address : Ada.Strings.Unbounded.Unbounded_String);



   not overriding
   procedure Send (This    : in out Socket;
                   Msg     : Messages.Message'Class;
                   Flags   : Socket_Flags := No_Flags);

   not overriding
   procedure Send (This    : in out Socket;
                   Msg     : String;
                   Flags   : Socket_Flags := No_Flags);

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
                   Msg_Addres     : System.Address;
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



   --     not overriding
   --     procedure flush (This    : in out Socket);

   not overriding
   procedure recv (This    : in Socket;
                   Msg     : Messages.Message'Class;
                   Flags   : Socket_Flags := No_Flags);

   procedure recv (This    : in Socket;
                   msg     : out Ada.Strings.Unbounded.Unbounded_String;
                   Flags   : Socket_Flags := No_Flags);


   not overriding
   function recv (This    : in Socket;
                  Flags   : Socket_Flags := No_Flags) return String;

   not overriding
   function recv (This    : in Socket;
                  Flags   : Socket_Flags := No_Flags)
                  return Ada.Strings.Unbounded.Unbounded_String;

   overriding
   procedure Finalize (this : in out Socket);
   procedure Close (this : in out Socket) renames Finalize;



   --  function "=" (Left, Right : in Context) return Boolean;
   function get_impl (This : in Socket) return System.Address;
private
   type Socket is new Ada.Finalization.Limited_Controlled with record
      c : System.Address := System.Null_Address;
   end record;
   function img (item : Ada.Streams.Stream_Element_Array) return String;

   type Socket_Opt is
     (HWM,
      SWAP,
      AFFINITY,
      IDENTITY,
      SUBSCRIBE,
      UNSUBSCRIBE,
      RATE,
      RECOVERY_IVL,
      MCAST_LOOP,
      SNDBUF,
      RCVBUF,
      RCVMORE,
      FD,
      EVENTS,
      GET_TYPE,
      LINGER,
      RECONNECT_IVL,
      BACKLOG);

   not overriding

   procedure  setsockopt (This    : in out Socket;
                          Option  : Socket_Opt;
                          Value   : String);
   not overriding
   procedure  setsockopt (This    : in out Socket;
                          Option  : Socket_Opt;
                          Value   : Boolean);
   not overriding
   procedure  setsockopt (This    : in out Socket;
                          Option  : Socket_Opt;
                          Value   : Natural);
   not overriding
   procedure setsockopt
     (This    : in out Socket;
      Option  : Socket_Opt;
      Value   : Ada.Streams.Stream_Element_Array);

   not overriding
   procedure  setsockopt (This       : in out Socket;
                          Option     : Socket_Opt;
                          Value      : System.Address;
                          Value_Size : Natural);

   --------------------------------------------------------
   --------------------------------------------------------

   function  getsockopt (This    : in Socket;
                         Option  : Socket_Opt) return String;
   not overriding
   function  getsockopt (This    : in Socket;
                         Option  : Socket_Opt) return Boolean;
   not overriding
   function  getsockopt (This    : in Socket;
                         Option  : Socket_Opt) return Natural;
   not overriding
   function getsockopt
     (This    : in Socket;
      Option  : Socket_Opt) return Ada.Streams.Stream_Element_Array;

   not overriding
   procedure  getsockopt (This       : in out Socket;
                          Option     : Socket_Opt;
                          Value      : System.Address;
                          Value_Size : Natural);



end ZMQ.Sockets;
