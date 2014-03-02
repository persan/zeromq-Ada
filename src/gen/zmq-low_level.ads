-------------------------------------------------------------------------------
--                                                                           --
--                             0MQ Ada-binding                               --
--                                                                           --
--                         Z M Q . L O W _ L E V E L                         --
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

--  The contents of this file is derived from zmq.h using the
--   -fdump-ada-spec switch for gcc.

pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;
with System;

with Interfaces.C.Extensions;

package ZMQ.Low_Level is

   pragma Preelaborate;
   pragma Warnings (Off);

   package Defs is
      --  This package is here to give a namespace to constants, since
      --  identifiers in Ada are caseinsensetive.

      ZMQ_VERSION_MAJOR : constant := 4;  --  zmq.h:32
      ZMQ_VERSION_MINOR : constant := 0;  --  zmq.h:33
      ZMQ_VERSION_PATCH : constant := 3;  --  zmq.h:34
      --  arg-macro: function ZMQ_MAKE_VERSION (major, minor, p((major) * 10000 + (minor) * 100 + (patch)
      --    return (major) * 10000 + (minor) * 100 + (patch);
      --  unsupported macro: ZMQ_VERSION ZMQ_MAKE_VERSION(ZMQ_VERSION_MAJOR, ZMQ_VERSION_MINOR, ZMQ_VERSION_PATCH)

      ZMQ_HAUSNUMERO : constant := 156384712;  --  zmq.h:97
      EFSM           : constant := ZMQ_HAUSNUMERO + 51;
      ENOCOMPATPROTO : constant := ZMQ_HAUSNUMERO + 52;
      ETERM          : constant := ZMQ_HAUSNUMERO + 53;
      EMTHREAD       : constant := ZMQ_HAUSNUMERO + 54;

      ZMQ_IO_THREADS  : constant := 1;  --  zmq.h:179
      ZMQ_MAX_SOCKETS : constant := 2;  --  zmq.h:180

      ZMQ_IO_THREADS_DFLT  : constant := 1;  --  zmq.h:183
      ZMQ_MAX_SOCKETS_DFLT : constant := 1023;  --  zmq.h:184

      ZMQ_PAIR   : constant := 0;  --  zmq.h:227
      ZMQ_PUB    : constant := 1;  --  zmq.h:228
      ZMQ_SUB    : constant := 2;  --  zmq.h:229
      ZMQ_REQ    : constant := 3;  --  zmq.h:230
      ZMQ_REP    : constant := 4;  --  zmq.h:231
      ZMQ_DEALER : constant := 5;  --  zmq.h:232
      ZMQ_ROUTER : constant := 6;  --  zmq.h:233
      ZMQ_PULL   : constant := 7;  --  zmq.h:234
      ZMQ_PUSH   : constant := 8;  --  zmq.h:235
      ZMQ_XPUB   : constant := 9;  --  zmq.h:236
      ZMQ_XSUB   : constant := 10;  --  zmq.h:237
      ZMQ_STREAM : constant := 11;  --  zmq.h:238
      ZMQ_XREQ   : constant := ZMQ_DEALER;
      ZMQ_XREP   : constant := ZMQ_ROUTER;

      ZMQ_AFFINITY            : constant := 4;  --  zmq.h:245
      ZMQ_IDENTITY            : constant := 5;  --  zmq.h:246
      ZMQ_SUBSCRIBE           : constant := 6;  --  zmq.h:247
      ZMQ_UNSUBSCRIBE         : constant := 7;  --  zmq.h:248
      ZMQ_RATE                : constant := 8;  --  zmq.h:249
      ZMQ_RECOVERY_IVL        : constant := 9;  --  zmq.h:250
      ZMQ_SNDBUF              : constant := 11;  --  zmq.h:251
      ZMQ_RCVBUF              : constant := 12;  --  zmq.h:252
      ZMQ_RCVMORE             : constant := 13;  --  zmq.h:253
      ZMQ_FD                  : constant := 14;  --  zmq.h:254
      ZMQ_EVENTS              : constant := 15;  --  zmq.h:255
      ZMQ_TYPE                : constant := 16;  --  zmq.h:256
      ZMQ_LINGER              : constant := 17;  --  zmq.h:257
      ZMQ_RECONNECT_IVL       : constant := 18;  --  zmq.h:258
      ZMQ_BACKLOG             : constant := 19;  --  zmq.h:259
      ZMQ_RECONNECT_IVL_MAX   : constant := 21;  --  zmq.h:260
      ZMQ_MAXMSGSIZE          : constant := 22;  --  zmq.h:261
      ZMQ_SNDHWM              : constant := 23;  --  zmq.h:262
      ZMQ_RCVHWM              : constant := 24;  --  zmq.h:263
      ZMQ_MULTICAST_HOPS      : constant := 25;  --  zmq.h:264
      ZMQ_RCVTIMEO            : constant := 27;  --  zmq.h:265
      ZMQ_SNDTIMEO            : constant := 28;  --  zmq.h:266
      ZMQ_LAST_ENDPOINT       : constant := 32;  --  zmq.h:267
      ZMQ_ROUTER_MANDATORY    : constant := 33;  --  zmq.h:268
      ZMQ_TCP_KEEPALIVE       : constant := 34;  --  zmq.h:269
      ZMQ_TCP_KEEPALIVE_CNT   : constant := 35;  --  zmq.h:270
      ZMQ_TCP_KEEPALIVE_IDLE  : constant := 36;  --  zmq.h:271
      ZMQ_TCP_KEEPALIVE_INTVL : constant := 37;  --  zmq.h:272
      ZMQ_TCP_ACCEPT_FILTER   : constant := 38;  --  zmq.h:273
      ZMQ_IMMEDIATE           : constant := 39;  --  zmq.h:274
      ZMQ_XPUB_VERBOSE        : constant := 40;  --  zmq.h:275
      ZMQ_ROUTER_RAW          : constant := 41;  --  zmq.h:276
      ZMQ_IPV6                : constant := 42;  --  zmq.h:277
      ZMQ_MECHANISM           : constant := 43;  --  zmq.h:278
      ZMQ_PLAIN_SERVER        : constant := 44;  --  zmq.h:279
      ZMQ_PLAIN_USERNAME      : constant := 45;  --  zmq.h:280
      ZMQ_PLAIN_PASSWORD      : constant := 46;  --  zmq.h:281
      ZMQ_CURVE_SERVER        : constant := 47;  --  zmq.h:282
      ZMQ_CURVE_PUBLICKEY     : constant := 48;  --  zmq.h:283
      ZMQ_CURVE_SECRETKEY     : constant := 49;  --  zmq.h:284
      ZMQ_CURVE_SERVERKEY     : constant := 50;  --  zmq.h:285
      ZMQ_PROBE_ROUTER        : constant := 51;  --  zmq.h:286
      ZMQ_REQ_CORRELATE       : constant := 52;  --  zmq.h:287
      ZMQ_REQ_RELAXED         : constant := 53;  --  zmq.h:288
      ZMQ_CONFLATE            : constant := 54;  --  zmq.h:289
      ZMQ_ZAP_DOMAIN          : constant := 55;  --  zmq.h:290

      ZMQ_MORE : constant := 1;  --  zmq.h:293

      ZMQ_DONTWAIT : constant := 1;  --  zmq.h:296
      ZMQ_SNDMORE  : constant := 2;  --  zmq.h:297

      ZMQ_NULL  : constant := 0;  --  zmq.h:300
      ZMQ_PLAIN : constant := 1;  --  zmq.h:301
      ZMQ_CURVE : constant := 2;  --  zmq.h:302

      ZMQ_IPV4ONLY                : constant := 31;  --  zmq.h:305
      ZMQ_DELAY_ATTACH_ON_CONNECT : constant := ZMQ_IMMEDIATE;
      ZMQ_NOBLOCK                 : constant := ZMQ_DONTWAIT;
      ZMQ_FAIL_UNROUTABLE         : constant := ZMQ_ROUTER_MANDATORY;
      ZMQ_ROUTER_BEHAVIOR         : constant := ZMQ_ROUTER_MANDATORY;

      ZMQ_EVENT_CONNECTED       : constant := 1;  --  zmq.h:316
      ZMQ_EVENT_CONNECT_DELAYED : constant := 2;  --  zmq.h:317
      ZMQ_EVENT_CONNECT_RETRIED : constant := 4;  --  zmq.h:318

      ZMQ_EVENT_LISTENING   : constant := 8;  --  zmq.h:320
      ZMQ_EVENT_BIND_FAILED : constant := 16;  --  zmq.h:321

      ZMQ_EVENT_ACCEPTED      : constant := 32;  --  zmq.h:323
      ZMQ_EVENT_ACCEPT_FAILED : constant := 64;  --  zmq.h:324

      ZMQ_EVENT_CLOSED          : constant := 128;  --  zmq.h:326
      ZMQ_EVENT_CLOSE_FAILED    : constant := 256;  --  zmq.h:327
      ZMQ_EVENT_DISCONNECTED    : constant := 512;  --  zmq.h:328
      ZMQ_EVENT_MONITOR_STOPPED : constant := 1024;  --  zmq.h:329
      --  unsupported macro: ZMQ_EVENT_ALL ( ZMQ_EVENT_CONNECTED | ZMQ_EVENT_CONNECT_DELAYED | ZMQ_EVENT_CONNECT_RETRIED | ZMQ_EVENT_LISTENING | ZMQ_EVENT_BIND_FAILED | ZMQ_EVENT_ACCEPTED | ZMQ_EVENT_ACCEPT_FAILED | ZMQ_EVENT_CLOSED | ZMQ_EVENT_CLOSE_FAILED | ZMQ_EVENT_DISCONNECTED | ZMQ_EVENT_MONITOR_STOPPED)

      ZMQ_POLLIN  : constant := 1;  --  zmq.h:372
      ZMQ_POLLOUT : constant := 2;  --  zmq.h:373
      ZMQ_POLLERR : constant := 4;  --  zmq.h:374

      ZMQ_POLLITEMS_DFLT : constant := 16;  --  zmq.h:388

      ZMQ_STREAMER  : constant := 1;  --  zmq.h:403
      ZMQ_FORWARDER : constant := 2;  --  zmq.h:404
      ZMQ_QUEUE     : constant := 3;  --  zmq.h:405

   end Defs;

   procedure zmq_version (major : access int; minor : access int; patch : access int);  -- zmq.h:162
   pragma Import (C, zmq_version, "zmq_version");

   function zmq_errno return int;  -- zmq.h:168
   pragma Import (C, zmq_errno, "zmq_errno");

   function zmq_strerror (errnum : int) return Interfaces.C.Strings.chars_ptr;  -- zmq.h:171
   pragma Import (C, zmq_strerror, "zmq_strerror");

   function zmq_ctx_new return System.Address;  -- zmq.h:186
   pragma Import (C, zmq_ctx_new, "zmq_ctx_new");

   function zmq_ctx_term (context : System.Address) return int;  -- zmq.h:187
   pragma Obsolescent;
   pragma Import (C, zmq_ctx_term, "zmq_ctx_term");

   function zmq_ctx_shutdown (ctx_u : System.Address) return int;  -- zmq.h:188
   pragma Obsolescent;
   pragma Import (C, zmq_ctx_shutdown, "zmq_ctx_shutdown");

   function zmq_ctx_set (context : System.Address; option : int; optval : int) return int;  -- zmq.h:189
   pragma Import (C, zmq_ctx_set, "zmq_ctx_set");

   function zmq_ctx_get (context : System.Address; option : int) return int;  -- zmq.h:190
   pragma Import (C, zmq_ctx_get, "zmq_ctx_get");

   function zmq_init (io_threads : int) return System.Address;  -- zmq.h:193
   pragma Import (C, zmq_init, "zmq_init");

   function zmq_term (context : System.Address) return int;  -- zmq.h:194
   pragma Obsolescent;
   pragma Import (C, zmq_term, "zmq_term");

   function zmq_ctx_destroy (context : System.Address) return int;  -- zmq.h:195
   pragma Import (C, zmq_ctx_destroy, "zmq_ctx_destroy");

   type zmq_msg_t_u_u_array is array (0 .. 31) of aliased unsigned_char;
   type zmq_msg_t is record
      u_u : aliased zmq_msg_t_u_u_array;  -- zmq.h:202
   end record;
   pragma Convention (C_Pass_By_Copy, zmq_msg_t);  -- zmq.h:202

   --  skipped function type zmq_free_fn

   function zmq_msg_init (msg : access zmq_msg_t) return int;  -- zmq.h:206
   pragma Import (C, zmq_msg_init, "zmq_msg_init");

   function zmq_msg_init_size (msg : access zmq_msg_t; size : size_t) return int;  -- zmq.h:207
   pragma Import (C, zmq_msg_init_size, "zmq_msg_init_size");

   function zmq_msg_init_data
     (msg  : access zmq_msg_t;
      data : System.Address;
      size : size_t;
      ffn  : access procedure (arg1 : System.Address; arg2 : System.Address);
      hint : System.Address) return int;  -- zmq.h:208
   pragma Import (C, zmq_msg_init_data, "zmq_msg_init_data");

   function zmq_msg_send (msg : access zmq_msg_t; s : System.Address; flags : int) return int;  -- zmq.h:210
   pragma Import (C, zmq_msg_send, "zmq_msg_send");

   function zmq_msg_recv (msg : access zmq_msg_t; s : System.Address; flags : int) return int;  -- zmq.h:211
   pragma Import (C, zmq_msg_recv, "zmq_msg_recv");

   function zmq_msg_close (msg : access zmq_msg_t) return int;  -- zmq.h:212
   pragma Import (C, zmq_msg_close, "zmq_msg_close");

   function zmq_msg_move (dest : access zmq_msg_t; src : access zmq_msg_t) return int;  -- zmq.h:213
   pragma Import (C, zmq_msg_move, "zmq_msg_move");

   function zmq_msg_copy (dest : access zmq_msg_t; src : access zmq_msg_t) return int;  -- zmq.h:214
   pragma Import (C, zmq_msg_copy, "zmq_msg_copy");

   function zmq_msg_data (msg : access zmq_msg_t) return System.Address;  -- zmq.h:215
   pragma Import (C, zmq_msg_data, "zmq_msg_data");

   function zmq_msg_size (msg : access zmq_msg_t) return size_t;  -- zmq.h:216
   pragma Import (C, zmq_msg_size, "zmq_msg_size");

   function zmq_msg_more (msg : access zmq_msg_t) return int;  -- zmq.h:217
   pragma Import (C, zmq_msg_more, "zmq_msg_more");

   function zmq_msg_get (msg : access zmq_msg_t; option : int) return int;  -- zmq.h:218
   pragma Import (C, zmq_msg_get, "zmq_msg_get");

   function zmq_msg_set (msg : access zmq_msg_t; option : int; optval : int) return int;  -- zmq.h:219
   pragma Import (C, zmq_msg_set, "zmq_msg_set");

   type zmq_event_t is record
      event : aliased Extensions.Unsigned_16;  -- zmq.h:340
      value : aliased Extensions.Signed_32;  -- zmq.h:341
   end record;
   pragma Convention (C_Pass_By_Copy, zmq_event_t);  -- zmq.h:342

   --  skipped anonymous struct anon_6

   function zmq_socket (arg1 : System.Address; c_type : int) return System.Address;  -- zmq.h:344
   pragma Import (C, zmq_socket, "zmq_socket");

   function zmq_close (s : System.Address) return int;  -- zmq.h:345
   pragma Import (C, zmq_close, "zmq_close");

   function zmq_setsockopt
     (s         : System.Address;
      option    : int;
      optval    : System.Address;
      optvallen : size_t) return int;  -- zmq.h:346
   pragma Import (C, zmq_setsockopt, "zmq_setsockopt");

   function zmq_getsockopt
     (s         : System.Address;
      option    : int;
      optval    : System.Address;
      optvallen : access size_t) return int;  -- zmq.h:348
   pragma Import (C, zmq_getsockopt, "zmq_getsockopt");

   function zmq_bind (s : System.Address; addr : Interfaces.C.Strings.chars_ptr) return int;  -- zmq.h:350
   pragma Import (C, zmq_bind, "zmq_bind");

   function zmq_connect (s : System.Address; addr : Interfaces.C.Strings.chars_ptr) return int;  -- zmq.h:351
   pragma Import (C, zmq_connect, "zmq_connect");

   function zmq_unbind (s : System.Address; addr : Interfaces.C.Strings.chars_ptr) return int;  -- zmq.h:352
   pragma Import (C, zmq_unbind, "zmq_unbind");

   function zmq_disconnect (s : System.Address; addr : Interfaces.C.Strings.chars_ptr) return int;  -- zmq.h:353
   pragma Import (C, zmq_disconnect, "zmq_disconnect");

   function zmq_send (s : System.Address; buf : System.Address; len : size_t; flags : int) return int;  -- zmq.h:354
   pragma Import (C, zmq_send, "zmq_send");

   function zmq_send_const (s : System.Address; buf : System.Address; len : size_t; flags : int) return int;  -- zmq.h:355
   pragma Import (C, zmq_send_const, "zmq_send_const");

   function zmq_recv (s : System.Address; buf : System.Address; len : size_t; flags : int) return int;  -- zmq.h:356
   pragma Import (C, zmq_recv, "zmq_recv");

   function zmq_socket_monitor
     (s      : System.Address;
      addr   : Interfaces.C.Strings.chars_ptr;
      events : int) return int;  -- zmq.h:357
   pragma Import (C, zmq_socket_monitor, "zmq_socket_monitor");

   function zmq_sendmsg (s : System.Address; msg : access zmq_msg_t; flags : int) return int;  -- zmq.h:359
   pragma Obsolescent;
   pragma Import (C, zmq_sendmsg, "zmq_sendmsg");

   function zmq_recvmsg (s : System.Address; msg : access zmq_msg_t; flags : int) return int;  -- zmq.h:360
   pragma Obsolescent;
   pragma Import (C, zmq_recvmsg, "zmq_recvmsg");

   --  skipped empty struct iovec

   function zmq_sendiov (s : System.Address; iov : System.Address; count : size_t; flags : int) return int;  -- zmq.h:365
   pragma Import (C, zmq_sendiov, "zmq_sendiov");

   function zmq_recviov (s : System.Address; iov : System.Address; count : access size_t; flags : int) return int;  -- zmq.h:366
   pragma Import (C, zmq_recviov, "zmq_recviov");

   type zmq_pollitem_t is record
      socket  : System.Address;  -- zmq.h:378
      fd      : aliased int;  -- zmq.h:382
      events  : aliased short;  -- zmq.h:384
      revents : aliased short;  -- zmq.h:385
   end record;
   pragma Convention (C_Pass_By_Copy, zmq_pollitem_t);  -- zmq.h:386

   --  skipped anonymous struct anon_7

   function zmq_poll (items : access zmq_pollitem_t; nitems : int; timeout : long) return int;  -- zmq.h:390
   pragma Import (C, zmq_poll, "zmq_poll");

   function zmq_proxy (frontend : System.Address; backend : System.Address; capture : System.Address) return int;  -- zmq.h:394
   pragma Import (C, zmq_proxy, "zmq_proxy");

   function zmq_z85_encode
     (dest : Interfaces.C.Strings.chars_ptr;
      data : access Extensions.Unsigned_8;
      size : size_t) return Interfaces.C.Strings.chars_ptr;  -- zmq.h:397
   pragma Import (C, zmq_z85_encode, "zmq_z85_encode");

   function zmq_z85_decode
     (dest   : access Extensions.Unsigned_8;
      string : Interfaces.C.Strings.chars_ptr) return access Extensions.Unsigned_8;  -- zmq.h:400
   pragma Import (C, zmq_z85_decode, "zmq_z85_decode");

   function zmq_device (c_type : int; frontend : System.Address; backend : System.Address) return int;  -- zmq.h:407
   pragma Import (C, zmq_device, "zmq_device");

end ZMQ.Low_Level;
