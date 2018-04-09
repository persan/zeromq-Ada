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
--  begin read only
--
--  The contents of this file is derived from zmq.h using the
--   -fdump-ada-spec switch for gcc.
pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;
with System;

package ZMQ.Low_Level is

   pragma Preelaborate;
   pragma Warnings (Off);

   package Defs is
      --  This package is here to give a namespace to constants, since
      --  identifiers in Ada are caseinsensetive.

      ZMQ_VERSION_MAJOR : constant := 4;  --  zmq.h:32
      ZMQ_VERSION_MINOR : constant := 1;  --  zmq.h:33
      ZMQ_VERSION_PATCH : constant := 6;  --  zmq.h:34
      --  arg-macro: function ZMQ_MAKE_VERSION (major, minor, patch)
      --    return (major) * 10000 + (minor) * 100 + (patch);
      --  unsupported macro: ZMQ_VERSION ZMQ_MAKE_VERSION(ZMQ_VERSION_MAJOR, ZMQ_VERSION_MINOR, ZMQ_VERSION_PATCH)

      ZMQ_DEFINED_STDINT : constant := 1;  --  zmq.h:74

      ZMQ_HAUSNUMERO : constant := 156384712;  --  zmq.h:98
      EFSM           : constant := ZMQ_HAUSNUMERO + 51;
      ENOCOMPATPROTO : constant := ZMQ_HAUSNUMERO + 52;
      ETERM          : constant := ZMQ_HAUSNUMERO + 53;
      EMTHREAD       : constant := ZMQ_HAUSNUMERO + 54;

      ZMQ_IO_THREADS          : constant := 1;  --  zmq.h:180
      ZMQ_MAX_SOCKETS         : constant := 2;  --  zmq.h:181
      ZMQ_SOCKET_LIMIT        : constant := 3;  --  zmq.h:182
      ZMQ_THREAD_PRIORITY     : constant := 3;  --  zmq.h:183
      ZMQ_THREAD_SCHED_POLICY : constant := 4;  --  zmq.h:184

      ZMQ_IO_THREADS_DFLT          : constant := 1;  --  zmq.h:187
      ZMQ_MAX_SOCKETS_DFLT         : constant := 1023;  --  zmq.h:188
      ZMQ_THREAD_PRIORITY_DFLT     : constant := -1;  --  zmq.h:189
      ZMQ_THREAD_SCHED_POLICY_DFLT : constant := -1;  --  zmq.h:190

      ZMQ_PAIR   : constant := 0;  --  zmq.h:250
      ZMQ_PUB    : constant := 1;  --  zmq.h:251
      ZMQ_SUB    : constant := 2;  --  zmq.h:252
      ZMQ_REQ    : constant := 3;  --  zmq.h:253
      ZMQ_REP    : constant := 4;  --  zmq.h:254
      ZMQ_DEALER : constant := 5;  --  zmq.h:255
      ZMQ_ROUTER : constant := 6;  --  zmq.h:256
      ZMQ_PULL   : constant := 7;  --  zmq.h:257
      ZMQ_PUSH   : constant := 8;  --  zmq.h:258
      ZMQ_XPUB   : constant := 9;  --  zmq.h:259
      ZMQ_XSUB   : constant := 10;  --  zmq.h:260
      ZMQ_STREAM : constant := 11;  --  zmq.h:261
      ZMQ_XREQ   : constant := ZMQ_DEALER;
      ZMQ_XREP   : constant := ZMQ_ROUTER;

      ZMQ_AFFINITY                 : constant := 4;  --  zmq.h:268
      ZMQ_IDENTITY                 : constant := 5;  --  zmq.h:269
      ZMQ_SUBSCRIBE                : constant := 6;  --  zmq.h:270
      ZMQ_UNSUBSCRIBE              : constant := 7;  --  zmq.h:271
      ZMQ_RATE                     : constant := 8;  --  zmq.h:272
      ZMQ_RECOVERY_IVL             : constant := 9;  --  zmq.h:273
      ZMQ_SNDBUF                   : constant := 11;  --  zmq.h:274
      ZMQ_RCVBUF                   : constant := 12;  --  zmq.h:275
      ZMQ_RCVMORE                  : constant := 13;  --  zmq.h:276
      ZMQ_FD                       : constant := 14;  --  zmq.h:277
      ZMQ_EVENTS                   : constant := 15;  --  zmq.h:278
      ZMQ_TYPE                     : constant := 16;  --  zmq.h:279
      ZMQ_LINGER                   : constant := 17;  --  zmq.h:280
      ZMQ_RECONNECT_IVL            : constant := 18;  --  zmq.h:281
      ZMQ_BACKLOG                  : constant := 19;  --  zmq.h:282
      ZMQ_RECONNECT_IVL_MAX        : constant := 21;  --  zmq.h:283
      ZMQ_MAXMSGSIZE               : constant := 22;  --  zmq.h:284
      ZMQ_SNDHWM                   : constant := 23;  --  zmq.h:285
      ZMQ_RCVHWM                   : constant := 24;  --  zmq.h:286
      ZMQ_MULTICAST_HOPS           : constant := 25;  --  zmq.h:287
      ZMQ_RCVTIMEO                 : constant := 27;  --  zmq.h:288
      ZMQ_SNDTIMEO                 : constant := 28;  --  zmq.h:289
      ZMQ_LAST_ENDPOINT            : constant := 32;  --  zmq.h:290
      ZMQ_ROUTER_MANDATORY         : constant := 33;  --  zmq.h:291
      ZMQ_TCP_KEEPALIVE            : constant := 34;  --  zmq.h:292
      ZMQ_TCP_KEEPALIVE_CNT        : constant := 35;  --  zmq.h:293
      ZMQ_TCP_KEEPALIVE_IDLE       : constant := 36;  --  zmq.h:294
      ZMQ_TCP_KEEPALIVE_INTVL      : constant := 37;  --  zmq.h:295
      ZMQ_IMMEDIATE                : constant := 39;  --  zmq.h:296
      ZMQ_XPUB_VERBOSE             : constant := 40;  --  zmq.h:297
      ZMQ_ROUTER_RAW               : constant := 41;  --  zmq.h:298
      ZMQ_IPV6                     : constant := 42;  --  zmq.h:299
      ZMQ_MECHANISM                : constant := 43;  --  zmq.h:300
      ZMQ_PLAIN_SERVER             : constant := 44;  --  zmq.h:301
      ZMQ_PLAIN_USERNAME           : constant := 45;  --  zmq.h:302
      ZMQ_PLAIN_PASSWORD           : constant := 46;  --  zmq.h:303
      ZMQ_CURVE_SERVER             : constant := 47;  --  zmq.h:304
      ZMQ_CURVE_PUBLICKEY          : constant := 48;  --  zmq.h:305
      ZMQ_CURVE_SECRETKEY          : constant := 49;  --  zmq.h:306
      ZMQ_CURVE_SERVERKEY          : constant := 50;  --  zmq.h:307
      ZMQ_PROBE_ROUTER             : constant := 51;  --  zmq.h:308
      ZMQ_REQ_CORRELATE            : constant := 52;  --  zmq.h:309
      ZMQ_REQ_RELAXED              : constant := 53;  --  zmq.h:310
      ZMQ_CONFLATE                 : constant := 54;  --  zmq.h:311
      ZMQ_ZAP_DOMAIN               : constant := 55;  --  zmq.h:312
      ZMQ_ROUTER_HANDOVER          : constant := 56;  --  zmq.h:313
      ZMQ_TOS                      : constant := 57;  --  zmq.h:314
      ZMQ_CONNECT_RID              : constant := 61;  --  zmq.h:315
      ZMQ_GSSAPI_SERVER            : constant := 62;  --  zmq.h:316
      ZMQ_GSSAPI_PRINCIPAL         : constant := 63;  --  zmq.h:317
      ZMQ_GSSAPI_SERVICE_PRINCIPAL : constant := 64;  --  zmq.h:318
      ZMQ_GSSAPI_PLAINTEXT         : constant := 65;  --  zmq.h:319
      ZMQ_HANDSHAKE_IVL            : constant := 66;  --  zmq.h:320
      ZMQ_SOCKS_PROXY              : constant := 68;  --  zmq.h:321
      ZMQ_XPUB_NODROP              : constant := 69;  --  zmq.h:322

      ZMQ_MORE   : constant := 1;  --  zmq.h:325
      ZMQ_SRCFD  : constant := 2;  --  zmq.h:326
      ZMQ_SHARED : constant := 3;  --  zmq.h:327

      ZMQ_DONTWAIT : constant := 1;  --  zmq.h:330
      ZMQ_SNDMORE  : constant := 2;  --  zmq.h:331

      ZMQ_NULL   : constant := 0;  --  zmq.h:334
      ZMQ_PLAIN  : constant := 1;  --  zmq.h:335
      ZMQ_CURVE  : constant := 2;  --  zmq.h:336
      ZMQ_GSSAPI : constant := 3;  --  zmq.h:337

      ZMQ_TCP_ACCEPT_FILTER       : constant := 38;  --  zmq.h:340
      ZMQ_IPC_FILTER_PID          : constant := 58;  --  zmq.h:341
      ZMQ_IPC_FILTER_UID          : constant := 59;  --  zmq.h:342
      ZMQ_IPC_FILTER_GID          : constant := 60;  --  zmq.h:343
      ZMQ_IPV4ONLY                : constant := 31;  --  zmq.h:344
      ZMQ_DELAY_ATTACH_ON_CONNECT : constant := ZMQ_IMMEDIATE;
      ZMQ_NOBLOCK                 : constant := ZMQ_DONTWAIT;
      ZMQ_FAIL_UNROUTABLE         : constant := ZMQ_ROUTER_MANDATORY;
      ZMQ_ROUTER_BEHAVIOR         : constant := ZMQ_ROUTER_MANDATORY;

      ZMQ_EVENT_CONNECTED       : constant := 16#0001#;  --  zmq.h:356
      ZMQ_EVENT_CONNECT_DELAYED : constant := 16#0002#;  --  zmq.h:357
      ZMQ_EVENT_CONNECT_RETRIED : constant := 16#0004#;  --  zmq.h:358
      ZMQ_EVENT_LISTENING       : constant := 16#0008#;  --  zmq.h:359
      ZMQ_EVENT_BIND_FAILED     : constant := 16#0010#;  --  zmq.h:360
      ZMQ_EVENT_ACCEPTED        : constant := 16#0020#;  --  zmq.h:361
      ZMQ_EVENT_ACCEPT_FAILED   : constant := 16#0040#;  --  zmq.h:362
      ZMQ_EVENT_CLOSED          : constant := 16#0080#;  --  zmq.h:363
      ZMQ_EVENT_CLOSE_FAILED    : constant := 16#0100#;  --  zmq.h:364
      ZMQ_EVENT_DISCONNECTED    : constant := 16#0200#;  --  zmq.h:365
      ZMQ_EVENT_MONITOR_STOPPED : constant := 16#0400#;  --  zmq.h:366
      ZMQ_EVENT_ALL             : constant := 16#FFFF#;  --  zmq.h:367

      ZMQ_POLLIN  : constant := 1;  --  zmq.h:389
      ZMQ_POLLOUT : constant := 2;  --  zmq.h:390
      ZMQ_POLLERR : constant := 4;  --  zmq.h:391

      ZMQ_POLLITEMS_DFLT : constant := 16;  --  zmq.h:405

      ZMQ_HAS_CAPABILITIES : constant := 1;  --  zmq.h:420

      ZMQ_STREAMER  : constant := 1;  --  zmq.h:424
      ZMQ_FORWARDER : constant := 2;  --  zmq.h:425
      ZMQ_QUEUE     : constant := 3;  --  zmq.h:426

      --    Copyright (c) 2007-2015 Contributors as noted in the AUTHORS file
      --    This file is part of 0MQ.
      --    0MQ is free software; you can redistribute it and/or modify it under
      --    the terms of the GNU Lesser General Public License as published by
      --    the Free Software Foundation; either version 3 of the License, or
      --    (at your option) any later version.
      --    0MQ is distributed in the hope that it will be useful,
      --    but WITHOUT ANY WARRANTY; without even the implied warranty of
      --    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
      --    GNU Lesser General Public License for more details.
      --    You should have received a copy of the GNU Lesser General Public License
      --    along with this program.  If not, see <http://www.gnu.org/licenses/>.
      --    *************************************************************************
      --    NOTE to contributors. This file comprises the principal public contract
      --    for ZeroMQ API users (along with zmq_utils.h). Any change to this file
      --    supplied in a stable release SHOULD not break existing applications.
      --    In practice this means that the value of constants must not change, and
      --    that old values may not be reused for new constants.
      --    *************************************************************************
      --

      --  Version macros for compile-time API version detection
      --  Handle DSO symbol visibility
      --  Define integer types needed for event interface
      --  ****************************************************************************
      --  0MQ errors.
      --  ****************************************************************************
      --  A number random enough not to collide with different errno ranges on
      --  different OSes. The assumption is that error_t is at least 32-bit type.
      --  On Windows platform some of the standard POSIX errnos are not defined.
      --  Native 0MQ error codes.
      --  This function retrieves the errno as it is known to 0MQ library. The goal
      --  of this function is to make the code 100% portable, including where 0MQ
      --  compiled with certain CRT library (on Windows) is linked to an
      --  application that uses different CRT library.
   end Defs;

   function zmq_errno return int;  -- zmq.h:166
   pragma Import (C, zmq_errno, "zmq_errno");

   --  Resolves system errors and 0MQ errors to human-readable string.
   function zmq_strerror (errnum : int) return Interfaces.C.Strings.chars_ptr;  -- zmq.h:169
   pragma Import (C, zmq_strerror, "zmq_strerror");

   --  Run-time API version detection
   procedure zmq_version (major : access int; minor : access int; patch : access int);  -- zmq.h:172
   pragma Import (C, zmq_version, "zmq_version");

   --  ****************************************************************************
   --  0MQ infrastructure (a.k.a. context) initialisation & termination.
   --  ****************************************************************************
   --  New API
   --  Context options
   --  Default for new contexts
   function zmq_ctx_new return System.Address;  -- zmq.h:192
   pragma Import (C, zmq_ctx_new, "zmq_ctx_new");

   function zmq_ctx_term (context : System.Address) return int;  -- zmq.h:193
   pragma Obsolescent;
   pragma Import (C, zmq_ctx_term, "zmq_ctx_term");

   function zmq_ctx_shutdown (ctx_u : System.Address) return int;  -- zmq.h:194
   pragma Obsolescent;
   pragma Import (C, zmq_ctx_shutdown, "zmq_ctx_shutdown");

   function zmq_ctx_set (context : System.Address; option : int; optval : int) return int;  -- zmq.h:195
   pragma Import (C, zmq_ctx_set, "zmq_ctx_set");

   function zmq_ctx_get (context : System.Address; option : int) return int;  -- zmq.h:196
   pragma Import (C, zmq_ctx_get, "zmq_ctx_get");

   --  Old (legacy) API
   function zmq_init (io_threads : int) return System.Address;  -- zmq.h:199
   pragma Import (C, zmq_init, "zmq_init");

   function zmq_term (context : System.Address) return int;  -- zmq.h:200
   pragma Obsolescent;
   pragma Import (C, zmq_term, "zmq_term");

   function zmq_ctx_destroy (context : System.Address) return int;  -- zmq.h:201
   pragma Import (C, zmq_ctx_destroy, "zmq_ctx_destroy");

   --  ****************************************************************************
   --  0MQ message definition.
   --  ****************************************************************************
   -- Some architectures, like sparc64 and some variants of aarch64, enforce pointer
   -- * alignment and raise sigbus on violations. Make sure applications allocate
   -- * zmq_msg_t on addresses aligned on a pointer-size boundary to avoid this issue.
   --

   type zmq_msg_t_u_u_array is array (0 .. 63) of aliased unsigned_char;
   type zmq_msg_t is record
      u_u : aliased zmq_msg_t_u_u_array;  -- zmq.h:216
   end record;
   pragma Convention (C_Pass_By_Copy, zmq_msg_t);  -- zmq.h:212

   --  skipped function type zmq_free_fn

   function zmq_msg_init (msg : access zmq_msg_t) return int;  -- zmq.h:228
   pragma Import (C, zmq_msg_init, "zmq_msg_init");

   function zmq_msg_init_size (msg : access zmq_msg_t; size : size_t) return int;  -- zmq.h:229
   pragma Import (C, zmq_msg_init_size, "zmq_msg_init_size");

   function zmq_msg_init_data (msg : access zmq_msg_t; data : System.Address; size : size_t;
      ffn : access procedure (arg1 : System.Address; arg2 : System.Address); hint : System.Address) return int;  -- zmq.h:230
   pragma Import (C, zmq_msg_init_data, "zmq_msg_init_data");

   function zmq_msg_send (msg : access zmq_msg_t; s : System.Address; flags : int) return int;  -- zmq.h:232
   pragma Import (C, zmq_msg_send, "zmq_msg_send");

   function zmq_msg_recv (msg : access zmq_msg_t; s : System.Address; flags : int) return int;  -- zmq.h:233
   pragma Import (C, zmq_msg_recv, "zmq_msg_recv");

   function zmq_msg_close (msg : access zmq_msg_t) return int;  -- zmq.h:234
   pragma Import (C, zmq_msg_close, "zmq_msg_close");

   function zmq_msg_move (dest : access zmq_msg_t; src : access zmq_msg_t) return int;  -- zmq.h:235
   pragma Import (C, zmq_msg_move, "zmq_msg_move");

   function zmq_msg_copy (dest : access zmq_msg_t; src : access zmq_msg_t) return int;  -- zmq.h:236
   pragma Import (C, zmq_msg_copy, "zmq_msg_copy");

   function zmq_msg_data (msg : access zmq_msg_t) return System.Address;  -- zmq.h:237
   pragma Import (C, zmq_msg_data, "zmq_msg_data");

   function zmq_msg_size (msg : access zmq_msg_t) return size_t;  -- zmq.h:238
   pragma Import (C, zmq_msg_size, "zmq_msg_size");

   function zmq_msg_more (msg : access zmq_msg_t) return int;  -- zmq.h:239
   pragma Import (C, zmq_msg_more, "zmq_msg_more");

   function zmq_msg_get (msg : access zmq_msg_t; property : int) return int;  -- zmq.h:240
   pragma Import (C, zmq_msg_get, "zmq_msg_get");

   function zmq_msg_set (msg : access zmq_msg_t; property : int; optval : int) return int;  -- zmq.h:241
   pragma Import (C, zmq_msg_set, "zmq_msg_set");

   function zmq_msg_gets (msg : access zmq_msg_t;
      property                : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr;  -- zmq.h:242
   pragma Import (C, zmq_msg_gets, "zmq_msg_gets");

   --  ****************************************************************************
   --  0MQ socket definition.
   --  ****************************************************************************
   --  Socket types.
   --  Deprecated aliases
   --  Socket options.
   --  Message options
   --  Send/recv options.
   --  Security mechanisms
   --  Deprecated options and aliases
   --  ****************************************************************************
   --  0MQ socket events and monitoring
   --  ****************************************************************************
   --  Socket transport events (TCP and IPC only)
   function zmq_socket (arg1 : System.Address; c_type : int) return System.Address;  -- zmq.h:369
   pragma Import (C, zmq_socket, "zmq_socket");

   function zmq_close (s : System.Address) return int;  -- zmq.h:370
   pragma Import (C, zmq_close, "zmq_close");

   function zmq_setsockopt (s : System.Address; option : int; optval : System.Address;
      optvallen               : size_t) return int;  -- zmq.h:371
   pragma Import (C, zmq_setsockopt, "zmq_setsockopt");

   function zmq_getsockopt (s : System.Address; option : int; optval : System.Address;
      optvallen               : access size_t) return int;  -- zmq.h:373
   pragma Import (C, zmq_getsockopt, "zmq_getsockopt");

   function zmq_bind (s : System.Address; addr : Interfaces.C.Strings.chars_ptr) return int;  -- zmq.h:375
   pragma Import (C, zmq_bind, "zmq_bind");

   function zmq_connect (s : System.Address; addr : Interfaces.C.Strings.chars_ptr) return int;  -- zmq.h:376
   pragma Import (C, zmq_connect, "zmq_connect");

   function zmq_unbind (s : System.Address; addr : Interfaces.C.Strings.chars_ptr) return int;  -- zmq.h:377
   pragma Import (C, zmq_unbind, "zmq_unbind");

   function zmq_disconnect (s : System.Address; addr : Interfaces.C.Strings.chars_ptr) return int;  -- zmq.h:378
   pragma Import (C, zmq_disconnect, "zmq_disconnect");

   function zmq_send (s : System.Address; buf : System.Address; len : size_t; flags : int) return int;  -- zmq.h:379
   pragma Import (C, zmq_send, "zmq_send");

   function zmq_send_const (s : System.Address; buf : System.Address; len : size_t; flags : int) return int;  -- zmq.h:380
   pragma Import (C, zmq_send_const, "zmq_send_const");

   function zmq_recv (s : System.Address; buf : System.Address; len : size_t; flags : int) return int;  -- zmq.h:381
   pragma Import (C, zmq_recv, "zmq_recv");

   function zmq_socket_monitor (s : System.Address; addr : Interfaces.C.Strings.chars_ptr;
      events                      : int) return int;  -- zmq.h:382
   pragma Import (C, zmq_socket_monitor, "zmq_socket_monitor");

   --  ****************************************************************************
   --  I/O multiplexing.
   --  ****************************************************************************
   type zmq_pollitem_t is record
      socket  : System.Address;  -- zmq.h:395
      fd      : aliased int;  -- zmq.h:399
      events  : aliased short;  -- zmq.h:401
      revents : aliased short;  -- zmq.h:402
   end record;
   pragma Convention (C_Pass_By_Copy, zmq_pollitem_t);  -- zmq.h:393

   function zmq_poll (items : access zmq_pollitem_t; nitems : int; timeout : long) return int;  -- zmq.h:407
   pragma Import (C, zmq_poll, "zmq_poll");

   --  ****************************************************************************
   --  Message proxying
   --  ****************************************************************************
   function zmq_proxy (frontend : System.Address; backend : System.Address; capture : System.Address) return int;  -- zmq.h:413
   pragma Import (C, zmq_proxy, "zmq_proxy");

   function zmq_proxy_steerable (frontend : System.Address; backend : System.Address; capture : System.Address;
      control                             : System.Address) return int;  -- zmq.h:414
   pragma Import (C, zmq_proxy_steerable, "zmq_proxy_steerable");

   --  ****************************************************************************
   --  Probe library capabilities
   --  ****************************************************************************
   function zmq_has (capability : Interfaces.C.Strings.chars_ptr) return int;  -- zmq.h:421
   pragma Import (C, zmq_has, "zmq_has");

   --  Deprecated aliases
   --  Deprecated methods
   function zmq_device (c_type : int; frontend : System.Address; backend : System.Address) return int;  -- zmq.h:429
   pragma Import (C, zmq_device, "zmq_device");

   function zmq_sendmsg (s : System.Address; msg : access zmq_msg_t; flags : int) return int;  -- zmq.h:430
   pragma Obsolescent;
   pragma Import (C, zmq_sendmsg, "zmq_sendmsg");

   function zmq_recvmsg (s : System.Address; msg : access zmq_msg_t; flags : int) return int;  -- zmq.h:431
   pragma Obsolescent;
   pragma Import (C, zmq_recvmsg, "zmq_recvmsg");

   --  ****************************************************************************
   --  Encryption functions
   --  ****************************************************************************
   --  Encode data with Z85 encoding. Returns encoded data
   function zmq_z85_encode (dest : Interfaces.C.Strings.chars_ptr; data : access Interfaces.Unsigned_8;
      size                       : size_t) return Interfaces.C.Strings.chars_ptr;  -- zmq.h:439
   pragma Import (C, zmq_z85_encode, "zmq_z85_encode");

   --  Decode data with Z85 encoding. Returns decoded data
   function zmq_z85_decode (dest : access Interfaces.Unsigned_8;
      string                     : Interfaces.C.Strings.chars_ptr) return access Interfaces.Unsigned_8;  -- zmq.h:442
   pragma Import (C, zmq_z85_decode, "zmq_z85_decode");

   --  Generate z85-encoded public and private keypair with libsodium.
   --  Returns 0 on success.
   function zmq_curve_keypair (z85_public_key : Interfaces.C.Strings.chars_ptr;
      z85_secret_key                          : Interfaces.C.Strings.chars_ptr) return int;  -- zmq.h:446
   pragma Import (C, zmq_curve_keypair, "zmq_curve_keypair");

   --  ****************************************************************************
   --  These functions are not documented by man pages -- use at your own risk.
   --  If you need these to be part of the formal ZMQ API, then (a) write a man
   --  page, and (b) write a test case in tests.
   --  ****************************************************************************
   --  skipped incomplete struct iovec

   function zmq_sendiov (s : System.Address; iov : System.Address; count : size_t; flags : int) return int;  -- zmq.h:457
   pragma Import (C, zmq_sendiov, "zmq_sendiov");

   function zmq_recviov (s : System.Address; iov : System.Address; count : access size_t; flags : int) return int;  -- zmq.h:458
   pragma Import (C, zmq_recviov, "zmq_recviov");

   --  Helper functions are used by perf tests so that they don't have to care
   --  about minutiae of time-related functions on different OS platforms.
   --  Starts the stopwatch. Returns the handle to the watch.
   function zmq_stopwatch_start return System.Address;  -- zmq.h:464
   pragma Import (C, zmq_stopwatch_start, "zmq_stopwatch_start");

   --  Stops the stopwatch. Returns the number of microseconds elapsed since
   --  the stopwatch was started.
   function zmq_stopwatch_stop (watch_u : System.Address) return unsigned_long;  -- zmq.h:468
   pragma Import (C, zmq_stopwatch_stop, "zmq_stopwatch_stop");

   --  Sleeps for specified number of seconds.
   procedure zmq_sleep (seconds_u : int);  -- zmq.h:471
   pragma Import (C, zmq_sleep, "zmq_sleep");

   --  skipped function type zmq_thread_fn

   -- Start a thread. Returns a handle to the thread.
   function zmq_threadstart (func : access procedure (arg1 : System.Address);
      arg                         : System.Address) return System.Address;  -- zmq.h:476
   pragma Import (C, zmq_threadstart, "zmq_threadstart");

   -- Wait for thread to complete then free up resources.
   procedure zmq_threadclose (thread : System.Address);  -- zmq.h:479
   pragma Import (C, zmq_threadclose, "zmq_threadclose");

end ZMQ.Low_Level;
--  end read only
