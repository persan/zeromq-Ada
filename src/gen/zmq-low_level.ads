-------------------------------------------------------------------------------
--                                                                           --
--                             0MQ Ada-binding                               --
--                                                                           --
--                         Z M Q . L O W _ L E V E L                         --
--                                                                           --
--                                  S p e c                                  --
--                                                                           --
--            Copyright (C) 2020-2030, per.s.sandberg@bahnhof.se             --
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
--
--  The contents of this file is derived from zmq.h using the
--   -fdump-ada-spec switch for gcc.
pragma Ada_2012;

pragma Style_Checks (Off);
pragma Warnings (Off, "-gnatwu");

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;
with System;

package ZMQ.Low_Level is

   pragma Preelaborate;
   pragma Warnings (Off);

   package Defs is
      --  This package is here to give a namespace to constants, since
      --  identifiers in Ada are caseinsensetive.

      ZMQ_VERSION_MAJOR : constant := 4;  --  zmq.h:15
      ZMQ_VERSION_MINOR : constant := 3;  --  zmq.h:16
      ZMQ_VERSION_PATCH : constant := 5;  --  zmq.h:17
      --  arg-macro: function ZMQ_MAKE_VERSION (major, minor, patch)
      --    return (major) *10000 + (minor) *100 + (patch);
      --  unsupported macro: ZMQ_VERSION ZMQ_MAKE_VERSION (ZMQ_VERSION_MAJOR, ZMQ_VERSION_MINOR, ZMQ_VERSION_PATCH)

      ZMQ_DEFINED_STDINT : constant := 1;  --  zmq.h:58

      ZMQ_HAUSNUMERO : constant := 156_384_712;  --  zmq.h:100
      EFSM           : constant := ZMQ_HAUSNUMERO + 51;
      ENOCOMPATPROTO : constant := ZMQ_HAUSNUMERO + 52;
      ETERM          : constant := ZMQ_HAUSNUMERO + 53;
      EMTHREAD       : constant := ZMQ_HAUSNUMERO + 54;

      ZMQ_IO_THREADS                 : constant := 1;  --  zmq.h:181
      ZMQ_MAX_SOCKETS                : constant := 2;  --  zmq.h:182
      ZMQ_SOCKET_LIMIT               : constant := 3;  --  zmq.h:183
      ZMQ_THREAD_PRIORITY            : constant := 3;  --  zmq.h:184
      ZMQ_THREAD_SCHED_POLICY        : constant := 4;  --  zmq.h:185
      ZMQ_MAX_MSGSZ                  : constant := 5;  --  zmq.h:186
      ZMQ_MSG_T_SIZE                 : constant := 6;  --  zmq.h:187
      ZMQ_THREAD_AFFINITY_CPU_ADD    : constant := 7;  --  zmq.h:188
      ZMQ_THREAD_AFFINITY_CPU_REMOVE : constant := 8;  --  zmq.h:189
      ZMQ_THREAD_NAME_PREFIX         : constant := 9;  --  zmq.h:190

      ZMQ_IO_THREADS_DFLT          : constant := 1;  --  zmq.h:193
      ZMQ_MAX_SOCKETS_DFLT         : constant := 1_023;  --  zmq.h:194
      ZMQ_THREAD_PRIORITY_DFLT     : constant := -1;  --  zmq.h:195
      ZMQ_THREAD_SCHED_POLICY_DFLT : constant := -1;  --  zmq.h:196

      ZMQ_PAIR   : constant := 0;  --  zmq.h:258
      ZMQ_PUB    : constant := 1;  --  zmq.h:259
      ZMQ_SUB    : constant := 2;  --  zmq.h:260
      ZMQ_REQ    : constant := 3;  --  zmq.h:261
      ZMQ_REP    : constant := 4;  --  zmq.h:262
      ZMQ_DEALER : constant := 5;  --  zmq.h:263
      ZMQ_ROUTER : constant := 6;  --  zmq.h:264
      ZMQ_PULL   : constant := 7;  --  zmq.h:265
      ZMQ_PUSH   : constant := 8;  --  zmq.h:266
      ZMQ_XPUB   : constant := 9;  --  zmq.h:267
      ZMQ_XSUB   : constant := 10;  --  zmq.h:268
      ZMQ_STREAM : constant := 11;  --  zmq.h:269
      ZMQ_XREQ   : constant := ZMQ_DEALER;
      ZMQ_XREP   : constant := ZMQ_ROUTER;

      ZMQ_AFFINITY                  : constant := 4;  --  zmq.h:276
      ZMQ_ROUTING_ID                : constant := 5;  --  zmq.h:277
      ZMQ_SUBSCRIBE                 : constant := 6;  --  zmq.h:278
      ZMQ_UNSUBSCRIBE               : constant := 7;  --  zmq.h:279
      ZMQ_RATE                      : constant := 8;  --  zmq.h:280
      ZMQ_RECOVERY_IVL              : constant := 9;  --  zmq.h:281
      ZMQ_SNDBUF                    : constant := 11;  --  zmq.h:282
      ZMQ_RCVBUF                    : constant := 12;  --  zmq.h:283
      ZMQ_RCVMORE                   : constant := 13;  --  zmq.h:284
      ZMQ_FD                        : constant := 14;  --  zmq.h:285
      ZMQ_EVENTS                    : constant := 15;  --  zmq.h:286
      ZMQ_TYPE                      : constant := 16;  --  zmq.h:287
      ZMQ_LINGER                    : constant := 17;  --  zmq.h:288
      ZMQ_RECONNECT_IVL             : constant := 18;  --  zmq.h:289
      ZMQ_BACKLOG                   : constant := 19;  --  zmq.h:290
      ZMQ_RECONNECT_IVL_MAX         : constant := 21;  --  zmq.h:291
      ZMQ_MAXMSGSIZE                : constant := 22;  --  zmq.h:292
      ZMQ_SNDHWM                    : constant := 23;  --  zmq.h:293
      ZMQ_RCVHWM                    : constant := 24;  --  zmq.h:294
      ZMQ_MULTICAST_HOPS            : constant := 25;  --  zmq.h:295
      ZMQ_RCVTIMEO                  : constant := 27;  --  zmq.h:296
      ZMQ_SNDTIMEO                  : constant := 28;  --  zmq.h:297
      ZMQ_LAST_ENDPOINT             : constant := 32;  --  zmq.h:298
      ZMQ_ROUTER_MANDATORY          : constant := 33;  --  zmq.h:299
      ZMQ_TCP_KEEPALIVE             : constant := 34;  --  zmq.h:300
      ZMQ_TCP_KEEPALIVE_CNT         : constant := 35;  --  zmq.h:301
      ZMQ_TCP_KEEPALIVE_IDLE        : constant := 36;  --  zmq.h:302
      ZMQ_TCP_KEEPALIVE_INTVL       : constant := 37;  --  zmq.h:303
      ZMQ_IMMEDIATE                 : constant := 39;  --  zmq.h:304
      ZMQ_XPUB_VERBOSE              : constant := 40;  --  zmq.h:305
      ZMQ_ROUTER_RAW                : constant := 41;  --  zmq.h:306
      ZMQ_IPV6                      : constant := 42;  --  zmq.h:307
      ZMQ_MECHANISM                 : constant := 43;  --  zmq.h:308
      ZMQ_PLAIN_SERVER              : constant := 44;  --  zmq.h:309
      ZMQ_PLAIN_USERNAME            : constant := 45;  --  zmq.h:310
      ZMQ_PLAIN_PASSWORD            : constant := 46;  --  zmq.h:311
      ZMQ_CURVE_SERVER              : constant := 47;  --  zmq.h:312
      ZMQ_CURVE_PUBLICKEY           : constant := 48;  --  zmq.h:313
      ZMQ_CURVE_SECRETKEY           : constant := 49;  --  zmq.h:314
      ZMQ_CURVE_SERVERKEY           : constant := 50;  --  zmq.h:315
      ZMQ_PROBE_ROUTER              : constant := 51;  --  zmq.h:316
      ZMQ_REQ_CORRELATE             : constant := 52;  --  zmq.h:317
      ZMQ_REQ_RELAXED               : constant := 53;  --  zmq.h:318
      ZMQ_CONFLATE                  : constant := 54;  --  zmq.h:319
      ZMQ_ZAP_DOMAIN                : constant := 55;  --  zmq.h:320
      ZMQ_ROUTER_HANDOVER           : constant := 56;  --  zmq.h:321
      ZMQ_TOS                       : constant := 57;  --  zmq.h:322
      ZMQ_CONNECT_ROUTING_ID        : constant := 61;  --  zmq.h:323
      ZMQ_GSSAPI_SERVER             : constant := 62;  --  zmq.h:324
      ZMQ_GSSAPI_PRINCIPAL          : constant := 63;  --  zmq.h:325
      ZMQ_GSSAPI_SERVICE_PRINCIPAL  : constant := 64;  --  zmq.h:326
      ZMQ_GSSAPI_PLAINTEXT          : constant := 65;  --  zmq.h:327
      ZMQ_HANDSHAKE_IVL             : constant := 66;  --  zmq.h:328
      ZMQ_SOCKS_PROXY               : constant := 68;  --  zmq.h:329
      ZMQ_XPUB_NODROP               : constant := 69;  --  zmq.h:330
      ZMQ_BLOCKY                    : constant := 70;  --  zmq.h:331
      ZMQ_XPUB_MANUAL               : constant := 71;  --  zmq.h:332
      ZMQ_XPUB_WELCOME_MSG          : constant := 72;  --  zmq.h:333
      ZMQ_STREAM_NOTIFY             : constant := 73;  --  zmq.h:334
      ZMQ_INVERT_MATCHING           : constant := 74;  --  zmq.h:335
      ZMQ_HEARTBEAT_IVL             : constant := 75;  --  zmq.h:336
      ZMQ_HEARTBEAT_TTL             : constant := 76;  --  zmq.h:337
      ZMQ_HEARTBEAT_TIMEOUT         : constant := 77;  --  zmq.h:338
      ZMQ_XPUB_VERBOSER             : constant := 78;  --  zmq.h:339
      ZMQ_CONNECT_TIMEOUT           : constant := 79;  --  zmq.h:340
      ZMQ_TCP_MAXRT                 : constant := 80;  --  zmq.h:341
      ZMQ_THREAD_SAFE               : constant := 81;  --  zmq.h:342
      ZMQ_MULTICAST_MAXTPDU         : constant := 84;  --  zmq.h:343
      ZMQ_VMCI_BUFFER_SIZE          : constant := 85;  --  zmq.h:344
      ZMQ_VMCI_BUFFER_MIN_SIZE      : constant := 86;  --  zmq.h:345
      ZMQ_VMCI_BUFFER_MAX_SIZE      : constant := 87;  --  zmq.h:346
      ZMQ_VMCI_CONNECT_TIMEOUT      : constant := 88;  --  zmq.h:347
      ZMQ_USE_FD                    : constant := 89;  --  zmq.h:348
      ZMQ_GSSAPI_PRINCIPAL_NAMETYPE : constant := 90;  --  zmq.h:349

      ZMQ_BINDTODEVICE : constant := 92;  --  zmq.h:351

      ZMQ_MORE   : constant := 1;  --  zmq.h:354
      ZMQ_SHARED : constant := 3;  --  zmq.h:355

      ZMQ_DONTWAIT : constant := 1;  --  zmq.h:358
      ZMQ_SNDMORE  : constant := 2;  --  zmq.h:359

      ZMQ_NULL   : constant := 0;  --  zmq.h:362
      ZMQ_PLAIN  : constant := 1;  --  zmq.h:363
      ZMQ_CURVE  : constant := 2;  --  zmq.h:364
      ZMQ_GSSAPI : constant := 3;  --  zmq.h:365

      ZMQ_GROUP_MAX_LENGTH : constant := 255;  --  zmq.h:368
      ZMQ_IDENTITY         : constant := ZMQ_ROUTING_ID;
      ZMQ_CONNECT_RID      : constant := ZMQ_CONNECT_ROUTING_ID;

      ZMQ_TCP_ACCEPT_FILTER       : constant := 38;  --  zmq.h:373
      ZMQ_IPC_FILTER_PID          : constant := 58;  --  zmq.h:374
      ZMQ_IPC_FILTER_UID          : constant := 59;  --  zmq.h:375
      ZMQ_IPC_FILTER_GID          : constant := 60;  --  zmq.h:376
      ZMQ_IPV4ONLY                : constant := 31;  --  zmq.h:377
      ZMQ_DELAY_ATTACH_ON_CONNECT : constant := ZMQ_IMMEDIATE;
      ZMQ_NOBLOCK                 : constant := ZMQ_DONTWAIT;
      ZMQ_FAIL_UNROUTABLE         : constant := ZMQ_ROUTER_MANDATORY;
      ZMQ_ROUTER_BEHAVIOR         : constant := ZMQ_ROUTER_MANDATORY;

      ZMQ_SRCFD : constant := 2;  --  zmq.h:384

      ZMQ_GSSAPI_NT_HOSTBASED      : constant := 0;  --  zmq.h:391
      ZMQ_GSSAPI_NT_USER_NAME      : constant := 1;  --  zmq.h:392
      ZMQ_GSSAPI_NT_KRB5_PRINCIPAL : constant := 2;  --  zmq.h:393

      ZMQ_EVENT_CONNECTED       : constant := 16#0001#;  --  zmq.h:401
      ZMQ_EVENT_CONNECT_DELAYED : constant := 16#0002#;  --  zmq.h:402
      ZMQ_EVENT_CONNECT_RETRIED : constant := 16#0004#;  --  zmq.h:403
      ZMQ_EVENT_LISTENING       : constant := 16#0008#;  --  zmq.h:404
      ZMQ_EVENT_BIND_FAILED     : constant := 16#0010#;  --  zmq.h:405
      ZMQ_EVENT_ACCEPTED        : constant := 16#0020#;  --  zmq.h:406
      ZMQ_EVENT_ACCEPT_FAILED   : constant := 16#0040#;  --  zmq.h:407
      ZMQ_EVENT_CLOSED          : constant := 16#0080#;  --  zmq.h:408
      ZMQ_EVENT_CLOSE_FAILED    : constant := 16#0100#;  --  zmq.h:409
      ZMQ_EVENT_DISCONNECTED    : constant := 16#0200#;  --  zmq.h:410
      ZMQ_EVENT_MONITOR_STOPPED : constant := 16#0400#;  --  zmq.h:411
      ZMQ_EVENT_ALL             : constant := 16#FFFF#;  --  zmq.h:412

      ZMQ_EVENT_HANDSHAKE_SUCCEEDED : constant := 16#1000#;  --  zmq.h:417

      ZMQ_EVENT_HANDSHAKE_FAILED_AUTH : constant := 16#4000#;  --  zmq.h:423

      ZMQ_POLLIN  : constant := 1;  --  zmq.h:482
      ZMQ_POLLOUT : constant := 2;  --  zmq.h:483
      ZMQ_POLLERR : constant := 4;  --  zmq.h:484
      ZMQ_POLLPRI : constant := 8;  --  zmq.h:485

      ZMQ_POLLITEMS_DFLT : constant := 16;  --  zmq.h:495

      ZMQ_HAS_CAPABILITIES : constant := 1;  --  zmq.h:513

      ZMQ_STREAMER  : constant := 1;  --  zmq.h:517
      ZMQ_FORWARDER : constant := 2;  --  zmq.h:518
      ZMQ_QUEUE     : constant := 3;  --  zmq.h:519

      ZMQ_GSSAPI_SERVICE_PRINCIPAL_NAMETYPE : constant := 91;  --  zmq.h:350

      ZMQ_EVENT_HANDSHAKE_FAILED_NO_DETAIL : constant := 16#0800#;  --  zmq.h:414

      ZMQ_EVENT_HANDSHAKE_FAILED_PROTOCOL : constant := 16#2000#;  --  zmq.h:420

      ZMQ_PROTOCOL_ERROR_ZMTP_UNSPECIFIED                   : constant := 16#1000_0000#;  --  zmq.h:424
      ZMQ_PROTOCOL_ERROR_ZMTP_UNEXPECTED_COMMAND            : constant := 16#1000_0001#;  --  zmq.h:425
      ZMQ_PROTOCOL_ERROR_ZMTP_INVALID_SEQUENCE              : constant := 16#1000_0002#;  --  zmq.h:426
      ZMQ_PROTOCOL_ERROR_ZMTP_KEY_EXCHANGE                  : constant := 16#1000_0003#;  --  zmq.h:427
      ZMQ_PROTOCOL_ERROR_ZMTP_MALFORMED_COMMAND_UNSPECIFIED : constant := 16#1000_0011#;  --  zmq.h:428
      ZMQ_PROTOCOL_ERROR_ZMTP_MALFORMED_COMMAND_MESSAGE     : constant := 16#1000_0012#;  --  zmq.h:429
      ZMQ_PROTOCOL_ERROR_ZMTP_MALFORMED_COMMAND_HELLO       : constant := 16#1000_0013#;  --  zmq.h:430
      ZMQ_PROTOCOL_ERROR_ZMTP_MALFORMED_COMMAND_INITIATE    : constant := 16#1000_0014#;  --  zmq.h:431
      ZMQ_PROTOCOL_ERROR_ZMTP_MALFORMED_COMMAND_ERROR       : constant := 16#1000_0015#;  --  zmq.h:432
      ZMQ_PROTOCOL_ERROR_ZMTP_MALFORMED_COMMAND_READY       : constant := 16#1000_0016#;  --  zmq.h:433
      ZMQ_PROTOCOL_ERROR_ZMTP_MALFORMED_COMMAND_WELCOME     : constant := 16#1000_0017#;  --  zmq.h:434
      ZMQ_PROTOCOL_ERROR_ZMTP_INVALID_METADATA              : constant := 16#1000_0018#;  --  zmq.h:435

      ZMQ_PROTOCOL_ERROR_ZMTP_CRYPTOGRAPHIC      : constant := 16#1100_0001#;  --  zmq.h:437
      ZMQ_PROTOCOL_ERROR_ZMTP_MECHANISM_MISMATCH : constant := 16#1100_0002#;  --  zmq.h:438
      ZMQ_PROTOCOL_ERROR_ZAP_UNSPECIFIED         : constant := 16#2000_0000#;  --  zmq.h:439
      ZMQ_PROTOCOL_ERROR_ZAP_MALFORMED_REPLY     : constant := 16#2000_0001#;  --  zmq.h:440
      ZMQ_PROTOCOL_ERROR_ZAP_BAD_REQUEST_ID      : constant := 16#2000_0002#;  --  zmq.h:441
      ZMQ_PROTOCOL_ERROR_ZAP_BAD_VERSION         : constant := 16#2000_0003#;  --  zmq.h:442
      ZMQ_PROTOCOL_ERROR_ZAP_INVALID_STATUS_CODE : constant := 16#2000_0004#;  --  zmq.h:443
      ZMQ_PROTOCOL_ERROR_ZAP_INVALID_METADATA    : constant := 16#2000_0005#;  --  zmq.h:444
      ZMQ_PROTOCOL_ERROR_WS_UNSPECIFIED          : constant := 16#3000_0000#;  --  zmq.h:445

      -- SPDX-License-Identifier: MPL-2.0
      --  *************************************************************************
      --    NOTE to contributors. This file comprises the principal public contract
      --    for ZeroMQ API users. Any change to this file supplied in a stable
      --    release SHOULD not break existing applications.
      --    In practice this means that the value of constants must not change, and
      --    that old values may not be reused for new constants.
      --    *************************************************************************
      --

      --  Version macros for compile-time API version detection
      --  Handle DSO symbol visibility
      --  Define integer types needed for event interface
      -- needed for sigset_t definition in zmq_ppoll
      --  32-bit AIX's pollfd struct members are called reqevents and rtnevents so it
      --  defines compatibility macros for them. Need to include that header first to
      --  stop build failures since zmq_pollset_t defines them as events and revents.
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

   function zmq_errno
      return int  -- zmq.h:168
   with
     Import => True, Convention => C, External_Name => "zmq_errno";

   --  Resolves system errors and 0MQ errors to human-readable string.
   function zmq_strerror
     (errnum_u : int) return Interfaces.C.Strings
     .chars_ptr  -- zmq.h:171
   with
     Import => True, Convention => C, External_Name => "zmq_strerror";

   --  Run-time API version detection
   procedure zmq_version
     (major_u : access int;
      minor_u : access int;
      patch_u : access int)  -- zmq.h:174
   with
     Import => True, Convention => C, External_Name => "zmq_version";

   --  ****************************************************************************
   --  0MQ infrastructure (a.k.a. context) initialisation & termination.
   --  ****************************************************************************
   --  Context options
   --  Default for new contexts
   function zmq_ctx_new return System
     .Address  -- zmq.h:198
   with
     Import => True, Convention => C, External_Name => "zmq_ctx_new";

   function zmq_ctx_term
     (context_u : System.Address)
      return int  -- zmq.h:199
   with
     Import => True, Convention => C, External_Name => "zmq_ctx_term";

   function zmq_ctx_shutdown
     (context_u : System.Address)
      return int  -- zmq.h:200
   with
     Import => True, Convention => C, External_Name => "zmq_ctx_shutdown";

   function zmq_ctx_set
     (context_u : System.Address;
      option_u  : int;
      optval_u  : int)
      return int  -- zmq.h:201
   with
     Import => True, Convention => C, External_Name => "zmq_ctx_set";

   function zmq_ctx_get
     (context_u : System.Address;
      option_u  : int)
      return int  -- zmq.h:202
   with
     Import => True, Convention => C, External_Name => "zmq_ctx_get";

   --  Old (legacy) API
   function zmq_init
     (io_threads_u : int) return System
     .Address  -- zmq.h:205
   with
     Import => True, Convention => C, External_Name => "zmq_init";

   function zmq_term
     (context_u : System.Address)
      return int  -- zmq.h:206
   with
     Import => True, Convention => C, External_Name => "zmq_term";

   function zmq_ctx_destroy
     (context_u : System.Address)
      return int  -- zmq.h:207
   with
     Import => True, Convention => C, External_Name => "zmq_ctx_destroy";

     --  ****************************************************************************
     --  0MQ message definition.
     --  ****************************************************************************
     -- Some architectures, like sparc64 and some variants of aarch64, enforce pointer
     -- * alignment and raise sigbus on violations. Make sure applications allocate
     -- * zmq_msg_t on addresses aligned on a pointer-size boundary to avoid this issue.
     --

   type anon_array2034 is array (0 .. 63) of aliased unsigned_char;
   type zmq_msg_t is record
      u_u : aliased anon_array2034;  -- zmq.h:228
   end record with
     Convention => C_Pass_By_Copy;  -- zmq.h:218

   --  skipped function type zmq_free_fn

   function zmq_msg_init
     (msg_u : access zmq_msg_t)
      return int  -- zmq.h:236
   with
     Import => True, Convention => C, External_Name => "zmq_msg_init";

   function zmq_msg_init_size
     (msg_u  : access zmq_msg_t;
      size_u : size_t)
      return int  -- zmq.h:237
   with
     Import => True, Convention => C, External_Name => "zmq_msg_init_size";

   function zmq_msg_init_data
     (msg_u  : access zmq_msg_t; data_u : System.Address; size_u : size_t;
      ffn_u  : access procedure (arg1 : System.Address; arg2 : System.Address);
      hint_u : System.Address)
      return int  -- zmq.h:238
   with
     Import => True, Convention => C, External_Name => "zmq_msg_init_data";

   function zmq_msg_send
     (msg_u   : access zmq_msg_t;
      s_u     : System.Address;
      flags_u : int)
      return int  -- zmq.h:240
   with
     Import => True, Convention => C, External_Name => "zmq_msg_send";

   function zmq_msg_recv
     (msg_u   : access zmq_msg_t;
      s_u     : System.Address;
      flags_u : int)
      return int  -- zmq.h:241
   with
     Import => True, Convention => C, External_Name => "zmq_msg_recv";

   function zmq_msg_close
     (msg_u : access zmq_msg_t)
      return int  -- zmq.h:242
   with
     Import => True, Convention => C, External_Name => "zmq_msg_close";

   function zmq_msg_move
     (dest_u : access zmq_msg_t;
      src_u  : access zmq_msg_t)
      return int  -- zmq.h:243
   with
     Import => True, Convention => C, External_Name => "zmq_msg_move";

   function zmq_msg_copy
     (dest_u : access zmq_msg_t;
      src_u  : access zmq_msg_t)
      return int  -- zmq.h:244
   with
     Import => True, Convention => C, External_Name => "zmq_msg_copy";

   function zmq_msg_data
     (msg_u : access zmq_msg_t) return System
     .Address  -- zmq.h:245
   with
     Import => True, Convention => C, External_Name => "zmq_msg_data";

   function zmq_msg_size
     (msg_u : access constant zmq_msg_t)
      return size_t  -- zmq.h:246
   with
     Import => True, Convention => C, External_Name => "zmq_msg_size";

   function zmq_msg_more
     (msg_u : access constant zmq_msg_t)
      return int  -- zmq.h:247
   with
     Import => True, Convention => C, External_Name => "zmq_msg_more";

   function zmq_msg_get
     (msg_u      : access constant zmq_msg_t;
      property_u : int)
      return int  -- zmq.h:248
   with
     Import => True, Convention => C, External_Name => "zmq_msg_get";

   function zmq_msg_set
     (msg_u      : access zmq_msg_t;
      property_u : int;
      optval_u   : int)
      return int  -- zmq.h:249
   with
     Import => True, Convention => C, External_Name => "zmq_msg_set";

   function zmq_msg_gets
     (msg_u : access constant zmq_msg_t; property_u : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings
     .chars_ptr  -- zmq.h:250
   with
     Import => True, Convention => C, External_Name => "zmq_msg_gets";

   --  ****************************************************************************
   --  0MQ socket definition.
   --  ****************************************************************************
   --  Socket types.
   --  Deprecated aliases
   --  Socket options.
   --  Message options
   --  Send/recv options.
   --  Security mechanisms
   --  RADIO-DISH protocol
   --  Deprecated options and aliases
   --  Deprecated Message options
   --  ****************************************************************************
   --  GSSAPI definitions
   --  ****************************************************************************
   --  GSSAPI principal name types
   --  ****************************************************************************
   --  0MQ socket events and monitoring
   --  ****************************************************************************
   --  Socket transport events (TCP, IPC and TIPC only)
   --  Unspecified system errors during handshake. Event value is an errno.
   --  Handshake complete successfully with successful authentication (if        *
   -- *  enabled). Event value is unused.

   --  Protocol errors between ZMTP peers or between server and ZAP handler.     *
   -- *  Event value is one of ZMQ_PROTOCOL_ERROR_*

   --  Failed authentication requests. Event value is the numeric ZAP status     *
   -- *  code, i.e. 300, 400 or 500.

   -- the following two may be due to erroneous configuration of a peer
   function zmq_socket
     (arg1 : System.Address; type_u : int) return System
     .Address  -- zmq.h:447
   with
     Import => True, Convention => C, External_Name => "zmq_socket";

   function zmq_close
     (s_u : System.Address)
      return int  -- zmq.h:448
   with
     Import => True, Convention => C, External_Name => "zmq_close";

   function zmq_setsockopt
     (s_u         : System.Address;
      option_u    : int;
      optval_u    : System.Address;
      optvallen_u : size_t)
      return int  -- zmq.h:450
   with
     Import => True, Convention => C, External_Name => "zmq_setsockopt";

   function zmq_getsockopt
     (s_u         : System.Address;
      option_u    : int;
      optval_u    : System.Address;
      optvallen_u : access size_t)
      return int  -- zmq.h:452
   with
     Import => True, Convention => C, External_Name => "zmq_getsockopt";

   function zmq_bind
     (s_u    : System.Address;
      addr_u : Interfaces.C.Strings.chars_ptr)
      return int  -- zmq.h:453
   with
     Import => True, Convention => C, External_Name => "zmq_bind";

   function zmq_connect
     (s_u    : System.Address;
      addr_u : Interfaces.C.Strings.chars_ptr)
      return int  -- zmq.h:454
   with
     Import => True, Convention => C, External_Name => "zmq_connect";

   function zmq_unbind
     (s_u    : System.Address;
      addr_u : Interfaces.C.Strings.chars_ptr)
      return int  -- zmq.h:455
   with
     Import => True, Convention => C, External_Name => "zmq_unbind";

   function zmq_disconnect
     (s_u    : System.Address;
      addr_u : Interfaces.C.Strings.chars_ptr)
      return int  -- zmq.h:456
   with
     Import => True, Convention => C, External_Name => "zmq_disconnect";

   function zmq_send
     (s_u     : System.Address;
      buf_u   : System.Address;
      len_u   : size_t;
      flags_u : int)
      return int  -- zmq.h:457
   with
     Import => True, Convention => C, External_Name => "zmq_send";

   function zmq_send_const
     (s_u     : System.Address;
      buf_u   : System.Address;
      len_u   : size_t;
      flags_u : int)
      return int  -- zmq.h:459
   with
     Import => True, Convention => C, External_Name => "zmq_send_const";

   function zmq_recv
     (s_u     : System.Address;
      buf_u   : System.Address;
      len_u   : size_t;
      flags_u : int)
      return int  -- zmq.h:460
   with
     Import => True, Convention => C, External_Name => "zmq_recv";

   function zmq_socket_monitor
     (s_u      : System.Address;
      addr_u   : Interfaces.C.Strings.chars_ptr;
      events_u : int)
      return int  -- zmq.h:461
   with
     Import => True, Convention => C, External_Name => "zmq_socket_monitor";

     --  ****************************************************************************
     --  Hide socket fd type; this was before zmq_poller_event_t typedef below
     --  ****************************************************************************
     -- Windows uses a pointer-sized unsigned integer to store the socket fd.
   subtype zmq_fd_t is int;  -- zmq.h:475

   --  ****************************************************************************
   --  Deprecated I/O multiplexing. Prefer using zmq_poller API
   --  ****************************************************************************
   type zmq_pollitem_t is record
      socket  : System.Address;  -- zmq.h:489
      fd      : aliased zmq_fd_t;  -- zmq.h:490
      events  : aliased short;  -- zmq.h:491
      revents : aliased short;  -- zmq.h:492
   end record with
     Convention => C_Pass_By_Copy;  -- zmq.h:487

   function zmq_poll
     (items_u   : access zmq_pollitem_t;
      nitems_u  : int;
      timeout_u : long)
      return int  -- zmq.h:497
   with
     Import => True, Convention => C, External_Name => "zmq_poll";

   --  ****************************************************************************
   --  Message proxying
   --  ****************************************************************************
   function zmq_proxy
     (frontend_u : System.Address;
      backend_u  : System.Address;
      capture_u  : System.Address)
      return int  -- zmq.h:503
   with
     Import => True, Convention => C, External_Name => "zmq_proxy";

   function zmq_proxy_steerable
     (frontend_u : System.Address; backend_u : System.Address; capture_u : System.Address; control_u : System.Address)
      return int  -- zmq.h:504
   with
     Import => True, Convention => C, External_Name => "zmq_proxy_steerable";

   --  ****************************************************************************
   --  Probe library capabilities
   --  ****************************************************************************
   function zmq_has
     (capability_u : Interfaces.C.Strings.chars_ptr)
      return int  -- zmq.h:514
   with
     Import => True, Convention => C, External_Name => "zmq_has";

   --  Deprecated aliases
   --  Deprecated methods
   function zmq_device
     (type_u     : int;
      frontend_u : System.Address;
      backend_u  : System.Address)
      return int  -- zmq.h:522
   with
     Import => True, Convention => C, External_Name => "zmq_device";

   function zmq_sendmsg
     (s_u     : System.Address;
      msg_u   : access zmq_msg_t;
      flags_u : int)
      return int  -- zmq.h:523
   with
     Import => True, Convention => C, External_Name => "zmq_sendmsg";

   function zmq_recvmsg
     (s_u     : System.Address;
      msg_u   : access zmq_msg_t;
      flags_u : int)
      return int  -- zmq.h:524
   with
     Import => True, Convention => C, External_Name => "zmq_recvmsg";

   type iovec is null record;   -- incomplete struct

   function zmq_sendiov
     (s_u     : System.Address;
      iov_u   : access iovec;
      count_u : size_t;
      flags_u : int)
      return int  -- zmq.h:527
   with
     Import => True, Convention => C, External_Name => "zmq_sendiov";

   function zmq_recviov
     (s_u     : System.Address;
      iov_u   : access iovec;
      count_u : access size_t;
      flags_u : int)
      return int  -- zmq.h:529
   with
     Import => True, Convention => C, External_Name => "zmq_recviov";

   --  ****************************************************************************
   --  Encryption functions
   --  ****************************************************************************
   --  Encode data with Z85 encoding. Returns encoded data
   function zmq_z85_encode
     (dest_u : Interfaces.C.Strings.chars_ptr; data_u : access Interfaces.Unsigned_8; size_u : size_t)
      return Interfaces.C.Strings
     .chars_ptr  -- zmq.h:537
   with
     Import => True, Convention => C, External_Name => "zmq_z85_encode";

   --  Decode data with Z85 encoding. Returns decoded data
   function zmq_z85_decode
     (dest_u : access Interfaces.Unsigned_8; string_u : Interfaces.C.Strings
        .chars_ptr) return access Interfaces
     .Unsigned_8  -- zmq.h:540
   with
     Import => True, Convention => C, External_Name => "zmq_z85_decode";

   --  Generate z85-encoded public and private keypair with libsodium.
   --  Returns 0 on success.
   function zmq_curve_keypair
     (z85_public_key_u : Interfaces.C.Strings.chars_ptr;
      z85_secret_key_u : Interfaces.C.Strings.chars_ptr)
      return int  -- zmq.h:544
   with
     Import => True, Convention => C, External_Name => "zmq_curve_keypair";

   --  Derive the z85-encoded public key from the z85-encoded secret key.
   --  Returns 0 on success.
   function zmq_curve_public
     (z85_public_key_u : Interfaces.C.Strings.chars_ptr;
      z85_secret_key_u : Interfaces.C.Strings.chars_ptr)
      return int  -- zmq.h:548
   with
     Import => True, Convention => C, External_Name => "zmq_curve_public";

   --  ****************************************************************************
   --  Atomic utility methods
   --  ****************************************************************************
   function zmq_atomic_counter_new return System
     .Address  -- zmq.h:555
   with
     Import => True, Convention => C, External_Name => "zmq_atomic_counter_new";

   procedure zmq_atomic_counter_set
     (counter_u : System.Address;
      value_u   : int)  -- zmq.h:556
   with
     Import => True, Convention => C, External_Name => "zmq_atomic_counter_set";

   function zmq_atomic_counter_inc
     (counter_u : System.Address)
      return int  -- zmq.h:557
   with
     Import => True, Convention => C, External_Name => "zmq_atomic_counter_inc";

   function zmq_atomic_counter_dec
     (counter_u : System.Address)
      return int  -- zmq.h:558
   with
     Import => True, Convention => C, External_Name => "zmq_atomic_counter_dec";

   function zmq_atomic_counter_value
     (counter_u : System.Address)
      return int  -- zmq.h:559
   with
     Import => True, Convention => C, External_Name => "zmq_atomic_counter_value";

   procedure zmq_atomic_counter_destroy
     (counter_p_u : System
        .Address)  -- zmq.h:560
   with
     Import => True, Convention => C, External_Name => "zmq_atomic_counter_destroy";

   --  ****************************************************************************
   --  Scheduling timers
   --  ****************************************************************************
   --  skipped function type zmq_timer_fn

   function zmq_timers_new return System
     .Address  -- zmq.h:570
   with
     Import => True, Convention => C, External_Name => "zmq_timers_new";

   function zmq_timers_destroy
     (timers_p : System.Address)
      return int  -- zmq.h:571
   with
     Import => True, Convention => C, External_Name => "zmq_timers_destroy";

   function zmq_timers_add
     (timers : System.Address; interval : size_t; handler : access procedure (arg1 : int; arg2 : System.Address);
      arg    : System.Address)
      return int  -- zmq.h:573
   with
     Import => True, Convention => C, External_Name => "zmq_timers_add";

   function zmq_timers_cancel
     (timers   : System.Address;
      timer_id : int)
      return int  -- zmq.h:574
   with
     Import => True, Convention => C, External_Name => "zmq_timers_cancel";

   function zmq_timers_set_interval
     (timers   : System.Address;
      timer_id : int;
      interval : size_t)
      return int  -- zmq.h:576
   with
     Import => True, Convention => C, External_Name => "zmq_timers_set_interval";

   function zmq_timers_reset
     (timers   : System.Address;
      timer_id : int)
      return int  -- zmq.h:577
   with
     Import => True, Convention => C, External_Name => "zmq_timers_reset";

   function zmq_timers_timeout
     (timers : System.Address)
      return long  -- zmq.h:578
   with
     Import => True, Convention => C, External_Name => "zmq_timers_timeout";

   function zmq_timers_execute
     (timers : System.Address)
      return int  -- zmq.h:579
   with
     Import => True, Convention => C, External_Name => "zmq_timers_execute";

   --  ****************************************************************************
   --  These functions are not documented by man pages -- use at your own risk.
   --  If you need these to be part of the formal ZMQ API, then (a) write a man
   --  page, and (b) write a test case in tests.
   --  ****************************************************************************
   --  Helper functions are used by perf tests so that they don't have to care
   --  about minutiae of time-related functions on different OS platforms.
   --  Starts the stopwatch. Returns the handle to the watch.
   function zmq_stopwatch_start return System
     .Address  -- zmq.h:592
   with
     Import => True, Convention => C, External_Name => "zmq_stopwatch_start";

   --  Returns the number of microseconds elapsed since the stopwatch was
   --  started, but does not stop or deallocate the stopwatch.
   function zmq_stopwatch_intermediate
     (watch_u : System.Address)
      return unsigned_long  -- zmq.h:596
   with
     Import => True, Convention => C, External_Name => "zmq_stopwatch_intermediate";

   --  Stops the stopwatch. Returns the number of microseconds elapsed since
   --  the stopwatch was started, and deallocates that watch.
   function zmq_stopwatch_stop
     (watch_u : System.Address)
      return unsigned_long  -- zmq.h:600
   with
     Import => True, Convention => C, External_Name => "zmq_stopwatch_stop";

   --  Sleeps for specified number of seconds.
   procedure zmq_sleep
     (seconds_u : int)  -- zmq.h:603
   with
     Import => True, Convention => C, External_Name => "zmq_sleep";

   --  skipped function type zmq_thread_fn

   -- Start a thread. Returns a handle to the thread.
   function zmq_threadstart
     (func_u : access procedure (arg1 : System.Address); arg_u : System.Address) return System
     .Address  -- zmq.h:608
   with
     Import => True, Convention => C, External_Name => "zmq_threadstart";

   -- Wait for thread to complete then free up resources.
   procedure zmq_threadclose
     (thread_u : System
        .Address)  -- zmq.h:611
   with
     Import => True, Convention => C, External_Name => "zmq_threadclose";

   --  ****************************************************************************
   --  These functions are DRAFT and disabled in stable releases, and subject to
   --  change at ANY time until declared stable.
   --  ****************************************************************************
   --  DRAFT Socket types.
   --  DRAFT Socket options.
   --  DRAFT ZMQ_NORM_MODE options
   --  DRAFT ZMQ_RECONNECT_STOP options
   --  DRAFT Context options
   --  DRAFT Context methods.
   --  DRAFT Socket methods.
   --  DRAFT Msg methods.
   --  DRAFT Msg property names.
   --  Router notify options
   --  ****************************************************************************
   --  Poller polling on sockets,fd and thread-safe sockets
   --  ****************************************************************************
   --  DRAFT Socket monitoring events
   -- Windows has no sigset_t
end ZMQ.Low_Level;

pragma Style_Checks (On);
pragma Warnings (On, "-gnatwu");
