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
      ZMQ_VERSION_MINOR : constant := 1;  --  zmq.h:33
      ZMQ_VERSION_PATCH : constant := 5;  --  zmq.h:34
      --  arg-macro: function ZMQ_MAKE_VERSION ((major) * 10000 + (minor) * 100 + (patch)
      --    return (major) * 10000 + (minor) * 100 + (patch);
      --  unsupported macro: ZMQ_VERSION ZMQ_MAKE_VERSION(ZMQ_VERSION_MAJOR, ZMQ_VERSION_MINOR, ZMQ_VERSION_PATCH)

      ZMQ_DEFINED_STDINT : constant := 1;  --  zmq.h:74

      ZMQ_HAUSNUMERO : constant := 156384712;  --  zmq.h:98
      EFSM : constant := ZMQ_HAUSNUMERO + 51;
      ENOCOMPATPROTO : constant := ZMQ_HAUSNUMERO + 52;
      ETERM : constant := ZMQ_HAUSNUMERO + 53;
      EMTHREAD : constant := ZMQ_HAUSNUMERO + 54;

      ZMQ_IO_THREADS          : constant := 1;  --  zmq.h:180
      ZMQ_MAX_SOCKETS         : constant := 2;  --  zmq.h:181
      ZMQ_SOCKET_LIMIT        : constant := 3;  --  zmq.h:182
      ZMQ_THREAD_PRIORITY     : constant := 3;  --  zmq.h:183
      ZMQ_THREAD_SCHED_POLICY : constant := 4;  --  zmq.h:184

      ZMQ_IO_THREADS_DFLT     : constant := 1;  --  zmq.h:187
      ZMQ_MAX_SOCKETS_DFLT    : constant := 1023;  --  zmq.h:188
      ZMQ_THREAD_PRIORITY_DFLT : constant := -1;  --  zmq.h:189
      ZMQ_THREAD_SCHED_POLICY_DFLT : constant := -1;  --  zmq.h:190

      ZMQ_PAIR                : constant := 0;  --  zmq.h:234
      ZMQ_PUB                 : constant := 1;  --  zmq.h:235
      ZMQ_SUB                 : constant := 2;  --  zmq.h:236
      ZMQ_REQ                 : constant := 3;  --  zmq.h:237
      ZMQ_REP                 : constant := 4;  --  zmq.h:238
      ZMQ_DEALER              : constant := 5;  --  zmq.h:239
      ZMQ_ROUTER              : constant := 6;  --  zmq.h:240
      ZMQ_PULL                : constant := 7;  --  zmq.h:241
      ZMQ_PUSH                : constant := 8;  --  zmq.h:242
      ZMQ_XPUB                : constant := 9;  --  zmq.h:243
      ZMQ_XSUB                : constant := 10;  --  zmq.h:244
      ZMQ_STREAM              : constant := 11;  --  zmq.h:245
      ZMQ_XREQ                : constant := ZMQ_DEALER;
      ZMQ_XREP                : constant := ZMQ_ROUTER;

      ZMQ_AFFINITY                 : constant := 4;  --  zmq.h:252
      ZMQ_IDENTITY                 : constant := 5;  --  zmq.h:253
      ZMQ_SUBSCRIBE                : constant := 6;  --  zmq.h:254
      ZMQ_UNSUBSCRIBE              : constant := 7;  --  zmq.h:255
      ZMQ_RATE                     : constant := 8;  --  zmq.h:256
      ZMQ_RECOVERY_IVL             : constant := 9;  --  zmq.h:257
      ZMQ_SNDBUF                   : constant := 11;  --  zmq.h:258
      ZMQ_RCVBUF                   : constant := 12;  --  zmq.h:259
      ZMQ_RCVMORE                  : constant := 13;  --  zmq.h:260
      ZMQ_FD                       : constant := 14;  --  zmq.h:261
      ZMQ_EVENTS                   : constant := 15;  --  zmq.h:262
      ZMQ_TYPE                     : constant := 16;  --  zmq.h:263
      ZMQ_LINGER                   : constant := 17;  --  zmq.h:264
      ZMQ_RECONNECT_IVL            : constant := 18;  --  zmq.h:265
      ZMQ_BACKLOG                  : constant := 19;  --  zmq.h:266
      ZMQ_RECONNECT_IVL_MAX        : constant := 21;  --  zmq.h:267
      ZMQ_MAXMSGSIZE               : constant := 22;  --  zmq.h:268
      ZMQ_SNDHWM                   : constant := 23;  --  zmq.h:269
      ZMQ_RCVHWM                   : constant := 24;  --  zmq.h:270
      ZMQ_MULTICAST_HOPS           : constant := 25;  --  zmq.h:271
      ZMQ_RCVTIMEO                 : constant := 27;  --  zmq.h:272
      ZMQ_SNDTIMEO                 : constant := 28;  --  zmq.h:273
      ZMQ_LAST_ENDPOINT            : constant := 32;  --  zmq.h:274
      ZMQ_ROUTER_MANDATORY         : constant := 33;  --  zmq.h:275
      ZMQ_TCP_KEEPALIVE            : constant := 34;  --  zmq.h:276
      ZMQ_TCP_KEEPALIVE_CNT        : constant := 35;  --  zmq.h:277
      ZMQ_TCP_KEEPALIVE_IDLE       : constant := 36;  --  zmq.h:278
      ZMQ_TCP_KEEPALIVE_INTVL      : constant := 37;  --  zmq.h:279
      ZMQ_IMMEDIATE                : constant := 39;  --  zmq.h:280
      ZMQ_XPUB_VERBOSE             : constant := 40;  --  zmq.h:281
      ZMQ_ROUTER_RAW               : constant := 41;  --  zmq.h:282
      ZMQ_IPV6                     : constant := 42;  --  zmq.h:283
      ZMQ_MECHANISM                : constant := 43;  --  zmq.h:284
      ZMQ_PLAIN_SERVER             : constant := 44;  --  zmq.h:285
      ZMQ_PLAIN_USERNAME           : constant := 45;  --  zmq.h:286
      ZMQ_PLAIN_PASSWORD           : constant := 46;  --  zmq.h:287
      ZMQ_CURVE_SERVER             : constant := 47;  --  zmq.h:288
      ZMQ_CURVE_PUBLICKEY          : constant := 48;  --  zmq.h:289
      ZMQ_CURVE_SECRETKEY          : constant := 49;  --  zmq.h:290
      ZMQ_CURVE_SERVERKEY          : constant := 50;  --  zmq.h:291
      ZMQ_PROBE_ROUTER             : constant := 51;  --  zmq.h:292
      ZMQ_REQ_CORRELATE            : constant := 52;  --  zmq.h:293
      ZMQ_REQ_RELAXED              : constant := 53;  --  zmq.h:294
      ZMQ_CONFLATE                 : constant := 54;  --  zmq.h:295
      ZMQ_ZAP_DOMAIN               : constant := 55;  --  zmq.h:296
      ZMQ_ROUTER_HANDOVER          : constant := 56;  --  zmq.h:297
      ZMQ_TOS                      : constant := 57;  --  zmq.h:298
      ZMQ_CONNECT_RID              : constant := 61;  --  zmq.h:299
      ZMQ_GSSAPI_SERVER            : constant := 62;  --  zmq.h:300
      ZMQ_GSSAPI_PRINCIPAL         : constant := 63;  --  zmq.h:301
      ZMQ_GSSAPI_SERVICE_PRINCIPAL : constant := 64;  --  zmq.h:302
      ZMQ_GSSAPI_PLAINTEXT         : constant := 65;  --  zmq.h:303
      ZMQ_HANDSHAKE_IVL            : constant := 66;  --  zmq.h:304
      ZMQ_SOCKS_PROXY              : constant := 68;  --  zmq.h:305
      ZMQ_XPUB_NODROP              : constant := 69;  --  zmq.h:306

      ZMQ_MORE                     : constant := 1;  --  zmq.h:309
      ZMQ_SRCFD                    : constant := 2;  --  zmq.h:310
      ZMQ_SHARED                   : constant := 3;  --  zmq.h:311

      ZMQ_DONTWAIT                 : constant := 1;  --  zmq.h:314
      ZMQ_SNDMORE                  : constant := 2;  --  zmq.h:315

      ZMQ_NULL                     : constant := 0;  --  zmq.h:318
      ZMQ_PLAIN                    : constant := 1;  --  zmq.h:319
      ZMQ_CURVE                    : constant := 2;  --  zmq.h:320
      ZMQ_GSSAPI                   : constant := 3;  --  zmq.h:321

      ZMQ_TCP_ACCEPT_FILTER        : constant := 38;  --  zmq.h:324
      ZMQ_IPC_FILTER_PID           : constant := 58;  --  zmq.h:325
      ZMQ_IPC_FILTER_UID           : constant := 59;  --  zmq.h:326
      ZMQ_IPC_FILTER_GID           : constant := 60;  --  zmq.h:327
      ZMQ_IPV4ONLY                 : constant := 31;  --  zmq.h:328
      ZMQ_DELAY_ATTACH_ON_CONNECT  : constant := ZMQ_IMMEDIATE;
      ZMQ_NOBLOCK                  : constant := ZMQ_DONTWAIT;
      ZMQ_FAIL_UNROUTABLE          : constant := ZMQ_ROUTER_MANDATORY;
      ZMQ_ROUTER_BEHAVIOR          : constant := ZMQ_ROUTER_MANDATORY;

      ZMQ_EVENT_CONNECTED       : constant := 16#0001#;  --  zmq.h:340
      ZMQ_EVENT_CONNECT_DELAYED : constant := 16#0002#;  --  zmq.h:341
      ZMQ_EVENT_CONNECT_RETRIED : constant := 16#0004#;  --  zmq.h:342
      ZMQ_EVENT_LISTENING       : constant := 16#0008#;  --  zmq.h:343
      ZMQ_EVENT_BIND_FAILED     : constant := 16#0010#;  --  zmq.h:344
      ZMQ_EVENT_ACCEPTED        : constant := 16#0020#;  --  zmq.h:345
      ZMQ_EVENT_ACCEPT_FAILED   : constant := 16#0040#;  --  zmq.h:346
      ZMQ_EVENT_CLOSED          : constant := 16#0080#;  --  zmq.h:347
      ZMQ_EVENT_CLOSE_FAILED    : constant := 16#0100#;  --  zmq.h:348
      ZMQ_EVENT_DISCONNECTED    : constant := 16#0200#;  --  zmq.h:349
      ZMQ_EVENT_MONITOR_STOPPED : constant := 16#0400#;  --  zmq.h:350
      ZMQ_EVENT_ALL             : constant := 16#FFFF#;  --  zmq.h:351

      ZMQ_POLLIN                : constant := 1;  --  zmq.h:373
      ZMQ_POLLOUT               : constant := 2;  --  zmq.h:374
      ZMQ_POLLERR               : constant := 4;  --  zmq.h:375

      ZMQ_POLLITEMS_DFLT        : constant := 16;  --  zmq.h:389

      ZMQ_HAS_CAPABILITIES      : constant := 1;  --  zmq.h:404

      ZMQ_STREAMER              : constant := 1;  --  zmq.h:408
      ZMQ_FORWARDER             : constant := 2;  --  zmq.h:409
      ZMQ_QUEUE                 : constant := 3;  --  zmq.h:410

      function Zmq_Errno return Int;  -- zmq.h:166
      pragma Import (C, Zmq_Errno, "zmq_errno");

      function Zmq_Strerror (Errnum : Int) return Interfaces.C.Strings.Chars_Ptr;  -- zmq.h:169
      pragma Import (C, Zmq_Strerror, "zmq_strerror");

   end Defs;

   procedure Zmq_Version
     (Major : access Int;
      Minor : access Int;
      Patch : access Int);  -- zmq.h:172
   pragma Import (C, Zmq_Version, "zmq_version");

   function Zmq_Ctx_New return System.Address;  -- zmq.h:192
   pragma Import (C, Zmq_Ctx_New, "zmq_ctx_new");

   function Zmq_Ctx_Term (Context : System.Address) return Int;  -- zmq.h:193
   pragma Obsolescent;
   pragma Import (C, Zmq_Ctx_Term, "zmq_ctx_term");

   function Zmq_Ctx_Shutdown (Ctx_U : System.Address) return Int;  -- zmq.h:194
   pragma Obsolescent;
   pragma Import (C, Zmq_Ctx_Shutdown, "zmq_ctx_shutdown");

   function Zmq_Ctx_Set
     (Context : System.Address;
      Option  : Int;
      Optval  : Int) return Int;  -- zmq.h:195
   pragma Import (C, Zmq_Ctx_Set, "zmq_ctx_set");

   function Zmq_Ctx_Get (Context : System.Address; Option : Int) return Int;  -- zmq.h:196
   pragma Import (C, Zmq_Ctx_Get, "zmq_ctx_get");

   function Zmq_Init (Io_Threads : Int) return System.Address;  -- zmq.h:199
   pragma Import (C, Zmq_Init, "zmq_init");

   function Zmq_Term (Context : System.Address) return Int;  -- zmq.h:200
   pragma Obsolescent;
   pragma Import (C, Zmq_Term, "zmq_term");

   function Zmq_Ctx_Destroy (Context : System.Address) return Int;  -- zmq.h:201
   pragma Import (C, Zmq_Ctx_Destroy, "zmq_ctx_destroy");

   type Zmq_Msg_T_U_U_Array is array (0 .. 63) of aliased Unsigned_Char;
   type Zmq_Msg_T is record
      U_U : aliased Zmq_Msg_T_U_U_Array;  -- zmq.h:208
   end record;
   pragma Convention (C_Pass_By_Copy, Zmq_Msg_T);  -- zmq.h:208

   --  skipped function type zmq_free_fn

   function Zmq_Msg_Init (Msg : access Zmq_Msg_T) return Int;  -- zmq.h:212
   pragma Import (C, Zmq_Msg_Init, "zmq_msg_init");

   function Zmq_Msg_Init_Size (Msg : access Zmq_Msg_T; Size : size_t) return Int;  -- zmq.h:213
   pragma Import (C, Zmq_Msg_Init_Size, "zmq_msg_init_size");

   function Zmq_Msg_Init_Data
     (Msg  : access Zmq_Msg_T;
      Data : System.Address;
      Size : size_t;
      Ffn  : access procedure (Arg1 : System.Address; Arg2 : System.Address);
      Hint : System.Address) return Int;  -- zmq.h:214
   pragma Import (C, Zmq_Msg_Init_Data, "zmq_msg_init_data");

   function Zmq_Msg_Send
     (Msg   : access Zmq_Msg_T;
      S     : System.Address;
      Flags : Int) return Int;  -- zmq.h:216
   pragma Import (C, Zmq_Msg_Send, "zmq_msg_send");

   function Zmq_Msg_Recv
     (Msg   : access Zmq_Msg_T;
      S     : System.Address;
      Flags : Int) return Int;  -- zmq.h:217
   pragma Import (C, Zmq_Msg_Recv, "zmq_msg_recv");

   function Zmq_Msg_Close (Msg : access Zmq_Msg_T) return Int;  -- zmq.h:218
   pragma Import (C, Zmq_Msg_Close, "zmq_msg_close");

   function Zmq_Msg_Move (Dest : access Zmq_Msg_T; Src : access Zmq_Msg_T) return Int;  -- zmq.h:219
   pragma Import (C, Zmq_Msg_Move, "zmq_msg_move");

   function Zmq_Msg_Copy (Dest : access Zmq_Msg_T; Src : access Zmq_Msg_T) return Int;  -- zmq.h:220
   pragma Import (C, Zmq_Msg_Copy, "zmq_msg_copy");

   function Zmq_Msg_Data (Msg : access Zmq_Msg_T) return System.Address;  -- zmq.h:221
   pragma Import (C, Zmq_Msg_Data, "zmq_msg_data");

   function Zmq_Msg_Size (Msg : access Zmq_Msg_T) return size_t;  -- zmq.h:222
   pragma Import (C, Zmq_Msg_Size, "zmq_msg_size");

   function Zmq_Msg_More (Msg : access Zmq_Msg_T) return Int;  -- zmq.h:223
   pragma Import (C, Zmq_Msg_More, "zmq_msg_more");

   function Zmq_Msg_Get (Msg : access Zmq_Msg_T; Property : Int) return Int;  -- zmq.h:224
   pragma Import (C, Zmq_Msg_Get, "zmq_msg_get");

   function Zmq_Msg_Set
     (Msg      : access Zmq_Msg_T;
      Property : Int;
      Optval   : Int) return Int;  -- zmq.h:225
   pragma Import (C, Zmq_Msg_Set, "zmq_msg_set");

   function Zmq_Msg_Gets (Msg : access Zmq_Msg_T; Property : Interfaces.C.Strings.Chars_Ptr) return Interfaces.C.Strings.Chars_Ptr;  -- zmq.h:226
   pragma Import (C, Zmq_Msg_Gets, "zmq_msg_gets");

   function Zmq_Socket (Arg1 : System.Address; C_Type : Int) return System.Address;  -- zmq.h:353
   pragma Import (C, Zmq_Socket, "zmq_socket");

   function Zmq_Close (S : System.Address) return Int;  -- zmq.h:354
   pragma Import (C, Zmq_Close, "zmq_close");

   function Zmq_Setsockopt
     (S         : System.Address;
      Option    : Int;
      Optval    : System.Address;
      Optvallen : size_t) return Int;  -- zmq.h:355
   pragma Import (C, Zmq_Setsockopt, "zmq_setsockopt");

   function Zmq_Getsockopt
     (S         : System.Address;
      Option    : Int;
      Optval    : System.Address;
      Optvallen : access size_t) return Int;  -- zmq.h:357
   pragma Import (C, Zmq_Getsockopt, "zmq_getsockopt");

   function Zmq_Bind (S : System.Address; Addr : Interfaces.C.Strings.Chars_Ptr) return Int;  -- zmq.h:359
   pragma Import (C, Zmq_Bind, "zmq_bind");

   function Zmq_Connect (S : System.Address; Addr : Interfaces.C.Strings.Chars_Ptr) return Int;  -- zmq.h:360
   pragma Import (C, Zmq_Connect, "zmq_connect");

   function Zmq_Unbind (S : System.Address; Addr : Interfaces.C.Strings.Chars_Ptr) return Int;  -- zmq.h:361
   pragma Import (C, Zmq_Unbind, "zmq_unbind");

   function Zmq_Disconnect (S : System.Address; Addr : Interfaces.C.Strings.Chars_Ptr) return Int;  -- zmq.h:362
   pragma Import (C, Zmq_Disconnect, "zmq_disconnect");

   function Zmq_Send
     (S     : System.Address;
      Buf   : System.Address;
      Len   : size_t;
      Flags : Int) return Int;  -- zmq.h:363
   pragma Import (C, Zmq_Send, "zmq_send");

   function Zmq_Send_Const
     (S     : System.Address;
      Buf   : System.Address;
      Len   : size_t;
      Flags : Int) return Int;  -- zmq.h:364
   pragma Import (C, Zmq_Send_Const, "zmq_send_const");

   function Zmq_Recv
     (S     : System.Address;
      Buf   : System.Address;
      Len   : size_t;
      Flags : Int) return Int;  -- zmq.h:365
   pragma Import (C, Zmq_Recv, "zmq_recv");

   function Zmq_Socket_Monitor
     (S      : System.Address;
      Addr   : Interfaces.C.Strings.Chars_Ptr;
      Events : Int) return Int;  -- zmq.h:366
   pragma Import (C, Zmq_Socket_Monitor, "zmq_socket_monitor");

   type Zmq_Pollitem_T is record
      Socket  : System.Address;  -- zmq.h:379
      Fd      : aliased Int;  -- zmq.h:383
      Events  : aliased Short;  -- zmq.h:385
      Revents : aliased Short;  -- zmq.h:386
   end record;
   pragma Convention (C_Pass_By_Copy, Zmq_Pollitem_T);  -- zmq.h:377

   function Zmq_Poll
     (Items   : access Zmq_Pollitem_T;
      Nitems  : Int;
      Timeout : Long) return Int;  -- zmq.h:391
   pragma Import (C, Zmq_Poll, "zmq_poll");

   function Zmq_Proxy
     (Frontend : System.Address;
      Backend  : System.Address;
      Capture  : System.Address) return Int;  -- zmq.h:397
   pragma Import (C, Zmq_Proxy, "zmq_proxy");

   function Zmq_Proxy_Steerable
     (Frontend : System.Address;
      Backend  : System.Address;
      Capture  : System.Address;
      Control  : System.Address) return Int;  -- zmq.h:398
   pragma Import (C, Zmq_Proxy_Steerable, "zmq_proxy_steerable");

   function zmq_has (Capability : Interfaces.C.Strings.Chars_Ptr) return Int;  -- zmq.h:405
   pragma Import (C, zmq_has, "zmq_has");

   function Zmq_Device
     (C_Type   : Int;
      Frontend : System.Address;
      Backend  : System.Address) return Int;  -- zmq.h:413
   pragma Import (C, Zmq_Device, "zmq_device");

   function Zmq_Sendmsg
     (S     : System.Address;
      Msg   : access Zmq_Msg_T;
      Flags : Int) return Int;  -- zmq.h:414
   pragma Obsolescent;
   pragma Import (C, Zmq_Sendmsg, "zmq_sendmsg");

   function Zmq_Recvmsg
     (S     : System.Address;
      Msg   : access Zmq_Msg_T;
      Flags : Int) return Int;  -- zmq.h:415
   pragma Obsolescent;
   pragma Import (C, Zmq_Recvmsg, "zmq_recvmsg");

   function Zmq_Z85_Encode
     (Dest : Interfaces.C.Strings.Chars_Ptr;
      Data : access Extensions.Unsigned_8;
      Size : size_t) return Interfaces.C.Strings.Chars_Ptr;  -- zmq.h:423
   pragma Import (C, Zmq_Z85_Encode, "zmq_z85_encode");

   function Zmq_Z85_Decode (Dest : access Extensions.Unsigned_8; String : Interfaces.C.Strings.Chars_Ptr) return access Extensions.Unsigned_8;  -- zmq.h:426
   pragma Import (C, Zmq_Z85_Decode, "zmq_z85_decode");

   function Zmq_Curve_Keypair (Z85_Public_Key : Interfaces.C.Strings.Chars_Ptr; Z85_Secret_Key : Interfaces.C.Strings.Chars_Ptr) return Int;  -- zmq.h:430
   pragma Import (C, Zmq_Curve_Keypair, "zmq_curve_keypair");

   --  skipped empty struct iovec

   function Zmq_Sendiov
     (S     : System.Address;
      Iov   : System.Address;
      Count : size_t;
      Flags : Int) return Int;  -- zmq.h:441
   pragma Import (C, Zmq_Sendiov, "zmq_sendiov");

   function Zmq_Recviov
     (S     : System.Address;
      Iov   : System.Address;
      Count : access size_t;
      Flags : Int) return Int;  -- zmq.h:442
   pragma Import (C, Zmq_Recviov, "zmq_recviov");

   function Zmq_Stopwatch_Start return System.Address;  -- zmq.h:448
   pragma Import (C, Zmq_Stopwatch_Start, "zmq_stopwatch_start");

   function Zmq_Stopwatch_Stop (Watch_U : System.Address) return Unsigned_Long;  -- zmq.h:452
   pragma Import (C, Zmq_Stopwatch_Stop, "zmq_stopwatch_stop");

   procedure Zmq_Sleep (Seconds_U : Int);  -- zmq.h:455
   pragma Import (C, Zmq_Sleep, "zmq_sleep");

   --  skipped function type zmq_thread_fn

   function Zmq_Threadstart (Func : access procedure (Arg1 : System.Address); Arg : System.Address) return System.Address;  -- zmq.h:460
   pragma Import (C, Zmq_Threadstart, "zmq_threadstart");

   procedure Zmq_Threadclose (Thread : System.Address);  -- zmq.h:463
   pragma Import (C, Zmq_Threadclose, "zmq_threadclose");

end ZMQ.Low_Level;
