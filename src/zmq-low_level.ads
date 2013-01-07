--------------------------------------------------------------------
--                                                                --
--  Do not edit, this file is automaticly generated from "zmq.h"  --
--                                                                --
--------------------------------------------------------------------

pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C;         use Interfaces.C;
with Interfaces.C.Strings;
with System;

package ZMQ.Low_Level is
   pragma Preelaborate;

   ZMQ_VERSION_MAJOR : constant := 3;  --  zmq.h:61
   ZMQ_VERSION_MINOR : constant := 2;  --  zmq.h:62
   ZMQ_VERSION_PATCH : constant := 2;  --  zmq.h:63
   --  arg-macro: function ZMQ_MAKE_VERSION (major, minor, p((major) * 10000 + (minor) * 100 + (patch)
   --    return (major) * 10000 + (minor) * 100 + (patch);
   --  unsupported macro: ZMQ_VERSION ZMQ_MAKE_VERSION(ZMQ_VERSION_MAJOR, ZMQ_VERSION_MINOR, ZMQ_VERSION_PATCH)

   ZMQ_HAUSNUMERO : constant := 156384712;  --  zmq.h:79
   EFSM           : constant := ZMQ_HAUSNUMERO + 51;
   ENOCOMPATPROTO : constant := ZMQ_HAUSNUMERO + 52;
   ETERM          : constant := ZMQ_HAUSNUMERO + 53;
   EMTHREAD       : constant := ZMQ_HAUSNUMERO + 54;

   ZMQ_IO_THREADS  : constant := 1;  --  zmq.h:158
   ZMQ_MAX_SOCKETS : constant := 2;  --  zmq.h:159

   ZMQ_IO_THREADS_DFLT  : constant := 1;  --  zmq.h:162
   ZMQ_MAX_SOCKETS_DFLT : constant := 1024;  --  zmq.h:163

   ZMQ_PAIR   : constant := 0;  --  zmq.h:204
   ZMQ_PUB    : constant := 1;  --  zmq.h:205
   ZMQ_SUB    : constant := 2;  --  zmq.h:206
   ZMQ_REQ    : constant := 3;  --  zmq.h:207
   ZMQ_REP    : constant := 4;  --  zmq.h:208
   ZMQ_DEALER : constant := 5;  --  zmq.h:209
   ZMQ_ROUTER : constant := 6;  --  zmq.h:210
   ZMQ_PULL   : constant := 7;  --  zmq.h:211
   ZMQ_PUSH   : constant := 8;  --  zmq.h:212
   ZMQ_XPUB   : constant := 9;  --  zmq.h:213
   ZMQ_XSUB   : constant := 10;  --  zmq.h:214
   --  unsupported macro: ZMQ_XREQ ZMQ_DEALER
   --  unsupported macro: ZMQ_XREP ZMQ_ROUTER

   ZMQ_AFFINITY                : constant := 4;  --  zmq.h:221
   ZMQ_IDENTITY                : constant := 5;  --  zmq.h:222
   ZMQ_SUBSCRIBE               : constant := 6;  --  zmq.h:223
   ZMQ_UNSUBSCRIBE             : constant := 7;  --  zmq.h:224
   ZMQ_RATE                    : constant := 8;  --  zmq.h:225
   ZMQ_RECOVERY_IVL            : constant := 9;  --  zmq.h:226
   ZMQ_SNDBUF                  : constant := 11;  --  zmq.h:227
   ZMQ_RCVBUF                  : constant := 12;  --  zmq.h:228
   ZMQ_RCVMORE                 : constant := 13;  --  zmq.h:229
   ZMQ_FD                      : constant := 14;  --  zmq.h:230
   ZMQ_EVENTS                  : constant := 15;  --  zmq.h:231
   ZMQ_TYPE                    : constant := 16;  --  zmq.h:232
   ZMQ_LINGER                  : constant := 17;  --  zmq.h:233
   ZMQ_RECONNECT_IVL           : constant := 18;  --  zmq.h:234
   ZMQ_BACKLOG                 : constant := 19;  --  zmq.h:235
   ZMQ_RECONNECT_IVL_MAX       : constant := 21;  --  zmq.h:236
   ZMQ_MAXMSGSIZE              : constant := 22;  --  zmq.h:237
   ZMQ_SNDHWM                  : constant := 23;  --  zmq.h:238
   ZMQ_RCVHWM                  : constant := 24;  --  zmq.h:239
   ZMQ_MULTICAST_HOPS          : constant := 25;  --  zmq.h:240
   ZMQ_RCVTIMEO                : constant := 27;  --  zmq.h:241
   ZMQ_SNDTIMEO                : constant := 28;  --  zmq.h:242
   ZMQ_IPV4ONLY                : constant := 31;  --  zmq.h:243
   ZMQ_LAST_ENDPOINT           : constant := 32;  --  zmq.h:244
   ZMQ_ROUTER_MANDATORY        : constant := 33;  --  zmq.h:245
   ZMQ_TCP_KEEPALIVE           : constant := 34;  --  zmq.h:246
   ZMQ_TCP_KEEPALIVE_CNT       : constant := 35;  --  zmq.h:247
   ZMQ_TCP_KEEPALIVE_IDLE      : constant := 36;  --  zmq.h:248
   ZMQ_TCP_KEEPALIVE_INTVL     : constant := 37;  --  zmq.h:249
   ZMQ_TCP_ACCEPT_FILTER       : constant := 38;  --  zmq.h:250
   ZMQ_DELAY_ATTACH_ON_CONNECT : constant := 39;  --  zmq.h:251
   ZMQ_XPUB_VERBOSE            : constant := 40;  --  zmq.h:252

   ZMQ_MORE : constant := 1;  --  zmq.h:256

   ZMQ_DONTWAIT : constant := 1;  --  zmq.h:259
   ZMQ_SNDMORE  : constant := 2;  --  zmq.h:260
   --  unsupported macro: ZMQ_NOBLOCK ZMQ_DONTWAIT
   --  unsupported macro: ZMQ_FAIL_UNROUTABLE ZMQ_ROUTER_MANDATORY
   --  unsupported macro: ZMQ_ROUTER_BEHAVIOR ZMQ_ROUTER_MANDATORY

   ZMQ_EVENT_CONNECTED       : constant := 1;  --  zmq.h:272
   ZMQ_EVENT_CONNECT_DELAYED : constant := 2;  --  zmq.h:273
   ZMQ_EVENT_CONNECT_RETRIED : constant := 4;  --  zmq.h:274

   ZMQ_EVENT_LISTENING   : constant := 8;  --  zmq.h:276
   ZMQ_EVENT_BIND_FAILED : constant := 16;  --  zmq.h:277

   ZMQ_EVENT_ACCEPTED      : constant := 32;  --  zmq.h:279
   ZMQ_EVENT_ACCEPT_FAILED : constant := 64;  --  zmq.h:280

   ZMQ_EVENT_CLOSED       : constant := 128;  --  zmq.h:282
   ZMQ_EVENT_CLOSE_FAILED : constant := 256;  --  zmq.h:283
   ZMQ_EVENT_DISCONNECTED : constant := 512;  --  zmq.h:284
   --  unsupported macro: ZMQ_EVENT_ALL ( ZMQ_EVENT_CONNECTED | ZMQ_EVENT_CONNECT_DELAYED | ZMQ_EVENT_CONNECT_RETRIED |
   --ZMQ_EVENT_LISTENING | ZMQ_EVENT_BIND_FAILED | ZMQ_EVENT_ACCEPTED | ZMQ_EVENT_ACCEPT_FAILED | ZMQ_EVENT_CLOSED |
   --ZMQ_EVENT_CLOSE_FAILED | ZMQ_EVENT_DISCONNECTED )

   ZMQ_POLLIN  : constant := 1;  --  zmq.h:366
   ZMQ_POLLOUT : constant := 2;  --  zmq.h:367
   ZMQ_POLLERR : constant := 4;  --  zmq.h:368

   ZMQ_STREAMER  : constant := 1;  --  zmq.h:389
   ZMQ_FORWARDER : constant := 2;  --  zmq.h:390
   ZMQ_QUEUE     : constant := 3;  --  zmq.h:391

   procedure zmq_version (major : access int; minor : access int; patch : access int);  -- zmq.h:71
   pragma Import (C, zmq_version, "zmq_version");

   function zmq_errno return int;  -- zmq.h:147
   pragma Import (C, zmq_errno, "zmq_errno");

   function zmq_strerror (errnum : int) return Interfaces.C.Strings.chars_ptr;  -- zmq.h:150
   pragma Import (C, zmq_strerror, "zmq_strerror");

   function zmq_ctx_new return  System.Address;  -- zmq.h:165
   pragma Import (C, zmq_ctx_new, "zmq_ctx_new");

   function zmq_ctx_destroy (context : System.Address) return int;  -- zmq.h:166
   pragma Import (C, zmq_ctx_destroy, "zmq_ctx_destroy");

   function zmq_ctx_set
     (context : System.Address;
      option  : int;
      optval  : int)
      return    int;  -- zmq.h:167
   pragma Import (C, zmq_ctx_set, "zmq_ctx_set");

   function zmq_ctx_get (context : System.Address; option : int) return int;  -- zmq.h:168
   pragma Import (C, zmq_ctx_get, "zmq_ctx_get");

   function zmq_init (io_threads : int) return System.Address;  -- zmq.h:171
   pragma Import (C, zmq_init, "zmq_init");

   function zmq_term (context : System.Address) return int;  -- zmq.h:172
   pragma Import (C, zmq_term, "zmq_term");

   type zmq_msg_t_u_u_array is array (0 .. 31) of aliased unsigned_char;
   type zmq_msg_t is record
      u_u : aliased zmq_msg_t_u_u_array;  -- zmq.h:179
   end record;
   pragma Convention (C_Pass_By_Copy, zmq_msg_t);  -- zmq.h:179

   --  skipped function type zmq_free_fn

   function zmq_msg_init (msg : access zmq_msg_t) return int;  -- zmq.h:183
   pragma Import (C, zmq_msg_init, "zmq_msg_init");

   function zmq_msg_init_size (msg : access zmq_msg_t; size : size_t) return int;  -- zmq.h:184
   pragma Import (C, zmq_msg_init_size, "zmq_msg_init_size");

   function zmq_msg_init_data
     (msg  : access zmq_msg_t;
      data : System.Address;
      size : size_t;
      ffn  : access procedure (arg1 : System.Address; arg2 : System.Address);
      hint : System.Address)
      return int;  -- zmq.h:185
   pragma Import (C, zmq_msg_init_data, "zmq_msg_init_data");

   function zmq_msg_send
     (msg   : access zmq_msg_t;
      s     : System.Address;
      flags : int)
      return  int;  -- zmq.h:187
   pragma Import (C, zmq_msg_send, "zmq_msg_send");

   function zmq_msg_recv
     (msg   : access zmq_msg_t;
      s     : System.Address;
      flags : int)
      return  int;  -- zmq.h:188
   pragma Import (C, zmq_msg_recv, "zmq_msg_recv");

   function zmq_msg_close (msg : access zmq_msg_t) return int;  -- zmq.h:189
   pragma Import (C, zmq_msg_close, "zmq_msg_close");

   function zmq_msg_move (dest : access zmq_msg_t; src : access zmq_msg_t) return int;  -- zmq.h:190
   pragma Import (C, zmq_msg_move, "zmq_msg_move");

   function zmq_msg_copy (dest : access zmq_msg_t; src : access zmq_msg_t) return int;  -- zmq.h:191
   pragma Import (C, zmq_msg_copy, "zmq_msg_copy");

   function zmq_msg_data (msg : access zmq_msg_t) return System.Address;  -- zmq.h:192
   pragma Import (C, zmq_msg_data, "zmq_msg_data");

   function zmq_msg_size (msg : access zmq_msg_t) return size_t;  -- zmq.h:193
   pragma Import (C, zmq_msg_size, "zmq_msg_size");

   function zmq_msg_more (msg : access zmq_msg_t) return int;  -- zmq.h:194
   pragma Import (C, zmq_msg_more, "zmq_msg_more");

   function zmq_msg_get (msg : access zmq_msg_t; option : int) return int;  -- zmq.h:195
   pragma Import (C, zmq_msg_get, "zmq_msg_get");

   function zmq_msg_set
     (msg    : access zmq_msg_t;
      option : int;
      optval : int)
      return   int;  -- zmq.h:196
   pragma Import (C, zmq_msg_set, "zmq_msg_set");

   type anon_7;
   type anon_8 is record
      addr : Interfaces.C.Strings.chars_ptr;  -- zmq.h:297
      fd   : aliased int;  -- zmq.h:298
   end record;
   pragma Convention (C_Pass_By_Copy, anon_8);
   type anon_9 is record
      addr : Interfaces.C.Strings.chars_ptr;  -- zmq.h:301
      err  : aliased int;  -- zmq.h:302
   end record;
   pragma Convention (C_Pass_By_Copy, anon_9);
   type anon_10 is record
      addr     : Interfaces.C.Strings.chars_ptr;  -- zmq.h:305
      interval : aliased int;  -- zmq.h:306
   end record;
   pragma Convention (C_Pass_By_Copy, anon_10);
   type anon_11 is record
      addr : Interfaces.C.Strings.chars_ptr;  -- zmq.h:309
      fd   : aliased int;  -- zmq.h:310
   end record;
   pragma Convention (C_Pass_By_Copy, anon_11);
   type anon_12 is record
      addr : Interfaces.C.Strings.chars_ptr;  -- zmq.h:313
      err  : aliased int;  -- zmq.h:314
   end record;
   pragma Convention (C_Pass_By_Copy, anon_12);
   type anon_13 is record
      addr : Interfaces.C.Strings.chars_ptr;  -- zmq.h:317
      fd   : aliased int;  -- zmq.h:318
   end record;
   pragma Convention (C_Pass_By_Copy, anon_13);
   type anon_14 is record
      addr : Interfaces.C.Strings.chars_ptr;  -- zmq.h:321
      err  : aliased int;  -- zmq.h:322
   end record;
   pragma Convention (C_Pass_By_Copy, anon_14);
   type anon_15 is record
      addr : Interfaces.C.Strings.chars_ptr;  -- zmq.h:325
      fd   : aliased int;  -- zmq.h:326
   end record;
   pragma Convention (C_Pass_By_Copy, anon_15);
   type anon_16 is record
      addr : Interfaces.C.Strings.chars_ptr;  -- zmq.h:329
      err  : aliased int;  -- zmq.h:330
   end record;
   pragma Convention (C_Pass_By_Copy, anon_16);
   type anon_17 is record
      addr : Interfaces.C.Strings.chars_ptr;  -- zmq.h:333
      fd   : aliased int;  -- zmq.h:334
   end record;
   pragma Convention (C_Pass_By_Copy, anon_17);
   type anon_7 (discr : unsigned := 0) is record
      case discr is
      when 0 =>
         connected : aliased anon_8;  -- zmq.h:299
      when 1 =>
         connect_delayed : aliased anon_9;  -- zmq.h:303
      when 2 =>
         connect_retried : aliased anon_10;  -- zmq.h:307
      when 3 =>
         listening : aliased anon_11;  -- zmq.h:311
      when 4 =>
         bind_failed : aliased anon_12;  -- zmq.h:315
      when 5 =>
         accepted : aliased anon_13;  -- zmq.h:319
      when 6 =>
         accept_failed : aliased anon_14;  -- zmq.h:323
      when 7 =>
         closed : aliased anon_15;  -- zmq.h:327
      when 8 =>
         close_failed : aliased anon_16;  -- zmq.h:331
      when others =>
         disconnected : aliased anon_17;  -- zmq.h:335
      end case;
   end record;
   pragma Convention (C_Pass_By_Copy, anon_7);
   pragma Unchecked_Union (anon_7);
   type zmq_event_t is record
      event : aliased int;  -- zmq.h:294
      data  : anon_7;  -- zmq.h:336
   end record;
   pragma Convention (C_Pass_By_Copy, zmq_event_t);  -- zmq.h:337

   --  skipped anonymous struct anon_6

   function zmq_socket (arg1 : System.Address; c_type : int) return System.Address;  -- zmq.h:339
   pragma Import (C, zmq_socket, "zmq_socket");

   function zmq_close (s : System.Address) return int;  -- zmq.h:340
   pragma Import (C, zmq_close, "zmq_close");

   function zmq_setsockopt
     (s         : System.Address;
      option    : int;
      optval    : System.Address;
      optvallen : size_t)
      return      int;  -- zmq.h:341
   pragma Import (C, zmq_setsockopt, "zmq_setsockopt");

   function zmq_getsockopt
     (s         : System.Address;
      option    : int;
      optval    : System.Address;
      optvallen : access size_t)
      return      int;  -- zmq.h:343
   pragma Import (C, zmq_getsockopt, "zmq_getsockopt");

   function zmq_bind (s : System.Address; addr : Interfaces.C.Strings.chars_ptr) return int;  -- zmq.h:345
   pragma Import (C, zmq_bind, "zmq_bind");

   function zmq_connect (s : System.Address; addr : Interfaces.C.Strings.chars_ptr) return int;  -- zmq.h:346
   pragma Import (C, zmq_connect, "zmq_connect");

   function zmq_unbind (s : System.Address; addr : Interfaces.C.Strings.chars_ptr) return int;  -- zmq.h:347
   pragma Import (C, zmq_unbind, "zmq_unbind");

   function zmq_disconnect (s : System.Address; addr : Interfaces.C.Strings.chars_ptr) return int;  -- zmq.h:348
   pragma Import (C, zmq_disconnect, "zmq_disconnect");

   function zmq_send
     (s     : System.Address;
      buf   : System.Address;
      len   : size_t;
      flags : int)
      return  int;  -- zmq.h:349
   pragma Import (C, zmq_send, "zmq_send");

   function zmq_recv
     (s     : System.Address;
      buf   : System.Address;
      len   : size_t;
      flags : int)
      return  int;  -- zmq.h:350
   pragma Import (C, zmq_recv, "zmq_recv");

   function zmq_socket_monitor
     (s      : System.Address;
      addr   : Interfaces.C.Strings.chars_ptr;
      events : int)
      return   int;  -- zmq.h:351
   pragma Import (C, zmq_socket_monitor, "zmq_socket_monitor");

   function zmq_sendmsg
     (s     : System.Address;
      msg   : access zmq_msg_t;
      flags : int)
      return  int;  -- zmq.h:353
   pragma Import (C, zmq_sendmsg, "zmq_sendmsg");

   function zmq_recvmsg
     (s     : System.Address;
      msg   : access zmq_msg_t;
      flags : int)
      return  int;  -- zmq.h:354
   pragma Import (C, zmq_recvmsg, "zmq_recvmsg");

   --  skipped empty struct iovec

   function zmq_sendiov
     (s     : System.Address;
      iov   : System.Address;
      count : size_t;
      flags : int)
      return  int;  -- zmq.h:359
   pragma Import (C, zmq_sendiov, "zmq_sendiov");

   function zmq_recviov
     (s     : System.Address;
      iov   : System.Address;
      count : access size_t;
      flags : int)
      return  int;  -- zmq.h:360
   pragma Import (C, zmq_recviov, "zmq_recviov");

   type zmq_pollitem_t is record
      socket  : System.Address;  -- zmq.h:372
      fd      : aliased int;  -- zmq.h:376
      events  : aliased short;  -- zmq.h:378
      revents : aliased short;  -- zmq.h:379
   end record;
   pragma Convention (C_Pass_By_Copy, zmq_pollitem_t);  -- zmq.h:380

   --  skipped anonymous struct anon_18

   function zmq_poll
     (items   : access zmq_pollitem_t;
      nitems  : int;
      timeout : long)
      return    int;  -- zmq.h:382
   pragma Import (C, zmq_poll, "zmq_poll");

   function zmq_proxy
     (frontend : System.Address;
      backend  : System.Address;
      capture  : System.Address)
      return     int;  -- zmq.h:386
   pragma Import (C, zmq_proxy, "zmq_proxy");

   function zmq_device
     (c_type   : int;
      frontend : System.Address;
      backend  : System.Address)
      return     int;  -- zmq.h:393
   pragma Import (C, zmq_device, "zmq_device");

end ZMQ.Low_Level;
