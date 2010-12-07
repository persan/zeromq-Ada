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

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;
with System;

package ZMQ.Low_Level is
   pragma Preelaborate;
   pragma Warnings (Off);
   package Defs is

      ZMQ_VERSION_MAJOR : constant := 2;  --  zmq.h:55
      ZMQ_VERSION_MINOR : constant := 1;  --  zmq.h:56
      ZMQ_VERSION_PATCH : constant := 0;  --  zmq.h:57
      ZMQ_VERSION : constant := ZMQ_VERSION_MAJOR * 10000 + ZMQ_VERSION_MINOR * 100 + ZMQ_VERSION_PATCH;

      ZMQ_HAUSNUMERO : constant := 156384712;  --  zmq.h:73
      EFSM : constant := (ZMQ_HAUSNUMERO + 51);
      ENOCOMPATPROTO : constant := (ZMQ_HAUSNUMERO + 52);
      ETERM : constant := (ZMQ_HAUSNUMERO + 53);
      EMTHREAD : constant := (ZMQ_HAUSNUMERO + 54);

      ZMQ_MAX_VSM_SIZE : constant := 30;  --  zmq.h:124

      ZMQ_DELIMITER : constant := 31;  --  zmq.h:128
      ZMQ_VSM : constant := 32;  --  zmq.h:129

      ZMQ_MSG_MORE : constant := 1;  --  zmq.h:134
      ZMQ_MSG_SHARED : constant := 128;  --  zmq.h:135

      ZMQ_PAIR : constant := 0;  --  zmq.h:172
      ZMQ_PUB : constant := 1;  --  zmq.h:173
      ZMQ_SUB : constant := 2;  --  zmq.h:174
      ZMQ_REQ : constant := 3;  --  zmq.h:175
      ZMQ_REP : constant := 4;  --  zmq.h:176
      ZMQ_XREQ : constant := 5;  --  zmq.h:177
      ZMQ_XREP : constant := 6;  --  zmq.h:178
      ZMQ_PULL : constant := 7;  --  zmq.h:179
      ZMQ_PUSH : constant := 8;  --  zmq.h:180
      ZMQ_UPSTREAM  : constant := ZMQ_PULL;
      ZMQ_DOWNSTREAM  : constant := ZMQ_PUSH;

      ZMQ_HWM : constant := 1;  --  zmq.h:185
      ZMQ_SWAP : constant := 3;  --  zmq.h:186
      ZMQ_AFFINITY : constant := 4;  --  zmq.h:187
      ZMQ_IDENTITY : constant := 5;  --  zmq.h:188
      ZMQ_SUBSCRIBE : constant := 6;  --  zmq.h:189
      ZMQ_UNSUBSCRIBE : constant := 7;  --  zmq.h:190
      ZMQ_RATE : constant := 8;  --  zmq.h:191
      ZMQ_RECOVERY_IVL : constant := 9;  --  zmq.h:192
      ZMQ_MCAST_LOOP : constant := 10;  --  zmq.h:193
      ZMQ_SNDBUF : constant := 11;  --  zmq.h:194
      ZMQ_RCVBUF : constant := 12;  --  zmq.h:195
      ZMQ_RCVMORE : constant := 13;  --  zmq.h:196
      ZMQ_FD : constant := 14;  --  zmq.h:197
      ZMQ_EVENTS : constant := 15;  --  zmq.h:198
      ZMQ_TYPE : constant := 16;  --  zmq.h:199
      ZMQ_LINGER : constant := 17;  --  zmq.h:200
      ZMQ_RECONNECT_IVL : constant := 18;  --  zmq.h:201
      ZMQ_BACKLOG : constant := 19;  --  zmq.h:202

      ZMQ_NOBLOCK : constant := 1;  --  zmq.h:205
      ZMQ_SNDMORE : constant := 2;  --  zmq.h:206

      ZMQ_POLLIN : constant := 1;  --  zmq.h:223
      ZMQ_POLLOUT : constant := 2;  --  zmq.h:224
      ZMQ_POLLERR : constant := 4;  --  zmq.h:225

      ZMQ_STREAMER : constant := 1;  --  zmq.h:245
      ZMQ_FORWARDER : constant := 2;  --  zmq.h:246
      ZMQ_QUEUE : constant := 3;  --  zmq.h:247


   end Defs;

   procedure zmq_version
     (major : access int;
      minor : access int;
      patch : access int);  -- zmq.h:65
   pragma Import (C, zmq_version, "zmq_version");

   function zmq_errno return int;  -- zmq.h:111
   pragma Import (C, zmq_errno, "zmq_errno");

   function zmq_strerror (errnum : int) return Interfaces.C.Strings.chars_ptr;  -- zmq.h:114
   pragma Import (C, zmq_strerror, "zmq_strerror");

   type zmq_msg_t_vsm_data_array is array (0 .. 29) of aliased unsigned_char;
   type zmq_msg_t is record
      content  : System.Address;  -- zmq.h:142
      flags    : aliased unsigned_char;  -- zmq.h:143
      vsm_size : aliased unsigned_char;  -- zmq.h:144
      vsm_data : aliased zmq_msg_t_vsm_data_array;  -- zmq.h:145
   end record;
   pragma Convention (C_Pass_By_Copy, zmq_msg_t);  -- zmq.h:146


   type zmq_free_fn is access procedure (data : System.Address; hint : System.Address);

   function zmq_msg_init (msg : access zmq_msg_t) return int;  -- zmq.h:150
   pragma Import (C, zmq_msg_init, "zmq_msg_init");

   function zmq_msg_init_size (msg : access zmq_msg_t; size : size_t) return int;  -- zmq.h:151
   pragma Import (C, zmq_msg_init_size, "zmq_msg_init_size");

   function zmq_msg_init_data
     (msg  : access zmq_msg_t;
      data : System.Address;
      size : size_t;
      ffn  : access procedure (arg1 : System.Address; arg2 : System.Address);
      hint : System.Address) return int;  -- zmq.h:152
   pragma Import (C, zmq_msg_init_data, "zmq_msg_init_data");

   function zmq_msg_close (msg : access zmq_msg_t) return int;  -- zmq.h:154
   pragma Import (C, zmq_msg_close, "zmq_msg_close");

   function zmq_msg_move (dest : access zmq_msg_t; src : access zmq_msg_t) return int;  -- zmq.h:155
   pragma Import (C, zmq_msg_move, "zmq_msg_move");

   function zmq_msg_copy (dest : access zmq_msg_t; src : access zmq_msg_t) return int;  -- zmq.h:156
   pragma Import (C, zmq_msg_copy, "zmq_msg_copy");

   function zmq_msg_data (msg : access zmq_msg_t) return System.Address;  -- zmq.h:157
   pragma Import (C, zmq_msg_data, "zmq_msg_data");

   function zmq_msg_size (msg : access zmq_msg_t) return size_t;  -- zmq.h:158
   pragma Import (C, zmq_msg_size, "zmq_msg_size");

   function zmq_init (io_threads : int) return System.Address;  -- zmq.h:164
   pragma Import (C, zmq_init, "zmq_init");

   function zmq_term (context : System.Address) return int;  -- zmq.h:165
   pragma Import (C, zmq_term, "zmq_term");

   function zmq_socket (context : System.Address; c_type : int) return System.Address;  -- zmq.h:208
   pragma Import (C, zmq_socket, "zmq_socket");

   function zmq_close (s : System.Address) return int;  -- zmq.h:209
   pragma Import (C, zmq_close, "zmq_close");

   function zmq_setsockopt
     (s         : System.Address;
      option    : int;
      optval    : System.Address;
      optvallen : size_t) return int;  -- zmq.h:210
   pragma Import (C, zmq_setsockopt, "zmq_setsockopt");

   function zmq_getsockopt
     (s         : System.Address;
      option    : int;
      optval    : System.Address;
      optvallen : access size_t) return int;  -- zmq.h:212
   pragma Import (C, zmq_getsockopt, "zmq_getsockopt");

   function zmq_bind (s : System.Address; addr : Interfaces.C.Strings.chars_ptr) return int;  -- zmq.h:214
   pragma Import (C, zmq_bind, "zmq_bind");

   function zmq_connect (s : System.Address; addr : Interfaces.C.Strings.chars_ptr) return int;  -- zmq.h:215
   pragma Import (C, zmq_connect, "zmq_connect");

   function zmq_send
     (s     : System.Address;
      msg   : access zmq_msg_t;
      flags : int) return int;  -- zmq.h:216
   pragma Import (C, zmq_send, "zmq_send");

   function zmq_recv
     (s     : System.Address;
      msg   : access zmq_msg_t;
      flags : int) return int;  -- zmq.h:217
   pragma Import (C, zmq_recv, "zmq_recv");

   type zmq_pollitem_t is record
      socket  : System.Address;  -- zmq.h:229
      fd      : aliased int;  -- zmq.h:233
      events  : aliased short;  -- zmq.h:235
      revents : aliased short;  -- zmq.h:236
   end record;
   pragma Convention (C_Pass_By_Copy, zmq_pollitem_t);  -- zmq.h:237


   function zmq_poll
     (items   : access zmq_pollitem_t;
      nitems  : int;
      timeout : long) return int;  -- zmq.h:239
   pragma Import (C, zmq_poll, "zmq_poll");

   function zmq_device
     (device    : int;
      insocket  : System.Address;
      outsocket : System.Address) return int;  -- zmq.h:249
   pragma Import (C, zmq_device, "zmq_device");

end ZMQ.Low_Level;
