/* shm-channel.h -- Shared memory probe interface module
   Copyright 2001 Free Software Foundation, Inc.
   Written by Stephane Carrez (stcarrez@worldnet.fr)

This file is part of gprofiler.

gprofiler is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

gprofiler is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with EBCS; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#ifndef _SHM_CHANNEL_H
#define _SHM_CHANNEL_H

#include "shm-ops.h"

// ----------------------------------------------------------------------
//
// Configuration parameters
//
#ifndef SHARED_MEMORY_SIZE
#  define SHARED_MEMORY_SIZE			(4096 * 2)
#endif

#ifndef SHARED_MEMORY_CLIENT_KEY
#  define SHARED_MEMORY_CLIENT_KEY		"SHM_PROBE_KEY"
#endif

#ifndef SHARED_MEMORY_SERVER_KEY
#  define SHARED_MEMORY_SERVER_KEY		"SHM_SERVER_KEY"
#endif

#ifndef SHARED_MEMORY_DEFAULT_KEY
#  define SHARED_MEMORY_DEFAULT_KEY		"12345678"
#endif


//
// The shared memory segment is represented by the `ShmHeader' structure.
// This is a small fifo in which a client writes and updates the `writePos'
// index, and the server reads and updates the `readPos' index. Indexes
// wraps to 0 when they reach `lastPos'. If the client can't write,
// it sets the `senderBlocked' flag and blocks on a semaphore object.
// The server checks (and clears) that flag to wake up the client.
//
// The shared memory segment and semaphore object are created by
// the server.
//
struct mat_shm_header {
    long	readPos;
    long	writePos;
    long	lastPos;
    long	senderBlocked;
    long	syncMode;
    char	buf[1];
};


// ----------------------------------------------------------------------
//
// Class:	ShmIpcHandler
//
// Goals :	Ipc manager for the shared memory implementation.
//
//		The ipc manager uses two memory segments: one for sending
// messages to the server (client), and one for the server to receive
// messages. The two memory segments are necessary to allow the tool
// to analyze itself.
//
struct mat_shm_channel
{
  struct mat_shm_segment shm_send;
  long   shm_key;
  
  int			siIpcLockCount;

	//
	// True if the sync mode is enabled.
  //
  char		siSyncMode;
#if 0
	//
	// Send `_size' bytes starting at addr `_addr'.
	//
    void send(const void* _addr, long _size);

    void send(ShmMessageType _type, const probe_info& _probe,
	      const ShmProbeInfo& _pInfo);
    
    void send(ShmMessageType _type, const probe_info& _probe,
	      const proc_info& _proc_info);

	//
	// If the synchronous mode is enabled, block until the server
	// has processed the message.
	//
    void synchronize();

    int initializeClient();
    
    int initializeServer();

	//
	// Find the key which must be used for the Ipc (shm or semaphore).
	//
    long getKey(const char* _env, const char* _def);
public:

    ShmIpcHandler();

    virtual ~ShmIpcHandler();
    
	//
	// Initialize the Ipc manager.
	//
    virtual int initialize(int* argc, char** argv);

	//
	// Wait for events and process them. Return when the timeout
	// specified in `timeout' has ellapsed.
	//
    virtual int processEvents(long timeout);

	//
	// Lock the probe and prevent the same thread to re-enter
	// in the probe routines (when two threads enter in the probe
	// routines, the second one must block; when a thread re-enters
	// in the probe rountine, lock() must return -1).
	//
    virtual int lock();
    
	//
	// Unlock the probe routines.
	//
    virtual void unlock();

    virtual int syncMode(const IpcMode _mode);

    void destroy();
    
    void* operator new(size_t _size);
#endif
};

#endif

