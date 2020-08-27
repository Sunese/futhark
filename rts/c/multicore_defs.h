// start of multicore_defs.h

#ifndef MULTICORE_DEFS
#define MULTICORE_DEFS


#include <pthread.h>
#include <stdlib.h>
#include <assert.h>

#define MULTICORE
/* #define MCPROFILE */

// Which queue implementation to use
#define MCJOBQUEUE
/* #define MCCHASELEV */


#ifdef _WIN32
#include <windows.h>
#elif __APPLE__
#include <sys/sysctl.h>
// For getting cpu usage of threads
#include <mach/mach.h>
#include <sys/resource.h>
#else // Linux
#include <sys/sysinfo.h>
#include <sys/resource.h>
#include <signal.h>
#endif


// Forward declarations
// Scheduler definitions
struct scheduler;
struct scheduler_info;
enum scheduling;
struct scheduler_subtask;
struct scheduler_task;


struct subtask_queue {
  int capacity; // Size of the buffer.
  int first; // Index of the start of the ring buffer.
  int num_used; // Number of used elements in the buffer.
  struct subtask **buffer;

  pthread_mutex_t mutex; // Mutex used for synchronisation.
  pthread_cond_t cond;   // Condition variable used for synchronisation.
  int dead;


  /* Profiling fields */
  uint64_t time_enqueue;
  uint64_t time_dequeue;
  uint64_t n_dequeues;
  uint64_t n_enqueues;
};


struct deque_buffer {
  struct subtask** array;
  int64_t size;
};

struct deque {
  struct deque_buffer *buffer;
  int64_t top, bottom;
  int dead;
};



// Function definitions
typedef int (*parloop_fn)(void* args, int64_t start, int64_t end, int subtask_id, int tid);
typedef int (*task_fn)(void* args, int64_t iterations, int tid, struct scheduler_info info);


/* A subtask that can be executed by a thread */
struct subtask {
  parloop_fn fn;
  void* args;
  int64_t start, end;
  const char *name;
  int chunkable;
  int64_t chunk_size;
  int stolen_from;
  int id;

  // Shared variables across subtasks
  volatile int *counter; // Counter for ongoing subtasks
  int64_t *total_time;
  int64_t *total_iter;
};



struct worker {
  pthread_t thread;
#if defined(MCCHASELEV)
  struct deque q;
#elif defined(MCJOBQUEUE)
  struct subtask_queue q;
#endif
  struct scheduler *scheduler;
  int cur_working;
  int dead;
  int output_usage;
  int tid;                     /* Just a thread id */

  uint64_t time_spent_working; /* Time spent in tasks functions */
  // Timing field used for online algorithm
  uint64_t timer;
  uint64_t total;
  int nested;
};

#endif

// end of multicore_defs.h
