// start of scheduler.h

#ifndef SCHEDULER_H
#define SCHEDULER_H


// returns the number of logical cores
#ifdef __APPLE__
#include <sys/sysctl.h>
static int num_processors() {
    int ncores;
    size_t ncores_size = sizeof(ncores);
    if (sysctlbyname("hw.logicalcpu", &ncores, &ncores_size, NULL, 0)) {
      fprintf(stderr, "[num_processors] failed to find number of cores %s", strerror(errno));
      return -1;
    }
    return ncores;
}
#else // If Linux
#include <sys/sysinfo.h>
static int num_processors() {
  return get_nprocs();
}

#endif
// RIP windows

#define MULTICORE

typedef int (*task_fn)(void*, int, int, int);

struct task {
  task_fn fn;
  void* args;
  int start, end;
  int task_id; // or a subtask id

  int *counter; // Counter ongoing subtasks
  pthread_mutex_t *mutex;
  pthread_cond_t *cond;
};


enum OP {
  SegMap,
  SegRed,
};

static inline void *futhark_worker(void* arg) {
  struct job_queue *q = (struct job_queue*) arg;
  while(1) {
    struct task *task;
    if (job_queue_pop(q, (void**)&task) == 0) {
      task->fn(task->args, task->start, task->end, task->task_id);
       pthread_mutex_lock(task->mutex);
       (*task->counter)--;
       pthread_cond_signal(task->cond);
       pthread_mutex_unlock(task->mutex);
       free(task);
    } else {
       break;
    }
  }
  return NULL;
}



static inline struct task* setup_task(task_fn fn, void* task_args, int task_id,
                                      pthread_mutex_t *mutex, pthread_cond_t *cond,
                                      int* counter, int start, int end) {
  // Don't allocate this on heap, use stack!
  struct task* task = malloc(sizeof(struct task));
  task->fn      = fn;
  task->args    = task_args;
  task->task_id = task_id;
  task->mutex   = mutex;
  task->cond    = cond;
  task->counter = counter;
  task->start   = start;
  task->end     = end;
  return task;
}


static inline int scheduler_do_task(struct job_queue *q,
                                    task_fn fn, void* task_args,
                                    int iterations, int *ntask)
{
  assert(q != NULL);
  if (iterations == 0) {
    if (ntask != NULL)  *ntask = 0;
    return 0;
  }

  pthread_mutex_t mutex;
  if (pthread_mutex_init(&mutex, NULL) != 0) {
     fprintf(stderr, "got error from pthread_mutex_init: %s\n", strerror(errno));
     return 1;
  }
  pthread_cond_t cond;
  if (pthread_cond_init(&cond, NULL) != 0) {
     fprintf(stderr, "got error from pthread_cond_init: %s\n", strerror(errno));
     return 1;
  }

  int task_id = 0;
  int shared_counter = 0;
  int iter_pr_task = iterations / q->num_workers;
  int remainder = iterations % q->num_workers;

  struct task *task = setup_task(fn, task_args, task_id,
                                 &mutex, &cond, &shared_counter,
                                 0, remainder + iter_pr_task);
  task_id++;
  pthread_mutex_lock(&mutex);
  shared_counter++;
  pthread_mutex_unlock(&mutex);
  job_queue_push(q, (void*)task);


  for (int i = remainder + iter_pr_task; i < iterations; i += iter_pr_task)
  {
    struct task *task = setup_task(fn, task_args, task_id,
                                   &mutex, &cond, &shared_counter,
                                   i, i + iter_pr_task);
    task_id++;

    pthread_mutex_lock(&mutex);
    shared_counter++;
    pthread_mutex_unlock(&mutex);
    job_queue_push(q, (void*)task);
  }


  // Join (wait for tasks to finish)
  pthread_mutex_lock(&mutex);
  while (shared_counter != 0) {
    pthread_cond_wait(&cond, &mutex);
  }

  if (ntask != NULL) {
    *ntask = task_id;
  }

  return 0;
}


#endif


// End of scheduler.h
