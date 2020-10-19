---
layout: page
title: Parallelization
nav_order: 5
permalink: /parallelization/
---

# Parallelization
{: .fs-9 }

---

## What is parallelization?

Parallelization is the process of speeding up code by taking a program, dividing it into independent tasks, and running those tasks simultaneously across multiple computer processors. Most modern laptops will have two or more processors (or "cores"), and many statisticians have access to so-called cluster computing systems, which can have dozens or hundreds of processing nodes, each of which can have multiple cores. Roughly speaking, a program that can be run in ten minutes when executed on a single core will take just one minute if it can be broken into ten separate tasks that all run at the same time. Therefore, parallelization can result in massive gains in computing speed and should be done whenever possible.

Not all code can be parallelized; the separate tasks cannot exchange information or depend on each other in any way. However, you can still write programs that are *partially* parallel, such as when you separately compute ten estimates in parallel and then take the mean of the ten estimates.

The terminology associated with parallel computing can be confusing - what is the difference between a node, a core, a processor, and a thread? What is the difference between a job and a task?

The most important distinction is between a *node* and a *core*. A node is a single computing device that runs your code and has access to resources, such as memory. You can think of your laptop as a single node with multiple cores.

!!!!! emphasize distinction between cores and nodes
!!!!! threads = nodes * cores ?????
!!!!! look at slurm documentation

## Parallelization in **simba**

There are two ways of parallelizing code using **simba**. The most straightforward way is referred to "outer parallelization". Most statistical simulations involve running multiple replicates of the same simulation; with outer parallelization, a single simulation replicate is assigned to a single task. **simba** automatically parallelizes code when you specify *parallel="outer"* using *set_config()*.

!!!!! automatically detect packages that have been loaded with library() or require(); use REGEX; alternatively, replace library() with another function (sim %<>% library(c("magrittr", "dplyr")))

!!!!! Allow for lib path on cluster to be set

```R
sim <- new_sim()
sim %<>% set_config(parallel = "outer")
```

The second way to parallelize is referred to as "inner parallelization". This is when parallelization occurs *within* a single simulation replicate. This is mainly useful when the total number of simulations you want to run is small (i.e. smaller than the number of available nodes/cores), but each simulation has pieces that can be parallelized. Inner parallelization requires you to specify pieces of your code to run in parallel using functions from the **parallel** package. Note that **simba** creates and manages the cluster; simply reference the special *CL* object anywhere your code calls for a cluster reference. If this doesn't make sense, check out the **parallel** package documentation (!!!!! link).

!!!!! Double-use of the word "cluster"
!!!!! Add "both"; need to test this out ?????
!!!!! Run on a "dummy cluster" if p=outer; allows switching be p=inner and p=outer ?????

```R
create_data <- function(sample_size) {

  data <- parLapply(CL, c(1:sample_size), function(x){
    x <- rnorm(n=1, mean=10, sd=1)
  	y <- 3*x + 9
  	z <- x / y
    return(list(x,y,z))
  })
  df <- as.data.frame(matrix(unlist(data), ncol=3))
  names(df) <- c("x","y","z")

  return(df)
  
}

sim %<>% add_creator(create_data)
```

In the example above, we parallelized code within the creator function. We can also use *run_in_parallel()*

If you don't want to parallelize at all, simply omit the *parallel* option from the 

!!!!! The sim_uid 0 (zero) is reserved for the one replicate that runs after all others have finished.

!!!!! Need a sample simulation to load/run for examples

Types: outer, inner, none

Setting the *parallel* configuration option to "none" can be useful for debugging.

We can see the efficiency gain incurred from parallelization by comparing the sum of the individual replicate runtimes to the total simulation time.

```R
print(sum(sim$results$runtime)) # !!!!!
print(sim$total_runtime) # !!!!!
```

; in most cases, the sum of the runtimes will be greater, indicating a gain in efficiency. However, there is "overhead" associated with parallelizing, especially if you are using a multi-user cluster computing system where there is a lot of competition for nodes.

## Parallelization within a cluster computing system

Although the situation becomes more complicated when using a cluster computing system (CCS), **simba** is built to streamline this process as much as possible. Before diving in, it is important to understand the basic workflow with a CCS. A CCS is a supercomputer that consists of a number of nodes, each of which may have multiple cores. A user will typically log into the head node of the CCS via SSH or an SSH client (such as PuTTY). Once logged into the head node, the user will submit jobs to the CCS using a special program called a [job management system]. Common [job management systems] include Slurm and SGE. The [job management system] will handle the process of passing your code from the head node to the other nodes in the CCS, running the code, and storing the results in your home directory. If this process is totally unfamiliar to you, ask the manager of your CCS or your IT team for a basic tutorial. (!!!!! web resource?)

There are two basic ways to run parallel code on a CCS. The first (and the one we recommend) is through job arrays. With job arrays, your [job management system] will run your 

When your [job management system] creates a job array, each task will be assigned some sort of task ID, which you can access from within your R code using the *Sys.getenv()* function. 

Also note that **simba** assigns a unique identifier called a *sim_uid* (simulation unique identifier) to each simulation replicate. These IDs are integers that range from 1 to N, where N is the total number of simulation replicates. Recall that the total number of replicates equals the number of simulation levels times the number of replicates per level, and can be accessed by typing `sim %>% get("total_replicates")`.



The key to making **simba** work with your [job management system] is to map this environment variable to simba. If you specify which [job management system] you are using via *set_config()*, **simba** will pass the task IDs for you.


!!!!! job arrays vs computing on a single multicore node

```R
sim %<>% set_config(jms="SGE")
```

If you are not using one of the supported job management systems (currently only SGE and Slurm are supported), you must manually pass these identifiers to your simulation object.

```R
tid <- as.numeric(Sys.getenv("SGE_TASK_ID"))
sim %>% set("task_id", tid)
```

Finally, submit your job array by typing the relevant command into your console, making sure that the range of task IDs corresponds to the range of sim_uids. For example, if you are using SGE and your simulation contains 100 replicates, type the following in your console:

```bash
> qsub run.sh -t 1-100
```

In a typical situation, you might have two files stored in your cluster's home directory, a shell script and the R file containing your simulation code. Your shell script (called *run.sh*) might look like this:

```bash
#!/bin/bash
Rscript my_sim.R
```

If you are using a [job management system], such as Slurm or Sun Grid Engine (!!!!!), . This Your corresponding R file might look like this:

```R

```
