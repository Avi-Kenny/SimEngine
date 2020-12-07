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

Parallelization is the process of speeding up a computer program by dividing it into independent tasks and running those tasks simultaneously across multiple computer processors. Most modern laptops will have two or more processors (or "cores"), and many statisticians have access to so-called cluster computing systems (CCS), which can have hundreds of processing nodes, each of which can have multiple cores. Roughly speaking, a program that can be run in ten minutes when executed on a single core will take just one minute if it can be broken into ten separate tasks that all run at the same time. Therefore, parallelization can result in massive gains in computing speed and should be done whenever possible.

Not all code can be parallelized; the separate tasks cannot exchange information or depend on each other in any way. However, you can still write programs that are *partially* parallel, such as when you separately compute ten estimates in parallel and then take the mean of the ten estimates.

The terminology associated with parallel computing can be confusing - what is the difference between a node, a core, and a processor? What is the difference between a job, a task, and a thread? We use the following definitions:

- A **node** is a single computer. Each node has access to physical resources, such as processing cores and memory. Your laptop is a node. A CCS is a collection of multiple nodes.
- A **core** (or a *processor*) is an electronic component within a computer that executes code. Many modern laptops will have more than one core, and each node on a CCS will usually have multiple cores.
- A **task** (or a *thread*) is a portion of code that runs on a single core.
- A **cluster computing system (CCS)** is a type of "supercomputer", usually created and managed by IT specialists, specifically designed to handle large numbers of parallel tasks coming from multiple users.
- A **job** is a collection of tasks that are part of the same simulation.
- A **job array** is a special type of job that contains a number of near-identical tasks
- A **job scheduler (JS)** is the software that runs on a CCS and manages the process of running jobs and job arrays. Slurm and Sun Grid Engine are examples of job schedulers.

## Parallelization in **simba**

There are three methods of parallelizing code using **simba**:

1) **Outer parallelization**. This is the most straightforward way to parallelize your code. Most statistical simulations involve running multiple <a href="/docs/concepts.html" target="_blank">replicates</a> of the same simulation, perhaps with certain things changing between replicates. With outer parallelization, a single simulation replicate is assigned to a single task.

2) **Inner parallelization**. With inner parallelization, one or more pieces within a single simulation replicate are parallelized. Inner parallelization is useful when your entire simulation only has a small handful of replicates (i.e. fewer replicates than available cores); otherwise, we recommend outer parallelization.

3) **Cluster parallelization**. Cluster parallelization is like outer parallelization but on a cluster computing system (CCS). Each simulation replicate is assigned to a single task, and tasks are submitted as a job array to the cluster computing system. This is the most complex method of parallelizing your code, but also the most powerful in terms of potential speed gains.

**simba** is designed to automate as much of the parallelization process as possible. We give an overview of each parallelization method below. Afterwards, we provide tips and tricks that apply to all methods.

## Outer parallelization

To use outer parallelization, all you have to do is specify *parallel="outer"* using *set_config()*. It's as simple as that.

```R
sim <- new_sim()
sim %<>% set_config(parallel = "outer")
```

Note that if a single simulation replicate runs in a very short amount of time (e.g. less than one second), using outer parallelization can actually result in a *decrease* in total speed. This is because there is a certain amount of computational overhead involved in setting up the parallelization engine inside **simba**. If you want to do a quick speed comparison, try running your code twice, once with *set_config(parallel = "outer")* and once with *set_config(parallel = "none")*, and run sim %<>% get("total_runtime") each time to see the difference in total runtime. The exact overhead involved with outer parallelization will differ between machines.

## Inner parallelization

With inner parallelization, one or more pieces within a single simulation replicate are parallelized. This method of parallelization requires you to specify pieces of your code to run in parallel using functions from the **parallel** package. See the <a href="https://www.rdocumentation.org/packages/parallel" target="_blank">documentation</a> for the **parallel** package if you have never used this package before. **simba** will create and manage the cluster object; simply reference the special *CL* object in your code (note: the term "cluster object" refers to an R object of class *cluster*; this is distinct from the use use of the word "cluster" in "cluster parallelization").

In the example below, inner parallelization is used within the *create_data()* function through *parLapply()*. However, you can also use parallel functions within your simulation script itself or within methods.

```R

sim <- new_sim()
sim %<>% set_config(parallel = "inner")

sim %<>% add_creator(
  "create_data",
  function(sample_size) {
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
)

sim %<>% add_script(
  "my script",
  function() {
    df <- create_data(100)
    estimate <- mean(df$z)
    return (list("estimate" = estimate))
  }
)

sim %<>% run("my script")

```

## Cluster parallelization

Although the situation becomes more complicated when using a cluster computing system (CCS), **simba** is built to streamline this process as much as possible. Before diving in, it is important to understand the basic workflow with a CCS. A CCS is a supercomputer that consists of a number of nodes, each of which may have multiple cores. A user will typically log into the CCS via SSH or an SSH client (such as PuTTY), and then send files containing computer programs to the CCS, either using Linux commands or using an FTP Client (such as FileZilla). Next, the user will run these programs by submitting "jobs" to the CCS using a special program called a job scheduler (JS). The JS manages the process of taking your jobs and running it in parallel across multiple nodes and/or multiple cores. If this process is totally unfamiliar to you, ask the manager of your CCS or your IT team for a basic tutorial.

Although there are multiple ways to run code in parallel on a CCS, we focus on job arrays. The main **simba** function that we use is *run_on_cluster()*. Throughout this example, we use Sun Grid Engine as our JS, but an analogous workflow will apply to other JS software.

Suppose we have written the following simulation and want to run it on a CCS:

```R
library(simba)
sim %<>% new_sim()
sim %<>% add_creator("create_data", function(n){ rnorm(n) })
sim %<>% add_script("my script", function() {
  data <- create_data(L$n)
  return(mean(data))
})
sim %<>% set_levels(n=c(100,1000))
sim %<>% set_config(num_sim=10)
sim %<>% run()
sim %<>% summary()
```

To run this code on a CCS, we must wrap in the *run_on_cluster()* function. To use this function, you must break your code into three blocks, called *first*, *main*, and *last*. The code in the *first* block will run only once, setting up the simulation object and saving it in the filesystem of your CCS. The code in the *main* block will run for every simulation replicate, and will have access to the simulation object you created in the *first* block. Typically, the code here will just include a single call to *run()*, as illustrated below. Finally, the code in the *last* block will run after all your simulation replicates have finished running, and after **simba** has automatically compiled the results into your simulation object. Use the *run_on_cluster()* function as follows:

```R
library(simba)
run_on_cluster(

  first = {
    sim %<>% new_sim()
    sim %<>% add_creator("create_data", function(n){ rnorm(n) })
    sim %<>% add_script("my script", function() {
      data <- create_data(L$n)
      return(mean(data))
    })
    sim %<>% set_levels(n=c(100,1000))
    sim %<>% set_config(num_sim=10)
  },

  main = {
    sim %<>% run()
  },

  last = {
    sim %<>% summary()
  },

  cluster_config = list(sim_var="sim", js="slurm")

)
```

Note that none of our actual simulation code changed; we just took chunks of the code and placed these chunks into the appropriate slot within *run_on_cluster()* (either *first*, *main*, or *last*). Additionally, we had to specify a few configuration options using the *cluster_config* argument. The first may seem a bit odd, but we need to tell *run_on_cluster()* the name of the variable that we use to store our simulation object. If we had instead written *my_sim2 %<>% new_sim()* as our first line, we would instead specify *sim_var="my_sim2"*. The second configuration option is the name of our job scheduler. Type *?run_on_cluster* in R for a list of supported JS software. Even if your JS is not supported, you can still use **simba** on a CCS (see "Tips and tricks" below).

We're not done yet, though. We need to give our job scheduler instructions for how to run this code. Assume that the R code above is stored in a file called "my_simulation.R" that you have transferred to your CCS. First, we need to create a simple shell script that will run the my_simulation.R file. We use BASH as our scripting language, but you can use the shell scripting language of your choice. Create a file called "run_sim.sh" with the following two lines and place it in the same directory on your CCS as the "my_simulation.R" file:

```bash
#!/bin/bash
Rscript my_simulation.R
```

Finally, you will use your JS to submit three jobs. The first will run the *first* code, the second will run the *main* code, and the third will run the *last* code. With Sun Grid Engine, you will type the following three commands into your shell:

```bash
qsub -v run='first' run_sim.sh
#> Your job 101 ("run_sim.sh") has been submitted
qsub -v run='main' -t 1-20 -hold_jid 101 run_sim.sh
#> Your job-array 102.1-10:1 ("run_sim.sh") has been submitted
qsub -v run='last' -hold_jid 102 run_sim.sh
#> Your job 103 ("run_sim.sh") has been submitted
```

In the first line, we submit the script using the "-v run='first'" option, which tells **simba** to only run the code in the *first* block within the *run_on_cluster()* function in *my_simulation.R*. Note that after running this line, SGE gives us the message "*Your job 101 ("run_sim.sh") has been submitted*". The number *101* is called the "job ID" and uniquely identifies our job on the CCS.

In the second line, we submit the script using the "-v run='main'" option and we tell SGE to run a job array with "task IDs" 1-20. Importantly, the number 20 corresponds to the total number of replicates in our simulation (see the "Tips and Tricks" section below if you are not sure how many replicates are in your simulation). This runs the code in the *main* block 20 times; **simba** will automatically assign each task to a different simulation replicate. Also note that we included the option *-hold_jid 101*, which tells SGE to wait until the first job finishes before starting the job array. Change the number 102 to whatever number SGE assigned to the first job.

In the third line, we submit the script using the "-v run='last'" option, which tells **simba** to only run the code in the *last* block. Again, we use *-hold_jid* to make sure this code doesn't run until all tasks in the job array have finished.

## Tips and tricks for parallelization

!!!!! TO DO (add_to_tid, run_on_cluster works locally, specify 'tid_var' instead of 'js')
