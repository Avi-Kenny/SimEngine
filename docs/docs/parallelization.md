---
layout: page
title: Parallelization
nav_order: 2
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
- A **job scheduler (JS)** is the software that runs on a CCS and manages the process of running jobs and job arrays. Slurm and Oracle Grid Engine are examples of job schedulers.

## Parallelization in **SimEngine**

There are three methods of parallelizing code using **SimEngine**:

1) **Outer parallelization**. This is the most straightforward way to parallelize your code. Most statistical simulations involve running multiple replicates of the same simulation, perhaps with certain things changing between replicates. With outer parallelization, a single simulation replicate is assigned to a single task.

2) **Inner parallelization**. With inner parallelization, one or more pieces within a single simulation replicate are parallelized. Inner parallelization is useful when your entire simulation only has a small handful of replicates (i.e. fewer replicates than available cores); otherwise, we recommend outer parallelization.

3) **Cluster parallelization**. Cluster parallelization is like outer parallelization but on a cluster computing system (CCS). Each simulation replicate is assigned to a single task, and tasks are submitted as a job array to the cluster computing system. This is the most complex method of parallelizing your code, but also the most powerful in terms of potential speed gains.

**SimEngine** is designed to automate as much of the parallelization process as possible. We give an overview of each parallelization method below. Afterwards, we provide tips and tricks that apply to all methods.

## Outer parallelization

To use outer parallelization, all you have to do is specify `parallel="outer"` using `set_config`. It's as simple as that.

```R
sim <- new_sim()
sim %<>% set_config(parallel = "outer")
```

Note that if a single simulation replicate runs in a very short amount of time (e.g. less than one second), using outer parallelization can actually result in a *decrease* in total speed. This is because there is a certain amount of computational overhead involved in setting up the parallelization engine inside **SimEngine**. If you want to do a quick speed comparison, try running your code twice, once with `set_config(parallel = "outer")` and once with `set_config(parallel = "none")`, and run `sim %>% vars("total_runtime")` each time to see the difference in total runtime. The exact overhead involved with outer parallelization will differ between machines.

If a machine has n cores available, **SimEngine** will n-1 cores by default. If you want to manually specify the number of cores to use, use the `n_cores` option of the `set_config` function:

```R
sim %<>% set_config(n_cores = 2)
```

## Inner parallelization

With inner parallelization, one or more pieces within a single simulation replicate are parallelized. This method of parallelization requires you to specify pieces of your code to run in parallel using functions from the **parallel** package. See the <a href="https://www.rdocumentation.org/packages/parallel">documentation</a> for the **parallel** package if you have never used this package before. **SimEngine** will create and manage the cluster object; simply reference the special `CL` object in your code (note: the term "cluster object" refers to an R object of class `cluster`; this is distinct from the use use of the word "cluster" in "cluster parallelization").

In the example below, inner parallelization is used within the `create_data` function through `parLapply`. However, you can also use parallel functions within your simulation script itself or within methods.

```R

sim <- new_sim()
sim %<>% set_config(parallel = "inner")

create_data <- function(sample_size) {
  data <- parLapply(CL, c(1:sample_size), function(x) {
    x <- rnorm(n=1, mean=10, sd=1)
    y <- 3*x + 9
    z <- x / y
    return(list(x,y,z))
  })
  df <- as.data.frame(matrix(unlist(data), ncol=3))
  names(df) <- c("x","y","z")
  return(df)
}

sim %<>% set_script(function() {
  df <- create_data(100)
  estimate <- mean(df$z)
  return (list("estimate" = estimate))
})

sim %<>% run()

```

## Cluster parallelization

Although the situation becomes more complicated when using a cluster computing system (CCS), **SimEngine** is built to streamline this process as much as possible. Before diving in, it is important to understand the basic workflow with a CCS. A CCS is a supercomputer that consists of a number of nodes, each of which may have multiple cores. A user will typically log into the CCS via SSH or an SSH client (such as PuTTY), and then send files containing computer programs to the CCS, either using Linux commands or using an FTP Client (such as FileZilla). Next, the user will run these programs by submitting "jobs" to the CCS using a special program called a job scheduler (JS). The JS manages the process of taking your jobs and running it in parallel across multiple nodes and/or multiple cores. If this process is totally unfamiliar to you, ask the manager of your CCS or your IT team for a basic tutorial.

Although there are multiple ways to run code in parallel on a CCS, we choose to make use of job arrays. The main **SimEngine** function that will be used is `run_on_cluster`. Throughout this example, we use Slurm as our JS, but an analogous workflow will apply to other JS software. Suppose we have written the following simulation and want to run it on a CCS:

```R
library(SimEngine)
sim <- new_sim()
create_data <- function(n) { rnorm(n) }
sim %<>% set_script(function() {
  data <- create_data(L$n)
  return(list("x"=mean(data)))
})
sim %<>% set_levels(n=c(100,1000))
sim %<>% set_config(num_sim=10)
sim %<>% run()
sim %>% summarize()
```

To run this code on a CCS, we must wrap in the `run_on_cluster` function. To use this function, you must break your code into three blocks, called `first`, `main`, and `last`. The code in the `first` block will run only once, and will set up the simulation object. When this is done, **SimEngine** will save the simulation object in the filesystem of your CCS. The code in the `main` block will then run for every simulation replicate, and will have access to the simulation object you created in the `first` block. In almost all cases, the code here will just include a single call to `run`, as illustrated below. Finally, the code in the `last` block will run after all your simulation replicates have finished running, and after **SimEngine** has automatically compiled the results into your simulation object. Use the `run_on_cluster` function as follows:

```R
library(SimEngine)
run_on_cluster(

  first = {
    sim <- new_sim()
    create_data <- function(n) { rnorm(n) }
    sim %<>% set_script(function() {
      data <- create_data(L$n)
      return(list("x"=mean(data)))
    })
    sim %<>% set_levels(n=c(100,1000))
    sim %<>% set_config(num_sim=10)
  },

  main = {
sim %<>% run()
  },

  last = {
    sim %>% summarize()
  },

  cluster_config = list(js="slurm")

)
```

Note that none of the actual simulation code changed; we just took chunks of the code and placed these chunks into the appropriate slot within `run_on_cluster` (either `first`, `main`, or `last`). Additionally, we had to tell **SimEngine** which job scheduler we are using, by specifying this in the `cluster_config` argument list. Run `js_support()` in R to see a list of supported JS software; the value in the `js_code` column is the value that should be specified in the `cluster_config` argument. Even if your JS is not supported, you can still use **SimEngine** on a CCS (see "Tips and tricks" below).

We're not done yet, though. We need to give the job scheduler instructions for how to run this code. Assume that the R code above is stored in a file called `my_simulation.R` that you have transferred to your CCS. First, we need to create a simple shell script that will run the my_simulation.R file. We use BASH as the scripting language, but you can use the shell scripting language of your choice. Create a file called `run_sim.sh` with the following two lines and place it in the same directory on your CCS as the `my_simulation.R` file:

```bash
#!/bin/bash
Rscript my_simulation.R
```

Finally, you will use your JS to submit three jobs. The first will run the `first` code, the second will run the `main` code, and the third will run the `last` code. With Slurm, you will type the following three commands into your shell:

```bash
sbatch --export=sim_run='first' run_sim.sh
#> Submitted batch job 101
sbatch --export=sim_run='main' --array=1-20 --depend=afterok:101 run_sim.sh
#> Submitted batch job 102
sbatch --export=sim_run='last' --depend=afterok:102 run_sim.sh
#> Submitted batch job 103
```

In the first line, we submit the script using the `sim_run='first'` environment variable, which tells **SimEngine** to only run the code in the `first` block within the `run_on_cluster` function in `my_simulation.R`. Note that after running this line, Slurm gives us the message "*Submitted batch job 101*". The number `101` is called the "job ID" and uniquely identifies a job on the CCS.

In the second line, we submit the script using the `sim_run='main'` environment variable and we tell Slurm to run a job array with "task IDs" 1-20. This means that 20 cores will be used, one per task. By default, the number of cores used will correspond with the total number of replicates in your simulation, such that each replicate is assigned to run on its own core (see the "Tips and Tricks" section below if you are not sure how many replicates are in your simulation). Also note that we included the option `--depend=afterok:101`, which tells Slurm to wait until the first job finishes before starting the job array. Change the number 101 to whatever number Slurm assigned to the first job. Once you submit this command, the code in the `main` block will be run for each replicate. A temporary folder called `sim_results` will be created and filled with objects representing the results and/or errors for each replicate.

In the third line, we submit the script using the `sim_run='last'` environment variable. Again, we use `--depend=afterok:102` to make sure this code doesn't run until all tasks in the job array have finished. When this job runs, **SimEngine** will compile the results from the `main` block, run the code in the `last` block, save the simulation object to the filesystem, and delete the temporary `sim_results` folder and its contents.

As mentioned above, the default **SimEngine** behavior is to run each replicate on its own core. However, it can sometimes be advantageous to run multiple replicates per core (e.g. if a single replicate has a short runtime). To do this, you need to do two things. First, specify the `n_cores` option using `set_config`.

```R
sim %<>% set_config(n_cores = 5)
```

Second, change the second `sbatch` command to match the value used for `n_cores`.

```bash
sbatch --export=sim_run='main' --array=1-5 --depend=afterok:101 run_sim.sh
#> Submitted batch job 102
```

## Tips and tricks

### Running locally

The `run_on_cluster` function is programmed such that it can also be run locally. What this means is that if you run the entire `run_on_cluster` function on your computer (rather than on a CCS), the code within the `first`, `main`, and `last` blocks will be executed. Objects created within these three blocks will not be saved, but a copy of your simulation object will be exported to the environment from which `run_on_cluster` was called (typically the global environment) so that you can examine the results, pass it to functions like `summarize`, and so on. This can be useful for testing simulations locally before sending them to a CCS.

### Dealing with Job Scheduler submission limits

Sometimes, Job Schedulers will impose limits in terms of the number of job array tasks that can be submitted. This may result in a situation where you cannot submit a job array with task ID greater than a certain number (for example 10,000). In these situations, the special environment variable `sim_add_to_tid` may be used to submit the job array in multiple chunks. If your Scheduler's limit is 10,000 and you want to submit a job array of 15,000 simulation replicates, you can do so as follows (note: you may have to wait for the first batch to finish before submitting the second batch so that the second batch is not rejected by the Scheduler):

```bash
sbatch --export=sim_run='first' run_sim.sh
#> Submitted batch job 101
sbatch --export=sim_run='main' --array=1-9000 --depend=afterok:101 run_sim.sh
#> Submitted batch job 102
sbatch --export=sim_run='main',sim_add_to_tid=9000 --array=1-6000 --depend=afterok:102 run_sim.sh
#> Submitted batch job 103
sbatch --export=sim_run='last' --depend=afterok:103 run_sim.sh
#> Submitted batch job 104
```

When the first batch is submitted, the task IDs 1 through 9,000 will be passed directly to **SimEngine** and the simulation replicates with `sim_uid` values 1 through 9,000 will be run, as usual. However, when the second batch is submitted, each of the task IDs 1 through 6,000 will have the number 9,000 (corresponding to the value set for `sim_add_to_tid`) added to them after they are passed to **SimEngine**, and so the simulation replicates with `sim_uid` values 9,001 through 15,000 will be run.

### Using "unsupported" job schedulers

It may be the case that you are using a Job Scheduler that **SimEngine** does not natively support. If this is the case, you can still use **SimEngine**; the key will be to identify the environment variable that your JS uses to uniquely identify tasks within a job array. For example, Slurm uses the variable `"SLURM_ARRAY_TASK_ID"` and Grid Engine uses the variable `"SGE_TASK_ID"`. Once you have identified this variable, specify it in your `cluster_config` block, as follows:

```R
run_on_cluster(

  first = {...},
  main = {...},
  last = {...},

  cluster_config = list(tid_var="SLURM_ARRAY_TASK_ID")

)
```

Alternatively, if you'd like for a job scheduler to be supported by **SimEngine**, please submit an issue on the <a href="https://github.com/Avi-Kenny/SimEngine/issues">**SimEngine** GitHub</a> page.

### Using the `batch` function on a CCS

Special caution must be taken if you use the `batch` function within your simulation. In this case, you need to set the `n_cores` option, rather than running one replicate per core. Furthermore, the maximum number of cores used cannot exceed the number of batches, since data is shared between replicates within a batch. If you do so, the extra cores will be unused. See the `batch` function documentation for additional information.
