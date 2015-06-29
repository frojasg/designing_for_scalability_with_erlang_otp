# Supervisor

## Manual supervisor
Compilation phase
```erlang
1> c(coffee).
{ok,coffee}
2> c(hw).
{ok,hw}
```

Then we start our coffee machine
```erlang
3>  whereis(coffee).
undefined
4>  my_supervisor:start(coffee_sup, [{coffee, start_link, []}]).
Machine:Rebooted Hardware
Display:Make Your Selection
{ok,<0.45.0>}
```
We ask for the coffee pid
```erlang
6>  Pid = whereis(coffee).
<0.46.0>
```

Then we test our supervisor
```erlang
7> exit(Pid, kill).
Machine:Rebooted Hardware
Display:Make Your Selection
true
8> whereis(coffee).
<0.54.0>
```

Finally we stop our supervisor and their childrens
```erlang
9> my_supervisor:stop(coffee_sup).
stop
10> whereis(coffee).
undefined
```

Most of our supervisor code is generic.  Also this naive implementation of
supervisor barley scratch the surface of the responsibilities supervisors.


## OTP Supervisors

### Restart strategy
The restart tuple ```{RestartType, MaxRestart, MaxTime}``` specifies what the supervise
does to the other children in the child specification list in case a child terminates abnormally.
There are four different restart type: one_for_one, one_for_all, rest_for_one and simple_one_for_one.

* ___one_for_one___: only the crashed process is restarted. This strategy is ideal if the workers don't depend on each other and the termination of one will not affect the others.
* ___one_for_all___: if a process terminate, all processes are terminated and restarted. This strategy is used if all the process depend on each other.
* ___rest_for_one___: all processes started after the crashed process are terminated and restarted. You use this strategy if you start the processes in order of dependency.
* ___simple_one_for_one___: It is used for children of the same type added dynamically at runtime, not at startup.

 The rest of the parameters are: ```MaxRestart``` specifies the maximum number of restarts all child processes are allowed to do in ```MaxTime``` seconds. If the maximum amount of restart is reached, the supervise shutdown and the problem is escalated to the next higher level of supervisor.

 ### Child specification list
 Contain all the information the supervisor need to start, stop and delete its child processes.

 ```erlang
 {Name,StartFunction,RestartType,ShutdownTime,ProcessType,Modules}
 ```

the Elements are:

* ___Name___: Any valid erlang term, It has to be unique within a supervisor, but can be reused across supervisors within the same node.
* ___StartFunction___: A tuple of the format ```{Module, Function, Args}``` which calls one of the behavior start_link function. Supervisors can start only OTP-compliant behaviors.
* ___RestartType___: Tells the supervisor how to react to a child's termination. The restart type can be: permanent | transient | temporary
* ___ShutdownTime___: the time we give the child process between the supervisor issuing the EXIT signal and the _terminate_ callback finish. you can specify _brutal_kill_ if you don't want to wait.
* ___ProcessType___:  specifies if the child process is a supervisor or a worker.
* ___Modules___: is the list of modules implementing the behavior.
