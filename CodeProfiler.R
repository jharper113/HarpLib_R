
## Use this script to profile some code


# make sure that i don't have any code running in the source, just function definitions
source("")

Rprof("profile.out")
z <- FunctionNameHere(parm1, parm2)
Rprof(NULL)  # this command stops the profiler


View(Proftable("profile.out"))
