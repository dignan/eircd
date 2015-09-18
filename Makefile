PROJECT = eircd

DEPS = gproc lager ranch
dep_gproc = git https://github.com/uwiger/gproc.git master
dep_lager = git https://github.com/basho/lager.git master
dep_ranch = git https://github.com/ninenines/ranch.git 1.1.0

include erlang.mk
