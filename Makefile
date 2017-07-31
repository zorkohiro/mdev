#
#
#
obj-m = mdev.o
KVERSION = $(shell uname -r)
KBUILD ?= /lib/modules/$(KVERSION)/build

all:
	make -C $(KBUILD) M=$(PWD) modules

clean:
	make -C $(KBUILD) M=$(PWD) clean
	@rm -f Module.* tags
