SRC=src/main.adb
SRC_TESTS=src/tests.adb

all: obj/main

check: obj/tests

flash: main.bin
	st-flash --reset write main.bin 0x08000000

flasht: tests.bin
	st-flash --reset write tests.bin 0X08000000

main.bin: obj/main
	arm-eabi-objcopy -O binary obj/main main.bin

tests.bin: obj/tests
	arm-eabi-objcopy -O binary obj/tests tests.bin

obj/main: prj.gpr $(SRC)
	gprbuild -Pprj.gpr

obj/tests:  tests.gpr $(SRC_TESTS)
	gprbuild -Ptests.gpr

.PHONY: clean

clean:
	gprclean prj.gpr
	gprclean tests.gpr
	${RM} main.bin
	${RM} tests.bin
