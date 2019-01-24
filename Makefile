SRC=src/main.adb

all: obj/main

flash: main.bin
	st-flash --reset write main.bin 0x08000000

main.bin: obj/main
	arm-eabi-objcopy -O binary obj/main main.bin

obj/main: prj.gpr $(SRC)
	gprbuild -Pprj.gpr

