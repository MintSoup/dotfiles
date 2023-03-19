file build/6ch_slave.elf
target extended-remote | \
openocd -f interface/stlink.cfg -f target/stm32g0x.cfg -c "gdb_port pipe; log_output openocd.log"
load
b main
cont
